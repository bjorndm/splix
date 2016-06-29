/*

The Sleepycat License

Splix is copyright (c) 2015 (bjorn.de.meyer@gmail.com),

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, 
   this list of conditions and the following disclaimer.
   
2. Redistributions in binary form must reproduce the above copyright notice, 
   this list of conditions and the following disclaimer in the documentation 
   and/or other materials provided with the distribution.
   
3. Redistributions in any form must be accompanied by information on how to 
   obtain complete source code for the Splix software and any accompanying 
   software that uses the Splix software. The source code must either be 
   included in the distribution or be available for no more than the cost of 
   distribution plus a nominal fee, and must be freely redistributable under 
   reasonable conditions. For an executable file, complete source code means 
   the source code for all modules it contains. It does not include source code 
   for modules or files that typically accompany the major components of the 
   operating system on which the executable file runs.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR(S) ``AS IS'' AND ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF 
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR NON-INFRINGEMENT, ARE 
DISCLAIMED. IN NO EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, 
OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF 
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE 
OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*/

#ifndef _WIN32
#define _POSIX_C_SOURCE 200809L
#endif

#include <stdio.h>
#include <stdlib.h>
#include <expat.h>
#include <errno.h>
#include <stdarg.h>
#include <ctype.h>
#include <assert.h>


/* mkdir and isatty portability hack */
#ifdef _WIN32
#include <direct.h>
#include <io.h>
#define SPLIX_MKDIR(PATH, MODE) _mkdir((PATH))
#define SPLIX_ISATTY(FD) _isatty(FD)
#define SPLIX_FILENO(AFILE) _fileno(AFILE)
#else
#include <unistd.h>
#include <sys/stat.h>
#define SPLIX_MKDIR(PATH, MODE) mkdir((PATH), (MODE))
#define SPLIX_ISATTY(FD) isatty(FD)
#define SPLIX_FILENO(AFILE) fileno(AFILE)
#endif

#include "ses.h"
#include "slre.h"
#include "kbtree.h"
#include "kvec.h"
#include "parg.h"


#define SPLIX_BUFF_SIZE (1024*256)
#define SPLIX_VERSION "2.0.0"
#define SPLIX_FILE_BUFF_SIZE (1024*512) 

/* Optional flags that influence splix's behavior. */
#define SPLIX_KEEP_NAMESPACES_FLAG                1
#define SPLIX_DO_NOT_ESCAPE_QUOTE_FLAG            2
#define SPLIX_DO_NOT_ESCAPE_APOS_FLAG             4
#define SPLIX_MAKE_DIRECTORY_FLAG                 8
#define SPLIX_KEEP_TOPLEVEL_NAMESPACES_ONLY_FLAG  16
#define SPLIX_QUIET_FLAG                          32
#define SPLIX_VERBOSE_FLAG                        64
#define SPLIX_TTY_FLAG                            128
 

/** Struct for file names generated. */
struct SplixName {
  char * name;
  int    used;
};

typedef struct SplixName SplixName;

#define splixname_compare(a, b) (strcmp((a).name,(b).name))

KBTREE_INIT(SplixNameTree, SplixName, splixname_compare)


/** Struct for file name parts. */
struct SplixNamePart {
  char * part;
};

typedef struct SplixNamePart SplixNamePart;

/** Struct for namespaces. */
struct SplixNamespace {
  char * key;
  char * namespace;
};

typedef struct SplixNamespace SplixNamespace;



struct SplixData {
  XML_Parser   parser;
  FILE       * fout;
  char       * dirname;
  int          last_index;
  char       * last_element;
  char       * outname;
  int          split_level;
  int          current_level;
  FILE       * fheader;
  FILE       * fsplit;
  int          split_number;
  int          must_add_ns;
  unsigned int flags;
  int          split_count;
  
  /* Flags to help preserve self-closing tags */
  long long                 start_index;
  int                       first_child;
  /* Names that the splittter already has used. */
  kbtree_t(SplixNameTree) * names; 
  
  /* Stack of name parts. */
  kvec_t(SplixNamePart)     parts;
  
  /* Stack of name spaces of the root tag. */
  kvec_t(SplixNamespace)    copy_ns;
};

/** Escapes xml into in into the buffer Swis in into with to_escape  replaced by escape in buffer me by escape */
char * swis_escape_xml(Swis * me, const char * in) {
  for (; (*in); in++) {
    switch (*in) { 
      case '<'  : swis_append_cstr(me, "&lt;"); break;
      case '>'  : swis_append_cstr(me, "&gt;"); break;
      case '&'  : swis_append_cstr(me, "&amp;"); break;
      case '"'  : swis_append_cstr(me, "&quot;"); break;
      case '\'' : swis_append_cstr(me, "&apos;"); break; 
      default   : swis_append_char(me, *in); break;
    }
  }
  swis_nul_terminate(me);
  return me->text;
}

/** Escapes xml into in into the buffer Swis in into with to_escape  
 * replaced by escape in buffer me by escape. the quoting is supressed based 
 * on the flags */
char * swis_escape_xml_flags(Swis * me, const char * in, unsigned int flags) {
  for (; (*in); in++) {
    switch (*in) { 
      case '<'  : swis_append_cstr(me, "&lt;"); break;
      case '>'  : swis_append_cstr(me, "&gt;"); break;
      case '&'  : swis_append_cstr(me, "&amp;"); break;
      case '"'  : if (flags & SPLIX_DO_NOT_ESCAPE_QUOTE_FLAG) {
                    swis_append_char(me, '"');
                  } else { 
                    swis_append_cstr(me, "&quot;");
                  }
                  break;
      case '\'' : if (flags & SPLIX_DO_NOT_ESCAPE_APOS_FLAG) {
                    swis_append_char(me, '\'');
                  } else { 
                    swis_append_cstr(me, "&apos;");
                  }
                  break;
      default   : swis_append_char(me, *in); break;
    }
  }
  swis_nul_terminate(me);
  return me->text;
}


/** Encodes the string to percent code. */
char * swis_percent_encode(Swis * out, char * in) {
  for (; (*in); in++) {
    int ch = 0xFF & (*in);
    if (isalnum(ch)) { 
      swis_append_char(out, ch);
    } else {
      char buf[16];
      sprintf(buf, "%02X", ch);
      swis_append_char(out, '%');
      swis_append_cstr(out, buf);
    }
  } 
  swis_nul_terminate(out);
  return out->text;
}

/** Decodes the string from percent code. */
char * swis_percent_decode(Swis * out, const char * in) {
  /* Walk over the bytes in the string to  decode. */     
  while ((*in)) {
    int ch = (*in);
    if (isalnum(ch)) { 
      swis_append_char(out, ch);
      in++;
    } else if (ch == '%') {
        char buf[16];
        int  n1     = *(in + 1);
        int  n2     = *(in + 2);
        int outch;
        
        sprintf(buf, "%c%c", n1, n2);
        outch = strtol(buf, NULL, 16);
        swis_append_char(out, outch);
        in += 3;
    } else { 
        /* Whatever, just copy the byte */
        swis_append_char(out, ch);
        in++;
    }
  }
  return out->text;
}


void splix_report_error_va(char * format, va_list args) {
  vfprintf(stderr, format, args);
  fprintf(stderr, "\n");
}

void splix_report_error(char * format, ...) {
  va_list args;
  va_start(args, format);
  splix_report_error_va(format, args);
  va_end(args);
}

void splix_report_fatal(int exitcode, char * format, ...) {
  va_list args;
  va_start(args, format);
  splix_report_error_va(format, args);
  va_end(args);
  exit(exitcode);
}

/* Recursive mkdir. */
int splix_mkpath(const char* file_path, mode_t mode) {
  int res;
  char * p;
  char * aid_path = strdup(file_path);
  assert(file_path && *file_path);

  p = aid_path;
  do {
    p   = strchr(p+1, '/');
    /* End case of the last subdirectory with no further subdirs. */
    if (!p) {
      res = SPLIX_MKDIR(aid_path, mode);
      if ((res == -1) && (errno == EEXIST)) {
        res = 0;
      }
      break;
    } else {
      *p    = '\0';
      errno = 0;
      res = SPLIX_MKDIR(aid_path, mode);
      if ((res == -1) && (errno != EEXIST)) {
        break;
      }
      *p    = '/';
    }
  } while(p);
  
  free(aid_path);
  return res;
}
  
const char * splix_find_matching_attribute_value(const XML_Char ** atts, char * regexp) {
  const XML_Char ** attr;  
  for (attr = atts; attr[0] ; attr += 2) {
    const char * attribute = attr[0];
    const char * value     = attr[1];
    if (slre_match(SLRE_NO_CAPTURE, regexp, attribute, strlen(attribute)) == SLRE_OK) {
      return value;
    }    
  }
  return NULL;
}  
  
  
char * splix_attribute_name(Swis * real_out, const XML_Char *name, const XML_Char **atts) {
  Swis out;
  const char * id;
  const char * ref;
  
  swis_new_empty(&out);
  swis_append_cstr(&out, (char *)name);
  
  /* Find the first attribute that matches id in any case. */
  id = splix_find_matching_attribute_value(atts, "[iI][dD]");
  if (id) {
    swis_append_cstr(&out, (char *)id);
  } else {
    ref = splix_find_matching_attribute_value(atts, ".*[rR][eR][fF].*");
    if (ref) {
      swis_append_cstr(&out, (char *)ref);
    }
  }
  
  /* Percent encode the whole to the real output. */
  swis_percent_encode(real_out, out.text);
  swis_free(&out);
  
  return real_out->text;
}


int splix_output_fwrite(const void *ptr, size_t size, size_t nmemb, void * splix_data) {
  struct SplixData * sdat = splix_data;
  FILE * fout = ( sdat->fout ? sdat->fout : stdout);
  return fwrite(ptr, size, nmemb, fout);
}


int splix_output_vprintf(void * data, const char * format, va_list args) {
  struct SplixData * sdat = data;
  FILE * fout = ( sdat->fout ? sdat->fout : stdout);
  return vfprintf(fout, format, args);
}

int splix_output_print(void * data, const char * format, ...) {
  int result;
  va_list args;
  va_start(args, format);
  result = splix_output_vprintf(data, format, args);
  va_end(args);
  return result;
}

                    

int splix_change_split_file(struct SplixData * sdat, const XML_Char *name, const XML_Char **atts) {
  Swis outname;
  char ibuf[255];
  int index, stop;
  SplixName  * used;
  SplixName   check;
  
  
  swis_new(&outname, sdat->outname);
  swis_append_char(&outname, '/');
  
  /* Concatenate all name parts. */
  stop = kv_size(sdat->parts);  
  for (index = 0; index < stop; index++) {
    SplixNamePart np;
    np = kv_a(SplixNamePart, sdat->parts, index);
    if (index > 0) { 
      swis_append_char(&outname, '_');
    }
    swis_append_cstr(&outname, np.part);
  }
  
  check.name = outname.text;
  check.used = 0;
  /* Look up if the name was already used in the btree */
  used = kb_getp(SplixNameTree, sdat->names, &check);
  
  if (used) {
    /* The name was already used. Increase the count and add a suffix to the name. */    
    char suffix[64];
    used->used++;
    sprintf(suffix, "%d", used->used);
    swis_append_char(&outname, '.');
    swis_append_cstr(&outname, suffix);
  } else {
    /* The name wasn't used yet. Store a duplicate of it */
    check.name = strdup(outname.text);
    kb_putp(SplixNameTree, sdat->names, &check);
  }
  
  /* Finally add an .xml extension. */
  swis_append_cstr(&outname, ".xml");
  
  if (sdat->fsplit) {
    fclose(sdat->fsplit);
    sdat->fsplit = NULL;
  }
  
  sdat->fsplit = fopen(outname.text, "w");
  
  if (!(sdat->flags & SPLIX_QUIET_FLAG)) {
    if (sdat->flags & SPLIX_TTY_FLAG) { 
      fprintf(stderr, "\r\33[Kwriting: %s", outname.text);
    } else {
      fprintf(stderr, "\nwriting: %s", outname.text);
    }
  }  
  
  if (sdat->fsplit) {   
    /* Keep reference to split-off file in output file. */
    fprintf(sdat->fheader, "<splix ref=\"%s\"/>", outname.text);
  } else {     
    fprintf(stderr, "\nCould not open: %s\n", outname.text);
  }
    
  swis_free(&outname);
  
  if (!sdat->fsplit) {
    splix_report_fatal(ENOENT, "Could not open output file.");
  }
  
  /* Use large buffers for writing. */
  setvbuf(sdat->fsplit, NULL, _IOFBF, SPLIX_FILE_BUFF_SIZE);
  
  /* Add an XML header to the new output file. */
  fprintf(sdat->fsplit,  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
  
  /* Prepare to add the namespaces to the next open tag if needed */
  sdat->must_add_ns = (sdat->flags & SPLIX_KEEP_NAMESPACES_FLAG);
  
  return 0;
}


void splix_handle_start_element(void *data, const XML_Char *name, const XML_Char **atts) {
  int index;
  const XML_Char ** attr;
  struct SplixData * sdat = data;
  Swis attribute_name;
  SplixNamePart part;
  /* Use the flags for XML exaping, but don't allow " to be unescaped
   * because it would make quoted attributes not work anymore. */
  unsigned int flags = sdat->flags & (~((unsigned int)SPLIX_DO_NOT_ESCAPE_QUOTE_FLAG));
  
  swis_new_empty(&attribute_name);
  splix_attribute_name(&attribute_name, name, atts);
  /* Push name part onto the name stack. */
  part.part = attribute_name.text;
  kv_push(SplixNamePart, sdat->parts, part);
  
  
  /* Split off to a new file if the depth level is reached */
  if (sdat->current_level == sdat->split_level) {
      splix_change_split_file(sdat, name, atts);
      sdat->fout = sdat->fsplit;
  }
  sdat->current_level++;
  
  sdat->first_child = !0;
  /* tag output... */
  splix_output_print(data, "<%s", name);
  
  /* Add namespaces if needed, i.e. on a new document if requested with -N. */
  if (sdat->must_add_ns) {   
    for (index = 0; index < kv_size(sdat->copy_ns); index++) {
      SplixNamespace ns = kv_a(SplixNamespace, sdat->copy_ns, index);
      splix_output_print(data, " %s=", ns.key);
      splix_output_print(data, "\"%s\"", ns.namespace);
    }  
    sdat->must_add_ns = 0;
  }
  
  /* And  parse attributes */
  for (attr = atts; attr[0] ; attr += 2) {
    Swis buf;
    swis_new_empty(&buf);
    swis_escape_xml_flags(&buf, attr[0], flags);
    splix_output_print(data, " %s=", buf.text);
    swis_free(&buf);
    swis_new_empty(&buf);
    swis_escape_xml_flags(&buf, attr[1], flags);
    splix_output_print(data, "\"%s\"", buf.text);
    swis_free(&buf);
    
    /* Handle namespaces, store them if we are in the "header" level. */
    if (slre_match(SLRE_NO_CAPTURE, "^xmlns:", attr[0], strlen(attr[0])) == SLRE_OK) {
      int do_save = (sdat->fheader == sdat->fout);
      if (sdat->flags & SPLIX_KEEP_TOPLEVEL_NAMESPACES_ONLY_FLAG) { 
        do_save = do_save && (sdat->current_level == 1);
      }
            
      if (do_save) {          
        SplixNamespace ns;
        ns.key        = strdup(attr[0]);
        ns.namespace  = strdup(attr[1]);
        if (sdat->flags & SPLIX_VERBOSE_FLAG) {
          fprintf(stderr, "Will copy namespace %s->%s\n", attr[0], attr[1]);
        }
        kv_push(SplixNamespace,  sdat->copy_ns, ns);
      }
    }  
        
  }
  /*  splix_output_print(data, ">"); */
}

/* Report position for debugging. */
void splix_report_position(struct SplixData * sdat, const char * prefix, const XML_Char * name) {
  int bi, bc;  
  bi                = XML_GetCurrentByteIndex(sdat->parser);
  bc                = XML_GetCurrentByteCount(sdat->parser);
  fprintf(stderr, "%s %s %d %d\n", prefix, name, bi, bc);  
}

void splix_handle_end_element(void *data, const XML_Char *name) {
  SplixNamePart part;
  struct SplixData * sdat = data;
  int bc;
  
  /* A self closing tag will have a byte count of 0 
   * since it doesnt take any space itself. */
  bc                = XML_GetCurrentByteCount(sdat->parser);
  if (0 == bc) {
    splix_output_print(data, "/>");  
  } else {  
    if (sdat->first_child) { /* There's a tag for us to put a closing > for */
      splix_output_print(data, ">");  
    }
    splix_output_print(data, "</%s>", name);  
  }
  sdat->first_child = 0;

  /* Close split file if the depth level is reached, output back to header. */
  sdat->current_level--;
  if (sdat->current_level == sdat->split_level) {
      fclose(sdat->fsplit);
      sdat->fsplit = NULL;
      sdat->fout   = sdat->fheader;
      sdat->split_count++;
  }
  
  /* Pop the last name part off the name stack and clean it up. */
  part = kv_pop(sdat->parts);
  free(part.part);
  
  
}

/* s is not 0 terminated. */
void splix_handle_character_data(void *data, const XML_Char *s, int len) {    
  
  struct SplixData * sdat = data;
  if (sdat->first_child) { /* There's a tag for us to put a closing > for */
    splix_output_print(data, ">");  
    sdat->first_child = 0;
  }
  
  splix_output_fwrite(s, sizeof(XML_Char), len, data);  
}

/* target and data are 0 terminated */
void splix_handle_processing_instruction(void *data, 
  const XML_Char *target, const XML_Char * instruction) {
  splix_output_print(data, "<?%s?>", instruction);  
}

/* data is 0 terminated */
void splix_handle_comment(void *data, const XML_Char * comment) {
  splix_output_print(data, "<!--%s-->", comment);  
}

void splix_handle_start_cdata(void *data) {
  splix_output_print(data, "<!CDATA[[");  
}

void splix_handle_end_cdata(void *data) {
  splix_output_print(data, "]]>");  
}

void splix_handle_default(void *data, const XML_Char *s, int len) {
  splix_output_fwrite(s, sizeof(XML_Char), len, data);
}

/* This is called for the start of the DOCTYPE declaration, before
   any DTD or internal subset is parsed.
*/
void splix_handle_start_doctype(void *data, const XML_Char *name,
                                            const XML_Char *sysid,
                                            const XML_Char *pubid,
                                            int has_internal_subset) {
  splix_output_print(data, "<!DOCTYPE %s %s %s", name, sysid, pubid);  
}

void splix_handle_end_doctype(void *data) {
  splix_output_print(data, ">");
}

void splix_handle_unparsed_entity_decl(void *data,    
                              const XML_Char *name,
                              const XML_Char *base,
                              const XML_Char *sysid,
                              const XML_Char *pubid,
                              const XML_Char *notation) {
  splix_output_print(data, "<!ENTITY %s %s %s %s %s>", name, base, sysid, pubid, notation);  
}

/* This is called for a declaration of notation.  The base argument is
   whatever was set by XML_SetBase. The notationName will never be
   NULL.  The other arguments can be.
*/
void splix_handle_notation_decl(void *data,
                                    const XML_Char *name,
                                    const XML_Char *base,
                                    const XML_Char *sysid,
                                    const XML_Char *pubid) {
  splix_output_print(data, "<!NOTATION %s %s %s %s %s>", name, base, sysid, pubid);                                        
}

/* When namespace processing is enabled, these are called once for
   each namespace declaration. The call to the start and end element
   handlers occur between the calls to the start and end namespace
   declaration handlers. For an xmlns attribute, prefix will be
   NULL.  For an xmlns="" attribute, uri will be NULL.
*/
void splix_handle_start_ns_decl(void *data,
                                const XML_Char *prefix,
                                const XML_Char *uri) {
fprintf(stderr,  "XXX namespace: %s %s\n", prefix, uri);

}

void splix_handle_end_ns_decl(void *data,
                                const XML_Char *prefix) {
  fprintf(stderr,  "XXX namespace end: %s\n", prefix);
}


/* This is called if the document is not standalone, that is, it has an
   external subset or a reference to a parameter entity, but does not
   have standalone="yes". If this handler returns XML_STATUS_ERROR,
   then processing will not continue, and the parser will return a
   XML_ERROR_NOT_STANDALONE error.
   If parameter entity parsing is enabled, then in addition to the
   conditions above this handler will only be called if the referenced
   entity was actually read.
*/
int splix_handle_not_standalone(void *data) {
  return XML_STATUS_OK;
}


/* This is called for a reference to an external parsed general
   entity.  The referenced entity is not automatically parsed.  The
   application can parse it immediately or later using
   XML_ExternalEntityParserCreate.

   The parser argument is the parser parsing the entity containing the
   reference; it can be passed as the parser argument to
   XML_ExternalEntityParserCreate.  The systemId argument is the
   system identifier as specified in the entity declaration; it will
   not be NULL.

   The base argument is the system identifier that should be used as
   the base for resolving systemId if systemId was relative; this is
   set by XML_SetBase; it may be NULL.

   The publicId argument is the public identifier as specified in the
   entity declaration, or NULL if none was specified; the whitespace
   in the public identifier will have been normalized as required by
   the XML spec.

   The context argument specifies the parsing context in the format
   expected by the context argument to XML_ExternalEntityParserCreate;
   context is valid only until the handler returns, so if the
   referenced entity is to be parsed later, it must be copied.
   context is NULL only when the entity is a parameter entity.

   The handler should return XML_STATUS_ERROR if processing should not
   continue because of a fatal error in the handling of the external
   entity.  In this case the calling parser will return an
   XML_ERROR_EXTERNAL_ENTITY_HANDLING error.

   Note that unlike other handlers the first argument is the parser,
   not userData.
*/
int splix_handle_external_entity_ref(XML_Parser parser,
                                    const XML_Char *context,
                                    const XML_Char *base,
                                    const XML_Char *sysid,
                                    const XML_Char *pubid) {
  return XML_STATUS_OK;
}

/* This is called in two situations:
   1) An entity reference is encountered for which no declaration
      has been read *and* this is not an error.
   2) An internal entity reference is read, but not expanded, because
      XML_SetDefaultHandler has been called.
   Note: skipped parameter entities in declarations and skipped general
         entities in attribute values cannot be reported, because
         the event would be out of sync with the reporting of the
         declarations or attribute values
*/
void splix_handle_skipped_entity(void *data,
                                 const XML_Char *name,
                                 int is_parameter_entity) {
}


/* This is called for an encoding that is unknown to the parser.

   The encodingHandlerData argument is that which was passed as the
   second argument to XML_SetUnknownEncodingHandler.

   The name argument gives the name of the encoding as specified in
   the encoding declaration.

   If the callback can provide information about the encoding, it must
   fill in the XML_Encoding structure, and return XML_STATUS_OK.
   Otherwise it must return XML_STATUS_ERROR.

   If info does not describe a suitable encoding, then the parser will
   return an XML_UNKNOWN_ENCODING error.
*/
int splix_handle_unknown_encoding(void *data,
                                   const XML_Char *name,
                                   XML_Encoding *info) {
  return XML_STATUS_ERROR;
}



/* Sets up the parser to use the splix_handle_xxx callbacks, and 
 * sets it's userdata tp data.
 */
void splix_setup_parser(XML_Parser parser, void * data) {
  XML_SetStartElementHandler(parser         , splix_handle_start_element);
  XML_SetEndElementHandler(parser           , splix_handle_end_element);
  XML_SetCharacterDataHandler(parser        , splix_handle_character_data);
  XML_SetProcessingInstructionHandler(parser, splix_handle_processing_instruction);
  XML_SetCommentHandler(parser              , splix_handle_comment);
  XML_SetStartCdataSectionHandler(parser    , splix_handle_start_cdata);
  XML_SetEndCdataSectionHandler(parser      , splix_handle_end_cdata);
  XML_SetDefaultHandler(parser              , splix_handle_default);
  XML_SetStartDoctypeDeclHandler(parser     , splix_handle_start_doctype);
  XML_SetEndDoctypeDeclHandler(parser       , splix_handle_end_doctype);
  XML_SetUnparsedEntityDeclHandler(parser   , splix_handle_unparsed_entity_decl);
  XML_SetNotationDeclHandler(parser         , splix_handle_notation_decl);
 /* 
  XML_SetStartNamespaceDeclHandler(parser   , splix_handle_start_ns_decl);
  XML_SetEndNamespaceDeclHandler(parser     , splix_handle_end_ns_decl);

  XML_SetNotStandaloneHandler(parser        , splix_handle_not_standalone);
  XML_SetExternalEntityRefHandler(parser    , splix_handle_external_entity_ref);
  XML_SetSkippedEntityHandler(parser        , splix_handle_skipped_entity);
  XML_SetUnknownEncodingHandler(parser      , splix_handle_unknown_encoding, NULL);
*/ 

  XML_SetUserData(parser                    , data);
}


int splix_setup_data(struct SplixData * data, XML_Parser parser, char * outname, int level, unsigned int flags) {
  Swis buf;
  
  if (!data) return -1;
  data->parser        = parser;
  data->first_child   = 0;
  data->start_index   = 0;
  data->outname       = outname;
  data->split_level   = level;
  data->current_level = 0;
  data->split_number  = 0;
  data->fsplit        = NULL;
  data->flags         = flags;
  data->must_add_ns   = 0;
  data->split_count   = 1;  /* 1 becase header is always written. */
  
  /* Check if running interactively on a tty. */
  if (SPLIX_ISATTY(SPLIX_FILENO(stdin))) {
    data->flags |= SPLIX_TTY_FLAG;
  }
  
  /* Open header file. */
  swis_new(&buf,outname);
  
  /* But first make directory if reqested. */
  if (data->flags & SPLIX_MAKE_DIRECTORY_FLAG) {
    int res = splix_mkpath(buf.text, 0770);
    if (res < 0) { 
      fprintf(stderr, "Could not make directory %s\n", buf.text);
    }
  }
  
  
  swis_append_cstr(&buf,"/header.xml");  
  data->fheader       = fopen(buf.text, "w");
  if (!data->fheader) { 
    splix_report_fatal(ENOENT, "Could not open file %s/header.xml", outname);
  }
  swis_free(&buf);
  
  /* buffer output for speed */
  setvbuf(data->fheader, NULL, _IOFBF, SPLIX_FILE_BUFF_SIZE);    
  data->fout          = data->fheader;
  
  /* Open binary tree */
  data->names         = kb_init(SplixNameTree, KB_DEFAULT_SIZE); 
  
  /* Open parts stack */ 
  kv_init(data->parts);
  
  /* Open namespace stack */ 
  kv_init(data->copy_ns);
  
  return 0;
}


/* Traversal handler */
#define cleanup_traverse(p) free((p)->name);

int splix_cleanup_data(struct SplixData * data) {
  int index;
  if (data->fheader) fclose(data->fheader);
  if (data->names) {     
    __kb_traverse(SplixName, data->names, cleanup_traverse);
    kb_destroy(SplixNameTree, data->names);
  }
  
  
  for (index = 0; index < kv_size(data->parts); index++) {
    SplixNamePart part = kv_a(SplixNamePart, data->parts, index);
    free(part.part);
  }  
  kv_destroy(data->parts);
  
  for (index = 0; index < kv_size(data->copy_ns); index++) {
    SplixNamespace ns = kv_a(SplixNamespace, data->copy_ns, index);
    free(ns.key);
    free(ns.namespace);
  }  
  kv_destroy(data->copy_ns);
  
  return 0; 
}


static int splix_split_file(FILE * fin, char * outname, int level, unsigned int flags) {
  int result = 0;
  int setres = 0;
  size_t bytes_read;
  XML_Parser parser;
  parser = XML_ParserCreate(NULL);
  if (!parser) return ENOMEM;  
  struct SplixData data;
  
  setres = splix_setup_data(&data, parser, outname, level, flags);
  if (setres) {
    return setres;
  }
  
  splix_setup_parser(parser, &data);
  
  do {  
    char * buff = XML_GetBuffer(parser, SPLIX_BUFF_SIZE);

    if (buff == NULL) {      
      fprintf(stderr, "XML_GetBuffer out of memory.\n");
      result = ENOMEM;
      break;
    }


    bytes_read = fread(buff, 1, SPLIX_BUFF_SIZE, fin);
    
    if (ferror(fin)) {
      result = EBADF;
      fprintf(stderr, "fread error.\n");
      break;
      /* handle error */
    }

    if (XML_Parse(parser, (const char *) buff, bytes_read, (bytes_read == 0)) == XML_STATUS_ERROR) {
      /* handle parse error */
      result = XML_GetErrorCode(parser);
      fprintf(stderr, "XML_ParseBuffer Error:%s.\n At %d %d %d; read: %u\n"
        , XML_ErrorString(XML_GetErrorCode(parser))
        , (int) XML_GetCurrentByteIndex(parser)
        , (int) XML_GetCurrentLineNumber(parser) 
        , (int) XML_GetCurrentColumnNumber(parser)
        , (unsigned int) bytes_read
        );     
      
      fprintf(stderr, "%*s\n", (int)bytes_read, (char *)buff);
      break;
    }

  } while (bytes_read != 0);
  
  if (!(data.flags & SPLIX_QUIET_FLAG)) {
    fprintf(stderr, "\nWritten %d files.\n", data.split_count);
  }
  
  splix_cleanup_data(&data);  
  XML_ParserFree(parser);  
  return result;
}


static int splix_split_filename(char * inname, char * outname, int level, unsigned int flags) {
  FILE * fin;
  int result;
  fin = fopen(inname, "r");
  if (!fin) { 
    fprintf(stderr, "Input file %s not found.\n", inname);
    return ENOENT;
  }
  result = splix_split_file(fin, outname, level, flags);
  fclose(fin);
  fprintf(stderr, "\n"); 
  return result;
}

void splix_show_help() {
  fprintf(stderr, "Usage: splix -i <input_file> -o <output_dir> -l <split_level> [-n] [-q] [-a]\n");
  fprintf(stderr, "\nSplits the XML file <input_file> into several fragments in <output_dir>"
                "\nbased on it's contents. The split will be created at <split_level>"
                "\nof the XML hierarchy. The three options are mandatory."
                "\n\nOptions:"
                "\n  -n: copy header-level namespaces from input to output."
                "\n  -q: supress &quot; escapes where possible"
                "\n  -a: supress &amp; escapes where possible"
                "\n  -v: show version and exit"
                "\n  -d: create output directory"
                "\n  -N: copy top-level namespaces only from input to output."
                "\n  -Q: be more quiet."
                "\n  -V: be more verbose."
                "\n\nSplix is copyright Björn De Meyer, 2015."
                "\nYou may freely use it under the Sleepycat License."
                "\n"
                );
}

void splix_show_version() {
  fprintf(stderr, "splix version " SPLIX_VERSION 
                "\n\n\tSplix is copyright Björn De Meyer, 2015."
                "\n\tYou may freely use it under the Sleepycat License."
                "\n"
                );
}



int main(int argc, char * argv[]) {
  int split_level = -1; 
  char * input_name = NULL;
  char * output_name = NULL;
  unsigned int flags = 0;
  int opt;
  struct parg_state ps;
  parg_init(&ps);
  
  while ((opt = parg_getopt(&ps, argc, argv, "l:i:o:adnqvNQV")) != -1) {
    switch (opt) {
    case 'i':
      input_name = (char *) ps.optarg;
      break;
    
    case 'o': 
      output_name = (char *) ps.optarg;
      break;
    
    case 'l':
      split_level = atoi(ps.optarg);
    break;
    
    case 'n': 
      flags |= SPLIX_KEEP_NAMESPACES_FLAG;
    break;
    
    case 'a':
      flags |= SPLIX_DO_NOT_ESCAPE_APOS_FLAG;
    break;
      
    case 'q':
      flags |= SPLIX_DO_NOT_ESCAPE_QUOTE_FLAG;
    break;
    
    case 'd': 
      flags |= SPLIX_MAKE_DIRECTORY_FLAG;
    break;

    case 'v':
      splix_show_version();
      exit(EXIT_FAILURE);
    break;
    
    case 'Q':
      flags |= SPLIX_QUIET_FLAG;
    break;
    
    case 'V':
      flags |= SPLIX_VERBOSE_FLAG;
    break;

    case 'N':           
      flags |= SPLIX_KEEP_NAMESPACES_FLAG;
      flags |= SPLIX_KEEP_TOPLEVEL_NAMESPACES_ONLY_FLAG;
    break;
                     
    default: /* '?' */
      splix_show_help();
      exit(EXIT_FAILURE);
      break;
    }
  }
  
  if ((!input_name) || (!output_name) || (split_level < 0)) {
    splix_show_help();
    exit(EXIT_FAILURE);
  } 
    
  return splix_split_filename(input_name, output_name, split_level, flags);
}






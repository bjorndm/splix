#include <stdio.h>
#include <stdlib.h>
#include <expat.h>
#include <errno.h>
#include <stdarg.h>
#include <unistd.h>

#include "ses.h"

#define SPLIX_BUFF_SIZE (1024*256)
#define SPLIX_VERSION "1.0-RC1"
#define SPLIX_FILE_BUFF_SIZE (1024*512) 


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
  /* Flags to help preserve self-closing tags */
  long long    start_index;
  int          first_child;
};

/** Escapes xml into in into the buffer Swis in into with to_escape  replaced by esxape in buffer me by escape */
char * swis_escape_xml(Swis * me, const char * in) {
  for (; (*in); in++) {
    switch (*in) { 
      case '<'  : swis_append_cstr(me, "&lt;"); break;
      case '>'  : swis_append_cstr(me, "&gt;"); break;
      case '&'  : swis_append_cstr(me, "&amp;"); break;
      case '"'  : swis_append_cstr(me, "&quot;"); break;
      /* case '\'' : swis_append_cstr(me, "&apos;"); break; 
       XXX this should be quoted I guess.
       */
      default   : swis_append_char(me, *in); break;
    }
  }
  swis_nul_terminate(me);
  return me->text;
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

int splix_change_split_file(struct SplixData * sdat) {
  Swis buf;
  char ibuf[255];
  swis_new(&buf, sdat->outname);
  swis_append_char(&buf, '/');
  
  sprintf(ibuf, "%d", sdat->split_number);
  swis_append_cstr(&buf, ibuf);

  sdat->split_number++;
  
  
  if (sdat->fsplit) {
    fclose(sdat->fsplit);
    sdat->fsplit = NULL;
  }
  
  sdat->fsplit = fopen(buf.text, "w");
    
  fprintf(stderr, "\33[1\rKwriting: %s %p",buf.text, sdat->fsplit);  
  swis_free(&buf);
  if (!sdat->fsplit) return ENOENT;
  
  /* Use large buffers for writing. */
  setvbuf(sdat->fsplit, NULL, _IOFBF, SPLIX_FILE_BUFF_SIZE);
  
  return 0;
}


void splix_handle_start_element(void *data, const XML_Char *name, const XML_Char **atts) {
  const XML_Char ** attr;
  struct SplixData * sdat = data;
  
  /* Split off to a new file if the depth level is reached */
  if (sdat->current_level == sdat->split_level) {
      splix_change_split_file(sdat);
      sdat->fout = sdat->fsplit;
  }
  sdat->current_level++;
  
  sdat->first_child = !0;
  splix_output_print(data, "<%s", name);
  for (attr = atts; attr[0] ; attr += 2) {
    Swis buf;
    swis_new_empty(&buf);
    swis_escape_xml(&buf, attr[0]);
    splix_output_print(data, " %s=", buf.text);
    swis_free(&buf);
    swis_new_empty(&buf);
    swis_escape_xml(&buf, attr[1]);
    splix_output_print(data, "\"%s\"", buf.text);
    swis_free(&buf);
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
  }
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
  XML_SetDefaultHandler(parser        , splix_handle_default);
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


int splix_setup_data(struct SplixData * data, XML_Parser parser, char * outname, int level) {
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
  swis_new(&buf,outname);
  swis_append_cstr(&buf,"/header.xml");  
  data->fheader = fopen(buf.text, "w");
  if (!data->fheader) return ENOENT;
  setvbuf(data->fheader, NULL, _IOFBF, SPLIX_FILE_BUFF_SIZE);
  
  swis_free(&buf);
  data->fout =  data->fheader;    
  return 0;
}


int splix_cleanup_data(struct SplixData * data) {
  if (data->fheader) fclose(data->fheader);
  return 0; 
}


static int splix_split_file(FILE * fin, char * outname, int level) {
  int result = 0;
  int setres = 0;
  size_t bytes_read;
  XML_Parser parser;
  parser = XML_ParserCreate(NULL);
  if (!parser) return ENOMEM;  
  struct SplixData data;
  
  setres = splix_setup_data(&data, parser, outname, level);
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
  
  
  splix_cleanup_data(&data);  
  XML_ParserFree(parser);  
  return result;
}


static int splix_split_filename(char * inname, char * outname, int level) {
  FILE * fin;
  int result;
  fin = fopen(inname, "r");
  if (!fin) { 
    fprintf(stderr, "Input file %s not found.\n", inname);
    return ENOENT;
  }
  result = splix_split_file(fin, outname, level);
  fclose(fin);
  return result;
}

void splix_show_help() {
  fprintf(stderr, "Usage: splix -i <input_file> -o <output_dir> -l <split_level> [-N]\n");
  fprintf(stderr, "\tSplits the XML file <input_file> into several fragments in <output_dir>"
                "\n\tbased on it's contents. The split will be created at <split_level>"
                "\n\tof the XML hierarchy."
                "\n\n\tSplix is copyright Björn De Meyer, 2015."
                "\n\tYou may freely use it under the Sleepycat License."
                "\n" 
                );
}



int main(int argc, char * argv[]) {
  int split_level = -1, keep_namespaes; 
  char * input_name = NULL, *output_name = NULL;
  int opt;
  
  while ((opt = getopt(argc, argv, "l:i:o:")) != -1) {
    switch (opt) {
    case 'i':
      input_name = optarg;
      break;
    
    case 'o': 
      output_name = optarg;
      break;
    
    case 'l':
      split_level = atoi(optarg);
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
  
  return splix_split_filename(input_name, output_name, split_level);
}






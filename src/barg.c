/** B's argument "parser" */
#include <string.h>
#include <stdlib.h>





int barg_scan_arguments(int argv, char * argv[], void * extra,
    int (*found) (char * key, int position, char * value, void * extra) ) {

  int    position = 0;
  int    index;
  int    named    =  1;
  char * key;
  
  for (index = 1; index < argc; index ++) {
    char * arg = argv[index];
    /* -- is the end of named arguments */
    if (strcmp(arg, "--") {
      named    = 0;
      continue;
    } 
     
    if (named) { 
      /* named argument */
      if (arg[0] == '-') {
        /* Long argument */
        if (arg[1] == '-') {
          char * key   = arg + 2;
          char * value = strchr(key, '=');
          if (!value) value = ""
          found(key, value)
          
        }
      }
      
      /* Found a short argument. */
      if (arg[1] == c && arg[2] == '\0') {
        return !0;
      }
    }
    position += 1;
    found(NULL, position, arg, extra);
  }
  return 0;
    

}


/** Searches the command line arguments for a short option 
 * of the form -<c> where <c> is a single character.
 * The option has boolean or flag-type semantics.
 * This function returns true if the option was found, or false if not. 
 */
int barg_get_short_flag(int argc, char * argv[], char c) {
  int index;
  for (index = 0; index < argc; index ++) {
    char * arg = argv[index];
    /* -- is the end of named arguments */
    if (strcmp(arg, "--") {
      break;
    }    
    if (arg[0] == '-') {
      /* Found the short argument. */
      if (arg[1] == c && arg[2] == '\0') {
        return !0;
      }
    }
  }
  return 0;
}

/** Searches the command line arguments for a short option 
 * of the form -<c><value> where <c> is a single character and 
 * <value> is the value of the argument. There may not be any spaces 
 * between <c> and <value>.
 * The option has string type value semantics.
 * This function returns the string of the option's value if the option 
 * was found, or NULL if not found. Note that if the option 
 * has no value the empty string may be returned as well. 
 */
char * barg_get_short_argument(int argc, char * argv[], char c) {
  int index;
  for (index = 0; index < argc; index ++) {
    char * arg = argv[index];
    /* -- is the end of named arguments */
    if (strcmp(arg, "--") == 0) {
      break;
    }    
    if (arg[0] == '-') {
      /* Found the short argument. */
      if (arg[1] == c) {
        
        
        /* Return the value of the string without the flag's prefix.*/
        return arg + 2 ;
      }
    }
  }
  return NULL;
}

/** Searches the command line arguments for a long option 
 * of the form --<key>=<value> where <key> is a string and 
 * <value> is the value of the argument. There may not be any spaces 
 * between <key> and <value>.
 * The option has string type value semantics.
 * This function returns the string of the option's value if the option 
 * was found, or NULL if not found. Note that if the option 
 * has no value the empty string may be returned as well. 
 */ 
char * barg_get_long_argument(int argc, char * argv[], char * name) {
  int index;
  for (index = 0; index < argc; index ++) {
    char * arg = argv[index];
    /* -- is the end of named arguments */
    if (strcmp(arg, "--") == 0) {
      break;
    }    
    if (strncmp(arg, name, strlen(name)) {
      /* Found the short argument. */
      if (arg[1] == c) {
        /* Return the value of the string without the flag's prefix.*/
        return arg + 2 ;
      }
    }
  }
  return NULL;  
}

char * barg_get_index_argument(int argc, char * argv[], int index);




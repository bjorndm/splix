
#define BPTRIE_BUFF_SIZE 8
#include <string.h>
#include <stdlib.h>

/* Bjorn's Prefix trie, a binary prefix tree for storing used file 
 * names efficiently.  */
struct Bptrie {
  char * prefix;
  size_t prefix_length;
  struct Bptrie * parent;
  struct Bptrie * left_child;
  struct Bptrie * right_child;
};


struct Bptrie * bptrie_init_empty(struct Bptrie * me) {
    if (!me)   return NULL;
    me->parent      = NULL;
    me->left_child  = NULL;
    me->right_child = NULL;
    return me;
}

struct Bptrie * bptrie_alloc() {
    return calloc(1, sizeof(struct Bptrie));
}

struct Bptrie * bptrie_new_empty() {
    return bptrie_init_empty(bptrie_alloc());
}

struct Bptrie * bptrie_lookup(struct Bptrie * me, char * key) {
  int cmp;
  if (!me) return NULL;
  cmp = strncmp(key, me->prefix, me->prefix_length);
  if (cmp == 0) { 
    if (strlen(key) == me->prefix_length) { 
      return me;
    }
  }
  if (cmp < 0) return bptrie_lookup(me->left_child, key);
  return bptrie_lookup(me->right_child, key);
}



/* run.config
   STDOPT: +"-no-val-builtins-auto"
   COMMENT: bts #1390, issue with typing of quantified variables
*/

#include "stdlib.h"

/*@behavior exists:
    assumes \exists integer i; 0 <= i < (int)n && ((char*)buf)[i] == c;
    ensures \forall int j; 0 <= j < (int)\offset((char*)\result) ==> ((char*)buf)[j] != c;
  behavior not_exists:
    assumes \forall integer k; 0 <= k < (int)n ==> ((char*)buf)[k] != c;
    ensures \result == (void*) 0; */
void *memchr(const void *buf, int c, size_t n) {
  int i;
  char *s = buf;
  for(i = 0; i < n; i++) {
    if(*s == c) return s;
    s++;
  }
  return (void*)0;
}

int main(void) {
  memchr("toto", 'o', 4);
  memchr("tata", 'o', 4);
  return 0;
}

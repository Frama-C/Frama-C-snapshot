#include "string.h"

void f(char* s) {
  char t[strlen(s) + 1];
  char* p = t;
  while(*s) *p++ = *s++;
  *p = 0;
}

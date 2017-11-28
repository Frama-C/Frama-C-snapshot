/* run.config
   DONTRUN: main test is in merge_attrs_align.c
*/

typedef struct __attribute__((packed)) {
  char a;
  short b; // offset: 1 (packed)
} s;

s s1;

// for testing with GCC/Clang
#ifndef __FRAMAC__
#include <stddef.h>
#include <stdio.h>
#endif
int f1() {
  char c = s1.a;
#ifndef __FRAMAC__
  printf("f1: offsetof b = %lu\n", offsetof(s, b));
#endif
  return 0;
}

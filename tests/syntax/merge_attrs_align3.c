/* run.config
   DONTRUN: main test is in merge_attrs_align.c
*/

typedef struct __attribute__((packed)){
  char a;
  short b __attribute__((aligned(2))); // offset: 2
} s;

s s1;

// for testing with GCC/Clang
#ifndef __FRAMAC__
#include <stddef.h>
#include <stdio.h>
int f3() {
  printf("f3: offsetof b = %lu\n", offsetof(s, b));
}
#endif

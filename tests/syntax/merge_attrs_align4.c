/* run.config
   DONTRUN: main test is in merge_attrs_align.c
*/

#pragma pack(1)

typedef struct {
  char a;
  short b; // offset: 1 (packed)
} s;

s s1;

// for testing with GCC/Clang
#ifndef __FRAMAC__
#include <stddef.h>
#include <stdio.h>
int f4() {
  printf("f4: offsetof b = %lu\n", offsetof(s, b));
}
#endif

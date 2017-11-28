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
#endif
int f4() {
  char c = s1.a;
#ifndef __FRAMAC__
  printf("f4: offsetof b = %lu\n", offsetof(s, b));
#endif
  return 0;
}

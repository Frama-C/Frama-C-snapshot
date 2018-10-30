#include <stdlib.h> // for size_t
#include <string.h>

char t1[5] = "abcde";
char t2[] = "abcde";

int main(int c){
  int r1a, r1b, r1c, r2a, r2b, r2c;
  r1a = strnlen(t1, 3);
  r1b = strnlen(t1, 5);
  if (c & 1) r1c = strnlen(t1, 6);

  r2a = strnlen(t2, 3);
  r2b = strnlen(t2, 5);
  r2c = strnlen(t2, 6);

  Frama_C_dump_each();
  return 0;
}

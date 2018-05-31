/* run.config
   OPT: -rte -print -machdep gcc_x86_32 -journal-disable
*/

#include <stdlib.h>

struct S {
unsigned length;
int fam[0];
};

int main () {
  unsigned l = 3;
  struct S* s = malloc(sizeof(*s) + sizeof(int) * l);
  if (s) {
    s->length = l;
    for (int i = 0; i < s->length; i++) s->fam[i] = i;
    return s->fam[s->length - 1];
  }
  return 0;
}

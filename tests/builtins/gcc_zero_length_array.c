/* run.config*
  STDOPT: +"-machdep gcc_x86_32 -val-builtin malloc:Frama_C_malloc_fresh -slevel 11"
 */
#include <stdlib.h>

// valid usage of a GCC-style flexible array member

typedef struct {
  int len;
  int buf[0];
} gcc_fam;

gcc_fam *make_fam(int len) {
  int i;
  gcc_fam *p = malloc(sizeof(gcc_fam)+sizeof(int)*len);
  p->len = len;
  for (i = 0; i < len; i++) {
    p->buf[i] = i;
  }
  return p;
}

int main() {
  gcc_fam *pfam = make_fam(11);
  int res = pfam->buf[10];
  free(pfam);
  return res;
}

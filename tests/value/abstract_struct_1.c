/* run.config*
   STDOPT: #"tests/value/abstract_struct_2.c -lib-entry -value-msg-key initial-state"
*/
#include "share/libc/stdlib.h"

struct abstracttype;
struct something {
  struct abstracttype *data;
};
static struct something *repositories;

void main(void) {
  repositories = calloc(1, sizeof(struct something));
}

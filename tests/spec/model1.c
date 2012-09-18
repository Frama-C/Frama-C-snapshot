/* run.config
STDOPT: +"tests/spec/model2.c"
*/

#include "tests/spec/model1.h"

void main () {
  struct S s;
  reset(&s);
  inc(&s);
  /*@ assert s.foo > 0; */
  /*@ loop variant s.foo; */
  while (is_pos(&s)) dec(&s);
  /*@ assert s.foo <= 0; */
}
  
  

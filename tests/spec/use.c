/* run.config
   STDOPT: +"tests/spec/use2.c"
*/

// BTS 0887

#include "tests/spec/dec.h"

//@ ensures X > 0 ; ensures F(1) > 0 ;
void f(void) {}

/* run.config
   OPT: tests/spec/volatile_aux.c -print -check -copy
*/

#include "tests/spec/volatile.h"
const int c = 1 ;
volatile int v ;
int * p;
//@lemma comp_const_addr: p==&c;
//@lemma comp_volatile_addr: p==&v;
//@lemma volatile_in_annot_is_illegal: v == 1 ==> v==1;

int main () {

  int x = v;
  v = f(x);

  return 0;

}

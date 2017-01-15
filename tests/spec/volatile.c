/* run.config
   OPT: tests/spec/volatile_aux.c -print -copy
*/

#include "tests/spec/volatile.h"

//@volatile x,y writes w ;
//@volatile y,z reads r writes w; // partially KO: y already has a writes
//@volatile x writes w; //KO: already a write function for x
//@ volatile y reads r; //KO: already a reads function for x

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

/* run.config_pruntime
OPT: -wp -wp-model Runtime -wp-no-logicvar -journal-disable -wp-verbose 2 -wp-proof z3

 */

typedef int TAB32[32];
TAB32 tab[32];
int * p = &tab[0][0];

/*@ requires p == &tab[0][0] ;
  ensures \true ;
*/
int main() {
  return 1 ;
}

//@ ensures \result == 1;
int f (void) {
  int x = 1;
  // check if M2 detects that it doesn't know how to do this :
  *((int *)(0x1234)) = 3;
  return x;
}

/* run.config
  STDOPT: +"-pdg"
*/


/* This example used to loop during the computation of control dependencies,
   more precisely "generalized postdominators". See bts 1436 */

void __VERIFIER_assert(int cond) {
  if (!(cond)) {
    ERROR: goto ERROR;
  }
  return;
}

int main(unsigned int loop1, unsigned int m1) {
  int sn=0;
  unsigned int x=0;

  while(1){
    sn = sn + 2;
    x++;
    __VERIFIER_assert(sn==x*2 || sn == 0);
  }
}

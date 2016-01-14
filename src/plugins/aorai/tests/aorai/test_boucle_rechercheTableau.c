/* run.config
   EXECNOW: make tests/aorai/Aorai_test.cmxs
   OPT: -aorai-automata tests/aorai/test_boucle_rechercheTableau.ya -aorai-test 1 -aorai-acceptance -load-module tests/aorai/Aorai_test.cmxs -aorai-test-number @PTEST_NUMBER@
*/



/*@ requires \valid_range(t,0,max);
  @ requires max>=0;
  @ ensures 0<=\result<=max || \result==-1 ;
  @ ensures 0<=\result<=max ==> t[\result]==val;
  @ ensures \result==-1 ==> (\forall integer j; 0<=j<=max ==> t[j]!=val);
 */
int isPresent(int t[], int max, int val) {  
  int i=0;
  /*@ loop invariant inv : 
    @      0<=i<=max
    @   && \valid_range(t,0,max)
    @   && max>=0
    @   && (\forall integer j; 0<=j<=i-1 ==> t[j]!=val);
    @  loop variant v : max-i ;
   */
  while (i<max && t[i]!=val) {
    i++;
  }
  if(t[i]==val) return i;
  return -1;
}

void foo(){}

int main(int argc, char** argv) {
  int tab[]={10,20,33,15};
  int r=isPresent(tab, 3, 33);

  if (r==-1) foo();
  
  return 1;
}

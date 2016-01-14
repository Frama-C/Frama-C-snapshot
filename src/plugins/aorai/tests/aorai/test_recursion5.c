/* run.config
   EXECNOW: make tests/aorai/Aorai_test.cmxs
   OPT: -aorai-automata tests/aorai/test_recursion5.ya -aorai-test 1 -aorai-acceptance -load-module tests/aorai/Aorai_test.cmxs -aorai-test-number @PTEST_NUMBER@
*/


//frama-c t2.c -aorai-automata t2.ya -aorai-dot -aorai-show-op-spec -aorai-output-c-file t2_annot.c
//frama-c -jessie t2_annot.c -jessie-why-opt="-fast-wp"

//#include <stdio.h>


/*@ requires \valid_range(t,0,max);
  @ requires max>=0;
  @ requires 0<=i<=max;
  @ decreases max-i;
  @ ensures i<=\result<=max || \result==-1 ;
  @ ensures i<=\result<=max ==> t[\result]==val;
  @ ensures \result==-1 ==> (\forall integer j; i<=j<=max ==> t[j]!=val);
 */
int isPresentRec(int t[], int i, int max, int val) {
//  printf("t : %d | s : %d | v : %d\n",t[0],size,val);
  if(t[i]==val) return i;
  if(max==i) return -1; // 1 de plus que max
  return isPresentRec(t, i+1, max, val);
}


/*@ requires \valid_range(t,0,max);
  @ requires max>=0;
  @ ensures 0<=\result<=max || \result==-1 ;
  @ ensures 0<=\result<=max ==> t[\result]==val;
  @ ensures \result==-1 ==> (\forall integer i; 0<=i<=max ==> t[i]!=val);
 */
int isPresent(int t[], int max, int val) {
  return isPresentRec(t, 0, max, val);
}

void foo(){}

int main(int argc, char** argv) {
  int tab[]={10,20,33,15};
  int r=isPresent(tab, 3, 33);
  if (r==-1) foo();

//  printf("Résultat = %d\n",r);
  return 1;
}

/* run.config
   EXECNOW: make tests/aorai/Aorai_test.cmxs
   OPT: -aorai-automata tests/aorai/test_recursion4.ya -aorai-test 1 -aorai-acceptance -load-module tests/aorai/Aorai_test.cmxs -aorai-test-number @PTEST_NUMBER@
*/

# pragma JessieIntegerModel(math)

/*@ requires \valid(t+(0..size-1));
  @ requires size>=0;
  @ decreases size;
  @ ensures -1<=\result<size ;
  @ behavior found :
  @   ensures 0<=\result ==> t[\result]==val;
  @ behavior notIn :
  @   ensures \result==-1 ==> (\forall integer i; 0<=i<size ==> t[i]!=val);
 */
int isPresent(int t[], int size, int val) {
  if(size==0) return -1;
  if(t[0]==val) return 0;
  int r=1+isPresent(t+1, size-1, val);
  if (r==0) r=-1;
  return r;
}

void foo(){}

int main(int argc, char** argv) {
  int tab[]={10,20,33,15};
  int r=isPresent(tab, 4, 33);
  if (r==-1) foo();

  return 1;
}

/* run.config
   COMMENT: \at
*/

int A = 0;

/*@ ensures \at(A,Post) == 3; */
void f(void) {
  A = 1;
 F: A = 2;
  /*@ assert \at(A,Pre) == 0; */
  /*@ assert \at(A,F) == 1; */
  /*@ assert \at(A,Here) == 2; */
  /*@ assert \at(\at(A,Pre),F) == 0; */
  A = 3;
}

void g(int *p, int *q) {
  *p = 0;
  *(p+1) = 1;
  *q = 0;
 L1: *p = 2;
  *(p+1) = 3;
  *q = 1;
 L2: A = 4;
  /*@ assert (\at(*(p+\at(*q,L1)),L2) == 2); */
 L3:
  /*@ assert (\at(*(p+\at(*q,L1)),Here) == 2); */

  //  /*@ assert (\at(*(p+\at(*q,L1)),L3) == 2); */ // doesn't work yet
  //  /*@ assert (\at(*(p+\at(*q,L2)),L1)) == 1; */
  return ;
}

/*@ ensures \result == x; */
int h(int x) { return x; }

int main(void) {

  int x;

  x = h(0);
 L: /*@ assert x == 0; */ x = 1;
  x = 2;

  f();

  /*@ assert \at(x,L) == 0; */
  /*@ assert \at(x+1,L) == 1; */
  /*@ assert \at(x,L)+1 == 1; */

  int t[2];
  g(t,&x);

  return 0;
}

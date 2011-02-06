/* run.config
   OPT: -rte -rte-no-all -rte-print -rte-precond -journal-disable
*/

/*@ 
  ensures \result == -x ;

  behavior pos:
    assumes x >= 0 ;
    ensures \result <= 0;
    assigns *y;

  behavior neg:
    assumes x < 0 ;
    ensures \result > 0;
    assigns \nothing;

  complete behaviors pos,neg;
  disjoint behaviors pos,neg;

*/
int f(int x, int *y) {
  if (x >= 0) 
    *y = x;
  return -x;
}

/*@
  assigns *x;
  ensures *x == y;
*/
void g(int y, int* x) {
  *x = f(y,&y);
}

int main() {
  
  int a = 5;
  int c;
  int b = f(a,&c);

  g(b,&a);

  b = b + a;

  return b;

}

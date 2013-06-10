/* run.config
   OPT: -rte -warn-signed-overflow -rte-no-all -print -rte-precond
*/

/*@ 
  ensures \result == -x ;

  behavior pos:
    assumes first_bhv:(x >= 0) ;
    ensures \result <= 0;
    assigns *y;

  behavior neg:
    assumes second_bhv:(x < 0 );
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

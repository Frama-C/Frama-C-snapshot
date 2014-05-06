/* run.config
   GCC:
   OPT: -val -deps -out -input -journal-disable
   OPT: -val -deps -out -input -main f -journal-disable
*/


int t[5];
int x;
int *p,*q;

void f(int i) {
//  x = t[i];
  p = t+i;
// q = &t[i];
}

void main () {
  t[2] = 77;
  f(2);

}

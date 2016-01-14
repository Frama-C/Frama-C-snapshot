/* run.config
   GCC:
   STDOPT: #"-main main"
   STDOPT: #"-main f"
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

/* run.config
   OPT: -val -semantic-const-folding -journal-disable
*/

void f(int *x) { (*x)++; }

int main () {
  int Y = 42;
  f(&Y);
  return Y;
}

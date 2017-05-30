/* run.config
   OPT: -val -val-show-progress -scf -val-show-progress -journal-disable
*/

void f(int *x) { (*x)++; }

int main () {
  int Y = 42;
  f(&Y);
  return Y;
}

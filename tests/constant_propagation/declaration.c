/* run.config
   DONTRUN: bugfix in progress
   OPT: -val -semantic-const-folding -journal-disable
*/

void f(int *x) { (*x)++; }

int Y = 42;

int main () {
  f(&Y);
  return Y;
}

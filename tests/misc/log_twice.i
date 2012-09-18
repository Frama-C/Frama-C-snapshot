/* run.config
   OPT: -load-script tests/misc/log_twice
*/

int* f() {
  int x;
  return &x;
}

void main(int x) {
  int *p = f();
  *p = 1;
}

/* run.config
   OPT: -val -slice-assert f -slice-print -slicing-level 0 -journal-disable
*/
/*@ requires a > 0; */
int f(int a) {
  int b = 2 * a;
  /*@ assert a < b; */
  return 42;
}

int main () {
  f(10);
  return 0;
}

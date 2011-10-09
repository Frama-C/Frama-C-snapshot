/* run.config
   OPT: -check -val -slice-assert f -slicing-level 0 -journal-disable -then-on 'Slicing export' -print
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

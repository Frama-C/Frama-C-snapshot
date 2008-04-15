/* run.config
   OPT: -slice-print -slice-pragma g -journal-disable
   OPT: -slice-print -slice-assert g -journal-disable
   OPT: -slice-print -slice-assert main -journal-disable
   OPT: -slice-print -slice-return g -journal-disable
 */

int X1, X2 ;
void f1() {
  int x1;
  x1 = 123;
  X1 = x1 ;
}

void f2() {
  int x2;
  x2 = 12345;
  X2 = x2 ;
}

int g() {
  int y ;
  /* Note: y is not initialised by g. */
  /* Note: GCC without optimization gives X1 == y. */
  printf ("%d\n", y);
  //@slice pragma expr y ;
  //@assert X1 == y ;
  return y;
}

main() {
  int r;
  f1();
  f2();
  r = g();
  /* Note: GCC without optimization gives X2 != y. */
  //@assert X2 != r ;
}

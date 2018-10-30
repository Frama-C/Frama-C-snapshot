/* run.config*
   OPT: -no-autoload-plugins -load-module from,inout,eva,report,rtegen -rte -then -val @VALUECONFIG@ -then -report -report-print-properties
   OPT: -no-autoload-plugins -load-module from,inout,eva,report,rtegen -val @VALUECONFIG@ -then -rte -then -report -report-print-properties
*/

// Fuse with precond.c when bts #1208 is solved
int x;

/*@ requires i_plus_one: i+1 >= 0;
  requires i: i >= 0;
  assigns x; */
void f (int i) {
  x = i;
}

//@ requires x <= 8;
void g(void);

void main (int c) {
  if (c) {
    f(1);
    if(c) f(-1);
  }
  g ();g ();
}

/* run.config*
   OPT: -no-autoload-plugins -load-module from,inout,value,report,rtegen -rte -rte-precond -then -val @VALUECONFIG@ -then -report -report-print-properties
   OPT: -no-autoload-plugins -load-module from,inout,value,report,rtegen -val @VALUECONFIG@ -then -rte -rte-precond -then -report -report-print-properties
*/

// Fuse with precond.c when bts #1208 is solved
int x;

/*@ requires i+1 >= 0;
  requires i >= 0;
  assigns x; */
void f (int i) {
  x = i;
}

//@ requires x <= 8;
void g();

void main (int c) {
  if (c) {
    f(1);
    if(c) f(-1);
  }
  g ();g ();
}

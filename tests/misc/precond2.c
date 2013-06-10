/* run.config
   OPT: -load-module lib/plugins/Report -rte -rte-precond -then -val -then -report -report-print-properties
   OPT: -load-module lib/plugins/Report -val -then -rte -rte-precond -then -report -report-print-properties
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

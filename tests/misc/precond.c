/* run.config
   OPT: -load-module lib/plugins/Report -lib-entry -val -then -report -report-print-properties
*/


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
  void (*p)(int) = f;
  if (c) {
    f(1);
    if(c) f(0);
  }
  g ();
  (*p)(0);
  (*p)(-1);
}

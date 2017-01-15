/* run.config*
   OPT: -no-autoload-plugins -load-module from,inout,value,report -lib-entry -val @VALUECONFIG@ -then -report -report-print-properties -then -report-no-specialized
*/


int x;

/*@ requires i+1 >= 0;
  requires i >= 0;
  assigns x; */
void f (int i) {
  x = i;
}

//@ requires i >= 0;
void f2(int i);

void (* const pf2)(int) = f2; // const for lib-entry mode

void aux(int i) {
  pf2(i);
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

  aux(0);
  aux(c);

  (*p)(0);
  (*p)(-1);
}

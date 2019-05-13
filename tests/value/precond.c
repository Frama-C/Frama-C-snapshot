/* run.config*
   OPT: -no-autoload-plugins -load-module from,inout,eva,report -lib-entry -eva @EVA_CONFIG@ -then -report -report-print-properties -then -report-no-specialized
*/


int x;

/*@ requires i_plus_one: i+1 >= 0;
  requires i: i >= 0;
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
void g(void);

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

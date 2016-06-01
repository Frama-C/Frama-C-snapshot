/* run.config
   LOG: csv.csv
   OPT: -val -remove-redundant-alarms -then -report-csv @PTEST_RESULT@/csv.csv -report-no-proven
*/

volatile v;

void main1(int x) {
  int t[10];
  int u[15];
  x = x * x;
  u[x] = 1;
  t[u[x]] = 2;
  t[u[x]] = 3;
}

//@ requires x >= 1;
int f(int x);

void main2(int x) {
  f(x);
  f(x-1);
  f(x-2);
  f(x);
}

//@ assigns \result \from x, y;
double Frama_C_pow(double x, double y);

void main3() {
  double f1 = v;
  double f2 = v;
  double r = Frama_C_pow(f1, f2);
}

/*@ 
  requires \false;
  terminates \false; 
  assigns \nothing;
*/
void __FC_assert(const char* file,int line,const char*expr);

#define assert(e) ((e)?(void)0:__FC_assert(__FILE__,__LINE__,#e))

//@ assigns \result \from \nothing;
double any_double();

void main4() {
  double d = any_double();
  assert (d >= 1.); // Check location of alarm on non-finite float, which may be wrong because of macros
}

void main() {
  main1(v);
  main2(v);
  main3();
  main4();
}

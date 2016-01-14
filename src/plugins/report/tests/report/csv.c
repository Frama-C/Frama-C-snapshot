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

void main() {
  main1(v);
  main2(v);
}

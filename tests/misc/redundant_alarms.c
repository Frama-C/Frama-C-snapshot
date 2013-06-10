/* run.config
   OPT: -remove-redundant-alarms -print -slice-threat main1 -then-on 'Slicing export' -print
 **/

volatile int v;

void main1(int c) {
  int x, y, t;
  int *p = c ? &x : &y;
  *p = 1;
  int z = *p+1;
  int w = *p+2;
  x = t; y = t;
  x = t;
  if (v) {z = *p+2;}
}

void main2(int i) {
  int t[10];
  t[i] = 1;
  t[i] += 3;
  t[i] += 5;
}

void main() {
  if (v) main1(v);
  main2(v);
}

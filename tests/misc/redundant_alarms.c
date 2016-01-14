/* run.config
   OPT: -val-warn-copy-indeterminate main3 -scope-msg-key rm_asserts -scope-verbose 2 -remove-redundant-alarms -print -slice-threat main1 -then-on 'Slicing export' -print
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

//@ requires i < 10 && j < 10;
void main3(unsigned int i, unsigned int j) {
  int t[10];

  if (v) t[i] = v;

  if (t[i] < t[j]) {
    int tmp = t[j];
    t[j] = t[i];
    t[i] = tmp;
  }
}

void main4(int i) {
  while(1) {
    int j = 0;
    //@ assert i <= 0; // Do not prove this assertion using itself
    int k = 0;
    int z = 0;
    int w = 0;
  }
}


void main() {
  if (v) main1(v);
  main2(v);
  main3(v, v);
  if (v) main4(v);
}

/* run.config
   STDOPT: +"-print -then -val-warn-copy-indeterminate @all -no-print"
   STDOPT: #"-print -no-collapse-call-cast"
*/

extern int i;

int f () {
  return i;
}

volatile int c;

int g() {
  int x;
  if (c) x = 1;
  return x;
}

char h() {
  return 1;
}

void main1 () {

  if(c) {float f_ = f();}
  if(c) {long long v = g();}
  if(c) {
    int* x = 0;;
    int **p = &x;
    **p = h();
  }
}

float fl1 () {
  float v;
  *(char*)&v = 1;
  return v;
}

float fl2 () {
  float v;
  if (c) v = 1;
  return v;
}

void main2() {
  double d1;
  double d2;
  if (c) { d1 = fl1(); }
  d2 = fl2();
}

void main() {
  main1();
  main2();
}

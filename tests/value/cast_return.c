/* run.config*
   STDOPT: +"-eva-warn-copy-indeterminate=-@all -print -then -eva-warn-copy-indeterminate @all -no-print"
   STDOPT: #"-eva-warn-copy-indeterminate=-g,-fl1,-fl2 -print -no-collapse-call-cast"
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

//@ assigns \result \from \nothing;
float ret_float(void);

void main3() {
  float f1 = ret_float ();
  float f2 = f1 + 1;
}

void main() {
  main1();
  main2();
  main3();
}

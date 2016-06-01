int t[10];
int u[11];
volatile int maybe;
extern int c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12;

int f1_aux () {
  return 1;
}

void f1 () {
  t[c1]=f1_aux();
  c1 = c1;
}

int f2() {
  c2 = c2;
  return t[c2];
}

int f3() {
  t[c3];
  c3 = c3;
  return 0;
}

int f4() {
  if(t[c4]) {
    int x = 1;
  }
  c4 = c4;
  return 0;
}

int f5() {
  int c = t[c5];
  c5 = c5;
  return 0;
}


void f6() {
  u[c6] = t[c6];
  t[c6] = u[c6];
  c6 = c6;
}

void f7() {
  t[c7] = u[c7];
  u[c7] = t[c7];
  c7 = c7;
}

typedef struct {
  int f1;
  int f2;
} typs;

typs ts[10];
typs *p8;

int f8 () {
  p8 = &ts[c8];

  p8->f1 = 1;
  p8->f2 = 2;
  c8 = c8;
  p8 = p8;
  return 0;
}

int f9 () {
  ts[c9].f1 = 1;
  c9 = c9;
  return 0;
}


typedef int ti4[4];
typedef int ti7[7];

int ti_4[4];
int ti_7[7];

void f10 () {
  ti7* p7 = &ti_7;
  (*p7)[c10] = 10;
  c10 = c10;
}

void f11 () {
  ti4* p4 = &ti_4;
  ti7* p7 = (ti7*)p4;
  (*p7)[c11]=11;
  c11 = c11;
}

void f12 () {
  ti7* p7 = &ti_7;
  ti4* p4 = (ti4*)p7;
  (*p4)[c12]=12;
  c12 = c12;
}

extern int k1, k2, k3, k4;

void pointer_index(void)
{
  int *p = u;
  int l;
  l = p[k1];
  k1 = k1;
  p = (char*)u + 5;
  l = p[k2];
  k2 = k2;
  p = (char*)u + 11;
  l = p[k3];
  k3 = k3;
  p = u + 3;
  l = p[k4];
  k4 = k4;
}

extern int nulli;

void null_index(void) {
  *((int*)0+nulli) = 0;
}

void main () {
  f1();
  f2();
  f3();
  f4();
  f5();
  f6();
  f7();
  f8();
  f9();
  f10();
  f11();
  f12();

  pointer_index();
  if (maybe) null_index();
}

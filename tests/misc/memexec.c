/* run.config
   STDOPT: #"-memexec-all" +"-rte-select fbug -rte"
*/

int x1, y1, z1; volatile int c;

void f11() {
  x1 = 1;
}

void f1 () {
  f11();
  f11();
  f11();
  x1 = 0;
  f11();
  x1 = 1;
  f11();
  x1 = 2;
  f11();
  f11();
}

void f2 () {
}

void f3 () {
}

int *p;

int fbug() {
  return *p;
}

void bug() {
  p = 0;
  int x = 1;
  if (c)
    fbug();
  p = &x;
  fbug();
}



int i;
int t[10];

struct s {
  int i;
};

struct s* ps;

struct s S[10];

void f4_11() {  // Evaluation fails the first time, but we need ps as a dependency for the subsequent evaluations.
  t[ps->i] = 1;
}

void f4_12() {
  t[i] = 2; // Same here
  t[i-3] = 3;
}

volatile int c;

void f4_2(int *v) {
  ps = &S[8];
  ps->i = *v;
  i = *v-1;
  if (c) {
    f4_11();
  } else {
    f4_12();
  }
}

void f4() {
  int n;
  n = 12;
  if (c)
    f4_2(&n);
  if (c) {
    n = 6;
    f4_2(&n); // This call must not be cached
  }
}

int g_f5_1, g_f5_2;

void f5_aux (int x) {
  //@ assert g_f5_1 <= 6;
  int v = g_f5_2;
  //@ assert g_f5_2 <= 7;

  while (x <= 8);
}

void f5() {
  int arg;

  g_f5_1 = c;
  g_f5_2 = c;
  arg = c;
  f5_aux(arg);
  Frama_C_show_each_f5(arg, g_f5_1, g_f5_2);

  g_f5_1 = c;
  g_f5_2 = c;
  arg = c;
  f5_aux(arg);
  Frama_C_show_each_f5(arg, g_f5_1, g_f5_2); // Cache, but reduce g_f5_* and arg after the call. Currently does not work for g_f5_1, because dependencies are not taken into account
}

struct two_fields { int x; int y; } two_fields;
void f6_1() {
  two_fields.x = 1;
}
void f6() {
  two_fields.y = 2;
  f6_1();

  two_fields.y = 3;
  f6_1();
}

void f7_1(struct two_fields *p) {
  p->x = 1;
  p->y = 1;
}

void f7() {
  struct two_fields x;
  f7_1(&x);
  f7_1(&x);
}

void f8_1(int *q) {
  if (*q == 1)
    q = 0;
}

void f8() {
  int x;
  if (c) f8_1(&x);
  x = 1;
  f8_1(&x);
  f8_1(&x);
}

void main () {
  f1 ();
  f2 ();
  f3 ();
  bug();
  f4();
  f5();
  f6();
  f7();
  f8();
}

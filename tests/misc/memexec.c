/* run.config
   STDOPT:  +"-rte-select fbug -rte -memexec-all"
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


void main () {
  f1 ();
  f2 ();
  f3 ();
  bug();
  f4();
}



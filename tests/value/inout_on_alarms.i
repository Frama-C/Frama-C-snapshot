/* run.config*
   STDOPT: +"-inout"
*/

int i;
int t[10];

struct s {
  int i;
};

struct s* p;

struct s S[10];

void f() {
  t[p->i] = 1;
}

void g() {
  t[i] = 2;
  t[i-3] = 3;
}

void h(int *q) {
  if (*q == 1)
    q = 0;
}

volatile int c;

void main(int *v) {
  p = &S[8];
  p->i = 12;
  i = 12;
  if (c) {
    f();
  }
  if (c) {
    g();
  }
  if (c) {
    int z;
    h(&z);
  }
}

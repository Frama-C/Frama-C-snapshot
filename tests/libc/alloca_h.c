#include <alloca.h>

volatile int nondet;

char *f() {
  char *p = alloca(3);
  char *q = malloc(4);
  char *r = nondet ? p : q;
  free(r); // warning: q can be free'd, but not p
  return p; // p will be dangling after function exit
}

char *f2(char *allocaed) { // allocaed must contain at least 2 bytes
  *allocaed = 42;
  *(allocaed+1) = 43;
  char *p = alloca(5);
  return p;
}

char *loop() {
  char *q = 0;
  for (int i = 0; i < 100; i++) {
    char *p = alloca(i);
    q = nondet ? q : p;
  }
  return q;
}

int main() {
  int *a = alloca(sizeof(int));
  //@ assert !\initialized(a);
  *a = 42;
  char *p;
  {
    char *local = alloca(2);
    local[0] = 'A';
    local[1] = 0;
    p = local;
  }
  //@ assert !\dangling(&p);
  char *q = f();
  //@ assert \dangling(&q);
  char *r = loop();
  //@ assert \dangling(&r);
  char *in = alloca(2);
  char *s = f2(in);
  *in = 44;
  //@ assert \dangling(&s);
  return 0;
}

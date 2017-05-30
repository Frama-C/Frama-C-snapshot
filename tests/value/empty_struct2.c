#include <string.h>

volatile int nondet;

struct empty {};

struct empty global_empty;

typedef struct {
  int a;
  struct empty e;
  int b;
} comp;

typedef struct {
  struct empty s;
  int i;
} comp_begin;

typedef struct {
  char ch;
  struct empty ss;
} comp_end;

comp f(comp s, comp *p) {
  comp res;
  res.a = s.b + 10;
  res.b = s.a - 3;
  res.e = nondet ? s.e : p->e;
  return res;
}

//@ assigns \result \from \nothing;
struct empty ret_empty();

/*@ allocates \result; ensures \fresh(\result, sizeof(struct empty)); */
struct empty * ret_ptr_empty();

int main() {
  struct empty e1, e2;
  //@ assert sizeof(e1) == 0;
  e1 = global_empty;
  Frama_C_show_each_global_empty(global_empty);
  Frama_C_show_each_e1(e1);
  memcpy(&e2, &e1, sizeof(e1)); // imprecise, no builtin
  comp c1, c2;
  c1.a = 42;
  c1.b = 77;
  c1.e = e1;
  memcpy(&c2, &c1, sizeof(c1)); // imprecise, no builtin
  Frama_C_show_each_c2(c2);
  Frama_C_show_each_c2_e(c2.e);
  comp res = f(c2, &c1);
  res.e = c1.e;
  Frama_C_show_each_res(res);
  comp_begin cb = {.i = 91};
  comp_end ce = {.ch = 'Z'};
  struct empty *p = &cb.s;
  //@ assert \valid(p);
  ce.ss = *p;
  struct empty ret = ret_empty();
  struct empty * ptr_ret = ret_ptr_empty();
  struct empty copy_ptr_ret = *ptr_ret;
  return 0;
}

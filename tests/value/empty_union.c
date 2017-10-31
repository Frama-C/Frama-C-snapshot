/* run.config*
   STDOPT: +"-machdep gcc_x86_32"
 */
#include <string.h>

volatile int nondet;

union empty {};

union empty global_empty;

typedef union {
  int a;
  union empty e;
  int b;
} comp;

typedef union {
  union empty s;
  int i;
} comp_begin;

typedef union {
  char ch;
  union empty ss;
} comp_end;

comp f(comp s, comp *p) {
  comp res;
  res.a = s.b + 10;
  res.b = s.a - 3;
  res.e = nondet ? s.e : p->e;
  return res;
}

/*@ assigns *p1 \from *p2;
    ensures *(((char*)p1)+(0..\block_length(p1)-1)) ==
            *(((char*)p2)+(0..\block_length(p2)-1)); */
void copy_empty(union empty *p1, union empty *p2) {
  *p1 = *p2;
}

union empty empty_array_of_empty[0];
union empty array_of_empty[1];
//union empty many_empty[3] = {{}}; // error: cannot create init of empty union

comp array_of_comp[1] = {{.a = 17}};

int empty_int_array[0];
char empty_init_array[] = {};

int empty_initialized_array[0] = {};

// some examples from https://gcc.gnu.org/onlinedocs/gcc/Zero-Length.html
union foo { int x; int y[]; };
//union bar c = { { 1, { } } }; // error: cannot create init of empty union
union f1 {
  int x; int y[];
} f1;
union f2 {
  int data[3];
} f2 = { { 1 } };

int main() {
  int k;
  union empty e1, e2;
  //@ assert sizeof(e1) == 0;
  //@ assert \block_length(&e1) == 0;
  e1 = global_empty;
  Frama_C_show_each_global_empty(global_empty);
  Frama_C_show_each_e1(e1);
  memcpy(&e2, &e1, sizeof(e1)); // imprecise, no builtin
  comp c1, c2;
  c1.a = 42;
  c1.b = 77;
  c1.e = e1;
  memcpy(&c2, &c1, sizeof(c1));  // imprecise, no builtin
  Frama_C_show_each_c2(c2);
  Frama_C_show_each_c2_e(c2.e);
  comp res = f(c1, &c1);
  copy_empty(&e2, &e1);
  res.e = c1.e;
  Frama_C_show_each_res(res.a);
  comp_begin cb = {.i = 91};
  comp_end ce = {.ch = 'Z'};
  union empty *p = &cb.s;
  ce.ss = *p;

  e1 = array_of_empty[0];
  //e2 = many_empty[2];
  e1 = array_of_comp[0].e;
  char *pc = empty_init_array;

  return 0;
}

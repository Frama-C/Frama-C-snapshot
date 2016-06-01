/* run.config*
   STDOPT: #"-machdep gcc_x86_32"
   STDOPT:
 */
// the tests above must be done separately because both fail:
// - in gcc mode, the initialization of empty structs leads to parsing errors
// - outside of gcc mode, empty initializers ({}) lead to parsing errors
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

/*@ assigns *p1 \from *p2;
    ensures *(((char*)p1)+(0..\block_length(p1)-1)) ==
            *(((char*)p2)+(0..\block_length(p2)-1)); */
void copy_empty(struct empty *p1, struct empty *p2) {
  *p1 = *p2;
}

struct empty empty_array_of_empty[0];
struct empty array_of_empty[1];
struct empty many_empty[3] = {{}};

comp array_of_comp[1] = {{.a = 17, .b = 45, .e = {}}};

int empty_int_array[0];
char empty_init_array[] = {};

int empty_initialized_array[0] = {};

// examples from https://gcc.gnu.org/onlinedocs/gcc/Zero-Length.html
struct foo { int x; int y[]; };
//struct foo a = { 1, { 2, 3, 4, 5 } }; // sizeof(a) does not include y
//struct bar { struct foo z; };
struct bar c = { { 1, { } } }; // error expected here
struct f1 {
  int x; int y[];
} f1;// = { 1, { 2, 3, 4 } };
struct f2 {
  //struct f1 f1;
  int data[3];
} f2 = { { 1 }, { 2, 3, 4 } }; // error expected here

void gcc_zero_length_examples() {
  struct foo { int x; int y[]; };
  //struct foo a = { 1, { 2, 3, 4, 5 } };
  struct bar { struct foo z; }; // error expected here
  struct bar c = { { 1, { } } };
  struct f1 {
    int x; int y[];
  } f1;// = { 1, { 2, 3, 4 } };
  struct f2 {
    struct f1 f1; int data[3];
  } f2 = { { 1 }, { 2, 3, 4 } }; // error expected here
}

int main() {
  int k;
  struct empty e1, e2;
  //@ assert sizeof(e1) == 0;
  //@ assert \block_length(&e1) == 0;
  e1 = global_empty;
  Frama_C_show_each_global_empty(global_empty);
  Frama_C_show_each_e1(e1);
  memcpy(&e2, &e1, sizeof(e1));
  comp c1, c2;
  c1.a = 42;
  c1.b = 77;
  c1.e = e1;
  memcpy(&c2, &c1, sizeof(c1));
  Frama_C_show_each_c2(c2);
  Frama_C_show_each_c2_e(c2.e);
  comp res = f(c1, &c1);
  copy_empty(&e2, &e1);
  res.e = c1.e;
  Frama_C_show_each_res(res);
  comp_begin cb = {.i = 91};
  comp_end ce = {.ch = 'Z'};
  struct empty *p = &cb.s;
  ce.ss = *p;

  e1 = array_of_empty[0];
  e2 = many_empty[2];
  e1 = array_of_comp[0].e;
  char *pc = empty_init_array;

  gcc_zero_length_examples();

  return 0;
}

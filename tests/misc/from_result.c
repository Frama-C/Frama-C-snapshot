/* run.config
   OPT: -deps -journal-disable
*/
#define FRAMA_C_MALLOC_INFINITE
#include "../../share/libc/stdlib.c"

struct T { int a; int b; };

/*@ assigns \result.b \from x;
  @ assigns \result.a \from y; */
struct T create_t(int x, int y);

/*@ assigns \result.a \from x;
  @ assigns \result.b \from y; */
struct T create_t1(int x, int y);

int* bar (int x) {
  int* ax = (int *)malloc(sizeof(int));
  *ax = x;
  return ax;
}

void change_t(struct T* t0, int x, int y) {
  t0->a = x;
  t0->b = y;
}

int main() {
  int* t = bar(0);
  int* t1 = bar(1);
  struct T v = create_t(*t,*t1);
  struct T v1 = create_t1(*t,*t1);
  change_t(&v,0,0);
  return 0;
}

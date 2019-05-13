/* run.config
   OPT: -obfuscate
*/

int my_var = 0;

/*@ global invariant I: my_var >= 0; */

enum my_enum {
  first, second, third = 4
};

/*@ requires my_var > 0;
    ensures my_var > \old(my_var);
    ensures \forall integer x; x == x; */
int my_func () {
  enum my_enum x = first;
  /*@ assert my_var >= first; */
  my_var++;
  if (!my_var) goto end;
  return my_var + x;
 end: ;
  return -1;
}

/*@ requires \valid(p);
    ensures *p == 0;
*/
void f(int* p);

/*@ behavior bhv:
      exits never: \false;
    complete behaviors bhv;
    disjoint behaviors bhv; */
int logic(int f1)
{
  int V1;
  V1 = 0;
  if (f1) goto end;
  V1 ++;
  /*@ assert property: V1 ? 1: 0; */ ;
  end: ;
  return V1;
}

int main(int* p) { 
  if ("ti\rti" == "ti\rti") f(p); 
}

/* Obfuscate logic types and logic constructors. */
/*@ type t = T | F; */

#include "stdint.h"

/* Do not obfuscate builtins and stdlib types and functions. */
int builtin_and_stdlib () {
  int32_t x = 42;
  Frama_C_show_each(x);
  /*@ assert \true; */
  return 1;
}

/* obfuscate names of arguments of function pointers. */

typedef void (*fct_ptr)(int x, int y);

struct S { fct_ptr my_func; };

void implem(int c, int d) { };

struct S example_struct = { .my_func = implem };

void test_func(struct S* s) { s->my_func(3,4); example_struct.my_func(5,6); }

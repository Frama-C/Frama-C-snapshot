/* run.config_ci
   COMMENT: test option -e-acsl-no-valid
   DONTRUN:
   LOG: gen_@PTEST_NAME@.c
   STDOPT: #"-e-acsl-prepare -e-acsl-share ./share/e-acsl -eva -eva-verbose 0 -then -e-acsl-no-valid"
*/

#include <stdlib.h>

/*@ requires \valid(y);
  @ requires *x >= 0;
  @ ensures *x == \old(*x)+1;
  @ assigns *x \from *x,x;
  @ behavior b1:
  @   assumes *x == 1;
  @   assigns \nothing;
  @   ensures *x < 0;
  @ behavior b2:
  @   assumes *x == 0;
  @   ensures *x == 1;
  @ complete behaviors;
  @ disjoint behaviors b1, b2;
  @ */
void f(int *x, int *y) {
  /*@ requires *x >= 0;
    @ ensures 2 >= 1;
    @ assigns *x; */
  { (*x)++; }
  /*@ loop invariant 0 <= i <= 1;
    @ loop variant 2 - i; */
  for(int i = 0; i < 1; i++) /*@ assert 1 == 1; */ /*@ assert \valid(y); */ ;
}

int main(void) {
  int x = 0;
  int *y = (int *)malloc(sizeof(int));
  f(&x, y);
  return 0;
}

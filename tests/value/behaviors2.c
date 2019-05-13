/* run.config*
   STDOPT: +"-eva-verbose 2"
*/

#include <stdlib.h>

// Tests behaviors with disjunctions.

typedef enum { OK = 0, DIVBYZERO = 1, INVALID_PARAMETER = 2} res_t;

#define MAX_ERRMSG_LEN 20

/*
   Divides [dividend] by [divisor], storing the results in [quotient],
   [remainder], [sign] (the sign of the resulting quotient).
   [sign] is redundant, only used to make more complex specifications.
   [magic_code] is a useless parameter, used to add a requires clause.
   It should be different from 42.
   [errmsg] is a somewhat redundant output parameter related to the error
   message (if necessary). It is only assigned in case of error.

   Returns 0 if the division has been successful
   (e.g. divisor != 0, quotient != NULL, remainder != NULL, sign != NULL),
   a positive error code otherwise.
 */
/*@
   requires magic_code > 42 || magic_code <= 41;
   assigns  *quotient, *remainder, *sign \from dividend, divisor;
   assigns  \result, *(errmsg + (0 .. MAX_ERRMSG_LEN-1))
            \from dividend, divisor, *quotient, *remainder, *sign;

   behavior division_by_zero:
     assumes  divisor == 0;
     requires \valid(errmsg + (0 .. MAX_ERRMSG_LEN-1));
     assigns  *(errmsg + (0 .. MAX_ERRMSG_LEN-1)) \from \nothing;
     assigns  \result \from \nothing;
     ensures  \result == DIVBYZERO;

   behavior invalid_parameter:
     assumes  !\valid(quotient) || !\valid(remainder) || !\valid(sign);
     requires \valid(errmsg + (0 .. MAX_ERRMSG_LEN-1));
     assigns  *(errmsg + (0 .. MAX_ERRMSG_LEN-1)) \from \nothing;
     assigns  \result \from \nothing;
     ensures  \result == INVALID_PARAMETER;

   behavior error:
     assumes  divisor == 0 || !\valid(quotient)
              || !\valid(remainder) || !\valid(sign);
     requires \valid(errmsg + (0 .. MAX_ERRMSG_LEN-1));
     assigns  *(errmsg + (0 .. MAX_ERRMSG_LEN-1)) \from \nothing;
     assigns  \result \from \nothing;
     ensures  \result == DIVBYZERO || \result == INVALID_PARAMETER;

   behavior ok:
     assumes  \valid(quotient) && \valid(remainder) && \valid(sign);
     assumes  divisor < 0 || divisor > 0;
     assigns  *quotient, *remainder, *sign \from dividend, divisor;
     assigns  \result \from \nothing;
     ensures  \result == OK;
     ensures  \initialized(sign);
     ensures -1 <= *sign <= 1;

   behavior ok_res_sign_nonnegative:
     assumes  \valid(quotient) && \valid(remainder) && \valid(sign);
     assumes  dividend >= 0 && divisor > 0 || dividend <= 0 && divisor < 0;
     assigns  *quotient, *remainder, *sign \from dividend, divisor;
     assigns  \result \from \nothing;
     ensures  \result == OK;
     ensures  \initialized(quotient);
     ensures  *quotient == dividend / divisor;
     ensures  \initialized(remainder);
     ensures  *remainder == dividend % divisor;
     ensures  \initialized(sign);
     ensures  *sign == 1 || *sign == 0;

   behavior ok_res_sign_nonpositive:
     assumes  \valid(quotient) && \valid(remainder) && \valid(sign);
     assumes  dividend >= 0 && divisor < 0 || dividend <= 0 && divisor > 0;
     assigns  *quotient, *remainder, *sign \from dividend, divisor;
     assigns  \result \from \nothing;
     ensures  \result == OK;
     ensures  \initialized(quotient);
     ensures  *quotient == dividend / divisor;
     ensures  \initialized(remainder);
     ensures  *remainder == dividend % divisor;
     ensures  \initialized(sign);
     ensures  *sign == -1 || *sign == 0;

   behavior ok_quotient_zero:
     assumes  \valid(quotient) && \valid(remainder) && \valid(sign);
     assumes  divisor < 0 || divisor > 0; // redundant
     assumes  dividend >= 0 && dividend < divisor
              || dividend < 0 && dividend > divisor;
     assigns  *quotient, *remainder, *sign \from dividend, divisor;
     assigns  \result \from \nothing;
     ensures  \result == OK;
     ensures  \initialized(quotient);
     ensures  *quotient == 0;
     ensures  \initialized(remainder);
     ensures  *remainder == dividend % divisor;
     ensures  \initialized(sign);
     ensures  *sign == 0;

   behavior ok_quotient_non_zero:
     assumes  \valid(quotient) && \valid(remainder) && \valid(sign);
     assumes  divisor < 0 || divisor > 0;
     assumes  dividend > 0 && dividend >= divisor
              || dividend < 0 && dividend < divisor;
     assigns  *quotient, *remainder, *sign \from dividend, divisor;
     assigns  \result \from \nothing;
     ensures  \result == OK;
     ensures  \initialized(quotient);
     ensures  *quotient == dividend / divisor;
     ensures  \initialized(remainder);
     ensures  *remainder == dividend % divisor;
     ensures  \initialized(sign);
     ensures  *sign >= -1 && *sign <= 1;

  complete behaviors error, ok;
  disjoint behaviors error, ok;

  complete behaviors division_by_zero, invalid_parameter, ok;

  complete behaviors division_by_zero, invalid_parameter,
    ok_res_sign_nonnegative, ok_res_sign_nonpositive;

  complete behaviors division_by_zero, invalid_parameter,
    ok_quotient_zero, ok_quotient_non_zero;

  complete behaviors;

  disjoint behaviors ok_quotient_zero, ok_quotient_non_zero;
  disjoint behaviors ok_res_sign_nonpositive, error;
  disjoint behaviors ok_res_sign_nonnegative, error;
 */
res_t divi(int dividend, int divisor, int *quotient, int *remainder, int *sign,
	   unsigned int magic_code, char *errmsg);
/*
void mystrcpy(char *dst, char *src) {
  while (*src) {
    *dst = *src;
    dst++;
    src++;
  }
}

res_t divi(int dividend, int divisor, int *quotient, int *remainder, int *sign,
           unsigned int magic_code, char *errmsg) {
  if (divisor == 0) {
    mystrcpy(errmsg, "division by zero");
    return DIVBYZERO;
  }
  if (!quotient) {
    mystrcpy(errmsg, "invalid quotient");
    return INVALID_PARAMETER;
  }
  if (!remainder) {
    mystrcpy(errmsg, "invalid remainder");
    return INVALID_PARAMETER;
  }
  if (!sign) {
    mystrcpy(errmsg, "invalid sign");
    return INVALID_PARAMETER;
  }
  *quotient = dividend / divisor;
  *remainder = dividend % divisor;
  *sign = *quotient > 0 ? 1 : (*quotient < 0 ? -1 : 0);
  return OK;
} */

volatile int nondet;

void test1() {
  int quotient, remainder, sign;
  char errmsg[MAX_ERRMSG_LEN];
  res_t r1, r2, r3, r4, r5, r6, r7, r8;

  r1 = divi(60, 3, &quotient, &remainder, &sign, 41, errmsg);

  r2 = divi(1, 3, &quotient, &remainder, &sign, 43, errmsg);

  r3 = divi(-2, -5, &quotient, &remainder, &sign, 41, errmsg);

  r4 = divi(1, 0, &quotient, &remainder, &sign, 43, errmsg);

  r5 = divi(1, 1, NULL, &remainder, &sign, 0, errmsg);

  r6 = divi(1, 1, &quotient, NULL, &sign, 0, errmsg);

  r7 = divi(1, 1, &quotient, &remainder, NULL, 0, errmsg);

  r8 = divi(nondet, nondet, &quotient, &remainder, &sign, 0, errmsg);

}

/*@
  requires x == 2 || x == 4;
  assigns \result \from p1, p2;
  assigns *y \from x;
  ensures \initialized(\result);

  behavior b1:
    assumes x == 2;
    requires \initialized(p1);
    requires \valid(y);
    assigns \result \from p1;
    assigns *y \from \nothing;
    ensures *y == x;
    ensures \result == p1;

  behavior b2:
    assumes x == 4;
    requires \initialized(p2);
    requires \valid(y);
    assigns \result \from p2;
    assigns *y \from \nothing;
    ensures *y == -x;
    ensures \result == p2;

  complete behaviors;
 */
int *f1(int x, int *y, int *p1, int *p2);

void test2() {
  int a = 1, b = 2;
  int *p;
  int y = 42;
  int x = nondet;
  p = f1(x, &y, &a, &b);
}

/*@
  requires x == 2 || x == 4;
  assigns \result \from p1, p2;
  assigns *y \from x;
  ensures \initialized(\result);

  behavior b1:
    assumes x == 2;
    requires \initialized(p1);
    requires \valid(y);
    assigns \result \from p1;
    assigns *y \from \nothing;
    ensures *y == x;
    ensures \result == p1;

  behavior b2:
    assumes x == 4;
    requires \initialized(p2);
    requires \valid(y);
    assigns \result \from p2;
    assigns *y \from \nothing;
    ensures *y == -x;
    ensures \result == p2;

  complete behaviors;
 */
int *f2(int x, int *y, int *p1, int *p2);

void test3() {
  int a = 1, b = 2;
  int *p;
  int y = 42;
  int x = nondet;
  p = f2(x, &y, &a, &b);
}

/*@
  requires x == 2 || x == 4;
  assigns \result \from x;
  behavior b1:
    assumes x == 2;
    ensures \false;
  behavior b2:
    assumes x == 4;
    ensures \result == 1;
  behavior b3:
    assumes x == 6;
    ensures \result == 1;
 */
int f3(int x);

void test4() {
  int x = nondet;
  f3(x);
}

/*@
  requires x == 2 || x == 4;
  assigns \result \from x;
  behavior b1:
    assumes x == 2;
    ensures \false;
  behavior b2:
    assumes x == 4;
    ensures \result == 1;
  behavior b3:
    assumes x == 6;
    ensures \result == 1;
  complete behaviors;
 */
int f4(int x);

void test5() {
  int x = nondet;
  f4(x);
}

// test below not directly related to behaviors; tests the precision of
// logic reductions
/*@
  assigns p == \null ? \empty : *p \from indirect:p;
  ensures p == \null || *p == 1; // if p non-null, then *p must equal 1;
 */
void opt_ptr(int *p);

void test_red() {
  int a = 2;
  opt_ptr(&a);
  //@ assert a == 1;
}

int main() {
  int quotient, remainder, sign;
  char errmsg[MAX_ERRMSG_LEN];
  int r;

  test1();
  test2();
  test3();
  test4();
  test5();
  test_red();

  return 0;
}

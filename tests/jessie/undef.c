/***************************************************************
 *
 * This file is a collection of simple test cases for modular C
 * checking and verification tools.  The goal is to exercise support
 * for guarding against undefined and unspecified behavior in C99
 * programs.  Many of these behaviors are routinely overlooked by
 * "sound" tools.  We omit many kind of undefined behavior that can
 * easily be detected syntactically. We are not interested in testing
 * tool support for avoiding reliance on implementation-defined
 * behavior.
 *
 * Will Archer <warcher@cs.utah.edu>
 * John Regehr <regehr@cs.utah.edu>
 *
 ***************************************************************/

#include "../../share/jessie/jessie_machine_prolog.h"

/***************************************************************/

/* precondition \false */
int
div0_wrong (int x, int y)
{
  return x / (y * 0);
}

/*@ requires y != 0; */
int
div0_unsafe (int x, int y)
{
  return x / y;
}

/*@ requires y > 0; */
int
div0_unsafe_pos (int x, int y)
{
  return x / y;
}

/*@ requires \true; */
int
div0_safe (int x, int y)
{
  if (y == 0)
    {
      return 0;
    }
  else
    {
      return x / y;
    }
}

/*@ requires \true; */
int
div0_safe_pos (int x, int y)
{
  if (y <= 0)
    {
      return 0;
    }
  else
    {
      return x / y;
    }
}

/* precondition \false */
unsigned
udiv0_wrong (unsigned x, unsigned y)
{
  return x / (y * 0);
}

/*@ requires y != 0; */
unsigned
udiv0_unsafe (unsigned x, unsigned y)
{
  return x / y;
}

/*@ requires \true; */
unsigned
udiv0_safe (unsigned x, unsigned y)
{
  if (y == 0)
    {
      return 0;
    }
  else
    {
      return x / y;
    }
}

/***************************************************************/

/* precondition \false */
int
mod0_wrong (int x, int y)
{
  return x % (y * 0);
}

/*@ requires y != 0; */
int
mod0_unsafe (int x, int y)
{
  return x % y;
}

/*@ requires \true; */
int
mod0_safe (int x, int y)
{
  if (y == 0)
    {
      return 0;
    }
  else
    {
      return x % y;
    }
}

/***************************************************************/

/* precondition \false */
int
shift_left_signed_wrong_1 (int x, int y)
{
  return x << (sizeof (int) * CHAR_BIT);
}

/* precondition \false */
int
shift_left_signed_wrong_2 (int x, int y)
{
  return x << -1;
}

/* precondition \false */
int
shift_left_signed_wrong_3 (int x, int y)
{
  return (INT_MAX >> 4) << 6;
}

/* SIZEOF NOT YET SUPPORTED IN LOGIC
 requires x >= 0 &&
                 0 =< y < (sizeof (int) * CHAR_BIT) &&
                 x <= (INT_MAX >> y); */
int
shift_left_signed_unsafe (int x, int y)
{
  return x << y;
}

/*@ requires \true; */
int
shift_left_signed_safe (int x, int y)
{
  if ((x < 0) ||
      (y < 0) || (y >= sizeof (int) * CHAR_BIT) || (x > (INT_MAX >> y)))
    {
      return 0;
    }
  else
    {
      return x << y;
    }
  /* unreachable */
  return 0;
}

/***************************************************************/

/* precondition \false */
unsigned
shift_left_unsigned_wrong (unsigned x, unsigned y)
{
  return x << sizeof (unsigned) * CHAR_BIT;
}

/* SIZEOF NOT YET SUPPORTED IN LOGIC
 requires y < sizeof(unsigned)*CHAR_BIT; */
unsigned
shift_left_unsigned_unsafe (unsigned x, unsigned y)
{
  return x << y;
}

/*@ requires \true; */
unsigned
shift_left_unsigned_safe (unsigned x, unsigned y)
{
  if (y >= sizeof (unsigned) * CHAR_BIT)
    {
      return 0;
    }
  else
    {
      return x << y;
    }
}

/***************************************************************/

/*
 * N.B. it is implementation defined what happens when a signed int
 * with negative value is right shifted
 */

/* precondition \false */
int
shift_right_signed_wrong_1 (int x, int y)
{
  return x >> -1;
}

/* precondition \false */
int
shift_right_signed_wrong_2 (int x, int y)
{
  return x >> sizeof (int) * CHAR_BIT;
}

/* SIZEOF NOT YET SUPPORTED IN LOGIC
 requires y >= 0 && y < sizeof(int)*CHAR_BIT; */
int
shift_right_signed_unsafe (int x, int y)
{
  return x >> y;
}

/*@ requires \true; */
int
shift_right_signed_safe (int x, int y)
{
  if ((y < 0) || (y >= sizeof (int) * CHAR_BIT))
    {
      return 0;
    }
  else
    {
      return x >> y;
    }
  /* unreachable */
  return 0;
}

/***************************************************************/

/* precondition \false */
unsigned
shift_right_unsigned_wrong (unsigned x, unsigned y)
{
  return x >> sizeof (unsigned) * CHAR_BIT;
}

/* SIZEOF NOT YET SUPPORTED IN LOGIC
 requires y < sizeof(unsigned)*CHAR_BIT; */
unsigned
shift_right_unsigned_unsafe (unsigned x, unsigned y)
{
  return x >> y;
}

/*@ requires \true; */
unsigned
shift_right_unsigned_safe (unsigned x, unsigned y)
{
  if (y >= sizeof (unsigned) * CHAR_BIT)
    {
      return 0;
    }
  else
    {
      return x >> y;
    }
}

/***************************************************************/

/* precondition \false */
int
null_ptr_wrong (int *x)
{
  x = NULL;
  return *x;
}

/*@ requires \valid(x); */
int
null_ptr_unsafe (int *x)
{
  return *x;
}

/*@ requires \valid(x) || x == NULL; */
int
null_ptr_safe (int *x)
{
  if (x == NULL)
    {
      return 0;
    }
  else
    {
      return *x;
    }
}

/***************************************************************/

/*@ ensures \valid(\result); */
int *
bad_local_pointer (void)
{
  int x;
  return &x;
}

/* precondition \false */
int
stack_access_wrong (void)
{
  int *x = bad_local_pointer ();
  return *x;
}

/***************************************************************/
/* function pointer miscast
   (must abuse union to bypass compiler
   checks)
*/

/* typedef union */
/* { */
/*   int (*int_op) (int x); */
/*   float (*float_op) (float x); */
/* } fptr; */

/* int */
/* iop (int x) */
/* { */
/*   return x + 1; */
/* } */

/* float */
/* fop (float x) */
/* { */
/*   return x + 1.0; */
/* } */

/* /\* precondition \false *\/ */
/* int */
/* fun_p_wrong (void) */
/* { */
/*   fptr fp; */

/*   fp.float_op = fop; */
/*   return fp.int_op (1); */
/* } */

/***************************************************************/

/* precondition \false */
int
ptr_sub_wrong (void)
{
  int a[20];
  int b[10];
  int *x = &a[2];
  int *y = &b[0];

  return (x - y);
}

/*@ requires \valid (x) && \valid (y);
  @ // and x and y must point into the same array object
  @*/
int
ptr_sub_unsafe (int *x, int *y)
{
  return (x - y);
}

/*@ requires \true; */
int
ptr_sub_safe (void)
{
  int a[3];
  int *x = &a[2];
  int *y = &a[0];

  return (x - y);
}

/***************************************************************/
/* pointer past array bounds */

int a[2];

int
oob_pointer_wrong (unsigned x)
{
  int *i = a + x + 2;
  return *i;
}

/*@ requires 0 <= x < 2; */
int
oob_pointer_unsafe (unsigned x)
{
  int *i = a + x;
  return *i;
}

int
oob_pointer_safe (unsigned x)
{
  int *i;
  if (x < 2)
    {
      i = a + x;
      return *i;
    }
  else
    {
      return 0;
    }
}

/***************************************************************/

/* precondition \false */
int
array_bounds_wrong (unsigned i)
{
  return a[2 + i];
}

/*@ requires i < 2; */
int
array_bounds_unsafe (unsigned i)
{
  return a[i];
}

/*@ requires \true; */
int
array_bounds_safe (unsigned i)
{
  if (i < 2)
    {
      return a[i];
    }
  else
    {
      return a[0];
    }
}

/***************************************************************/

/* precondition \false */
int
initialization_wrong (void)
{
  int x;
  return x;
}

/*@ requires i != 0; */
int
initialization_unsafe (int i)
{
  int x;
  if (i)
    x = 1;
  return x;
}

/*@ requires \true; */
int
initialization_safe (int i)
{
  int x;
  if (i)
    {
      x = 1;
    }
  else
    {
      x = 2;
    }
  return x;
}

/***************************************************************/

/* precondition \false */
int
signed_overflow_wrong (int x)
{
  if (x > 0)
    {
      return x + INT_MAX;
    }
  else
    {
      return x - 1 - INT_MAX;
    }
}

/*@ requires x <= INT_MAX-10; */
int
signed_overflow_unsafe (int x)
{
  return x + 10;
}

/*@ requires \true; */
int
signed_overflow_safe (int x)
{
  if (x < 0)
    {
      return x + INT_MAX;
    }
  else
    {
      return x - INT_MAX;
    }
}

/***************************************************************/

/* precondition \false */
int
missing_return_wrong (int x)
{
}

/*@ requires x < 0; */
int
missing_return_unsafe (int x)
{
  if (x < 0)
    return 0;
}

/*@ requires \true; */
int
missing_return_safe (int x)
{
  if (x < 0)
    {
      return 0;
    }
  else
    {
      return 1;
    }
}

/***************************************************************/

/* precondition \false */
int
multiple_update_wrong_1 (int *x, int *y)
{
  return (*x = 0) + (*x = 0);
}

/* precondition \false */
int
multiple_update_wrong_2 (int i)
{
  i = ++i + 1;
  return i;
}

/* precondition \false */
int
multiple_update_wrong_3 (int i)
{
  a[i++] = i;
  return i;
}

/*@ requires x != y; */
int
multiple_update_unsafe (int *x, int *y)
{
  return (*x = 0) + (*y = 0);
}

/*@ requires \true; */
int
multiple_update_safe (int *x, int *y)
{
  if (x == y)
    {
      return 0;
    }
  else
    {
      return (*x = 0) + (*y = 0);
    }
}

/***************************************************************/

extern int foo (int, int);
extern int bar (void);
extern int bax (void);

/* precondition:
     read set of bar and write set of baz do not overlap &&
     read set of baz and write set of bar do not overlap && */
int
order_of_arg_eval (void)
{
  return foo (bar (), baz ());
}

/***************************************************************/
/* cast away volatile */

/* precondition \false */
int
volatile_cast_wrong (volatile int *ip)
{
  int *x = (int *) ip;
  return *x;
}

/***************************************************************/
/* cast away const */

/* precondition \false */
int
const_cast_wrong (const int *ip)
{
  int *x = (int *) ip;
  *x = 1;
  return 1;
}

/***************************************************************/
/* modify a string literal */

void
mod_string (void)
{
  char *str = "abc";
  str[2] = 'z';
  return;
}

/***************************************************************/

/*
Local Variables:
compile-command: "LC_ALL=C make undef"
End:
*/

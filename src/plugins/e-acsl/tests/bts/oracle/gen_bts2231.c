/* Generated by Frama-C */
#include "stdio.h"
#include "stdlib.h"
long A = (long)0;
int main(void)
{
  int __retres;
  __e_acsl_memory_init((int *)0,(char ***)0,(size_t)8);
  /*@ assert A + (long)((long)(3 * A) - 1) ≡ -1; */
  {
    __e_acsl_mpz_t __gen_e_acsl_A;
    __e_acsl_mpz_t __gen_e_acsl_;
    __e_acsl_mpz_t __gen_e_acsl_mul;
    long __gen_e_acsl__2;
    __e_acsl_mpz_t __gen_e_acsl__3;
    __e_acsl_mpz_t __gen_e_acsl_add;
    __e_acsl_mpz_t __gen_e_acsl__4;
    int __gen_e_acsl_eq;
    __gmpz_init_set_si(__gen_e_acsl_A,A);
    __gmpz_init_set_si(__gen_e_acsl_,3L);
    __gmpz_init(__gen_e_acsl_mul);
    __gmpz_mul(__gen_e_acsl_mul,(__e_acsl_mpz_struct const *)(__gen_e_acsl_),
               (__e_acsl_mpz_struct const *)(__gen_e_acsl_A));
    __gen_e_acsl__2 = __gmpz_get_si((__e_acsl_mpz_struct const *)(__gen_e_acsl_mul));
    /*@ assert
        Eva: signed_overflow: -9223372036854775808 ≤ __gen_e_acsl__2 - 1;
    */
    __gmpz_init_set_si(__gen_e_acsl__3,__gen_e_acsl__2 - 1L);
    __gmpz_init(__gen_e_acsl_add);
    __gmpz_add(__gen_e_acsl_add,
               (__e_acsl_mpz_struct const *)(__gen_e_acsl_A),
               (__e_acsl_mpz_struct const *)(__gen_e_acsl__3));
    __gmpz_init_set_si(__gen_e_acsl__4,(long)(-1));
    __gen_e_acsl_eq = __gmpz_cmp((__e_acsl_mpz_struct const *)(__gen_e_acsl_add),
                                 (__e_acsl_mpz_struct const *)(__gen_e_acsl__4));
    __e_acsl_assert(__gen_e_acsl_eq == 0,(char *)"Assertion",(char *)"main",
                    (char *)"A + (long)((long)(3 * A) - 1) == -1",8);
    __gmpz_clear(__gen_e_acsl_A);
    __gmpz_clear(__gen_e_acsl_);
    __gmpz_clear(__gen_e_acsl_mul);
    __gmpz_clear(__gen_e_acsl__3);
    __gmpz_clear(__gen_e_acsl_add);
    __gmpz_clear(__gen_e_acsl__4);
  }
  __retres = 0;
  return __retres;
}


/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2018                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  you can redistribute it and/or modify it under the terms of the GNU   */
/*  Lesser General Public License as published by the Free Software       */
/*  Foundation, version 2.1.                                              */
/*                                                                        */
/*  It is distributed in the hope that it will be useful,                 */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */
/*  GNU Lesser General Public License for more details.                   */
/*                                                                        */
/*  See the GNU Lesser General Public License version 2.1                 */
/*  for more details (enclosed in the file licenses/LGPLv2.1).            */
/*                                                                        */
/**************************************************************************/

/* ISO C: 7.12 */
#ifndef __FC_MATH
#define __FC_MATH
#include "features.h"
__PUSH_FC_STDLIB

#include "__fc_string_axiomatic.h"
#include "errno.h"

__BEGIN_DECLS

typedef float float_t;
typedef double double_t;

#define MATH_ERRNO	1
#define MATH_ERREXCEPT	2

#define HUGE_VAL  0x1.0p2047
#define HUGE_VALF 0x1.0p255f
#define HUGE_VALL 0x1.0p32767L

/* The constants below are not part of C99/C11 but they are defined in POSIX */
#ifdef _XOPEN_SOURCE
# define M_E 0x1.5bf0a8b145769p1         /* e          */
# define M_LOG2E 0x1.71547652b82fep0     /* log_2 e    */
# define M_LOG10E 0x1.bcb7b1526e50ep-2   /* log_10 e   */
# define M_LN2 0x1.62e42fefa39efp-1      /* log_e 2    */
# define M_LN10 0x1.26bb1bbb55516p1      /* log_e 10   */
# define M_PI 0x1.921fb54442d18p1        /* pi         */
# define M_PI_2 0x1.921fb54442d18p0      /* pi/2       */
# define M_PI_4 0x1.921fb54442d18p-1     /* pi/4       */
# define M_1_PI 0x1.45f306dc9c883p-2     /* 1/pi       */
# define M_2_PI 0x1.45f306dc9c883p-1     /* 2/pi       */
# define M_2_SQRTPI 0x1.20dd750429b6dp0  /* 2/sqrt(pi) */
# define M_SQRT2 0x1.6a09e667f3bcdp0     /* sqrt(2)    */
# define M_SQRT1_2 0x1.6a09e667f3bcdp-1  /* 1/sqrt(2)  */
#endif

/* The following specifications will set errno. */
#define math_errhandling	MATH_ERRNO

#define FP_NAN 0
#define FP_INFINITE 1
#define FP_ZERO 2
#define FP_SUBNORMAL 3
#define FP_NORMAL 4

#include <float.h> // for DBL_MIN and FLT_MIN

/*@
  assigns \result \from x;
  behavior nan:
    assumes is_nan: \is_NaN(x);
    ensures fp_nan: \result == FP_NAN;
  behavior inf:
    assumes is_infinite: !\is_NaN(x) && !\is_finite(x);
    ensures fp_infinite: \result == FP_INFINITE;
  behavior zero:
    assumes is_a_zero: x == 0.0; // also includes -0.0
    ensures fp_zero: \result == FP_ZERO;
  behavior subnormal:
    assumes is_finite: \is_finite(x);
    assumes is_subnormal: (x > 0.0 && x < FLT_MIN) || (x < 0.0 && x > -FLT_MIN);
    ensures fp_subnormal: \result == FP_SUBNORMAL;
  behavior normal:
    assumes is_finite: \is_finite(x);
    assumes not_subnormal: (x <= -FLT_MIN || x >= FLT_MIN);
    ensures fp_normal: \result == FP_NORMAL;
  complete behaviors;
  disjoint behaviors;
 */
int __fc_fpclassifyf(float x);

/*@
  assigns \result \from x;
  behavior nan:
    assumes is_nan: \is_NaN(x);
    ensures fp_nan: \result == FP_NAN;
  behavior inf:
    assumes is_infinite: !\is_NaN(x) && !\is_finite(x);
    ensures fp_infinite: \result == FP_INFINITE;
  behavior zero:
    assumes is_a_zero: x == 0.0; // also includes -0.0
    ensures fp_zero: \result == FP_ZERO;
  behavior subnormal:
    assumes is_finite: \is_finite(x);
    assumes is_subnormal: (x > 0.0 && x < DBL_MIN) || (x < 0.0 && x > -DBL_MIN);
    ensures fp_subnormal: \result == FP_SUBNORMAL;
  behavior normal:
    assumes is_finite: \is_finite(x);
    assumes not_subnormal: (x <= -DBL_MIN || x >= DBL_MIN);
    ensures fp_normal: \result == FP_NORMAL;
  complete behaviors;
  disjoint behaviors;
 */
int __fc_fpclassify(double x);

// Incorrect in presence of long double with >64 bits
#define fpclassify(x) \
  (sizeof(x) == sizeof(float) ? __fc_fpclassifyf(x) : __fc_fpclassify(x))

#define isinf(x) \
  (sizeof(x) == sizeof(float) ? __fc_fpclassifyf(x) == FP_INFINITE : __fc_fpclassify(x) == FP_INFINITE)

#define isnan(x) \
  (sizeof(x) == sizeof(float) ? __fc_fpclassifyf(x) == FP_NAN : __fc_fpclassify(x) == FP_NAN)

#define isnormal(x) \
  (sizeof(x) == sizeof(float) ? __fc_fpclassifyf(x) == FP_NORMAL : __fc_fpclassify(x) == FP_NORMAL)

/*@
  assigns __fc_errno, \result \from x;
  behavior normal:
    assumes in_domain: \is_finite(x) && \abs(x) <= 1;
    assigns \result \from x;
    ensures positive_result: \is_finite(\result) && \result >= 0;
  behavior domain_error:
    assumes out_of_domain: \is_infinite(x) || (\is_finite(x) && \abs(x) > 1);
    assigns __fc_errno, \result \from x;
    ensures errno_set: __fc_errno == 1;
  disjoint behaviors;
 */
extern double acos(double x);

/*@
  assigns __fc_errno, \result \from x;
  behavior normal:
    assumes in_domain: \is_finite(x) && \abs(x) <= 1;
    assigns \result \from x;
    ensures positive_result: \is_finite(\result) && \result >= 0;
  behavior domain_error:
    assumes out_of_domain: \is_infinite(x) || (\is_finite(x) && \abs(x) > 1);
    assigns __fc_errno, \result \from x;
    ensures errno_set: __fc_errno == 1;
  disjoint behaviors;
 */
extern float acosf(float x);

/*@
  assigns __fc_errno, \result \from x;
  behavior normal:
    assumes in_domain: \is_finite(x) && \abs(x) <= 1;
    assigns \result \from x;
    ensures positive_result: \is_finite(\result) && \result >= 0;
  behavior domain_error:
    assumes out_of_domain: \is_infinite(x) || (\is_finite(x) && \abs(x) > 1);
    assigns __fc_errno, \result \from x;
    ensures errno_set: __fc_errno == 1;
  disjoint behaviors;
 */
extern long double acosl(long double x);

/*@
  assigns __fc_errno, \result \from x;
  behavior normal:
    assumes in_domain: \is_finite(x) && \abs(x) <= 1;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
  behavior domain_error:
    assumes out_of_domain: \is_infinite(x) || (\is_finite(x) && \abs(x) > 1);
    assigns __fc_errno, \result \from x;
    ensures errno_set: __fc_errno == 1;
  disjoint behaviors;
 */
extern double asin(double x);

/*@
  assigns __fc_errno, \result \from x;
  behavior normal:
    assumes in_domain: \is_finite(x) && \abs(x) <= 1;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
  behavior domain_error:
    assumes out_of_domain: \is_infinite(x) || (\is_finite(x) && \abs(x) > 1);
    assigns __fc_errno, \result \from x;
    ensures errno_set: __fc_errno == 1;
  disjoint behaviors;
 */
extern float asinf(float x);

/*@
  assigns __fc_errno, \result \from x;
  behavior normal:
    assumes in_domain: \is_finite(x) && \abs(x) <= 1;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
  behavior domain_error:
    assumes out_of_domain: \is_infinite(x) || (\is_finite(x) && \abs(x) > 1);
    assigns __fc_errno, \result \from x;
    ensures errno_set: __fc_errno == 1;
  disjoint behaviors;
 */
extern long double asinl(long double x);

/*@ requires finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures result_domain: -1.571 <= \result <= 1.571;
*/
extern float atanf(float x);

/*@ requires finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures result_domain: -1.571 <= \result <= 1.571;
*/
extern double atan(double x);

/*@ requires finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures result_domain: -1.571 <= \result <= 1.571;
*/
extern long double atanl(long double x);

/*@ requires finite_args: \is_finite(x) && \is_finite(y);
    requires finite_result: \is_finite(atan2(x, y));
    assigns \result \from x, y;
    ensures finite_result: \is_finite(\result);
*/
extern double atan2(double y, double x);

/*@ requires finite_args: \is_finite(x) && \is_finite(y);
    requires finite_logic_result: \is_finite(atan2f(x, y));
    assigns \result \from x, y;
    ensures finite_result: \is_finite(\result);
*/
extern float atan2f(float y, float x);

extern long double atan2l(long double y, long double x);

/*@ requires finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures result_domain: -1. <= \result <= 1.;
*/
extern double cos(double x);

/*@ requires finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures result_domain: -1. <= \result <= 1.;
*/
extern float cosf(float x);

/*@ requires finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures result_domain: -1. <= \result <= 1.;
*/
extern long double cosl(long double x);

/*@ requires finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures result_domain: -1. <= \result <= 1.;
*/
extern double sin(double x);

/*@ requires finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures result_domain: -1. <= \result <= 1.;
*/
extern float sinf(float x);

/*@ requires finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures result_domain: -1. <= \result <= 1.;
*/
extern long double sinl(long double x);

extern double tan(double x);
extern float tanf(float x);
extern long double tanl(long double x);

/*@
  assigns __fc_errno, \result \from x;
  behavior normal:
    assumes in_domain: \is_finite(x) && x >= 1;
    assigns \result \from x;
    ensures positive_result: \is_finite(\result) && \result >= 0;
  behavior infinite:
    assumes is_plus_infinity: \is_plus_infinity(x);
    assigns \result \from x;
    ensures result_plus_infinity: \is_plus_infinity(\result);
  behavior domain_error:
    assumes out_of_domain: \is_minus_infinity(x) || (\is_finite(x) && x < 1);
    assigns __fc_errno, \result \from x;
    ensures errno_set: __fc_errno == 1;
  disjoint behaviors;
 */
extern double acosh(double x);

/*@
  assigns __fc_errno, \result \from x;
  behavior normal:
    assumes in_domain: \is_finite(x) && x >= 1;
    assigns \result \from x;
    ensures positive_result: \is_finite(\result) && \result >= 0;
  behavior infinite:
    assumes is_plus_infinity: \is_plus_infinity(x);
    assigns \result \from x;
    ensures result_plus_infinity: \is_plus_infinity(\result);
  behavior domain_error:
    assumes out_of_domain: \is_minus_infinity(x) || (\is_finite(x) && x < 1);
    assigns __fc_errno, \result \from x;
    ensures errno_set: __fc_errno == 1;
  disjoint behaviors;
 */
extern float acoshf(float x);

/*@
  assigns __fc_errno, \result \from x;
  behavior normal:
    assumes in_domain: \is_finite(x) && x >= 1;
    assigns \result \from x;
    ensures positive_result: \is_finite(\result) && \result >= 0;
  behavior infinite:
    assumes is_plus_infinity: \is_plus_infinity(x);
    assigns \result \from x;
    ensures result_plus_infinity: \is_plus_infinity(\result);
  behavior domain_error:
    assumes out_of_domain: \is_minus_infinity(x) || (\is_finite(x) && x < 1);
    assigns __fc_errno, \result \from x;
    ensures errno_set: __fc_errno == 1;
  disjoint behaviors;
 */
extern long double acoshl(long double x);

extern double asinh(double x);
extern float asinhf(float x);
extern long double asinhl(long double x);

extern double atanh(double x);
extern float atanhf(float x);
extern long double atanhl(long double x);

extern double cosh(double x);
extern float coshf(float x);
extern long double coshl(long double x);

extern double sinh(double x);
extern float sinhf(float x);
extern long double sinhl(long double x);

extern double tanh(double x);
extern float tanhf(float x);
extern long double tanhl(long double x);

/*@ requires finite_arg: \is_finite(x);
    requires finite_domain: x <= 0x1.62e42fefa39efp+9;
    assigns \result \from x;
    ensures res_finite: \is_finite(\result);
    ensures positive_result: \result > 0.;
*/
extern double exp(double x);

/*@ requires finite_arg: \is_finite(x);
    requires res_finite: x <= 0x1.62e42ep+6;
    assigns \result \from x;
    ensures res_finite: \is_finite(\result);
    ensures positive_result: \result > 0.;
*/
extern float expf(float x);

extern long double expl(long double x);

extern double exp2(double x);
extern float exp2f(float x);
extern long double exp2l(long double x);

extern double expm1(double x);
extern float expm1f(float x);
extern long double expm1l(long double x);

extern double frexp(double value, int *exp);
extern float frexpf(float value, int *exp);
extern long double frexpl(long double value, int *exp);

extern int ilogb(double x);
extern int ilogbf(float x);
extern int ilogbl(long double x);

extern double ldexp(double x, int exp);
extern float ldexpf(float x, int exp);
extern long double ldexpl(long double x, int exp);

/*@ requires finite_arg: \is_finite(x);
    requires arg_positive: x > 0;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
*/
extern double log(double x);

/*@ requires finite_arg: \is_finite(x);
    requires arg_positive: x > 0;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
*/
extern float logf(float x);

/*@ requires finite_arg: \is_finite(x);
    requires arg_pos: x > 0;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
*/
extern long double logl(long double x);

/*@ requires finite_arg: \is_finite(x);
    requires arg_positive: x > 0;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
*/
extern double log10(double x);

/*@ requires finite_arg: \is_finite(x);
    requires arg_positive: x > 0;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
*/
extern float log10f(float x);

/*@ requires finite_arg: \is_finite(x);
    requires arg_postive: x > 0;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
*/
extern long double log10l(long double x);

extern double log1p(double x);
extern float log1pf(float x);
extern long double log1pl(long double x);

/*@ requires finite_arg: \is_finite(x);
    requires arg_positive: x > 0;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
*/
extern double log2(double x);

/*@ requires finite_arg: \is_finite(x);
    requires arg_positive: x > 0;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
*/
extern float log2f(float x);

/*@ requires finite_arg: \is_finite(x);
    requires arg_positive: x > 0;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
*/
extern long double log2l(long double x);

extern double logb(double x);
extern float logbf(float x);
extern long double logbl(long double x);

extern double modf(double value, double *iptr);
extern float modff(float value, float *iptr);
extern long double modfl(long double value, long double *iptr);

extern double scalbn(double x, int n);
extern float scalbnf(float x, int n);
extern long double scalbnl(long double x, int n);

extern double scalbln(double x, long int n);
extern float scalblnf(float x, long int n);
extern long double scalblnl(long double x, long int n);

extern double cbrt(double x);
extern float cbrtf(float x);
extern long double cbrtl(long double x);

/*@ requires finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures res_finite: \is_finite(\result);
    ensures positive_result: \result >= 0.;
    ensures equal_magnitude_result: \result == x || \result == -x;
*/
extern double fabs(double x);

/*@ requires finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures res_finite: \is_finite(\result);
    ensures positive_result: \result >= 0.;
    ensures equal_magnitude_result: \result == x || \result == -x;
*/
extern float fabsf(float x);

/*@ requires finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures res_finite: \is_finite(\result);
    ensures positive_result: \result >= 0.;
    ensures equal_magnitude_result: \result == x || \result == -x;
*/
extern long double fabsl(long double x);

extern double hypot(double x, double y);
extern float hypotf(float x, float y);
extern long double hypotl(long double x, long double y);

/*@ requires finite_args: \is_finite(x) && \is_finite(y);
    requires finite_logic_res: \is_finite(pow(x, y));
    assigns \result \from x, y;
    ensures finite_result: \is_finite(\result);
*/
extern double pow(double x, double y);

/*@ requires finite_args: \is_finite(x) && \is_finite(y);
    requires finite_logic_res: \is_finite(powf(x, y));
    assigns \result \from x, y;
    ensures finite_result: \is_finite(\result);
*/
extern float powf(float x, float y);

extern long double powl(long double x, long double y);

/*@ requires finite_arg: \is_finite(x);
    requires arg_positive: x >= -0.;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures positive_result: \result >= -0.;
*/
extern double sqrt(double x);

/*@ requires finite_arg: \is_finite(x);
    requires arg_positive: x >= -0.;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures positive_result: \result >= -0.;
*/
extern float sqrtf(float x);

/*@ requires finite_arg: \is_finite(x);
    requires arg_positive: x >= -0.;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures positive_result: \result >= -0.;
*/
extern long double sqrtl(long double x);

extern double erf(double x);
extern float erff(float x);
extern long double erfl(long double x);

extern double erfc(double x);
extern float erfcf(float x);
extern long double erfcl(long double x);

extern double lgamma(double x);
extern float lgammaf(float x);
extern long double lgammal(long double x);

extern double tgamma(double x);
extern float tgammaf(float x);
extern long double tgammal(long double x);

/*@ requires finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
*/
extern double ceil(double x);

/*@ requires finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
*/

extern float ceilf(float x);

/*@ requires finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
*/
extern long double ceill(long double x);

/*@ requires finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
*/
extern double floor(double x);

/*@ requires finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
*/
extern float floorf(float x);

/*@ requires finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
*/
extern long double floorl(long double x);

extern double nearbyint(double x);
extern float nearbyintf(float x);
extern long double nearbyintl(long double x);

extern double rint(double x);
extern float rintf(float x);
extern long double rintl(long double x);

extern long int lrint(double x);
extern long int lrintf(float x);
extern long int lrintl(long double x);

extern long long int llrint(double x);
extern long long int llrintf(float x);
extern long long int llrintl(long double x);

/*@ requires finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
*/
extern double round(double x);

/*@ requires finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
*/
extern float roundf(float x);

/*@ requires finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
*/
extern long double roundl(long double x);

extern long int lround(double x);
extern long int lroundf(float x);
extern long int lroundl(long double x);

extern long long int llround(double x);
extern long long int llroundf(float x);
extern long long int llroundl(long double x);

/*@ requires finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
*/
extern double trunc(double x);

/*@ requires finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
*/
extern float truncf(float x);

/*@ requires finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
*/
extern long double truncl(long double x);

/*@ requires finite_args: \is_finite(x) && \is_finite(y);
    requires finite_logic_result: \is_finite(fmod(x, y));
    assigns \result \from x, y;
    ensures finite_result: \is_finite(\result);
*/
extern double fmod(double x, double y);

/*@ requires finite_args: \is_finite(x) && \is_finite(y);
    requires finite_logic_result: \is_finite(fmodf(x, y));
    assigns \result \from x, y;
    ensures finite_result: \is_finite(\result);
*/
extern float fmodf(float x, float y);

extern long double fmodl(long double x, long double y);

extern double remainder(double x, double y);
extern float remainderf(float x, float y);
extern long double remainderl(long double x, long double y);

extern double remquo(double x, double y, int *quo);
extern float remquof(float x, float y, int *quo);
extern long double remquol(long double x, long double y, int *quo);

extern double copysign(double x, double y);
extern float copysignf(float x, float y);
extern long double copysignl(long double x, long double y);

/*@
  requires tagp_valid_string: valid_read_string(tagp);
  assigns \result \from indirect:tagp[0..];
  ensures result_is_nan: \is_NaN(\result);
 */
extern double nan(const char *tagp);

/*@
  requires tagp_valid_string: valid_read_string(tagp);
  assigns \result \from indirect:tagp[0..];
  ensures result_is_nan: \is_NaN(\result);
 */
extern float nanf(const char *tagp);

/*@
  requires tagp_valid_string: valid_read_string(tagp);
  assigns \result \from indirect:tagp[0..];
  ensures result_is_nan: \is_NaN(\result);
 */
extern long double nanl(const char *tagp);

extern double nextafter(double x, double y);
extern float nextafterf(float x, float y);
extern long double nextafterl(long double x, long double y);

extern double nexttoward(double x, long double y);
extern float nexttowardf(float x, long double y);
extern long double nexttowardl(long double x, long double y);

extern double fdim(double x, double y);
extern float fdimf(float x, float y);
extern long double fdiml(long double x, long double y);

extern double fmax(double x, double y);
extern float fmaxf(float x, float y);
extern long double fmaxl(long double x, long double y);

extern double fmin(double x, double y);
extern float fminf(float x, float y);
extern long double fminl(long double x, long double y);

extern double fma(double x, double y, double z);
extern float fmaf(float x, float y, float z);
extern long double fmal(long double x, long double y, long double z);

extern int __finitef(float f);

extern int __finite(double d);

#  define isfinite(x) \
     (sizeof (x) == sizeof (float) ? __finitef (x) : __finite (x))

__END_DECLS

__POP_FC_STDLIB
#endif

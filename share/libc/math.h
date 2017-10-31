/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2017                                               */
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

/*@
  behavior normal:
    assumes \is_finite(x) && \abs(x) <= 1;
    assigns \nothing;
    ensures \is_finite(\result) && \result >= 0;
  behavior edom:
    assumes \is_infinite(x) || (\is_finite(x) && \abs(x) > 1);
    assigns __fc_errno;
    ensures __fc_errno == 1;
  disjoint behaviors normal, edom;
 */
extern double acos(double x);

/*@
  behavior normal:
    assumes \is_finite(x) && \abs(x) <= 1;
    assigns \nothing;
    ensures \is_finite(\result) && \result >= 0;
  behavior edom:
    assumes \is_infinite(x) || (\is_finite(x) && \abs(x) > 1);
    assigns __fc_errno;
    ensures __fc_errno == 1;
  disjoint behaviors normal, edom;
 */
extern float acosf(float x);

/*@
  behavior normal:
    assumes \is_finite(x) && \abs(x) <= 1;
    assigns \nothing;
    ensures \is_finite(\result) && \result >= 0;
  behavior edom:
    assumes \is_infinite(x) || (\is_finite(x) && \abs(x) > 1);
    assigns __fc_errno;
    ensures __fc_errno == 1;
  disjoint behaviors normal, edom;
 */
extern long double acosl(long double x);

/*@
  behavior normal:
    assumes \is_finite(x) && \abs(x) <= 1;
    assigns \nothing;
    ensures \is_finite(\result);
  behavior edom:
    assumes \is_infinite(x) || (\is_finite(x) && \abs(x) > 1);
    assigns __fc_errno;
    ensures __fc_errno == 1;
  disjoint behaviors normal, edom;
 */
extern double asin(double x);

/*@
  behavior normal:
    assumes \is_finite(x) && \abs(x) <= 1;
    assigns \nothing;
    ensures \is_finite(\result);
  behavior edom:
    assumes \is_infinite(x) || (\is_finite(x) && \abs(x) > 1);
    assigns __fc_errno;
    ensures __fc_errno == 1;
  disjoint behaviors normal, edom;
 */
extern float asinf(float x);

/*@
  behavior normal:
    assumes \is_finite(x) && \abs(x) <= 1;
    assigns \nothing;
    ensures \is_finite(\result);
  behavior edom:
    assumes \is_infinite(x) || (\is_finite(x) && \abs(x) > 1);
    assigns __fc_errno;
    ensures __fc_errno == 1;
  disjoint behaviors normal, edom;
 */
extern long double asinl(long double x);

extern float atanf(float x);
extern double atan(double x);
extern long double atanl(long double x);

/*@ assigns \result \from y, x; */
extern double atan2(double y, double x);
extern float atan2f(float y, float x);
extern long double atan2l(long double y, long double x);

/*@ assigns \result \from x; */
extern double cos(double x);
extern float cosf(float x);
extern long double cosl(long double x);

/*@ assigns \result \from x; */
extern double sin(double x);
extern float sinf(float x);
extern long double sinl(long double x);

extern double tan(double x);
extern float tanf(float x);
extern long double tanl(long double x);

/*@
  behavior normal:
    assumes \is_finite(x) && x >= 1;
    assigns \nothing;
    ensures \is_finite(\result) && \result >= 0;
  behavior infinite:
    assumes \is_plus_infinity(x);
    assigns \nothing;
    ensures \is_plus_infinity(\result);
  behavior edom:
    assumes \is_minus_infinity(x) || (\is_finite(x) && x < 1);
    assigns __fc_errno;
    ensures __fc_errno == 1;
  disjoint behaviors normal, infinite, edom;
 */
extern double acosh(double x);

/*@
  behavior normal:
    assumes \is_finite(x) && x >= 1;
    assigns \nothing;
    ensures \is_finite(\result) && \result >= 0;
  behavior infinite:
    assumes \is_plus_infinity(x);
    assigns \nothing;
    ensures \is_plus_infinity(\result);
  behavior edom:
    assumes \is_minus_infinity(x) || (\is_finite(x) && x < 1);
    assigns __fc_errno;
    ensures __fc_errno == 1;
  disjoint behaviors normal, infinite, edom;
 */
extern float acoshf(float x);

/*@
  behavior normal:
    assumes \is_finite(x) && x >= 1;
    assigns \nothing;
    ensures \is_finite(\result) && \result >= 0;
  behavior infinite:
    assumes \is_plus_infinity(x);
    assigns \nothing;
    ensures \is_plus_infinity(\result);
  behavior edom:
    assumes \is_minus_infinity(x) || (\is_finite(x) && x < 1);
    assigns __fc_errno;
    ensures __fc_errno == 1;
  disjoint behaviors normal, infinite, edom;
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

/*@ assigns \result \from x; */
extern double exp(double x);

/*@ assigns \result \from x; */
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

/*@ assigns \result \from x; */
extern double log(double x);

/*@ assigns \result \from x; */
extern float logf(float x);
extern long double logl(long double x);

/*@ assigns \result \from x; */
extern double log10(double x);

/*@ assigns \result \from x; */
extern float log10f(float x);
extern long double log10l(long double x);

extern double log1p(double x);
extern float log1pf(float x);
extern long double log1pl(long double x);

extern double log2(double x);
extern float log2f(float x);
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

extern double fabs(double x);
extern float fabsf(float x);
extern long double fabsl(long double x);

extern double hypot(double x, double y);
extern float hypotf(float x, float y);
extern long double hypotl(long double x, long double y);

/*@ assigns \result \from x, y; */
extern double pow(double x, double y);

/*@ assigns \result \from x, y; */
extern float powf(float x, float y);
extern long double powl(long double x, long double y);

/*@ assigns \result \from x; */
extern double sqrt(double x);

/*@ assigns \result \from x; */
extern float sqrtf(float x);
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

/*@ assigns \result \from x; */
extern double ceil(double x);

/*@ assigns \result \from x; */
extern float ceilf(float x);

extern long double ceill(long double x);

/*@ assigns \result \from x; */
extern double floor(double x);

/*@ assigns \result \from x; */
extern float floorf(float x);

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

/*@ assigns \result \from x; */
extern double round(double x);

/*@ assigns \result \from x; */
extern float roundf(float x);

extern long double roundl(long double x);

extern long int lround(double x);
extern long int lroundf(float x);
extern long int lroundl(long double x);

extern long long int llround(double x);
extern long long int llroundf(float x);
extern long long int llroundl(long double x);

/*@ assigns \result \from x; */
extern double trunc(double x);

/*@ assigns \result \from x; */
extern float truncf(float x);

extern long double truncl(long double x);

/*@ assigns \result \from x, y; */
extern double fmod(double x, double y);
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
  requires valid_read_string(tagp);
  assigns \nothing;
  ensures \is_NaN(\result);
 */
extern double nan(const char *tagp);

/*@
  requires valid_read_string(tagp);
  assigns \nothing;
  ensures \is_NaN(\result);
 */
extern float nanf(const char *tagp);

/*@
  requires valid_read_string(tagp);
  assigns \nothing;
  ensures \is_NaN(\result);
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

int __finitef(float f);

int __finite(double d);

#  define isfinite(x) \
     (sizeof (x) == sizeof (float) ? __finitef (x) : __finite (x))

__END_DECLS

__POP_FC_STDLIB
#endif

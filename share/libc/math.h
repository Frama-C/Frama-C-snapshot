/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2016                                               */
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
    assigns __FC_errno;
    ensures __FC_errno == 1;
  disjoint behaviors normal, edom;
 */
double acos(double x);

/*@
  behavior normal:
    assumes \is_finite(x) && \abs(x) <= 1;
    assigns \nothing;
    ensures \is_finite(\result) && \result >= 0;
  behavior edom:
    assumes \is_infinite(x) || (\is_finite(x) && \abs(x) > 1);
    assigns __FC_errno;
    ensures __FC_errno == 1;
  disjoint behaviors normal, edom;
 */
float acosf(float x);

/*@
  behavior normal:
    assumes \is_finite(x) && \abs(x) <= 1;
    assigns \nothing;
    ensures \is_finite(\result) && \result >= 0;
  behavior edom:
    assumes \is_infinite(x) || (\is_finite(x) && \abs(x) > 1);
    assigns __FC_errno;
    ensures __FC_errno == 1;
  disjoint behaviors normal, edom;
 */
long double acosl(long double x);

/*@
  behavior normal:
    assumes \is_finite(x) && \abs(x) <= 1;
    assigns \nothing;
    ensures \is_finite(\result);
  behavior edom:
    assumes \is_infinite(x) || (\is_finite(x) && \abs(x) > 1);
    assigns __FC_errno;
    ensures __FC_errno == 1;
  disjoint behaviors normal, edom;
 */
double asin(double x);

/*@
  behavior normal:
    assumes \is_finite(x) && \abs(x) <= 1;
    assigns \nothing;
    ensures \is_finite(\result);
  behavior edom:
    assumes \is_infinite(x) || (\is_finite(x) && \abs(x) > 1);
    assigns __FC_errno;
    ensures __FC_errno == 1;
  disjoint behaviors normal, edom;
 */
float asinf(float x);

/*@
  behavior normal:
    assumes \is_finite(x) && \abs(x) <= 1;
    assigns \nothing;
    ensures \is_finite(\result);
  behavior edom:
    assumes \is_infinite(x) || (\is_finite(x) && \abs(x) > 1);
    assigns __FC_errno;
    ensures __FC_errno == 1;
  disjoint behaviors normal, edom;
 */
long double asinl(long double x);

float atanf(float x);
double atan(double x);
long double atanl(long double x);

/*@ assigns \result \from y, x; */
double atan2(double y, double x);
float atan2f(float y, float x);
long double atan2l(long double y, long double x);

/*@ assigns \result \from x; */
double cos(double x);
float cosf(float x);
long double cosl(long double x);

/*@ assigns \result \from x; */
double sin(double x);
float sinf(float x);
long double sinl(long double x);

double tan(double x);
float tanf(float x);
long double tanl(long double x);

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
    assigns __FC_errno;
    ensures __FC_errno == 1;
  disjoint behaviors normal, infinite, edom;
 */
double acosh(double x);

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
    assigns __FC_errno;
    ensures __FC_errno == 1;
  disjoint behaviors normal, infinite, edom;
 */
float acoshf(float x);

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
    assigns __FC_errno;
    ensures __FC_errno == 1;
  disjoint behaviors normal, infinite, edom;
 */
long double acoshl(long double x);

double asinh(double x);
float asinhf(float x);
long double asinhl(long double x);

double atanh(double x);
float atanhf(float x);
long double atanhl(long double x);

double cosh(double x);
float coshf(float x);
long double coshl(long double x);

double sinh(double x);
float sinhf(float x);
long double sinhl(long double x);

double tanh(double x);
float tanhf(float x);
long double tanhl(long double x);

/*@ assigns \result \from x; */
double exp(double x);

/*@ assigns \result \from x; */
float expf(float x);
long double expl(long double x);

double exp2(double x);
float exp2f(float x);
long double exp2l(long double x);

double expm1(double x);
float expm1f(float x);
long double expm1l(long double x);

double frexp(double value, int *exp);
float frexpf(float value, int *exp);
long double frexpl(long double value, int *exp);

int ilogb(double x);
int ilogbf(float x);
int ilogbl(long double x);

double ldexp(double x, int exp);
float ldexpf(float x, int exp);
long double ldexpl(long double x, int exp);

/*@ assigns \result \from x; */
double log(double x);

/*@ assigns \result \from x; */
float logf(float x);
long double logl(long double x);

/*@ assigns \result \from x; */
double log10(double x);

/*@ assigns \result \from x; */
float log10f(float x);
long double log10l(long double x);

double log1p(double x);
float log1pf(float x);
long double log1pl(long double x);

double log2(double x);
float log2f(float x);
long double log2l(long double x);

double logb(double x);
float logbf(float x);
long double logbl(long double x);

double modf(double value, double *iptr);
float modff(float value, float *iptr);
long double modfl(long double value, long double *iptr);

double scalbn(double x, int n);
float scalbnf(float x, int n);
long double scalbnl(long double x, int n);

double scalbln(double x, long int n);
float scalblnf(float x, long int n);
long double scalblnl(long double x, long int n);

double cbrt(double x);
float cbrtf(float x);
long double cbrtl(long double x);

double fabs(double x);
float fabsf(float x);
long double fabsl(long double x);

double hypot(double x, double y);
float hypotf(float x, float y);
long double hypotl(long double x, long double y);

/*@ assigns \result \from x, y; */
double pow(double x, double y);

/*@ assigns \result \from x, y; */
float powf(float x, float y);
long double powl(long double x, long double y);

/*@ assigns \result \from x; */
double sqrt(double x);

/*@ assigns \result \from x; */
float sqrtf(float x);
long double sqrtl(long double x);

double erf(double x);
float erff(float x);
long double erfl(long double x);

double erfc(double x);
float erfcf(float x);
long double erfcl(long double x);

double lgamma(double x);
float lgammaf(float x);
long double lgammal(long double x);

double tgamma(double x);
float tgammaf(float x);
long double tgammal(long double x);

/*@ assigns \result \from x; */
double ceil(double x);

/*@ assigns \result \from x; */
float ceilf(float x);

long double ceill(long double x);

/*@ assigns \result \from x; */
double floor(double x);

/*@ assigns \result \from x; */
float floorf(float x);

long double floorl(long double x);

double nearbyint(double x);
float nearbyintf(float x);
long double nearbyintl(long double x);

double rint(double x);
float rintf(float x);
long double rintl(long double x);

long int lrint(double x);
long int lrintf(float x);
long int lrintl(long double x);

long long int llrint(double x);
long long int llrintf(float x);
long long int llrintl(long double x);

/*@ assigns \result \from x; */
double round(double x);

/*@ assigns \result \from x; */
float roundf(float x);

long double roundl(long double x);

long int lround(double x);
long int lroundf(float x);
long int lroundl(long double x);

long long int llround(double x);
long long int llroundf(float x);
long long int llroundl(long double x);

/*@ assigns \result \from x; */
double trunc(double x);

/*@ assigns \result \from x; */
float truncf(float x);

long double truncl(long double x);

/*@ assigns \result \from x, y; */
double fmod(double x, double y);
float fmodf(float x, float y);
long double fmodl(long double x, long double y);

double remainder(double x, double y);
float remainderf(float x, float y);
long double remainderl(long double x, long double y);

double remquo(double x, double y, int *quo);
float remquof(float x, float y, int *quo);
long double remquol(long double x, long double y, int *quo);

double copysign(double x, double y);
float copysignf(float x, float y);
long double copysignl(long double x, long double y);

/*@
  requires valid_read_string(tagp);
  assigns \nothing;
  ensures \is_NaN(\result);
 */
double nan(const char *tagp);

/*@
  requires valid_read_string(tagp);
  assigns \nothing;
  ensures \is_NaN(\result);
 */
float nanf(const char *tagp);

/*@
  requires valid_read_string(tagp);
  assigns \nothing;
  ensures \is_NaN(\result);
 */
long double nanl(const char *tagp);

double nextafter(double x, double y);
float nextafterf(float x, float y);
long double nextafterl(long double x, long double y);

double nexttoward(double x, long double y);
float nexttowardf(float x, long double y);
long double nexttowardl(long double x, long double y);

double fdim(double x, double y);
float fdimf(float x, float y);
long double fdiml(long double x, long double y);

double fmax(double x, double y);
float fmaxf(float x, float y);
long double fmaxl(long double x, long double y);

double fmin(double x, double y);
float fminf(float x, float y);
long double fminl(long double x, long double y);

double fma(double x, double y, double z);
float fmaf(float x, float y, float z);
long double fmal(long double x, long double y, long double z);

__END_DECLS

#endif

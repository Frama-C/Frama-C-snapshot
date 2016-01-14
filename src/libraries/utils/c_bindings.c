/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2015                                               */
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

 #ifdef _WIN32
/* Must be the first included header */
#include "windows.h"
#endif

#include "caml/mlvalues.h"
#include "caml/alloc.h"
#include "caml/bigarray.h"
#include "caml/fail.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <unistd.h>

// Some BSD flavors do not implement all of C99
#if defined(__OpenBSD__) || defined(__NetBSD__) 
# include <ieeefp.h>
# define FE_DOWNWARD FP_RM
# define FE_UPWARD FP_RP
# define FE_TONEAREST FP_RN
# define fegetround()	fpgetround()
# define fesetround(RM)	fpsetround(RM)
#else 
# include <fenv.h>
#endif

#include <float.h>
#include <math.h>

// Must be synchronized with Floating_point.c_rounding_mode
typedef enum {
  FE_ToNearest, FE_Upward, FE_Downward, FE_TowardZero
} c_rounding_mode_t;

#if defined(__i386__) 
#define GETCOUNTER(low,high)						\
  __asm__ volatile ("rdtsc" : "=a" (low), "=d" (high));
#else
#if  defined(__x86_64__)
#define GETCOUNTER(low,high)						\
{ \
     unsigned int __a,__d; \
     asm volatile("rdtsc" : "=a" (__a), "=d" (__d)); \
     low = ((unsigned long)__a) | (((unsigned long)__d)<<32); \
     high = 0; \
}
#else
#define GETCOUNTER(low,high)						\
  { low = 0; high = 0; }
#endif
#endif

value c_round(value d)
{
  return caml_copy_double(round(Double_val(d)));
}

value c_trunc(value d)
{
  return caml_copy_double(trunc(Double_val(d)));
}

value c_expf(value d)
{
  float f = Double_val(d);
  float res = expf(f);
  return caml_copy_double(res);
}

value c_logf(value d)
{
  float f = Double_val(d);
  float res = logf(f);
  return caml_copy_double(res);
}

value c_log10f(value d)
{
  float f = Double_val(d);
  float res = log10f(f);
  return caml_copy_double(res);
}

value c_powf(value x, value y)
{
  float fx = Double_val(x);
  float fy = Double_val(y);
  float res = powf(fx, fy);
  return caml_copy_double(res);
}

value c_sqrtf(value d)
{
  float f = Double_val(d);
  float res = sqrtf(f);
  return caml_copy_double(res);
}

value getperfcount1024(value dum)
{
  unsigned long l,h,acc;
  GETCOUNTER(l,h);
  acc = (l >> 10) | (h << 22);
  return (acc | 1);
}

value getperfcount(value dum)
{
  unsigned long l, h;
  GETCOUNTER(l,h);
  (void) h;
  return (l | 1);
}

value compare_strings(value v1, value v2, value vlen) {
  if (memcmp(String_val(v1), String_val(v2), Long_val(vlen)) == 0)
    return Val_true;
  else
    return Val_false;
}

value address_of_value(value v)
{
  return (Val_long(((unsigned long)v)/sizeof(long)));
}

value round_to_float(value d)
{
  float f = Double_val(d);
  return caml_copy_double(f);
}

value set_round_downward(value dummy)
{
  fesetround(FE_DOWNWARD);
  return Val_unit;
}

value set_round_upward(value dummy)
{
  fesetround(FE_UPWARD);
  return Val_unit;
}

value set_round_nearest_even(value dummy)
{
  fesetround(FE_TONEAREST);
  return Val_unit;
}

value set_round_toward_zero(value dummy)
{
  fesetround(FE_TOWARDZERO);
  return Val_unit;
}

value get_rounding_mode(value dummy)
{
  switch (fegetround()) {
  case FE_TONEAREST: return Val_int(FE_ToNearest);
  case FE_DOWNWARD: return Val_int(FE_Downward);
  case FE_UPWARD: return Val_int(FE_Upward);
  case FE_TOWARDZERO: return Val_int(FE_TowardZero);
  }
  caml_failwith("illegal rounding mode (should never happen)");
}

value set_rounding_mode(value rm)
{
  int new_rm;
  switch (Int_val(rm)) {
  case FE_ToNearest: new_rm = FE_TONEAREST; break;
  case FE_Downward: new_rm = FE_DOWNWARD; break;
  case FE_Upward: new_rm = FE_UPWARD; break;
  case FE_TowardZero: new_rm = FE_TOWARDZERO; break;
  default:
    caml_invalid_argument("set_rounding_mode");
  }
  fesetround(new_rm);
  return Val_unit;
}

value float_compare_total(value x, value y)
{
  union { double d; int64_t i; } ux, uy;
  ux.d = Double_val(x);
  uy.d = Double_val(y);

  if (ux.i == uy.i) return Val_int(0);
  
  ux.i = ux.i ^ (((uint64_t)(ux.i >> 63))>>1);
  uy.i = uy.i ^ (((uint64_t)(uy.i >> 63))>>1);
 
  if (ux.i < uy.i) return Val_int(-1); else return Val_int(1);
}

value float_is_negative(value v)
{
  union { double d; uint64_t i; } uv;
  uv.d = Double_val(v);
  return (Val_int((int)((uv.i) >> 63)));
}

/* Some compilers apply the C90 standard stricly and do not
   prototype strtof() although it is available in the C library. */
float strtof(const char *, char **);

value single_precision_of_string(value str)
{
  char *end;
  float f = strtof((const char *)str, &end);
  if (end != (char *)str + caml_string_length(str))
    caml_failwith("single_precision_of_string");
  double d = f;
  return caml_copy_double(d);
}

#include <signal.h>
value terminate_process(value v) 
{
  long pid = Long_val(v);
#if _POSIX_C_SOURCE >= 1 || _XOPEN_SOURCE || _POSIX_SOURCE || __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ 
  kill(pid,9);
#else
 #ifdef _WIN32
  TerminateProcess((HANDLE)pid,9);
 #else 
  #warning Does your system have kill()?
 #endif
#endif 

  return Val_unit;
}

value ml_usleep(value v)
{
  usleep( Int_val(v) );
  return Val_unit ;
}

/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2010                                               */
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

#include "caml/mlvalues.h"
#include "caml/bigarray.h"
///#include <stdio.h>
#include <assert.h>


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
  return (l | 1);
}

value address_of_value(value v)
{
  return (Val_long(((unsigned long)v)/sizeof(long)));
}

#if 0
value round_down_to_float(value d)
{
  if (d == 0.)
    return (- MIN_FLOAT);
  float f = Double_val(d);
  float f1;
}
#endif

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


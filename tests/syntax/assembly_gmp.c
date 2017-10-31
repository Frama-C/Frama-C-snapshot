/* run.config
STDOPT: +"-machdep gcc_x86_32"
STDOPT: +"-machdep gcc_x86_64"
STDOPT: +"-machdep ppc_32"

*/
/* mpn_mod_1_1p (ap, n, b, cps)
   Divide (ap,,n) by b.  Return the single-limb remainder.

   Contributed to the GNU project by Torbjorn Granlund and Niels Möller.
   Based on a suggestion by Peter L. Montgomery.

   THE FUNCTIONS IN THIS FILE ARE INTERNAL WITH MUTABLE INTERFACES.  IT IS ONLY
   SAFE TO REACH THEM THROUGH DOCUMENTED INTERFACES.  IN FACT, IT IS ALMOST
   GUARANTEED THAT THEY WILL CHANGE OR DISAPPEAR IN A FUTURE GNU MP RELEASE.

Copyright 2008-2011, 2013 Free Software Foundation, Inc.

This file is part of the GNU MP Library.

The GNU MP Library is free software; you can redistribute it and/or modify
it under the terms of either:

  * the GNU Lesser General Public License as published by the Free
    Software Foundation; either version 3 of the License, or (at your
    option) any later version.

or

  * the GNU General Public License as published by the Free Software
    Foundation; either version 2 of the License, or (at your option) any
    later version.

or both in parallel, as here.

The GNU MP Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received copies of the GNU General Public License and the
GNU Lesser General Public License along with the GNU MP Library.  If not,
see https://www.gnu.org/licenses/.  

File modified by CEA LIST for inclusion in Frama-C test suite

*/

/* Define some longlong.h-style macros, but for wider operations.
 * add_mssaaaa is like longlong.h's add_ssaaaa, but also generates
 * carry out, in the form of a mask. */

#include "stddef.h"

#ifdef __FC_MACHDEP_X86_32
#define GMP_LIMB_BITS 32
#define add_mssaaaa(m, s1, s0, a1, a0, b1, b0)				\
  __asm__ (  "add	%6, %k2\n\t"					\
	     "adc	%4, %k1\n\t"					\
	     "sbb	%k0, %k0"					\
	   : "=r" (m), "=r" (s1), "=&r" (s0)				\
	   : "1"  ((USItype)(a1)), "g" ((USItype)(b1)),			\
	     "%2" ((USItype)(a0)), "g" ((USItype)(b0)))
#endif

#ifdef __FC_MACHDEP_X86_64
#define GMP_LIMB_BITS 64
#define add_mssaaaa(m, s1, s0, a1, a0, b1, b0)				\
  __asm__ (  "add	%6, %q2\n\t"					\
	     "adc	%4, %q1\n\t"					\
	     "sbb	%q0, %q0"					\
	   : "=r" (m), "=r" (s1), "=&r" (s0)				\
	   : "1"  ((UDItype)(a1)), "rme" ((UDItype)(b1)),		\
	     "%2" ((UDItype)(a0)), "rme" ((UDItype)(b0)))
#endif

#ifdef __FC_MACHDEP_PPC_32
#define GMP_LIMB_BITS 32
#define add_mssaaaa(m, s1, s0, a1, a0, b1, b0)				\
  __asm__ (  "add%I6c	%2, %5, %6\n\t"					\
	     "adde	%1, %3, %4\n\t"					\
	     "subfe	%0, %0, %0\n\t"					\
	     "nor	%0, %0, %0"					\
	   : "=r" (m), "=r" (s1), "=&r" (s0)				\
	   : "r"  (a1), "r" (b1), "%r" (a0), "rI" (b0))
#endif

#ifndef add_mssaaaa
#error "This test must run with machdep equals to x86_32, x86_64 or ppc_32"
#endif

#define umul_ppmm(ph, pl, m0, m1) \
  do {									\
    UDItype __m0 = (m0), __m1 = (m1);					\
    __asm__ ("umulh %r1,%2,%0"						\
	     : "=r" (ph)						\
	     : "%rJ" (m0), "rI" (m1));					\
    (pl) = __m0 * __m1;							\
  } while (0)

// CEA: we do not attempt to carry any meaning with those stubs, we're
// just interested in checking the generation of assigns contracts.
typedef long mp_limb_t;
typedef unsigned long UDItype;
typedef long* mp_srcptr;
typedef size_t mp_size_t;

void mpn_mod_1_1p_cps (mp_limb_t cps[4], mp_limb_t b);

void ADDC_LIMB(mp_limb_t, mp_limb_t, mp_limb_t, mp_limb_t);
void udiv_rnnd_preinv (mp_limb_t, mp_limb_t, mp_limb_t, mp_limb_t, mp_limb_t);

#define LIKELY(x) x

mp_limb_t
mpn_mod_1_1p (mp_srcptr ap, mp_size_t n, mp_limb_t b, const mp_limb_t bmodb[4])
{
  int cnt;
  mp_limb_t bi;
  mp_limb_t r0, r1;
  mp_limb_t r;

  r0 = ap[n-2];
  r1 = ap[n-1];

  if (n > 2)
    {
      mp_limb_t B2modb, B2mb;
      mp_limb_t p0, p1;
      mp_limb_t r2;
      mp_size_t j;

      B2modb = bmodb[3];
      B2mb = B2modb - b;

      umul_ppmm (p1, p0, r1, B2modb);
      add_mssaaaa (r2, r1, r0, r0, ap[n-3], p1, p0);

      for (j = n-4; j >= 0; j--)
	{
	  mp_limb_t cy;
	  /* mp_limb_t t = r0 + B2mb; */
	  umul_ppmm (p1, p0, r1, B2modb);

	  ADDC_LIMB (cy, r0, r0, r2 & B2modb);
	  /* Alternative, for cmov: if (cy) r0 = t; */
	  r0 -= (-cy) & b;
	  add_mssaaaa (r2, r1, r0, r0, ap[j], p1, p0);
	}

      r1 -= (r2 & b);
    }

  cnt = bmodb[1];

  if (LIKELY (cnt != 0))
    {
      mp_limb_t t;
      mp_limb_t B1modb = bmodb[2];

      umul_ppmm (r1, t, r1, B1modb);
      r0 += t;
      r1 += (r0 < t);

      /* Normalize */
      r1 = (r1 << cnt) | (r0 >> (GMP_LIMB_BITS - cnt));
      r0 <<= cnt;

      /* NOTE: Might get r1 == b here, but udiv_rnnd_preinv allows that. */
    }
  else
    {
      mp_limb_t mask = -(mp_limb_t) (r1 >= b);
      r1 -= mask & b;
    }

  bi = bmodb[0];

  udiv_rnnd_preinv (r, r1, r0, b, bi);
  return r >> cnt;
}

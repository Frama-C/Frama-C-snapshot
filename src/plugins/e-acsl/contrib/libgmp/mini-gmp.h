/* mini-gmp, a minimalistic implementation of a GNU GMP subset.

Copyright 2011-2015 Free Software Foundation, Inc.

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
see https://www.gnu.org/licenses/.  */

/* About mini-gmp: This is a minimal implementation of a subset of the
   GMP interface. It is intended for inclusion into applications which
   have modest bignums needs, as a fallback when the real GMP library
   is not installed.

   This file defines the public interface. */

#ifndef __MINI_GMP_H__
#define __MINI_GMP_H__

#define preconcat(x,y) x ## y
#define concat(x,y) preconcat(x,y)
#define public_prefix __g
#define export_alias(n) concat(public_prefix,n)

/****************/
/* Initializers */
/****************/
#define mpz_init export_alias(mpz_init)
#define mpz_init_set export_alias(mpz_init_set)
#define mpz_init_set_ui export_alias(mpz_init_set_ui)
#define mpz_init_set_si export_alias(mpz_init_set_si)
#define mpz_init_set_str export_alias(mpz_init_set_str)
#define mpz_import export_alias(mpz_import)

/***************/
/* Assignments */
/***************/
#define mpz_set export_alias(mpz_set)
#define mpz_set_ui export_alias(mpz_set_ui)
#define mpz_set_si export_alias(mpz_set_si)

/*************/
/* Finalizer */
/*************/
#define mpz_clear export_alias(mpz_clear)

/********************/
/* Logical operator */
/********************/
#define mpz_cmp export_alias(mpz_cmp)

/************************/
/* Arithmetic operators */
/************************/
#define mpz_neg export_alias(mpz_neg)
#define mpz_add export_alias(mpz_add)
#define mpz_sub export_alias(mpz_sub)
#define mpz_mul export_alias(mpz_mul)
#define mpz_tdiv_q export_alias(mpz_tdiv_q)
#define mpz_tdiv_r export_alias(mpz_tdiv_r)

/*********************/
/* Bitwise operators */
/*********************/
#define mpz_com export_alias(mpz_com)

/************************/
/* Coercions to C types */
/************************/
#define mpz_get_si export_alias(mpz_get_si)
#define mpz_get_ui export_alias(mpz_get_ui)

/* For size_t */
#include <stddef.h>

static void mp_set_memory_functions (void *(*) (size_t),
			      void *(*) (void *, size_t, size_t),
			      void (*) (void *, size_t));

static void mp_get_memory_functions (void *(**) (size_t),
			      void *(**) (void *, size_t, size_t),
			      void (**) (void *, size_t));

typedef unsigned long mp_limb_t;
typedef long mp_size_t;
typedef unsigned long mp_bitcnt_t;

typedef mp_limb_t *mp_ptr;
typedef const mp_limb_t *mp_srcptr;

typedef struct
{
  int _mp_alloc;		/* Number of *limbs* allocated and pointed
				   to by the _mp_d field.  */
  int _mp_size;			/* abs(_mp_size) is the number of limbs the
				   last field points to.  If _mp_size is
				   negative this is a negative number.  */
  mp_limb_t *_mp_d;		/* Pointer to the limbs.  */
} __mpz_struct;

typedef __mpz_struct mpz_t[1];

typedef __mpz_struct *mpz_ptr;
typedef const __mpz_struct *mpz_srcptr;

static const int mp_bits_per_limb;

static void mpn_copyi (mp_ptr, mp_srcptr, mp_size_t);
static void mpn_copyd (mp_ptr, mp_srcptr, mp_size_t);
static void mpn_zero (mp_ptr, mp_size_t);

static int mpn_cmp (mp_srcptr, mp_srcptr, mp_size_t);
static int mpn_zero_p (mp_srcptr, mp_size_t);

static mp_limb_t mpn_add_1 (mp_ptr, mp_srcptr, mp_size_t, mp_limb_t);
static mp_limb_t mpn_add_n (mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);
static mp_limb_t mpn_add (mp_ptr, mp_srcptr, mp_size_t, mp_srcptr, mp_size_t);

static mp_limb_t mpn_sub_1 (mp_ptr, mp_srcptr, mp_size_t, mp_limb_t);
static mp_limb_t mpn_sub_n (mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);
static mp_limb_t mpn_sub (mp_ptr, mp_srcptr, mp_size_t, mp_srcptr, mp_size_t);

static mp_limb_t mpn_mul_1 (mp_ptr, mp_srcptr, mp_size_t, mp_limb_t);
static mp_limb_t mpn_addmul_1 (mp_ptr, mp_srcptr, mp_size_t, mp_limb_t);
static mp_limb_t mpn_submul_1 (mp_ptr, mp_srcptr, mp_size_t, mp_limb_t);

static mp_limb_t mpn_mul (mp_ptr, mp_srcptr, mp_size_t, mp_srcptr, mp_size_t);
static void mpn_mul_n (mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);
static void mpn_sqr (mp_ptr, mp_srcptr, mp_size_t);
static int mpn_perfect_square_p (mp_srcptr, mp_size_t);
static mp_size_t mpn_sqrtrem (mp_ptr, mp_ptr, mp_srcptr, mp_size_t);

static mp_limb_t mpn_lshift (mp_ptr, mp_srcptr, mp_size_t, unsigned int);
static mp_limb_t mpn_rshift (mp_ptr, mp_srcptr, mp_size_t, unsigned int);

static mp_bitcnt_t mpn_scan0 (mp_srcptr, mp_bitcnt_t);
static mp_bitcnt_t mpn_scan1 (mp_srcptr, mp_bitcnt_t);

static mp_bitcnt_t mpn_popcount (mp_srcptr, mp_size_t);

static mp_limb_t mpn_invert_3by2 (mp_limb_t, mp_limb_t);
#define mpn_invert_limb(x) mpn_invert_3by2 ((x), 0)

static size_t mpn_get_str (unsigned char *, int, mp_ptr, mp_size_t);
static mp_size_t mpn_set_str (mp_ptr, const unsigned char *, size_t, int);

       void mpz_init (mpz_t);
static void mpz_init2 (mpz_t, mp_bitcnt_t);
       void mpz_clear (mpz_t);

#define mpz_odd_p(z)   (((z)->_mp_size != 0) & (int) (z)->_mp_d[0])
#define mpz_even_p(z)  (! mpz_odd_p (z))

static int mpz_sgn (const mpz_t);
static int mpz_cmp_si (const mpz_t, long);
static int mpz_cmp_ui (const mpz_t, unsigned long);
       int mpz_cmp (const mpz_t, const mpz_t);
static int mpz_cmpabs_ui (const mpz_t, unsigned long);
static int mpz_cmpabs (const mpz_t, const mpz_t);
static int mpz_cmp_d (const mpz_t, double);
static int mpz_cmpabs_d (const mpz_t, double);

static void mpz_abs (mpz_t, const mpz_t);
       void mpz_neg (mpz_t, const mpz_t);
static void mpz_swap (mpz_t, mpz_t);

static void mpz_add_ui (mpz_t, const mpz_t, unsigned long);
       void mpz_add (mpz_t, const mpz_t, const mpz_t);
static void mpz_sub_ui (mpz_t, const mpz_t, unsigned long);
static void mpz_ui_sub (mpz_t, unsigned long, const mpz_t);
       void mpz_sub (mpz_t, const mpz_t, const mpz_t);

static void mpz_mul_si (mpz_t, const mpz_t, long int);
static void mpz_mul_ui (mpz_t, const mpz_t, unsigned long int);
void mpz_mul (mpz_t, const mpz_t, const mpz_t);
static void mpz_mul_2exp (mpz_t, const mpz_t, mp_bitcnt_t);
static void mpz_addmul_ui (mpz_t, const mpz_t, unsigned long int);
static void mpz_addmul (mpz_t, const mpz_t, const mpz_t);
static void mpz_submul_ui (mpz_t, const mpz_t, unsigned long int);
static void mpz_submul (mpz_t, const mpz_t, const mpz_t);

static void mpz_cdiv_qr (mpz_t, mpz_t, const mpz_t, const mpz_t);
static void mpz_fdiv_qr (mpz_t, mpz_t, const mpz_t, const mpz_t);
static void mpz_tdiv_qr (mpz_t, mpz_t, const mpz_t, const mpz_t);
static void mpz_cdiv_q (mpz_t, const mpz_t, const mpz_t);
static void mpz_fdiv_q (mpz_t, const mpz_t, const mpz_t);
       void mpz_tdiv_q (mpz_t, const mpz_t, const mpz_t);
static void mpz_cdiv_r (mpz_t, const mpz_t, const mpz_t);
static void mpz_fdiv_r (mpz_t, const mpz_t, const mpz_t);
       void mpz_tdiv_r (mpz_t, const mpz_t, const mpz_t);

static void mpz_cdiv_q_2exp (mpz_t, const mpz_t, mp_bitcnt_t);
static void mpz_fdiv_q_2exp (mpz_t, const mpz_t, mp_bitcnt_t);
static void mpz_tdiv_q_2exp (mpz_t, const mpz_t, mp_bitcnt_t);
static void mpz_cdiv_r_2exp (mpz_t, const mpz_t, mp_bitcnt_t);
static void mpz_fdiv_r_2exp (mpz_t, const mpz_t, mp_bitcnt_t);
static void mpz_tdiv_r_2exp (mpz_t, const mpz_t, mp_bitcnt_t);

static void mpz_mod (mpz_t, const mpz_t, const mpz_t);

static void mpz_divexact (mpz_t, const mpz_t, const mpz_t);

static int mpz_divisible_p (const mpz_t, const mpz_t);
static int mpz_congruent_p (const mpz_t, const mpz_t, const mpz_t);

static unsigned long mpz_cdiv_qr_ui (mpz_t, mpz_t, const mpz_t, unsigned long);
static unsigned long mpz_fdiv_qr_ui (mpz_t, mpz_t, const mpz_t, unsigned long);
static unsigned long mpz_tdiv_qr_ui (mpz_t, mpz_t, const mpz_t, unsigned long);
static unsigned long mpz_cdiv_q_ui (mpz_t, const mpz_t, unsigned long);
static unsigned long mpz_fdiv_q_ui (mpz_t, const mpz_t, unsigned long);
static unsigned long mpz_tdiv_q_ui (mpz_t, const mpz_t, unsigned long);
static unsigned long mpz_cdiv_r_ui (mpz_t, const mpz_t, unsigned long);
static unsigned long mpz_fdiv_r_ui (mpz_t, const mpz_t, unsigned long);
static unsigned long mpz_tdiv_r_ui (mpz_t, const mpz_t, unsigned long);
static unsigned long mpz_cdiv_ui (const mpz_t, unsigned long);
static unsigned long mpz_fdiv_ui (const mpz_t, unsigned long);
static unsigned long mpz_tdiv_ui (const mpz_t, unsigned long);

static unsigned long mpz_mod_ui (mpz_t, const mpz_t, unsigned long);

static void mpz_divexact_ui (mpz_t, const mpz_t, unsigned long);

static int mpz_divisible_ui_p (const mpz_t, unsigned long);

static unsigned long mpz_gcd_ui (mpz_t, const mpz_t, unsigned long);
static void mpz_gcd (mpz_t, const mpz_t, const mpz_t);
static void mpz_gcdext (mpz_t, mpz_t, mpz_t, const mpz_t, const mpz_t);
static void mpz_lcm_ui (mpz_t, const mpz_t, unsigned long);
static void mpz_lcm (mpz_t, const mpz_t, const mpz_t);
static int mpz_invert (mpz_t, const mpz_t, const mpz_t);

static void mpz_sqrtrem (mpz_t, mpz_t, const mpz_t);
static void mpz_sqrt (mpz_t, const mpz_t);
static int mpz_perfect_square_p (const mpz_t);

static void mpz_pow_ui (mpz_t, const mpz_t, unsigned long);
static void mpz_ui_pow_ui (mpz_t, unsigned long, unsigned long);
static void mpz_powm (mpz_t, const mpz_t, const mpz_t, const mpz_t);
static void mpz_powm_ui (mpz_t, const mpz_t, unsigned long, const mpz_t);

static void mpz_rootrem (mpz_t, mpz_t, const mpz_t, unsigned long);
static int mpz_root (mpz_t, const mpz_t, unsigned long);

static void mpz_fac_ui (mpz_t, unsigned long);
static void mpz_bin_uiui (mpz_t, unsigned long, unsigned long);

static int mpz_probab_prime_p (const mpz_t, int);

static int mpz_tstbit (const mpz_t, mp_bitcnt_t);
static void mpz_setbit (mpz_t, mp_bitcnt_t);
static void mpz_clrbit (mpz_t, mp_bitcnt_t);
static void mpz_combit (mpz_t, mp_bitcnt_t);

       void mpz_com (mpz_t, const mpz_t);
static void mpz_and (mpz_t, const mpz_t, const mpz_t);
static void mpz_ior (mpz_t, const mpz_t, const mpz_t);
static void mpz_xor (mpz_t, const mpz_t, const mpz_t);

static mp_bitcnt_t mpz_popcount (const mpz_t);
static mp_bitcnt_t mpz_hamdist (const mpz_t, const mpz_t);
static mp_bitcnt_t mpz_scan0 (const mpz_t, mp_bitcnt_t);
static mp_bitcnt_t mpz_scan1 (const mpz_t, mp_bitcnt_t);

static int mpz_fits_slong_p (const mpz_t);
static int mpz_fits_ulong_p (const mpz_t);
       long int mpz_get_si (const mpz_t);
       unsigned long int mpz_get_ui (const mpz_t);
static double mpz_get_d (const mpz_t);
static size_t mpz_size (const mpz_t);
static mp_limb_t mpz_getlimbn (const mpz_t, mp_size_t);

static void mpz_realloc2 (mpz_t, mp_bitcnt_t);
static mp_srcptr mpz_limbs_read (mpz_srcptr);
static mp_ptr mpz_limbs_modify (mpz_t, mp_size_t);
static mp_ptr mpz_limbs_write (mpz_t, mp_size_t);
static void mpz_limbs_finish (mpz_t, mp_size_t);
static mpz_srcptr mpz_roinit_n (mpz_t, mp_srcptr, mp_size_t);

#define MPZ_ROINIT_N(xp, xs) {{0, (xs),(xp) }}

       void mpz_set_si (mpz_t, signed long int);
       void mpz_set_ui (mpz_t, unsigned long int);
       void mpz_set (mpz_t, const mpz_t);
static void mpz_set_d (mpz_t, double);

       void mpz_init_set_si (mpz_t, signed long int);
       void mpz_init_set_ui (mpz_t, unsigned long int);
       void mpz_init_set (mpz_t, const mpz_t);
static void mpz_init_set_d (mpz_t, double);

static size_t mpz_sizeinbase (const mpz_t, int);
static char *mpz_get_str (char *, int, const mpz_t);
static int mpz_set_str (mpz_t, const char *, int);
       int mpz_init_set_str (mpz_t, const char *, int);

/* This long list taken from gmp.h. */
/* For reference, "defined(EOF)" cannot be used here.  In g++ 2.95.4,
   <iostream> defines EOF but not FILE.  */
#if defined (FILE)                                              \
  || defined (H_STDIO)                                          \
  || defined (_H_STDIO)               /* AIX */                 \
  || defined (_STDIO_H)               /* glibc, Sun, SCO */     \
  || defined (_STDIO_H_)              /* BSD, OSF */            \
  || defined (__STDIO_H)              /* Borland */             \
  || defined (__STDIO_H__)            /* IRIX */                \
  || defined (_STDIO_INCLUDED)        /* HPUX */                \
  || defined (__dj_include_stdio_h_)  /* DJGPP */               \
  || defined (_FILE_DEFINED)          /* Microsoft */           \
  || defined (__STDIO__)              /* Apple MPW MrC */       \
  || defined (_MSL_STDIO_H)           /* Metrowerks */          \
  || defined (_STDIO_H_INCLUDED)      /* QNX4 */		\
  || defined (_ISO_STDIO_ISO_H)       /* Sun C++ */		\
  || defined (__STDIO_LOADED)         /* VMS */
static size_t mpz_out_str (FILE *, int, const mpz_t);
#endif

       void mpz_import (mpz_t, size_t, int, size_t, int, size_t, const void *);
static void *mpz_export (void *, size_t *, int, size_t, int, size_t, const mpz_t);

#endif /* __MINI_GMP_H__ */

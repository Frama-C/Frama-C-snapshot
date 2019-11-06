/**************************************************************************/
/*                                                                        */
/*  This file is part of the Frama-C's E-ACSL plug-in.                    */
/*                                                                        */
/*  Copyright (C) 2012-2019                                               */
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

/*! ***********************************************************************
 * \file  e_acsl_gmp.h
 * \brief Prototypes of functions belonging to GNU Multiple
 * Precision Arithmetic Library (GMP) used within E-ACSL
***************************************************************************/

/******************/
/* GMP prototypes */
/******************/

#ifndef E_ACSL_GMP_API_H
#define E_ACSL_GMP_API_H

#include "stdlib.h"
#include "e_acsl_alias.h"

#define mpz_struct export_alias(mpz_struct)
#define mpz_t      export_alias(mpz_t)
#define mpq_struct export_alias(mpq_struct)
#define mpq_t      export_alias(mpq_t)

struct mpz_struct {
  int _mp_alloc;
  int _mp_size;
  unsigned long *_mp_d;
};

typedef struct mpz_struct mpz_struct;
typedef mpz_struct (__attribute__((__FC_BUILTIN__)) mpz_t)[1];

struct mpq_struct {
  mpz_struct _mp_num;
  mpz_struct _mp_den;
};

typedef struct mpq_struct mpq_struct;
typedef mpq_struct (__attribute__((__FC_BUILTIN__)) mpq_t)[1];

/****************/
/* Initializers */
/****************/

/*@ ghost extern int __e_acsl_init; */

/*@ requires ! \initialized(z);
  @ ensures \valid(z);
  @ allocates z;
  @ assigns *z \from __e_acsl_init; */
extern void __gmpz_init(mpz_t z)
  __attribute__((FC_BUILTIN));

/*@ requires ! \initialized(q);
  @ ensures \valid(q);
  @ allocates q;
  @ assigns *q \from __e_acsl_init; */
extern void __gmpq_init(mpq_t q)
  __attribute__((FC_BUILTIN));

/*@ requires \valid_read(z_orig);
  @ requires ! \initialized(z);
  @ allocates z;
  @ ensures \valid(z);
//  @ ensures z->n == z_orig->n;
  @ assigns *z \from *z_orig; */
extern void __gmpz_init_set(mpz_t z, const mpz_t z_orig)
  __attribute__((FC_BUILTIN));

/*@ requires ! \initialized(z);
  @ allocates z;
  @ ensures \valid(z);
  @ ensures \initialized(z);
//  @ ensures z->n == n;
  @ assigns *z \from n; */
extern void __gmpz_init_set_ui(mpz_t z, unsigned long int n)
  __attribute__((FC_BUILTIN));

/*@ requires ! \initialized(z);
  @ allocates z;
  @ ensures \valid(z);
  @ ensures \initialized(z);
//  @ ensures z->n == n;
  @ assigns *z \from n; */
extern void __gmpz_init_set_si(mpz_t z, signed long int n)
  __attribute__((FC_BUILTIN));

/*@ requires ! \initialized(z);
  @ allocates z;
  @ ensures \valid(z);
  @ ensures \initialized(z);
  @ assigns *z \from str[0..],base;
  @ assigns \result \from str[0..],base; */
extern int __gmpz_init_set_str(mpz_t z, const char *str, int base)
  __attribute__((FC_BUILTIN));

/*@ requires ! \initialized(z);
  @ allocates z;
  @ ensures \valid(z);
  @ ensures \initialized(z);
  @ assigns *z \from base; */
extern void __gmpz_import (mpz_t z, size_t, int, size_t, int, size_t, const void *base)
  __attribute__((FC_BUILTIN));

/***************/
/* Assignments */
/***************/

/*@ requires \valid_read(z_orig);
  @ requires \valid(z);
//  @ ensures z->n == z_orig->n;
  @ assigns *z \from *z_orig; */
extern void __gmpz_set(mpz_t z, const mpz_t z_orig)
  __attribute__((FC_BUILTIN));

/*@ requires \valid_read(q_orig);
  @ requires \valid(q);
  @ assigns *q \from *q_orig; */
extern void __gmpq_set(mpq_t q, const mpq_t q_orig)
  __attribute__((FC_BUILTIN));

/*@ requires \valid(q);
  @ assigns *q \from d; */
extern void __gmpq_set_d(mpq_t q, double d)
  __attribute__((FC_BUILTIN));

/*@ requires \valid(q);
  @ assigns *q \from n; */
extern void __gmpq_set_si(mpq_t q, signed long int n)
  __attribute__((FC_BUILTIN));

/*@ allocates q;
  @ ensures \valid(q);
  @ ensures \initialized(q);
  @ assigns *q \from str[0..],base;
  @ assigns \result \from str[0..],base; */
extern int __gmpq_set_str(mpq_t q, const char *str, int base)
  __attribute__((FC_BUILTIN));

/*@ requires \valid(z);
//  @ ensures z->n == n;
  @ assigns *z \from n; */
extern void __gmpz_set_ui(mpz_t z, unsigned long int n)
  __attribute__((FC_BUILTIN));

/*@ requires \valid(z);
//  @ ensures z->n == n;
  @ assigns *z \from n; */
extern void __gmpz_set_si(mpz_t z, signed long int n)
  __attribute__((FC_BUILTIN));

/*************/
/* Finalizer */
/*************/

/*@ requires \valid(x);
//  @ frees x;
  @ assigns *x \from *x; */
extern void __gmpz_clear(mpz_t x)
  __attribute__((FC_BUILTIN));

/*@ requires \valid(x);
//  @ frees x;
  @ assigns *x \from *x; */
extern void __gmpq_clear(mpq_t x)
  __attribute__((FC_BUILTIN));

/********************/
/* Logical operator */
/********************/

/*@ requires \valid_read(z1);
  @ requires \valid_read(z2);
  @ assigns \result \from *z1, *z2; */
extern int __gmpz_cmp(const mpz_t z1, const mpz_t z2)
  __attribute__((FC_BUILTIN));

/*@ requires \valid_read(q1);
  @ requires \valid_read(q2);
  @ assigns \result \from *q1, *q2; */
extern int __gmpq_cmp(const mpq_t q1, const mpq_t q2)
  __attribute__((FC_BUILTIN));

/************************/
/* Arithmetic operators */
/************************/

/*@ requires \valid(z1);
  @ requires \valid_read(z2);
  @ assigns *z1 \from *z2; */
extern void __gmpz_neg(mpz_t z1, const mpz_t z2)
  __attribute__((FC_BUILTIN));

/*@ requires \valid(z1);
  @ requires \valid_read(z2);
  @ requires \valid_read(z3);
  @ assigns *z1 \from *z2, *z3; */
extern void __gmpz_add(mpz_t z1, const mpz_t z2, const mpz_t z3)
  __attribute__((FC_BUILTIN));

/*@ requires \valid(q1);
  @ requires \valid_read(q2);
  @ requires \valid_read(q3);
  @ assigns *q1 \from *q2, *q3; */
extern void __gmpq_add(mpq_t q1, const mpq_t q2, const mpq_t q3)
  __attribute__((FC_BUILTIN));

/*@ requires \valid(z1);
  @ requires \valid_read(z2);
  @ requires \valid_read(z3);
  @ assigns *z1 \from *z2, *z3; */
extern void __gmpz_sub(mpz_t z1, const mpz_t z2, const mpz_t z3)
  __attribute__((FC_BUILTIN));

/*@ requires \valid(q1);
  @ requires \valid_read(q2);
  @ requires \valid_read(q3);
  @ assigns *q1 \from *q2, *q3; */
extern void __gmpq_sub(mpq_t q1, const mpq_t q2, const mpq_t q3)
  __attribute__((FC_BUILTIN));

/*@ requires \valid(z1);
  @ requires \valid_read(z2);
  @ requires \valid_read(z3);
  @ assigns *z1 \from *z2, *z3; */
extern void __gmpz_mul(mpz_t z1, const mpz_t z2, const mpz_t z3)
  __attribute__((FC_BUILTIN));

/*@ requires \valid(q1);
  @ requires \valid_read(q2);
  @ requires \valid_read(q3);
  @ assigns *q1 \from *q2, *q3; */
extern void __gmpq_mul(mpq_t q1, const mpq_t q2, const mpq_t q3)
  __attribute__((FC_BUILTIN));

/*@ requires \valid(z1);
  @ requires \valid_read(z2);
  @ requires \valid_read(z3);
  @ assigns *z1 \from *z2, *z3; */
extern void __gmpz_tdiv_q(mpz_t z1, const mpz_t z2, const mpz_t z3)
  __attribute__((FC_BUILTIN));

/*@ requires \valid(z1);
  @ requires \valid_read(z2);
  @ requires \valid_read(z3);
  @ assigns *z1 \from *z2, *z3; */
extern void __gmpz_tdiv_r(mpz_t z1, const mpz_t z2, const mpz_t z3)
  __attribute__((FC_BUILTIN));

/*@ requires \valid(q1);
  @ requires \valid_read(q2);
  @ requires \valid_read(q3);
  @ assigns *q1 \from *q2, *q3; */
extern void __gmpq_div(mpq_t q1, const mpq_t q2, const mpq_t q3)
  __attribute__((FC_BUILTIN));

/*********************/
/* Bitwise operators */
/*********************/

/*@ requires \valid(z1);
  @ requires \valid_read(z2);
  @ assigns *z1 \from *z2;
  @ assigns \result \from *z1,*z2; */
extern int __gmpz_com(mpz_t z1, const mpz_t z2)
  __attribute__((FC_BUILTIN));

/************************/
/* Coercions to C types */
/************************/

/*@ requires \valid_read(z);
  @ assigns \result \from *z; */
extern long __gmpz_get_si(const mpz_t z)
  __attribute__((FC_BUILTIN));

/*@ requires \valid_read(q);
  @ assigns \result \from *q; */
extern double __gmpq_get_d(const mpq_t q)
  __attribute__((FC_BUILTIN));

/*@ requires \valid_read(z);
  @ assigns \result \from *z; */
extern unsigned long __gmpz_get_ui(const mpz_t z)
  __attribute__((FC_BUILTIN));

#endif

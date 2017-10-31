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

/* ISO C: 7.20 */
#ifndef __FC_STDLIB
#define __FC_STDLIB
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_machdep.h"
#include "__fc_define_size_t.h"
#include "__fc_define_wchar_t.h"

__BEGIN_DECLS

typedef struct __fc_div_t {
  int quot;              /* Quotient.  */
  int rem;               /* Remainder.  */
} div_t;
typedef struct __fc_ldiv_t {
  long int quot;              /* Quotient.  */
  long int rem;               /* Remainder.  */
} ldiv_t;

typedef struct __fc_lldiv_t {
  long long int quot;              /* Quotient.  */
  long long int rem;               /* Remainder.  */
} lldiv_t;

#include "__fc_define_null.h"

/* These could be customizable */
#define EXIT_FAILURE 1
#define EXIT_SUCCESS 0

#include "limits.h"

#define RAND_MAX __FC_RAND_MAX
#define MB_CUR_MAX __FC_MB_CUR_MAX

/*@
  requires \valid_read(nptr); // cannot be precise, valid_read_string too strong
  assigns \result \from indirect:nptr, indirect:nptr[0 ..];
 */
extern double atof(const char *nptr);

/*@
  requires \valid_read(nptr); // cannot be precise, valid_read_string too strong
  assigns \result \from indirect:nptr, indirect:nptr[0 ..];
 */
extern int atoi(const char *nptr);

/*@
  requires \valid_read(nptr); // cannot be precise, valid_read_string too strong
  assigns \result \from indirect:nptr, indirect:nptr[0 ..];
 */
extern long int atol(const char *nptr);

/*@
  requires \valid_read(nptr); // cannot be precise, valid_read_string too strong
  assigns \result \from indirect:nptr, indirect:nptr[0 ..];
 */
extern long long int atoll(const char *nptr);

/* See ISO C: 7.20.1.3 to complete these specifications */

/*@
  requires \valid_read(nptr); // cannot be precise, valid_read_string too strong
  requires \separated(nptr, endptr);
  assigns \result \from indirect:nptr, indirect:nptr[0 ..];
  assigns *endptr \from nptr, indirect:nptr[0 ..], indirect:endptr;
  behavior null_endptr:
    assumes endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 ..];
  behavior nonnull_endptr:
    assumes endptr != \null;
    requires \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 ..];
    assigns *endptr \from nptr, indirect:nptr[0 ..], indirect:endptr;
    ensures \initialized(endptr);
    ensures \subset(*endptr, nptr + (0 ..));
  complete behaviors;
  disjoint behaviors;
*/
extern double strtod(const char * restrict nptr,
     char ** restrict endptr);

/*@
  requires \valid_read(nptr); // cannot be precise, valid_read_string too strong
  requires \separated(nptr, endptr);
  assigns \result \from indirect:nptr, indirect:nptr[0 ..];
  assigns *endptr \from nptr, indirect:nptr[0 ..], indirect:endptr;
  behavior null_endptr:
    assumes endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 ..];
  behavior nonnull_endptr:
    assumes endptr != \null;
    requires \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 ..];
    assigns *endptr \from nptr, indirect:nptr[0 ..], indirect:endptr;
    ensures \initialized(endptr);
    ensures \valid_read(endptr);
    ensures \subset(*endptr, nptr + (0 ..));
  complete behaviors;
  disjoint behaviors;
*/
extern float strtof(const char * restrict nptr,
     char ** restrict endptr);

/*@
  requires \valid_read(nptr); // cannot be precise, valid_read_string too strong
  requires \separated(nptr, endptr);
  assigns \result \from indirect:nptr, indirect:nptr[0 ..];
  assigns *endptr \from nptr, indirect:nptr[0 ..], indirect:endptr;
  behavior null_endptr:
    assumes endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 ..];
  behavior nonnull_endptr:
    assumes endptr != \null;
    requires \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 ..];
    assigns *endptr \from nptr, indirect:nptr[0 ..], indirect:endptr;
    ensures \initialized(endptr);
    ensures \valid_read(endptr);
    ensures \subset(*endptr, nptr + (0 ..));
  complete behaviors;
  disjoint behaviors;
*/
extern long double strtold(const char * restrict nptr,
     char ** restrict endptr);

/* TODO: See ISO C 7.20.1.4 to complete these specifications */
/*@
  requires \valid_read(nptr); // cannot be precise, valid_read_string too strong
  requires \separated(nptr, endptr);
  requires base == 0 || 2 <= base <= 36;
  assigns \result \from indirect:nptr, indirect:nptr[0 ..], indirect:base;
  assigns *endptr \from nptr, indirect:nptr[0 ..], indirect:endptr, indirect:base;
  behavior null_endptr:
    assumes endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 ..], indirect:base;
  behavior nonnull_endptr:
    assumes endptr != \null;
    requires \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 ..], indirect:base;
    assigns *endptr \from nptr, indirect:nptr[0 ..], indirect:endptr, indirect:base;
    ensures \initialized(endptr);
    ensures \valid_read(endptr);
    ensures \subset(*endptr, nptr + (0 ..));
  complete behaviors;
  disjoint behaviors;
*/
extern long int strtol(
     const char * restrict nptr,
     char ** restrict endptr,
     int base);

/*@
  requires \valid_read(nptr); // cannot be precise, valid_read_string too strong
  requires \separated(nptr, endptr);
  requires base == 0 || 2 <= base <= 36;
  assigns \result \from indirect:nptr, indirect:nptr[0 ..], indirect:base;
  assigns *endptr \from nptr, indirect:nptr[0 ..], indirect:endptr, indirect:base;
  behavior null_endptr:
    assumes endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 ..], indirect:base;
  behavior nonnull_endptr:
    assumes endptr != \null;
    requires \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 ..], indirect:base;
    assigns *endptr \from nptr, indirect:nptr[0 ..], indirect:endptr, indirect:base;
    ensures \initialized(endptr);
    ensures \valid_read(endptr);
    ensures \subset(*endptr, nptr + (0 ..));
  complete behaviors;
  disjoint behaviors;
*/
extern long long int strtoll(
     const char * restrict nptr,
     char ** restrict endptr,
     int base);

/*@
  requires \valid_read(nptr); // cannot be precise, valid_read_string too strong
  requires \separated(nptr, endptr);
  requires base == 0 || 2 <= base <= 36;
  assigns \result \from indirect:nptr, indirect:nptr[0 ..], indirect:base;
  assigns *endptr \from nptr, indirect:nptr[0 ..], indirect:endptr, indirect:base;
  behavior null_endptr:
    assumes endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 ..], indirect:base;
  behavior nonnull_endptr:
    assumes endptr != \null;
    requires \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 ..], indirect:base;
    assigns *endptr \from nptr, indirect:nptr[0 ..], indirect:endptr, indirect:base;
    ensures \initialized(endptr);
    ensures \valid_read(endptr);
    ensures \subset(*endptr, nptr + (0 ..));
  complete behaviors;
  disjoint behaviors;
*/
extern unsigned long int strtoul(
     const char * restrict nptr,
     char ** restrict endptr,
     int base);

/*@
  requires \valid_read(nptr); // cannot be precise, valid_read_string too strong
  requires \separated(nptr, endptr);
  requires base == 0 || 2 <= base <= 36;
  assigns \result \from indirect:nptr, indirect:nptr[0 ..], indirect:base;
  assigns *endptr \from nptr, indirect:nptr[0 ..], indirect:endptr, indirect:base;
  behavior null_endptr:
    assumes endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 ..], indirect:base;
  behavior nonnull_endptr:
    assumes endptr != \null;
    requires \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 ..], indirect:base;
    assigns *endptr \from nptr, indirect:nptr[0 ..], indirect:endptr, indirect:base;
    ensures \initialized(endptr);
    ensures \valid_read(endptr);
    ensures \subset(*endptr, nptr + (0 ..));
  complete behaviors;
  disjoint behaviors;
*/
extern unsigned long long int strtoull(
     const char * restrict nptr,
     char ** restrict endptr,
     int base);

//@ ghost extern int __fc_random_counter __attribute__((unused)) __attribute__((FRAMA_C_MODEL));
const unsigned long __fc_rand_max = __FC_RAND_MAX;
/* ISO C: 7.20.2 */
/*@ assigns \result \from __fc_random_counter ;
  @ assigns __fc_random_counter \from __fc_random_counter ;
  @ ensures 0 <= \result <= __fc_rand_max ;
*/
extern int rand(void);

#ifdef _POSIX_C_SOURCE
# if _POSIX_C_SOURCE >= 200112L
/*@ assigns \result \from __fc_random_counter ;
  @ assigns __fc_random_counter \from __fc_random_counter ;
  @ ensures 0 <= \result < 2147483648 ;
*/
extern long int lrand48 (void);

/*@ assigns __fc_random_counter \from seed ; */
extern void srand48 (long int seed);
# endif
#endif

/*@ assigns __fc_random_counter \from seed ; */
extern void srand(unsigned int seed);


/*@ ghost extern int __fc_heap_status __attribute__((FRAMA_C_MODEL)); */

/*@ axiomatic dynamic_allocation {
  @   predicate is_allocable{L}(integer n) // Can a block of n bytes be allocated?
  @     reads __fc_heap_status;
  @   // The logic label L is not used, but it must be present because the
  @   // predicate depends on the memory state
  @   axiom never_allocable{L}:
  @     \forall integer i;
  @        i < 0 || i > __FC_SIZE_MAX ==> !is_allocable(i);
  @ }
*/

/* ISO C: 7.20.3.1 */
/*@
  allocates \result;
  assigns __fc_heap_status \from indirect:nmemb, indirect:size, __fc_heap_status;
  assigns \result \from indirect:nmemb, indirect:size,
                        indirect:__fc_heap_status;

  behavior allocation:
    assumes is_allocable(nmemb * size);
    ensures \fresh(\result, nmemb * size);
    ensures \initialized(((char *)\result)+(0..nmemb*size-1));
    ensures \subset(((char *)\result)[0..nmemb*size-1], {0});

  behavior no_allocation:
    assumes !is_allocable(nmemb * size);
    assigns \result \from \nothing;
    allocates \nothing;
    ensures \result == \null;

  complete behaviors;
  disjoint behaviors; */
extern void *calloc(size_t nmemb, size_t size);
 
/*@ allocates \result;
  @ assigns __fc_heap_status \from size, __fc_heap_status;
  @ assigns \result \from indirect:size, indirect:__fc_heap_status;
  @ behavior allocation:
  @   assumes is_allocable(size);
  @   assigns __fc_heap_status \from size, __fc_heap_status;
  @   assigns \result \from indirect:size, indirect:__fc_heap_status;
  @   ensures \fresh(\result,size);
  @ behavior no_allocation:
  @   assumes !is_allocable(size);
  @   assigns \result \from \nothing;
  @   allocates \nothing;
  @   ensures \result==\null;
  @ complete behaviors;
  @ disjoint behaviors;
  @*/
extern void *malloc(size_t size);

/*@ frees p;
  @ assigns  __fc_heap_status \from __fc_heap_status;
  @ behavior deallocation:
  @   assumes  p!=\null;
  @   requires freeable:\freeable(p);
  @   assigns  __fc_heap_status \from __fc_heap_status;
  @   ensures  \allocable(p);
  @ behavior no_deallocation:
  @   assumes  p==\null;
  @   assigns  \nothing;
  @   frees    \nothing;
  @ complete behaviors;
  @ disjoint behaviors;
  @*/
extern void free(void *p);

/*@
   requires ptr == \null || \freeable(ptr);
   allocates \result;
   frees     ptr;
   assigns   __fc_heap_status \from __fc_heap_status;
   assigns   \result \from size, ptr, __fc_heap_status;

   behavior alloc:
     assumes   is_allocable(size);
     allocates \result;
     assigns   \result \from size, __fc_heap_status;
     ensures   \fresh(\result,size);

   behavior dealloc:
     assumes   ptr != \null;
     assumes   is_allocable(size);
     requires  \freeable(ptr);
     frees     ptr;
     ensures   \allocable(ptr);
     ensures   \result == \null || \freeable(\result);

   behavior fail:
     assumes !is_allocable(size);
     allocates \nothing;
     frees     \nothing;
     assigns   \result \from size, __fc_heap_status;
     ensures   \result == \null;

   complete behaviors;
   disjoint behaviors alloc, fail;
   disjoint behaviors dealloc, fail;
  */
extern void *realloc(void *ptr, size_t size);


/* ISO C: 7.20.4 */

/*@ assigns \nothing;
  @ ensures \false; */
extern void abort(void);

/*@ assigns \result \from \nothing ;*/
extern int atexit(void (*func)(void));

/*@ assigns \result \from \nothing ;*/
extern int at_quick_exit(void (*func)(void));

/*@
  assigns \nothing;
  ensures \false;
*/
extern void exit(int status) __attribute__ ((noreturn));

/*@
  assigns \nothing;
  ensures \false;
*/
extern void _Exit(int status) __attribute__ ((__noreturn__));

/*@
  requires \valid_read(name);
  assigns \result \from indirect:name, name[0 ..];
  ensures \result == \null || \valid(\result);
 */
extern char *getenv(const char *name);

extern int putenv(char *string);

extern int setenv(const char *name, const char *value, int overwrite);

extern int unsetenv(const char *name);

/*@
  assigns \nothing;
  ensures \false; */
extern void quick_exit(int status) __attribute__ ((__noreturn__));

/*@
  requires \valid_read(command);
  assigns \result \from indirect:command, indirect:command[0 ..];
*/
extern int system(const char *command);

/* ISO C: 7.20.5 */

/* TODO: use one of the well known specification with high order compare :-) */
// NOTE: the assigns of function [compar] are not currently taken into account
// by ACSL. If [compar] is not purely functional, the result may be unsound.
/*@
  requires \valid_function(compar);
  assigns ((char*)\result)[0 ..] \from indirect:key, ((char*)key)[0 ..],
                                       indirect:base, ((char*)base)[0 ..],
                                       indirect:nmemb, indirect:size,
                                       indirect:compar, indirect:*compar;
*/
extern void *bsearch(const void *key, const void *base,
     size_t nmemb, size_t size,
     int (*compar)(const void *, const void *));

// NOTE: the assigns of function [compar] are not currently taken into account
// by ACSL. If [compar] is not purely functional, the result may be unsound.
/*@
  requires \valid_function(compar);
  assigns ((char*)base)[0 ..] \from indirect:base, ((char*)base)[0 ..],
                                    indirect:nmemb, indirect:size,
                                    indirect:compar, indirect:*compar;
 */
extern void qsort(void *base, size_t nmemb, size_t size,
             int (*compar)(const void *, const void *));

/* ISO C: 7.20.6 */

/*@
  requires abs_representable: j > INT_MIN;
  assigns \result \from j;
  behavior neg:
    assumes j < 0;
    ensures \result == -j;
  behavior nonneg:
    assumes j >= 0;
    ensures \result == j;
  complete behaviors;
  disjoint behaviors;
 */
extern int abs(int j);

/*@
  requires abs_representable: j > LONG_MIN ;
  assigns \result \from j;
  behavior neg:
    assumes j < 0;
    ensures \result == -j;
  behavior nonneg:
    assumes j >= 0;
    ensures \result == j;
  complete behaviors;
  disjoint behaviors;
 */
extern long int labs(long int j);

/*@
  requires abs_representable: j > LLONG_MIN ;
  assigns \result \from j;
  behavior neg:
    assumes j < 0;
    ensures \result == -j;
  behavior nonneg:
    assumes j >= 0;
    ensures \result == j;
  complete behaviors;
  disjoint behaviors;
 */
extern long long int llabs(long long int j);

/*@ assigns \result \from numer,denom ; */
extern div_t div(int numer, int denom);
/*@ assigns \result \from numer,denom ; */
extern ldiv_t ldiv(long int numer, long int denom);
/*@ assigns \result \from numer,denom ; */
extern lldiv_t lldiv(long long int numer, long long int denom);

/* ISO C: 7.20.7 */

//@ ghost extern int __fc_mblen_state;

/*@ assigns \result, __fc_mblen_state \from
    indirect:s, indirect:s[0 ..], indirect:n, __fc_mblen_state; */
extern int mblen(const char *s, size_t n);

//@ ghost extern int __fc_mbtowc_state;

/*@
  requires \separated(pwc, s);
  assigns \result \from indirect:s, indirect:s[0 .. n-1], indirect:n,
                        __fc_mbtowc_state;
  assigns pwc[0 .. \result-1], __fc_mbtowc_state
    \from indirect:s, s[0 .. n-1], indirect:n, __fc_mbtowc_state;
  ensures \result <= n;
*/
extern int mbtowc(wchar_t * restrict pwc,
     const char * restrict s,
     size_t n);

//@ ghost extern int __fc_wctomb_state;

/*@
  assigns \result \from indirect:wc, __fc_wctomb_state;
  assigns s[0 ..], __fc_wctomb_state \from wc, __fc_wctomb_state;
*/
extern int wctomb(char *s, wchar_t wc);

/* ISO C: 7.20.8 */

/*@
  requires \separated(pwcs, s);
  assigns \result \from indirect:s, indirect:s[0 .. n-1], indirect:n;
  assigns pwcs[0 .. n-1] \from indirect:s, s[0 .. n-1], indirect:n;
*/
extern size_t mbstowcs(wchar_t * restrict pwcs,
     const char * restrict s,
     size_t n);

/*@
  requires \separated(s, pwcs);
  assigns \result \from indirect:pwcs, indirect:pwcs[0 .. n-1], indirect:n;
  assigns s[0 .. n-1] \from indirect:pwcs, pwcs[0 .. n-1], indirect:n;
*/
extern size_t wcstombs(char * restrict s,
     const wchar_t * restrict pwcs,
     size_t n);


__END_DECLS

__POP_FC_STDLIB
#endif

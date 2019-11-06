/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2019                                               */
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
#include "__fc_alloc_axiomatic.h"
#include "__fc_string_axiomatic.h"

__BEGIN_DECLS

#ifndef __div_t_defined
typedef struct __fc_div_t {
  int quot;              /* Quotient.  */
  int rem;               /* Remainder.  */
} div_t;
#define __div_t_defined
#endif

#ifndef __ldiv_t_defined
typedef struct __fc_ldiv_t {
  long int quot;              /* Quotient.  */
  long int rem;               /* Remainder.  */
} ldiv_t;
#define __ldiv_t_defined
#endif

#ifndef __lldiv_t_defined
typedef struct __fc_lldiv_t {
  long long int quot;              /* Quotient.  */
  long long int rem;               /* Remainder.  */
} lldiv_t;
#define __lldiv_t_defined
#endif

#include "__fc_define_null.h"

/* These could be customizable */
#define EXIT_FAILURE 1
#define EXIT_SUCCESS 0

#include "limits.h"

#define RAND_MAX __FC_RAND_MAX
#define MB_CUR_MAX __FC_MB_CUR_MAX

/*@
  requires valid_nptr: \valid_read(nptr); // cannot be precise, valid_read_string too strong
  assigns \result \from indirect:nptr, indirect:nptr[0 ..];
 */
extern double atof(const char *nptr);

/*@
  requires valid_nptr: \valid_read(nptr); // cannot be precise, valid_read_string too strong
  assigns \result \from indirect:nptr, indirect:nptr[0 ..];
 */
extern int atoi(const char *nptr);

/*@
  requires valid_nptr: \valid_read(nptr); // cannot be precise, valid_read_string too strong
  assigns \result \from indirect:nptr, indirect:nptr[0 ..];
 */
extern long int atol(const char *nptr);

/*@
  requires valid_nptr: \valid_read(nptr); // cannot be precise, valid_read_string too strong
  assigns \result \from indirect:nptr, indirect:nptr[0 ..];
 */
extern long long int atoll(const char *nptr);

/* See ISO C: 7.20.1.3 to complete these specifications */

/*@
  requires valid_nptr: \valid_read(nptr); // cannot be precise, valid_read_string too strong
  requires separation: \separated(nptr, endptr);
  assigns \result \from indirect:nptr, indirect:nptr[0 ..];
  assigns *endptr \from nptr, indirect:nptr[0 ..], indirect:endptr;
  behavior no_storage:
    assumes null_endptr: endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 ..];
  behavior store_position:
    assumes nonnull_endptr: endptr != \null;
    requires valid_endptr: \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 ..];
    assigns *endptr \from nptr, indirect:nptr[0 ..], indirect:endptr;
    ensures initialization: \initialized(endptr);
    ensures valid_endptr: \valid_read(endptr);
    ensures position_subset: \subset(*endptr, nptr + (0 ..));
  complete behaviors;
  disjoint behaviors;
*/
extern double strtod(const char * restrict nptr,
     char ** restrict endptr);

/*@
  requires valid_nptr: \valid_read(nptr); // cannot be precise, valid_read_string too strong
  requires separation: \separated(nptr, endptr);
  assigns \result \from indirect:nptr, indirect:nptr[0 ..];
  assigns *endptr \from nptr, indirect:nptr[0 ..], indirect:endptr;
  behavior no_storage:
    assumes null_endptr: endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 ..];
  behavior store_position:
    assumes nonnull_endptr: endptr != \null;
    requires valid_endptr: \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 ..];
    assigns *endptr \from nptr, indirect:nptr[0 ..], indirect:endptr;
    ensures initialization: \initialized(endptr);
    ensures valid_endptr: \valid_read(endptr);
    ensures position_subset: \subset(*endptr, nptr + (0 ..));
  complete behaviors;
  disjoint behaviors;
*/
extern float strtof(const char * restrict nptr,
     char ** restrict endptr);

/*@
  requires valid_nptr: \valid_read(nptr); // cannot be precise, valid_read_string too strong
  requires separation: \separated(nptr, endptr);
  assigns \result \from indirect:nptr, indirect:nptr[0 ..];
  assigns *endptr \from nptr, indirect:nptr[0 ..], indirect:endptr;
  behavior no_storage:
    assumes null_endptr: endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 ..];
  behavior store_position:
    assumes nonnull_endptr: endptr != \null;
    requires valid_endptr: \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 ..];
    assigns *endptr \from nptr, indirect:nptr[0 ..], indirect:endptr;
    ensures initialization: \initialized(endptr);
    ensures valid_endptr: \valid_read(endptr);
    ensures position_subset: \subset(*endptr, nptr + (0 ..));
  complete behaviors;
  disjoint behaviors;
*/
extern long double strtold(const char * restrict nptr,
     char ** restrict endptr);

/* TODO: See ISO C 7.20.1.4 to complete these specifications */
/*@
  requires valid_nptr: \valid_read(nptr); // cannot be precise, valid_read_string too strong
  requires separation: \separated(nptr, endptr);
  requires base_range: base == 0 || 2 <= base <= 36;
  assigns \result \from indirect:nptr, indirect:nptr[0 ..], indirect:base;
  assigns *endptr \from nptr, indirect:nptr[0 ..], indirect:endptr, indirect:base;
  behavior no_storage:
    assumes null_endptr: endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 ..], indirect:base;
  behavior store_position:
    assumes nonnull_endptr: endptr != \null;
    requires valid_endptr: \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 ..], indirect:base;
    assigns *endptr \from nptr, indirect:nptr[0 ..], indirect:endptr, indirect:base;
    ensures initialization: \initialized(endptr);
    ensures valid_endptr: \valid_read(endptr);
    ensures position_subset: \subset(*endptr, nptr + (0 ..));
  complete behaviors;
  disjoint behaviors;
*/
extern long int strtol(
     const char * restrict nptr,
     char ** restrict endptr,
     int base);

/*@
  requires valid_nptr: \valid_read(nptr); // cannot be precise, valid_read_string too strong
  requires separation: \separated(nptr, endptr);
  requires base_range: base == 0 || 2 <= base <= 36;
  assigns \result \from indirect:nptr, indirect:nptr[0 ..], indirect:base;
  assigns *endptr \from nptr, indirect:nptr[0 ..], indirect:endptr, indirect:base;
  behavior no_storage:
    assumes null_endptr: endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 ..], indirect:base;
  behavior store_position:
    assumes nonnull_endptr: endptr != \null;
    requires valid_endptr: \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 ..], indirect:base;
    assigns *endptr \from nptr, indirect:nptr[0 ..], indirect:endptr, indirect:base;
    ensures initialization: \initialized(endptr);
    ensures valid_endptr: \valid_read(endptr);
    ensures position_subset: \subset(*endptr, nptr + (0 ..));
  complete behaviors;
  disjoint behaviors;
*/
extern long long int strtoll(
     const char * restrict nptr,
     char ** restrict endptr,
     int base);

/*@
  requires valid_nptr: \valid_read(nptr); // cannot be precise, valid_read_string too strong
  requires separation: \separated(nptr, endptr);
  requires base_range: base == 0 || 2 <= base <= 36;
  assigns \result \from indirect:nptr, indirect:nptr[0 ..], indirect:base;
  assigns *endptr \from nptr, indirect:nptr[0 ..], indirect:endptr, indirect:base;
  behavior no_storage:
    assumes null_endptr: endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 ..], indirect:base;
  behavior store_position:
    assumes nonnull_endptr: endptr != \null;
    requires valid_endptr: \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 ..], indirect:base;
    assigns *endptr \from nptr, indirect:nptr[0 ..], indirect:endptr, indirect:base;
    ensures initialization: \initialized(endptr);
    ensures valid_endptr: \valid_read(endptr);
    ensures position_subset: \subset(*endptr, nptr + (0 ..));
  complete behaviors;
  disjoint behaviors;
*/
extern unsigned long int strtoul(
     const char * restrict nptr,
     char ** restrict endptr,
     int base);

/*@
  requires valid_nptr: \valid_read(nptr); // cannot be precise, valid_read_string too strong
  requires separation: \separated(nptr, endptr);
  requires base_range: base == 0 || 2 <= base <= 36;
  assigns \result \from indirect:nptr, indirect:nptr[0 ..], indirect:base;
  assigns *endptr \from nptr, indirect:nptr[0 ..], indirect:endptr, indirect:base;
  behavior no_storage:
    assumes null_endptr: endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 ..], indirect:base;
  behavior store_position:
    assumes nonnull_endptr: endptr != \null;
    requires valid_endptr: \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 ..], indirect:base;
    assigns *endptr \from nptr, indirect:nptr[0 ..], indirect:endptr, indirect:base;
    ensures initialization: \initialized(endptr);
    ensures valid_endptr: \valid_read(endptr);
    ensures position_subset: \subset(*endptr, nptr + (0 ..));
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
  @ ensures result_range: 0 <= \result <= __fc_rand_max ;
*/
extern int rand(void);

/*@ assigns __fc_random_counter \from seed ; */
extern void srand(unsigned int seed);

/*@
  assigns \result \from __fc_random_counter;
  ensures result_range: 0 <= \result <= __fc_rand_max;
*/
extern long int random(void);

/*@ assigns __fc_random_counter \from seed; */
extern void srandom(unsigned int seed);

// used to check if some *48() functions have called the seed initializer
int __fc_random48_init __attribute__((FRAMA_C_MODEL));

extern unsigned short __fc_random48_counter[3] __attribute__((FRAMA_C_MODEL));
unsigned short *__fc_p_random48_counter = __fc_random48_counter;

/*@
  assigns __fc_random48_counter[0..2] \from seed;
  assigns __fc_random48_init \from \nothing;
  ensures random48_initialized: __fc_random48_init == 1;
*/
extern void srand48 (long int seed);

/*@
  requires initialization:initialized_seed16v: \initialized(seed16v+(0..2));
  assigns __fc_random48_counter[0..2] \from indirect:seed16v[0..2];
  assigns __fc_random48_init \from \nothing;
  assigns \result \from __fc_p_random48_counter;
  ensures random48_initialized: __fc_random48_init == 1;
  ensures result_counter: \result == __fc_p_random48_counter;
*/
extern unsigned short *seed48(unsigned short seed16v[3]);

/*@
  assigns __fc_random48_counter[0..2] \from param[0..5];
  assigns __fc_random48_init \from \nothing;
  ensures random48_initialized: __fc_random48_init == 1;
*/
extern void lcong48(unsigned short param[7]);

/*@
  requires random48_initialized: __fc_random48_init == 1;
  assigns __fc_random48_counter[0..2] \from __fc_random48_counter[0..2];
  assigns \result \from __fc_random48_counter[0..2];
  ensures result_range: \is_finite(\result) && 0.0 <= \result < 1.0;
*/
extern double drand48(void);

/*@
  requires initialization:initialized_xsubi: \initialized(xsubi+(0..2));
  assigns __fc_random48_counter[0..2] \from __fc_random48_counter[0..2];
  assigns \result \from __fc_random48_counter[0..2];
  ensures result_range: \is_finite(\result) && 0.0 <= \result < 1.0;
*/
extern double erand48(unsigned short xsubi[3]);

/*@
  requires random48_initialized: __fc_random48_init == 1;
  assigns __fc_random48_counter[0..2] \from __fc_random48_counter[0..2];
  assigns \result \from __fc_random48_counter[0..2];
  ensures result_range: 0 <= \result < 2147483648;
*/
extern long int lrand48 (void);

/*@
  requires initialization:initialized_xsubi: \initialized(xsubi+(0..2));
  assigns __fc_random48_counter[0..2] \from __fc_random48_counter[0..2];
  assigns \result \from __fc_random48_counter[0..2];
  ensures result_range: 0 <= \result < 2147483648;
*/
extern long int nrand48 (unsigned short xsubi[3]);

/*@
  requires random48_initialized: __fc_random48_init == 1;
  assigns __fc_random48_counter[0..2] \from __fc_random48_counter[0..2];
  assigns \result \from __fc_random48_counter[0..2];
  ensures result_range: -2147483648 <= \result < 2147483648;
*/
extern long int mrand48 (void);

/*@
  requires initialization:initialized_xsubi: \initialized(xsubi+(0..2));
  assigns __fc_random48_counter[0..2] \from __fc_random48_counter[0..2];
  assigns \result \from __fc_random48_counter[0..2];
  ensures result_range: -2147483648 <= \result < 2147483648;
*/
extern long int jrand48 (unsigned short xsubi[3]);

/* ISO C: 7.20.3.1 */
/*@
  allocates \result;
  assigns __fc_heap_status \from indirect:nmemb, indirect:size, __fc_heap_status;
  assigns \result \from indirect:nmemb, indirect:size,
                        indirect:__fc_heap_status;

  behavior allocation:
    assumes can_allocate: is_allocable(nmemb * size);
    ensures allocation: \fresh(\result, nmemb * size);
    ensures initialization: \initialized(((char *)\result)+(0..nmemb*size-1));
    ensures zero_initialization: \subset(((char *)\result)[0..nmemb*size-1], {0});

  behavior no_allocation:
    assumes cannot_allocate: !is_allocable(nmemb * size);
    assigns \result \from \nothing;
    allocates \nothing;
    ensures null_result: \result == \null;

  complete behaviors;
  disjoint behaviors; */
extern void *calloc(size_t nmemb, size_t size);
 
/*@ allocates \result;
  @ assigns __fc_heap_status \from size, __fc_heap_status;
  @ assigns \result \from indirect:size, indirect:__fc_heap_status;
  @ behavior allocation:
  @   assumes can_allocate: is_allocable(size);
  @   assigns __fc_heap_status \from size, __fc_heap_status;
  @   assigns \result \from indirect:size, indirect:__fc_heap_status;
  @   ensures allocation: \fresh(\result,size);
  @ behavior no_allocation:
  @   assumes cannot_allocate: !is_allocable(size);
  @   assigns \result \from \nothing;
  @   allocates \nothing;
  @   ensures null_result: \result==\null;
  @ complete behaviors;
  @ disjoint behaviors;
  @*/
extern void *malloc(size_t size);

/*@ requires freeable: p==\null || \freeable(p);
  @ frees p;
  @ assigns  __fc_heap_status \from __fc_heap_status;
  @ behavior deallocation:
  @   assumes  nonnull_p: p!=\null;
  @   assigns  __fc_heap_status \from __fc_heap_status;
  @   ensures  freed: \allocable(p);
  @ behavior no_deallocation:
  @   assumes  null_p: p==\null;
  @   assigns  \nothing;
  @   frees    \nothing;
  @ complete behaviors;
  @ disjoint behaviors;
  @*/
extern void free(void *p);

/*@
   requires freeable: ptr == \null || \freeable(ptr);
   allocates \result;
   frees     ptr;
   assigns   __fc_heap_status \from __fc_heap_status;
   assigns   \result \from size, ptr, __fc_heap_status;

   behavior allocation:
     assumes   can_allocate: is_allocable(size);
     allocates \result;
     assigns   \result \from size, __fc_heap_status;
     ensures   allocation: \fresh(\result,size);

   behavior deallocation:
     assumes   nonnull_ptr: ptr != \null;
     assumes   can_allocate: is_allocable(size);
     frees     ptr;
     ensures   freed: \allocable(ptr);
     ensures   freeable: \result == \null || \freeable(\result);

   behavior fail:
     assumes cannot_allocate: !is_allocable(size);
     allocates \nothing;
     frees     \nothing;
     assigns   \result \from size, __fc_heap_status;
     ensures   null_result: \result == \null;

   complete behaviors;
   disjoint behaviors allocation, fail;
   disjoint behaviors deallocation, fail;
  */
extern void *realloc(void *ptr, size_t size);


/* ISO C: 7.20.4 */

/*@
  assigns \exit_status \from \nothing;
  exits status: \exit_status != EXIT_SUCCESS;
  ensures never_terminates: \false;
 */
extern void abort(void) __attribute__ ((__noreturn__));

/*@ assigns \result \from \nothing ;*/
extern int atexit(void (*func)(void));

/*@ assigns \result \from \nothing ;*/
extern int at_quick_exit(void (*func)(void));

/*@
  assigns \exit_status \from status;
  exits status: \exit_status == status;
  ensures never_terminates: \false;
*/
extern void exit(int status) __attribute__ ((__noreturn__));

/*@
  assigns \nothing;
  ensures never_terminates: \false;
*/
extern void _Exit(int status) __attribute__ ((__noreturn__));

extern char *__fc_env[ARG_MAX] __attribute__((FRAMA_C_MODEL));

/*@
  requires valid_name: valid_read_string(name);
  assigns \result \from __fc_env[0..], indirect:name, name[0 ..];
  ensures null_or_valid_result: \result == \null || \valid(\result);
 */
extern char *getenv(const char *name);

/*@
  requires valid_string: valid_read_string(string);
  assigns __fc_env[0..] \from __fc_env[0..], string;
  assigns \result \from indirect:__fc_env[0..], indirect:string;
*/
extern int putenv(char *string);

/*@
  requires valid_name: valid_read_string(name);
  requires valid_value: valid_read_string(value);
  assigns \result, __fc_env[0..]
    \from __fc_env[0..], indirect:name, indirect:name[0 ..],
          indirect:value, indirect:value[0 ..], indirect:overwrite;
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int setenv(const char *name, const char *value, int overwrite);

/*@
  requires valid_name: valid_read_string(name);
  assigns \result, __fc_env[0..]
    \from __fc_env[0..], indirect:name, indirect:name[0 ..];
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int unsetenv(const char *name);

/*@
  assigns \nothing;
  ensures never_terminates: \false;
 */
extern void quick_exit(int status) __attribute__ ((__noreturn__));

/*@
  requires null_or_valid_string_command:
     command == \null || valid_read_string(command);
  assigns \result \from indirect:command, indirect:command[0 ..];
*/
extern int system(const char *command);

/* ISO C: 7.20.5 */

/* TODO: use one of the well known specification with high order compare :-) */
// NOTE: the assigns of function [compar] are not currently taken into account
// by ACSL. If [compar] is not purely functional, the result may be unsound.
// To ensure soundness, you should manually give a specification to the
// comparison function that is equivalent to:
//     assigns \result \from *(<type>*)a, *(<type>*)b;
// where <type> is the type of the compared arguments.
/*@
  requires valid_function_compar: \valid_function(compar);
  assigns \result \from indirect:key, ((char*)key)[0 .. size-1], base,
                        ((char*)base)[0 .. size * (nmemb-1)], indirect:nmemb,
                        indirect:size, indirect:*compar;
  ensures null_or_correct_result: \result == \null ||
          \subset(\result, (void*)(((char*)base) + (0 .. size * (nmemb-1))));
*/
extern void *bsearch(const void *key, const void *base, size_t nmemb,
                     size_t size, int (*compar)(const void *, const void *));

// NOTE: the assigns of function [compar] are not currently taken into account
// by ACSL. If [compar] is not purely functional, the result may be unsound.
/*@
  requires valid_function_compar: \valid_function(compar);
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
  behavior negative:
    assumes negative: j < 0;
    ensures opposite_result: \result == -j;
  behavior nonnegative:
    assumes nonnegative: j >= 0;
    ensures same_result: \result == j;
  complete behaviors;
  disjoint behaviors;
 */
extern int abs(int j);

/*@
  requires abs_representable: j > LONG_MIN ;
  assigns \result \from j;
  behavior negative:
    assumes negative: j < 0;
    ensures opposite_result: \result == -j;
  behavior nonnegative:
    assumes nonnegative: j >= 0;
    ensures same_result: \result == j;
  complete behaviors;
  disjoint behaviors;
 */
extern long int labs(long int j);

/*@
  requires abs_representable: j > LLONG_MIN ;
  assigns \result \from j;
  behavior negative:
    assumes negative: j < 0;
    ensures opposite_result: \result == -j;
  behavior nonnegative:
    assumes nonnegative: j >= 0;
    ensures same_result: \result == j;
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
  requires separation: \separated(pwc, s);
  assigns \result \from indirect:s, indirect:s[0 .. n-1], indirect:n,
                        __fc_mbtowc_state;
  assigns pwc[0 .. \result-1], __fc_mbtowc_state
    \from indirect:s, s[0 .. n-1], indirect:n, __fc_mbtowc_state;
  ensures consumed_range: \result <= n;
*/
extern int mbtowc(wchar_t * restrict pwc,
     const char * restrict s,
     size_t n);

//@ ghost extern int __fc_wctomb_state;

/*@
  //requires room_string: \valid(s + (0 .. __fc_mb_cur_max - 1));
  assigns \result \from indirect:wc, __fc_wctomb_state;
  assigns s[0 ..], __fc_wctomb_state \from wc, __fc_wctomb_state;
*/
extern int wctomb(char *s, wchar_t wc);

/* ISO C: 7.20.8 */

/*@
  requires separation: \separated(pwcs, s);
  assigns \result \from indirect:s, indirect:s[0 .. n-1], indirect:n;
  assigns pwcs[0 .. n-1] \from indirect:s, s[0 .. n-1], indirect:n;
*/
extern size_t mbstowcs(wchar_t * restrict pwcs,
     const char * restrict s,
     size_t n);

/*@
  requires separation: \separated(s, pwcs);
  assigns \result \from indirect:pwcs, indirect:pwcs[0 .. n-1], indirect:n;
  assigns s[0 .. n-1] \from indirect:pwcs, pwcs[0 .. n-1], indirect:n;
*/
extern size_t wcstombs(char * restrict s,
     const wchar_t * restrict pwcs,
     size_t n);

// Note: this specification should ideally use a more specific predicate,
//       such as 'is_allocable_aligned(alignment, size)'.
/*@
  requires valid_memptr: \valid(memptr);
  requires alignment_is_a_suitable_power_of_two:
    alignment >= sizeof(void*) &&
    ((size_t)alignment & ((size_t)alignment - 1)) == 0;
  allocates *memptr;
  assigns __fc_heap_status \from indirect:alignment, size, __fc_heap_status;
  assigns \result \from indirect:alignment, indirect:size,
                        indirect:__fc_heap_status;
  behavior allocation:
    assumes can_allocate: is_allocable(size);
    assigns __fc_heap_status \from indirect:alignment, size, __fc_heap_status;
    assigns \result \from indirect:alignment, indirect:size,
                          indirect:__fc_heap_status;
    ensures allocation: \fresh(*memptr,size);
    ensures result_zero: \result == 0;
  behavior no_allocation:
    assumes cannot_allocate: !is_allocable(size);
    assigns \result \from indirect:alignment;
    allocates \nothing;
    ensures result_non_zero: \result < 0 || \result > 0;
  complete behaviors;
  disjoint behaviors;
 */
extern int posix_memalign(void **memptr, size_t alignment, size_t size);

/*@
  // missing: requires 'last 6 characters of template must be XXXXXX'
  // missing: assigns \result, templat[0..] \from 'filesystem', 'RNG';
  requires valid_template: valid_string(templat);
  assigns templat[0..] \from \nothing;
  assigns \result \from \nothing;
  ensures result_error_or_valid_fd: \result == -1 ||
                                    0 <= \result < __FC_FOPEN_MAX;
 */
extern int mkstemp(char *templat);

__END_DECLS

__POP_FC_STDLIB
#endif

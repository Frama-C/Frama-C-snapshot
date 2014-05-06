/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2014                                               */
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
#include "__fc_define_size_t.h"
#include "__fc_define_wchar_t.h"
#include "__fc_define_restrict.h"

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
#define EXIT_FAILURE (-1)
#define EXIT_SUCCESS 0

#include "limits.h"

#define RAND_MAX __FC_RAND_MAX
#define MB_CUR_MAX __FC_MB_CUR_MAX

/*@ assigns \result \from nptr[..] ; */
double atof(const char *nptr);

/*@ assigns \result \from nptr[..] ; */
int atoi(const char *nptr);
/*@ assigns \result \from nptr[..] ; */
long int atol(const char *nptr);
/*@ assigns \result \from nptr[..] ; */
long long int atoll(const char *nptr);

/* See ISO C: 7.20.1.3 to complete these specifications */
/*@ assigns \result,*endptr \from nptr[0..],nptr ; */
double strtod(const char * restrict nptr,
     char ** restrict endptr);
/*@ assigns \result,*endptr \from nptr[0..],nptr ; */
float strtof(const char * restrict nptr,
     char ** restrict endptr);
/*@ assigns \result,*endptr \from nptr[0..],nptr ; */
long double strtold(const char * restrict nptr,
     char ** restrict endptr);

/* TODO: See ISO C 7.20.1.4 to complete these specifications */
/*@ assigns \result,*endptr \from nptr[0..]; */
long int strtol(
     const char * restrict nptr,
     char ** restrict endptr,
     int base);
/*@ assigns \result,*endptr \from nptr[0..]; */
long long int strtoll(
     const char * restrict nptr,
     char ** restrict endptr,
     int base);
/*@ assigns \result,*endptr \from nptr[0..]; */
unsigned long int strtoul(
     const char * restrict nptr,
     char ** restrict endptr,
     int base);
/*@ assigns \result,*endptr \from nptr[0..]; */
unsigned long long int strtoull(
     const char * restrict nptr,
     char ** restrict endptr,
     int base);

int __fc_random_counter __attribute__((unused));
const unsigned long __fc_rand_max = __FC_RAND_MAX;
/* ISO C: 7.20.2 */
/*@ assigns \result \from __fc_random_counter ;
  @ assigns __fc_random_counter ;
  @ ensures 0 <= \result <= __fc_rand_max ;
*/
int rand(void);

/*@ assigns __fc_random_counter \from seed ; */
void srand(unsigned int seed);

/* ISO C: 7.20.3.1 */
void *calloc(size_t nmemb, size_t size);

/*@ ghost extern int __fc_heap_status; */
/*@ axiomatic dynamic_allocation {
  @ predicate is_allocable(size_t n) // Can a block of n bytes be allocated?
  @ reads __fc_heap_status; 
  @ }
*/
 
/*@ allocates \result;
  @ assigns __fc_heap_status \from size, __fc_heap_status;
  @ assigns \result \from size, __fc_heap_status;
  @ behavior allocation:
  @   assumes is_allocable(size);
  @   assigns __fc_heap_status \from size, __fc_heap_status;
  @   assigns \result \from size, __fc_heap_status;
  @   ensures \fresh(\result,size);
  @ behavior no_allocation:
  @   assumes !is_allocable(size);
  @   assigns \result \from \nothing;
  @   allocates \nothing;
  @   ensures \result==\null;
  @ complete behaviors;
  @ disjoint behaviors;
  @*/
void *malloc(size_t size);

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
void free(void *p);

#ifdef FRAMA_C_MALLOC_POSITION
#define __FRAMA_C_STRINGIFY(x) #x
#define __FRAMA_C_XSTRINGIFY(x) __FRAMA_C_STRINGIFY(x)
#define FRAMA_C_LOCALIZE_WARNING (" file " __FILE__ " line " __FRAMA_C_XSTRINGIFY(__LINE__))
#define malloc(x) (__Frama_C_malloc_at_pos(x,__FILE__ "_function_" __func__ "_line_" __FRAMA_C_XSTRINGIFY(__LINE__)))
#define free(x) (__Frama_C_free_at_pos(x,FRAMA_C_LOCALIZE_WARNING))
void *__Frama_C_malloc_at_pos(size_t size,const char* file);
void __Frama_C_free_at_pos(void* ptr,const char* pos);
#endif

void *realloc(void *ptr, size_t size);

/* ISO C: 7.20.4 */

/*@ assigns \nothing;
  @ ensures \false; */
void abort(void);

/*@ assigns \result \from \nothing ;*/
int atexit(void (*func)(void));

/*@ assigns \result \from \nothing ;*/
int at_quick_exit(void (*func)(void));

/*@
  assigns \nothing;
  ensures \false;
*/
void exit(int status);

/*@
  assigns \nothing;
  ensures \false;
*/
void _Exit(int status);

/*@ assigns \nothing ;
  ensures \result == \null || \valid(\result) ;
 */
char *getenv(const char *name);

/*@
  assigns \nothing;
  ensures \false; */
void quick_exit(int status);

/*@ assigns \result \from string[..]; */
int system(const char *string);

/* ISO C: 7.20.5 */

/* TODO: use one of the well known specification with high order compare :-) */
/*@  assigns ((char*)\result)[..] \from ((char*)key)[..], ((char*)base)[..],
                                        nmemb, size, *compar;  */
void *bsearch(const void *key, const void *base,
     size_t nmemb, size_t size,
     int (*compar)(const void *, const void *));

/*@ assigns ((char*)base)[..] \from ((char*)base)[..], nmemb, size, *compar ;
 */
  void qsort(void *base, size_t nmemb, size_t size,
             int (*compar)(const void *, const void *));

/* ISO C: 7.20.6 */

/*@ 
  requires abs_representable:(int)(-j) == -j ;
  assigns \result \from j ;
*/
int abs(int j);

/*@ 
  requires abs_representable:(long)(-j) == -j ;
  assigns \result \from j ; */
long int labs(long int j);

/*@
  requires abs_representable:(long long)(-j) == -j ;
  assigns \result \from j ; */
long long int llabs(long long int j);

/*@ assigns \result \from numer,denom ; */
div_t div(int numer, int denom);
/*@ assigns \result \from numer,denom ; */
ldiv_t ldiv(long int numer, long int denom);
/*@ assigns \result \from numer,denom ; */
lldiv_t lldiv(long long int numer, long long int denom);

/* ISO C: 7.20.7 */
/*@ assigns \result \from s[0..], n ;*/
int mblen(const char *s, size_t n);

/*@ assigns \result, pwc[0..n-1] \from s[0..n-1], n ;
*/
int mbtowc(wchar_t * restrict pwc,
     const char * restrict s,
     size_t n);

/*@ assigns \result, s[0..] \from wc ; */
int wctomb(char *s, wchar_t wc);

/* ISO C: 7.20.8 */

/*@ assigns \result, pwcs[0..n-1] \from s[0..n-1], n ; */
size_t mbstowcs(wchar_t * restrict pwcs,
     const char * restrict s,
     size_t n);

/*@ assigns \result, s[0..n-1] \from pwcs[0..n-1] , n ; */
size_t wcstombs(char * restrict s,
     const wchar_t * restrict pwcs,
     size_t n);


#endif

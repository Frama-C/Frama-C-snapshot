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

/*! ***********************************************************************
 * \file  e_acsl_malloc.h
 *
 * \brief E-ACSL memory allocation bindings.
***************************************************************************/

/* Should be included after
 * printf, debug and assert but before the actual code */

#include <stddef.h>

#ifndef E_ACSL_MALLOC
#define E_ACSL_MALLOC

/** Define `aliasname` as a strong alias for `name`. */
# define strong_alias(name, aliasname) _strong_alias(name, aliasname)
# define _strong_alias(name, aliasname) \
  extern __typeof (name) aliasname __attribute__ ((alias (#name)));

/** Define `aliasname` as a weak alias for `name`. */
# define weak_alias(name, aliasname) _weak_alias (name, aliasname)
# define _weak_alias(name, aliasname) \
  extern __typeof (name) aliasname __attribute__ ((weak, alias (#name)));

# define preconcat(x,y) x ## y
# define concat(x,y) preconcat(x,y)

/** Prefix added to all jemalloc functions, e.g., an actual jemalloc `malloc`
 * is renamed to `__e_acsl_native_malloc` */
# define native_prefix __e_acsl_native_
# define alloc_func_def(f,...) concat(native_prefix,f)(__VA_ARGS__)
# define alloc_func_macro(f) concat(native_prefix,f)

/** Prefix added to public functions of E-ACSL public API */
# define public_prefix __e_acsl_
/** Make a strong alias from some function named `f` to __e_acsl_f */
# define public_alias(f) strong_alias(f, concat(public_prefix,f))
/** Make a strong alias from some function named `f1` to __e_acsl_f2 */
# define public_alias2(f1,f2) strong_alias(f1, concat(public_prefix,f2))

/* The following declares jemalloc allocation functions.
 * For instance:
 *  extern void *alloc_func_def(malloc, size_t);
 * becomes:
 *  extern void *__e_acsl_native_malloc(size_t); */
extern void  *alloc_func_def(malloc, size_t);
extern void  *alloc_func_def(calloc, size_t, size_t);
extern void  *alloc_func_def(realloc, void*, size_t);
extern void  *alloc_func_def(aligned_alloc, size_t, size_t);
extern int    alloc_func_def(posix_memalign, void **, size_t, size_t);
extern void   alloc_func_def(free,void*);

/* Shortcuts for allocation functions used within this RTL (so they are not
 * tracked). For example:
 *  native_malloc => __e_acsl_native_malloc */
# define native_malloc          alloc_func_macro(malloc)
# define native_realloc         alloc_func_macro(realloc)
# define native_calloc          alloc_func_macro(calloc)
# define native_posix_memalign  alloc_func_macro(posix_memalign)
# define native_aligned_alloc   alloc_func_macro(aligned_alloc)
# define native_free            alloc_func_macro(free)

/* \return a true value if x is a power of 2 and false otherwise */
static int powof2(size_t x) {
  while (((x & 1) == 0) && x > 1) /* while x is even and > 1 */
    x >>= 1;
  return (x == 1);
}
#endif

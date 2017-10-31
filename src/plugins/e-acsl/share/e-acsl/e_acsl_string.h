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
 * \file   e_acsl_string.h
 * \brief Replacement of system-wide \p <string.h> header for use with E-ACSL
 * runtime library.
 *
 * Intended use:
 *  - For the case when the sources are compiled using GCC prefer \p __builtin_
 *    versions of some of the string.h functions (e.g., \p memset). This is
 *    mostly because the GCC builtins are on average faster.
 *  - For the case it is not GCC system-wide versions should be used. This
 *    and the above options require \p E_ACSL_BUILTINS macro to be defined
 *    at compile-time.
 *  - For the case when the analysed program contains customised definitions
 *    of string.h functions use GLIBC-based implementations.
***************************************************************************/

#ifndef E_ACSL_STD_STRING_H
#define E_ACSL_STD_STRING_H

#ifndef E_ACSL_NO_COMPILER_BUILTINS
# define memset  __builtin_memset
# define memcmp  __builtin_memcmp
# define memcpy  __builtin_memcpy
# define memmove __builtin_memmove
# define strlen  __builtin_strlen
# define strcmp  __builtin_strcmp
# define strncmp __builtin_strncmp
#else
# include <string.h>
#endif

#include <stddef.h>
#include "e_acsl_malloc.h"

/* \brief Local version of `strcat` */
static char *nstrcat(char *dest, const char *src) {
  memcpy(dest + strlen(dest), src, strlen(src) + 1);
  return dest;
}

/* \brief Local version of `strdup` */
static char *nstrdup(const char *s) {
  if (s) {
    size_t len = strlen(s) + 1;
    void *n = private_malloc(len);
    return (n == NULL) ? NULL : (char*)memcpy(n, s, len);
  }
  return NULL;
}

/* \brief Append `src` to `dest` by re-allocating `dest`.
 *
 * `sappend` assumes that `dest` is either NULL (in which case it is
 * allocated on the heap) or a heap-allocated C string that can be passed
 * as an input to realloc.  If `delim` and `dest` are not NULLs them string
 * `delim` is appended to `dest` before `src`
 *
 * \return Result of concatenation of `dest` and `src` */
static char *sappend(char *dest, const char *src, const char *delim) {
  if (!dest && src)
    dest = nstrdup(src);
  else if (src && dest) {
    size_t ldelim = delim ? strlen(delim) : 0;
    size_t len = strlen(src) + strlen(dest) + 1;
    if (ldelim)
      len += ldelim;
    dest = private_realloc(dest, len);
    if (ldelim)
      dest = nstrcat(dest, delim);
    dest = nstrcat(dest, src);
  }
  return dest;
}

/** \brief Return 0 if C string `str` ends with string `pat` and a non-zero
 * value otherwise. The function assumes that both, `str` and `path` are valid,
 * NUL-terminated C strings. If any of the input strings are NULLs, a non-zero
 * value is returned. */
static int endswith(char *str, char *pat) {
  if (str && pat) {
    size_t slen = strlen(str);
    size_t plen = strlen(pat);
    if (slen >= plen) {
      str += slen - plen;
      return strncmp(str, pat, plen);
    }
  }
  return 1;
}

#define ZERO_BLOCK_SIZE 1024
static unsigned char zeroblock [ZERO_BLOCK_SIZE];

/** \brief Return a non-zero value if `size` bytes past address `p` are
 * nullified and zero otherwise. */
static int zeroed_out(const void *p, size_t size) {
  size_t lim = size/ZERO_BLOCK_SIZE,
         rem = size%ZERO_BLOCK_SIZE;
  unsigned char *pc = (unsigned char *)p;

  size_t i;
  for (i = 0; i < lim; i++) {
    if (memcmp(pc, &zeroblock, ZERO_BLOCK_SIZE))
      return 0;
    pc += ZERO_BLOCK_SIZE;
  }
  return !memcmp(pc, &zeroblock, rem);
}
#endif

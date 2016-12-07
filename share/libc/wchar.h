/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2016                                               */
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

/* ISO C: 7.25 */
#ifndef __FC_WCHAR_H
#define __FC_WCHAR_H

#include "__fc_define_wchar_t.h"
#include "__fc_define_size_t.h"
#include "__fc_define_file.h"
#include "features.h"

__BEGIN_DECLS

/*@
  assigns \result \from s, indirect:s[0 .. n-1], indirect:c, indirect:n;
  ensures \result == \null || \subset (\result, s+(0 .. n-1));
 */
wchar_t * wmemchr(const wchar_t *s, wchar_t c, size_t n);

/*@ assigns \result \from indirect:s1[0 .. n-1], indirect:s2[0 .. n-1], indirect:n; */
int wmemcmp(const wchar_t *s1, const wchar_t *s2, size_t n);

/*@
  requires \separated(dest+(0 .. n-1), src+(0 .. n-1));
  assigns dest[0 .. n-1] \from src[0 .. n-1], indirect:src, indirect:n;
  assigns \result \from dest;
  ensures \result == dest;
 */
wchar_t * wmemcpy(wchar_t *restrict dest, const wchar_t *restrict src, size_t n);

/*@
  assigns dest[0 .. n-1] \from src[0 .. n-1], indirect:src, indirect:n;
  assigns \result \from dest;
  ensures \result == dest;
*/
wchar_t * wmemmove(wchar_t *dest, const wchar_t *src, size_t n);

/*@
  assigns wcs[0 .. n-1] \from wc, indirect:n;
  assigns \result \from wcs;
  ensures \result == wcs;
*/
wchar_t * wmemset(wchar_t *wcs, wchar_t wc, size_t n);

/*@
  assigns dest[0 .. ] \from dest[0 .. ], indirect:dest, src[0 .. ], indirect:src;
  assigns \result \from dest;
  ensures \result == dest;
*/
wchar_t * wcscat(wchar_t *restrict dest, const wchar_t *restrict src);

/*@
  assigns \result \from wcs, indirect:wc;
  ensures \result == \null || \subset (\result, wcs+(0 .. ));
*/
wchar_t * wcschr(const wchar_t *wcs, wchar_t wc);

/*@ assigns \result \from indirect:s1[0 .. ], indirect:s2[0 .. ]; */
int wcscmp(const wchar_t *s1, const wchar_t *s2);

/*@
  assigns dest[0 .. ] \from src[0 .. ], indirect:src, dest[0 .. ], indirect:dest;
  assigns \result \from dest;
  ensures \result == dest;
 */
wchar_t * wcscpy(wchar_t *restrict dest, const wchar_t *restrict src);

/*@ assigns \result \from indirect:wcs[0 .. ], indirect:accept[0 .. ]; */
size_t wcscspn(const wchar_t *wcs, const wchar_t *accept);

// wcslcat is a BSD extension (non-C99, non-POSIX)
/*@
  assigns dest[0 .. ] \from dest[0 .. ], indirect:dest, src[0 .. n-1], indirect:src, indirect:n;
  assigns \result \from indirect:dest[0 .. ], indirect:src[0 .. n-1], indirect:n;
*/
size_t wcslcat(wchar_t *restrict dest, const wchar_t *restrict src, size_t n);

// wcslcpy is a BSD extension (non-C99, non-POSIX)
/*@
  requires \separated(dest+(0 .. n-1), src+(0 .. n-1));
  assigns dest[0 .. n-1] \from src[0 .. n-1], indirect:src, indirect:n;
  assigns \result \from indirect:dest[0 .. n-1], indirect:dest, indirect:src[0 .. n-1], indirect:src, indirect:n;
 */
size_t wcslcpy(wchar_t *dest, const wchar_t *src, size_t n);

/*@ assigns \result \from indirect:s[0 .. ]; */
size_t wcslen(const wchar_t *s);

/*@
  assigns dest[0 .. ] \from dest[0 .. ], indirect:dest, src[0 .. n-1], indirect:src, indirect:n;
  assigns \result \from dest;
  ensures \result == dest;
*/
wchar_t * wcsncat(wchar_t *restrict dest, const wchar_t *restrict src, size_t n);

/*@ assigns \result \from indirect:s1[0 .. n-1], indirect:s2[0 .. n-1], indirect:n; */
int wcsncmp(const wchar_t *s1, const wchar_t *s2, size_t n);

/*@
  requires \separated(dest+(0 .. n-1), src+(0 .. n-1));
  assigns dest[0 .. n-1] \from src[0 .. n-1], indirect:src, indirect:n;
  assigns \result \from dest;
  ensures \result == dest;
 */
wchar_t * wcsncpy(wchar_t *restrict dest, const wchar_t *restrict src, size_t n);

/*@
  assigns \result \from wcs, indirect:wcs[0 .. ], indirect:accept[0 .. ];
  ensures \result == \null || \subset (\result, wcs+(0 .. ));
*/
wchar_t * wcspbrk(const wchar_t *wcs, const wchar_t *accept);

/*@
  assigns \result \from wcs, indirect:wcs[0 .. ], indirect:wc;
  ensures \result == \null || \subset (\result, wcs+(0 .. ));
 */
wchar_t * wcsrchr(const wchar_t *wcs, wchar_t wc);

/*@ assigns \result \from indirect:wcs[0 .. ], indirect:accept[0 .. ]; */
size_t wcsspn(const wchar_t *wcs, const wchar_t *accept);

/*@
  assigns \result \from haystack, indirect:haystack[0 .. ], indirect:needle[0 .. ];
  ensures \result == \null || \subset (\result, haystack+(0 .. ));
 */
wchar_t * wcsstr(const wchar_t *haystack, const wchar_t *needle);


/* It is unclear whether these are more often in wchar.h or stdio.h */

int fwprintf(FILE * stream, const wchar_t * format, ...);

int swprintf(wchar_t * ws, size_t n, const wchar_t * format, ...);

int wprintf(const wchar_t * format, ...);


int wscanf(const wchar_t * format, ...);

int fwscanf(FILE * stream, const wchar_t * format, ...);

int swscanf(const wchar_t * str, const wchar_t * format, ...);

typedef struct { int __count; char __value[4]; } mbstate_t;

__END_DECLS

#endif

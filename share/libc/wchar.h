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

/* ISO C: 7.25 */
#ifndef __FC_WCHAR_H
#define __FC_WCHAR_H

#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_wchar_t.h"
#include "__fc_define_wint_t.h"
#include "__fc_define_size_t.h"
#include "__fc_define_file.h"
#include "__fc_string_axiomatic.h"

// Include <stdint.h> to retrieve definitions such as WCHAR_MIN and WINT_MAX,
// required by ISO C (and not necessarily respected by the glibc).
// Note that POSIX does not specify that all symbols in <stdint.h> can be
// made visible in wchar.h, but in practice this should be fine.
#include "stdint.h"

// ISO C requires the tag 'struct tm' (as declared in <time.h>) to be declared.
#include "time.h"

#include "string.h"

__BEGIN_DECLS

#ifndef WEOF
#define WEOF __FC_WEOF
#endif

/*@
  requires valid:
    valid_read_or_empty((char*)s, (size_t)(sizeof(wchar_t)*n))
    || \valid_read(((unsigned char*)s)+(0..wmemchr_off(s,c,n)));
  @ requires initialization:
        \initialized(s+(0..n - 1))
     || \initialized(s+(0..wmemchr_off(s,c,n)));
  @ requires danglingness:
        non_escaping(s, (size_t)(sizeof(wchar_t)*n))
     || non_escaping(s, (size_t)(sizeof(wchar_t)*(wmemchr_off(s,c,n)+1)));
  assigns \result \from s, indirect:s[0 .. n-1], indirect:c, indirect:n;
  ensures result_null_or_inside_s:
    \result == \null || \subset (\result, s+(0 .. n-1));
 */
extern wchar_t * wmemchr(const wchar_t *s, wchar_t c, size_t n);

/*@
  requires valid_s1: valid_read_or_empty(s1, (size_t)(sizeof(wchar_t)*n));
  requires valid_s2: valid_read_or_empty(s2, (size_t)(sizeof(wchar_t)*n));
  requires initialization:s1: \initialized(s1+(0..n-1));
  requires initialization:s2: \initialized(s2+(0..n-1));
  requires danglingness:s1: non_escaping(s1, (size_t)(sizeof(wchar_t)*n));
  requires danglingness:s2: non_escaping(s2, (size_t)(sizeof(wchar_t)*n));
  assigns \result \from indirect:s1[0 .. n-1], indirect:s2[0 .. n-1], indirect:n;
*/
extern int wmemcmp(const wchar_t *s1, const wchar_t *s2, size_t n);

/*@
  requires valid_dest: valid_or_empty(dest, (size_t)(sizeof(wchar_t)*n));
  requires valid_src: valid_read_or_empty(src, (size_t)(sizeof(wchar_t)*n));
  requires separation:dest:src: \separated(dest+(0 .. n-1), src+(0 .. n-1));
  assigns dest[0 .. n-1] \from src[0 .. n-1], indirect:src, indirect:n;
  assigns \result \from dest;
  ensures result_ptr: \result == dest;
 */
extern wchar_t * wmemcpy(wchar_t *restrict dest, const wchar_t *restrict src, size_t n);

/*@
  requires valid_src: \valid_read(src+(0..n-1));
  requires valid_dest: \valid(dest+(0..n-1));
  assigns dest[0 .. n-1] \from src[0 .. n-1], indirect:src, indirect:n;
  assigns \result \from dest;
  ensures result_ptr: \result == dest;
*/
extern wchar_t * wmemmove(wchar_t *dest, const wchar_t *src, size_t n);

/*@
  requires valid_wcs: \valid(wcs+(0..n-1));
  assigns wcs[0 .. n-1] \from wc, indirect:n;
  assigns \result \from wcs;
  ensures result_ptr: \result == wcs;
  ensures initialization:wcs: \initialized(wcs + (0 .. n-1));
  ensures contents_equal_wc: \subset(wcs[0 .. n-1], wc);
*/
extern wchar_t * wmemset(wchar_t *wcs, wchar_t wc, size_t n);

/*@
  requires valid_wstring_src: valid_read_wstring(src);
  requires valid_wstring_dest: valid_wstring(dest);
  requires room_for_concatenation: \valid(dest+(wcslen(dest)..wcslen(dest)+wcslen(src)));
  requires separation:\separated(dest+(0..wcslen(dest)+wcslen(src)),src+(0..wcslen(src)));
  assigns dest[0 .. ] \from dest[0 .. ], indirect:dest, src[0 .. ], indirect:src;
  assigns \result \from dest;
  ensures result_ptr: \result == dest;
*/
extern wchar_t * wcscat(wchar_t *restrict dest, const wchar_t *restrict src);

/*@
  requires valid_wstring_src: valid_read_wstring(wcs);
  assigns \result \from wcs, indirect:wcs[0 ..], indirect:wc;
  ensures result_null_or_inside_wcs:
    \result == \null || \subset(\result, wcs+(0..));
*/
extern wchar_t * wcschr(const wchar_t *wcs, wchar_t wc);

/*@
  requires valid_wstring_s1: valid_read_wstring(s1); // over-strong
  requires valid_wstring_s2: valid_read_wstring(s2); // over-strong
  assigns \result \from indirect:s1[0 .. ], indirect:s2[0 .. ];
*/
extern int wcscmp(const wchar_t *s1, const wchar_t *s2);

/*@
  requires valid_wstring_src: valid_read_wstring(src);
  requires room_wstring: \valid(dest+(0 .. wcslen(src)));
  requires separation:\separated(dest+(0..wcslen(src)),src+(0..wcslen(src)));
  assigns dest[0 .. wcslen(src)] \from src[0 .. wcslen(src)], indirect:src;
  assigns \result \from dest;
  ensures result_ptr: \result == dest;
 */
extern wchar_t * wcscpy(wchar_t *restrict dest, const wchar_t *restrict src);

/*@
  requires valid_wstring_wcs: valid_read_wstring(wcs);
  requires valid_wstring_accept: valid_read_wstring(accept);
  assigns \result \from indirect:wcs[0 .. ], indirect:accept[0 .. ];
 */
extern size_t wcscspn(const wchar_t *wcs, const wchar_t *accept);

// wcslcat is a BSD extension (non-C99, non-POSIX)
/*@
  requires valid_nwstring_src: valid_read_nwstring(src, n);
  requires valid_wstring_dest: valid_wstring(dest);
  requires room_for_concatenation: \valid(dest+(wcslen(dest)..wcslen(dest)+\min(wcslen(src), n)));
  requires separation:\separated(dest+(0..wcslen(dest)+wcslen(src)),src+(0..wcslen(src)));
  assigns dest[0 .. ] \from dest[0 .. ], indirect:dest, src[0 .. n-1], indirect:src, indirect:n;
  assigns \result \from indirect:dest[0 .. ], indirect:src[0 .. n-1], indirect:n;
*/
extern size_t wcslcat(wchar_t *restrict dest, const wchar_t *restrict src, size_t n);

// wcslcpy is a BSD extension (non-C99, non-POSIX)
/*@
  requires valid_wstring_src: valid_read_wstring(src);
  requires room_nwstring: \valid(dest+(0 .. n));
  requires separation:dest:src: \separated(dest+(0 .. n-1), src+(0 .. n-1));
  assigns dest[0 .. n-1] \from src[0 .. n-1], indirect:src, indirect:n;
  assigns \result \from indirect:dest[0 .. n-1], indirect:dest,
    indirect:src[0 .. n-1], indirect:src, indirect:n;
 */
extern size_t wcslcpy(wchar_t *dest, const wchar_t *src, size_t n);

/*@
  requires valid_string_s: valid_read_wstring(s);
  assigns \result \from indirect:s[0 .. wcslen(s)];
  ensures result_is_length: \result == wcslen(s);
 */
extern size_t wcslen(const wchar_t *s);

/*@
  requires valid_nwstring_src: valid_read_nwstring(src, n);
  requires valid_wstring_dest: valid_wstring(dest);
  requires room_for_concatenation: \valid(dest+(wcslen(dest)..wcslen(dest)+\min(wcslen(src), n)));
  requires separation:\separated(dest+(0..wcslen(dest)+wcslen(src)),src+(0..wcslen(src)));
  assigns dest[0 .. ] \from dest[0 .. ], indirect:dest, src[0 .. n-1], indirect:src, indirect:n;
  assigns \result \from dest;
  ensures result_ptr: \result == dest;
*/
extern wchar_t * wcsncat(wchar_t *restrict dest, const wchar_t *restrict src, size_t n);

/*@
  requires valid_wstring_s1: valid_read_wstring(s1); // over-strong
  requires valid_wstring_s2: valid_read_wstring(s2); // over-strong
  assigns \result \from indirect:s1[0 .. n-1], indirect:s2[0 .. n-1], indirect:n;
*/
extern int wcsncmp(const wchar_t *s1, const wchar_t *s2, size_t n);

/*@
  requires valid_wstring_src: valid_read_wstring(src);
  requires room_nwstring: \valid(dest+(0 .. n-1));
  requires separation:dest:src: \separated(dest+(0 .. n-1), src+(0 .. n-1));
  assigns dest[0 .. n-1] \from src[0 .. n-1], indirect:src, indirect:n;
  assigns \result \from dest;
  ensures result_ptr: \result == dest;
  ensures initialization: \initialized(dest+(0 .. n-1));
 */
extern wchar_t * wcsncpy(wchar_t *restrict dest, const wchar_t *restrict src, size_t n);

/*@
  requires valid_wstring_wcs: valid_read_wstring(wcs);
  requires valid_wstring_accept: valid_read_wstring(accept);
  assigns \result \from wcs, indirect:wcs[0 .. ], indirect:accept[0 .. ];
  ensures result_null_or_inside_wcs:
    \result == \null || \subset (\result, wcs+(0 .. ));
*/
extern wchar_t * wcspbrk(const wchar_t *wcs, const wchar_t *accept);

/*@
  requires valid_wstring_wcs: valid_read_wstring(wcs);
  assigns \result \from wcs, indirect:wcs[0 .. wcslen(wcs)], indirect:wc;
  ensures result_null_or_inside_wcs:
    \result == \null || \subset (\result, wcs+(0 .. ));
 */
extern wchar_t * wcsrchr(const wchar_t *wcs, wchar_t wc);

/*@
  requires valid_wstring_wcs: valid_read_wstring(wcs);
  requires valid_wstring_accept: valid_read_wstring(accept);
  assigns \result \from indirect:wcs[0 .. wcslen(wcs)],
                        indirect:accept[0 .. wcslen(accept)];
*/
extern size_t wcsspn(const wchar_t *wcs, const wchar_t *accept);

/*@
  requires valid_wstring_haystack: valid_read_wstring(haystack);
  requires valid_wstring_needle: valid_read_wstring(needle);
  assigns \result \from haystack, indirect:haystack[0 .. ], indirect:needle[0 .. ];
  ensures result_null_or_inside_haystack:
    \result == \null || \subset (\result, haystack+(0 .. ));
 */
extern wchar_t * wcsstr(const wchar_t *haystack, const wchar_t *needle);

/*@
  requires room_nwstring: \valid(ws+(0..n-1));
  requires valid_stream: \valid(stream);
  assigns ws[0..n-1] \from indirect:n, indirect:*stream;
  assigns \result \from ws, indirect:n, indirect:*stream;
  ensures result_null_or_same: \result == \null || \result == ws;
  ensures terminated_string_on_success:
    \result != \null ==> valid_wstring(ws);
 */
extern wchar_t *fgetws(wchar_t * restrict ws, int n, FILE * restrict stream);

/*@
  // Axiomatic used by the Variadic plugin to generate specifications
  // for some functions, e.g. swprintf().
  axiomatic wformat_length {
    //TODO: this logic function will be extended to handle variadic formats
    logic integer wformat_length{L}(wchar_t *format);
  }
*/

/* It is unclear whether these are more often in wchar.h or stdio.h */

extern int fwprintf(FILE * stream, const wchar_t * format, ...);

extern int swprintf(wchar_t * ws, size_t n, const wchar_t * format, ...);

extern int wprintf(const wchar_t * format, ...);


extern int wscanf(const wchar_t * format, ...);

extern int fwscanf(FILE * stream, const wchar_t * format, ...);

extern int swscanf(const wchar_t * str, const wchar_t * format, ...);

#ifndef __mbstate_t_defined
typedef struct __fc_mbstate_t { int __count; char __value[4]; } mbstate_t;
#define __mbstate_t_defined
#endif

__END_DECLS

__POP_FC_STDLIB
#endif

/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2008                                               */
/*    INRIA (Institut National de Recherche en Informatique et en         */
/*           Automatique)                                                 */
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
/**************************************************************************/

/* $Id: wchar.h,v 1.7 2008/11/17 16:32:43 uid570 Exp $ */

#ifndef _WCHAR_H_
#define _WCHAR_H_

#include <stddef.h>

/*@ requires valid_wstring(s);
  @ assigns \nothing;
  @ ensures \result == wcslen(s);
  @*/
size_t wcslen(const wchar_t *s);

/*@ requires \valid_range(dest,0,wcslen(src)) && valid_wstring(src);
  @ assigns dest[0..wcslen(src)];
  @ ensures valid_wstring(dest) && wcscmp(dest,src) == 0 && \result == dest;
  @*/
extern wchar_t *wcscpy(wchar_t *dest, const wchar_t *src);

/*@ requires \valid_range(dest,0,n - 1) && valid_wstring(src);
  @ assigns dest[0..n - 1];
  @ ensures \result == dest;
  @ behavior complete:
  @   assumes wcslen(src) < n;
  @   assigns dest[0..n - 1];
  @   ensures valid_wstring(dest) && wcscmp(dest,src) == 0;
  @ behavior partial:
  @   assumes n <= wcslen(src);
  @   assigns dest[0..n - 1];
  @   //ensures memcmp(dest,src,n) == 0;
  @*/
extern wchar_t *wcsncpy(wchar_t *dest, const wchar_t *src, size_t n);

/*@ requires valid_wstring(wcs) && valid_wstring(reject);
  @ assigns \nothing;
  @ ensures 0 <= \result <= wcslen(wcs);
  @*/
extern size_t wcscspn(wchar_t *wcs, const wchar_t *reject);

/*@ requires valid_wstring(wcs) && valid_wstring(accept);
  @ assigns \nothing;
  @ ensures 0 <= \result <= wcslen(wcs);
  @*/
extern size_t wcsspn(const wchar_t *wcs, const wchar_t *accept);

extern wchar_t *wcspbrk(const wchar_t *wcs, const wchar_t *accept);

extern wchar_t *wcsstr(const wchar_t *haystack, const wchar_t *needle);

extern wchar_t *wcstok(wchar_t *restrict s, 
		       const wchar_t *restrict delim,
		       wchar_t **restrict ptr);

// Allocate wide strings

/*@ requires valid_wstring(wcs);
  @ assigns \nothing;
  @ ensures \valid_range(\result,0,wcslen(wcs)) && wcscmp(\result,wcs) == 0;
  @*/
extern wchar_t *wcsdup(const wchar_t *wcs);

/*@ requires valid_wstring(wcs);
  @ assigns \nothing;
  @ ensures \valid_range(\result,0,minimum(wcslen(wcs),n)) 
  @         && valid_wstring(\result) && wcslen(\result) <= n
  @         && wcsncmp(\result,wcs,n) == 0;
  @*/
extern wchar_t *wcsndup(const wchar_t *wcs, size_t n);

#endif /* _WCHAR_H_ */

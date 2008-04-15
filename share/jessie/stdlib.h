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

/* $Id: stdlib.h,v 1.8 2008/11/10 13:33:44 uid570 Exp $ */

#ifndef _STDLIB_H_
#define _STDLIB_H_

#include <stddef.h>
#include <string.h>
#include <wchar.h>

// Conversion functions

/*@ requires valid_string(nptr);
  @ assigns \nothing;
  @*/
extern double atof(const char *nptr);

/*@ requires valid_string(nptr);
  @ assigns \nothing;
  @*/
extern int atoi(const char *nptr);

/*@ requires valid_string(nptr);
  @ assigns \nothing;
  @*/
extern long int atol(const char *nptr);

/*@ requires valid_string(nptr);
  @ assigns \nothing;
  @*/
extern long long int atoll(const char *nptr);

// Random numbers

extern int rand(void);

extern void srand(unsigned int seed);

// Memory management

/*@ assigns \nothing;
  @ ensures \valid_range((char*)\result,0,size - 1);
  @*/
extern void *malloc (size_t size);

/*@ assigns \nothing;
  @ ensures \valid_range((char*)\result,0,size * nmemb - 1);
  @*/
extern void *calloc (size_t nmemb, size_t size);

/*@ requires \valid((char*)ptr) || ptr == NULL;
  @ assigns \nothing;
  @ ensures \valid_range((char*)\result,0,size - 1);
  @*/
extern void *realloc (void *ptr, size_t size);

// [free] treated specially

// Program termination

/*@ assigns \nothing;
  @ ensures \false;
  @*/
extern void abort(void);

extern int atexit(void (*func)(void));

/*@ assigns \nothing;
  @ ensures \false;
  @*/
extern void exit(int status);

// Multi-byte conversion functions

/*@ requires valid_string(mbstr) && \valid_range(wcstr,0,n - 1);
  @ assigns wcstr[0..n - 1];
  @ ensures valid_wstring(wcstr) && wcslen(wcstr) < n;
  @*/
extern size_t mbstowcs (wchar_t *wcstr, const char *mbstr, size_t n);

/*@ requires valid_wstring(wcstr) && \valid_range(mbstr,0,n - 1);
  @ assigns mbstr[0..n - 1];
  @ ensures valid_string(mbstr) && strlen(mbstr) < n;
  @*/
extern size_t wcstombs (char *mbstr, const wchar_t *wcstr, size_t n);

/*@ //requires \valid_range(s,0,sizeof(wchar_t)-1) || s == NULL;
  @ // Commented out due to bug that keeps sizeof
  @ requires \valid(s) || s == NULL;
  @ assigns s[..];
  @ behavior zero:
  @   assumes s == NULL;
  @   assigns \nothing;
  @ behavior non_null:
  @   assumes s != NULL;
  @   assigns s[..];
  @*/
extern int wctomb(char *s, wchar_t wc);

#endif /* _STDLIB_H_ */

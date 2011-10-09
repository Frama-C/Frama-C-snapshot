/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2011                                               */
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

/* $Id: string.h,v 1.8 2008-11-19 17:41:56 uid570 Exp $ */

#ifndef __FC_STRING_H_
#define __FC_STRING_H_

#include "__fc_string_axiomatic.h"
#include "stddef.h"
#include "limits.h"
#include "__fc_define_restrict.h"

// Query memory

/*@ requires \valid_range((char*)s1,0,n - 1)
  @          && \valid_range((char*)s2,0,n - 1);
  @ assigns \nothing;
  @ ensures \result == memcmp((char*)s1,(char*)s2,n);
  @*/
extern int memcmp (const void *s1, const void *s2, size_t n);

/*@ requires \valid_range((char*)s,0,n - 1);
  @ assigns \nothing;
  @ behavior found:
  @   assumes memchr((char*)s,c,n);
  @   ensures \base_addr((char*)\result) == \base_addr((char*)s);
  @   ensures *(char*)\result == c;
  @ behavior not_found:
  @   assumes ! memchr((char*)s,c,n);
  @   ensures \result == \null;
  @*/
extern void *memchr(const void *s, int c, size_t n);

// Copy memory

/*@ requires \valid_range((char*)dest,0,n - 1)
  @          && \valid_range((char*)src,0,n - 1);
  @ requires \separated(((char *)dest)+(0..n-1),((char *)src)+(0..n-1));
  @ assigns ((char*)dest)[0..n - 1] \from ((char*)src)[0..n-1];
  @ ensures memcmp((char*)dest,(char*)src,n) == 0 && \result == dest;
  @ ensures \base_addr((char*)\result) == \base_addr((char*)dest);
  @*/
extern void *memcpy(void *restrict dest,
		    const void *restrict src, size_t n);

/*@ requires \valid_range((char*)dest,0,n - 1)
  @          && \valid_range((char*)src,0,n - 1);
  @ assigns ((char*)dest)[0..n - 1];
  @ ensures memcmp((char*)dest,(char*)src,n) == 0 && \result == dest;
  @ ensures \base_addr((char*)\result) == \base_addr((char*)dest);
  @*/
extern void *memmove(void *dest, const void *src, size_t n);

// Set memory

/*@ requires \valid_range((char*)s,0,n - 1);
  @ assigns ((char*)s)[0..n - 1];
  @ ensures memset((char*)s,c,n) && \result == s;
  @ ensures \base_addr((char*)\result) == \base_addr((char*)s);
  @*/
extern void *memset(void *s, int c, size_t n);

// Query strings

/*@ requires valid_string(s);
  @ assigns \nothing;
  @ ensures \result == strlen(s);
  @*/
extern size_t strlen (const char *s);

/*@ requires valid_string(s1) && valid_string(s2);
  @ assigns \nothing;
  @ ensures \result == strcmp(s1,s2);
  @*/
extern int strcmp (const char *s1, const char *s2);

/*@ requires valid_string(s1) && valid_string(s2);
  @ assigns \nothing;
  @ ensures \result == strncmp(s1,s2,n);
  @*/
extern int strncmp (const char *s1, const char *s2, size_t n);

/*@ requires valid_string(s1) && valid_string(s2);
  @ assigns \nothing;
  @*/
extern int strcoll (const char *s1, const char *s2);

/*@ requires valid_string(s);
  @ assigns \nothing;
  @ behavior found:
  @   assumes strchr(s,c);
  @   ensures *\result == c && \base_addr(\result) == \base_addr(s);
  @   ensures valid_string(\result);
  @ behavior not_found:
  @   assumes ! strchr(s,c);
  @   ensures \result == \null;
  @*/
extern char *strchr(const char *s, int c);

/*@ requires valid_string(s);
  @ assigns \nothing;
  @ behavior found:
  @   assumes strchr(s,c);
  @   ensures *\result == c && \base_addr(\result) == \base_addr(s);
  @   ensures valid_string(\result);
  @ behavior not_found:
  @   assumes ! strchr(s,c);
  @   ensures \result == \null;
  @*/
extern char *strrchr(const char *s, int c);

/*@ requires valid_string(s) && valid_string(reject);
  @ assigns \nothing;
  @ ensures 0 <= \result <= strlen(s);
  @*/
extern size_t strcspn(const char *s, const char *reject);

/*@ requires valid_string(s) && valid_string(accept);
  @ assigns \nothing;
  @ ensures 0 <= \result <= strlen(s);
  @*/
extern size_t strspn(const char *s, const char *accept);

/*@ requires valid_string(s) && valid_string(accept);
  @ assigns \nothing;
  @ ensures \result == 0 || \base_addr(\result) == \base_addr(s);
  @*/
extern char *strpbrk(const char *s, const char *accept);

/*@ requires valid_string(haystack) && valid_string(needle);
  @ assigns \nothing;
  @ ensures \result == 0
  @      || (\base_addr(\result) == \base_addr(haystack)
  @          && memcmp(\result,needle,strlen(needle)) == 0);
  @*/
extern char *strstr(const char *haystack, const char *needle);

/*@ requires (valid_string(s) || s == \null) && valid_string(delim);
  @ assigns \nothing;
  @*/
extern char *strtok(char *restrict s, const char *restrict delim);

/*@ assigns \nothing;
  @ ensures valid_string(\result);
  @*/
extern char *strerror(int errnum);

// Copy strings

/*@ requires \valid_range(dest,0,strlen(src)) && valid_string(src);
  @ assigns dest[0..strlen(src)];
  @ ensures strcmp(dest,src) == 0 && \result == dest;
  @ ensures \base_addr(\result) == \base_addr(dest);
  @*/
extern char *strcpy(char *restrict dest, const char *restrict src);

/*@ requires \valid_range(dest,0,n - 1) && valid_string(src);
  @ assigns dest[0..n - 1];
  @ ensures \result == dest;
  @ behavior complete:
  @   assumes strlen(src) < n;
  @   assigns dest[0..n - 1];
  @   ensures strcmp(dest,src) == 0;
  @   ensures \base_addr(\result) == \base_addr(dest);
  @ behavior partial:
  @   assumes n <= strlen(src);
  @   assigns dest[0..n - 1];
  @   ensures memcmp(dest,src,n) == 0;
  @*/
extern char *strncpy(char *restrict dest,
		     const char *restrict src, size_t n);

/*@ requires \valid_range(dest,0,strlen(dest) + strlen(src))
  @          && valid_string(dest) && valid_string(src);
  @ assigns dest[0..strlen(dest) + strlen(src)];
  @ ensures strlen(dest) == \old(strlen(dest) + strlen(src))
  @         && \result == dest;
  @ ensures \base_addr(\result) == \base_addr(dest);
  @*/
extern char *strcat(char *restrict dest, const char *restrict src);

/*@ requires \valid_range(dest,0,n - 1)
  @          && valid_string(dest) && valid_string(src);
  @ assigns dest[0..strlen(dest) + n - 1];
  @ ensures \result == dest;
  @ ensures \base_addr(\result) == \base_addr(dest);
  @ behavior complete:
  @   assumes strlen(src) <= n;
  @   assigns dest[0..strlen(dest) + strlen(src)];
  @   ensures strlen(dest) == \old(strlen(dest) + strlen(src));
  @ behavior partial:
  @   assumes n < strlen(src);
  @   assigns dest[0..strlen(dest) + n];
  @   ensures strlen(dest) == \old(strlen(dest)) + n;
  @*/
extern char *strncat(char *restrict dest, const char *restrict src, size_t n);

/*@ requires \valid_range(dest,0,n - 1) && valid_string(src);
  @ assigns dest[0..n - 1];
  @*/
extern size_t strxfrm (char *restrict dest,
		       const char *restrict src, size_t n);

// Allocate strings

/*@ requires valid_string(s);
  @ assigns \nothing;
  @ ensures \valid_range(\result,0,strlen(s)) && strcmp(\result,s) == 0;
  @*/
extern char *strdup (const char *s);

/*@ requires valid_string(s);
  @ assigns \nothing;
  @ ensures \valid_range(\result,0,minimum(strlen(s),n))
  @         && valid_string(\result) && strlen(\result) <= n
  @         && strncmp(\result,s,n) == 0;
  @*/
extern char *strndup (const char *s, size_t n);

#endif /* _STRING_H_ */

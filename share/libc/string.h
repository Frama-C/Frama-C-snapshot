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

#ifndef __FC_STRING_H_
#define __FC_STRING_H_

#include "__fc_string_axiomatic.h"
#include "stddef.h"
#include "limits.h"
#include "__fc_define_restrict.h"

// Query memory

/*@ requires \valid_read(((char*)s1)+(0..n - 1));
  @ requires \valid_read(((char*)s2)+(0..n - 1));
  @ assigns \result \from ((char*)s1)[0.. n-1], ((char*)s2)[0.. n-1];
  @ ensures \result == memcmp((char*)s1,(char*)s2,n);
  @*/
extern int memcmp (const void *s1, const void *s2, size_t n);

/*@ requires \valid_read(((char*)s)+(0..n - 1));
  @ assigns \result \from s, c, ((char*)s)[0..n-1];
  @ behavior found:
  @   assumes memchr((char*)s,c,n);
  @   ensures \base_addr(\result) == \base_addr(s);
  @   ensures *(char*)\result == c;
  @ behavior not_found:
  @   assumes ! memchr((char*)s,c,n);
  @   ensures \result == \null;
  @*/
extern void *memchr(const void *s, int c, size_t n);

// Copy memory

/*@ requires valid_dst: \valid(((char*)dest)+(0..n - 1));
  @ requires valid_src: \valid_read(((char*)src)+(0..n - 1));
  @ requires \separated(((char *)dest)+(0..n-1),((char *)src)+(0..n-1));
  @ assigns ((char*)dest)[0..n - 1] \from ((char*)src)[0..n-1];
  @ assigns \result \from dest;
  @ ensures memcmp((char*)dest,(char*)src,n) == 0;
  @ ensures \result == dest;
  @*/
extern void *memcpy(void *restrict dest,
		    const void *restrict src, size_t n);

/*@ requires valid_dst: \valid(((char*)dest)+(0..n - 1));
  @ requires valid_src: \valid_read(((char*)src)+(0..n - 1));
  @ assigns ((char*)dest)[0..n - 1] \from ((char*)src)[0..n-1];
  @ assigns \result \from dest;
  @ ensures memcmp((char*)dest,(char*)src,n) == 0;
  @ ensures \result == dest;
  @*/
extern void *memmove(void *dest, const void *src, size_t n);

// Set memory

/*@ requires \valid(((char*)s)+(0..n - 1));
  @ assigns ((char*)s)[0..n - 1] \from c;
  @ assigns \result \from s;
  @ ensures memset((char*)s,c,n);
  @ ensures \result == s;
  @*/
extern void *memset(void *s, int c, size_t n);

// Query strings

/*@ requires valid_string_src: valid_string(s);
  @ assigns \result \from s[0..];
  @ ensures \result == strlen(s);
  @*/
extern size_t strlen (const char *s);

/*@ requires valid_string_src: valid_string(s1);
  @ requires valid_string_src: valid_string(s2);
  @ assigns \result \from s1[0..], s2[0..];
  @ ensures \result == strcmp(s1,s2);
  @*/
extern int strcmp (const char *s1, const char *s2);

/*@ requires valid_string_src: valid_string(s1);
  @ requires valid_string_src: valid_string(s2);
  @ assigns \result \from s1[0 .. n-1], s2[0 ..n-1];
  @ ensures \result == strncmp(s1,s2,n);
  @*/
extern int strncmp (const char *s1, const char *s2, size_t n);

/*@ requires valid_string(s1) && valid_string(s2);
  @ assigns \nothing;
  @*/
extern int strcoll (const char *s1, const char *s2);

/*@ requires valid_string_src: valid_string(s);
  @ assigns \result \from s, s[0..],c;
  @ behavior found:
  @   assumes strchr(s,c);
  @   ensures *\result == c;
  @   ensures \base_addr(\result) == \base_addr(s);
  @   ensures s <= \result < s + strlen(s);
  @   ensures valid_string(\result);
  @   ensures \forall char* p; s<=p<\result ==> *p != c;
  @ behavior not_found:
  @   assumes ! strchr(s,c);
  @   ensures \result == \null;
  @ behavior default:
  @ ensures \result == \null || \base_addr(\result) == \base_addr(s);
  @*/
extern char *strchr(const char *s, int c);

/*@ requires valid_string_src: valid_string(s);
  @ assigns \result \from s, s[0..],c;
  @ behavior found:
  @   assumes strchr(s,c);
  @   ensures *\result == c;
  @   ensures \base_addr(\result) == \base_addr(s);
  @   ensures valid_string(\result);
  @ behavior not_found:
  @   assumes ! strchr(s,c);
  @   ensures \result == \null;
  @ behavior default:
  @ ensures \result == \null || \base_addr(\result) == \base_addr(s);
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
  @ assigns \result \from haystack, haystack[0..], needle, needle[0..];
  @ ensures \result == 0
  @      || (\base_addr(\result) == \base_addr(haystack)
  @          && memcmp(\result,needle,strlen(needle)) == 0);
  @*/
extern char *strstr(const char *haystack, const char *needle);

/*@ requires (valid_string(s) || s == \null) && valid_string(delim);
  @ assigns \nothing;
  @*/
extern char *strtok(char *restrict s, const char *restrict delim);

/*@ requires \valid(stringp) && valid_string(*stringp) && valid_string(delim);
  @ assigns *stringp \from delim[..], *stringp[..];
  @ assigns \result \from delim[..], *stringp[..];
  @*/
extern char *strsep (char **stringp, const char *delim);


/*@ assigns \result \from errnum;
  @ ensures valid_string(\result);
  @*/
extern char *strerror(int errnum);

// Copy strings

/*@ requires valid_string_src: valid_string(src);
  @ requires room_string: \valid(dest+(0..strlen(src)));
  @ assigns dest[0..strlen(src)] \from src[0..strlen(src)];
  @ assigns \result \from dest;
  @ ensures strcmp(dest,src) == 0;
  @ ensures \result == dest;
  @*/
extern char *strcpy(char *restrict dest, const char *restrict src);

/*@ 
  @ requires valid_string_src: valid_string(src);
  @ // FIXME: min(...) requires room_nstring: \valid(dest+(0 .. n)); 
  @ assigns dest[0..n - 1];
  @ ensures \result == dest;
  @ behavior complete:
  @   assumes strlen(src) < n;
  @   assigns dest[0..n - 1];
  @   ensures strcmp(dest,src) == 0;
  @ behavior partial:
  @   assumes n <= strlen(src);
  @   assigns dest[0..n - 1];
  @   ensures memcmp(dest,src,n) == 0;
  @*/
extern char *strncpy(char *restrict dest,
		     const char *restrict src, size_t n);

/*@ // missing: separation
  @ requires valid_string_src: valid_string(src);
  @ requires valid_string_dst: valid_string(dest);
  @ requires room_string: \valid(dest+(0..strlen(dest) + strlen(src)));
  @ assigns dest[strlen(dest)..strlen(dest) + strlen(src)]
  @   \from src[0..strlen(src)];
  @ ensures strlen(dest) == \old(strlen(dest) + strlen(src));
  @ assigns \result \from dest;
  @ ensures \result == dest;
  @*/
extern char *strcat(char *restrict dest, const char *restrict src);

/*@ // missing: separation
  @ requires valid_string_src: valid_string(src) || \valid(src+(0..n-1));
  @ requires valid_string_dst: valid_string(dest);
  @ requires room_string: \valid(dest + (strlen(dest) .. strlen(dest) + n)) ;
  @ assigns dest[strlen(dest) .. strlen(dest) + n] \from src[0..n];
  @ ensures \result == dest;
  @ behavior complete:
  @   assumes valid_string(src) && strlen(src) <= n;
  @   assigns dest[strlen(dest)..strlen(dest) + strlen(src)]
  @   \from src[0..strlen(src)];
  @   ensures strlen(dest) == \old(strlen(dest) + strlen(src));
  @ behavior partial:
  @   assumes ! (valid_string(src) && strlen(src) <= n);
  @   ensures strlen(dest) == \old(strlen(dest)) + n;
  @*/
extern char *strncat(char *restrict dest, const char *restrict src, size_t n);

/*@ requires \valid(dest+(0..n - 1));
  @ requires valid_string_src: valid_string(src);
  @ assigns dest[0..n - 1];
  @*/
extern size_t strxfrm (char *restrict dest,
		       const char *restrict src, size_t n);

// Allocate strings

/*@ requires valid_string_src: valid_string(s);
  @ assigns \nothing;
  @ ensures \valid(\result+(0..strlen(s))) && strcmp(\result,s) == 0;
  @*/
extern char *strdup (const char *s);

/*@ requires valid_string_src: valid_string(s); // FIXME
  @ assigns \nothing;
  @ ensures \valid(\result+(0..minimum(strlen(s),n)))
  @         && valid_string(\result) && strlen(\result) <= n
  @         && strncmp(\result,s,n) == 0;
  @*/
extern char *strndup (const char *s, size_t n);

/* Include strings.h: this is what BSD does, and glibc does something
   equivalent (having copied prototypes to string.h). */
#include <strings.h>

#endif /* _STRING_H_ */

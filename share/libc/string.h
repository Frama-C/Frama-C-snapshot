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

#ifndef __FC_STRING_H_
#define __FC_STRING_H_

#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_string_axiomatic.h"
#include "__fc_alloc_axiomatic.h"
#include "stddef.h"
#include "limits.h"

__BEGIN_DECLS

// Query memory

/*@ predicate non_escaping{L}(void *s, size_t n) =
      \forall size_t i; 0 <= i < n ==> !\dangling((char *)s + i);
*/

/*@
  predicate empty_block{L}(void *s) =
    \block_length((char*)s) == 0 && \offset((char*)s) == 0;

  // Note: the [\valid_read] below is intentional: if [malloc(0)] may return a
  // const base, then [memset(p, 0, 0)] requires p to have a readable
  // (but not writable) byte.
  predicate valid_or_empty{L}(void *s, size_t n) =
    (empty_block(s) || \valid_read((char*)s)) && \valid(((char*)s)+(0..n-1));

  predicate valid_read_or_empty{L}(void *s, size_t n) =
    (empty_block(s) || \valid_read((char*)s)) && \valid_read(((char*)s)+(1..n-1));
*/

/*@ requires valid_s1: valid_read_or_empty(s1, n);
  @ requires valid_s2: valid_read_or_empty(s2, n);
  @ requires initialization:s1: \initialized(((char*)s1)+(0..n - 1));
  @ requires initialization:s2: \initialized(((char*)s2)+(0..n - 1));
  @ requires danglingness:s1: non_escaping(s1, n);
  @ requires danglingness:s2: non_escaping(s2, n);
  @ assigns \result \from
  @   indirect:((char*)s1)[0.. n-1], indirect:((char*)s2)[0.. n-1];
  @ ensures logic_spec: \result == memcmp{Pre,Pre}((char*)s1,(char*)s2,n);
  @*/
extern int memcmp (const void *s1, const void *s2, size_t n);

/*@ requires valid:
         valid_read_or_empty(s, n)
         || \valid_read(((unsigned char*)s)+(0..memchr_off((char*)s,c,n)));
  @ requires initialization:
        \initialized(((unsigned char*)s)+(0..n - 1))
     || \initialized(((unsigned char*)s)+(0..memchr_off((char*)s,c,n)));
  @ requires danglingness:
        non_escaping(s, n)
     || non_escaping(s, (size_t)(memchr_off((char*)s,c,n)+1));
  @ assigns \result \from s, c, ((unsigned char*)s)[0..n-1];
  @ behavior found:
  @   assumes char_found: memchr((char*)s,c,n);
  @   ensures result_same_base: \base_addr(\result) == \base_addr(s);
  @   ensures result_char: *(char*)\result == c;
  @   ensures result_in_str: \forall integer i;
  @     0 <= i < n ==> *((unsigned char*)s+i) == c
  @     ==> \result <= s+i;
  @ behavior not_found:
  @   assumes char_not_found: !memchr((char*)s,c,n);
  @   ensures result_null: \result == \null;
  @*/
extern void *memchr(const void *s, int c, size_t n);

// Copy memory

/*@ requires valid_dest: valid_or_empty(dest, n);
  @ requires valid_src: valid_read_or_empty(src, n);
  @ requires separation:
  @   \separated(((char *)dest)+(0..n-1),((char *)src)+(0..n-1));
  @ assigns ((char*)dest)[0..n - 1] \from ((char*)src)[0..n-1];
  @ assigns \result \from dest;
  @ ensures copied_contents: memcmp{Post,Pre}((char*)dest,(char*)src,n) == 0;
  @ ensures result_ptr: \result == dest;
  @*/
extern void *memcpy(void *restrict dest,
		    const void *restrict src, size_t n);

/*@ requires valid_dest: valid_or_empty(dest, n);
  @ requires valid_src: valid_read_or_empty(src, n);
  @ assigns ((char*)dest)[0..n - 1] \from ((char*)src)[0..n-1];
  @ assigns \result \from dest;
  @ ensures copied_contents: memcmp{Post,Pre}((char*)dest,(char*)src,n) == 0;
  @ ensures result_ptr: \result == dest;
  @*/
extern void *memmove(void *dest, const void *src, size_t n);

// Set memory

/*@ requires valid_s: valid_or_empty(s, n);
  @ assigns ((char*)s)[0..n - 1] \from c;
  @ assigns \result \from s;
  @ ensures acsl_c_equiv: memset((char*)s,c,n);
  @ ensures result_ptr: \result == s;
  @*/
extern void *memset(void *s, int c, size_t n);

// Query strings

/*@ requires valid_string_s: valid_read_string(s);
  @ assigns \result \from indirect:s[0..];
  @ ensures acsl_c_equiv: \result == strlen(s);
  @*/
extern size_t strlen (const char *s);

/*@ requires valid_string_s: valid_read_nstring(s, n);
  @ assigns \result \from indirect:s[0..n-1], indirect:n;
  @ ensures result_bounded: \result == strlen(s) || \result == n;
  @*/
extern size_t strnlen (const char *s, size_t n);

/*@ requires valid_string_s1: valid_read_string(s1);
  @ requires valid_string_s2: valid_read_string(s2);
  @ assigns \result \from indirect:s1[0..], indirect:s2[0..];
  @ ensures acsl_c_equiv: \result == strcmp(s1,s2);
  @*/
extern int strcmp (const char *s1, const char *s2);

/*@ requires valid_string_s1: valid_read_nstring(s1, n); // over-strong
  @ requires valid_string_s2: valid_read_nstring(s2, n); // over-strong
  @ assigns \result \from indirect:s1[0 .. n-1], indirect:s2[0 ..n-1], indirect:n;
  @ ensures acsl_c_equiv: \result == strncmp(s1,s2,n);
  @*/
extern int strncmp (const char *s1, const char *s2, size_t n);

/*@ requires valid_string_s1: valid_read_string(s1); // over-strong
  @ requires valid_string_s2: valid_read_string(s2); // over-strong
  @ assigns \result \from indirect:s1[0..], indirect:s2[0..];
  @*/
extern int strcoll (const char *s1, const char *s2);

/*@ requires valid_string_s: valid_read_string(s);
  @ assigns \result \from s, s[0..],c;
  @ behavior found:
  @   assumes char_found: strchr(s,c);
  @   ensures result_char: *\result == (char)c;
  @   ensures result_same_base: \base_addr(\result) == \base_addr(s);
  @   ensures result_in_length: s <= \result <= s + strlen(s);
  @   ensures result_valid_string: valid_read_string(\result);
  @   ensures result_first_occur: \forall char* p; s<=p<\result ==> *p != (char)c;
  @ behavior not_found:
  @   assumes char_not_found: !strchr(s,c);
  @   ensures result_null: \result == \null;
  @ behavior default:
  @   ensures result_null_or_same_base:
  @     \result == \null || \base_addr(\result) == \base_addr(s);
  @*/
extern char *strchr(const char *s, int c);

/*@ requires valid_string_s: valid_read_string(s);
  @ assigns \result \from s, s[0..],c;
  @ behavior found:
  @   assumes char_found: strchr(s,c);
  @   ensures result_char: *\result == c;
  @   ensures result_same_base: \base_addr(\result) == \base_addr(s);
  @   ensures result_valid_string: valid_read_string(\result);
  @ behavior not_found:
  @   assumes char_not_found: !strchr(s,c);
  @   ensures result_null: \result == \null;
  @ behavior default:
  @   ensures result_null_or_same_base:
  @     \result == \null || \base_addr(\result) == \base_addr(s);
  @*/
extern char *strrchr(const char *s, int c);

/*@ requires valid_string_s: valid_read_string(s);
  @ requires valid_string_reject: valid_read_string(reject);
  @ assigns \result \from indirect:s[0..], indirect:reject[0..];
  @ ensures result_bounded: 0 <= \result <= strlen(s);
  @*/
extern size_t strcspn(const char *s, const char *reject);

/*@ requires valid_string_s: valid_read_string(s);
  @ requires valid_string_accept: valid_read_string(accept);
  @ assigns \result \from s[0..], accept[0..];
  @ assigns \result \from indirect:s[0..], indirect:accept[0..];
  @ ensures result_bounded: 0 <= \result <= strlen(s);
  @*/
extern size_t strspn(const char *s, const char *accept);

/*@ requires valid_string_s: valid_read_string(s);
  @ requires valid_string_accept: valid_read_string(accept);
  @ assigns \result \from s, s[0..], accept[0..];
  @ ensures result_null_or_same_base:
  @   \result == \null || \base_addr(\result) == \base_addr(s);
  @*/
extern char *strpbrk(const char *s, const char *accept);

/*@ requires valid_string_haystack: valid_read_string(haystack);
  @ requires valid_string_needle: valid_read_string(needle);
  @ assigns \result \from haystack, indirect:haystack[0..],
  @                       indirect:needle[0..];
  @ ensures result_null_or_in_haystack:
  @   \result == \null
  @   || (\subset(\result, haystack+(0..)) && \valid_read(\result)
  @       && memcmp{Pre,Pre}(\result,needle,strlen(needle)) == 0);
  @*/
extern char *strstr(const char *haystack, const char *needle);

/*@ requires valid_string_haystack: valid_read_string(haystack);
  @ requires valid_string_needle: valid_read_string(needle);
  @ assigns \result \from haystack, indirect:haystack[0..],
  @                       indirect:needle[0..];
  @ ensures result_null_or_in_haystack:
  @   \result == \null
  @   || (\subset(\result, haystack+(0..)) && \valid_read(\result));
  @*/
extern char *strcasestr (const char *haystack, const char *needle);

// internal state of strtok
char *__fc_strtok_ptr;

/*@ // missing: separation
  requires valid_string_delim: valid_read_string(delim);
  assigns s[0..] \from s[0..],
      indirect:s, indirect:__fc_strtok_ptr, indirect:delim[0..];
  assigns __fc_strtok_ptr[0..] \from __fc_strtok_ptr[0..],
      indirect:s, indirect:__fc_strtok_ptr, indirect:delim[0..];
  assigns \result \from s, __fc_strtok_ptr, indirect:s[0..],
      indirect:__fc_strtok_ptr[0..], indirect:delim[0..];
  assigns __fc_strtok_ptr \from \old(__fc_strtok_ptr), s,
                                indirect:__fc_strtok_ptr[0..],
                                indirect:delim[0..];
  behavior new_str:
    assumes s_not_null: s != \null;
    requires valid_string_s_or_delim_not_found:
      valid_string(s) ||
      (valid_read_string(s) &&
        \forall int i; 0 <= i < strlen(delim) ==> !strchr(s,delim[i]));
    assigns __fc_strtok_ptr \from s, indirect:s[0..], indirect:delim[0..];
    assigns s[0..] \from s[0..], indirect:s, indirect:delim[0..];
    assigns \result \from s, indirect:s[0..], indirect:delim[0..];
    ensures result_subset: \result == \null || \subset(\result, s+(0..));
    ensures ptr_subset: \subset(__fc_strtok_ptr, s+(0..));
  behavior resume_str:
    assumes s_null: s == \null;
    requires not_first_call: __fc_strtok_ptr != \null;
    assigns __fc_strtok_ptr[0..] \from __fc_strtok_ptr[0..],
                                       indirect:__fc_strtok_ptr,
                                       indirect:delim[0..];
    assigns __fc_strtok_ptr \from \old(__fc_strtok_ptr),
                                  indirect:__fc_strtok_ptr[0..],
                                  indirect:delim[0..];
    assigns \result \from __fc_strtok_ptr, indirect:__fc_strtok_ptr[0..],
                          indirect:delim[0..];
    ensures result_subset: \result == \null ||
                           \subset(\result, \old(__fc_strtok_ptr)+(0..));
    ensures ptr_subset: \subset(__fc_strtok_ptr, \old(__fc_strtok_ptr)+(0..));
  complete behaviors;
  disjoint behaviors;
*/
extern char *strtok(char *restrict s, const char *restrict delim);

/*@ // missing: separation
  requires valid_string_delim: valid_read_string(delim);
  requires valid_saveptr: \valid(saveptr);
  assigns s[0..] \from s[0..],
      indirect:s, indirect:*saveptr, indirect:delim[0..];
  assigns (*saveptr)[0..] \from (*saveptr)[0..],
      indirect:s, indirect:*saveptr, indirect:delim[0..];
  assigns \result \from s, *saveptr, indirect:s[0..],
      indirect:(*saveptr)[0..], indirect:delim[0..];
  assigns *saveptr \from \old(*saveptr), s,
                         indirect:(*saveptr)[0..],
                         indirect:delim[0..];
  behavior new_str:
    assumes s_not_null: s != \null;
    requires valid_string_s_or_delim_not_found:
      valid_string(s) ||
      (valid_read_string(s) &&
        \forall int i; 0 <= i < strlen(delim) ==> !strchr(s,delim[i]));
    assigns *saveptr \from s, indirect:s[0..], indirect:delim[0..];
    assigns s[0..] \from s[0..], indirect:s, indirect:delim[0..];
    assigns \result \from s, indirect:s[0..], indirect:delim[0..];
    ensures result_subset: \result == \null || \subset(\result, s+(0..));
    ensures initialization: \initialized(saveptr);
    ensures saveptr_subset: \subset(*saveptr, s+(0..));
  behavior resume_str:
    assumes s_null: s == \null;
    requires not_first_call: *saveptr != \null;
    requires initialization:saveptr: \initialized(saveptr);
    assigns (*saveptr)[0..] \from (*saveptr)[0..],
                                       indirect:*saveptr,
                                       indirect:delim[0..];
    assigns *saveptr \from \old(*saveptr),
                           indirect:(*saveptr)[0..],
                           indirect:delim[0..];
    assigns \result \from *saveptr, indirect:(*saveptr)[0..],
                          indirect:delim[0..];
    ensures result_subset: \result == \null ||
                           \subset(\result, \old(*saveptr)+(0..));
    ensures saveptr_subset: \subset(*saveptr, \old(*saveptr)+(0..));
  complete behaviors;
  disjoint behaviors;
*/
extern char *strtok_r(char *restrict s, const char *restrict delim, char **restrict saveptr);

/*@ requires valid_string_stringp: \valid(stringp) && valid_string(*stringp);
  @ requires valid_string_delim: valid_read_string(delim);
  @ assigns *stringp \from delim[..], *stringp[..];
  @ assigns \result \from delim[..], *stringp[..];
  @*/
extern char *strsep (char **stringp, const char *delim);

extern char __fc_strerror[64];
char * const __fc_p_strerror = __fc_strerror;

// Note: postcondition "result_nul_terminated" is only a temporary patch,
//       to help plug-ins which are currently unable to reduce the post-state
//       using only 'result_valid_string'.
/*@ assigns \result \from __fc_p_strerror, indirect:errnum;
  @ ensures result_internal_str: \result == __fc_p_strerror;
  @ ensures result_nul_terminated: \result[63] == 0;
  @ ensures result_valid_string: valid_read_string(\result);
  @*/
extern char *strerror(int errnum);

// Copy strings

/*@ requires valid_string_src: valid_read_string(src);
  @ requires room_string: \valid(dest+(0..strlen(src)));
  @ requires separation:
  @   \separated(dest+(0..strlen(src)), src+(0..strlen(src)));
  @ assigns dest[0..strlen(src)] \from src[0..strlen(src)];
  @ assigns \result \from dest;
  @ ensures equal_contents: strcmp(dest,src) == 0;
  @ ensures result_ptr: \result == dest;
  @*/
extern char *strcpy(char *restrict dest, const char *restrict src);

/*@ 
  @ requires valid_string_src: valid_read_string(src);
  @ requires room_nstring: \valid(dest+(0 .. n-1));
  @ requires separation:
  @   \separated(dest+(0..n-1), src+(0..n-1));
  @ assigns dest[0..n - 1] \from src[0..n-1];
  @ assigns \result \from dest;
  @ ensures result_ptr: \result == dest;
  @ ensures initialization: \initialized(dest+(0 .. n-1));
  @ behavior complete:
  @   assumes src_fits: strlen(src) < n;
  @   ensures equal_after_copy: strcmp(dest,src) == 0;
  @ behavior partial:
  @   assumes src_too_long: n <= strlen(src);
  @   ensures equal_prefix: memcmp{Post,Post}(dest,src,n) == 0;
  @*/
extern char *strncpy(char *restrict dest,
		     const char *restrict src, size_t n);

/*@ // Non-POSIX, but often present
  @ requires valid_string_src: valid_read_string(src);
  @ requires room_nstring: \valid(dest+(0..n-1));
  @ requires separation:
  @   \separated(dest+(0..n-1), src+(0..\max(n-1,strlen(src))));
  @ assigns dest[0..n-1] \from src[0..n-1];
  @ assigns \result \from indirect:src, indirect:src[0..n-1], indirect:n;
  @ ensures initialization: \initialized(dest+(0..\min(strlen(src),n-1)));
  @ ensures bounded_result: \result == strlen(src);
 */
size_t strlcpy(char * restrict dest, const char * restrict src, size_t n);

// stpcpy is POSIX.1-2008
/*@ requires valid_string_src: valid_read_string(src);
  @ requires room_string: \valid(dest+(0..strlen(src)));
  @ requires separation:
  @   \separated(dest+(0..strlen(src)), src+(0..strlen(src)));
  @ assigns dest[0..strlen(src)] \from src[0..strlen(src)];
  @ assigns \result \from dest;
  @ ensures equal_contents: strcmp(dest,src) == 0;
  @ ensures points_to_end: \result == dest + strlen(dest);
  @*/
extern char *stpcpy(char *restrict dest, const char *restrict src);

/*@ // missing: separation
  @ requires valid_string_src: valid_read_string(src);
  @ requires valid_string_dest: valid_string(dest);
  @ requires room_string: \valid(dest+(0..strlen(dest) + strlen(src)));
  @ assigns dest[strlen(dest)..strlen(dest) + strlen(src)]
  @   \from src[0..strlen(src)];
  @ ensures sum_of_lengths: strlen(dest) == \old(strlen(dest) + strlen(src));
  @ assigns \result \from dest;
  @ ensures initialization:dest:
  @   \initialized(dest+(0..\old(strlen(dest) + strlen(src))));
  @ ensures dest_null_terminated: dest[\old(strlen(dest) + strlen(src))] == 0;
  @ ensures result_ptr: \result == dest;
  @*/
extern char *strcat(char *restrict dest, const char *restrict src);

/*@ // missing: separation
  @ requires valid_nstring_src: valid_read_nstring(src, n);
  @ requires valid_string_dest: valid_string(dest);
  @ assigns dest[strlen(dest) .. strlen(dest) + n] \from src[0..n];
  @ assigns \result \from dest;
  @ ensures result_ptr: \result == dest;
  @ behavior complete:
  @   assumes valid_string_src_fits: valid_read_string(src) && strlen(src) <= n;
  @   requires room_string: \valid(dest + strlen(dest) + (0 .. strlen(src)));
  @   assigns dest[strlen(dest)..strlen(dest) + strlen(src)]
  @   \from src[0..strlen(src)];
  @   assigns \result \from dest;
  @   ensures sum_of_lengths: strlen(dest) == \old(strlen(dest) + strlen(src));
  @ behavior partial:
  @   assumes valid_string_src_too_large:
  @     !(valid_read_string(src) && strlen(src) <= n);
  @   requires room_string: \valid(dest + strlen(dest) + (0 .. n));
  @   assigns dest[strlen(dest)..strlen(dest) + n]
  @   \from src[0..strlen(src)];
  @   assigns \result \from dest;
  @   ensures sum_of_bounded_lengths: strlen(dest) == \old(strlen(dest)) + n;
  @*/
extern char *strncat(char *restrict dest, const char *restrict src, size_t n);

/*@ // Non-POSIX, but often present
  @ // missing: separation
  @ requires valid_string_src: valid_read_string(src);
  @ requires valid_string_dest: valid_string(dest);
  @ requires room_nstring: \valid(dest+(0..n-1));
  @ assigns dest[strlen(dest)..n] \from indirect:n, src[0..strlen(src)];
  @ assigns \result \from indirect:src, indirect:src[0..n-1], indirect:n;
  @ ensures bounded_result: \result == strlen(dest) + strlen(src);
  @*/
extern size_t strlcat(char *restrict dest, const char *restrict src, size_t n);

/*@ // missing: separation
  @ requires valid_dest: \valid(dest+(0..n - 1));
  @ requires valid_string_src: valid_read_string(src);
  @ assigns dest[0..n - 1] \from indirect:src[0..], indirect:n;
  @ assigns \result \from dest;
  @*/
extern size_t strxfrm (char *restrict dest,
		       const char *restrict src, size_t n);

// Allocate strings

/*@ requires valid_string_s: valid_read_string(s);
  @ allocates \result;
  @ assigns \result \from indirect:s[0..strlen(s)], indirect:__fc_heap_status;
  @ behavior allocation:
  @   assumes can_allocate: is_allocable(strlen(s));
  @   assigns __fc_heap_status \from indirect:s, __fc_heap_status;
  @   assigns \result \from indirect:s[0..strlen(s)], indirect:__fc_heap_status;
  @   ensures allocation: \fresh(\result,strlen(s));
  @   ensures result_valid_string_and_same_contents:
  @     valid_string(\result) && strcmp(\result,s) == 0;
  @ behavior no_allocation:
  @   assumes cannot_allocate: !is_allocable(strlen(s));
  @   assigns \result \from \nothing;
  @   allocates \nothing;
  @   ensures result_null: \result == \null;
  @*/
extern char *strdup (const char *s);

/*@ allocates \result;
  @ assigns \result \from indirect:s[0..strlen(s)], indirect:n,
  @                       indirect:__fc_heap_status;
  @ behavior allocation:
  @   assumes can_allocate: is_allocable(\min(strlen(s), n+1));
  @   assigns __fc_heap_status \from indirect:s, indirect:n, __fc_heap_status;
  @   assigns \result \from indirect:s[0..strlen(s)], indirect:n,
  @                         indirect:__fc_heap_status;
  @   ensures allocation: \fresh(\result,\min(strlen(s), n+1));
  @   ensures result_valid_string_bounded_and_same_prefix:
  @     \valid(\result+(0..\min(strlen(s),n))) &&
  @     valid_string(\result) && strlen(\result) <= n &&
  @     strncmp(\result,s,n) == 0;
  @ behavior no_allocation:
  @   assumes cannot_allocate: !is_allocable(\min(strlen(s), n+1));
  @   assigns \result \from \nothing;
  @   allocates \nothing;
  @   ensures result_null: \result == \null;
  @*/
extern char *strndup (const char *s, size_t n);

// More POSIX, non-C99 functions
extern char *stpncpy(char *restrict dest, const char *restrict src, size_t n);
//extern int strcoll_l(const char *s1, const char *s2, locale_t locale);
//extern char *strerror_l(int errnum, locale_t locale);
extern int strerror_r(int errnum, char *strerrbuf, size_t buflen);

extern char __fc_strsignal[64];
char * const __fc_p_strsignal = __fc_strsignal;

/*@ //missing: requires valid_signal(signum);
  @ assigns \result \from __fc_p_strsignal, indirect:signum;
  @ ensures result_internal_str: \result == __fc_p_strsignal;
  @ ensures result_nul_terminated: \result[63] == 0;
  @ ensures result_valid_string: valid_read_string(\result);
  @*/
extern char *strsignal(int signum);

//extern size_t strxfrm_l(char *restrict s1, const char *restrict s2, size_t n,
//                        locale_t locale);

__END_DECLS

/* Include strings.h: this is what BSD does, and glibc does something
   equivalent (having copied prototypes to string.h). */
#include "strings.h"

__POP_FC_STDLIB
#endif /* _STRING_H_ */

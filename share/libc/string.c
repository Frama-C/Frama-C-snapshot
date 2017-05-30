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

#include <string.h>
#include <stdint.h> // for uintptr_t
#include <stdlib.h> // for malloc()
__PUSH_FC_STDLIB

void* memcpy(void* restrict dest, const void* restrict src, size_t n)
{
  char* res = (char*)dest;
  for (size_t i = 0; i < n; i++) {
    ((char*)dest)[i] = ((char*)src)[i];
  }
  return res;
}

// memoverlap: auxiliary function that returns
//   0 if p[0..n-1] and q[0..n-1] do not overlap;
//   -1/+1 otherwise, according to whether p is before or after q in the memory
/*@
  assigns \result \from indirect:p, indirect:q, indirect:n;
  behavior separated:
    assumes  \separated(p + (0 .. n-1), q + (0 .. n-1));
    ensures  \result == 0;
  behavior not_separated_lt:
    assumes !\separated(p + (0 .. n-1), q + (0 .. n-1));
    assumes p <= q <  p + n;
    ensures  \result == -1;
  behavior not_separated_gt:
    assumes !\separated(p + (0 .. n-1), q + (0 .. n-1));
    assumes q <  p <= q + n;
    ensures  \result == 1;
  complete behaviors;
  disjoint behaviors;
*/
static int memoverlap(char const *p, char const *q, size_t n);

void* memmove(void* dest, const void* src, size_t n)
{
  if (n == 0) return dest;
  char *s = (char*)src;
  if (memoverlap(dest, src, n) <= 0) { /* default: copy up (use memcpy) */
    return memcpy(dest, src, n);
  } else { // beginning of dest overlaps with src: copy down
    char *d = (char*)dest;
    // to avoid unsigned overflow in the loop below, the '0' case is
    // done outside the loop (note: n == 0 has already been tested)
    for (size_t i = n-1; i > 0; i--)
      d[i] = s[i];
    d[0] = s[0];
    return dest;
  }
}

size_t strlen(const char *s)
{
  size_t i;
  for (i = 0; s[i] != 0; i++);
  return i;
}

void* memset (void* s, int c, size_t n)
{
  unsigned char *p = (unsigned char*)s;
  for (size_t i = 0; i < n; i++) {
    p[i] = c;
  }
  return s;
}

int strcmp(const char *s1, const char *s2)
{
  size_t i;
  for (i = 0; s1[i] == s2[i]; i++) {
    if (s1[i] == 0) return 0;
  }
  return (((unsigned char *)s1)[i] - ((unsigned char *)s2)[i]);
}

int strncmp(const char *s1, const char *s2, size_t n)
{
  for (size_t i = 0; i < n; i++) {
    if (s1[i] != s2[i])
      return ((unsigned char *)s1)[i] - ((unsigned char *)s2)[i];
    /* stop comparison when strings end */
    if (s1[i] == 0) return 0;
  }
  return 0;
}

int memcmp(const void *s1, const void *s2, size_t n)
{
  const unsigned char *p1, *p2;
  p1 = (const unsigned char *)s1;
  p2 = (const unsigned char *)s2;
  for (size_t i = 0; i < n; i++)
    if (p1[i] != p2[i]) return p1[i] - p2[i];
  return 0;
}

// NOTE: strcasecmp is in POSIX's strings.h but not in C99
// auxiliary function for strcasecmp
static int char_equal_ignore_case(char c1, char c2)
{
  if (c1 >= 'A' && c1 <= 'Z') c1 -= ('A' - 'a');
  if (c2 >= 'A' && c2 <= 'Z') c2 -= ('A' - 'a');
  if (c1 == c2) return 0;
  else return (int) ((unsigned char)c2 - (unsigned char)c1);
}

int strcasecmp(const char *s1, const char *s2)
{
  size_t i;
  for (i = 0; s1[i] != 0 && s2[i] != 0; i++) {
    int res = char_equal_ignore_case(s1[i], s2[i]);
    if (res != 0) return res;
  }
  if (s1[i] == 0 && s2[i] == 0) return 0;
  else if (s1[i] == 0) return -1;
  else return 1;
}

char* strcat(char *dest, const char *src)
{
  size_t i;
  size_t n = strlen(dest);
  for (i = 0; src[i] != 0; i++) {
    dest[n+i] = src[i];
  }
  dest[n+i] = 0;
  return dest;
}

/* From the strncat man page */
char* strncat(char *dest, const char *src, size_t n)
{
  size_t dest_len = strlen(dest);
  size_t i;
  for (i = 0; i < n; i++) {
    if (src[i] == 0) break;
    dest[dest_len + i] = src[i];
  }
  dest[dest_len + i] = 0;

  return dest;
}

char* strcpy(char *dest, const char *src)
{
  size_t i;
  for (i = 0; src[i] != 0; i++)
    dest[i] = src[i];
  dest[i] = 0;
  return dest;
}

char *strncpy(char *dest, const char *src, size_t n)
{
  size_t i;
  for (i = 0; i < n; i++) {
    dest[i] = src[i];
    if (src[i] == 0) break;
  }
  for (; i < n; i++)
    dest[i] = 0;
  return dest;
}

char *strchr(const char *s, int c)
{
  const char ch = c;
  size_t i;
  for (i = 0; s[i] != ch; i++)
    if (s[i] == 0) return NULL;
  return (char*)&s[i];
}

char *strrchr(const char *s, int c)
{
  const char ch = c;
  for (size_t i = strlen(s)+1; i > 0; i--)
    if (s[i-1] == ch) return (char *)&s[i-1];
  return NULL;
}

void *memchr(const void *s, int c, size_t n)
{
  const unsigned char ch = c;
  const unsigned char *ss = (const unsigned char *)s;
  for (size_t i = 0; i < n; i++)
    if (ss[i] == ch) return (void *)&ss[i];
  return NULL;
}

void *memrchr(const void *s, int c, size_t n)
{
  const unsigned char ch = c;
  const unsigned char *ss = (const unsigned char *)s;
  for (size_t i = n; i > 0; i--)
    if (ss[i-1] == ch) return (void *)&ss[i-1];
  return NULL;
}

char *strstr(const char *haystack, const char *needle)
{
  // special case: empty string starts everywhere
  if (needle[0] == 0) return (char*)haystack;
  for (size_t i = 0; haystack[i] != 0; i++) {
    size_t j;
    for (j = 0; haystack[i+j] != 0; j++) {
      if (haystack[i+j] != needle[j]) break;
    }
    if (needle[j] == 0) return (char*)&haystack[i];
  }
  return NULL;
}

char *strerror(int errnum)
{
  return "strerror message by Frama-C";
}

/* Warning: read considerations about malloc() in Frama-C */
char *strdup(const char *s)
{
  size_t l = strlen(s) + 1;
  char *p = malloc(l);
  memcpy(p, s, l);
  return p;
}

/* Warning: read considerations about malloc() in Frama-C */
char *strndup(const char *s, size_t n)
{
  /* find length up to n bytes */
  size_t l;
  for (l = 0; l < n; l++) {
    if (s[l] == 0) break;
  }
  char *p = malloc(l+1); /* include terminating '\0' */
  memcpy(p, s, l);
  p[l] = 0;
  return p;
}

__POP_FC_STDLIB

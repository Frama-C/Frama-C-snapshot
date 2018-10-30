/**************************************************************************/
/*                                                                        */
/*  This file is part of the Frama-C's E-ACSL plug-in.                    */
/*                                                                        */
/*  Copyright (C) 2012-2018                                               */
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
 * \file   e_acsl_libc_replacements.h
 * \brief  Drop-in replacements for C library functions
***************************************************************************/

#ifndef E_ACSL_LIBC_REPLACEMENTS_H
#define E_ACSL_LIBC_REPLACEMENTS_H

/************************************************************************/
/*** Support functionality {{{ ***/
/************************************************************************/

/* *** String validation {{{ */

/*! \brief Determine if `s` describes a C string up to length `n`.

   @return the index of `\0` character (i.e., the length of the string)
     if `s` is a valid pointer of byte-size `len`, and
       - `n` is negative and there is `\0` between `s` and the end of
       the block `s` points to.
       - `n` is positive and there is `\0` at index `i` (`i` < `n`)
       and `s+i` belongs to the same block as `s`.
    @return `n` if there is no `\0` between `s` and `s+n-1` but both
       `s` and `s+n-1` belong to the same block.

   @return -1 if `s` does not belong to tracked allocation
   @return -2 if `wrtbl` is set to a non-zero value and `s` is read-only
   @return -3 if there is no `\0` between `s` and the end of its block and
    `s+n-1` is unallocated or belongs to a different block.
   @return -4 if `n` is negative and `s` is not NUL-terminated */
static long valid_nstring(char *s, long n, int wrtbl) {
  if (n == 0)
    return n;

  int alc = allocated((uintptr_t)s, 1, (uintptr_t)s);
  if (alc) {
    if (wrtbl && readonly(s))
      return -2; /* Not writeable */
    long size = block_length(s) - offset(s);
    long i;
    for (i = 0; i < size; i++) {
      if (s[i] == '\0' || n == i)
        return i;
    }
    if (n == size)
      return n;
    if (n > size)
      return -3; /* Insufficient length */
    return -4; /* Not NUL-terminated */
  }
  return -1 /* Not allocated */;
}

/*!\brief Same as ::valid_nstring but for wide characters.

   This function is very similar to ::valid_nstring. It is possible make it
   more concise (say define it as a macro with types provided explicitly) yet
   it is left this way for readibility reasons. */
static long valid_nwstring(wchar_t *s, long n, int wrtbl) {
  if (n == 0)
    return n;

  int alc = allocated((uintptr_t)s, 1, (uintptr_t)s);
  if (alc) {
    if (wrtbl && readonly(s))
      return -2; /* Not writeable */
    long size = (block_length(s) - offset(s))/sizeof(wchar_t);
    long i;
    for (i = 0; i < size; i++) {
      if (s[i] == L'\0' || n == i)
        return i;
    }
    if (n == size)
      return n;
    if (n > size)
      return -3; /* Insufficient length */
    return -4; /* Not NUL-terminated */
  }
  return -1 /* Not allocated */;
}

/*! \brief Same as ::valid_nstring but check a NUL-terminated string */
static long inline valid_string(char *s, int wrtbl) {
  return valid_nstring(s, -1, wrtbl);
}

/*! \brief same as ::valid_string but for wide characters */
static long inline valid_wstring(wchar_t *s, int wrtbl) {
  return valid_nwstring(s, -1, wrtbl);
}

static long validate_string
  (char *s, long n, int wrtbl, const char *fun, const char *desc)
{
  long size = valid_nstring(s, n, wrtbl);

  switch(size) {
    case -1:
      vabort("%s: %sstring unallocated\n", fun, desc);
    case -2:
      vabort("%s: %sstring is not writable\n", fun, desc);
    case -3:
      vabort("%s: %sstring has insufficient length\n", fun, desc);
    case -4:
      vabort("%s: %sstring not NUL-terminated\n", fun, desc);
  }
  /* at this point negative return values should have been handled */
  vassert(size >= 0, "unexpected return value of %d\n", size);
  return size;
}

static inline long validate_writeable_string
  (char *s, long n, const char *fun, const char *desc)
{
  return validate_string(s, n, 1, fun, desc);
}

static inline long validate_allocated_string
   (char *s, long n, const char *fun, const char *desc)
{
  return validate_string(s, n, 0, fun, desc);
}
/* }}} */

/* *** Memory spaces {{{ */
/** \brief Return a true value if memory spaces given by intervals
    [s1, s1 + s1_sz] and [s2, s2 + s2_sz] are disjoint */
static inline int disjoint_spaces
  (uintptr_t s1, size_t s1_sz, uintptr_t s2, size_t s2_sz)
{
  return s1 + s1_sz <= s2 || s2 + s2_sz <= s1;
}

static inline void validate_allocated_space
  (void *p, size_t sz, const char *func, const char *space)
{
  if (!allocated((uintptr_t)p, sz, (uintptr_t)p)) {
    vabort("%s: unallocated (or insufficient) space in %s\n", func, space);
  }
}

static inline void validate_writeable_space(void *p, size_t sz,
    const char *func, const char *space) {
  if (!writeable((uintptr_t)p, sz, (uintptr_t)p)) {
    if (writeable((uintptr_t)p, 1, (uintptr_t)p)) {
      vabort("%s: insufficient space in %s, "
          "at least %lu bytes required\n", func, space, sz);
    } else {
      vabort("%s: %s space unallocated or cannot be written\n", func, space);
    }
  }
}

static inline void validate_overlapping_spaces
  (uintptr_t s1, size_t s1_sz, uintptr_t s2, size_t s2_sz, const char *func)
{
  if (!disjoint_spaces(s1, s1_sz, s2, s2_sz))
    vabort("%s: overlapping memory areas\n", func);
}
/* }}} */
/* }}} */

/************************************************************************/
/*** strlen/strcpy/strcat/strcmp {{{ ***/
/************************************************************************/

/* drop-in replacement for `strlen` */
size_t builtin_strlen(const char *s) {
  return validate_allocated_string((char*)s, -1, "strlen", "input ");
}

/* drop-in replacement for `strcpy` */
char *builtin_strcpy(char *dest, const char *src) {
  // `src` string should be a valid NUL-terminated C string
  size_t size =
    validate_allocated_string((char*)src, -1, "strlen", "source string ");
  /* `dest` should be writable and at least `size + 1` bytes long to
     accommodate the NUL-terminator */
  validate_writeable_space(dest, size + 1, "strlen", "destination string");
  /* source and destination strings should not overlap */
  validate_overlapping_spaces
    ((uintptr_t)dest, size + 1, (uintptr_t)src, size + 1, "strcpy");
  return strcpy(dest, src);
}

/* drop-in replacement for `strncpy` */
char *builtin_strncpy(char *dest, const char *src, size_t n) {
  /* `src` should be a valid string up to `nth` character */
  validate_allocated_string((char*)src, n, "strncpy", "source string ");
  /* `dest` should be allocated and writeable up to `nth` character */
  validate_writeable_space(dest, n, "strncpy", "destination string ");
  /* source and destination strings should not overlap */
  validate_overlapping_spaces((uintptr_t)dest, n, (uintptr_t)src, n, "strncpy");
  return strncpy(dest, src, n);
}

/* drop-in replacement for `strcmp` */
int builtin_strcmp(const char *s1, const char *s2) {
  /* both strings should be valid NUL-terminated strings */
  validate_allocated_string((char*)s1, -1, "strcmp", "string 1 ");
  validate_allocated_string((char*)s2, -1, "strcmp", "string 2 ");
  return strcmp(s1, s2);
}

/* drop-in replacement for `strcmp` */
int builtin_strncmp(const char *s1, const char *s2, size_t n) {
  /* both strings should be valid up to nth character */
  validate_allocated_string((char*)s1, n, "strncmp", "string 1 ");
  validate_allocated_string((char*)s2, n, "strncmp", "string 2 ");
  return strncmp(s1, s2, n);
}

/* drop-in replacement for `strcat` */
char *builtin_strcat(char *dest, const char *src) {
  long src_sz =
    validate_allocated_string((char*)src, -1, "strcat", "source string ");
  long dest_sz =
    validate_writeable_string((char*)dest, -1, "strcat", "destination string ");
  size_t avail_sz = block_length(dest) - offset(dest);
  if (!(avail_sz >= src_sz + dest_sz + 1)) {
    vabort("strcat: insufficient space in destination string, "
      "available: %lu bytes, requires at least %lu bytes\n",
      avail_sz, src_sz + dest_sz + 1);
  }
  validate_overlapping_spaces
    ((uintptr_t)src, src_sz + 1, (uintptr_t)dest, dest_sz + 1, "strcat");
  return strcat(dest, src);
}

/* drop-in replacement for `strncat` */
char *builtin_strncat(char *dest, const char *src, size_t n) {
  validate_allocated_string((char*)src, n, "strncat", "source string ");
  long dest_sz =
    validate_writeable_string((char*)dest, -1, "strcat", "destination string ");
  size_t avail_sz = block_length(dest) - offset(dest);
  if (!(avail_sz >= n + dest_sz + 1)) {
    vabort("strncat: insufficient space in destination string, "
      "available: %lu bytes, requires at least %lu bytes\n",
      avail_sz, n + dest_sz + 1);
  }
  validate_overlapping_spaces
    ((uintptr_t)src, n, (uintptr_t)dest, dest_sz, "strcat");
  return strncat(dest, src, n);
}
/* }}} */

/************************************************************************/
/*** memcpy/memcmp/memset/memmove {{{ ***/
/************************************************************************/

/* drop-in replacement for `memcpy` */
void *builtin_memcpy(void *dest, const void *src, size_t n) {
  validate_allocated_space((void*)src, n, "memcpy", "source space ");
  validate_writeable_space((void*)dest, n, "memcpy", "destination space ");
  validate_overlapping_spaces((uintptr_t)src, n, (uintptr_t)dest, n, "memcpy");
  return memcpy(dest, src, n);
}

/* drop-in replacement for `memset` */
void *builtin_memset(void *s, int c, size_t n) {
  validate_writeable_space((void*)s, n, "memset", "space ");
  return memset(s, c, n);
}

/* drop-in replacement for `memcmp` */
int builtin_memcmp(const void *s1, const void *s2, size_t n) {
  validate_allocated_space((void*)s1, n, "memcmp", "space 1 ");
  validate_allocated_space((void*)s2, n, "memcmp", "space 1 ");
  validate_overlapping_spaces((uintptr_t)s1, n, (uintptr_t)s2, n, "memcpy");
  return memcmp(s1, s2, n);
}

/* drop-in replacement for `memmove` */
void *builtin_memmove(void *dest, const void *src, size_t n) {
  validate_allocated_space((void*)src, n, "memcmp", "source space ");
  validate_writeable_space((void*)dest, n, "memcmp", "destination space ");
  return memmove(dest, src, n);
}

/* }}} */
#endif

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

/* ISO C: 7.20 */
#include "stdlib.h"
#include "__fc_builtin.h"
#include "ctype.h"

int abs (int i)
{
  if (i < 0)
    return -i;
  return i;
}

int atoi(const char *p)
{
  int n;
  int c, neg = 0;
  unsigned char	*up = (unsigned char *)p;

  if (!isdigit(c = *up)) {
    while (isspace(c))
      c = *++up;
    switch (c) {
    case '-':
      neg++;
      /* FALLTHROUGH */
    case '+':
      c = *++up;
    }
    if (!isdigit(c))
      return (0);
  }
  for (n = '0' - c; isdigit(c = *++up); ) {
    n *= 10; /* two steps to avoid unnecessary overflow */
    n += '0' - c; /* accum neg to avoid surprises at MAX */
  }
  return (neg ? n : -n);
}


/* This file defines 2 different implementation of malloc: you should define
   one of FRAMA_C_MALLOC_STACK or FRAMA_C_MALLOC_INDIVIDUAL to select the
   proper one. */

#ifdef FRAMA_C_MALLOC_INDIVIDUAL

/* This malloc must not be used if the analyzer cannot determine that there is
   only a finite number of calls to malloc. */

extern void *Frama_C_malloc_fresh(size_t size);

void *malloc(size_t size) {
  return Frama_C_malloc_fresh(size);
}

#else

#ifdef FRAMA_C_MALLOC_STACK

extern void * Frama_C_malloc_by_stack(size_t size);

void *malloc(size_t size) {
  return Frama_C_malloc_by_stack(size);
}

#else
#error Please define one of: FRAMA_C_MALLOC_STACK FRAMA_C_MALLOC_INDIVIDUAL
#endif
#endif

extern void Frama_C_free(void*base);
void free(void *p) {
  if (p) Frama_C_free(p);
}

void *calloc(size_t nmemb, size_t size)
{
  size_t l = nmemb * size;
  // test overflow, and fail if detected
  if (size != 0 && l / size != nmemb) {
    return 0;
  }
  char *p = malloc(l);
  if (p) Frama_C_memset(p, 0, l);
  return p;
}

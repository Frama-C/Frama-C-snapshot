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

/* ISO C: 7.20 */
#include "stdlib.h"
#include "__fc_builtin.h"
#include "ctype.h"
#include "string.h"
#include "limits.h"
#include "errno.h"
__PUSH_FC_STDLIB

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

void *calloc(size_t nmemb, size_t size)
{
  size_t l = nmemb * size;
  // test overflow, and fail if detected
  if (size != 0 && l / size != nmemb) {
    return 0;
  }
  char *p = malloc(l);
  if (p) memset(p, 0, l);
  return p;
}

char *__fc_env[ARG_MAX] __attribute__((FRAMA_C_MODEL));
// To provide for some non-determinism, __fc_initenv initializes the
// environment with an arbitrary string
#define __FC_INITENV_LEN 64
static char __fc_env_strings[__FC_INITENV_LEN];

static void __fc_initenv() {
  static char init;
  if (!init) {
    // -1 to ensure null-termination
    Frama_C_make_unknown(__fc_env_strings, __FC_INITENV_LEN-1);
    for (int i = 0; i < ARG_MAX; i++) {
      __fc_env[i] = __fc_env_strings + Frama_C_interval(0,__FC_INITENV_LEN-1);
    }
    init = 1;
  }
}

// all *env functions below follow this pattern:
// - check input argument(s);
// - initialize the environment, if not done already
// - perform the actual function

char *getenv(const char *name)
{
  //@ assert !strchr(name, '=');

  __fc_initenv();
  if (Frama_C_nondet(0, 1)) {
    return __fc_env[Frama_C_interval(0, ARG_MAX-1)];
  } else {
    return 0;
  }
}

int putenv(char *string)
{
  char *separator = strchr(string, '=');
  //@ assert string_contains_separator: separator != \null;
  //@ assert name_is_not_empty: separator != string;

  __fc_initenv();

  // possible cases:
  // 1. key in string not found in env:
  //    a. no more memory ==> ENOMEM
  //    b. available memory ==> modify env to point to string
  // 2. key in string found in env ==> modify an existing entry
  if (Frama_C_nondet(0, 1)) {
    if (Frama_C_nondet(0, 1)) {
      //TODO: errno = ENOMEM;
      return Frama_C_interval(INT_MIN, INT_MAX); // return a non-zero value
    }
    __fc_env[Frama_C_interval(0, ARG_MAX-1)] = string;
  }
  return 0;
}

int setenv(const char *name, const char *value, int overwrite)
{
  if (strchr(name, '=')) {
    //TODO: errno = EINVAL;
    return -1;
  }
  size_t namelen = strlen(name);
  if (namelen == 0) {
    //TODO: errno = EINVAL;
    return -1;
  }

  __fc_initenv();

  // possible cases:
  // 1. found 'name' and will overwrite, or did not find, but no more memory
  // 2. found 'name' but will not overwrite
  // 3. did not find name and has available memory
  if (Frama_C_nondet(0, 1)) {
    //TODO: errno = ENOMEM;
    return -1;
  } else {
    if (Frama_C_nondet(0, 1)) {
      Frama_C_make_unknown(__fc_env_strings, __FC_INITENV_LEN-1);
    }
    __fc_env[Frama_C_interval(0,ARG_MAX-1)] = __fc_env_strings + Frama_C_interval(0,__FC_INITENV_LEN-1);
    return 0;
  }
}

int unsetenv(const char *name)
{
  if (strchr(name, '=')) {
    //TODO: errno = EINVAL;
    return -1;
  }
  size_t namelen = strlen(name);
  if (namelen == 0) {
    //TODO: errno = EINVAL;
    return -1;
  }

  __fc_initenv();

  if (Frama_C_nondet(0, 1)) {
    __fc_env[Frama_C_interval(0,ARG_MAX-1)] = 0;
  }
  return 0;
}


unsigned short __fc_random48_counter[3];


// Note: this implementation does not check the alignment, since it cannot
//       currently be specified in the memory model of most plug-ins
int posix_memalign(void **memptr, size_t alignment, size_t size) {
  // By default, specifications in the libc are ignored for defined functions,
  // and since we do not actually use alignment, we need to check its validity.
  // The assertion below is the requires in the specification.
  /*@ assert alignment_is_a_suitable_power_of_two:
      alignment >= sizeof(void*) &&
      ((size_t)alignment & ((size_t)alignment - 1)) == 0;
  */
  *memptr = malloc(size);
  if (!*memptr) return ENOMEM;
  return 0;
}

__POP_FC_STDLIB

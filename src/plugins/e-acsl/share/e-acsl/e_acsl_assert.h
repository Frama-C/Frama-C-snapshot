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

/*! ***********************************************************************
 * \file  e_acsl_assert.h
 * \brief E-ACSL assertions and abort statements.
***************************************************************************/

#ifndef E_ACSL_ASSERT
#define E_ACSL_ASSERT

#include "e_acsl_string.h"
#include "e_acsl_printf.h"
#include "e_acsl_syscall.h"
#include "e_acsl_trace.h"

/*! \brief Drop-in replacement for abort function */
#define abort() exec_abort(__LINE__, __FILE__)

/*! \brief Output a message to error stream using printf-like format string
 * and abort the execution.
 *
 * This is a wrapper for \p eprintf combined with \p abort */
static void vabort(char *fmt, ...);

/*! \brief Drop-in replacement for system-wide assert macro */
#define assert(expr) \
  ((expr) ? (void)(0) : vabort("%s at %s:%d\n", "Assertion "#expr" failed", \
    __FILE__, __LINE__))

/*! \brief Assert with printf-like error message support */
#define vassert(expr, fmt, ...) \
  vassert_fail(expr, __LINE__, __FILE__, fmt, __VA_ARGS__)

static void exec_abort(int line, const char *file) {
#ifdef E_ACSL_DEBUG
  trace();
#endif
  kill(getpid(), SIGABRT);
}

/*! \brief Print a message to stderr and abort the execution */
static void vabort(char *fmt, ...) {
  va_list va;
  va_start(va,fmt);
  _format(NULL,_charc_stderr,fmt,va);
  va_end(va);
  abort();
}

static void vassert_fail(int expr, int line, char *file, char *fmt,  ...) {
  if (!expr) {
    char *afmt = "%s at %s:%d\n";
    char buf [strlen(fmt) + strlen(afmt) + PATH_MAX +  11];
    sprintf(buf, afmt, fmt, file, line);
    fmt = buf;

    va_list va;
    va_start(va,fmt);
    _format(NULL,_charc_stderr,fmt,va);
    va_end(va);
    abort();
  }
}

/*! \brief Default implementation of E-ACSL runtime assertions */
static void runtime_assert(int predicate, char *kind,
  char *fct, char *pred_txt, int line) {
  if (!predicate) {
    eprintf("%s failed at line %d in function %s.\n"
      "The failing predicate is:\n%s.\n", kind, line, fct, pred_txt);
    abort();
  }
}

/*! \brief Alias for runtime assertions.
 *
 * Since \p __e_acsl_assert is added as a weak alias user-defined implementation
 * of the \p __e_acsl_assert function will be preferred at link time. */
void __e_acsl_assert(int pred, char *kind, char *fct, char *pred_txt, int line)
  __attribute__ ((weak, alias ("runtime_assert")));

/* Instances of assertions shared accross different memory models */

/*! \brief Abort the execution if the size of the pointer computed during
 * instrumentation (\p _ptr_sz) does not match the size of the pointer used
 * by a compiler (\p void*) */
#define arch_assert(_ptr_sz) \
  vassert(_ptr_sz == sizeof(void*), \
    "Mismatch of instrumentation- and compile-time pointer sizes: " \
    "%lu vs %lu\n", _ptr_sz, sizeof(void*))

#endif

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

#ifndef E_ACSL_ASSERT_H
#define E_ACSL_ASSERT_H

#include <sys/types.h>
#include <signal.h>
#include <limits.h>
#include "e_acsl_alias.h"
#include "e_acsl_printf.h"
#include "e_acsl_string.h"
#include "e_acsl_trace.h"

#define runtime_assert export_alias(assert)

/*! \brief Drop-in replacement for abort function */
#define runtime_abort() exec_abort(__LINE__, __FILE__)

/*! \brief Output a message to error stream using printf-like format string
 * and abort the execution.
 *
 * This is a wrapper for \p eprintf combined with \p abort */
static void vabort(char *fmt, ...);

/*! \brief Assert with printf-like error message support */
#define vassert(expr, fmt, ...) \
  vassert_fail(expr, __LINE__, __FILE__, fmt, __VA_ARGS__)

/* This ::exec_abort replaces `abort` via a macro at the top of this file */
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
  runtime_abort();
}

static void vassert_fail(int expr, int line, char *file, char *fmt,  ...) {
  if (!expr) {
    char *afmt = "%s at %s:%d\n";
    char buf [strlen(fmt) + strlen(afmt) + PATH_MAX +  11];
    rtl_sprintf(buf, afmt, fmt, file, line);
    fmt = buf;

    va_list va;
    va_start(va,fmt);
    _format(NULL,_charc_stderr,fmt,va);
    va_end(va);
    runtime_abort();
  }
}

#ifdef E_ACSL_NO_ASSERT_FAIL
# define E_ACSL_ASSERT_NO_FAIL_DESC "pass through"
#else
# define E_ACSL_ASSERT_NO_FAIL_DESC "abort"
#endif

#ifndef E_ACSL_EXTERNAL_ASSERT
/*! \brief Default implementation of E-ACSL runtime assertions */
void runtime_assert(int predicate, char *kind, char *fct, char *pred_txt, int line) {
  if (!predicate) {
      STDERR("%s failed at line %d in function %s.\n"
        "The failing predicate is:\n%s.\n", kind, line, fct, pred_txt);
#ifndef E_ACSL_NO_ASSERT_FAIL /* Do fail on assertions */
#ifdef E_ACSL_FAIL_EXITCODE /* Fail by exit with a given code */
    exit(E_ACSL_FAIL_EXITCODE);
#else
    runtime_abort(); /* Raise abort signal */
#endif
#endif
  }
}
#endif

/* Instances of assertions shared accross different memory models */

/*! \brief Abort the execution if the size of the pointer computed during
 * instrumentation (\p _ptr_sz) does not match the size of the pointer used
 * by a compiler (\p void*) */
#define arch_assert(_ptr_sz) \
  vassert(_ptr_sz == sizeof(void*), \
    "Mismatch of instrumentation- and compile-time pointer sizes: " \
    "%lu vs %lu\n", _ptr_sz, sizeof(void*))

#endif

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
 * \file  e_acsl_rtl.c
 * \brief RTL configuration and assembly
***************************************************************************/

/* Get default definitions and macros e.g., PATH_MAX */
#ifndef _DEFAULT_SOURCE
# define _DEFAULT_SOURCE 1
#endif

#include "e_acsl_alias.h"
#include "e_acsl_malloc.h"
#include "e_acsl_string.h"
#include "e_acsl_bits.h"
#include "e_acsl_printf.h"
#include "e_acsl_debug.h"
#include "e_acsl_shexec.h"
#include "e_acsl_trace.h"
#include "e_acsl_assert.h"
#include "e_acsl_floating_point.h"
#include "e_acsl_safe_locations.h"
#include "e_acsl_temporal_timestamp.h"
#include "e_acsl.h"

/* Memory model settings
 *    Memory model:
 *      E_ACSL_BITTREE_MMODEL - use Patricia-trie (tree-based) memory model, or
 *      E_ACSL_SEGMENT_MMODEL - use segment-based (shadow) memory model
 *    Verbosity level:
 *      E_ACSL_VERBOSE - put an executable in verbose mode that
 *        prints extra messages (unset by default)
 *    Debug Features:
 *      E_ACSL_DEBUG - enable debug features in RTL (unset by default)
 *      E_ACSL_DEBUG_VERBOSE - verbose debug output (via DVLOG macro)
 *      E_ACSL_DEBUG_LOG - name of the log file where debug messages are
 *        output. The file name should be unquoted string with '-'
 *        (set by default) indicating a standard stream
 *    Validity:
 *      E_ACSL_WEAK_VALIDITY - use notion of weak validity
 *        Given an expression `(p+i)`, where `p` is a pointer and `i` is an
 *        integer offset weak validity indicates that `(p+i)` is valid if it
 *        belongs to memory allocation. In strong validity `(p+i)` is valid
 *        iff both `p` and `(p+i)` belong to memory allocation and to the same
 *        memory block.
 *    Temporal analysis:
 *      E_ACSL_TEMPORAL - enable temporal analysis in RTL
 *    Assertions:
 *      E_ACSL_NO_ASSERT_FAIL - do not issue abort signal of E-ACSL
 *        assertion failure
 *      E_ACSL_FAIL_EXITCODE - do not issue abort signal but exit with a
 *        given code
 *    Shadow spaces (only for segment model):
 *      E_ACSL_STACK_SIZE - size (in MB) of the tracked program stack
 *      E_ACSL_HEAP_SIZE - size (in MB) of the tracked program heap
 *    String functions:
 *      E_ACSL_NO_COMPILER_BUILTINS - if undefined (default) then use
 *      compiler builtin string functions (e.g., memset -> __builtin_memset)
 *    Behaviour of assert:
 *      E_ACSL_EXTERNAL_ASSERT - if this macro is defined then function
 *      `__e_acsl_assert` is excluded from compilation. This is to allow
 *      providing alternative definitions of assertions by users.
 *    Memory deallocation:
 *      E_ACSL_FREE_VALID_ADDRESS -- Clause 7.20.3.2 of C99 standard states
 *      that NULL is a valid input to free:
 *        "The free function causes the space pointed to by ptr [its argument]
 *         to be deallocated, that is, made available for further allocation.
 *         If ptr is a null pointer, no action occurs."
 *      Yet, some tools insist that it is a bug. For instance, there is a
 *      bunch of test cases in Toyota ITC Benchmarks. To make such tools
 *      happy the following option is introduced. By default it should be
 *      undefined (disabled) though.
*/

/* Functionality tracking leaks is shared between models */
#include "e_acsl_leak.h"

/* Print a header indicating current configuration of a run to STDIN. */
static void describe_run();

/* Select memory model, either segment-based or bittree-based model should
   be defined */
#if defined E_ACSL_SEGMENT_MMODEL
# include "segment_model/e_acsl_segment_mmodel.c"
#elif defined E_ACSL_BITTREE_MMODEL
# include "bittree_model/e_acsl_bittree_mmodel.c"
#else
# error "No E-ACSL memory model defined. Aborting compilation"
#endif

/* This header contains temporal analysis shared by both models.
   It should be here as it uses differently defined macros */
#include "e_acsl_temporal.h"

#ifdef E_ACSL_WEAK_VALIDITY
# define E_ACSL_VALIDITY_DESC "weak"
#else
# define E_ACSL_VALIDITY_DESC "strong"
#endif

/* Print basic configuration before each run */
static void describe_run() {
#if defined(E_ACSL_VERBOSE) || defined(E_ACSL_DEBUG)
  rtl_printf("/* ========================================================= */\n");
  rtl_printf(" * E-ACSL instrumented run\n" );
  rtl_printf(" * Memory tracking: %s\n", E_ACSL_MMODEL_DESC);
#ifdef E_ACSL_SEGMENT_MMODEL
  rtl_printf(" *   Heap  %d MB\n", E_ACSL_HEAP_SIZE);
  rtl_printf(" *   Stack %d MB\n", E_ACSL_STACK_SIZE);
#endif
  rtl_printf(" * Temporal checks: %s\n", E_ACSL_TEMPORAL_DESC);
  rtl_printf(" * Execution mode:  %s\n", E_ACSL_DEBUG_DESC);
  rtl_printf(" * Assertions mode: %s\n", E_ACSL_ASSERT_NO_FAIL_DESC);
  rtl_printf(" * Validity notion: %s\n", E_ACSL_VALIDITY_DESC);
  rtl_printf("/* ========================================================= */\n");
#endif
}

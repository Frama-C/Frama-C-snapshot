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
 * \file  e_acsl_memory_mmodel.c
 * \brief Configuration macros and RTL assembly
***************************************************************************/

#include <sys/mman.h>
#include <errno.h>
#include <sys/resource.h>

#include "e_acsl_string.h"
#include "e_acsl_bits.h"
#include "e_acsl_printf.h"
#include "e_acsl_debug.h"
#include "e_acsl_assert.h"
#include "e_acsl_malloc.h"
#include "e_acsl_safe_locations.h"

/*    Memory model:
 *      E_ACSL_BITTREE_MMODEL - use Patricia-trie (tree-based) memory model, or
 *      E_ACSL_SEGMENT_MMODEL - use segment-based (shadow) memory model
 *    Debug Features:
 *      E_ACSL_DEBUG - enable debug features in RTL
 *    Extra messages:
 *      E_ACSL_IDENTIFY - print a message showing run configuration
*/

/* Print basic configuration before each run */
static void identify_run() {
#ifdef  E_ACSL_IDENTIFY
  printf("/* ========================================================= */\n");
  printf(" * E-ACSL instrumented run with: "
#ifndef E_ACSL_DEBUG
  "NO-"
#endif
  "DEBUG\n");
  printf("/* ========================================================= */\n");
#endif
}

#if defined E_ACSL_SEGMENT_MMODEL
#  include "segment_model/e_acsl_segment_mmodel.c"
#elif defined E_ACSL_BITTREE_MMODEL
#  include "bittree_model/e_acsl_bittree_mmodel.c"
#else
#  error "No EACSL memory model defined. Aborting compilation"
#endif

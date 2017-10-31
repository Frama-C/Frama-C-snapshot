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
 * \file  e_acsl_leak.h
 *
 * \brief Functionality to report/track memory leaks. Shared between models
***************************************************************************/

#ifndef E_ACSL_HEAP_LEAK_H
#define E_ACSL_HEAP_LEAK_H

#include "e_acsl.h"

/* Variable tracking byte-count of user-allocated heap memory.
   Visible externally via __e_acsl_ prefix */
size_t heap_allocation_size = 0;

/* Variable tracking count of heap memory blocks */
static size_t heap_allocated_blocks = 0;

/* Return the number of bytes in heap application allocation */
size_t get_heap_allocation_size(void) {
  return heap_allocation_size;
}

/* Return the number of blocks in heap application allocation */
static inline size_t get_heap_allocated_blocks(void) {
  return heap_allocated_blocks;
}

/* Update heap allocation stats */
static void update_heap_allocation(long size) {
  heap_allocation_size += size;
  if (size > 0)
    heap_allocated_blocks++;
  else if (size < 0)
    heap_allocated_blocks--;
}

static void report_heap_leaks() {
#if defined(E_ACSL_VERBOSE) || defined(E_ACSL_DEBUG)
  size_t size = get_heap_allocation_size();
  size_t blocks = get_heap_allocated_blocks();
  if (size) {
    rtl_printf(" *** WARNING: Leaked %lu bytes of heap memory in %ld block%s\n",
      size, blocks, (blocks == 1) ? "" : "s");
  }
#endif
}
#endif

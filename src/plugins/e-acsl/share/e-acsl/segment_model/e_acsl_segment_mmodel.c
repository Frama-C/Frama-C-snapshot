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
 * \file   e_acsl_segment_mmodel.c
 * \brief  Implementation of E-ACSL public API for a segment (shadow) memory
 *   model. See e_acsl_mmodel_api.h for details.
***************************************************************************/

static int valid(void*, size_t, void*, void*);
static int valid_read(void*, size_t, void*, void*);

#include "e_acsl_shadow_layout.h"
#include "e_acsl_segment_tracking.h"
#include "e_acsl_mmodel_api.h"

static void * store_block(void * ptr, size_t size) {
  /* Only stack-global memory blocks are recorded explicitly via this function.
   * Heap blocks should be tracked internally using memory allocation functions
   * such as malloc or calloc. */
  shadow_alloca(ptr, size);
  return ptr;
}

static void delete_block(void * ptr) {
  /* Block deletion should be performed on stack/global addresses only,
   * heap blocks should be deallocated manually via free/cfree/realloc. */
  shadow_freea(ptr);
}

static void * store_block_duplicate(void * ptr, size_t size) {
  if (allocated((uintptr_t)ptr, size, (uintptr_t)ptr))
    delete_block(ptr);
  shadow_alloca(ptr, size);
  return ptr;
}

static void full_init(void * ptr) {
  initialize(ptr, block_length(ptr));
}

static void mark_readonly(void * ptr) {
  mark_readonly_region((uintptr_t)ptr, block_length(ptr));
}

/* ****************** */
/* E-ACSL annotations */
/* ****************** */

/** \brief Return 1 if a given memory location is read-only and 0 otherwise */
static int readonly (void *ptr) {
  uintptr_t addr = (uintptr_t)ptr;
  return IS_ON_GLOBAL(addr) && global_readonly(addr) ? 1 : 0;
}

static int valid(void * ptr, size_t size, void *ptr_base, void *addr_of_base) {
  return allocated((uintptr_t)ptr, size, (uintptr_t)ptr_base) && !readonly(ptr);
}

static int valid_read(void * ptr, size_t size, void *ptr_base, void *addr_of_base) {
  return allocated((uintptr_t)ptr, size, (uintptr_t)ptr_base);
}

/*! NB: The implementation for this function can also be specified via
 * \p base_addr macro that will eventually call ::TRY_SEGMENT. The following
 * implementation is preferred for performance reasons. */
static void * segment_base_addr(void * ptr) {
  TRY_SEGMENT(ptr,
    return (void*)heap_info((uintptr_t)ptr, 'B'),
    return (void*)static_info((uintptr_t)ptr, 'B'));
  return NULL;
}

/*! NB: Implementation of the following function can also be specified
 * via \p block_length macro. A more direct approach via ::TRY_SEGMENT
 * is preferred for performance reasons. */
static size_t segment_block_length(void * ptr) {
  TRY_SEGMENT(ptr,
    return heap_info((uintptr_t)ptr, 'L'),
    return static_info((uintptr_t)ptr, 'L'))
  return 0;
}

/*! NB: Implementation of the following function can also be specified
 * via \p offset macro. A more direct approach via ::TRY_SEGMENT
 * is preferred for performance reasons. */
static int segment_offset(void *ptr) {
  TRY_SEGMENT(ptr,
    return heap_info((uintptr_t)ptr, 'O'),
    return static_info((uintptr_t)ptr, 'O'));
  return 0;
}

static int initialized(void * ptr, size_t size) {
  uintptr_t addr = (uintptr_t)ptr;
  TRY_SEGMENT_WEAK(addr,
    return heap_initialized(addr, size),
    return static_initialized(addr, size));
  return 0;
}

static size_t get_heap_allocation_size(void) {
  return heap_allocation_size;
}

static void memory_init(int *argc_ref, char *** argv_ref, size_t ptr_size) {
  /** Verify that the given size of a pointer matches the one in the present
   * architecture. This is a guard against Frama-C instrumentations using
   * architectures different to the given one. */
  arch_assert(ptr_size);
  /* Initialize report file with debug logs (only in debug mode). */
  initialize_report_file(argc_ref, argv_ref);
  /* Lift stack limit to account for extra stack memory overhead.  */
  increase_stack_limit(get_stack_size()*2);
  /* Allocate and log shadow memory layout of the execution */
  init_memory_layout(argc_ref, argv_ref);
  /* Make sure the layout holds */
  DVALIDATE_SHADOW_LAYOUT;
  /* Track program arguments. */
  if (argc_ref && argv_ref)
    argv_alloca(*argc_ref, *argv_ref);
  /* Tracking safe locations */
  collect_safe_locations();
  int i;
  for (i = 0; i < safe_location_counter; i++) {
    void *addr = (void*)safe_locations[i].address;
    uintptr_t len = safe_locations[i].length;
    shadow_alloca(addr, len);
    if (safe_locations[i].initialized)
      initialize(addr, len);
  }
}

static void memory_clean(void) {
  clean_memory_layout();
}

/* API BINDINGS {{{ */
/* Heap allocation (native) */
strong_alias(shadow_malloc, malloc)
strong_alias(shadow_calloc, calloc)
strong_alias(shadow_realloc, realloc)
strong_alias(shadow_free, free)
strong_alias(shadow_free, cfree)
strong_alias(shadow_aligned_alloc, aligned_alloc)
strong_alias(shadow_posix_memalign, posix_memalign)
/* Explicit tracking */
public_alias(delete_block)
public_alias(store_block)
public_alias(store_block_duplicate)
/* Predicates */
public_alias2(segment_offset, offset)
public_alias2(segment_base_addr, base_addr)
public_alias2(segment_block_length, block_length)
public_alias(valid_read)
public_alias(valid)
public_alias(initialized)
public_alias(freeable)
/* Block initialization  */
public_alias(mark_readonly)
public_alias(initialize)
public_alias(full_init)
/* Memory state initialization */
public_alias(memory_clean)
public_alias(memory_init)
/* Heap size */
public_alias(get_heap_allocation_size)
public_alias(heap_allocation_size)
/* }}} */

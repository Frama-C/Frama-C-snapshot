/**************************************************************************/
/*                                                                        */
/*  This file is part of the Frama-C's E-ACSL plug-in.                    */
/*                                                                        */
/*  Copyright (C) 2012-2017                                               */
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
/*  for more details (enclosed in the file license/LGPLv2.1).             */
/*                                                                        */
/**************************************************************************/

/*! ***********************************************************************
 * \file  e_acsl_temporal.h
 * \brief Implementation of the tenporal API shared by all models
***************************************************************************/
#ifndef E_ACSL_TEMPORAL_H
#define E_ACSL_TEMPORAL_H

#include "e_acsl_temporal_timestamp.h"

#ifdef E_ACSL_TEMPORAL
#define E_ACSL_TEMPORAL_DESC "enabled"

/* Temporal timestamp retrieval [forward declarations] {{{ */
/*! \brief Return origin time stamp associated with a memory block containing
 * address given by `ptr`. `0` indicates an invalid timestamp, i.e., timestamp
 * of a memory block which does not exist. */
static uint32_t origin_timestamp(void *ptr);

/*! \brief Return address of referent shadow */
static uintptr_t temporal_referent_shadow(void *addr);

/*! \brief Return referent time stamp associated with a pointer which address
 * is given by `ptr`. This function expects that `ptr` is allocated and at
 * least `sizeof(uintptr_t)` bytes long */
uint32_t referent_timestamp(void *ptr);

/*! \brief Store a referent number `ref` in the shadow of `ptr` */
void store_temporal_referent(void *ptr, uint32_t ref);
/* }}} */

/* Temporal store {{{ */
void temporal_store_nblock(void *lhs, void *rhs) {
  store_temporal_referent(lhs, origin_timestamp(rhs));
}

void temporal_store_nreferent(void *lhs, void *rhs) {
  store_temporal_referent(lhs, referent_timestamp(rhs));
}

/* }}} */

/* Memcpy/memset {{{ */
void temporal_memcpy(void *dest, void *src, size_t size) {
  /* Memcpy is only relevant for pointers here, so if there is a
   * copy under a pointer's size then there no point in copying memory*/
  if (size >= sizeof(void*)) {
    DVALIDATE_ALLOCATED(src, size, src);
    DVALIDATE_WRITEABLE(dest, size, dest);
    void *dest_shadow = (void *)temporal_referent_shadow(dest);
    void *src_shadow = (void *)temporal_referent_shadow(src);
    memcpy(dest_shadow, src_shadow, size);
  }
}

void temporal_memset(void *dest, int c, size_t size) {
  DVALIDATE_WRITEABLE(dest, size, dest);
  void *dest_shadow = (void *)temporal_referent_shadow(dest);
  memset(dest_shadow, 0, size);
}
/* }}} */

/* Function parameters {{{ */
void temporal_save_nblock_parameter(void *ptr, unsigned int param) {
  parameter_referents[param].ptr = ptr;
  parameter_referents[param].temporal_flow = TBlockN;
}

void temporal_save_nreferent_parameter(void *ptr, unsigned int param) {
  parameter_referents[param].ptr = ptr;
  parameter_referents[param].temporal_flow = TReferentN;
}

void temporal_save_copy_parameter(void *ptr, unsigned int param) {
  parameter_referents[param].ptr = ptr;
  parameter_referents[param].temporal_flow = TCopy;
}

void temporal_pull_parameter(void *ptr, unsigned int param, size_t size) {
  struct temporal_parameter *tpar = &parameter_referents[param];
  switch(tpar->temporal_flow) {
    case TBlockN:
      store_temporal_referent(ptr, origin_timestamp(tpar->ptr));
      break;
    case TReferentN:
      store_temporal_referent(ptr, referent_timestamp(tpar->ptr));
      break;
    case TCopy:
      temporal_memcpy(ptr, tpar->ptr, size);
      break;
    default:
      vassert(0, "Unreachable", NULL);
  }
}

void temporal_reset_parameters() {
  reset_parameter_referents();
}
/* }}} */

/* Return values {{{ */
void temporal_save_return(void *ptr) {
  return_referent = (ptr, sizeof(void*)) ? referent_timestamp(ptr) : 0;
}

void temporal_pull_return(void *ptr) {
  store_temporal_referent(ptr, return_referent);
}

void temporal_reset_return() {
  return_referent = 0;
}
/* }}} */

/* Temporal valid {{{ */
int temporal_valid(void *ptr, void *addr_of_ptr) {
  /* Could check for NULL, but since temporal_valid if ran by `valid`, this
   * has been already checked.
   * FIXME: If the address of pointer and the pointer itself reference the same
   * address the access is deemed temporally valid by default.
   * One issue associated  with such checking is the case when a pointer points
   * to itself. One way to address such issue is to mark pointers, arrays and
   * integers differently. Here one can use the "readonly" bit to mark
   * something which does not need to be checked (e.g. arrays) and then
   * recognise this mark. Blocks can be recognised as readonly by using range
   * checking. For instance if some existing block belongs to a text segment
   * then it is readonly. */
  if (addr_of_ptr && (uintptr_t)ptr != (uintptr_t)addr_of_ptr) {
    /* The case where there is an actual pointer pointing to some memory
     * chunk, otherwise temporal valid holds trivially since the block points
     * to itself */
    uint32_t stored_referent = referent_timestamp(addr_of_ptr);
    uint32_t actual_referent = origin_timestamp(ptr);
    return stored_referent == actual_referent;
  }
  return 1;
}
/* }}} */
#else
#  define E_ACSL_TEMPORAL_DESC "disabled"
#endif
#endif

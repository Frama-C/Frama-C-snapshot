/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2018                                               */
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

#ifndef __FC_ALLOC_AXIOMATIC
#define __FC_ALLOC_AXIOMATIC
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_machdep.h"
#include "__fc_define_wchar_t.h"

__BEGIN_DECLS

/*@ ghost extern int __fc_heap_status __attribute__((FRAMA_C_MODEL)); */

/*@ axiomatic dynamic_allocation {
  @   predicate is_allocable{L}(integer n) // Can a block of n bytes be allocated?
  @     reads __fc_heap_status;
  @   // The logic label L is not used, but it must be present because the
  @   // predicate depends on the memory state
  @   axiom never_allocable{L}:
  @     \forall integer i;
  @        i < 0 || i > __FC_SIZE_MAX ==> !is_allocable(i);
  @ }
*/

__POP_FC_STDLIB
#endif

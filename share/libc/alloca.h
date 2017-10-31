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

#ifndef __FC_ALLOCA
#define __FC_ALLOCA
#include "features.h"
__PUSH_FC_STDLIB
#include <stdlib.h>

__BEGIN_DECLS

/*@ ghost extern int __fc_stack_status __attribute__((FRAMA_C_MODEL)); */

// Note: alloca is considered to never fail, unlike malloc
// Currently, ACSL does not allow specifying that the memory allocated by
// alloca must be freed at the end of the execution of its caller,
// therefore this responsibility is given to the user of this function.
/*@
  allocates \result;
  assigns __fc_stack_status \from size, __fc_stack_status;
  assigns \result \from indirect:size, indirect:__fc_stack_status;
  ensures \fresh(\result,size);
*/
void *alloca(size_t size);

__END_DECLS
__POP_FC_STDLIB
#endif

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

#ifndef __FC_ASSERT
#define __FC_ASSERT
#include "features.h"
__PUSH_FC_STDLIB

__BEGIN_DECLS

/*@
  requires nonnull_c: c != 0;
  terminates c != 0;
  assigns \nothing;
*/
extern void __FC_assert(int c, const char* file, int line, const char*expr);

__END_DECLS
__POP_FC_STDLIB
#endif

#undef assert
#ifdef NDEBUG 
#define assert(ignore) ((void)0)
#else
#define assert(e) (__FC_assert((e) != 0,__FILE__,__LINE__,#e))
#endif

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

/* ISO C: 7.15 */
#ifndef __FC_STDARG
#define __FC_STDARG
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_machdep.h" // for __FC_VA_LIST_T
__BEGIN_DECLS
typedef __FC_VA_LIST_T va_list;
__END_DECLS
#define va_arg(a,b) __builtin_va_arg(a,b)
#define va_copy(a,b) __builtin_va_copy(a,b)
#define va_end(a) __builtin_va_end(a)
#define va_start(a,b) __builtin_va_start(a,b)
__POP_FC_STDLIB
#endif

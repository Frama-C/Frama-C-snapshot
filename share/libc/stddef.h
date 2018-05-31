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

#ifndef __FC_STDDEF
#define __FC_STDDEF
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_machdep.h"
__BEGIN_DECLS
#ifndef __ptrdiff_t_defined
typedef __PTRDIFF_T ptrdiff_t;
#define __ptrdiff_t_defined
#endif
__END_DECLS
#include "__fc_define_size_t.h"
#ifdef __GNU_C__
#include "__fc_define_ssize_t.h"
#endif
#include "__fc_define_wchar_t.h"
#include "__fc_define_null.h"
#define offsetof(type, member) __builtin_offsetof(type,member)

__POP_FC_STDLIB
#endif

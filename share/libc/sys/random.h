/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2019                                               */
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

#ifndef _SYS_RANDOM_H
#define _SYS_RANDOM_H 1
#include "features.h"
#include "../__fc_define_size_t.h"
#include "../__fc_define_ssize_t.h"
__PUSH_FC_STDLIB

__BEGIN_DECLS

extern ssize_t getrandom (void *__buffer, size_t __length,
                          unsigned int __flags);

extern int getentropy (void *__buffer, size_t __length);

__END_DECLS

__POP_FC_STDLIB
#endif

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

#ifndef __FC_SYS_FILE_H__
#define __FC_SYS_FILE_H__

#include "../features.h"
__PUSH_FC_STDLIB

// Note: this file is not C11 nor POSIX, but Linux-specific.
// The values for the constants below are based on the glibc.

#define L_SET 0
#define L_INCR 1
#define L_XTND 2

#define LOCK_SH 1
#define LOCK_EX 2
#define LOCK_UN 8

#define LOCK_NB 4

extern int flock(int fd, int operation);

__POP_FC_STDLIB
#endif

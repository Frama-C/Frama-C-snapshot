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

#ifndef __FC_FNMATCH
#define __FC_FNMATCH
#include "features.h"
__PUSH_FC_STDLIB

// The values for the constants below are based on those
// of the glibc, declared in the order given by POSIX.1-2008.

#define FNM_NOMATCH 1
#define FNM_PATHNAME (1 << 0)
#define FNM_PERIOD (1 << 2)
#define FNM_NOESCAPE (1 << 1)

extern int fnmatch(const char *, const char *, int);

__END_DECLS
__POP_FC_STDLIB
#endif

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

#ifndef __FC_SYS_UTSNAME_H__
#define __FC_SYS_UTSNAME_H__

#include "../features.h"
__PUSH_FC_STDLIB

// Arbitrary length, based on the one used in Linux
#define _FC_UTSNAME_LENGTH 65

struct utsname
{
  char sysname[_FC_UTSNAME_LENGTH];
  char nodename[_FC_UTSNAME_LENGTH];
  char release[_FC_UTSNAME_LENGTH];
  char version[_FC_UTSNAME_LENGTH];
  char machine[_FC_UTSNAME_LENGTH];
};

extern int uname (struct utsname *name);

__POP_FC_STDLIB
#endif

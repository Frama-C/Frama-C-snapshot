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

#ifndef __FC_SYS_IPC_H__
#define __FC_SYS_IPC_H__
#include "../features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

#include "../__fc_define_mode_t.h"
#include "../__fc_define_uid_and_gid.h"
#include "../__fc_define_key_t.h"

struct ipc_perm {
  uid_t uid;
  gid_t gid;
  uid_t cuid;
  gid_t cgid;
  mode_t mode;
};

// The values for the constants below are based on an x86 Linux,
// declared in the order given by POSIX.1-2008.

#define IPC_CREAT 01000
#define IPC_EXCL 02000
#define IPC_NOWAIT 04000

#define IPC_PRIVATE ((key_t) 0)

#define IPC_RMID 0
#define IPC_SET 1
#define IPC_STAT 2

extern key_t ftok(const char *, int);

__END_DECLS
__POP_FC_STDLIB
#endif

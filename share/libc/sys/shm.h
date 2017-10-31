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

#ifndef __FC_SYS_SHM_H__
#define __FC_SYS_SHM_H__
#include "../features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

#include "../__fc_define_pid_t.h"
#include "../__fc_define_size_t.h"
#include "../__fc_define_time_t.h"

// POSIX: "the <sys/shm.h> header shall include the <sys/ipc.h> header."
#include <sys/ipc.h>

// The values for the constants below are based on an x86 Linux,
// declared in the order given by POSIX.1-2008.

#define SHM_RDONLY 010000
#define SHM_RND 020000

// TODO: parametrize the page size according to the machdep?
#define __FC_PAGE_SIZE 4096
#define SHMLBA __FC_PAGE_SIZE

typedef unsigned long shmatt_t;

struct shmid_ds {
  struct ipc_perm shm_perm;
  size_t shm_segsz;
  pid_t shm_lpid;
  pid_t shm_cpid;
  shmatt_t shm_nattch;
  time_t shm_atime;
  time_t shm_dtime;
  time_t shm_ctime;
};

extern void *shmat(int, const void *, int);
extern int   shmctl(int, int, struct shmid_ds *);
extern int   shmdt(const void *);
extern int   shmget(key_t, size_t, int);

__END_DECLS
__POP_FC_STDLIB
#endif

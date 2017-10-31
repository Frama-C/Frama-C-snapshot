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

// POSIX-compatible minimalist interface for sched.h

#ifndef __FC_SEMAPHORE
#define __FC_SEMAPHORE
#include "features.h"
__PUSH_FC_STDLIB
#include <time.h>

typedef union {
  char __size[16];
} sem_t;

#define SEM_FAILED ((sem_t *) 0)

extern int    sem_close(sem_t *);
extern int    sem_destroy(sem_t *);
extern int    sem_getvalue(sem_t *restrict, int *restrict);
extern int    sem_init(sem_t *, int, unsigned);
extern sem_t *sem_open(const char *, int, ...);
extern int    sem_post(sem_t *);
extern int    sem_timedwait(sem_t *restrict, const struct timespec *restrict);
extern int    sem_trywait(sem_t *);
extern int    sem_unlink(const char *);
extern int    sem_wait(sem_t *);

__END_DECLS
__POP_FC_STDLIB
#endif

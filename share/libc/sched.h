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

#ifndef __FC_SCHED
#define __FC_SCHED
#include "features.h"
__PUSH_FC_STDLIB

#include "__fc_define_timespec.h"
#include "__fc_define_pid_t.h"

__BEGIN_DECLS

struct sched_param {
  int sched_priority;
};

#define SCHED_OTHER             0
#define SCHED_FIFO              1
#define SCHED_RR                2
#define SCHED_SPORADIC          6

extern int    sched_get_priority_max(int);
extern int    sched_get_priority_min(int);
extern int    sched_getparam(pid_t, struct sched_param *);
extern int    sched_getscheduler(pid_t);
extern int    sched_rr_get_interval(pid_t, struct timespec *);
extern int    sched_setparam(pid_t, const struct sched_param *);
extern int    sched_setscheduler(pid_t, int, const struct sched_param *);
extern int    sched_yield(void);

__END_DECLS
__POP_FC_STDLIB
#endif

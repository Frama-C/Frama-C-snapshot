/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2014                                               */
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

#ifndef __FC_SYS_RESOURCE_H__
#define __FC_SYS_RESOURCE_H__
#include "time.h"
#include "../__fc_define_id_t.h"

#define PRIO_PROCESS    0
#define PRIO_PGRP       1
#define PRIO_USER       2

typedef unsigned long rlim_t;
struct rlimit {
        rlim_t   rlim_cur;
        rlim_t   rlim_max;
};

struct rusage {
  struct timeval ru_utime;
  struct timeval ru_stime;
};

#define RLIM_INFINITY 0xFFFFFFFFul
#define RLIM_SAVED_MAX RLIM_INFINITY
#define RLIM_SAVED_CUR RLIM_INFINITY

#define RUSAGE_SELF 0
#define RUSAGE_CHILDREN 1

#define RLIMIT_CORE 0
#define RLIMIT_CPU 1
#define RLIMIT_DATA 2
#define  RLIMIT_FSIZE 3
#define RLIMIT_NOFILE 4
#define RLIMIT_STACK 5
#define RLIMIT_AS 6

/*@ assigns \result \from which,who; */
int getpriority(int which, id_t who);

/*@ assigns \result \from which,who,prio; */
int setpriority(int which, id_t who, int prio);

/*@ assigns \result \from r;
  @ assigns rl->rlim_cur \from r;
  @ assigns rl->rlim_max \from r;
*/
int getrlimit(int r, struct rlimit *rl);

/*@ assigns \result \from r;
  @ assigns ru->ru_utime \from r;
  @ assigns ru->ru_stime \from r;
*/
int getrusage(int r, struct rusage *ru);

/*@ assigns \result \from r,rl->rlim_cur,rl->rlim_max;
*/
int setrlimit(int r, const struct rlimit * rl);

#endif

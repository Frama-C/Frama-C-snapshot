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

#ifndef __FC_SYS_RESOURCE_H__
#define __FC_SYS_RESOURCE_H__
#include "../features.h"
__PUSH_FC_STDLIB
#include "time.h"
#include "../__fc_define_id_t.h"

#define PRIO_PROCESS    0
#define PRIO_PGRP       1
#define PRIO_USER       2

__BEGIN_DECLS

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
extern int getpriority(int which, id_t who);

/*@ assigns \result \from which,who,prio; */
extern int setpriority(int which, id_t who, int prio);

/*@
  requires valid_rlp: \valid(rlp);
  assigns \result, *rlp \from resource;
*/
extern int getrlimit(int resource, struct rlimit *rlp);

/*@
  requires valid_r_usage: \valid(r_usage);
  assigns *r_usage \from who;
  assigns \result \from indirect:who;
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int getrusage(int who, struct rusage *r_usage);

/*@
  requires valid_rlp: \valid_read(rlp);
  assigns *rlp \from resource;
  assigns \result \from indirect:resource, indirect:*rlp;
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int setrlimit(int resource, const struct rlimit *rlp);

__END_DECLS
__POP_FC_STDLIB
#endif

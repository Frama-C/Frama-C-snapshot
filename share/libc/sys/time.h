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

#ifndef __FC_SYS_TIME_H__
#define __FC_SYS_TIME_H__

#include "../__fc_define_time_t.h"
#include "../__fc_define_suseconds_t.h"
#include "../__fc_define_fd_set_t.h"
#include "../__fc_define_timespec.h"
struct timeval {
  time_t         tv_sec;
  suseconds_t    tv_usec;
};

struct timezone {
  int tz_minuteswest;
  int tz_dsttime;
};

/* Abstract representation of the current time. */
extern volatile int __fc_time;
extern int __fc_tz;

/*@ assigns \result \from path[0..],times[0..1]; */
int utimes(const char *path, const struct timeval times[2]);

/*@ behavior tv_and_tz_null:
  @   assumes tv == \null && tz == \null;
  @   assigns \nothing;
  @
  @ behavior tv_not_null:
  @   assumes tv != \null && tz == \null;
  @   assigns tv->tv_sec \from __fc_time;
  @   assigns tv->tv_usec \from __fc_time;
  @   ensures \initialized(&tv->tv_sec) && \initialized(&tv->tv_usec);
  @
  @ behavior tz_not_null:
  @   assumes tv == \null && tz != \null;
  @   assigns *tz \from __fc_tz;
  @   ensures \initialized(tz);
  @
  @ behavior tv_and_tz_not_null:
  @   assumes tv != \null && tz != \null;
  @   assigns tv->tv_sec \from __fc_time;
  @   assigns tv->tv_usec \from __fc_time;
  @   assigns *tz \from __fc_tz;
  @   ensures \initialized(&tv->tv_sec) && \initialized(&tv->tv_usec);
  @   ensures \initialized(&tz);
  @
  @ complete behaviors;
  @ disjoint behaviors;
  @*/
int gettimeofday(struct timeval *tv, struct timezone *tz);

/*@ assigns \result,__fc_time,__fc_tz 
  @            \from      tv->tv_sec, tv->tv_usec,
  @                       tz->tz_dsttime, tz->tz_minuteswest; 
  @*/
int settimeofday(const struct timeval *tv, const struct timezone *tz);

#endif

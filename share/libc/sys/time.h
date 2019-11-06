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

#ifndef __FC_SYS_TIME_H__
#define __FC_SYS_TIME_H__
#include "../features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

#include "../__fc_define_time_t.h"
#include "../__fc_define_suseconds_t.h"
#include "../__fc_define_fd_set_t.h"
#include "../__fc_define_timespec.h"
#include "../__fc_string_axiomatic.h"
struct timeval {
  time_t         tv_sec;
  suseconds_t    tv_usec;
};

struct timezone {
  int tz_minuteswest;
  int tz_dsttime;
};

/* Abstract representation of the current time. */
//@ ghost volatile unsigned int __fc_time __attribute__((FRAMA_C_MODEL));
//@ ghost extern int __fc_tz __attribute__((FRAMA_C_MODEL));

/*@
  requires valid_path: valid_read_string(path);
  requires valid_times_or_null: \valid_read(times+(0..1)) || times == \null;
  assigns \result \from indirect:path[0..strlen(path)],
    indirect:times, indirect:times[0..1];
*/
extern int utimes(const char *path, const struct timeval times[2]);

/*@ assigns tv->tv_sec, tv->tv_usec \from __fc_time;
  @ assigns *(struct timezone *)tz \from __fc_tz;
  @ assigns \result \from indirect:tv, indirect:tz, *tv, *(struct timezone *)tz, __fc_tz;
  @ ensures result_ok_or_error: \result == 0 || \result == -1;
  @ behavior tv_and_tz_null:
  @   assumes null_tv_tz: tv == \null && tz == \null;
  @   assigns \result \from indirect:__fc_tz;
  @
  @ behavior tv_not_null:
  @   assumes non_null_tv_null_tz: tv != \null && tz == \null;
  @   assigns tv->tv_sec, tv->tv_usec \from indirect:__fc_time;
  @   assigns \result \from indirect:*tv, indirect:__fc_tz;
  @   ensures initialization:tv_sec:tv_usec:
  @     \initialized(&tv->tv_sec) && \initialized(&tv->tv_usec);
  @   ensures tv_usec_bounded: 0 <= tv->tv_usec <= 999999;
  @
  @ behavior tz_not_null:
  @   assumes null_tv_non_null_tz: tv == \null && tz != \null;
  @   assigns *(struct timezone *)tz \from __fc_tz;
  @   assigns \result \from indirect:*(struct timezone *)tz, indirect:__fc_tz;
  @   ensures initialization:tz: \initialized((struct timezone *)tz);
  @
  @ behavior tv_and_tz_not_null:
  @   assumes non_null_tv_tz: tv != \null && tz != \null;
  @   assigns tv->tv_sec, tv->tv_usec \from indirect:__fc_time;
  @   assigns *(struct timezone *)tz \from __fc_tz;
  @   assigns \result \from indirect:*tv, indirect:*(struct timezone *)tz, indirect:__fc_tz;
  @   ensures initialization:tv_sec:tv_usec:
  @     \initialized(&tv->tv_sec) && \initialized(&tv->tv_usec);
  @   ensures initialization:tz: \initialized((struct timezone *)tz);
  @
  @ complete behaviors;
  @ disjoint behaviors;
  @*/
extern int gettimeofday(struct timeval * restrict tv, void * restrict tz);

/*@
  requires valid_tv_or_null: \valid_read(tv) || tv == \null;
  requires valid_tz_or_null: \valid_read(tz) || tz == \null;
  assigns __fc_time, __fc_tz \from tv->tv_sec, tv->tv_usec,
                                   tz->tz_dsttime, tz->tz_minuteswest;
  assigns \result \from indirect:*tv, indirect:*tz;
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int settimeofday(const struct timeval *tv, const struct timezone *tz);

#if (defined _POSIX_C_SOURCE && (_POSIX_C_SOURCE) >= 200112L) ||        \
  (defined _XOPEN_SOURCE && (_XOPEN_SOURCE) >= 600)
#define ITIMER_REAL    0
#define ITIMER_VIRTUAL 1
#define ITIMER_PROF    2

struct itimerval {
  struct timeval it_interval; /* timer interval */
  struct timeval it_value;    /* current value */
};

/* Abstract representation of interval timers. */
//@ ghost volatile struct itimerval __fc_itimer_real    __attribute__((FRAMA_C_MODEL));
//@ ghost volatile struct itimerval __fc_itimer_virtual __attribute__((FRAMA_C_MODEL));
//@ ghost volatile struct itimerval __fc_itimer_prof    __attribute__((FRAMA_C_MODEL));

// NOTE: for functions setitimer/getitimer, we do not currently model
// the signals generated by the timers when they reach zero.

/*@
  requires valid_curr_value: \valid(curr_value);
  assigns \result \from indirect:which;
  assigns *curr_value \from __fc_itimer_real, __fc_itimer_virtual,
                            __fc_itimer_prof;
  ensures initialization:curr_value: \initialized(curr_value);
  behavior real:
    assumes itimer_real: which == ITIMER_REAL;
    assigns \result \from \nothing;
    assigns *curr_value \from __fc_itimer_real;
    ensures result_ok: \result == 0;
  behavior virtual:
    assumes itimer_virtual: which == ITIMER_VIRTUAL;
    assigns \result \from \nothing;
    assigns *curr_value \from __fc_itimer_virtual;
    ensures result_ok: \result == 0;
  behavior prof:
    assumes itimer_prof: which == ITIMER_PROF;
    assigns \result \from \nothing;
    assigns *curr_value \from __fc_itimer_prof;
    ensures result_ok: \result == 0;
  behavior invalid:
    assumes invalid_which:
      which != ITIMER_REAL && which != ITIMER_VIRTUAL && which != ITIMER_PROF;
    assigns \result \from \nothing;
    ensures result_error: \result == -1;
  complete behaviors;
  disjoint behaviors;
*/
extern int getitimer(int which, struct itimerval *curr_value);

// TODO: replace with a predicate, when Value will be able to evalute it
// precisely
#define __VALID_ITIMERVAL(tv) (0 <= (tv)->it_value.tv_usec <= 999999 &&    \
                             0 <= (tv)->it_interval.tv_usec <= 999999)

// Note: this specification is more strict than POSIX, since we forbid new_value
// from being NULL.
// Note: old_value depends on new_value because e.g. if new_value contains
// an invalid tv_usec field, the function may fail.
/*@
  requires valid_new_value: \valid_read(new_value);
  requires old_value_null_or_valid: old_value == \null || \valid(old_value);
  assigns *old_value \from indirect:which, indirect:old_value,
                           indirect:new_value, __fc_itimer_real,
                           __fc_itimer_virtual, __fc_itimer_prof;
  assigns \result \from indirect:which, indirect:new_value, indirect:*new_value;
  ensures result_ok_or_error: \result == 0 || \result == -1;
  behavior real:
    assumes itimer_real_and_valid:
      which == ITIMER_REAL && __VALID_ITIMERVAL(new_value);
    assigns \result \from \nothing;
    assigns *old_value \from __fc_itimer_real;
    assigns __fc_itimer_real \from *new_value;
    ensures result_ok: \result == 0;
    ensures initialization:old_value: \initialized(old_value);
  behavior virtual:
    assumes itimer_virtual_and_valid:
      which == ITIMER_VIRTUAL && __VALID_ITIMERVAL(new_value);
    assigns \result \from \nothing;
    assigns *old_value \from __fc_itimer_virtual;
    ensures result_ok: \result == 0;
    ensures initialization:old_value: \initialized(old_value);
  behavior prof:
    assumes itimer_prof_and_valid:
      which == ITIMER_PROF && __VALID_ITIMERVAL(new_value);
    assigns \result \from \nothing;
    assigns *old_value \from __fc_itimer_prof;
    ensures result_ok: \result == 0;
    ensures initialization:old_value: \initialized(old_value);
  behavior invalid:
    assumes invalid_itimer_or_new_value:
      (which != ITIMER_REAL && which != ITIMER_VIRTUAL && which != ITIMER_PROF)
      || !__VALID_ITIMERVAL(new_value);
    assigns \result \from \nothing;
    ensures result_error: \result == -1;
  disjoint behaviors;
*/
extern int setitimer (int which,
      const struct itimerval *restrict new_value,
      struct itimerval *restrict old_value);
#endif

// Non-POSIX, non-C99 functions (present in Linux and most BSDs)
extern void timeradd(struct timeval *a, struct timeval *b,
              struct timeval *res);

extern void timersub(struct timeval *a, struct timeval *b,
              struct timeval *res);

extern void timerclear(struct timeval *tvp);

extern int timerisset(struct timeval *tvp);

#define timercmp(a, b, _CMP) _timercmp(a, b)
extern int _timercmp(struct timeval *a, struct timeval *b);

// From POSIX, and for better compatibility with existing code bases:
// "Inclusion of the <sys/time.h> header may make visible all symbols
// from the <sys/select.h> header."
#include "select.h"

__END_DECLS
__POP_FC_STDLIB
#endif

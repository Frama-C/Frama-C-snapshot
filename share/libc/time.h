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

#ifndef __FC_TIME_H
#define __FC_TIME_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_null.h"
#include "__fc_define_size_t.h"

/*
 * Names of the interval timers, and structure
 * defining a timer setting:
 */
#define	ITIMER_REAL		0
#define	ITIMER_VIRTUAL		1
#define	ITIMER_PROF		2


__BEGIN_DECLS

typedef unsigned int clock_t;
#include "__fc_define_time_t.h"
// From POSIX.1-2008: "The value of CLOCKS_PER_SEC shall be 1 million on
// XSI-conformant systems. [...]"
#define CLOCKS_PER_SEC ((time_t)1000000)

struct tm {
  int tm_sec; // seconds after the minute [0, 60]
  int tm_min; // minutes after the hour [0, 59]
  int tm_hour; // hours since midnight [0, 23]
  int tm_mday; // day of the month [1, 31]
  int tm_mon; // months since January [0, 11]
  int tm_year; // years since 1900
  int tm_wday; // days since Sunday [0, 6]
  int tm_yday; // days since January 1 [0, 365]
  int tm_isdst; // Daylight Saving Time flag
};

#include "__fc_define_timespec.h"

struct itimerspec {
  struct timespec  it_interval;
  struct timespec  it_value;
};



#define CLOCK_REALTIME 666
#define CLOCK_MONOTONIC 1
#define TIMER_ABSTIME 0

//@ ghost volatile unsigned int __fc_time __attribute__((FRAMA_C_MODEL));

/*@ assigns \result \from __fc_time; */
extern clock_t clock(void);

/*@ assigns \result \from time1, time0; */
extern double difftime(time_t time1, time_t time0);

/*@ assigns *timeptr, \result \from *timeptr; */
extern time_t mktime(struct tm *timeptr);

/*@
  assigns *timer, \result \from __fc_time;
  behavior null:
    assumes timer == \null;
    assigns \result \from __fc_time;
  behavior not_null:
    assumes timer != \null;
    requires \valid(timer);
    assigns *timer, \result \from __fc_time;
    ensures \initialized(timer);
  complete behaviors;
  disjoint behaviors;
*/
extern time_t time(time_t *timer);

extern char *asctime(const struct tm *timeptr);

extern char *ctime(const time_t *timer);

struct tm __fc_time_tm;
struct tm * const  __fc_p_time_tm = &__fc_time_tm;

/*@ assigns \result \from __fc_p_time_tm;
  assigns __fc_time_tm \from *timer;
  ensures \result == &__fc_time_tm || \result == \null ;
*/
extern struct tm *gmtime(const time_t *timer);

/*@ assigns \result \from __fc_p_time_tm;
  assigns __fc_time_tm \from *timer;
  ensures \result == &__fc_time_tm || \result == \null;
*/
extern struct tm *localtime(const time_t *timer);

extern size_t strftime(char * restrict s,
		size_t maxsize,
		const char * restrict format,
		const struct tm * restrict timeptr);

/* POSIX */
extern char *asctime_r(const struct tm *restrict, char *restrict);
//Note: uncomment functions below when the necessary types will be defined:
// clockid_t, locale_t, timer_t
//extern int clock_getres(clockid_t, struct timespec *);
//extern int clock_gettime(clockid_t, struct timespec *);
//extern int clock_nanosleep(clockid_t, int, const struct timespec *,
//                           struct timespec *);
//extern int clock_settime(clockid_t, const struct timespec *);
extern char *ctime_r(const time_t *timep, char *buf);
extern struct tm *getdate(const char *string);
extern struct tm *gmtime_r(const time_t *restrict timer,
                           struct tm *restrict result);
extern struct tm *localtime_r(const time_t *restrict timep,
                              struct tm *restrict result);
extern int nanosleep(const struct timespec *req, struct timespec *rem);
//extern size_t strftime_l(char *restrict, size_t, const char *restrict,
//                         const struct tm *restrict, locale_t);
extern char *strptime(const char *restrict s, const char *restrict format,
                      struct tm *restrict tm);
//extern int timer_create(clockid_t, struct sigevent *restrict,
//                        timer_t *restrict);
//extern int timer_delete(timer_t);
//extern int timer_getoverrun(timer_t);
//extern int timer_gettime(timer_t, struct itimerspec *);
//extern int timer_settime(timer_t, int, const struct itimerspec *restrict,
//                         struct itimerspec *restrict);
extern void tzset(void);

extern int daylight;
extern long timezone;
extern char *tzname[2];
/* assigns tzname[0..1][0..] \from \nothing ;*/
extern void tzset(void);

__END_DECLS

__POP_FC_STDLIB
#endif

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

#ifndef __FC_TIME_H
#define __FC_TIME_H
#include "__fc_define_null.h"
#include "__fc_define_size_t.h"
#include "__fc_define_restrict.h"

/*
 * Names of the interval timers, and structure
 * defining a timer setting:
 */
#define	ITIMER_REAL		0
#define	ITIMER_VIRTUAL		1
#define	ITIMER_PROF		2


typedef unsigned int clock_t;
#include "__fc_define_time_t.h"
#define CLOCKS_PER_SEC ((time_t)16000)

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
#define TIMER_ABSTIME 0

/*@ assigns \nothing ; */
clock_t clock(void);

/*@ assigns \result \from time1, time0; */
double difftime(time_t time1, time_t time0);

/*@ assigns *timeptr, \result \from *timeptr; */
time_t mktime(struct tm *timeptr);

/*@ assigns *timer, \result \from \nothing; */
time_t time(time_t *timer);

char *asctime(const struct tm *timeptr);

char *ctime(const time_t *timer);

struct tm __fc_time_tm;
struct tm * const  __fc_time_tm_ptr=&__fc_time_tm;

/*@ assigns \result \from __fc_time_tm_ptr;
  assigns __fc_time_tm \from *timer;
  ensures \result == &__fc_time_tm || \result == \null ;
*/
struct tm *gmtime(const time_t *timer);

/*@ assigns \result \from __fc_time_tm_ptr;
  assigns __fc_time_tm \from *timer;
  ensures \result == &__fc_time_tm || \result == \null;
*/
struct tm *localtime(const time_t *timer);

size_t strftime(char * restrict s,
		size_t maxsize,
		const char * restrict format,
		const struct tm * restrict timeptr);

/* POSIX */
int nanosleep(const struct timespec *, struct timespec *);

extern int daylight;
extern long timezone;
extern char *tzname[2];
/* assigns tzname[0..1][0..] \from \nothing ;*/
void tzset(void); 

#endif

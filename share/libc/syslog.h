/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2011                                               */
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



#define LOG_PID 1
#define LOG_CONS (1<<2)
#define LOG_NDELAY (1<<3)
#define LOG_ODELAY (1<<4)
#define LOG_NOWAIT (1<<5)

#define LOG_KERN 1
#define LOG_USER 2
#define LOG_MAIL 3
#define LOG_NEWS 4
#define LOG_UUCP 5
#define LOG_DAEMON 6
#define LOG_AUTH 7
#define LOG_CRON 8
#define LOG_LPR 9
#define LOG_LOCAL0 10
#define LOG_LOCAL1 11
#define LOG_LOCAL2 12
#define LOG_LOCAL3 13
#define LOG_LOCAL4 14
#define LOG_LOCAL5 15
#define LOG_LOCAL6 16
#define LOG_LOCAL7 17

#define LOG_MASK(pri) pri

#define LOG_EMERG 1
#define LOG_ALERT 2
#define LOG_CRIT 3
#define LOG_ERR 4
#define LOG_WARNING 5
#define LOG_NOTICE 6
#define LOG_INFO 7
#define LOG_DEBUG 8

/*@ assigns \nothing ; */
void  closelog(void);
/*@ assigns \nothing ; */
void  openlog(const char *, int, int);
/*@ assigns \nothing ; */
int   setlogmask(int);
/*@ assigns \nothing ; */
void  syslog(int, const char *, ...);


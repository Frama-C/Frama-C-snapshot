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

#ifndef __FC_UTMPX
#define __FC_UTMPX
#include "features.h"
__PUSH_FC_STDLIB

#include "__fc_define_pid_t.h"
#include <sys/time.h>

// The sizes of arrays and values for the constants below are based on those
// of the glibc, declared in the order given by POSIX.1-2008.

struct utmpx {
  char ut_user[32];
  char ut_id[4];
  char ut_line[32];
  char ut_host[256]; // not POSIX, but allowed by it, and present in glibc
  pid_t ut_pid;
  short ut_type;
  struct timeval ut_tv;
};

#define EMPTY 0
#define BOOT_TIME 2
#define OLD_TIME 4
#define NEW_TIME 3
#define USER_PROCESS 7
#define INIT_PROCESS 5
#define LOGIN_PROCESS 6
#define DEAD_PROCESS 8

extern void endutxent(void);
extern struct utmpx *getutxent(void);
extern struct utmpx *getutxid(const struct utmpx *);
extern struct utmpx *getutxline(const struct utmpx *);
extern struct utmpx *pututxline(const struct utmpx *);
extern void setutxent(void);

__END_DECLS
__POP_FC_STDLIB
#endif

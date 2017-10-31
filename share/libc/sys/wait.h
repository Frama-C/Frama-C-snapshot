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

#ifndef __FC_SYS_WAIT_H__
#define __FC_SYS_WAIT_H__
#include "../features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

// The values for the constants/macros below are based on the glibc on
// an x86 Linux, declared in the order given by POSIX.1-2008.

#define WNOHANG    1
#define WCONTINUED 8
#define WUNTRACED  2

#define WEXITSTATUS(status)  (((status) & 0xff00) >> 8)
#define WIFCONTINUED(status) ((status) == 0xffff)
#define WIFEXITED(status)    (((status) & 0x7f) == 0)
#define WIFSIGNALED(status)  (((signed char) (((status) & 0x7f) + 1) >> 1) > 0)
#define WIFSTOPPED(status)   (((status) & 0xff) == 0x7f)
#define WSTOPSIG(status)     WEXITSTATUS(status)
#define WTERMSIG(status)     ((status) & 0x7f)

#define WEXITED 4
#define WNOWAIT 0x01000000
#define WSTOPPED 2

#include "../__fc_define_id_t.h"
#include "../__fc_define_pid_t.h"
#include "../__fc_define_uid_and_gid.h"
#include "../signal.h"

typedef enum __FC_IDTYPE_T { P_ALL, P_PID, P_PGID } idtype_t;

extern pid_t wait(int *stat_loc);
extern int waitid(idtype_t idt, id_t id, siginfo_t * sig, int options);
extern pid_t waitpid(pid_t pid, int *stat_loc, int options);

#include "resource.h"
// non-POSIX
extern pid_t wait3(int *, int, struct rusage *);


__END_DECLS
__POP_FC_STDLIB
#endif

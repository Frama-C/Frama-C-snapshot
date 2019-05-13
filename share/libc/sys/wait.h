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

# ifndef __ENUM_IDTYPE_T
# define __ENUM_IDTYPE_T 1
typedef enum __FC_IDTYPE_T { P_ALL, P_PID, P_PGID } idtype_t;
#endif

/*@ //missing: assigns \result \from 'child processes'
    //missing: terminates 'depending on child processes'
    //missing: may set errno to ECHILD or EINTR
  assigns \result \from \nothing;
  assigns *stat_loc \from \nothing;
  ensures result_ok_or_error: \result == -1 || \result >= 0;
  ensures initialization:stat_loc_init_on_success:
    \result >= 0 && stat_loc != \null ==> \initialized(stat_loc);
  behavior stat_loc_null:
    assumes stat_loc_null: stat_loc == \null;
    assigns \result \from \nothing;
  behavior stat_loc_non_null:
    assumes stat_loc_non_null: stat_loc != \null;
    requires valid_stat_loc: \valid(stat_loc);
    //missing: assigns *stat_loc \from 'child processes'
*/
extern pid_t wait(int *stat_loc);

extern int waitid(idtype_t idt, id_t id, siginfo_t * sig, int options);


/*@ //missing: assigns \result \from 'child processes'
    //missing: terminates 'depending on child processes'
    //missing: may set errno to ECHILD, EINTR or EINVAL
  assigns \result \from indirect:options;
  assigns *stat_loc \from indirect:options;
  ensures result_ok_or_error: \result == -1 || \result >= 0;
  ensures initialization:stat_loc_init_on_success:
    \result >= 0 && stat_loc != \null ==> \initialized(stat_loc);
  behavior stat_loc_null:
    assumes stat_loc_null: stat_loc == \null;
    assigns \result \from \nothing;
  behavior stat_loc_non_null:
    assumes stat_loc_non_null: stat_loc != \null;
    requires valid_stat_loc: \valid(stat_loc);
    //missing: assigns *stat_loc \from 'child processes'
*/
extern pid_t waitpid(pid_t pid, int *stat_loc, int options);

#include "resource.h"
// non-POSIX
extern pid_t wait3(int *, int, struct rusage *);


__END_DECLS
__POP_FC_STDLIB
#endif

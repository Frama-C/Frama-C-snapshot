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

#ifndef __FC_SELECT__
#define __FC_SELECT__
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_time_t.h"
#include "__fc_define_suseconds_t.h"
#include "__fc_define_fd_set_t.h"
#include "__fc_define_sigset_t.h"

#include "sys/time.h"

__BEGIN_DECLS

/* assigns \result \from nfds, *readfds, *writefds,*errorfds,*timeout,*sigmask;
 */
extern int pselect(int nfds, fd_set * readfds,
       fd_set * writefds, fd_set * errorfds,
       const struct timespec * timeout,
       const sigset_t * sigmask);

// __fc_fds_state is a very coarse model for the state of all
// file descriptor sets; it is sound, but very imprecise.
//@ ghost volatile int __fc_fds_state;

/*@
  requires nfds >= 0;
  requires readfds == \null || \valid(readfds);
  requires writefds == \null || \valid(writefds);
  requires errorfds == \null || \valid(errorfds);
  requires timeout == \null || \valid(timeout);
  assigns __fc_fds_state \from __fc_fds_state;
  assigns readfds  == \null ? \empty : *readfds,
          writefds == \null ? \empty : *writefds,
          errorfds == \null ? \empty : *errorfds,
          timeout == \null ? \empty : *timeout,
          \result
    \from indirect:nfds,
          indirect:readfds, indirect:*readfds,
          indirect:writefds, indirect:*writefds,
          indirect:errorfds, indirect:*errorfds,
          indirect:timeout, indirect:*timeout,
          __fc_fds_state;
  behavior read_notnull:
    assumes readfds != \null;
    ensures \initialized(readfds);
  behavior write_notnull:
    assumes writefds != \null;
    ensures \initialized(writefds);
  behavior error_notnull:
    assumes errorfds != \null;
    ensures \initialized(errorfds);
  behavior timeout_notnull:
    assumes timeout != \null;
    ensures \initialized(timeout);
 */
extern int select(int nfds, fd_set * readfds,
       fd_set * writefds, fd_set * errorfds,
       struct timeval * timeout);

__END_DECLS

__POP_FC_STDLIB
#endif

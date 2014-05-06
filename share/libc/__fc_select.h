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

#ifndef __FC_SELECT__
#define __FC_SELECT__

#include "__fc_define_time_t.h"
#include "__fc_define_suseconds_t.h"
#include "__fc_define_fd_set_t.h"
#include "__fc_define_sigset_t.h"

#include "time.h"
/* assigns \result \from nfds, *readfds, *writefds,*errorfds,*timeout,*sigmask;
 */
int pselect(int nfds, fd_set * readfds,
       fd_set * writefds, fd_set * errorfds,
       const struct timespec * timeout,
       const sigset_t * sigmask);

/* assigns \result \from nfds, *readfds, *writefds,*errorfds,*timeout ;*/
int select(int nfds, fd_set * readfds,
       fd_set * writefds, fd_set * errorfds,
       struct timeval * timeout);

#endif

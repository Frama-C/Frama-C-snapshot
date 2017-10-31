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

#ifndef __FC_POLL
#define __FC_POLL
#include "features.h"
__PUSH_FC_STDLIB

struct pollfd {
  int fd; // input parameter in poll()
  short events; // input parameter in poll()
  short revents; // output parameter in poll()
};

typedef unsigned long nfds_t;

extern volatile int Frama_C_entropy_source;

// The values used below are based on Linux.
#define POLLIN     0x001
#define POLLPRI    0x002
#define POLLOUT    0x004
#define POLLERR    0x008
#define POLLHUP    0x010
#define POLLNVAL   0x020
#define POLLRDNORM 0x040
#define POLLRDBAND 0x080
#define POLLWRNORM 0x100
#define POLLWRBAND 0x200

/*@
  requires \valid(fds+(0 .. nfds-1));
  assigns fds[0 .. nfds-1].revents \from indirect:fds[0 .. nfds-1].fd,
                                       fds[0 .. nfds-1].events,
                                       indirect:nfds, indirect:timeout,
                                       indirect:Frama_C_entropy_source;
  assigns \result \from indirect:fds[0 .. nfds-1].fd,
                        indirect:fds[0 .. nfds-1].events,
                        indirect:nfds, indirect:timeout,
                        indirect:Frama_C_entropy_source;
  ensures -1 <= \result <= nfds;
  ensures \initialized(&fds[0 .. nfds-1].revents);
 */
extern int poll (struct pollfd *fds, nfds_t nfds, int timeout);

__END_DECLS

__POP_FC_STDLIB
#endif

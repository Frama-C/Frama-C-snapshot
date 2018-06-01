/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2018                                               */
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

#include "netdb.h"
#include "sys/socket.h"
#include "netinet/in.h"
#include "stdlib.h"
#include "stddef.h"
#include "errno.h"
#include "__fc_builtin.h"

/* dummy implementation of getaddrinfo. Returns at most one addrinfo.
 */
int getaddrinfo(
  const char *restrict nodename,
  const char *restrict servname,
  const struct addrinfo *restrict hints,
  struct addrinfo **restrict res) {
  static unsigned int volatile net_state;
  if (nodename == NULL && servname == NULL) return EAI_NONAME;
  // very crude modelling: except for AGAIN, FAIL, and SYSTEM, failures
  // could be inferred from the arguments.
  switch (net_state) {
    case 0: return EAI_BADFLAGS;
    case 1: return EAI_AGAIN;
    case 2: return EAI_FAIL;
    case 3: return EAI_FAMILY;
    case 5: return EAI_SERVICE;
    case 6: return EAI_SOCKTYPE;
    case 7: {
      errno = EIO; // pick a semi-plausible errcode.
      return EAI_SYSTEM;
    }
    default: {
      struct addrinfo* ai = malloc(sizeof(*ai));
      if (!ai) return EAI_MEMORY;
      struct sockaddr* sa = malloc(sizeof(*sa));
      if (!sa) return EAI_MEMORY;
      sa -> sa_family = Frama_C_interval(0,AF_MAX);
      //@ slevel 15;
      for (int i = 0; i < 14; i++) {
        sa -> sa_data[i] = Frama_C_interval(CHAR_MIN,CHAR_MAX);
      }
      //@ slevel default;
      ai -> ai_flags = 0;
      ai -> ai_family = sa -> sa_family;
      ai -> ai_socktype = Frama_C_interval(0,SOCK_SEQPACKET);
      ai -> ai_protocol = Frama_C_interval(0,IPPROTO_MAX);
      ai -> ai_addrlen = sizeof(*sa) ;
      ai -> ai_addr = sa ;
      ai -> ai_canonname = "dummy" ;
      ai -> ai_next = NULL;
      *res = ai;
      return 0;
    }
  }
}

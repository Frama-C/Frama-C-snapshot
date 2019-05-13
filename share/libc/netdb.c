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

#include "netdb.h"
#include "sys/socket.h"
#include "netinet/in.h"
#include "stdlib.h"
#include "stddef.h"
#include "string.h"
#include "errno.h"
#include "__fc_builtin.h"
__PUSH_FC_STDLIB

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

#define __FC_MAX_HOST_ADDRS 2
#define __FC_MAX_HOST_ALIASES 2
#define __FC_HOSTBUF_SIZE 128
#define __FC_QUERYBUF_SIZE 128
struct __fc_gethostbyname {
  struct hostent host;
  unsigned char host_addr[sizeof(struct in_addr)];
  char *h_addr_ptrs[__FC_MAX_HOST_ADDRS + 1];
  char *host_aliases[__FC_MAX_HOST_ALIASES];
  char hostbuf[__FC_HOSTBUF_SIZE];
};

struct __fc_gethostbyname __fc_ghbn;

int res_search(const char *dname, int class, int type,
               char *answer, int anslen) {
  for (int i = 0; i < anslen-1; i++) {
    answer[i] = Frama_C_char_interval(CHAR_MIN, CHAR_MAX);
  }
  answer[anslen-1] = 0;
  return Frama_C_interval(-1, anslen);
}

struct hostent *gethostbyname(const char *name) {
  char buf[__FC_QUERYBUF_SIZE];
  const char *cp;
  int n;
  __fc_ghbn.host.h_addrtype = AF_INET;
  __fc_ghbn.host.h_length = sizeof(struct in_addr);

  // Disallow names consisting only of digits/dots, unless they end in a dot
  if (*name >= '0' && *name <= '9') {
    for (cp = name;; ++cp) {
      if (!*cp) {
        struct in_addr addr;

        if (*--cp == '.') break;

        // All-numeric, no dot at the end. Fake up a hostent as if we'd actually done a lookup.
        addr.s_addr = inet_addr(name);
        if (addr.s_addr == INADDR_NONE) return NULL;

        memcpy(__fc_ghbn.host_addr, &addr, __fc_ghbn.host.h_length);
        strncpy(__fc_ghbn.hostbuf, name, __FC_HOSTBUF_SIZE - 1);
        __fc_ghbn.hostbuf[__FC_HOSTBUF_SIZE - 1] = '\0';
        __fc_ghbn.host.h_name = __fc_ghbn.hostbuf;
        __fc_ghbn.host.h_aliases = __fc_ghbn.host_aliases;
        __fc_ghbn.host_aliases[0] = NULL;
        __fc_ghbn.h_addr_ptrs[0] = (char *) __fc_ghbn.host_addr;
        __fc_ghbn.h_addr_ptrs[1] = NULL;
        __fc_ghbn.host.h_addr_list = __fc_ghbn.h_addr_ptrs;

        return &__fc_ghbn.host;
      }

      if (*cp < '0' && *cp > '9' && *cp != '.') break;
    }
  }

  n = res_search(name, 1, 1, buf, sizeof(buf));
  if (n < 0) return NULL;

  if (Frama_C_nondet(0, 1)) return NULL;
  else {
    struct in_addr addr;
    addr.s_addr = inet_addr(name);
    memcpy(__fc_ghbn.host_addr, &addr, __fc_ghbn.host.h_length);
    strncpy(__fc_ghbn.hostbuf, name, __FC_HOSTBUF_SIZE - 1);
    __fc_ghbn.hostbuf[__FC_HOSTBUF_SIZE - 1] = '\0';
    __fc_ghbn.host.h_name = __fc_ghbn.hostbuf;
    __fc_ghbn.host.h_aliases = __fc_ghbn.host_aliases;
    __fc_ghbn.host_aliases[0] = NULL;
    __fc_ghbn.h_addr_ptrs[0] = (char *) __fc_ghbn.host_addr;
    __fc_ghbn.h_addr_ptrs[1] = NULL;
    __fc_ghbn.host.h_addr_list = __fc_ghbn.h_addr_ptrs;
    return &__fc_ghbn.host;
  }
}


__POP_FC_STDLIB

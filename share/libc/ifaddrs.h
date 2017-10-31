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

#ifndef FC_IFADDRS
#define FC_IFADDRS
#include "features.h"
__PUSH_FC_STDLIB

#include "__fc_define_sockaddr.h"

__BEGIN_DECLS

/* Linux header */
struct ifaddrs {
  struct ifaddrs  *ifa_next;
  char *ifa_name;
  unsigned int ifa_flags;
  struct sockaddr *ifa_addr;
  struct sockaddr *ifa_netmask;
  struct sockaddr *ifa_dstaddr;
  union {
    struct sockaddr *ifu_broadaddr;
    struct sockaddr *ifu_dstaddr;
  } ifa_ifu;
# ifndef ifa_broadaddr
#  define ifa_broadaddr  ifa_ifu.ifu_broadaddr
# endif
# ifndef ifa_dstaddr
#  define ifa_dstaddr    ifa_ifu.ifu_dstaddr
# endif
  void *ifa_data;
};

struct ifmaddrs {
	struct ifmaddrs	*ifma_next;
	struct sockaddr	*ifma_name;
	struct sockaddr	*ifma_addr;
	struct sockaddr	*ifma_lladdr;
};

extern int getifaddrs(struct ifaddrs **);
extern void freeifaddrs(struct ifaddrs *);
extern int getifmaddrs(struct ifmaddrs **);
extern void freeifmaddrs(struct ifmaddrs *);

__END_DECLS

__POP_FC_STDLIB
#endif

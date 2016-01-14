/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2015                                               */
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

#ifndef __FC_SOCKET_H__
#define __FC_SOCKET_H__
#include "../__fc_machdep.h"

typedef __UINT_LEAST32_T socklen_t;
#include "../__fc_define_sa_family_t.h"
#include "../__fc_define_sockaddr.h"
/* Not POSIX compliant but seems needed for some functions... */
#include "../__fc_define_ssize_t.h"
#include "../features.h"

struct sockaddr_storage {
  sa_family_t   ss_family;
};

#include "../__fc_define_iovec.h"

struct cmsghdr {
  socklen_t  cmsg_len;
  int        cmsg_level;
  int        cmsg_type;
};

#define SCM_RIGHTS 0

struct msghdr {
  void          *msg_name;
  socklen_t      msg_namelen;
  struct iovec  *msg_iov;
  int            msg_iovlen;
  void          *msg_control;
  socklen_t      msg_controllen;
  int            msg_flags;
};

/* Socket types. */
#define SOCK_STREAM	1		/* stream (connection) socket	*/
#define SOCK_DGRAM	2		/* datagram (conn.less) socket	*/
#define SOCK_RAW	3		/* raw socket			*/
#define SOCK_RDM	4		/* reliably-delivered message	*/
#define SOCK_SEQPACKET	5		/* sequential packet socket	*/

/* Supported address families. */
/*
 * Address families.
 */
#define AF_UNSPEC       0               /* unspecified */
#define AF_UNIX         1               /* local to host (pipes, portals) */
#define AF_LOCAL        1               /* POSIX name for AF_UNIX */
#define AF_INET         2               /* internetwork: UDP, TCP, etc. */
#define AF_IMPLINK      3               /* arpanet imp addresses */
#define AF_PUP          4               /* pup protocols: e.g. BSP */
#define AF_CHAOS        5               /* mit CHAOS protocols */
#define AF_NS           6               /* XEROX NS protocols */
#define AF_ISO          7               /* ISO protocols */
#define AF_OSI          AF_ISO          /* OSI is ISO */
#define AF_ECMA         8               /* european computer manufacturers */
#define AF_DATAKIT      9               /* datakit protocols */
#define AF_CCITT        10              /* CCITT protocols, X.25 etc */
#define AF_SNA          11              /* IBM SNA */
#define AF_DECnet       12              /* DECnet */
#define AF_DLI          13              /* Direct data link interface */
#define AF_LAT          14              /* LAT */
#define AF_HYLINK       15              /* NSC Hyperchannel */
#define AF_APPLETALK    16              /* AppleTalk */
#define AF_NETBIOS      17              /* NetBios-style addresses */
#define AF_INET6        18              /* IP version 6 */

#define AF_MAX          32
/*
 * Protocol families, same as address families for now.
 */
#define PF_UNSPEC       AF_UNSPEC
#define PF_UNIX         AF_UNIX
#define PF_LOCAL        AF_LOCAL
#define PF_INET         AF_INET
#define PF_IMPLINK      AF_IMPLINK
#define PF_PUP          AF_PUP
#define PF_CHAOS        AF_CHAOS
#define PF_NS           AF_NS
#define PF_ISO          AF_ISO
#define PF_OSI          AF_OSI
#define PF_ECMA         AF_ECMA
#define PF_DATAKIT      AF_DATAKIT
#define PF_CCITT        AF_CCITT
#define PF_SNA          AF_SNA
#define PF_DECnet       AF_DECnet
#define PF_DLI          AF_DLI
#define PF_LAT          AF_LAT
#define PF_HYLINK       AF_HYLINK
#define PF_APPLETALK    AF_APPLETALK
#define PF_NETBIOS      AF_NETBIOS

#define PF_MAX          AF_MAX

#define SOL_SOCKET      0xffff          /* options for socket level */

#define SO_DEBUG        0x0001          /* turn on debugging info recording */
#define SO_ACCEPTCONN   0x0002          /* socket has had listen() */
#define SO_REUSEADDR    0x0004          /* allow local address reuse */
#define SO_KEEPALIVE    0x0008          /* keep connections alive */
#define SO_DONTROUTE    0x0010          /* just use interface addresses */
#define SO_BROADCAST    0x0020          /* permit sending of broadcast msgs */
#define SO_USELOOPBACK  0x0040          /* bypass hardware when possible */
#define SO_LINGER       0x0080          /* linger on close if data present */
#define SO_OOBINLINE    0x0100          /* leave received OOB data in line */
#define SO_DONTLINGER   (unsigned int)(~SO_LINGER)
#define SO_PEERCRED	0x0200		/* same as getpeereid */

#define SO_ERROR        0x1000

#define SOMAXCONN 0xFF

#ifndef __FC_MAX_OPEN_SOCKETS
// arbitrary number
#define __FC_MAX_OPEN_SOCKETS 1024
#endif

// Allows different implementations for internal socket structures
#ifndef __FC_INTERNAL_SOCKFDS_PROVIDED
struct __fc_sockfds_type { int x; };
#endif
//@ ghost struct __fc_sockfds_type __fc_sockfds[__FC_MAX_OPEN_SOCKETS];

/* Represents the creation of new file descriptors for sockets. */
//@ ghost extern int __fc_socket_counter __attribute__((__FRAMA_C_MODEL__));

// __fc_sockfds represents the state of open socket descriptors.
//@ ghost volatile int __fc_open_sock_fds;
// TODO: Model the state of some functions more precisely.

/*@
  requires 0 <= sockfd < __FC_MAX_OPEN_SOCKETS;
  assigns  \result, *(((char *)addr)+(0 .. *addrlen-1)), __fc_sockfds[sockfd]
           \from *addr, *addrlen, __fc_sockfds[sockfd];
  ensures 0 <= \result < __FC_MAX_OPEN_SOCKETS || \result == -1;
  behavior addr_null:
    assumes addr == \null;
    requires addrlen == \null;
    assigns \result, __fc_sockfds[sockfd] \from __fc_sockfds[sockfd];
  behavior addr_not_null:
    assumes addr != \null;
    requires \valid(addrlen);
    requires \valid(((char *)addr)+(0 .. *addrlen-1));
    ensures \initialized(((char *)addr)+(0..*addrlen-1));
  disjoint behaviors;
  // TODO: check what to do when the buffer addr is too small
 */
int     accept(int sockfd, struct sockaddr *addr, socklen_t *addrlen);

/*@
  requires 0 <= sockfd < __FC_MAX_OPEN_SOCKETS;
  requires \valid_read(((char*)addr)+(0..addrlen-1));
  assigns \result, __fc_sockfds[sockfd] 
          \from sockfd, *addr, addrlen, __fc_sockfds[sockfd];
  ensures \result == 0 || \result == -1;
 */
int     bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen);

int     connect(int, const struct sockaddr *, socklen_t);
int     getpeername(int, struct sockaddr *, socklen_t *);
int     getsockname(int, struct sockaddr *, socklen_t *);
int     getsockopt(int, int, int, void *, socklen_t *);

/*@
  requires 0 <= sockfd < __FC_MAX_OPEN_SOCKETS;
  assigns  \result \from sockfd, __fc_sockfds[sockfd];
  assigns  __fc_sockfds[sockfd] \from sockfd, backlog, __fc_sockfds[sockfd];
  ensures  \result == 0 || \result == -1;
 */
int listen(int sockfd, int backlog);

/* Flags for passing to recv() and others */
#define MSG_OOB 1
#define MSG_PEEK 2
#define MSG_DONTROUTE 4
#define MSG_DONTWAIT 64

/*@ 
  requires 0 <= sockfd < __FC_MAX_OPEN_SOCKETS;
  requires \valid((char *)buf+(0 .. len-1));
  assigns  *((char *)buf+(0 .. len-1)), __fc_sockfds[sockfd], \result
           \from sockfd, len, flags, __fc_sockfds[sockfd];
  ensures  0 <= \result <= len || \result == -1;
  ensures  \initialized(((char *)buf+(0 .. \result-1)));
 */
ssize_t recv(int sockfd, void * buf, size_t len, int flags);

ssize_t recvfrom(int, void *, size_t, int,
        struct sockaddr *, socklen_t *);

/*@ requires 0 <= sockfd < __FC_MAX_OPEN_SOCKETS;
  @ requires \valid(&((char *)hdr->msg_control)[0..hdr->msg_controllen-1]);
  @ requires \valid(&(hdr->msg_iov[0..hdr->msg_iovlen-1]));
  @ requires hdr->msg_name == 0
      || \valid(&((char *)hdr->msg_name)[0..hdr->msg_namelen-1]);
  @ assigns ((char *) hdr->msg_name)[0..hdr->msg_namelen-1] \from 
                                                           __fc_sockfds[sockfd];
  @ assigns hdr->msg_namelen \from __fc_sockfds[sockfd];
  @ assigns ((char *) hdr->msg_iov[0..hdr->msg_iovlen-1].iov_base)[0..] \from
                                                           __fc_sockfds[sockfd];
  @ assigns ((char *) hdr->msg_control)[0..hdr->msg_controllen-1] \from __fc_sockfds[sockfd];
  @ assigns \result \from __fc_sockfds[sockfd];
  @ assigns hdr->msg_controllen \from __fc_sockfds[sockfd];
  @ assigns hdr->msg_flags \from __fc_sockfds[sockfd];
  @ assigns __fc_sockfds[sockfd] \from __fc_sockfds[sockfd];
  @ ensures \result <= hdr->msg_iovlen;
*/
ssize_t recvmsg(int sockfd, struct msghdr *hdr, int flags);
ssize_t send(int, const void *, size_t, int);
ssize_t sendmsg(int, const struct msghdr *, int);
ssize_t sendto(int, const void *, size_t, int, const struct sockaddr *,
        socklen_t);

/*@
  requires 0 <= sockfd < __FC_MAX_OPEN_SOCKETS;
  requires optval == \null || \valid_read(((char *)optval)+(0..optlen-1));
  assigns  \result, __fc_sockfds[sockfd] 
           \from  __fc_sockfds[sockfd], level, optname,
             ((char *)optval)[0..optlen-1], optlen;
  ensures  \result == 0 || \result == -1;
 */
int setsockopt(int sockfd, int level, int optname, const void *optval, socklen_t optlen);

/*@
  requires 0 <= sockfd < __FC_MAX_OPEN_SOCKETS;
  assigns \result, __fc_sockfds[sockfd] \from how, __fc_sockfds[sockfd];
  ensures \result == 0 || \result == -1;
 */
int shutdown(int sockfd, int how);

int sockatmark(int);

/*@
  assigns  \result, __fc_socket_counter
           \from domain, type, protocol, __fc_socket_counter;
  ensures  0 <= \result < __FC_MAX_OPEN_SOCKETS || \result == -1;
*/
int socket(int domain, int type, int protocol);

int sockatmark(int);

/*@ requires \valid(&socket_vector[0..1]);
  @ assigns \result, __fc_socket_counter, socket_vector[0..1] \from
                     __fc_socket_counter;
  @ ensures \initialized(&socket_vector[0..1]);
  @ ensures \result == 0 || \result == -1;
  @ ensures 0 <= socket_vector[0] < __FC_MAX_OPEN_SOCKETS;
  @ ensures 0 <= socket_vector[1] < __FC_MAX_OPEN_SOCKETS;
  @*/
int socketpair(int domain, int type, int protocol, int socket_vector[2]);
#endif

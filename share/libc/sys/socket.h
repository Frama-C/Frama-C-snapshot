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

#ifndef __FC_SOCKET_H__
#define __FC_SOCKET_H__
#include "../features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS
#include "../__fc_machdep.h"

// Note: most constants used here are based on Linux, independently of the
// chosen machdep. If using other OSs (e.g. MacOS), consider redefining them.

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

/* Protocol families (not described by POSIX) */
#define PF_UNSPEC 0
#define PF_LOCAL 1
#define PF_UNIX PF_LOCAL
#define PF_FILE PF_LOCAL
#define PF_INET 2
#define PF_AX25 3
#define PF_IPX 4
#define PF_APPLETALK 5
#define PF_NETROM 6
#define PF_BRIDGE 7
#define PF_ATMPVC 8
#define PF_X25 9
#define PF_INET6 10
#define PF_ROSE 11
#define PF_DECnet 12
#define PF_NETBEUI 13
#define PF_SECURITY 14
#define PF_KEY 15
#define PF_NETLINK 16
#define PF_ROUTE PF_NETLINK
#define PF_PACKET 17
#define PF_ASH 18
#define PF_ECONET 19
#define PF_ATMSVC 20
#define PF_RDS 21
#define PF_SNA 22
#define PF_IRDA 23
#define PF_PPPOX 24
#define PF_WANPIPE 25
#define PF_LLC 26
#define PF_IB 27
#define PF_MPLS 28
#define PF_CAN 29
#define PF_TIPC 30
#define PF_BLUETOOTH 31
#define PF_IUCV 32
#define PF_RXRPC 33
#define PF_ISDN 34
#define PF_PHONET 35
#define PF_IEEE802154 36
#define PF_CAIF 37
#define PF_ALG 38
#define PF_NFC 39
#define PF_VSOCK 40
#define PF_KCM 41
#define PF_QIPCRTR 42
#define PF_MAX 43

/* Address families (only AF_INET, AF_INET6, AF_UNIX and AF_UNSPEC are
   described in POSIX; AF_INET6 is optional) */
#define AF_UNSPEC 0
#define AF_LOCAL PF_LOCAL
#define AF_UNIX PF_UNIX
#define AF_FILE PF_FILE
#define AF_INET PF_INET
#define AF_AX25 PF_AX25
#define AF_IPX PF_IPX
#define AF_APPLETALK PF_APPLETALK
#define AF_NETROM PF_NETROM
#define AF_BRIDGE PF_BRIDGE
#define AF_ATMPVC PF_ATMPVC
#define AF_X25 PF_X25
#define AF_INET6 PF_INET6
#define AF_ROSE PF_ROSE
#define AF_DECnet PF_DECnet
#define AF_NETBEUI PF_NETBEUI
#define AF_SECURITY PF_SECURITY
#define AF_KEY PF_KEY
#define AF_NETLINK PF_NETLINK
#define AF_ROUTE PF_ROUTE
#define AF_PACKET PF_PACKET
#define AF_ASH PF_ASH
#define AF_ECONET PF_ECONET
#define AF_ATMSVC PF_ATMSVC
#define AF_RDS PF_RDS
#define AF_SNA PF_SNA
#define AF_IRDA PF_IRDA
#define AF_PPPOX PF_PPPOX
#define AF_WANPIPE PF_WANPIPE
#define AF_LLC PF_LLC
#define AF_IB PF_IB
#define AF_MPLS PF_MPLS
#define AF_CAN PF_CAN
#define AF_TIPC PF_TIPC
#define AF_BLUETOOTH PF_BLUETOOTH
#define AF_IUCV PF_IUCV
#define AF_RXRPC PF_RXRPC
#define AF_ISDN PF_ISDN
#define AF_PHONET PF_PHONET
#define AF_IEEE802154 PF_IEEE802154
#define AF_CAIF PF_CAIF
#define AF_ALG PF_ALG
#define AF_NFC PF_NFC
#define AF_VSOCK PF_VSOCK
#define AF_KCM PF_KCM
#define AF_QIPCRTR PF_QIPCRTR
#define AF_MAX PF_MAX

#define SOL_SOCKET 1

#define SO_DEBUG 1
#define SO_REUSEADDR 2
#define SO_TYPE 3
#define SO_ERROR 4
#define SO_DONTROUTE 5
#define SO_BROADCAST 6
#define SO_SNDBUF 7
#define SO_RCVBUF 8
#define SO_SNDBUFFORCE 32
#define SO_RCVBUFFORCE 33
#define SO_KEEPALIVE 9
#define SO_OOBINLINE 10
#define SO_NO_CHECK 11
#define SO_PRIORITY 12
#define SO_LINGER 13
#define SO_BSDCOMPAT 14
#define SO_REUSEPORT 15
#define SO_PASSCRED 16
#define SO_PEERCRED 17
#define SO_RCVLOWAT 18
#define SO_SNDLOWAT 19
#define SO_RCVTIMEO 20
#define SO_SNDTIMEO 21
#define SO_SECURITY_AUTHENTICATION 22
#define SO_SECURITY_ENCRYPTION_TRANSPORT 23
#define SO_SECURITY_ENCRYPTION_NETWORK 24
#define SO_BINDTODEVICE 25
#define SO_ATTACH_FILTER 26
#define SO_DETACH_FILTER 27
#define SO_GET_FILTER SO_ATTACH_FILTER
#define SO_PEERNAME 28
#define SO_TIMESTAMP 29
#define SCM_TIMESTAMP SO_TIMESTAMP
#define SO_ACCEPTCONN 30
#define SO_PEERSEC 31
#define SO_PASSSEC 34
#define SO_TIMESTAMPNS 35
#define SCM_TIMESTAMPNS SO_TIMESTAMPNS
#define SO_MARK 36
#define SO_TIMESTAMPING 37
#define SCM_TIMESTAMPING SO_TIMESTAMPING
#define SO_PROTOCOL 38
#define SO_DOMAIN 39
#define SO_RXQ_OVFL             40
#define SO_WIFI_STATUS 41
#define SCM_WIFI_STATUS SO_WIFI_STATUS
#define SO_PEEK_OFF 42
#define SO_NOFCS 43
#define SO_LOCK_FILTER 44
#define SO_SELECT_ERR_QUEUE 45
#define SO_BUSY_POLL 46
#define SO_MAX_PACING_RATE 47
#define SO_BPF_EXTENSIONS 48
#define SO_INCOMING_CPU 49
#define SO_ATTACH_BPF 50
#define SO_DETACH_BPF SO_DETACH_FILTER
#define SO_ATTACH_REUSEPORT_CBPF 51
#define SO_ATTACH_REUSEPORT_EBPF 52
#define SO_CNX_ADVICE 53
#define SCM_TIMESTAMPING_OPT_STATS 54
#define SO_MEMINFO 55
#define SO_INCOMING_NAPI_ID 56
#define SO_COOKIE 57
#define SCM_TIMESTAMPING_PKTINFO 58
#define SO_PEERGROUPS 59

#define SOMAXCONN 128

enum {
  SHUT_RD,
  SHUT_WR,
  SHUT_RDWR
};
// POSIX requires these SHUT_* constants to be defined as macros
#define SHUT_RD SHUT_RD
#define SHUT_WR SHUT_WR
#define SHUT_RDWR SHUT_RDWR

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
extern int     accept(int sockfd, struct sockaddr *addr, socklen_t *addrlen);

/*@
  requires 0 <= sockfd < __FC_MAX_OPEN_SOCKETS;
  requires \valid_read(((char*)addr)+(0..addrlen-1));
  assigns \result, __fc_sockfds[sockfd] 
          \from sockfd, *addr, addrlen, __fc_sockfds[sockfd];
  ensures \result == 0 || \result == -1;
 */
extern int     bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen);

/*@
  // ideally, we should check whether addrlen is compatible with the kind of
  // socket of [sockfd] (created by calling socket()).
  requires 0 <= sockfd < __FC_MAX_OPEN_SOCKETS;
  requires \valid_read(((char*)addr)+(0 .. addrlen-1));
  assigns __fc_sockfds[sockfd]
          \from __fc_sockfds[sockfd], indirect:sockfd, indirect:addr,
                indirect:*addr, indirect:addrlen;
  assigns \result
          \from indirect:__fc_sockfds[sockfd], indirect:sockfd, indirect:addr,
                indirect:*addr, indirect:addrlen;
  ensures \result == 0 || \result == -1;
 */
extern int connect(int sockfd, const struct sockaddr *addr, socklen_t addrlen);

extern int     getpeername(int, struct sockaddr *, socklen_t *);
extern int     getsockname(int, struct sockaddr *, socklen_t *);

// getsockopt is incrementally specified: options which are used more often
// are gradually refined; the rest are handled by behavior "other_options".
// Note: this specification may be more restrictive than what the manpage says,
//       to allow for a more precise analysis. It should however correspond to
//       expected usage.
/*@
  requires 0 <= sockfd < __FC_MAX_OPEN_SOCKETS;
  requires \valid(optlen);
  assigns ((char*)optval)[0..], \result
          \from indirect:sockfd, indirect:level, indirect:optname,
                indirect:*optlen, indirect:optval,
                indirect:__fc_sockfds[sockfd];
  assigns *optlen
          \from indirect:sockfd, indirect:level, indirect:optname,
                *optlen, indirect:optval, indirect:__fc_sockfds[sockfd];
  ensures \result == 0 || \result == -1;
  behavior so_error:
    assumes level == SOL_SOCKET && optname == SO_ERROR;
    requires \valid(optlen);
    requires *optlen == sizeof(int);
    requires \valid((int*)optval);
    assigns *(int*)optval, \result \from indirect:sockfd, indirect:optlen,
                                         indirect:__fc_sockfds[sockfd];
  behavior other_options:
    assumes !(level == SOL_SOCKET && optname == SO_ERROR);
    requires optval == \null || \valid(((char*)optval)+(0..));
    assigns ((char*)optval)[0..], \result
            \from indirect:sockfd, indirect:level, indirect:optname,
                  indirect:*optlen, indirect:optval,
                  indirect:__fc_sockfds[sockfd];
    assigns *optlen
            \from indirect:sockfd, indirect:level, indirect:optname,
                  *optlen, indirect:optval, indirect:__fc_sockfds[sockfd];
  disjoint behaviors;
  complete behaviors;
*/
extern int getsockopt(int sockfd, int level, int optname,
                      void *optval, socklen_t *optlen);

/*@
  requires 0 <= sockfd < __FC_MAX_OPEN_SOCKETS;
  assigns  \result \from sockfd, __fc_sockfds[sockfd];
  assigns  __fc_sockfds[sockfd] \from sockfd, backlog, __fc_sockfds[sockfd];
  ensures  \result == 0 || \result == -1;
 */
extern int listen(int sockfd, int backlog);

/* Flags for passing to recv() and others */
#define MSG_OOB          0x1
#define MSG_PEEK         0x2
#define MSG_DONTROUTE    0x4
#define MSG_CTRUNC       0x8
#define MSG_PROXY        0x10
#define MSG_TRUNC        0x20
#define MSG_DONTWAIT     0x40
#define MSG_EOR          0x80
#define MSG_WAITALL      0x100
#define MSG_FIN          0x200
#define MSG_SYN          0x400
#define MSG_CONFIRM      0x800
#define MSG_RST          0x1000
#define MSG_RSTERRQUEUE  0x2000
#define MSG_NOSIGNAL     0x4000
#define MSG_MORE         0x8000
#define MSG_WAITFORONE   0x10000
#define MSG_BATCH        0x40000
#define MSG_FASTOPEN     0x20000000
#define MSG_CMSG_CLOEXEC 0x40000000


/*@ 
  requires 0 <= sockfd < __FC_MAX_OPEN_SOCKETS;
  requires \valid((char *)buf+(0 .. len-1));
  assigns  *((char *)buf+(0 .. len-1)), __fc_sockfds[sockfd], \result
           \from sockfd, len, flags, __fc_sockfds[sockfd];
  ensures  0 <= \result <= len || \result == -1;
  ensures  \initialized(((char *)buf+(0 .. \result-1)));
 */
extern ssize_t recv(int sockfd, void * buf, size_t len, int flags);

extern ssize_t recvfrom(int, void *, size_t, int,
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
extern ssize_t recvmsg(int sockfd, struct msghdr *hdr, int flags);
extern ssize_t send(int, const void *, size_t, int);
extern ssize_t sendmsg(int, const struct msghdr *, int);
extern ssize_t sendto(int, const void *, size_t, int, const struct sockaddr *,
        socklen_t);

/*@
  requires 0 <= sockfd < __FC_MAX_OPEN_SOCKETS;
  requires optval == \null || \valid_read(((char *)optval)+(0..optlen-1));
  assigns  \result, __fc_sockfds[sockfd] 
           \from  __fc_sockfds[sockfd], level, optname,
             ((char *)optval)[0..optlen-1], optlen;
  ensures  \result == 0 || \result == -1;
 */
extern int setsockopt(int sockfd, int level, int optname, const void *optval, socklen_t optlen);

/*@
  requires 0 <= sockfd < __FC_MAX_OPEN_SOCKETS;
  assigns \result, __fc_sockfds[sockfd] \from how, __fc_sockfds[sockfd];
  ensures \result == 0 || \result == -1;
 */
extern int shutdown(int sockfd, int how);

extern int sockatmark(int);

/*@
  assigns  \result \from indirect:domain, indirect:type, indirect:protocol,
                         indirect:__fc_socket_counter;
  assigns  __fc_socket_counter \from indirect:domain, indirect:type,
                                     indirect:protocol, __fc_socket_counter;
  ensures  0 <= \result < __FC_MAX_OPEN_SOCKETS || \result == -1;
*/
extern int socket(int domain, int type, int protocol);

extern int sockatmark(int);

/*@ requires \valid(&socket_vector[0..1]);
  @ assigns \result, __fc_socket_counter, socket_vector[0..1] \from
                     __fc_socket_counter;
  @ ensures \initialized(&socket_vector[0..1]);
  @ ensures \result == 0 || \result == -1;
  @ ensures 0 <= socket_vector[0] < __FC_MAX_OPEN_SOCKETS;
  @ ensures 0 <= socket_vector[1] < __FC_MAX_OPEN_SOCKETS;
  @*/
extern int socketpair(int domain, int type, int protocol, int socket_vector[2]);

__END_DECLS
__POP_FC_STDLIB
#endif

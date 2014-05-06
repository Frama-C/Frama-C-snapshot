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

#ifndef __FC_NETINET_IN_H__
#define __FC_NETINET_IN_H__

#include "inttypes.h"
#include "sys/socket.h"

typedef uint16_t in_port_t;
typedef uint32_t in_addr_t;

struct in_addr {
  in_addr_t  s_addr;
};

struct sockaddr_in {
  sa_family_t     sin_family;
  in_port_t       sin_port;
  struct in_addr  sin_addr;
};

struct in6_addr {
  uint8_t s6_addr[16];
};

struct sockaddr_in6 {
  sa_family_t     sin6_family;
  in_port_t       sin6_port;
  uint32_t        sin6_flowinfo;
  struct in6_addr  sin6_addr;
  uint32_t         sin6_scope_id;
};

#define INADDR_ANY 0
#define INADDR_BROADCAST 0XFFFFFFFFUL

#define IN6ADDR_ANY 0
#define IN6ADDR_BROADCAST 0XFFFFFFFFFFFFFFFFULL

const struct in6_addr in6addr_any={{0}};
const struct in6_addr in6addr_loopback=
  {{0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
    0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF}}
  ;

struct ipv6_mreq {
  struct in6_addr  ipv6mr_multiaddr;
  unsigned         ipv6mr_interface;
};


struct in6_pktinfo
  {
    struct in6_addr ipi6_addr;
    int ipi6_ifindex;
  };


/* Standard well-defined IP protocols.  */
enum
  {
    IPPROTO_IP = 0,	   /* Dummy protocol for TCP.  */
#define IPPROTO_IP		IPPROTO_IP
    IPPROTO_HOPOPTS = 0,   /* IPv6 Hop-by-Hop options.  */
#define IPPROTO_HOPOPTS		IPPROTO_HOPOPTS
    IPPROTO_ICMP = 1,	   /* Internet Control Message Protocol.  */
#define IPPROTO_ICMP		IPPROTO_ICMP
    IPPROTO_IGMP = 2,	   /* Internet Group Management Protocol. */
#define IPPROTO_IGMP		IPPROTO_IGMP
    IPPROTO_IPIP = 4,	   /* IPIP tunnels (older KA9Q tunnels use 94).  */
#define IPPROTO_IPIP		IPPROTO_IPIP
    IPPROTO_TCP = 6,	   /* Transmission Control Protocol.  */
#define IPPROTO_TCP		IPPROTO_TCP
    IPPROTO_EGP = 8,	   /* Exterior Gateway Protocol.  */
#define IPPROTO_EGP		IPPROTO_EGP
    IPPROTO_PUP = 12,	   /* PUP protocol.  */
#define IPPROTO_PUP		IPPROTO_PUP
    IPPROTO_UDP = 17,	   /* User Datagram Protocol.  */
#define IPPROTO_UDP		IPPROTO_UDP
    IPPROTO_IDP = 22,	   /* XNS IDP protocol.  */
#define IPPROTO_IDP		IPPROTO_IDP
    IPPROTO_TP = 29,	   /* SO Transport Protocol Class 4.  */
#define IPPROTO_TP		IPPROTO_TP
    IPPROTO_DCCP = 33,	   /* Datagram Congestion Control Protocol.  */
#define IPPROTO_DCCP		IPPROTO_DCCP
    IPPROTO_IPV6 = 41,     /* IPv6 header.  */
#define IPPROTO_IPV6		IPPROTO_IPV6
    IPPROTO_ROUTING = 43,  /* IPv6 routing header.  */
#define IPPROTO_ROUTING		IPPROTO_ROUTING
    IPPROTO_FRAGMENT = 44, /* IPv6 fragmentation header.  */
#define IPPROTO_FRAGMENT	IPPROTO_FRAGMENT
    IPPROTO_RSVP = 46,	   /* Reservation Protocol.  */
#define IPPROTO_RSVP		IPPROTO_RSVP
    IPPROTO_GRE = 47,	   /* General Routing Encapsulation.  */
#define IPPROTO_GRE		IPPROTO_GRE
    IPPROTO_ESP = 50,      /* encapsulating security payload.  */
#define IPPROTO_ESP		IPPROTO_ESP
    IPPROTO_AH = 51,       /* authentication header.  */
#define IPPROTO_AH		IPPROTO_AH
    IPPROTO_ICMPV6 = 58,   /* ICMPv6.  */
#define IPPROTO_ICMPV6		IPPROTO_ICMPV6
    IPPROTO_NONE = 59,     /* IPv6 no next header.  */
#define IPPROTO_NONE		IPPROTO_NONE
    IPPROTO_DSTOPTS = 60,  /* IPv6 destination options.  */
#define IPPROTO_DSTOPTS		IPPROTO_DSTOPTS
    IPPROTO_MTP = 92,	   /* Multicast Transport Protocol.  */
#define IPPROTO_MTP		IPPROTO_MTP
    IPPROTO_ENCAP = 98,	   /* Encapsulation Header.  */
#define IPPROTO_ENCAP		IPPROTO_ENCAP
    IPPROTO_PIM = 103,	   /* Protocol Independent Multicast.  */
#define IPPROTO_PIM		IPPROTO_PIM
    IPPROTO_COMP = 108,	   /* Compression Header Protocol.  */
#define IPPROTO_COMP		IPPROTO_COMP
    IPPROTO_SCTP = 132,	   /* Stream Control Transmission Protocol.  */
#define IPPROTO_SCTP		IPPROTO_SCTP
    IPPROTO_UDPLITE = 136, /* UDP-Lite protocol.  */
#define IPPROTO_UDPLITE		IPPROTO_UDPLITE
    IPPROTO_RAW = 255,	   /* Raw IP packets.  */
#define IPPROTO_RAW		IPPROTO_RAW
    IPPROTO_MAX
  };



/*** originaly from bits/in.h ***/

/* Options for use with `getsockopt' and `setsockopt' at the IP level.
   The first word in the comment at the right is the data type used;
   "bool" means a boolean value stored in an `int'.  */
#define        IP_OPTIONS      4       /* ip_opts; IP per-packet options.  */
#define        IP_HDRINCL      3       /* int; Header is included with data.  */
#define        IP_TOS          1       /* int; IP type of service and precedence.  */
#define        IP_TTL          2       /* int; IP time to live.  */
#define        IP_RECVOPTS     6       /* bool; Receive all IP options w/datagram.  */
/* For BSD compatibility.  */
#define        IP_RECVRETOPTS  IP_RETOPTS       /* bool; Receive IP options for response.  */
#define        IP_RETOPTS      7       /* ip_opts; Set/get IP per-packet options.  */
#define IP_MULTICAST_IF 32	/* in_addr; set/get IP multicast i/f */
#define IP_MULTICAST_TTL 33	/* u_char; set/get IP multicast ttl */
#define IP_MULTICAST_LOOP 34	/* i_char; set/get IP multicast loopback */
#define IP_ADD_MEMBERSHIP 35	/* ip_mreq; add an IP group membership */
#define IP_DROP_MEMBERSHIP 36	/* ip_mreq; drop an IP group membership */
#define IP_UNBLOCK_SOURCE 37	/* ip_mreq_source: unblock data from source */
#define IP_BLOCK_SOURCE 38	/* ip_mreq_source: block data from source */
#define IP_ADD_SOURCE_MEMBERSHIP 39 /* ip_mreq_source: join source group */
#define IP_DROP_SOURCE_MEMBERSHIP 40 /* ip_mreq_source: leave source group */
#define IP_MSFILTER 41
#if defined __USE_MISC || defined __USE_GNU
# define MCAST_JOIN_GROUP 42	/* group_req: join any-source group */
# define MCAST_BLOCK_SOURCE 43	/* group_source_req: block from given group */
# define MCAST_UNBLOCK_SOURCE 44 /* group_source_req: unblock from given group*/
# define MCAST_LEAVE_GROUP 45	/* group_req: leave any-source group */
# define MCAST_JOIN_SOURCE_GROUP 46 /* group_source_req: join source-spec gr */
# define MCAST_LEAVE_SOURCE_GROUP 47 /* group_source_req: leave source-spec gr*/
# define MCAST_MSFILTER 48

# define MCAST_EXCLUDE   0
# define MCAST_INCLUDE   1
#endif


#define IP_ROUTER_ALERT	5	/* bool */
#define IP_PKTINFO	8	/* bool */
#define IP_PKTOPTIONS	9
#define IP_PMTUDISC	10	/* obsolete name? */
#define IP_MTU_DISCOVER	10	/* int; see below */
#define IP_RECVERR	11	/* bool */
#define IP_RECVTTL	12	/* bool */
#define IP_RECVTOS	13	/* bool */
#define IP_MTU		14	/* int */
#define IP_FREEBIND	15
#define IP_IPSEC_POLICY 16
#define IP_XFRM_POLICY	17
#define IP_PASSSEC	18
#define IP_TRANSPARENT	19


/* TProxy original addresses */
#define IP_ORIGDSTADDR       20
#define IP_RECVORIGDSTADDR   IP_ORIGDSTADDR

#define IP_MINTTL       21


/* IP_MTU_DISCOVER arguments.  */
#define IP_PMTUDISC_DONT   0	/* Never send DF frames.  */
#define IP_PMTUDISC_WANT   1	/* Use per route hints.  */
#define IP_PMTUDISC_DO     2	/* Always DF.  */
#define IP_PMTUDISC_PROBE  3	/* Ignore dst pmtu.  */

/* To select the IP level.  */
#define SOL_IP	0

#define IP_DEFAULT_MULTICAST_TTL        1
#define IP_DEFAULT_MULTICAST_LOOP       1
#define IP_MAX_MEMBERSHIPS              20


#if defined __USE_MISC || defined __USE_GNU
/* Structure used to describe IP options for IP_OPTIONS and IP_RETOPTS.
   The `ip_dst' field is used for the first-hop gateway when using a
   source route (this gets put into the header proper).  */
struct ip_opts
  {
    struct in_addr ip_dst;	/* First hop; zero without source route.  */
    char ip_opts[40];		/* Actually variable in size.  */
  };

/* Like `struct ip_mreq' but including interface specification by index.  */
struct ip_mreqn
  {
    struct in_addr imr_multiaddr;	/* IP multicast address of group */
    struct in_addr imr_address;		/* local IP address of interface */
    int	imr_ifindex;			/* Interface index */
  };

/* Structure used for IP_PKTINFO.  */
struct in_pktinfo
  {
    int ipi_ifindex;			/* Interface index  */
    struct in_addr ipi_spec_dst;	/* Routing destination address  */
    struct in_addr ipi_addr;		/* Header destination address  */
  };
#endif

/* Options for use with `getsockopt' and `setsockopt' at the IPv6 level.
   The first word in the comment at the right is the data type used;
   "bool" means a boolean value stored in an `int'.  */
#define IPV6_ADDRFORM		1
#define IPV6_2292PKTINFO	2
#define IPV6_2292HOPOPTS	3
#define IPV6_2292DSTOPTS	4
#define IPV6_2292RTHDR		5
#define IPV6_2292PKTOPTIONS	6
#define IPV6_CHECKSUM		7
#define IPV6_2292HOPLIMIT	8

#define SCM_SRCRT		IPV6_RXSRCRT

#define IPV6_NEXTHOP		9
#define IPV6_AUTHHDR		10
#define IPV6_UNICAST_HOPS	16
#define IPV6_MULTICAST_IF	17
#define IPV6_MULTICAST_HOPS	18
#define IPV6_MULTICAST_LOOP	19
#define IPV6_JOIN_GROUP		20
#define IPV6_LEAVE_GROUP	21
#define IPV6_ROUTER_ALERT	22
#define IPV6_MTU_DISCOVER	23
#define IPV6_MTU		24
#define IPV6_RECVERR		25
#define IPV6_V6ONLY		26
#define IPV6_JOIN_ANYCAST	27
#define IPV6_LEAVE_ANYCAST	28
#define IPV6_IPSEC_POLICY	34
#define IPV6_XFRM_POLICY	35

#define IPV6_RECVPKTINFO	49
#define IPV6_PKTINFO		50
#define IPV6_RECVHOPLIMIT	51
#define IPV6_HOPLIMIT		52
#define IPV6_RECVHOPOPTS	53
#define IPV6_HOPOPTS		54
#define IPV6_RTHDRDSTOPTS	55
#define IPV6_RECVRTHDR		56
#define IPV6_RTHDR		57
#define IPV6_RECVDSTOPTS	58
#define IPV6_DSTOPTS		59

#define IPV6_RECVTCLASS		66
#define IPV6_TCLASS		67

/* Obsolete synonyms for the above.  */
#define IPV6_ADD_MEMBERSHIP	IPV6_JOIN_GROUP
#define IPV6_DROP_MEMBERSHIP	IPV6_LEAVE_GROUP
#define IPV6_RXHOPOPTS		IPV6_HOPOPTS
#define IPV6_RXDSTOPTS		IPV6_DSTOPTS

/* IPV6_MTU_DISCOVER values.  */
#define IPV6_PMTUDISC_DONT	0	/* Never send DF frames.  */
#define IPV6_PMTUDISC_WANT	1	/* Use per route hints.  */
#define IPV6_PMTUDISC_DO	2	/* Always DF.  */
#define IPV6_PMTUDISC_PROBE	3	/* Ignore dst pmtu.  */

/* Socket level values for IPv6.  */
#define SOL_IPV6        41
#define SOL_ICMPV6      58

/* Routing header options for IPv6.  */
#define IPV6_RTHDR_LOOSE	0	/* Hop doesn't need to be neighbour. */
#define IPV6_RTHDR_STRICT	1	/* Hop must be a neighbour.  */

#define IPV6_RTHDR_TYPE_0	0	/* IPv6 Routing header type 0.  */

#endif

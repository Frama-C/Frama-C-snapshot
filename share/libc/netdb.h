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

#ifndef __FC_NETDB
#define __FC_NETDB
#include "features.h"
__PUSH_FC_STDLIB

#include "netinet/in.h"
#include "sys/socket.h"
#include "inttypes.h"
#include "__fc_string_axiomatic.h"

__BEGIN_DECLS

struct hostent
{
  char *h_name;			/* Official name of host.  */
  char **h_aliases;		/* Alias list.  */
  int h_addrtype;		/* Host address type.  */
  int h_length;			/* Length of address.  */
  char **h_addr_list;		/* List of addresses from name server.  */
};
#define h_addr h_addr_list[0] /* for backward compatibility */

struct netent
{
  char *n_name;			/* Official name of network.  */
  char **n_aliases;		/* Alias list.  */
  int n_addrtype;		/* Net address type.  */
  uint32_t n_net;		/* Network number.  */
};
struct protoent
{
  char *p_name;			/* Official protocol name.  */
  char **p_aliases;		/* Alias list.  */
  int p_proto;			/* Protocol number.  */
};
struct servent
{
  char *s_name;			/* Official service name.  */
  char **s_aliases;		/* Alias list.  */
  int s_port;			/* Port number.  */
  char *s_proto;		/* Protocol to use.  */
};

#define IPPORT_RESERVED	1024

int h_errno;

# define HOST_NOT_FOUND	1
# define TRY_AGAIN	2
# define NO_RECOVERY	3
# define NO_DATA	4

struct addrinfo
{
  int ai_flags;			/* Input flags.  */
  int ai_family;		/* Protocol family for socket.  */
  int ai_socktype;		/* Socket type.  */
  int ai_protocol;		/* Protocol for socket.  */
  socklen_t ai_addrlen;		/* Length of socket address.  */
  struct sockaddr *ai_addr;	/* Socket address for socket.  */
  char *ai_canonname;		/* Canonical name for service location.  */
  struct addrinfo *ai_next;	/* Pointer to next in list.  */
};

# define AI_PASSIVE	0x0001	/* Socket address is intended for `bind'.  */
# define AI_CANONNAME	0x0002	/* Request for canonical name.  */
# define AI_NUMERICHOST	0x0004	/* Don't use name resolution.  */
# define AI_NUMERICSERV	0x0400	/* Don't use name resolution.  */
# define AI_V4MAPPED	0x0008	/* IPv4 mapped addresses are acceptable.  */
# define AI_ALL		0x0010	/* Return IPv4 mapped and IPv6 addresses.  */
# define AI_ADDRCONFIG	0x0020	/* Use configuration of this host to choose
				   returned address type..  */
# define NI_NUMERICHOST	1	/* Don't try to look up hostname.  */
# define NI_NUMERICSERV 2	/* Don't convert port number to name.  */
# define NI_NOFQDN	4	/* Only return nodename portion.  */
# define NI_NAMEREQD	8	/* Don't return numeric addresses.  */
# define NI_DGRAM	16	/* Look up UDP service rather than TCP.  */
# define NI_NUMERICSCOPE 32

# define EAI_BADFLAGS	  -1	/* Invalid value for `ai_flags' field.  */
# define EAI_NONAME	  -2	/* NAME or SERVICE is unknown.  */
# define EAI_AGAIN	  -3	/* Temporary failure in name resolution.  */
# define EAI_FAIL	  -4	/* Non-recoverable failure in name res.  */
# define EAI_FAMILY	  -6	/* `ai_family' not supported.  */
# define EAI_SOCKTYPE	  -7	/* `ai_socktype' not supported.  */
# define EAI_SERVICE	  -8	/* SERVICE not supported for `ai_socktype'.  */
# define EAI_MEMORY	  -10	/* Memory allocation failure.  */
# define EAI_SYSTEM	  -11	/* System error returned in `errno'.  */
# define EAI_OVERFLOW	  -12	/* Argument buffer overflow.  */

extern void endhostent(void);
extern void endnetent(void);
extern void endprotoent(void);
extern void endservent(void);
/*@ requires addrinfo_valid: \valid(addrinfo);
  assigns \nothing;
  frees addrinfo;
  ensures allocation: \allocable(addrinfo);
*/
extern void freeaddrinfo(struct addrinfo * addrinfo);

char *__fc_gai_strerror = "<error message reported by gai_strerror>";

/*@
  assigns \result \from indirect:errcode, __fc_gai_strerror;
  ensures result_string: \result == __fc_gai_strerror;
  ensures result_valid_string: valid_read_string(\result);
*/
extern const char *gai_strerror(int errcode);

/*@ requires nodename_string: nodename == \null || valid_read_string(nodename);
    requires servname_string: servname == \null || valid_read_string(servname);
    requires hints_option: hints == \null || \valid_read(hints);
    requires valid_res: \valid(res);

    assigns *res \from indirect:nodename, indirect:servname, indirect:hints;
    assigns \result \from indirect:nodename, indirect:servname,indirect:hints;
    assigns errno \from indirect:nodename, indirect:servname, indirect:hints;

    allocates *res;

    behavior empty_request:
      assumes empty: nodename == \null && servname == \null;
      assigns \result \from indirect:nodename, indirect:servname;
      ensures no_name: \result == EAI_NONAME;

    behavior normal_request:
      assumes has_name: nodename != \null || servname != \null;
      ensures initialization:allocation:success_or_error:
      (\result == 0 && \fresh(*res,sizeof(*res))
        && \initialized(*res))
      || \result == EAI_AGAIN
      || \result == EAI_BADFLAGS
      || \result == EAI_FAIL
      || \result == EAI_FAMILY
      || \result == EAI_MEMORY
      || \result == EAI_SERVICE
      || \result == EAI_SOCKTYPE
      || \result == EAI_SYSTEM;

    complete behaviors;
    disjoint behaviors;
*/
extern int getaddrinfo(
  const char *restrict nodename,
  const char *restrict servname,
  const struct addrinfo *restrict hints,
  struct addrinfo **restrict res);

extern struct hostent *gethostbyaddr(const void *, socklen_t, int);
extern struct hostent *gethostbyname(const char *);
extern struct hostent *gethostent(void);
extern int getnameinfo(const struct sockaddr *restrict, socklen_t,
 char *restrict, socklen_t, char *restrict,
 socklen_t, int);
extern struct netent *getnetbyaddr(uint32_t, int);
extern struct netent *getnetbyname(const char *);
extern struct netent *getnetent(void);
extern struct protoent *getprotobyname(const char *);
extern struct protoent *getprotobynumber(int);
extern struct protoent *getprotoent(void);
extern struct servent *getservbyname(const char *, const char *);
extern struct servent *getservbyport(int, const char *);
extern struct servent *getservent(void);
extern void sethostent(int);
extern void setnetent(int);
extern void setprotoent(int);
extern void setservent(int);

__END_DECLS

__POP_FC_STDLIB
#endif


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

#ifndef __FC_STROPTS_H__
#define __FC_STROPTS_H__
#include "features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

#include "__fc_define_uid_and_gid.h"
#include "__fc_machdep.h"

typedef __INT32_T t_scalar_t;
typedef __UINT32_T t_uscalar_t;

struct bandinfo {
  int            bi_flag;
  unsigned char  bi_pri;
};

struct strbuf {
  char  *buf;
  int    len;
  int    maxlen;
};

struct strpeek {
  struct strbuf  ctlbuf;
  struct strbuf  databuf;
  t_uscalar_t    flags;
};

struct strfdinsert {
  struct strbuf  ctlbuf;
  struct strbuf  databuf;
  int            fildes;
  t_uscalar_t    flags;
  int            offset;
};

struct strioctl {
  int    ic_cmd;
  char  *ic_dp;
  int    ic_len;
  int    ic_timout;
};

struct strrecvfd {
  int    fd;
  gid_t  gid;
  uid_t  uid;
};

#define FMNAMESZ 8

struct str_mlist {
  char  l_name[FMNAMESZ+1];
};

struct str_list {
  struct str_mlist  *sl_modlist;
  int                sl_nmods;
};

// The values for the constants below are based on those of the glibc,
// declared in the order given by POSIX.1-2008.

#define I_ATMARK (__SID |31)
#define I_CANPUT (__SID |34)
#define I_CKBAND (__SID |29)
#define I_FDINSERT (__SID |16)
#define I_FIND (__SID |11)
#define I_FLUSH (__SID | 5)
#define I_FLUSHBAND (__SID |28)
#define I_FLUSHBAND (__SID |28)
#define I_GETBAND (__SID |30)
#define I_GETCLTIME (__SID |33)
#define I_GETSIG (__SID |10)
#define I_GRDOPT (__SID | 7)
#define I_GWROPT (__SID |20)
#define I_LINK (__SID |12)
#define I_LIST (__SID |21)
#define I_LOOK (__SID | 4)
#define I_NREAD (__SID | 1)
#define I_PEEK (__SID |15)
#define I_PLINK (__SID |22)
#define I_POP (__SID | 3)
#define I_PUNLINK (__SID |23)
#define I_PUSH (__SID | 2)
#define I_RECVFD (__SID |14)
#define I_SENDFD (__SID |17)
#define I_SETCLTIME (__SID |32)
#define I_SETSIG (__SID | 9)
#define I_SRDOPT (__SID | 6)
#define I_STR (__SID | 8)
#define I_SWROPT (__SID |19)
#define I_UNLINK (__SID |13)

#define FLUSHR 0x01
#define FLUSHRW 0x03
#define FLUSHRW 0x03
#define FLUSHW 0x02

#define S_BANDURG 0x0200
#define S_ERROR 0x0010
#define S_HANGUP 0x0020
#define S_HIPRI 0x0002
#define S_INPUT 0x0001
#define S_MSG 0x0008
#define S_OUTPUT 0x0004
#define S_RDBAND 0x0080
#define S_RDNORM 0x0040
#define S_WRBAND 0x0100
#define S_WRNORM S_OUTPUT

#define RS_HIPRI 0x01

#define RMSGD 0x0001
#define RMSGN 0x0002
#define RNORM 0x0000
#define RPROTDAT 0x0004
#define RPROTDIS 0x0008
#define RPROTNORM 0x0010

#define SNDZERO 0x001

#define ANYMARK 0x01
#define LASTMARK 0x02

#define MUXID_ALL (-1)

#define MORECTL 1
#define MOREDATA 2
#define MSG_ANY 0x02
#define MSG_BAND 0x04
#define MSG_HIPRI 0x01

extern int    fattach(int, const char *);
extern int    fdetach(const char *);
extern int    getmsg(int, struct strbuf *restrict, struct strbuf *restrict,
                     int *restrict);
extern int    getpmsg(int, struct strbuf *restrict, struct strbuf *restrict,
                      int *restrict, int *restrict);

extern int    ioctl(int, int, ...);

// for Variadic
/*@ assigns \result \from fd, request; */
extern int    __va_ioctl_void(int fd, int request);
/*@ assigns \result \from fd, request, argp[0..]; */
extern int    __va_ioctl_ptr(int fd, int request, char* argp);

extern int    isastream(int);
extern int    putmsg(int, const struct strbuf *, const struct strbuf *, int);
extern int    putpmsg(int, const struct strbuf *, const struct strbuf *, int,
                      int);

__END_DECLS
__POP_FC_STDLIB
#endif

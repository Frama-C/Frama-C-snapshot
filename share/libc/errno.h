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

/* ISO C: 7.5 */
#ifndef __FC_ERRNO
#define __FC_ERRNO
#include "__fc_machdep.h"

/* Mandatory */
#define EDOM __FC_EDOM
#define EILSEQ __FC_EILSEQ
#define ERANGE __FC_ERANGE

/* Implementation defined by POSIX and GNU Linux */
#define E2BIG __FC_E2BIG
#define EACCES __FC_EACCES
#define EADDRINUSE __FC_EADDRINUSE
#define EADDRNOTAVAIL __FC_EADDRNOTAVAIL
#define EAFNOSUPPORT __FC_EAFNOSUPPORT
#define EAGAIN __FC_EAGAIN
#define EALREADY __FC_EALREADY
#define EBADE __FC_EBADE
#define EBADF __FC_EBADF
#define EBADFD __FC_EBADFD
#define EBADMSG __FC_EBADMSG
#define EBADR __FC_EBADR
#define EBADRQC __FC_EBADRQC
#define EBADSLT __FC_EBADSLT
#define EBUSY __FC_EBUSY
#define ECANCELED __FC_ECANCELED
#define ECHILD __FC_ECHILD
#define ECHRNG __FC_ECHRNG
#define ECOMM __FC_ECOMM
#define ECONNABORTED __FC_ECONNABORTED
#define ECONNREFUSED __FC_ECONNREFUSED
#define ECONNRESET __FC_ECONNRESET
#define EDEADLK __FC_EDEADLK
#define EDEADLOCK __FC_EDEADLOCK
#define EDESTADDRREQ __FC_EDESTADDRREQ
#define EDQUOT __FC_EDQUOT
#define EEXIST __FC_EEXIST
#define EFAULT __FC_EFAULT
#define EFBIG __FC_EFBIG
#define EHOSTDOWN __FC_EHOSTDOWN
#define EHOSTUNREACH __FC_EHOSTUNREACH
#define EIDRM __FC_EIDRM
#define EINPROGRESS __FC_EINPROGRESS
#define EINTR __FC_EINTR
#define EINVAL __FC_EINVAL
#define EIO __FC_EIO
#define EISCONN __FC_EISCONN
#define EISDIR __FC_EISDIR
#define EISNAM __FC_EISNAM
#define EKEYEXPIRED __FC_EKEYEXPIRED
#define EKEYREJECTED __FC_EKEYREJECTED
#define EKEYREVOKED __FC_EKEYREVOKED
#define EL2HLT __FC_EL2HLT
#define EL2NSYNC __FC_EL2NSYNC
#define EL3HLT __FC_EL3HLT
#define EL3RST __FC_EL3RST
#define ELIBACC __FC_ELIBACC
#define ELIBBAD __FC_ELIBBAD
#define ELIBMAX __FC_ELIBMAX
#define ELIBSCN __FC_ELIBSCN
#define ELIBEXEC      __FC_ELIBEXEC     
#define ELOOP __FC_ELOOP
#define EMEDIUMTYPE __FC_EMEDIUMTYPE
#define EMFILE __FC_EMFILE
#define EMLINK __FC_EMLINK
#define EMSGSIZE __FC_EMSGSIZE
#define EMULTIHOP __FC_EMULTIHOP
#define ENAMETOOLONG __FC_ENAMETOOLONG
#define ENETDOWN __FC_ENETDOWN
#define ENETRESET __FC_ENETRESET
#define ENETUNREACH __FC_ENETUNREACH
#define ENFILE __FC_ENFILE
#define ENOBUFS __FC_ENOBUFS
#define ENODATA __FC_ENODATA
#define ENODEV __FC_ENODEV
#define ENOENT __FC_ENOENT
#define ENOEXEC __FC_ENOEXEC
#define ENOKEY __FC_ENOKEY
#define ENOLCK __FC_ENOLCK
#define ENOLINK __FC_ENOLINK
#define ENOMEDIUM __FC_ENOMEDIUM
#define ENOMEM __FC_ENOMEM
#define ENOMSG __FC_ENOMSG
#define ENONET __FC_ENONET
#define ENOPKG __FC_ENOPKG
#define ENOPROTOOPT __FC_ENOPROTOOPT
#define ENOSPC __FC_ENOSPC
#define ENOSR __FC_ENOSR
#define ENOSTR __FC_ENOSTR
#define ENOSYS __FC_ENOSYS
#define ENOTBLK __FC_ENOTBLK
#define ENOTCONN __FC_ENOTCONN
#define ENOTDIR __FC_ENOTDIR
#define ENOTEMPTY __FC_ENOTEMPTY
#define ENOTSOCK __FC_ENOTSOCK
#define ENOTSUP __FC_ENOTSUP
#define ENOTTY __FC_ENOTTY
#define ENOTUNIQ __FC_ENOTUNIQ
#define ENXIO __FC_ENXIO
#define EOPNOTSUPP __FC_EOPNOTSUPP
#define EOVERFLOW __FC_EOVERFLOW
#define EPERM __FC_EPERM
#define EPFNOSUPPORT __FC_EPFNOSUPPORT
#define EPIPE __FC_EPIPE
#define EPROTO __FC_EPROTO
#define EPROTONOSUPPORT __FC_EPROTONOSUPPORT
#define EPROTOTYPE __FC_EPROTOTYPE
#define EREMCHG __FC_EREMCHG
#define EREMOTE __FC_EREMOTE
#define EREMOTEIO __FC_EREMOTEIO
#define ERESTART __FC_ERESTART
#define EROFS __FC_EROFS
#define ESHUTDOWN __FC_ESHUTDOWN
#define ESPIPE __FC_ESPIPE
#define ESOCKTNOSUPPORT __FC_ESOCKTNOSUPPORT
#define ESRCH __FC_ESRCH
#define ESTALE __FC_ESTALE
#define ESTRPIPE __FC_ESTRPIPE
#define ETIME __FC_ETIME
#define ETIMEDOUT __FC_ETIMEDOUT
#define ETXTBSY __FC_ETXTBSY
#define EUCLEAN __FC_EUCLEAN
#define EUNATCH __FC_EUNATCH
#define EUSERS __FC_EUSERS
#define EWOULDBLOCK __FC_EWOULDBLOCK
#define EXDEV __FC_EXDEV
#define EXFULL __FC_EXFULL

extern int __FC_errno;

#define errno __FC_errno

/* _GNU_SOURCE */
extern char *program_invocation_name;
extern char *program_invocation_short_name;

#endif

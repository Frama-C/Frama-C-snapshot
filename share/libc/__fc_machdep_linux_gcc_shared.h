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

#ifndef __FC_FORCE_INCLUDE_MACHDEP__
#error "Frama-C: This file shall not be directly included"
#endif
/* This file contains common machine specific values between 
 Linux/GCC x86 32-bit, AMD64 and x86 16-bit.*/

#ifndef __FC_MACHDEP_LINUX_SHARED
#define __FC_MACHDEP_LINUX_SHARED

/* Optional */
#define __INT8_T signed char
#define __UINT8_T unsigned char
#define __INT16_T signed short
#define __UINT16_T unsigned short

/* Required */
#define __INT_LEAST8_T signed char
#define __UINT_LEAST8_T unsigned char
#define __INT_LEAST16_T signed short
#define __UINT_LEAST16_T unsigned short
#define __INT_LEAST64_T signed long long
#define __UINT_LEAST64_T unsigned long long

/* Required */
#define __INT_FAST8_T signed char
#define __UINT_FAST8_T unsigned char
#define __INT_FAST16_T signed int
#define __UINT_FAST16_T unsigned int
#define __INT_FAST64_T signed long long
#define __UINT_FAST64_T unsigned long long

/* Required */
#define __INT_MAX_T signed long long
#define __UINT_MAX_T unsigned long long

/* min and max values as specified in limits.h */
#define __FC_SCHAR_MIN (-128)
#define __FC_SCHAR_MAX 127
#define __FC_UCHAR_MAX 255
#define __FC_SHRT_MIN	(-32768)
#define __FC_SHRT_MAX	32767
#define __FC_USHRT_MAX	65535
#define __FC_LONG_MIN (-2147483647L -1L)
#define __FC_LLONG_MIN (-9223372036854775807LL -1LL)
#define __FC_LLONG_MAX 9223372036854775807LL
#define __FC_ULLONG_MAX 18446744073709551615ULL
#define __FC_PATH_MAX 256

/* for stdarg.h */
#define __FC_VA_LIST_T __builtin_va_list

/* stdint.h */
/* NB: in signal.h, sig_atomic_t is hardwired to int. */
#define __FC_SIG_ATOMIC_MIN __FC_INT_MIN
#define __FC_SIG_ATOMIC_MAX __FC_INT_MAX
#define __FC_WCHAR_MIN __FC_INT_MIN
#define __FC_WCHAR_MAX __FC_INT_MAX
#define __FC_INTMAX_MIN (-9223372036854775807LL -1LL)
#define __FC_INTMAX_MAX 9223372036854775807LL
#define __FC_UINTMAX_MAX 18446744073709551615ULL

// Linux/GCC usually defines wchar_t as a signed int, but this is not required
#define __WCHAR_T int

/* stdio.h */
#define __FC_BUFSIZ 8192
#define __FC_EOF (-1)
#define __FC_FOPEN_MAX 16
#define __FC_FILENAME_MAX 2048
#define __FC_L_tmpnam 2048
#define __FC_TMP_MAX 0xFFFFFFFF

/* stdlib.h */
#define __FC_RAND_MAX 32767
#define __FC_MB_CUR_MAX ((size_t)16)

/* errno.h */
#define __FC_EPERM 1
#define __FC_ENOENT 2
#define __FC_ESRCH 3
#define __FC_EINTR 4
#define __FC_EIO 5
#define __FC_ENXIO 6
#define __FC_E2BIG 7
#define __FC_ENOEXEC 8
#define __FC_EBADF 9
#define __FC_ECHILD 10
#define __FC_EAGAIN 11
#define __FC_ENOMEM 12
#define __FC_EACCES 13
#define __FC_EFAULT 14
#define __FC_ENOTBLK 15
#define __FC_EBUSY 16
#define __FC_EEXIST 17
#define __FC_EXDEV 18
#define __FC_ENODEV 19
#define __FC_ENOTDIR 20
#define __FC_EISDIR 21
#define __FC_EINVAL 22
#define __FC_ENFILE 23
#define __FC_EMFILE 24
#define __FC_ENOTTY 25
#define __FC_ETXTBSY 26
#define __FC_EFBIG 27
#define __FC_ENOSPC 28
#define __FC_ESPIPE 29
#define __FC_EROFS 30
#define __FC_EMLINK 31
#define __FC_EPIPE 32
#define __FC_EDOM 33
#define __FC_ERANGE 34
#define __FC_EDEADLK 35
#define __FC_ENAMETOOLONG 36
#define __FC_ENOLCK 37
#define __FC_ENOSYS 38
#define __FC_ENOTEMPTY 39
#define __FC_ELOOP 40
#define __FC_EWOULDBLOCK EAGAIN
#define __FC_ENOMSG 42
#define __FC_EIDRM 43
#define __FC_ECHRNG 44
#define __FC_EL2NSYNC 45
#define __FC_EL3HLT 46
#define __FC_EL3RST 47
#define __FC_ELNRNG 48
#define __FC_EUNATCH 49
#define __FC_ENOCSI 50
#define __FC_EL2HLT 51
#define __FC_EBADE 52
#define __FC_EBADR 53
#define __FC_EXFULL 54
#define __FC_ENOANO 55
#define __FC_EBADRQC 56
#define __FC_EBADSLT 57
#define __FC_EDEADLOCK EDEADLK
#define __FC_EBFONT 59
#define __FC_ENOSTR 60
#define __FC_ENODATA 61
#define __FC_ETIME 62
#define __FC_ENOSR 63
#define __FC_ENONET 64
#define __FC_ENOPKG 65
#define __FC_EREMOTE 66
#define __FC_ENOLINK 67
#define __FC_EADV 68
#define __FC_ESRMNT 69
#define __FC_ECOMM 70
#define __FC_EPROTO 71
#define __FC_EMULTIHOP 72
#define __FC_EDOTDOT 73
#define __FC_EBADMSG 74
#define __FC_EOVERFLOW 75
#define __FC_ENOTUNIQ 76
#define __FC_EBADFD 77
#define __FC_EREMCHG 78
#define __FC_ELIBACC 79
#define __FC_ELIBBAD 80
#define __FC_ELIBSCN 81
#define __FC_ELIBMAX 82
#define __FC_ELIBEXEC 83
#define __FC_EILSEQ 84
#define __FC_ERESTART 85
#define __FC_ESTRPIPE 86
#define __FC_EUSERS 87
#define __FC_ENOTSOCK 88
#define __FC_EDESTADDRREQ 89
#define __FC_EMSGSIZE 90
#define __FC_EPROTOTYPE 91
#define __FC_ENOPROTOOPT 92
#define __FC_EPROTONOSUPPORT 93
#define __FC_ESOCKTNOSUPPORT 94
#define __FC_ENOTSUP 95
#define __FC_EOPNOTSUPP 95
#define __FC_EPFNOSUPPORT 96
#define __FC_EAFNOSUPPORT 97
#define __FC_EADDRINUSE 98
#define __FC_EADDRNOTAVAIL 99
#define __FC_ENETDOWN 100
#define __FC_ENETUNREACH 101
#define __FC_ENETRESET 102
#define __FC_ECONNABORTED 103
#define __FC_ECONNRESET 104
#define __FC_ENOBUFS 105
#define __FC_EISCONN 106
#define __FC_ENOTCONN 107
#define __FC_ESHUTDOWN 108
#define __FC_ETOOMANYREFS 109
#define __FC_ETIMEDOUT 110
#define __FC_ECONNREFUSED 111
#define __FC_EHOSTDOWN 112
#define __FC_EHOSTUNREACH 113
#define __FC_EALREADY 114
#define __FC_EINPROGRESS 115
#define __FC_ESTALE 116
#define __FC_EUCLEAN 117
#define __FC_ENOTNAM 118
#define __FC_ENAVAIL 119
#define __FC_EISNAM 120
#define __FC_EREMOTEIO 121
#define __FC_EDQUOT 122
#define __FC_ENOMEDIUM 123
#define __FC_EMEDIUMTYPE 124
#define __FC_ECANCELED 125
#define __FC_ENOKEY 126
#define __FC_EKEYEXPIRED 127
#define __FC_EKEYREVOKED 128
#define __FC_EKEYREJECTED 129
#define __FC_EOWNERDEAD 130
#define __FC_ENOTRECOVERABLE 131
#define __FC_ERFKILL 132
#define __FC_EHWPOISON 133

/* time.h */
#define __FC_TIME_T long

#endif

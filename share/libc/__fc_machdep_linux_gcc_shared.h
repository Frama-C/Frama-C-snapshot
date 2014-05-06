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
#define __FC_INT_MIN (-INT_MAX - 1)
#define __FC_INT_MAX 2147483647
#define __FC_UINT_MAX 4294967295U
#define __FC_LONG_MIN (-LONG_MAX -1L)
#define __FC_LLONG_MIN (-LLONG_MAX -1LL)
#define __FC_LLONG_MAX 9223372036854775807LL
#define __FC_ULLONG_MAX 18446744073709551615ULL

/* Unused at this time */
#define __FC_umax(n) ((uint##n##_t)(-1))
#define __FC_smin(n) (2*(-(1ll << (sizeof(int##n##_t)*__CHAR_BIT - 2))))
#define __FC_smax(n) ((1ll<<(sizeof(int##n##_t)*__CHAR_BIT - 2))-1+(1ll<<(sizeof(int##n##_t)*__CHAR_BIT - 2)))

/* stdint.h */
/* NB: in signal.h, sig_atomic_t is hardwired to int. */
#define __FC_SIG_ATOMIC_MIN __FC_INT_MIN
#define __FC_SIG_ATOMIC_MAX __FC_INT_MAX
#define __FC_SIZE_MAX __FC_UINT_MAX
#define __FC_WCHAR_MIN __FC_INT_MIN
#define __FC_WCHAR_MAX __FC_INT_MAX

// To be defined in coordination with wchar.h which is currently unsupported
#define __WCHAR_T int
#define __FC_WINT_MIN __FC_INT_MIN
#define __FC_WINT_MAX __FC_INT_MAX
// 7.25 mandates that WINT_T can handle at least one character in addition
// to those that are in the extended character set (to account for EOF)
#define __WINT_T long long int

/* stdio.h */
#define __FC_BUFSIZ 8192
#define __FC_EOF (-1)
#define __FC_FOPEN_MAX 512
#define __FC_FILENAME_MAX 2048
#define __FC_L_tmpnam 2048
#define __FC_TMP_MAX 0xFFFFFFFF

/* stdlib.h */
#define __FC_RAND_MAX 32767
#define __FC_MB_CUR_MAX ((size_t)16)

/* errno.h */
#define __FC_EDOM 1
#define __FC_EILSEQ 2
#define __FC_ERANGE 3

#define __FC_E2BIG 4
#define __FC_EACCES 5
#define __FC_EADDRINUSE 6
#define __FC_EADDRNOTAVAIL 7
#define __FC_EAFNOSUPPORT 8
#define __FC_EAGAIN 9
#define __FC_EALREADY 10
#define __FC_EBADE 11
#define __FC_EBADF 12
#define __FC_EBADFD 13
#define __FC_EBADMSG 14
#define __FC_EBADR 15
#define __FC_EBADRQC 16
#define __FC_EBADSLT 17
#define __FC_EBUSY 18
#define __FC_ECANCELED 19
#define __FC_ECHILD 20
#define __FC_ECHRNG 21
#define __FC_ECOMM 22
#define __FC_ECONNABORTED 23
#define __FC_ECONNREFUSED 24
#define __FC_ECONNRESET 25
#define __FC_EDEADLK 26
#define __FC_EDEADLOCK 27
#define __FC_EDESTADDRREQ 28
#define __FC_EDQUOT 29
#define __FC_EEXIST 30
#define __FC_EFAULT 31
#define __FC_EFBIG 32
#define __FC_EHOSTDOWN 33
#define __FC_EHOSTUNREACH 34
#define __FC_EIDRM 35
#define __FC_EINPROGRESS 36
#define __FC_EINTR 37
#define __FC_EINVAL 38
#define __FC_EIO 39
#define __FC_EISCONN 40
#define __FC_EISDIR 41
#define __FC_EISNAM 42
#define __FC_EKEYEXPIRED 43
#define __FC_EKEYREJECTED 44
#define __FC_EKEYREVOKED 45
#define __FC_EL2HLT 46
#define __FC_EL2NSYNC 47
#define __FC_EL3HLT 48
#define __FC_EL3RST 49
#define __FC_ELIBACC 50
#define __FC_ELIBBAD 51
#define __FC_ELIBMAX 52
#define __FC_ELIBSCN 53
#define __FC_ELIBEXEC 54
#define __FC_ELOOP 55
#define __FC_EMEDIUMTYPE 56
#define __FC_EMFILE 57
#define __FC_EMLINK 58
#define __FC_EMSGSIZE 59
#define __FC_EMULTIHOP 60
#define __FC_ENAMETOOLONG 61
#define __FC_ENETDOWN 62
#define __FC_ENETRESET 63
#define __FC_ENETUNREACH 64
#define __FC_ENFILE 65
#define __FC_ENOBUFS 66
#define __FC_ENODATA 67
#define __FC_ENODEV 68
#define __FC_ENOENT 69
#define __FC_ENOEXEC 70
#define __FC_ENOKEY 71
#define __FC_ENOLCK 72
#define __FC_ENOLINK 73
#define __FC_ENOMEDIUM 74
#define __FC_ENOMEM 75
#define __FC_ENOMSG 76
#define __FC_ENONET 77
#define __FC_ENOPKG 78
#define __FC_ENOPROTOOPT 79
#define __FC_ENOSPC 80
#define __FC_ENOSR 81
#define __FC_ENOSTR 82
#define __FC_ENOSYS 83
#define __FC_ENOTBLK 84
#define __FC_ENOTCONN 85
#define __FC_ENOTDIR 86
#define __FC_ENOTEMPTY 87
#define __FC_ENOTSOCK 88
#define __FC_ENOTSUP 89
#define __FC_ENOTTY 90
#define __FC_ENOTUNIQ 91
#define __FC_ENXIO 92
#define __FC_EOPNOTSUPP 93
#define __FC_EOVERFLOW 94
#define __FC_EPERM 95
#define __FC_EPFNOSUPPORT 96
#define __FC_EPIPE 97
#define __FC_EPROTO 98
#define __FC_EPROTONOSUPPORT 99
#define __FC_EPROTOTYPE 100
#define __FC_EREMCHG 101
#define __FC_EREMOTE 102
#define __FC_EREMOTEIO 103
#define __FC_ERESTART 104
#define __FC_EROFS 105
#define __FC_ESHUTDOWN 106
#define __FC_ESPIPE 107
#define __FC_ESOCKTNOSUPPORT 108
#define __FC_ESRCH 109
#define __FC_ESTALE 110
#define __FC_ESTRPIPE 111
#define __FC_ETIME 112
#define __FC_ETIMEDOUT 113
#define __FC_ETXTBSY 114
#define __FC_EUCLEAN 115
#define __FC_EUNATCH 116
#define __FC_EUSERS 117
#define __FC_EWOULDBLOCK 118
#define __FC_EXDEV 119
#define __FC_EXFULL 120

/* sys/un.h */
#define __FC_SOCKADDR_SUN_SUN_PATH 108

#endif

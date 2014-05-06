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

#ifndef MACHINE_H
#define MACHINE_H

#include "libc/__fc_machdep.h"

#ifdef FRAMA_C_LYNX
typedef unsigned int size_t;
typedef int ssize_t;
typedef void FILE;
typedef long time_t;
struct tm {
	int tm_sec;
	int tm_min;
	int tm_hour;
	int tm_mday;
	int tm_mon;
	int tm_year;
	int tm_wday;
	int tm_yday;
	int tm_isdst;
};

typedef struct fsynch {
	int w_count;
	int mut_owner;
	unsigned int id;
	int owncnt;
} fsynch_struct;

typedef struct __iobuf {
	char *_ptr;
	int _cnt;
	char *_base;
	short _flag;
	char _nobuf;		/* One-character buffer used for _UNBUF */
	char _unused;
	int _fd;
	long _mark;			/* position relative to start of file of _base */
	int _bufsize;		/* buffer size for this file */
	fsynch_struct lock;	/* Guards against concurrent access */
} FILE;

#else
// This is the default for regression tests
typedef __SIZE_T size_t;
typedef __SSIZE_T ssize_t;
//typedef  FILE;
typedef long time_t;
#ifdef FRAMA_CXX
struct tm;
struct FRAMA_C_IO_FILE;
#else
struct tm {
	int tm_sec;
	int tm_min;
	int tm_hour;
	int tm_mday;
	int tm_mon;
	int tm_year;
	int tm_wday;
	int tm_yday;
	int tm_isdst;
};
struct FRAMA_C_IO_FILE { char *content; };
#endif
typedef struct FRAMA_C_IO_FILE FILE;
#endif

#endif //MACHINE_H

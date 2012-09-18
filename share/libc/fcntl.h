/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2012                                               */
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

#ifndef __FC_FCNTL
#define __FC_FCNTL

#include "__fc_define_off_t.h"
#include "__fc_define_pid_t.h"
#include "__fc_define_mode_t.h"

/* For posix fcntl() and `l_type' field of a `struct flock' for lockf().  */
#define F_RDLCK		0	/* Read lock.  */
#define F_WRLCK		1	/* Write lock.	*/
#define F_UNLCK		2	/* Remove lock.	 */

/* For old implementation of bsd flock().  */
#define F_EXLCK		4	/* or 3 */
#define F_SHLCK		8	/* or 4 */
struct flock
  {
    short int l_type;	/* Type of lock: F_RDLCK, F_WRLCK, or F_UNLCK.	*/
    short int l_whence;	/* Where `l_start' is relative to (like `lseek').  */
    off_t l_start;	/* Offset where the lock begins.  */
    off_t l_len;	/* Size of the locked area; zero means until EOF.  */
    pid_t l_pid;	/* Process holding the lock.  */
 };

#define F_DUPFD 1
#define F_GETFD 2
#define F_SETFD 3
#define F_GETFL 4
#define F_SETFL 5
#define F_GETLK 6
#define F_SETLK 7
#define F_SETLKW 8
#define F_GETOWN 9
#define F_SETOWN 10

#define FD_CLOEXEC 1

#include "__fc_define_seek_macros.h"
#define O_CREAT (1<<1)
#define O_EXCL (1<<2)
#define O_NOCTTY (1<<3)
#define O_TRUNC (1<<4)

#define O_APPEND 1
#define O_DSYNC 2
#define O_NONBLOCK 3
#define O_RSYNC 4
#define O_SYNC 5

#define O_ACCMODE 0xFF

#define O_RDONLY 1
#define O_RDWR 2
#define O_WRONLY 3

int  creat(const char *, mode_t);
int  fcntl(int, int, ...);
int  open(const char *, int, ...);

#endif

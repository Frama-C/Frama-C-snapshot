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

#ifndef __FC_FCNTL
#define __FC_FCNTL
#include "features.h"
__PUSH_FC_STDLIB

#include "__fc_define_off_t.h"
#include "__fc_define_pid_t.h"
#include "__fc_define_mode_t.h"

// The values for the constants below are based on an x86 Linux,
// declared in the order given by POSIX.1-2008.
// Macros which are not defined in such Linux are commented out.
#define F_DUPFD 0
#define F_DUPFD_CLOEXEC 0x406
#define F_GETFD 1
#define F_SETFD 2
#define F_GETFL 3
#define F_SETFL 4
#define F_GETLK 5
#define F_SETLK 6
#define F_SETLKW 7
#define F_GETOWN 9
#define F_SETOWN 8

#define FD_CLOEXEC 1

#define F_RDLCK 0
#define F_UNLCK 2
#define F_WRLCK 1

#include "__fc_define_seek_macros.h"

#define O_CLOEXEC 0x80000
#define O_CREAT 0x40
#define O_DIRECTORY 0x10000
#define O_EXCL 0x80
#define O_NOCTTY 0x100
#define O_NOFOLLOW 0x20000
#define O_TRUNC 0x200
//#define O_TTY_INIT

#define O_APPEND 0x400
#define O_DSYNC 0x1000
#define O_NONBLOCK 0x800
#define O_RSYNC 0x101000
#define O_SYNC 0x101000

#define O_ACCMODE 3

//#define O_EXEC
#define O_RDONLY 0
#define O_RDWR 2
//#define O_SEARCH
#define O_WRONLY 1

#define AT_FDCWD -100

#define AT_EACCESS 0x200

#define AT_SYMLINK_NOFOLLOW 0x100

#define AT_SYMLINK_FOLLOW 0x400

#define AT_REMOVEDIR 0x200

#define POSIX_FADV_DONTNEED 4
#define POSIX_FADV_NOREUSE 5
#define POSIX_FADV_NORMAL 0
#define POSIX_FADV_RANDOM 1
#define POSIX_FADV_SEQUENTIAL 2
#define POSIX_FADV_WILLNEED 3

__BEGIN_DECLS

struct flock
  {
    short int l_type;	/* Type of lock: F_RDLCK, F_WRLCK, or F_UNLCK.	*/
    short int l_whence;	/* Where `l_start' is relative to (like `lseek').  */
    off_t l_start;	/* Offset where the lock begins.  */
    off_t l_len;	/* Size of the locked area; zero means until EOF.  */
    pid_t l_pid;	/* Process holding the lock.  */
 };

/*@ assigns \result \from filename[0..], mode ; */
extern int creat(const char *filename, mode_t mode);
/*@ assigns \result \from fd, cmd ; */
extern int fcntl(int fd, int cmd, ...);
/*@ assigns \result \from filename[0..], flags ; */
extern int open(const char *filename, int flags, ...);
/*@ assigns \result \from dirfd, filename[0..], flags ; */
extern int openat(int dirfd, const char *filename, int flags, ...);

/* The following functions are "fixed-argument" versions of open/fcntl. They
   are used when the translation of variadic function to fixed-adic is
   enabled */

/*@ requires valid_cmd: cmd == F_GETFD || cmd == F_GETFL ||
                        cmd == F_GETOWN ;
    assigns \result \from fd, cmd ; */
extern int __va_fcntl_void(int fd, int cmd);
/*@ requires valid_cmd: cmd == F_DUPFD || cmd == F_SETFD ||
                        cmd == F_SETFL || cmd == F_SETOWN ;
    assigns \result \from fd, cmd, arg ;*/
extern int __va_fcntl_int(int fd, int cmd, int arg);
/*@ requires valid_cmd: cmd == F_GETLK || cmd == F_SETLK ||
                        cmd == F_SETLKW ;
    requires valid_arg: \valid(arg) ; 
    assigns \result, *arg \from fd, cmd, *arg ; */
extern int __va_fcntl_flock(int fd, int cmd, struct flock *arg);
/*@ requires valid_flag: !(flags & O_CREAT) ;
    assigns \result \from filename[0..], flags ; */
extern int __va_open_void(const char *filename, int flags);
/*@ assigns \result \from filename[0..], flags, mode ; */
extern int __va_open_mode_t(const char *filename, int flags, mode_t mode);
/*@ requires valid_flag: !(flags & O_CREAT) ;
    assigns \result \from dirfd, filename[0..], flags ; */
extern int __va_openat_void(int dirfd, const char *filename, int flags);
/*@ assigns \result \from dirfd, filename[0..], flags, mode ; */
extern int __va_openat_mode_t(int dirfd, const char *filename, int flags, mode_t mode);

// The constants below are not in POSIX-1.2008, but are kept for compatibility

#define O_NDELAY O_NONBLOCK
#define O_FSYNC O_SYNC
#define O_ASYNC 0x2000

/* For old implementation of bsd flock().  */
#define F_EXLCK 4
#define F_SHLCK 8

__END_DECLS

__POP_FC_STDLIB
#endif

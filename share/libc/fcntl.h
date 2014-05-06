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

# define AT_FDCWD		-100	/* Special value used to indicate
					   the *at functions should use the
					   current working directory. */
# define AT_SYMLINK_NOFOLLOW	0x100	/* Do not follow symbolic links.  */
# define AT_REMOVEDIR		0x200	/* Remove directory instead of
					   unlinking file.  */
# define AT_SYMLINK_FOLLOW	0x400	/* Follow symbolic links.  */
# define AT_NO_AUTOMOUNT	0x800	/* Suppress terminal automount
					   traversal.  */
# define AT_EACCESS		0x200	/* Test access permitted for
					   effective IDs, not real IDs.  */


#define	O_RDONLY	0x0000		/* open for reading only */
#define	O_WRONLY	0x0001		/* open for writing only */
#define	O_RDWR		0x0002		/* open for reading and writing */
#define	O_ACCMODE	0x0003		/* mask for above modes */
#define	O_NONBLOCK	0x0004		/* no delay */
#define	O_APPEND	0x0008		/* set append mode */
#define	O_CREAT		0x0200		/* create if nonexistent */
#define	O_TRUNC		0x0400		/* truncate to zero length */
#define	O_EXCL		0x0800		/* error if already exists */

#define O_NDELAY	O_NONBLOCK
#define O_SYNC	       04010000
#define O_FSYNC		 O_SYNC
#define O_ASYNC		 020000

# define O_DIRECTORY	0200000	/* Must be a directory.	 */
# define O_NOFOLLOW	0400000	/* Do not follow links.	 */
# define O_CLOEXEC     02000000 /* Set close_on_exec.  */

# define O_DIRECT	 040000	/* Direct disk access.	*/
# define O_NOATIME     01000000 /* Do not set atime.  */

/* Advise to `posix_fadvise'.  */
# define POSIX_FADV_NORMAL	0 /* No further special treatment.  */
# define POSIX_FADV_RANDOM	1 /* Expect random page references.  */
# define POSIX_FADV_SEQUENTIAL	2 /* Expect sequential page references.	 */
# define POSIX_FADV_WILLNEED	3 /* Will need these pages.  */
# define POSIX_FADV_DONTNEED	4 /* Don't need these pages.  */
# define POSIX_FADV_NOREUSE	5 /* Data will be accessed once.  */

/* Defined by POSIX 1003.1; BSD default, but must be distinct from O_RDONLY. */
#define	O_NOCTTY	0x8000		/* don't assign controlling terminal */

/*@ assigns \result \from filename[0..], mode ; */
int  creat(const char * filename, mode_t mode);
int  fcntl(int, int, ...);
/*@ assigns \result \from filename[0..], mode ; */
int  open(const char * filename, int mode, ...);

#endif

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

#ifndef __FC_DEFINE_MODE_T
#define __FC_DEFINE_MODE_T

typedef unsigned int mode_t;
/* POSIX symbolic values */
#define S_IFMT (1<<1)
#define S_IFBLK (1<<2)
#define S_IFCHR (1<<3)
#define S_IFIFO (1<<4)
#define S_IFREG (1<<5)
#define S_IFDIR (1<<6)
#define S_IFLNK (1<<7)
#define S_IFSOCK (1<<8)

#define S_IRWXU (S_IRUSR|S_IWUSR|S_IXUSR)
#define S_IRUSR (1<<9)
#define S_IWUSR (1<<10)
#define S_IXUSR (1<<11)
#define S_IRWXG (S_IRGRP|S_IWGRP|S_IXGRP)
#define S_IRGRP (1<<12)
#define S_IWGRP (1<<13)
#define S_IXGRP (1<<14)
#define S_IRWXO (S_IROTH|S_IWOTH|S_IXOTH)
#define S_IROTH (1<<15)
#define S_IWOTH (1<<16)
#define S_IXOTH (1<<17)
#define S_ISUID (1<<18)
#define S_ISGID (1<<19)
#define S_ISVTX (1<<20)

#define S_IEXEC         S_IXUSR
#define S_IWRITE        S_IWUSR
#define S_IREAD         S_IRUSR

#define S_ISREG(m)      (((m) & S_IFMT) == S_IFREG)
#define S_ISDIR(m)      (((m) & S_IFMT) == S_IFDIR)
#define S_ISCHR(m)      (((m) & S_IFMT) == S_IFCHR)
#define S_ISBLK(m)      (((m) & S_IFMT) == S_IFBLK)
#define S_ISLNK(m)      (((m) & S_IFMT) == S_IFLNK)
#define S_ISFIFO(m)     (((m) & S_IFMT) == S_IFIFO)

#endif


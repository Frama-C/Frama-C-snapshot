/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2015                                               */
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

#ifndef __FC_DIRENT_H
#define __FC_DIRENT_H
#include "features.h"

#include "errno.h"

#include "__fc_define_ino_t.h"
#include "__fc_define_off_t.h"

__BEGIN_DECLS

struct dirent {
    ino_t          d_ino;       /* inode number */
    off_t          d_off;       /* offset to the next dirent */
    unsigned short d_reclen;    /* length of this record */
    unsigned char  d_type;      /* type of file; not supported
                                   by all file system types */
    char           d_name[256]; /* filename */
};

typedef struct DIR {
  unsigned int __fc_dir_id;
  unsigned int __fc_dir_position;
  struct stat* __fc_dir_inode;
  struct dirent ** __fc_dir_entries;
} DIR;

DIR __fc_opendir[__FC_FOPEN_MAX];
DIR* const __p_fc_opendir = __fc_opendir;

int            alphasort(const struct dirent **, const struct dirent **);

/*@
  requires \subset(dirp,&__fc_opendir[0 .. __FC_FOPEN_MAX-1]);
  assigns \result \from dirp, *dirp, __p_fc_opendir;
  assigns __FC_errno \from dirp, *dirp, __p_fc_opendir;
  assigns *dirp \from dirp, *dirp, __p_fc_opendir;
  ensures (\result == 0 && dirp->__fc_dir_inode == \null)
           || \result == -1;
*/
int            closedir(DIR *dirp);
int            dirfd(DIR *);
DIR           *fdopendir(int);

/*@
  assigns \result \from path[0..], __p_fc_opendir;
  assigns __FC_errno \from path[0..], __p_fc_opendir;
  ensures \result == \null || \valid(\result);
  ensures \result != \null ==>
             \result == &__fc_opendir[\result->__fc_dir_id];
  ensures \result != \null ==> \result->__fc_dir_inode != \null;
*/
DIR           *opendir(const char *path);

/*@
  requires \subset(dirp, &__fc_opendir[0 .. __FC_FOPEN_MAX-1]);
  assigns \result \from *dirp, __p_fc_opendir;
  assigns dirp->__fc_dir_position \from dirp->__fc_dir_position;
  assigns __FC_errno \from dirp, *dirp, __p_fc_opendir;
  ensures \result == \null || \valid(\result);
*/
struct dirent *readdir(DIR *dirp);

int            readdir_r(DIR * dirp, struct dirent * entry,
			 struct dirent ** result);
void           rewinddir(DIR *);
int            scandir(const char *, struct dirent ***,
                   int (*)(const struct dirent *),
                   int (*)(const struct dirent **,
                   const struct dirent **));

void           seekdir(DIR *, long);
long           telldir(DIR *);



/* File types for `d_type'.  */
enum
  {
    DT_UNKNOWN = 0,
# define DT_UNKNOWN	DT_UNKNOWN
    DT_FIFO = 1,
# define DT_FIFO	DT_FIFO
    DT_CHR = 2,
# define DT_CHR		DT_CHR
    DT_DIR = 4,
# define DT_DIR		DT_DIR
    DT_BLK = 6,
# define DT_BLK		DT_BLK
    DT_REG = 8,
# define DT_REG		DT_REG
    DT_LNK = 10,
# define DT_LNK		DT_LNK
    DT_SOCK = 12,
# define DT_SOCK	DT_SOCK
    DT_WHT = 14
# define DT_WHT		DT_WHT
  };

/* Convert between stat structure types and directory types.  */
# define IFTODT(mode)	(((mode) & 0170000) >> 12)
# define DTTOIF(dirtype)	((dirtype) << 12)

__END_DECLS

#endif


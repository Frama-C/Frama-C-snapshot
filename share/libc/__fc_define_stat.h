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

#ifndef __FC_DEFINE_STAT_H
#define __FC_DEFINE_STAT_H


#include "__fc_define_ino_t.h"
#include "__fc_define_uid_and_gid.h"
#include "__fc_define_time_t.h"
#include "__fc_define_blkcnt_t.h"
#include "__fc_define_blksize_t.h"
#include "__fc_define_dev_t.h"
#include "__fc_define_mode_t.h"
#include "__fc_define_nlink_t.h"
#include "__fc_define_off_t.h"

#define __statfs_word unsigned int

struct statfs {
	__statfs_word f_type;
	__statfs_word f_bsize;
	__statfs_word f_blocks;
	__statfs_word f_bfree;
	__statfs_word f_bavail;
	__statfs_word f_files;
	__statfs_word f_ffree;
	__statfs_word  f_fsid;
	__statfs_word f_namelen;
	__statfs_word f_frsize;
	__statfs_word f_flags;
	__statfs_word f_spare[4];
};

struct stat {
  dev_t     st_dev;
  ino_t     st_ino;
  mode_t    st_mode;
  nlink_t   st_nlink;
  uid_t     st_uid;
  gid_t     st_gid;
  dev_t     st_rdev;
  off_t     st_size;
  time_t    st_atime;
  time_t    st_mtime;
  time_t    st_ctime;
  blksize_t st_blksize;
  blkcnt_t  st_blocks;
  char *    __fc_real_data;
  int       __fc_real_data_max_size;
};

#endif

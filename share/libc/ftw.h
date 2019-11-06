/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2019                                               */
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

#ifndef __FC_FTW
#define __FC_FTW
#include "features.h"
__PUSH_FC_STDLIB

__BEGIN_DECLS

struct FTW
{
  int base;
  int level;
};

enum __fc_ftw
{
  FTW_F,
#define FTW_F FTW_F
  FTW_D,
#define FTW_D FTW_D
  FTW_DNR,
#define FTW_DNR FTW_DNR
  FTW_DP,
#define FTW_DP FTW_DP
  FTW_NS,
#define FTW_NS FTW_NS
  FTW_SL,
#define FTW_SL FTW_SL
  FTW_SLN,
#define FTW_SLN FTW_SLN
};

enum __fc_nftw
{
  NFTW_PHYS,
#define NFTW_PHYS NFTW_PHYS
  NFTW_MOUNT,
#define NFTW_MOUNT NFTW_MOUNT
  NFTW_DEPTH,
#define NFTW_DEPTH NFTW_DEPTH
  NFTW_CHDIR,
#define NFTW_CHDIR NFTW_CHDIR
};

// From POSIX 1.2008: "Inclusion of the <ftw.h> header may also make visible
//                     all symbols from <sys/stat.h>".
#include "sys/stat.h"

int ftw(const char *path,
        int (*fn)(const char *, const struct stat *ptr, int flag), int ndirs);

int nftw(const char *path,
         int (*fn)(const char *, const struct stat *, int, struct FTW *),
         int fd_limit, int flags);

__END_DECLS

__POP_FC_STDLIB
#endif

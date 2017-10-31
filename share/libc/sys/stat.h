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

#ifndef __FC_SYS_STAT_H
#define __FC_SYS_STAT_H
#include "features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

#include "../__fc_define_stat.h"

extern int    chmod(const char *, mode_t);
extern int    fchmod(int, mode_t);
extern int    fstat(int, struct stat *);
extern int    lstat(const char *, struct stat *);
extern int    mkdir(const char *, mode_t);
extern int    mkfifo(const char *, mode_t);
extern int    mknod(const char *, mode_t, dev_t);
extern int    stat(const char *, struct stat *);
extern mode_t umask(mode_t);

__END_DECLS
__POP_FC_STDLIB
#endif

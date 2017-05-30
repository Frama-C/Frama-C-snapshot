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

#ifndef __FC_GRP_H
#define __FC_GRP_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_uid_and_gid.h"
#include "__fc_define_size_t.h"

__BEGIN_DECLS

struct group {
  char   *gr_name;
  gid_t   gr_gid;
  char  **gr_mem;
};

extern struct group  *getgrgid(gid_t);
extern struct group  *getgrnam(const char *);
extern int getgrgid_r(gid_t, struct group *, char *,
 size_t, struct group **);
extern int getgrnam_r(const char *, struct group *, char *,
 size_t , struct group **);
extern struct group *getgrent(void);
extern void endgrent(void);
extern void setgrent(void);

/* BSD function */
extern int initgroups (const char *user, gid_t group);

__END_DECLS

__POP_FC_STDLIB
#endif


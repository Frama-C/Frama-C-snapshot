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

#ifndef __FC_STRINGS_H_
#define __FC_STRINGS_H_
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_size_t.h"

__BEGIN_DECLS

extern int    bcmp(const void *, const void *, size_t);
extern void   bcopy(const void *, void *, size_t);


/*@ requires \valid (((char*) s)+(0 .. n-1));
  assigns ((char*) s)[0 .. n-1] \from \nothing;
  ensures \subset(((char*) s)[0 .. n-1], {0}); */
extern void   bzero(void *s, size_t n);
extern int    ffs(int);
extern char   *index(const char *, int);
extern char   *rindex(const char *, int);
extern int    strcasecmp(const char *, const char *);
extern int    strncasecmp(const char *, const char *, size_t);

__END_DECLS

__POP_FC_STDLIB
#endif

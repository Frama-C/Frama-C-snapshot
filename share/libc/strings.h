/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2018                                               */
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
#include "__fc_string_axiomatic.h"

__BEGIN_DECLS

extern int    bcmp(const void *, const void *, size_t);
extern void   bcopy(const void *, void *, size_t);


/*@ requires valid_memory_area: \valid (((char*) s)+(0 .. n-1));
  assigns ((char*) s)[0 .. n-1] \from \nothing;
  ensures zero_initialized: \subset(((char*) s)[0 .. n-1], {0}); */
extern void   bzero(void *s, size_t n);
extern int    ffs(int);
extern char   *index(const char *, int);
extern char   *rindex(const char *, int);

/*@
  requires valid_string_s1: valid_read_string(s1);
  requires valid_string_s2: valid_read_string(s2);
  assigns \result \from indirect:s1[0..], indirect:s2[0..];
*/
extern int    strcasecmp(const char *s1, const char *s2);

/*@
  requires valid_string_s1: valid_read_nstring(s1, n);
  requires valid_string_s2: valid_read_nstring(s2, n);
  assigns \result \from indirect:n, indirect:s1[0..n-1], indirect:s2[0..n-1];
*/
extern int    strncasecmp(const char *s1, const char *s2, size_t n);

__END_DECLS

__POP_FC_STDLIB
#endif

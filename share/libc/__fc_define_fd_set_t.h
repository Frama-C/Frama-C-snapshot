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

#ifndef __FC_DEFINE_FD_SET_T
#define __FC_DEFINE_FD_SET_T
#include "features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS
typedef struct {char __fc_fd_set;} fd_set;

/*@
  requires \valid(fdset);
  requires \initialized(fdset);
  assigns *fdset \from *fdset, indirect:fd;
*/
extern void FD_CLR(int fd, fd_set *fdset);
#define FD_CLR FD_CLR

// Note: the 2nd argument in FD_ISSET is not const in some implementations
// due to historical and compatibility reasons.
/*@
  requires \valid_read(fdset);
  requires \initialized(fdset);
  assigns \result \from indirect:*fdset, indirect:fd;
*/
extern int FD_ISSET(int fd, const fd_set *fdset);
#define FD_ISSET FD_ISSET

/*@
  requires \valid(fdset);
  requires \initialized(fdset);
  assigns *fdset \from *fdset, indirect:fd;
*/
extern void FD_SET(int fd, fd_set *fdset);
#define FD_SET FD_SET

/*@
  assigns *fdset \from \nothing;
  ensures \initialized(fdset);
*/
extern void FD_ZERO(fd_set *fdset);
#define FD_ZERO FD_ZERO

__END_DECLS
#define FD_SETSIZE 1024
__POP_FC_STDLIB
#endif

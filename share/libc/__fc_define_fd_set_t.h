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

#ifndef __FC_DEFINE_FD_SET_T
#define __FC_DEFINE_FD_SET_T
typedef struct {char __fc_fd_set;} fd_set;
//@ assigns *fdset \from *fdset, fd;
void FD_CLR(int fd, fd_set *fdset);
//@ assigns \nothing ;
int FD_ISSET(int fd, fd_set *fdset);
//@ assigns *fdset \from *fdset, fd;
void FD_SET(int fd, fd_set *fdset);
//@ assigns *fdset \from \nothing;
void FD_ZERO(fd_set *fdset);
#define FD_SETSIZE 255
#endif

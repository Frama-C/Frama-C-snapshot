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

#ifndef FC_UIO
#define FC_UIO

#include "../__fc_define_ssize_t.h"
#include "../__fc_define_size_t.h"
#include "../__fc_define_iovec.h"

/*@ requires \valid( &iov[0..iovcnt-1] );
// Value cannot yet interpret the precise assigns clause; we use the weaker one as a fallback.
//@ assigns { ((char *) iov[i].iov_base)[0..iov[i].iov_len - 1] | integer i; 0 <= i < iovcnt };
  @ assigns   ((char *) iov[0..iovcnt -1].iov_base)[0..];
 */
ssize_t readv(int fd, const struct iovec *iov, int iovcnt);
ssize_t writev(int fd, const struct iovec *iov, int iovcnt);

#endif

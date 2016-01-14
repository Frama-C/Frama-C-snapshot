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

#ifndef __FC_DEFINE_FILE
#define __FC_DEFINE_FILE
#include "features.h"
#include "__fc_define_stat.h"
#include "__fc_define_fpos_t.h"

__BEGIN_DECLS

struct __fc_FILE {
  unsigned int __fc_stdio_id;
  fpos_t       __fc_position;
  char         __fc_error;
  char         __fc_eof;
  int          __fc_flags; // O_RDONLY 1 | O_RDWR 2 | O_WRONLY 3 + more flags.
  struct stat* __fc_inode;
  unsigned char * __fc_real_data;
  int          __fc_real_data_max_size;
};
typedef struct __fc_FILE FILE;

__END_DECLS
#endif


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

#ifndef __FC_DEFINE_FILE
#define __FC_DEFINE_FILE

#include "__fc_define_stat.h"
struct __fc_FILE {
  unsigned int __fc_stdio_id;
  unsigned int 	__fc_maxsz;
  unsigned int 	__fc_writepos;
  unsigned int 	__fc_readpos;
  int 		__fc_is_a_socket;
  int		mode; // O_RDONLY 1 | O_RDWR 2 | O_WRONLY 3
  struct stat* 	__fc_inode;
};
typedef struct __fc_FILE FILE;

#endif


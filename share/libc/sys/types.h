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

#ifndef __FC_SYS_TYPES_H__
#define __FC_SYS_TYPES_H__
#include "../features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

#include "../__fc_machdep.h"
#include "../__fc_define_id_t.h"
#include "../__fc_define_pid_t.h"
#include "../__fc_define_size_t.h"
#include "../__fc_define_ssize_t.h"
#include "../__fc_define_uid_and_gid.h"
#include "../__fc_define_time_t.h"
#include "../__fc_define_suseconds_t.h"
#include "../__fc_define_ino_t.h"
#include "../__fc_define_blkcnt_t.h"
#include "../__fc_define_blksize_t.h"
#include "../__fc_define_dev_t.h"
#include "../__fc_define_mode_t.h"
#include "../__fc_define_nlink_t.h"
#include "../__fc_define_off_t.h"
#include "../__fc_define_pthread_types.h"
#include "../__fc_define_key_t.h"

#ifndef __u_char_defined
typedef unsigned long u_long;
typedef unsigned int u_int;
typedef unsigned short u_short;
typedef unsigned char u_char;
extern dev_t makedev(int maj, int min);
#define __u_char_defined
#endif

__END_DECLS
__POP_FC_STDLIB
#endif

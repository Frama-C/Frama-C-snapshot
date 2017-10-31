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

#ifndef FC_UN
#define FC_UN
#include "../features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS
#include "../__fc_define_sa_family_t.h"

struct sockaddr_un
  {
    sa_family_t sun_family;
    // Note: the length has been hard-coded to the value typically found in
    // Linux. Move it to the machdep to support other implementations.
    char sun_path[108];         /* Path name.  */
  };

__END_DECLS
__POP_FC_STDLIB
#endif

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

#ifndef __FC_DEFINE_PTHREAD_TYPES_T
#define __FC_DEFINE_PTHREAD_TYPES_T
#include "features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS
// These types are defined as structs with a meaningless field just to provide
// stronger typing constraints

#ifndef __have_pthread_attr_t
typedef struct { int _fc; } pthread_attr_t;
#define __have_pthread_attr_t
#endif

typedef struct { int _fc; } pthread_barrier_t;
typedef struct { int _fc; } pthread_barrierattr_t;
typedef struct { int _fc; } pthread_cond_t;
typedef struct { int _fc; } pthread_condattr_t;
typedef struct { int _fc; } pthread_key_t;
typedef struct { int _fc; } pthread_mutex_t;
typedef struct { int _fc; } pthread_mutexattr_t;
typedef struct { int _fc; } pthread_once_t;
typedef struct { int _fc; } pthread_rwlock_t;
typedef struct { int _fc; } pthread_rwlockattr_t;
typedef struct { int _fc; } pthread_spinlock_t;
typedef struct { int _fc; } pthread_t;
__END_DECLS
__POP_FC_STDLIB
#endif

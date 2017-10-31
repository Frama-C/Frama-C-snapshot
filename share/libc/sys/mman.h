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

#ifndef __FC_SYS_MMAN_H__
#define __FC_SYS_MMAN_H__

#include "../features.h"
#include "../__fc_define_mode_t.h"
#include "../__fc_define_off_t.h"
#include "../__fc_define_size_t.h"
__PUSH_FC_STDLIB

// The values for the constants below are based on an x86 Linux,
// declared in the order given by POSIX.1-2008.

#define PROT_EXEC  0x4
#define PROT_NONE  0x0
#define PROT_READ  0x1
#define PROT_WRITE 0x2

#define MAP_FIXED   0x10
#define MAP_PRIVATE 0x02
#define MAP_SHARED  0x01

#define MAP_FAILED ((void*) -1)

#define MS_ASYNC      1
#define MS_INVALIDATE 2
#define MS_SYNC       4

#define MCL_CURRENT 1
#define MCL_FUTURE  2

#define POSIX_MADV_DONTNEED   4
#define POSIX_MADV_NORMAL     0
#define POSIX_MADV_RANDOM     1
#define POSIX_MADV_SEQUENTIAL 2
#define POSIX_MADV_WILLNEED   3

// Not currently defined in any Linux header
//#define POSIX_TYPED_MEM_ALLOCATE
//#define POSIX_TYPED_MEM_ALLOCATE_CONTIG
//#define POSIX_TYPED_MEM_MAP_ALLOCATABLE
//
//struct posix_typed_mem_info {
//  size_t posix_tmi_length;
//}

extern int mlock (const void *addr, size_t len);
extern int mlockall (int __flags);

extern void *mmap(void *addr, size_t len, int prot, int flags,
                  int fildes, off_t off);

extern int mprotect (void *__addr, size_t __len, int __prot);
extern int msync (void *__addr, size_t __len, int __flags);
extern int munlock (const void *__addr, size_t __len);
extern int munlockall (void);
extern int munmap (void *__addr, size_t __len);
extern int posix_madvise (void *__addr, size_t __len, int __advice);

// Not currently defined in any Linux header
//int    posix_mem_offset(const void *restrict, size_t, off_t *restrict,
//                        size_t *restrict, int *restrict);
//int    posix_typed_mem_get_info(int, struct posix_typed_mem_info *);
//int    posix_typed_mem_open(const char *, int, int);

extern int shm_open (const char *__name, int __oflag, mode_t __mode);
extern int shm_unlink (const char *__name);

__POP_FC_STDLIB
#endif

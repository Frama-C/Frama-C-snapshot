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

/*! ***********************************************************************
 * \file  e_acsl_syscall.h
 * \brief E-ACSL \p syscall bindings.
 *
 * Re-declaration of standard libc syscall-based functions using direct
 * application of \p syscall. The aim is to avoid issues for the case when a
 * target program provides custom implementations or wrappers for such
 * functions. For instance, if an instrumented program provides a custom
 * implementation of `write` E-ACSL RTL will still use the native system
 * call.
 *
 * Note that this header following does not provide a re-declaration for \p
 * mmap.  This is because \p  syscall returns \p  int, while \p  mmap should
 * return an integer type wide enough to hold a memory address address. Also,
 * since there is no \p sbrk system call, a re-declaration of \p  sbrk is also
 * missing. As a result, programs providing custom definitions of \p syscall,
 * \p mmap or \p sbrk should be rejected. Re-definitions of the below functions
 * should be safe.
***************************************************************************/

#ifndef E_ACSL_SYSCALL
#define E_ACSL_SYSCALL
#  include <stdlib.h>
#  include <fcntl.h>
#  include <unistd.h>
#  include <sys/syscall.h>   /* SYS_xxx definitions */

int syscall(int number, ...);

#  define write(...) syscall(SYS_write, __VA_ARGS__)
#  define open(...) syscall(SYS_open, __VA_ARGS__)
#  define close(...) syscall(SYS_close, __VA_ARGS__)
#  define getrlimit(...) syscall(SYS_getrlimit, __VA_ARGS__)
#  define munmap(...) syscall(SYS_munmap, __VA_ARGS__)
#  define exit(...) syscall(SYS_exit, __VA_ARGS__)
#  define getpid(...) syscall(SYS_getpid)
#  define kill(...) syscall(SYS_kill, __VA_ARGS__)
#endif

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

#ifndef __FC_PTHREAD_H__
#define __FC_PTHREAD_H__

#include "features.h"
__PUSH_FC_STDLIB

// The values for the constants below are based on an x86 Linux,
// declared in the order given by POSIX.1-2008.

enum
{
  PTHREAD_CANCEL_ENABLE,
  PTHREAD_CANCEL_DISABLE
};

enum
{
  PTHREAD_CANCEL_DEFERRED,
  PTHREAD_CANCEL_ASYNCHRONOUS
};

enum
{
  PTHREAD_CREATE_JOINABLE,
  PTHREAD_CREATE_DETACHED
};

enum
{
  PTHREAD_INHERIT_SCHED,
  PTHREAD_EXPLICIT_SCHED
};


/* Scope handling.  */
enum
{
  PTHREAD_SCOPE_SYSTEM,
  PTHREAD_SCOPE_PROCESS
};

enum
{
  PTHREAD_MUTEX_NORMAL,
  PTHREAD_MUTEX_RECURSIVE,
  PTHREAD_MUTEX_ERRORCHECK,
  PTHREAD_MUTEX_DEFAULT
};

enum
{
  PTHREAD_MUTEX_STALLED,
  PTHREAD_MUTEX_ROBUST
};

enum
{
  PTHREAD_PRIO_NONE,
  PTHREAD_PRIO_INHERIT,
  PTHREAD_PRIO_PROTECT
};

enum
{
  PTHREAD_PROCESS_PRIVATE,
  PTHREAD_PROCESS_SHARED
};

#define PTHREAD_BARRIER_SERIAL_THREAD (-1)
#define PTHREAD_CANCEL_ASYNCHRONOUS PTHREAD_CANCEL_ASYNCHRONOUS
#define PTHREAD_CANCEL_ENABLE PTHREAD_CANCEL_ENABLE
#define PTHREAD_CANCEL_DEFERRED PTHREAD_CANCEL_DEFERRED
#define PTHREAD_CANCEL_DISABLE PTHREAD_CANCEL_DISABLE
#define PTHREAD_CANCELED ((void *) -1)
#define PTHREAD_CREATE_DETACHED PTHREAD_CREATE_DETACHED
#define PTHREAD_CREATE_JOINABLE PTHREAD_CREATE_JOINABLE

#define PTHREAD_EXPLICIT_SCHED PTHREAD_EXPLICIT_SCHED
#define PTHREAD_INHERIT_SCHED PTHREAD_INHERIT_SCHED

#define PTHREAD_MUTEX_DEFAULT PTHREAD_MUTEX_DEFAULT
#define PTHREAD_MUTEX_ERRORCHECK PTHREAD_MUTEX_ERRORCHECK
#define PTHREAD_MUTEX_NORMAL PTHREAD_MUTEX_NORMAL
#define PTHREAD_MUTEX_RECURSIVE PTHREAD_MUTEX_RECURSIVE
#define PTHREAD_MUTEX_ROBUST PTHREAD_MUTEX_ROBUST
#define PTHREAD_MUTEX_STALLED PTHREAD_MUTEX_STALLED
#define PTHREAD_ONCE_INIT 0

#define PTHREAD_PRIO_INHERIT PTHREAD_PRIO_INHERIT

#define PTHREAD_PRIO_NONE PTHREAD_PRIO_NONE

#define PTHREAD_PRIO_PROTECT PTHREAD_PRIO_PROTECT

#define PTHREAD_PROCESS_SHARED PTHREAD_PROCESS_SHARED
#define PTHREAD_PROCESS_PRIVATE PTHREAD_PROCESS_PRIVATE

#define PTHREAD_SCOPE_PROCESS   PTHREAD_SCOPE_PROCESS
#define PTHREAD_SCOPE_SYSTEM    PTHREAD_SCOPE_SYSTEM

#define PTHREAD_COND_INITIALIZER { 0 }
#define PTHREAD_MUTEX_INITIALIZER { 0 }
#define PTHREAD_RWLOCK_INITIALIZER { 0 }

// Include pthread-related types
#include "__fc_define_pthread_types.h"
#include "__fc_define_size_t.h"

extern int pthread_atfork(void (*)(void), void (*)(void), void(*)(void));
extern int pthread_attr_destroy(pthread_attr_t *);
extern int pthread_attr_getdetachstate(const pthread_attr_t *, int *);
extern int pthread_attr_getguardsize(const pthread_attr_t *restrict,
                                     size_t *restrict);
extern int pthread_attr_getinheritsched(const pthread_attr_t *restrict,
                                        int *restrict);
extern int pthread_attr_getschedparam(const pthread_attr_t *restrict,
                                      struct sched_param *restrict);
extern int pthread_attr_getschedpolicy(const pthread_attr_t *restrict,
                                       int *restrict);
extern int pthread_attr_getscope(const pthread_attr_t *restrict,
                                 int *restrict);
extern int pthread_attr_getstack(const pthread_attr_t *restrict,
                                 void **restrict, size_t *restrict);
extern int pthread_attr_getstacksize(const pthread_attr_t *restrict,
                                     size_t *restrict);
extern int pthread_attr_init(pthread_attr_t *);
extern int pthread_attr_setdetachstate(pthread_attr_t *, int);
extern int pthread_attr_setguardsize(pthread_attr_t *, size_t);
extern int pthread_attr_setinheritsched(pthread_attr_t *, int);
extern int pthread_attr_setschedparam(pthread_attr_t *restrict,
                                      const struct sched_param *restrict);
extern int pthread_attr_setschedpolicy(pthread_attr_t *, int);
extern int pthread_attr_setscope(pthread_attr_t *, int);
extern int pthread_attr_setstack(pthread_attr_t *, void *, size_t);
extern int pthread_attr_setstacksize(pthread_attr_t *, size_t);
extern int pthread_barrier_destroy(pthread_barrier_t *);
extern int pthread_barrier_init(pthread_barrier_t *restrict,
                                const pthread_barrierattr_t *restrict,
                                unsigned);
extern int pthread_barrier_wait(pthread_barrier_t *);
extern int pthread_barrierattr_destroy(pthread_barrierattr_t *);
extern int pthread_barrierattr_getpshared(const pthread_barrierattr_t *restrict,
                                          int *restrict);
extern int pthread_barrierattr_init(pthread_barrierattr_t *);
extern int pthread_barrierattr_setpshared(pthread_barrierattr_t *, int);
extern int pthread_cancel(pthread_t);
extern int pthread_cond_broadcast(pthread_cond_t *);
extern int pthread_cond_destroy(pthread_cond_t *);
extern int pthread_cond_init(pthread_cond_t *restrict,
                             const pthread_condattr_t *restrict);
extern int pthread_cond_signal(pthread_cond_t *);
extern int pthread_cond_timedwait(pthread_cond_t *restrict,
                                  pthread_mutex_t *restrict,
                                  const struct timespec *restrict);
extern int pthread_cond_wait(pthread_cond_t *restrict,
                             pthread_mutex_t *restrict);
extern int pthread_condattr_destroy(pthread_condattr_t *);
//clockid_t not available yet
//extern int pthread_condattr_getclock(const pthread_condattr_t *restrict,
//                                     clockid_t *restrict);
extern int pthread_condattr_getpshared(const pthread_condattr_t *restrict,
                                       int *restrict);
extern int pthread_condattr_init(pthread_condattr_t *);
//clockid_t not available yet
//extern int pthread_condattr_setclock(pthread_condattr_t *, clockid_t);
extern int pthread_condattr_setpshared(pthread_condattr_t *, int);
extern int pthread_create(pthread_t *restrict, const pthread_attr_t *restrict,
                          void *(*)(void*), void *restrict);
extern int pthread_detach(pthread_t);
extern int pthread_equal(pthread_t, pthread_t);
extern void pthread_exit(void *);
extern int pthread_getconcurrency(void);
//clockid_t not available yet
//extern int pthread_getcpuclockid(pthread_t, clockid_t *);
extern int pthread_getschedparam(pthread_t, int *restrict,
                                 struct sched_param *restrict);
extern void *pthread_getspecific(pthread_key_t);
extern int pthread_join(pthread_t, void **);
extern int pthread_key_create(pthread_key_t *, void (*)(void*));
extern int pthread_key_delete(pthread_key_t);
extern int pthread_mutex_consistent(pthread_mutex_t *);
extern int pthread_mutex_destroy(pthread_mutex_t *);
extern int pthread_mutex_getprioceiling(const pthread_mutex_t *restrict,
                                        int *restrict);
extern int pthread_mutex_init(pthread_mutex_t *restrict,
                              const pthread_mutexattr_t *restrict);
extern int pthread_mutex_lock(pthread_mutex_t *);
extern int pthread_mutex_setprioceiling(pthread_mutex_t *restrict, int,
                                        int *restrict);
extern int pthread_mutex_timedlock(pthread_mutex_t *restrict,
                                   const struct timespec *restrict);
extern int pthread_mutex_trylock(pthread_mutex_t *);
extern int pthread_mutex_unlock(pthread_mutex_t *);
extern int pthread_mutexattr_destroy(pthread_mutexattr_t *);
extern int pthread_mutexattr_getprioceiling(const pthread_mutexattr_t *restrict,
                                            int *restrict);
extern int pthread_mutexattr_getprotocol(const pthread_mutexattr_t *restrict,
                                         int *restrict);
extern int pthread_mutexattr_getpshared(const pthread_mutexattr_t *restrict,
                                        int *restrict);
extern int pthread_mutexattr_getrobust(const pthread_mutexattr_t *restrict,
                                       int *restrict);
extern int pthread_mutexattr_gettype(const pthread_mutexattr_t *restrict,
                                     int *restrict);
extern int pthread_mutexattr_init(pthread_mutexattr_t *);
extern int pthread_mutexattr_setprioceiling(pthread_mutexattr_t *, int);
extern int pthread_mutexattr_setprotocol(pthread_mutexattr_t *, int);
extern int pthread_mutexattr_setpshared(pthread_mutexattr_t *, int);
extern int pthread_mutexattr_setrobust(pthread_mutexattr_t *, int);
extern int pthread_mutexattr_settype(pthread_mutexattr_t *, int);
extern int pthread_once(pthread_once_t *, void (*)(void));
extern int pthread_rwlock_destroy(pthread_rwlock_t *);
extern int pthread_rwlock_init(pthread_rwlock_t *restrict,
                               const pthread_rwlockattr_t *restrict);
extern int pthread_rwlock_rdlock(pthread_rwlock_t *);
extern int pthread_rwlock_timedrdlock(pthread_rwlock_t *restrict,
                                      const struct timespec *restrict);
extern int pthread_rwlock_timedwrlock(pthread_rwlock_t *restrict,
                                      const struct timespec *restrict);
extern int pthread_rwlock_tryrdlock(pthread_rwlock_t *);
extern int pthread_rwlock_trywrlock(pthread_rwlock_t *);
extern int pthread_rwlock_unlock(pthread_rwlock_t *);
extern int pthread_rwlock_wrlock(pthread_rwlock_t *);
extern int pthread_rwlockattr_destroy(pthread_rwlockattr_t *);
extern int pthread_rwlockattr_getpshared(const pthread_rwlockattr_t *restrict,
                                         int *restrict);
extern int pthread_rwlockattr_init(pthread_rwlockattr_t *);
extern int pthread_rwlockattr_setpshared(pthread_rwlockattr_t *, int);
extern pthread_t pthread_self(void);
extern int pthread_setcancelstate(int, int *);
extern int pthread_setcanceltype(int, int *);
extern int pthread_setconcurrency(int);
extern int pthread_setschedparam(pthread_t, int, const struct sched_param *);
extern int pthread_setschedprio(pthread_t, int);
extern int pthread_setspecific(pthread_key_t, const void *);
extern int pthread_spin_destroy(pthread_spinlock_t *);
extern int pthread_spin_init(pthread_spinlock_t *, int);
extern int pthread_spin_lock(pthread_spinlock_t *);
extern int pthread_spin_trylock(pthread_spinlock_t *);
extern int pthread_spin_unlock(pthread_spinlock_t *);
extern void pthread_testcancel(void);

// From POSIX: "Inclusion of the <pthread.h> header shall make symbols defined
//              in the headers <sched.h> and <time.h> visible."
#include <sched.h>
#include <time.h>

__POP_FC_STDLIB
#endif

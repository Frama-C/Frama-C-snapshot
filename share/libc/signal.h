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

#ifndef __FC_SIGNAL
#define __FC_SIGNAL

/* ISO C: 7.14 */

#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_pid_t.h"
#include "__fc_define_uid_and_gid.h"
#include "__fc_define_pthread_types.h"

__BEGIN_DECLS

/* TODO: put sig_atomic_t in machdep */
#ifndef __sig_atomic_t_defined
typedef volatile int sig_atomic_t;
#define __sig_atomic_t_defined
#endif

typedef void (*__fc_sighandler_t) (int);

#define sighandler_t __fc_sighandler_t

/* for BSD 4.4 */
#ifdef __USE_MISC
typedef __fc_sighandler_t sig_t;
#endif

#define SIG_DFL ((__fc_sighandler_t)0)     /* default signal handling */
#define SIG_IGN ((__fc_sighandler_t)1)     /* ignore signal */
#define SIG_ERR ((__fc_sighandler_t)-1)    /* error return from signal */

#define SIG_BLOCK 0
#define SIG_UNBLOCK 1
#define SIG_SETMASK 2



#define SIGHUP		 1
#define SIGINT		 2
#define SIGQUIT		 3
#define SIGILL		 4
#define SIGTRAP		 5
#define SIGABRT		 6
#define SIGIOT		 6
#define SIGBUS		 7
#define SIGFPE		 8
#define SIGKILL		 9
#define SIGUSR1		10
#define SIGSEGV		11
#define SIGUSR2		12
#define SIGPIPE		13
#define SIGALRM		14
#define SIGTERM		15
#define SIGSTKFLT	16
#define SIGCHLD		17
#define SIGCONT		18
#define SIGSTOP		19
#define SIGTSTP		20
#define SIGTTIN		21
#define SIGTTOU		22
#define SIGURG		23
#define SIGXCPU		24
#define SIGXFSZ		25
#define SIGVTALRM	26
#define SIGPROF		27
#define SIGWINCH	28
#define SIGIO		29
#define SIGPOLL		SIGIO
/*
#define SIGLOST		29
*/
#define SIGPWR		30
#define SIGSYS		31
#define	SIGUNUSED	31



#define SA_NOCLDSTOP	0x00000001
#define SA_NOCLDWAIT	0x00000002
#define SA_SIGINFO	0x00000004
#define SA_ONSTACK	0x08000000
#define SA_RESTART	0x10000000
#define SA_NODEFER	0x40000000
#define SA_RESETHAND	0x80000000

#define SA_NOMASK	SA_NODEFER
#define SA_ONESHOT	SA_RESETHAND

/*@ assigns \nothing; */
extern void (*signal(int sig, void (*func)(int)))(int);

/*@ 
  assigns \nothing;
  ensures never_terminates: \false; */
extern int raise(int sig);
#include "__fc_define_sigset_t.h"

union sigval {
	int sival_int;
	void *sival_ptr;
};

struct sigevent {
  int sigev_notify;
  int sigev_signo;
  union sigval sigev_value;
  void (*sigev_notify_function) (union sigval);
  pthread_attr_t *sigev_notify_attributes;
};

#ifndef __have_siginfo_t
#define __have_siginfo_t
typedef struct {
	int si_signo;
	int si_code;
	union sigval si_value;
	int si_errno;
	pid_t si_pid;
	uid_t si_uid;
	void *si_addr;
	int si_status;
	int si_band;
} siginfo_t;
#endif

struct sigaction {
               void     (*sa_handler)(int);
               void     (*sa_sigaction)(int, siginfo_t *, void *);
               sigset_t   sa_mask;
               int        sa_flags;
           };

/*@
  requires valid_set: \valid(set);
  assigns *set \from \nothing;
  assigns \result \from \nothing;
  ensures initialization:set: \initialized(set);
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int sigemptyset(sigset_t *set);

/*@
  requires valid_set: \valid(set);
  assigns *set \from \nothing;
  assigns \result \from \nothing;
  ensures initialization:set: \initialized(set);
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int sigfillset(sigset_t *set);

/*@
  requires valid_set: \valid(set);
  requires initialization:set: \initialized(set);
  assigns *set \from indirect:signum;
  assigns \result \from signum;
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int sigaddset(sigset_t *set, int signum);

/*@
  requires valid_set: \valid(set);
  requires initialization:set: \initialized(set);
  assigns *set \from indirect:signum;
  assigns \result \from signum;
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int sigdelset(sigset_t *set, int signum);

/*@
  requires valid_read_set: \valid_read(set);
  requires initialization:set: \initialized(set);
  assigns \result \from *set, signum;
  ensures result_found_not_found_or_error: \result == 0 || \result == 1
    || \result == -1;
*/
extern int sigismember(const sigset_t *set, int signum);

extern int sigaction(int signum, const struct sigaction *act,
                     struct sigaction *oldact);


/*@ // missing: assigns *oldset \from 'previous mask in process'
  requires valid_set_or_null: set == \null || \valid_read(set);
  requires valid_how: set != \null ==>
                      how \in {SIG_BLOCK, SIG_SETMASK, SIG_UNBLOCK};
  requires valid_oldset_or_null: oldset == \null || \valid(oldset);
  requires separation: (set == oldset == \null) ||
                       \separated(set, oldset);
  assigns \result \from indirect:how, indirect:set, indirect:oldset;
  assigns oldset == \null ? \empty : *oldset
          \from indirect:how, indirect:oldset;
  ensures result_ok_or_error: \result == 0 || \result == -1;
  ensures initialization:oldset_initialized:
    oldset != \null && \result == 0 ==> \initialized(oldset);
*/
extern int sigprocmask(int how, const sigset_t * restrict set,
                       sigset_t *restrict oldset);

/*@ // missing: errno may be set to EINVAL, EPERM, ESRCH
    // missing: assigns 'other processes' \from 'other processes'
  assigns \result \from indirect:pid, indirect: sig;
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int kill(pid_t pid, int sig);

__END_DECLS

__POP_FC_STDLIB
#endif

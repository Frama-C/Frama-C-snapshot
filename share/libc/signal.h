/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2012                                               */
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

#include "__fc_define_pid_t.h"
#include "__fc_define_uid_and_gid.h"

typedef volatile int sig_atomic_t;
typedef void (*__fc_sighandler_t) (int);


#define SIG_DFL ((__fc_sighandler_t)0)     /* default signal handling */
#define SIG_IGN ((__fc_sighandler_t)1)     /* ignore signal */
#define SIG_ERR ((__fc_sighandler_t)-1)    /* error return from signal */

#define SIG_BLOCK 0
#define SIG_UNBLOCK 1
#define SIG_SETMASK 2

#define SIGABRT 4
#define SIGFPE 5
#define SIGILL 6
#define SIGINT 7
#define SIGSEGV 8
#define SIGTERM 9

/* POSIX signals */
#define SIGHUP 10
#define SIGCHLD 11
#define SIGPIPE 12


/*@ assigns \nothing; */
void (*signal(int sig, void (*func)(int)))(int);

/*@ 
  assigns \nothing;
  ensures \false; */
int raise(int sig);
#include "__fc_define_sigset_t.h"

union sigval {
	int sival_int;
	void *sival_ptr;
};
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

struct sigaction {
               void     (*sa_handler)(int);
               void     (*sa_sigaction)(int, siginfo_t *, void *);
               sigset_t   sa_mask;
               int        sa_flags;
           };

int sigemptyset(sigset_t *set);
int sigfillset(sigset_t *set);
int sigaddset(sigset_t *set, int signum);
int sigdelset(sigset_t *set, int signum);
int sigismember(const sigset_t *set, int signum);
int sigaction(int signum, const struct sigaction *act,
                     struct sigaction *oldact);
int sigprocmask(int how, const sigset_t *set, sigset_t *oldset);

int kill(pid_t pid, int sig);

#endif

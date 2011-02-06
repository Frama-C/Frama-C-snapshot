/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2011                                               */
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

/* ISO C: 7.14 */

typedef volatile int sig_atomic_t;

/* TODO: choose more probable libc values */
#define SIG_DFL 1
#define SIG_ERR 2
#define SIG_IGN 3

#define SIGABRT 4
#define SIGFPE 5
#define SIGILL 6
#define SIGINT 7
#define SIGSEGV 8
#define SIGTERM 9

/* POSIX signals */
#define SIGHUP 10
#define SIGCHLD 11

/*@ assigns \nothing; */
void (*signal(int sig, void (*func)(int)))(int);

/*@ 
  assigns \nothing;
  ensures \false; */
int raise(int sig);






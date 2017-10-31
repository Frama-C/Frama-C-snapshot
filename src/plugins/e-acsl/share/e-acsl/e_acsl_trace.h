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
 * \file  e_acsl_trace.h
 * \brief Interface for producing backtrace. Requires GLIBC.
***************************************************************************/

#ifndef E_ACSL_TRACE_H
#define E_ACSL_TRACE_H

#include <stddef.h>
#include <limits.h>
#include "e_acsl_printf.h"
#include "e_acsl_shexec.h"

extern void  *__libc_stack_end;

struct frame_layout {
  void *next;
  void *return_address;
};

/* The following implementation of malloc-free backtrace [native_backtrace]
   is mostly taken from Glibc-2.22 (see file debug/backtrace.c) */
static int native_backtrace (void **array, int size) {
  struct frame_layout *current;
  void *top_frame,
       *top_stack;
  int cnt = 0;

  top_frame = __builtin_frame_address(0);
  /* Some notion of current stack.  Need not be exactly the top of the stack,
     just something somewhere in the current frame.  */
  top_stack = ({ char __csf; &__csf; });

  /* We skip the call to this function, it makes no sense to record it.  */
  current = ((struct frame_layout *) top_frame);
  while (cnt < size) {
    /* Assume that the stack grows downwards  */
    if ((void *) current < top_stack || !((void *) current < __libc_stack_end))
      /* This means the address is out of range.  Note that for the
        toplevel we see a frame pointer with value NULL which clearly is
        out of range.  */
      break;
    array[cnt++] = current->return_address;
    current = ((struct frame_layout *) (current->next));
  }
  return cnt;
}

static void trace() {
# ifdef __linux__

  int size = 24;
  void **bb = private_malloc(sizeof(void*)*size);
  native_backtrace(bb, size);

  char executable [PATH_MAX];
  rtl_sprintf(executable, "/proc/%d/exe", getpid());

  STDOUT("/** Backtrace **************************/\n");
  int counter = 0;
  while (*bb) {
    char *addr = (char*)private_malloc(21);
    rtl_sprintf(addr,"%p", *bb);
    char *ar[] = { "addr2line", "-f", "-p", "-C", "-s", "-e",
      executable, addr, NULL};
    ipr_t *ipr = shexec(ar, NULL);
    char *prefix = (counter) ? " - " : "";
    if (ipr) {
      char *outs = (char*)ipr->stdouts;
      if (outs) {
        outs[strlen(outs)-1] = '\0';
        if (strlen(outs) && endswith(outs, "??:0") && endswith(outs, "??:?")) {
          STDOUT("%s%s\n", prefix, outs);
        }
      } else {
        char *errs = (char*)ipr->stderrs;
        if (errs) {
          STDOUT("%s\n", errs);
        }
      }
    }
    bb++;
    counter++;
  }
  STDOUT("/***************************************/\n");
# endif /* __linux__ */
}
#endif

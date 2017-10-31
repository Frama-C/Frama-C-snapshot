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
 * \file  e_acsl_malloc.h
 *
 * \brief E-ACSL memory allocation bindings.
***************************************************************************/

/* Declaration of memory locations considered safe before a program starts.
 * Most of these should be declared somewhere in start procedures of c
 * and gcc libraries. One example of a safe location is errno. */

#ifndef E_ACSL_SAFE_LOCATIONS_H
#define E_ACSL_SAFE_LOCATIONS_H

#include <stdio.h>
#include <stdint.h>
#include <errno.h>

/* Simple representation of a safe location */
struct memory_location {
  uintptr_t address; /* Address */
  uintptr_t length; /* Byte-length */
  int is_initialized; /* Notion of initialization */
};

typedef struct memory_location memory_location;

/* An array storing safe locations up to `safe_location_counter` position.
 * This array should be initialized via a below function called
 * `collect_safe_locations`. */
static memory_location safe_locations [16];
static int safe_location_counter = 0;

#define add_safe_location(_addr,_len,_init) { \
  safe_locations[safe_location_counter].address = _addr; \
  safe_locations[safe_location_counter].length = _len; \
  safe_location_counter++; \
}

#ifdef errno
#undef errno
extern __thread int errno;
#endif

extern FILE *stdin;		  /* Standard input stream.  */
extern FILE *stdout;		/* Standard output stream.  */
extern FILE *stderr;		/* Standard error output stream. */

static void collect_safe_locations() {
  /* Tracking of errno and standard streams */
  add_safe_location((uintptr_t)&errno, sizeof(int), "errno");
  add_safe_location((uintptr_t)stdout, sizeof(FILE), "stdout");
  add_safe_location((uintptr_t)stderr, sizeof(FILE), "stderr");
  add_safe_location((uintptr_t)stdin, sizeof(FILE), "stdin");
}
#endif

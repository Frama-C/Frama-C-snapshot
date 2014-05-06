/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2014                                               */
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

#ifndef __FC_GLOB_H
#define __FC_GLOB_H

#include "__fc_machdep.h"

#define	GLOB_ERR	(1 << 0)/* Return on read errors.  */
#define	GLOB_MARK	(1 << 1)/* Append a slash to each name.  */
#define	GLOB_NOSORT	(1 << 2)/* Don't sort the names.  */
#define	GLOB_DOOFFS	(1 << 3)/* Insert PGLOB->gl_offs NULLs.  */
#define	GLOB_NOCHECK	(1 << 4)/* If nothing matches, return the pattern.  */
#define	GLOB_APPEND	(1 << 5)/* Append to results of a previous call.  */
#define	GLOB_NOESCAPE	(1 << 6)/* Backslashes don't quote metacharacters.  */
#define	GLOB_PERIOD	(1 << 7)/* Leading `.' can be matched by metachars.  */

#define	GLOB_NOSPACE	1	/* Ran out of memory.  */
#define	GLOB_ABORTED	2	/* Read error.  */
#define	GLOB_NOMATCH	3	/* No matches found.  */
#define GLOB_NOSYS	4	/* Not implemented.  */

typedef struct {
  __SIZE_T gl_pathc;		/* Count of paths matched by the pattern.  */
  char **gl_pathv;		/* List of matched pathnames.  */
  __SIZE_T gl_offs;		/* Slots to reserve in `gl_pathv'.  */
  int gl_flags;		/* Set to FLAGS, maybe | GLOB_MAGCHAR.  */
  
  /* If the GLOB_ALTDIRFUNC flag is set, the following functions
     are used instead of the normal file access functions.  */
  void (*gl_closedir) (void *);
#ifdef __USE_GNU
  struct dirent *(*gl_readdir) (void *);
#else
  void *(*gl_readdir) (void *);
#endif
  void *(*gl_opendir) (__const char *);
#ifdef __USE_GNU
  int (*gl_lstat) (__const char *__restrict, struct stat *__restrict);
  int (*gl_stat) (__const char *__restrict, struct stat *__restrict);
#else
  int (*gl_lstat) (__const char *__restrict, void *__restrict);
  int (*gl_stat) (__const char *__restrict, void *__restrict);
#endif
} glob_t;


#endif

/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2019                                               */
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

#include "glob.h"
#include "stdlib.h"
#include "__fc_builtin.h"
__PUSH_FC_STDLIB

int glob(const char *pattern, int flags,
         int (*errfunc) (const char *epath, int eerrno),
         glob_t *pglob) {
  // arbitrarily simulate finding a number of results between 0 and 10
  pglob->gl_pathc = Frama_C_interval(0, 10);
  // reserve_offs is 0 unless GLOB_DOOFFS is set; it is the number of
  // offsets to skip
  size_t reserve_offs = flags & GLOB_DOOFFS ? pglob->gl_offs : 0;
  // prev_len is 0 unless GLOB_APPEND is set; it is the length
  // of the previous call to glob()
  size_t prev_len = 0;
  // count previous list size, used for new allocation
  if (flags & GLOB_APPEND)
    while (pglob->gl_pathv[reserve_offs+prev_len]) prev_len++;

  // path points to pglob->gl_pathv if GLOB_APPEND, or NULL otherwise
  char **path = flags & GLOB_APPEND ? pglob->gl_pathv : NULL;
  if (pglob->gl_pathc == 0) { // no results found
    if (flags & GLOB_NOCHECK) {
      // allocate 1 slot per reserved offset, + previous length,
      // +1 for the pattern itself (to be recopied), +1 for the terminator
      pglob->gl_pathv =
        realloc(path,
                (reserve_offs + prev_len + 2) * sizeof(char*));
      if (!pglob->gl_pathv) return GLOB_NOSPACE;
      // 0-init reserved offsets
      for (size_t i = 0; i < reserve_offs; i++) pglob->gl_pathv[i] = 0;
      pglob->gl_pathv[reserve_offs + prev_len] = (char*)pattern;
      pglob->gl_pathv[reserve_offs + prev_len + 1] = 0; // terminator
      return 0;
    } else {
      return GLOB_NOMATCH;
    }
  }
  // found some results
  pglob->gl_pathv =
    // allocate 1 slot per reserved offset, + previous length,
    // +1 for each result, +1 for the terminator
    realloc(path,
            (reserve_offs + prev_len + pglob->gl_pathc + 1) * sizeof(char*));
  if (!pglob->gl_pathv) return GLOB_NOSPACE;

  // 0-init reserved offsets
  for (size_t i = 0; i < reserve_offs; i++) pglob->gl_pathv[i] = 0;

  for (size_t i = 0; i < pglob->gl_pathc; i++) {
    pglob->gl_pathv[reserve_offs + prev_len + i] = "glob result";
  }
  pglob->gl_pathv[prev_len + reserve_offs + pglob->gl_pathc] = 0; // terminator
  if (Frama_C_nondet(0, 1)) { // simulate "no error"
    return 0;
  } else {
    // simulate error during operation
    if (errfunc) {
      int res = errfunc("glob.c error path", Frama_C_interval(0, 255));
      if (res || flags & GLOB_ERR) return GLOB_ABORTED;
    }
    return 0;
  }
}

void globfree(glob_t *pglob) {
  // check for gl_pathc, because:
  // "if gl_pathc is zero, the contents of gl_pathv are undefined"
  if (pglob->gl_pathc > 0)
    free(pglob->gl_pathv);
}

__POP_FC_STDLIB

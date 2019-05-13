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

#include "stdio.h"
#include "stdlib.h"
#include "stdint.h" // for SIZE_MAX
#include "sys/types.h" // for ssize_t
#include "errno.h"
__PUSH_FC_STDLIB

FILE __fc_initial_stdout = {.__fc_FILE_id=1}; 
FILE * __fc_stdout = &__fc_initial_stdout;

FILE __fc_initial_stderr = {.__fc_FILE_id=2}; 
FILE * __fc_stderr = &__fc_initial_stderr;

FILE __fc_initial_stdin = {.__fc_FILE_id=0}; 
FILE * __fc_stdin = &__fc_initial_stdin;

// inefficient but POSIX-conforming implementation of getline
ssize_t getline(char **lineptr, size_t *n, FILE *stream) {
  if (!lineptr || !n || !stream) {
    errno = EINVAL;
    //TODO: set error indicator for stream
    return -1;
  }
  if (ferror(stream) || feof(stream)) {
    //TODO: set error indicator for stream
    return -1;
  }
  if (!*lineptr || *n == 0) {
    *lineptr = malloc(2);
    if (!lineptr) {
      errno = ENOMEM;
      //TODO: set error indicator for stream
      return -1;
    }
    *n = 2;
  }
  size_t cur = 0;
  while (!ferror(stream) && !feof(stream)) {
    while (cur < *n-1) {
      char c = fgetc(stream);
      if (c == EOF && cur == 0) {
        // no characters were read
        //TODO: set error indicator for stream
        return -1;
      }
      if (c != EOF) (*lineptr)[cur++] = c;
      if (c == '\n' || c == EOF) {
        // finished reading a line or the file
        (*lineptr)[cur] = '\0';
        return cur;
      }
    }
    // try to realloc larger buffer
    if (*n == SSIZE_MAX) {
      errno = EOVERFLOW;
      //TODO: set error indicator for stream
      return -1;
    }
    size_t new_size = *n+1;
    *lineptr = realloc(*lineptr, new_size);
    if (!*lineptr) {
      // failed to realloc larger line
      errno = ENOMEM;
      //TODO: set error indicator for stream
      return -1;
    }
    *n = new_size;
  }
  //TODO: set error indicator for stream
  return -1;
}

__POP_FC_STDLIB

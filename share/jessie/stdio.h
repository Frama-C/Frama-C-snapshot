/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2008                                               */
/*    INRIA (Institut National de Recherche en Informatique et en         */
/*           Automatique)                                                 */
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
/**************************************************************************/

/* $Id: stdio.h,v 1.5 2008/11/24 10:29:18 uid570 Exp $ */

#ifndef _STDIO_H_
#define _STDIO_H_

#ifndef EOF
# define EOF (-1)
#endif

typedef void* FILE;

extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;

/*@ requires valid_string(path) && valid_string(mode);
  @ assigns \nothing;
  @ ensures \result == NULL || \valid(\result);
  @*/
extern FILE *fopen(const char *path, const char *mode);

/*@ requires \valid(stream);
  @ assigns \nothing;
  @*/
extern int getc(FILE *stream);

/*@ requires \valid(stream);
  @ assigns \nothing;
  @*/
extern int fgetc(FILE *stream);

/*@ requires \valid_range(s,0,size-1) && \valid(stream);
  @ assigns s[0..size-1];
  @ ensures \result == NULL 
  @      || (\result == s && valid_string(s) && strlen(s) <= size-1);
  @*/
extern char *fgets(char *s, int size, FILE *stream);

/*@ requires \valid(fp);
  @ assigns \nothing;
  @*/
extern int fclose(FILE *fp);

#define printf(...)

/*@ requires \valid(stream) && buf == NULL;
  @ assigns \nothing;
  @*/
extern void setbuf(FILE *stream, char *buf);

/*@ requires valid_string(s);
  @ assigns \nothing;
  @*/
extern void perror(const char *s);

#endif /* _STDIO_H_ */

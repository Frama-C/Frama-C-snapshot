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

/* $Id: unistd.h,v 1.4 2008/11/24 10:29:18 uid570 Exp $ */

#ifndef _UNISTD_H_
#define _UNISTD_H_

extern char *FRAMA_C_STRING_OR_NULL optarg;
extern int optind, opterr, optopt;

/*@ assigns optarg, optind, opterr, optopt;
  @ ensures \result == -1 || valid_string(optarg);
  @*/
extern int getopt (int argc, char *FRAMA_C_STRING const argv[],           
		   const char *FRAMA_C_STRING_OR_NULL optstring);

/*@ assigns \nothing;
  @ ensures -1 <= \result <= 0;
  @*/
extern int chdir(const char *FRAMA_C_STRING path);

/*@ requires \valid_range(buf,0,size-1);
  @ assigns buf[0..size-1];
  @ ensures \result == NULL || \result == buf;
  @*/
extern char *getcwd(char *buf, size_t size);

#endif /* _UNISTD_H_ */

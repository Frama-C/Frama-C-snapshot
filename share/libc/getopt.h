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

#ifndef __FC_GETOPT_H
#define __FC_GETOPT_H

extern char *optarg;
extern int optind, opterr, optopt;

/*@ 
  assigns \result, *optarg, optind, opterr, optopt 
             \from argc, argv[0..argc-1], optstring[0..];
 */
extern int getopt(int argc, char * const argv[], const char *optstring);

/* GNU specific */
struct option
{
  const char *name;
  int has_arg;
  int *flag;
  int val;
};

# define no_argument		0
# define required_argument	1
# define optional_argument	2


/*@ 
  assigns \result, *optarg, optind, opterr, optopt, *(longopts[0..].flag)
             \from argc, argv[0..argc-1], shortopts[0..], longopts[0..];
 */
extern int getopt_long (int argc, char *const argv[],
			const char *shortopts,
			const struct option *longopts, int *longind);

/*@ 
  assigns \result, *optarg, optind, opterr, optopt, *(longopts[0..].flag)
             \from argc, argv[0..argc-1], shortopts[0..], longopts[0..];
 */
extern int getopt_long_only (int argc, char *const argv[],
			     const char *shortopts,
			     const struct option *longopts, int *longind);
#endif

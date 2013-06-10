/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2013                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  All rights reserved.                                                  */
/*  Contact CEA LIST for licensing.                                       */
/*                                                                        */
/**************************************************************************/

#include "stdio.h"

FILE __fc_initial_stdout = {.__fc_stdio_id=1}; 
FILE * __fc_stdout = &__fc_initial_stdout;

FILE __fc_initial_stderr = {.__fc_stdio_id=2}; 
FILE * __fc_stderr = &__fc_initial_stderr;

FILE __fc_initial_stdin = {.__fc_stdio_id=3}; 
FILE * __fc_stdin = &__fc_initial_stdin;

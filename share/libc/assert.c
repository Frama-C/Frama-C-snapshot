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

#include "__fc_builtin.h"
#include "assert.h"

__attribute__ ((__noreturn__)) void __FC_abort (void) {
  Frama_C_abort ();
}

void __FC_assert(const char* file,int line,const char*expr) {
  Frama_C_show_each_warning("Assertion may fail",file,line,expr);
  Frama_C_abort ();
}

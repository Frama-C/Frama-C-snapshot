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

#include "inttypes.h"

intmax_t imaxabs(intmax_t c) {
  if (c>0) return c; 
  else return (-c);
}

imaxdiv_t imaxdiv(intmax_t numer, intmax_t denom){
  imaxdiv_t r;
  r.quot=numer/denom;
  r.rem=numer%denom;
  return r;
};


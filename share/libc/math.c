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

/* ISO C: 7.12 */

#include "math.h"

double fabs(double x){
  if(x==0.0) return 0.0;
  if (x>0.0) return x;
  return -x;
}

float fabsf(float x)
{
  if (x == 0.0f) {
    return 0.0f;
  } else if (x > 0.0f) {
    return x;
  } else {
    return -x;
  }
}

int __finitef(float f)
{
  union { float f ; unsigned short w[2] ; } u ;
  unsigned short usExp ;

  u.f = f ;            /* Initilize for word access */
  usExp = (u.w[1] & 0x7F80) ;   /* Isolate the exponent */
  usExp >>= 7 ;                 /* Right align */

  /* A floating point value is invalid, if the exponent is 0xff */
  return !(usExp == 0xff) ;
}

int __finite(double d)
{
  union { double d ; unsigned short w[4] ; } u ;
  unsigned short usExp ;

  u.d = d ;            /* Initilize for word access */
  usExp = (u.w[3] & 0x7F80) ;   /* Isolate the exponent */
  usExp >>= 7 ;                 /* Right align */

  /* A floating point value is invalid, if the exponent is 0xff */
  return !(usExp == 0xff) ;
}

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

/*@ assigns \result \from x; */
extern double Frama_C_exp(double x);
/*@ assigns \result \from x; */
double exp(double x){
  return Frama_C_exp(x);
}

/*@ assigns \result \from x; */
extern double Frama_C_cos(double x);
/*@ assigns \result \from x; */
double cos(double x){
  return Frama_C_cos(x);
}

/*@ assigns \result \from x; */
extern double Frama_C_sin(double x);
/*@ assigns \result \from x; */
double sin(double x){
  return Frama_C_sin(x);
}

/*@ assigns \result \from x; */
extern double Frama_C_sqrt(double x);
/*@ assigns \result \from x; */
double sqrt(double x)
{
  return Frama_C_sqrt(x);
}

double fabs(double x){
  if(x==0.0) return 0.0;
  if (x>0.0) return x;
  return -x;
}


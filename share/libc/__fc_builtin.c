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

#include "__fc_builtin.h"

/* Those builtins implementations could probably be removed entirely for
   Value, as the spec is informative enough. There remains a slight difference
   with Frama_C_float/double_interval and +0./-0., because the specification
   is not sufficient to to exclude -0. when requiring >= +0. */

int volatile Frama_C_entropy_source;

//@ assigns Frama_C_entropy_source \from Frama_C_entropy_source;
extern void Frama_C_update_entropy(void);

int Frama_C_nondet(int a, int b)
{
  Frama_C_update_entropy();
  return Frama_C_entropy_source ? a : b;
}

void *Frama_C_nondet_ptr(void *a, void *b)
{
  return (void*) Frama_C_nondet((int)a, (int)b);
}

int Frama_C_interval(int min, int max)
{
  int r,aux;
  Frama_C_update_entropy();
  aux = Frama_C_entropy_source;
  if ((aux>=min) && (aux <=max))
    r = aux;
  else
    r = min;
  return r;
}

float Frama_C_float_interval(float min, float max)
{
  Frama_C_update_entropy();
  return Frama_C_entropy_source ? min : max;
}

double Frama_C_double_interval(double min, double max)
{
  Frama_C_update_entropy();
  return Frama_C_entropy_source ? min : max;
}

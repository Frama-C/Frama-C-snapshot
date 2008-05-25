/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2008                                               */
/*    CEA (Commissariat à l'Énergie Atomique)                             */
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

static volatile int Frama_C_entropy_source;

int Frama_C_nondet(int a, int b)
{
  return Frama_C_entropy_source ? a : b;
}

void *Frama_C_nondet_ptr(void *a, void *b)
{
  return (void*) Frama_C_nondet((int)a, (int)b);
}

int Frama_C_interval(int min, int max)
{
  int r,aux;
  aux = Frama_C_entropy_source;
  if ((aux>=min) && (aux <=max))
    r = aux;
  else
    r = min;
  return r;
}

float Frama_C_float_interval(float min, float max)
{
  return Frama_C_entropy_source ? min : max;
}

#if 0
static int ex1, ex2;
static int *ex3;
static float f;

void Frama_C_builtin_examples(void)
{
  /* choix non-déterministe entre deux entiers */
  ex1 = Frama_C_nondet(17, 42);

  /* choix non-déterministe entre deux pointeurs */
  ex3 = Frama_C_nondet_ptr(&ex1, &ex2);

  /* intervalle d'entiers */
  ex2 = Frama_C_interval(17, 42);

  /* intervalle de flottants */ 
  f = Frama_C_float_interval(1.0, 5.0);
}
#endif

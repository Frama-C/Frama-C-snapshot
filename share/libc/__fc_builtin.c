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

/* $Id: builtin.c,v 1.16 2008-11-21 09:19:53 uid527 Exp $ */

#include "__fc_builtin.h"

int Frama_C_entropy_source;

//@ assigns Frama_C_entropy_source \from Frama_C_entropy_source;
void Frama_C_update_entropy(void);

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

#if 0
static int ex1, ex2;
static int *ex3;
static float f;

void Frama_C_builtin_examples(void)
{
  /* non-determinist choice between two integers */
  ex1 = Frama_C_nondet(17, 42);

  /* non-determinist choice between two pointers */
  ex3 = Frama_C_nondet_ptr(&ex1, &ex2);

  /* integers interval */
  ex2 = Frama_C_interval(17, 42);

  /* floats interval */
  f = Frama_C_float_interval(1.0, 5.0);
}
#endif

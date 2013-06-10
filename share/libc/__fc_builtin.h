/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2013                                               */
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

#ifndef Frama_C_BUILTIN
#define Frama_C_BUILTIN
#include "__fc_define_size_t.h"
#include "__fc_builtin_for_normalization.i"

extern int Frama_C_entropy_source;

/*@ assigns \result \from a, b, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures \result == a || \result == b ;
 */
int Frama_C_nondet(int a, int b);

/*@ assigns \result \from a, b, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures \result == a || \result == b ;
 */
void *Frama_C_nondet_ptr(void *a, void *b);

/*@ assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures min <= \result <= max ;
 */
int Frama_C_interval(int min, int max);

/*@ assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures min <= \result <= max ;
 */
float Frama_C_float_interval(float min, float max);

/*@ assigns ((char *)dest)[0..n-1] \from ((char *)src)[0..n-1];
    assigns \result \from dest; 
    ensures dest[0..n] == src[0..n];
*/
void* Frama_C_memcpy(char *dest, const char *src, unsigned long n);

/*@
  terminates \false;
  assigns \empty;
  ensures \false;
*/
void Frama_C_abort(void) __attribute__ ((noreturn));

void Frama_C_show_each_warning(const char*, ...);

size_t Frama_C_offset(const void*);

void *Frama_C_undegenerate(const void*);

#endif

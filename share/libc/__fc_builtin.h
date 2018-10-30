/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2018                                               */
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
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_size_t.h"

__BEGIN_DECLS

extern volatile int Frama_C_entropy_source __attribute__((unused)) __attribute__((FRAMA_C_MODEL));

/*@ requires valid_p: \valid(p + (0 .. l-1));
    assigns p[0 .. l-1] \from Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures initialization: \initialized(p + (0 .. l-1));
*/
extern void Frama_C_make_unknown(char *p, size_t l);

/*@ assigns \result \from a, b, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_a_or_b: \result == a || \result == b ;
 */
extern int Frama_C_nondet(int a, int b);

/*@ assigns \result \from a, b, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_a_or_b: \result == a || \result == b ;
 */
extern void *Frama_C_nondet_ptr(void *a, void *b);

/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern int Frama_C_interval(int min, int max);

/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern int Frama_C_interval_split(int min, int max);

/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern unsigned char Frama_C_unsigned_char_interval
  (unsigned char min, unsigned char max);

/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern char Frama_C_char_interval(char min, char max);

/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern unsigned short Frama_C_unsigned_short_interval(unsigned short min, unsigned short max);

/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern short Frama_C_short_interval(short min, short max);


/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern unsigned int Frama_C_unsigned_int_interval(unsigned int min, unsigned int max);

/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern int Frama_C_int_interval(int min, int max);


/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern unsigned long Frama_C_unsigned_long_interval
     (unsigned long min, unsigned long max);

/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern long Frama_C_long_interval(long min, long max);


/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern unsigned long long Frama_C_unsigned_long_long_interval
     (unsigned long long min, unsigned long long max);

/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern long long Frama_C_long_long_interval(long long min, long long max);


/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern size_t Frama_C_size_t_interval(size_t min, size_t max);


/*@ requires finite: \is_finite(min) && \is_finite(max);
    requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: \is_finite(\result) && min <= \result <= max;
 */
extern float Frama_C_float_interval(float min, float max);

/*@ requires finite: \is_finite(min) && \is_finite(max);
    requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: \is_finite(\result) && min <= \result <= max;
 */
extern double Frama_C_double_interval(double min, double max);

/*@ requires finite: \is_finite(min) && \is_finite(max);
    requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: \is_finite(\result) && min <= \result <= max;
 */
extern double Frama_C_real_interval_as_double(double min, double max);

/*@ // Signals an error;
  terminates \false;
  assigns \nothing;
  ensures never_terminates: \false;
*/
extern void Frama_C_abort(void) __attribute__ ((__noreturn__));

/*@ assigns \result \from p; */
extern size_t Frama_C_offset(const void* p);

extern void *Frama_C_malloc_fresh(size_t size);

//@ assigns \result \from i;
extern long long Frama_C_abstract_cardinal(long long i);
//@ assigns \result \from i;
extern long long Frama_C_abstract_max(long long i);
//@ assigns \result \from i;
extern long long Frama_C_abstract_min(long long i);

__END_DECLS

__POP_FC_STDLIB
#endif

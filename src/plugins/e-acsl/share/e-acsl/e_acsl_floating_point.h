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

/*! ***********************************************************************
 * \file   e_acsl_floating_point.h
 * \brief  Functionality related to processing of floating point values
***************************************************************************/

#ifndef E_ACSL_FLOATING_POINT_H
#define E_ACSL_FLOATING_POINT_H

#include "e_acsl.h"
#include <math.h>
#include <float.h>
#include <fenv.h>

/* Below variables hold infinity values for floating points defined in math.h.
   Most of them are defined as macros that expand to built-in function calls.
   As such, they cannot be used in E-ACSL specifications directly. To solve
   the issue this header provides alternative definitions prefixed
   __e_acsl_math_. For instance, if a call to `pow` overflows it
   returns `HUGE_VAL`. To make sure that the result of pow does not overflow
   one can use the following contract:

   extern double __e_acsl_math_HUGE_VAL;

   //@ ensures \result != __e_acsl_math_HUGE_VAL;
   double pow(double, double);
*/

/** \brief Positive infinity for doubles: same as HUGE_VAL */
double math_HUGE_VAL = 0.0;
/** \brief Positive infinity for floats: same as HUGE_VALF */
float  math_HUGE_VALF = 0.0;
/** \brief Representation of infinity value for doubles: same as INFINITY */
double math_INFINITY = 0.0;

/* FIXME: An additional variable that should be added to this list is
     long double math_HUGE_VALL;
   That represents positive infinity for long doubles. However, long doubles
   are unsupported Value plug-in analysis who start throwing errors once
   test suite is ran. */

static void init_infinity_values() {
  /* Initialize E-ACSL infinity values */
  math_HUGE_VAL  = HUGE_VAL;
  math_HUGE_VALF = HUGE_VALF;
  math_INFINITY  = INFINITY;
  /* Clear exceptions buffers */
  feclearexcept(FE_ALL_EXCEPT);
}

void floating_point_exception(const char *exp) {
  int except = fetestexcept(FE_ALL_EXCEPT);
  char *resp = NULL;
  if (except) {
    if (fetestexcept(FE_DIVBYZERO))
      resp = "Division by zero";
    else if (fetestexcept(FE_INEXACT))
      resp = "Rounded result of an operation is not equal to the infinite precision result";
    else if (fetestexcept(FE_INVALID))
      resp = "Result of a floating-point operation is not well-defined";
    else if (fetestexcept(FE_OVERFLOW))
      resp = "Floating-point overflow";
    else if (fetestexcept(FE_UNDERFLOW))
      resp = "Floating-point underflow";
  }
  if (resp) {
    rtl_printf("Execution of the statement `%s` leads to a floating point exception\n", exp);
    rtl_printf("Exception:  %s\n", resp);
  }
  feclearexcept(FE_ALL_EXCEPT);
}

#endif

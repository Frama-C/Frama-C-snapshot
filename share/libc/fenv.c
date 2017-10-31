/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2017                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  All rights reserved.                                                  */
/*  Contact CEA LIST for licensing.                                       */
/*                                                                        */
/**************************************************************************/

/* ISO C: 7.6 */
#include "fenv.h"

/** Determines which of a specified subset of the floating-point exception flags
 *  are currently set.
 *  \param excepts  Specifies the floating-point status flags to be queried.
 *  \return  The value of the bitwise OR of the floating-point exception macros
 *    corresponding to the currently set floating-point exceptions included in
 *    parameter excepts.
 */
int fetestexcept( int excepts )
{
  static volatile int __fc_random_fetestexcept __attribute__((FRAMA_C_MODEL)); /* random represent the FPU status word. */

  return (0x00FF & __fc_random_fetestexcept); /* B, C3, TOSP, C2, C1, and C0 don't matter. Mask the selected bits. */
}


volatile fenv_t __fc_fenv_state __attribute__((FRAMA_C_MODEL));


/** Saves the current floating-point environment in the object pointed to by
 *  envp, clears the floating-point status flags, and then installs a non-stop
 *  (continue on floating-point exceptions) mode for all floating-point
 *  exceptions.
 *  \return Always returns zero.
 */
int feholdexcept( fenv_t *envp )
{
  *envp = __fc_fenv_state; /* store the current FPU environment */

  return 0;
}


/** Establishes the floating-point environment represented by the object pointed
 *  to by envp. The argument envp shall point to a valid floating-point
 *  environment object.
 *  In principle, this function has the potential to trigger pending previous
 *  exceptions: If envp contains a raised exception flag and at the same time
 *  unmasks that exception type, then this will cause an interrupt.
 */
void fesetenv( const fenv_t *envp )
{
  __fc_fenv_state = *envp;
}

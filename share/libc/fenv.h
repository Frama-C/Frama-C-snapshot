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

/* ISO C: 7.6 */
#ifndef __FC_FENV
#define __FC_FENV

/* Define bits representing the exception.  We use the bit positions
   of the appropriate bits in the FPU control word.  */
enum
  {
    FE_INVALID = 0x01,
#define FE_INVALID	FE_INVALID
    __FE_DENORM = 0x02,
    FE_DIVBYZERO = 0x04,
#define FE_DIVBYZERO	FE_DIVBYZERO
    FE_OVERFLOW = 0x08,
#define FE_OVERFLOW	FE_OVERFLOW
    FE_UNDERFLOW = 0x10,
#define FE_UNDERFLOW	FE_UNDERFLOW
    FE_INEXACT = 0x20
#define FE_INEXACT	FE_INEXACT
  };

#define FE_ALL_EXCEPT \
	(FE_INEXACT | FE_DIVBYZERO | FE_UNDERFLOW | FE_OVERFLOW | FE_INVALID)

/* Type representing floating-point environment.  This structure
   corresponds to the layout of the block written by the `fstenv'
   instruction and has additional fields for the contents of the MXCSR
   register as written by the `stmxcsr' instruction.  */
typedef struct
  {
    unsigned short int __control_word;
    unsigned short int __unused1;
    unsigned short int __status_word;
    unsigned short int __unused2;
    unsigned short int __tags;
    unsigned short int __unused3;
    unsigned int __eip;
    unsigned short int __cs_selector;
    unsigned int __opcode:11;
    unsigned int __unused4:5;
    unsigned int __data_offset;
    unsigned short int __data_selector;
    unsigned short int __unused5;
#ifdef __FC_MACHDEP_X86_64 /* only for x86_64 */
    unsigned int __mxcsr;
#endif
  }
fenv_t;

/** Determines which of a specified subset of the floating-point exception flags
 *  are currently set.
 *  \param excepts  Specifies the floating-point status flags to be queried.
 *  \return  The value of the bitwise OR of the floating-point exception macros
 *    corresponding to the currently set floating-point exceptions included in
 *    parameter excepts.
 */
int fetestexcept( int excepts );

/** Saves the current floating-point environment in the object pointed to by
 *  envp, clears the floating-point status flags, and then installs a non-stop
 *  (continue on floating-point exceptions) mode for all floating-point
 *  exceptions.
 *  \return Always returns zero.
 */
int feholdexcept( fenv_t *envp );

/** Establishes the floating-point environment represented by the object pointed
 *  to by envp. The argument envp shall point to a valid floating-point
 *  environment object.
 *  In principle, this function has the potential to trigger pending previous
 *  exceptions: If envp contains a raised exception flag and at the same time
 *  unmasks that exception type, then this will cause an interrupt.
 */
void fesetenv( const fenv_t *envp );

/** Clears the supported floating-point exceptions represented by argument. 
 */
void feclearexcept( int excepts );

#endif /* __FC_FENV */
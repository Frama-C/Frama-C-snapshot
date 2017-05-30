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
 * \file  e_acsl.h
 * \brief E-ACSL Public API independent of memory models
 ***************************************************************************/

#ifndef E_ACSL
#define E_ACSL

/******************************/
/* Dedicated E-ACSL assertion */
/******************************/

/*! \brief Runtime assertion verifying a predicate
 *  \param pred  integer code of a predicate
 *  \param kind  a C string representing an annotation's
 *    kind (e.g., "Assertion")
 *  \param fct
 *  \param pred_txt  stringified predicate
 *  \param line  line number of the predicate placement in the
 *    un-instrumented file */
/*@ requires pred != 0;
  @ assigns \nothing; */
void __e_acsl_assert(int pred,
		   char *kind,
		   char *fct,
		   char *pred_txt,
		   int line)
  __attribute__((FC_BUILTIN));

#endif
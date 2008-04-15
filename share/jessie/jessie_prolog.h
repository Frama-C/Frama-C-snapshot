/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2008                                               */
/*    INRIA (Institut National de Recherche en Informatique et en         */
/*           Automatique)                                                 */
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
/**************************************************************************/

/* $Id: jessie_prolog.h,v 1.8 2008/12/09 10:17:25 uid525 Exp $ */

#ifndef _JESSIE_PROLOG_H_
#define _JESSIE_PROLOG_H_

#ifdef JESSIE_NO_PROLOG
#else

#ifdef JESSIE_EXACT_INT_MODEL
# include "jessie_exact_prolog.h"
#else
# include "jessie_machine_prolog.h"
#endif

/*@ logic integer minimum(integer i, integer j) = i < j ? i : j;
  @ logic integer maximum(integer i, integer j) = i < j ? j : i;
  @*/

/*@ predicate valid_string{L}(char *s) =
  @   0 <= strlen(s) && \valid_range(s,0,strlen(s));
  @
  @ predicate valid_string_or_null{L}(char *s) =
  @   s == NULL || valid_string(s);
  @
  @ predicate valid_wstring{L}(wchar_t *s) =
  @   0 <= wcslen(s) && \valid_range(s,0,wcslen(s));
  @
  @ predicate valid_wstring_or_null{L}(wchar_t *s) =
  @   s == NULL || valid_wstring(s);
  @*/

#define FRAMA_C_PTR __declspec(valid)
#define FRAMA_C_ARRAY(n) __declspec(valid_range(0,n))
#define FRAMA_C_STRING __declspec(valid_string)
#define FRAMA_C_STRING_OR_NULL __declspec(valid_string_or_null)
#define FRAMA_C_WSTRING __declspec(valid_wstring)
#define FRAMA_C_WSTRING_OR_NULL __declspec(valid_wstring_or_null)

#endif /* JESSIE_NO_PROLOG */

#endif /* _JESSIE_PROLOG_H_ */

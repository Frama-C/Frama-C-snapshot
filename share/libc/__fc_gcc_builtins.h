/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2019                                               */
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

// This file contains some GCC builtins which are not already hardcoded in
// Frama-C, and which can be expressed using ACSL.

#ifndef __FC_GCC_BUILTINS
#define __FC_GCC_BUILTINS
#include "features.h"

__PUSH_FC_STDLIB

__BEGIN_DECLS

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (int)(a + b);
  ensures result_overflow: a + b == (int)(a + b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_sadd_overflow (int a, int b, int *res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (long)(a + b);
  ensures result_overflow: a + b == (long)(a + b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_saddl_overflow (long a, long b, long *res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (long long)(a + b);
  ensures result_overflow: a + b == (long long)(a + b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_saddll_overflow (long long a, long long b, long long *res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (unsigned)(a + b);
  ensures result_overflow: a + b == (unsigned)(a + b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_uadd_overflow (unsigned a, unsigned b, unsigned *res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (unsigned long)(a + b);
  ensures result_overflow: a + b == (unsigned long)(a + b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_uaddl_overflow (unsigned long a, unsigned long b, unsigned long *res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (unsigned long long)(a + b);
  ensures result_overflow: a + b == (unsigned long long)(a + b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_uaddll_overflow (unsigned long long a, unsigned long long b, unsigned long long *res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (int)(a - b);
  ensures result_overflow: a - b == (int)(a - b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_ssub_overflow (int a, int b, int *res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (long)(a - b);
  ensures result_overflow: a - b == (long)(a - b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_ssubl_overflow (long a, long b, long *res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (long long)(a - b);
  ensures result_overflow: a - b == (long long)(a - b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_ssubll_overflow (long long a, long long b, long long *res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (unsigned)(a - b);
  ensures result_overflow: a - b == (unsigned)(a - b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_usub_overflow (unsigned a, unsigned b, unsigned *res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (unsigned long)(a - b);
  ensures result_overflow: a - b == (unsigned long)(a - b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_usubl_overflow (unsigned long a, unsigned long b, unsigned long *res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (unsigned long long)(a - b);
  ensures result_overflow: a - b == (unsigned long long)(a - b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_usubll_overflow (unsigned long long a, unsigned long long b, unsigned long long *res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (int)(a * b);
  ensures result_overflow: a * b == (int)(a * b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_smul_overflow (int a, int b, int *res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (long)(a * b);
  ensures result_overflow: a * b == (long)(a * b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_smull_overflow (long a, long b, long *res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (long long)(a * b);
  ensures result_overflow: a * b == (long long)(a * b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_smulll_overflow (long long a, long long b, long long *res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (unsigned)(a * b);
  ensures result_overflow: a * b == (unsigned)(a * b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_umul_overflow (unsigned a, unsigned b, unsigned *res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (unsigned long)(a * b);
  ensures result_overflow: a * b == (unsigned long)(a * b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_umull_overflow (unsigned long a, unsigned long b, unsigned long *res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (unsigned long long)(a * b);
  ensures result_overflow: a * b == (unsigned long long)(a * b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_umulll_overflow (unsigned long long a, unsigned long long b, unsigned long long *res);

__END_DECLS

__POP_FC_STDLIB
#endif

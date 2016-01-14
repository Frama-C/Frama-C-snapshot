/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2015                                               */
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

#ifndef __FC_SETJMP
#define __FC_SETJMP
#include "features.h"
__BEGIN_DECLS

/* Note: setjmp/longjmp/sigsetjmp/siglongjmp are currently unsupported
   by Frama-C and should not be used. */

typedef int jmp_buf[5]; // arbitrary size

/*@ assigns env[0..4]; // unsound - should "assigns \anything" */
int setjmp(jmp_buf env);

/*@
 assigns \nothing;
 ensures \false; // never terminates
*/
void longjmp(jmp_buf env, int val);

#include "__fc_define_sigset_t.h"
typedef struct {jmp_buf buf; sigset_t sigs;} sigjmp_buf;

/*@ assigns env.buf[0..4]; // unsound - should "assigns \anything" */
int sigsetjmp(sigjmp_buf env, int savesigs);

/*@
 assigns \nothing;
 ensures \false; // never terminates
*/
void siglongjmp(sigjmp_buf env, int val);


__END_DECLS

#endif

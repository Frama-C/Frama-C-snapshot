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

#ifndef __FC_FEATURES_H
#define __FC_FEATURES_H

// *** Definitions to improve compatibility with GCC-specific built-ins
// and GNU-based code ***

#  define __LEAF
#  define __LEAF_ATTR

#ifdef	__cplusplus
# define __BEGIN_DECLS	extern "C" {
# define __END_DECLS	}
#else
# define __BEGIN_DECLS
# define __END_DECLS
#endif

#if defined __cplusplus
# define __THROW	throw ()
# define __THROWNL	throw ()
# define __NTH(fct)	__LEAF_ATTR fct throw ()
#else
# define __THROW
# define __THROWNL
# define __NTH(fct)	fct
#endif

// Frama-C does not support GCC's __builtin_object_size.
// To improve compatibility with some codebases,
// we define it anyway, but it always returns -1, as if
// the compiler were unable to statically determine
// the object size (we only consider the cases where type
// is either 0 or 1).
#define __builtin_object_size (ptr, type) ((size_t)-1)
#define __bos(ptr) __builtin_object_size (ptr, 0)
#define __bos0(ptr) __builtin_object_size (ptr, 0)

#define __warndecl(name, msg) extern void name (void)
#define __warnattr(msg)
#define __errordecl(name, msg) extern void name (void)
#define __nonnull(args...)

#define __attribute_deprecated__ /* Ignore */
#define __attribute_format_arg__(x) /* Ignore */

#define __attribute_warn_unused_result__ /* empty */
#ifndef __wur
# define __wur /* Ignore */
#endif
#define __attribute_artificial__ /* Ignore */

#ifndef __STDC_VERSION__
#define restrict
#else
#define __restrict__
#define __restrict
# if __STDC_VERSION__ >= 199901L && defined (FRAMA_C_C99)
#define restrict restrict
#define __restrict__ restrict
#define __restrict restrict
#  else
#define restrict
#define __restrict__
#define __restrict
# endif
#endif

#define __USE_ISOC99	1

/* end __FC_FEATURES_H */
#endif

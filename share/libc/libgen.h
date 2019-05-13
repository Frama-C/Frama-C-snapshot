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

#ifndef __FC_LIBGEN_H
#define __FC_LIBGEN_H
#include "features.h"
#include "__fc_machdep.h"
#include "__fc_string_axiomatic.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

extern char __fc_basename[__FC_PATH_MAX];
char *__fc_p_basename = __fc_basename;

/*@ // missing: assigns path[0 ..], __fc_p_basename[0 ..] \from 'filesystem';
  requires null_or_valid_string_path: path == \null || valid_read_string(path);
  assigns path[0 ..], __fc_basename[0 ..] \from path[0 ..], __fc_basename[0 ..];
  assigns \result \from __fc_p_basename, path;
  ensures result_points_to_internal_storage_or_path:
    \subset(\result, {__fc_p_basename, path});
*/
extern char *basename(char *path);

extern char __fc_dirname[__FC_PATH_MAX];
char *__fc_p_dirname = __fc_dirname;

/*@ // missing: assigns path[0 ..], __fc_p_dirname[0 ..] \from 'filesystem';
  requires null_or_valid_string_path: path == \null || valid_read_string(path);
  assigns path[0 ..], __fc_dirname[0 ..] \from path[0 ..], __fc_dirname[0 ..];
  assigns \result \from __fc_p_dirname, path;
  ensures result_points_to_internal_storage_or_path:
    \subset(\result, {__fc_p_dirname, path});
*/
extern char *dirname(char *path);

__END_DECLS
__POP_FC_STDLIB
#endif

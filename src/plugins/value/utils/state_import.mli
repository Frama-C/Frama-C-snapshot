(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(** Saving/loading of Value states, possibly among different ASTs.
    Used by the command-line options defined by
    [Value_parameters.SaveFunctionState] and
    [Value_parameters.LoadFunctionState].
    @since Aluminium-20160501 *)

(** Loads the saved initial global state, and merges it with the given state
    (locals plus new globals which were not present in the original AST).
    The saved state may come from a different project.
    Note that, to ensure soundness of the merge, some constraints must be
    respected according to where the merge takes place.
    The intended use is to replace costly function calls, in which case
    the state of local variables should not be modified by the function. *)
val load_and_merge_function_state: Cvalue.Model.t -> Cvalue.Model.t

(** Saves the final state of globals variables after the return statement of
    the function defined via [Value_parameters.SaveFunctionState]. The result
    is saved in the file defined by the same option.
    The function must have been called exactly once during the value analysis,
    otherwise the saved state is unspecified. *)
val save_globals_state: unit -> unit

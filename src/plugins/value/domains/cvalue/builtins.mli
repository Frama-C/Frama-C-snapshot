(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

(** Value analysis builtin shipped with Frama-C, more efficient than their
    equivalent in C *)

exception Invalid_nb_of_args of int

(** Register the given OCaml function as a builtin, that will be used
    instead of the Cil C function of the same name *)
val register_builtin: string -> ?replace:string -> Db.Value.builtin_sig -> unit

(** [clobbered_set_from_ret state ret] can be used for functions that return
    a pointer to where they have written some data. It returns all the bases
    of [ret] whose contents may contain local variables. *)
val clobbered_set_from_ret: Cvalue.Model.t -> Cvalue.V.t -> Base.SetLattice.t

(** Emits warnings for each function definition that will be overridden by an
    Eva built-in.
    Does not include definitions in the Frama-C stdlib.
    @since Phosphorus-20170501-beta1 *)
val warn_definitions_overridden_by_builtins: unit -> unit

type builtin
type call = (Precise_locs.precise_location, Cvalue.V.t) Eval.call
type result = Cvalue.Model.t * Locals_scoping.clobbered_set

(** Returns the cvalue builtin for a function, if any. Also returns the name of
    the builtin and the specification of the function; the preconditions must be
    evaluated along with the builtin. *)
val find_builtin_override:
  Cil_types.kernel_function -> (string * builtin * Cil_types.funspec) option

(* Applies a cvalue builtin for the given call, in the given cvalue state. *)
val apply_builtin:
  builtin -> call -> Cvalue.Model.t -> result list * Value_types.cacheable


(*
Local Variables:
compile-command: "make -C ../../../../.."
End:
*)

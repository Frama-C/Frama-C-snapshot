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

(** Value analysis builtin shipped with Frama-C, more efficient than their
    equivalent in C *)

exception Invalid_nb_of_args of int

(** Register the given OCaml function as a builtin, that will be used
    instead of the Cil C function of the same name *)
val register_builtin: string -> ?replace:string -> Db.Value.builtin_sig -> unit

(** Returns a list of the pairs (name, builtin_sig) registered via
    [register_builtin].
    @since Aluminium-20160501 *)
val registered_builtins: unit -> (string * Db.Value.builtin_sig) list

(** Find a previously registered builtin. Raises [Not_found] if no such
    builtin exists. *)
val find_builtin: string -> Db.Value.builtin_sig

(** Does the builtin table contain an entry for [name]? *)
val mem_builtin: string -> bool

(** Should the given function be replaced by a call to a builtin *)
val find_builtin_override: Kernel_function.t -> Db.Value.builtin_sig option

val clobbered_set_from_ret: Cvalue.Model.t -> Cvalue.V.t -> Base.SetLattice.t
(** [clobbered_set_from_ret state ret] can be used for functions that return
    a pointer to where they have written some data. It returns all the bases
    of [ret] whose contents may contain local variables. *)


(** Emit an ACSL assert (using \warning predicate) to signal that the
    builtin encountered an alarm described by the string.
    [kind] is used to describe the alarm, similarly to [Alarms.get_name].
    Identical alarms are not re-emitted. Returns [true] iff it is a new alarm.

    @modify Silicon-20161101 change return type *)
val emit_alarm: kind:string -> text:string -> bool

(** Iteration on special assertions built by the builtins *)
val fold_emitted_alarms:
  (Cil_types.stmt -> Cil_datatype.Code_annotation.Set.t -> 'a -> 'a) -> 'a -> 'a

(** Emits warnings for each function definition that will be overridden by an
    EVA built-in.
    Does not include definitions in the Frama-C stdlib.
    @since Phosphorus-20170501-beta1 *)
val warn_definitions_overridden_by_builtins: unit -> unit

(*
Local Variables:
compile-command: "make -C ../../../../.."
End:
*)

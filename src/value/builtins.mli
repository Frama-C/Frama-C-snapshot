(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

(** Register the given OCaml function as a builtin, that will be used
    instead of the Cil C function of the same name *)
val register_builtin: string -> Db.Value.builtin_sig -> unit

(** Find a previously registered builtin. Raises [Not_found] if no such
    builtin exists. *)
val find_builtin: string -> Db.Value.builtin_sig

(** Does the builtin table contain an entry for [name]? *)
val mem_builtin: string -> bool

(** Should the given function be replaced by a call to a builtin *)
val overridden_by_builtin: string -> bool

(** Builtins with multiple names; the lookup is done using a distinctive
    prefix *)
(* TODO: move the lookup mechanism into find_builtin *)
val dump_state: Db.Value.builtin_sig
val dump_args: string -> Db.Value.builtin_sig
val dump_state_file: string -> Db.Value.builtin_sig


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

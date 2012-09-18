(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
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

(** Annotations in the AST.
    The AST should be computed before calling functions of this module. *)

open Cil_types
(*
val add_code_annot: 
  Emitter.t -> kernel_function -> stmt -> rooted_code_annotation -> unit

val add_global: Emitter.t -> global_annotation -> unit

type localized_funspec = 
  | Kf of kernel_function
  | Stmt of stmt

type 'a extend_funspec = 
    Emitter.t -> 
    localized_funspec -> 
    funspec ->
    'a

val add_behavior:
  Emitter.t -> localized_funspec -> (identified_predicate, identified_term) spec
  -> unit

val add_variant: Emitter.t -> kernel_function -> term variant -> unit
val add_terminates: Emitter.t -> kernel_function -> identified_predicate -> unit
val add_complete: Emitter.t -> kernel_function -> string list -> unit
val add_disjoint: Emitter.t -> kernel_function -> string list -> unit

val add_requires: 
  Emitter.t -> 
  (identified_predicate, identified_term) behavior ->
  identified_predicate ->
  unit

val add_assumes: 
  Emitter.t -> 
  (identified_predicate, identified_term) behavior ->
  identified_predicate ->
  unit

val add_postcond: 
  Emitter.t -> 
  (identified_predicate, identified_term) behavior ->
  termination_kind -> identified_predicate ->
  unit

val add_assigns:
  Emitter.t -> 
  (identified_predicate, identified_term) behavior ->
  identified_term assigns ->
  unit

val add_extension:
  Emitter.t -> 
  (identified_predicate, identified_term) behavior ->
  string * int * identified_predicate ->
  unit

module Code_annot: sig
end

module Funspec: sig
end

module Global_annot: sig
end
 *)
(*
  Local Variables:
  compile-command: "make -C ../.."
  End:
*)

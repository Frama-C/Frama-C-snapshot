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

open Cil_types

(** [translate_*] translates a given ACSL annotation into the corresponding C
    statement (if any) for runtime assertion checking. This C statements are
    part of the resulting environment. *)

val translate_pre_spec: kernel_function -> kinstr -> Env.t -> funspec -> Env.t
val translate_post_spec: kernel_function -> kinstr -> Env.t -> funspec -> Env.t
val translate_pre_code_annotation: 
  kernel_function -> stmt -> Env.t -> code_annotation -> Env.t
val translate_post_code_annotation: 
  kernel_function -> stmt -> Env.t -> code_annotation -> Env.t
val translate_named_predicate: 
  kernel_function -> Env.t -> predicate -> Env.t

val translate_rte_annots:
  (Format.formatter -> 'a -> unit) -> 
  'a ->
  kernel_function -> 
  Env.t -> 
  code_annotation list ->
  Env.t

exception No_simple_translation of term
val term_to_exp: typ option -> term -> exp

val predicate_to_exp: kernel_function -> predicate -> exp

val set_original_project: Project.t -> unit

(*
Local Variables:
compile-command: "make"
End:
*)

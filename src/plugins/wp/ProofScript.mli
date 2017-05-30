(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

open Tactical
open Conditions

class console : title:string -> Tactical.feedback

type jscript = alternative list
and alternative = private
  | Prover of VCS.prover * VCS.result
  | Tactic of int * jtactic * jscript list (** With number of pending goals *)
  | Error of string * Json.t
and jtactic = {
  header : string ;
  tactic : string ;
  params : Json.t ;
  select : Json.t ;
}

val is_prover : alternative -> bool
val is_tactic : alternative -> bool
val a_prover : VCS.prover -> VCS.result -> alternative
val a_tactic : jtactic -> jscript list -> alternative

val pending : alternative -> int (** pending goals *)
val status : jscript -> int (** minimum of pending goals *)
val decode : Json.t -> jscript
val encode : jscript -> Json.t

val jtactic : title:string -> tactical -> selection -> jtactic
val configure : jtactic -> sequent -> (tactical * selection) option

(** Json Codecs *)

val json_of_selection : selection -> Json.t
val selection_of_json : sequent -> Json.t -> selection
val selection_target : Json.t -> string

val json_of_param : tactical -> parameter -> string * Json.t
val param_of_json : tactical -> sequent -> Json.t -> parameter -> unit

val json_of_parameters : tactical -> Json.t
val parameters_of_json : tactical -> sequent -> Json.t -> unit

val json_of_tactic : jtactic -> Json.t list -> Json.t
val json_of_result : VCS.prover -> VCS.result -> Json.t

val prover_of_json : Json.t -> VCS.prover option
val result_of_json : Json.t -> VCS.result

(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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

(* -------------------------------------------------------------------------- *)
(** Verification Conditions Database *)
(* -------------------------------------------------------------------------- *)

(** {2 Prover} *)

type prover =
  | Why of string (* Prover via WHY *)
  | AltErgo       (* Alt-Ergo *)
  | Coq           (* Coq and Coqide *)
  | WP            (* Simplifier *)

type language =
  | L_why
  | L_coq
  | L_altergo

(* -------------------------------------------------------------------------- *)
(* --- Prover Names                                                       --- *)
(* -------------------------------------------------------------------------- *)

val language_of_prover : prover -> language
val language_of_name : string -> language option
val name_of_prover : prover -> string
val prover_of_name : string -> prover option
val language_of_prover_name: string -> language option
val is_interactive : string -> bool
val gui_provers : prover list

val pp_prover : Format.formatter -> prover -> unit
val pp_language : Format.formatter -> language -> unit

(** {2 Results} *)

type verdict =
  | NoResult
  | Invalid
  | Unknown
  | Timeout
  | Stepout
  | Computing
  | Valid
  | Failed

type result = {
  verdict : verdict ; 
  prover_time : float ; 
  prover_steps : int ;
  prover_errpos : Lexing.position option ;
  prover_errmsg : string ;
}

val no_result : result
val valid : result
val invalid : result
val unknown : result
val timeout : result
val stepout : result
val computing : result
val failed : ?pos:Lexing.position -> string -> result
val result : ?time:float -> ?steps:int -> verdict -> result

val pp_result : Format.formatter -> result -> unit

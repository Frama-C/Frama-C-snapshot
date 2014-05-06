(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

(* -------------------------------------------------------------------------- *)
(** Verification Conditions Database *)
(* -------------------------------------------------------------------------- *)

(** {2 Prover} *)

type prover =
  | Why3 of string (* Prover via WHY *)
  | Why3ide
  | AltErgo       (* Alt-Ergo *)
  | Coq           (* Coq and Coqide *)
  | Qed           (* Qed Solver *)

type mode =
  | BatchMode (* Only check scripts *)
  | EditMode  (* Edit then check scripts *)
  | FixMode   (* Try check script, then edit script on non-success *)

type language =
  | L_why3
  | L_coq
  | L_altergo

(* -------------------------------------------------------------------------- *)
(* --- Prover Names                                                       --- *)
(* -------------------------------------------------------------------------- *)

val language_of_prover : prover -> language
val language_of_name : string -> language option
val name_of_prover : prover -> string
val filename_for_prover : prover -> string
val prover_of_name : string -> prover option
val language_of_prover_name: string -> language option
val mode_of_prover_name : string -> mode
val name_of_mode : mode -> string

val pp_prover : Format.formatter -> prover -> unit
val pp_language : Format.formatter -> language -> unit
val pp_mode : Format.formatter -> mode -> unit

val cmp_prover : prover -> prover -> int

(** {2 Results} *)

type verdict =
  | NoResult
  | Invalid
  | Unknown
  | Timeout
  | Stepout
  | Computing of (unit -> unit) (* kill function *)
  | Valid
  | Failed

type result = {
  verdict : verdict ; 
  solver_time : float ;
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
val computing : (unit -> unit) -> result
val failed : ?pos:Lexing.position -> string -> result
val result : ?solver:float -> ?time:float -> ?steps:int -> verdict -> result

val pp_result : Format.formatter -> result -> unit

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

(* -------------------------------------------------------------------------- *)
(** Verification Condition Status *)
(* -------------------------------------------------------------------------- *)

(** {2 Prover} *)

type prover =
  | Why3 of string (* Prover via WHY *)
  | Why3ide
  | AltErgo       (* Alt-Ergo *)
  | Coq           (* Coq and Coqide *)
  | Qed           (* Qed Solver *)
  | Tactical      (* Interactive Prover *)

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

module Pset : Set.S with type elt = prover
module Pmap : Map.S with type key = prover

val language_of_prover : prover -> language
val language_of_name : string -> language option
val name_of_prover : prover -> string
val title_of_prover : prover -> string
val filename_for_prover : prover -> string
val prover_of_name : string -> prover option
val language_of_prover_name: string -> language option
val mode_of_prover_name : string -> mode
val title_of_mode : mode -> string

val pp_prover : Format.formatter -> prover -> unit
val pp_language : Format.formatter -> language -> unit
val pp_mode : Format.formatter -> mode -> unit

val cmp_prover : prover -> prover -> int

(** {2 Config} 
    [None] means current WP option default.
    [Some 0] means prover default. *)

type config = {
  valid : bool ;
  timeout : int option ;
  stepout : int option ;
  depth : int option ;
}


val current : unit -> config (** Current parameters *)
val default : config (** all None *)

val get_timeout : config -> int (** 0 means no-timeout *)
val get_stepout : config -> int (** 0 means no-stepout *)
val get_depth : config -> int (** 0 means prover default *)

(** {2 Results} *)

type verdict =
  | NoResult
  | Invalid
  | Unknown
  | Timeout
  | Stepout
  | Computing of (unit -> unit) (* kill function *)
  | Checked
  | Valid
  | Failed

type result = {
  verdict : verdict ;
  solver_time : float ;
  prover_time : float ;
  prover_steps : int ;
  prover_depth : int ;
  prover_errpos : Lexing.position option ;
  prover_errmsg : string ;
}

val no_result : result
val valid : result
val checked : result
val invalid : result
val unknown : result
val stepout : result
val timeout : int -> result
val computing : (unit -> unit) -> result
val failed : ?pos:Lexing.position -> string -> result
val kfailed : ?pos:Lexing.position -> ('a,Format.formatter,unit,result) format4 -> 'a
val result : ?solver:float -> ?time:float -> ?steps:int -> ?depth:int -> verdict -> result

val is_auto : prover -> bool
val is_verdict : result -> bool
val is_valid: result -> bool
val configure : result -> config
val autofit : result -> bool (** Result that fits the default configuration *)

val pp_result : Format.formatter -> result -> unit
val pp_result_perf : Format.formatter -> result -> unit

val compare : result -> result -> int (* best is minimal *)

val dkey_no_time_info: Log.category
val dkey_no_step_info: Log.category
val dkey_no_goals_info: Log.category

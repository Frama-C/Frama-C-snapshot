(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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
  | Why3 of Why3Provers.t (* Prover via WHY *)
  | NativeAltErgo (* Direct Alt-Ergo *)
  | NativeCoq     (* Direct Coq and Coqide *)
  | Qed           (* Qed Solver *)
  | Tactical      (* Interactive Prover *)

type mode =
  | BatchMode (* Only check scripts *)
  | EditMode  (* Edit then check scripts *)
  | FixMode   (* Try check script, then edit script on non-success *)

module Pset : Set.S with type elt = prover
module Pmap : Map.S with type key = prover

val name_of_prover : prover -> string
val title_of_prover : prover -> string
val filename_for_prover : prover -> string
val prover_of_name : string -> prover option
val mode_of_prover_name : string -> mode
val title_of_mode : mode -> string

val pp_prover : Format.formatter -> prover -> unit
val pp_mode : Format.formatter -> mode -> unit

val cmp_prover : prover -> prover -> int

(* -------------------------------------------------------------------------- *)
(** {2 Config}
    [None] means current WP option default.
    [Some 0] means prover default. *)
(* -------------------------------------------------------------------------- *)

type config = {
  valid : bool ;
  timeout : int option ;
  stepout : int option ;
}

val current : unit -> config (** Current parameters *)
val default : config (** all None *)

val get_timeout : config -> int (** 0 means no-timeout *)
val get_stepout : config -> int (** 0 means no-stepout *)

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
  cached : bool ;
  solver_time : float ;
  prover_time : float ;
  prover_steps : int ;
  prover_errpos : Lexing.position option ;
  prover_errmsg : string ;
}

val no_result : result
val valid : result
val checked : result
val invalid : result
val unknown : result
val stepout : int -> result
val timeout : int -> result
val computing : (unit -> unit) -> result
val failed : ?pos:Lexing.position -> string -> result
val kfailed : ?pos:Lexing.position -> ('a,Format.formatter,unit,result) format4 -> 'a
val cached : result -> result (** only for true verdicts *)

val result : ?cached:bool -> ?solver:float -> ?time:float -> ?steps:int -> verdict -> result

val is_auto : prover -> bool
val is_verdict : result -> bool
val is_valid: result -> bool
val is_computing: result -> bool
val configure : result -> config
val autofit : result -> bool (** Result that fits the default configuration *)

val pp_result : Format.formatter -> result -> unit
val pp_result_perf : Format.formatter -> result -> unit

val compare : result -> result -> int (* best is minimal *)
val merge : result -> result -> result
val choose : result -> result -> result
val best : result list -> result

val dkey_no_time_info: Wp_parameters.category
val dkey_no_step_info: Wp_parameters.category
val dkey_no_goals_info: Wp_parameters.category
val dkey_no_cache_info: Wp_parameters.category
val dkey_success_only: Wp_parameters.category

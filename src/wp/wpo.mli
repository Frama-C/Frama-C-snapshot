(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

(* ------------------------------------------------------------------------ *)
(**{1 Proof Obligations}                                                    *)
(* ------------------------------------------------------------------------ *)

open Cil_types

(* Dynamically exported as ["Wpo.po"] *)
type t =
{
  po_fun   : kernel_function ; (* function *)
  po_bhv   : string option ;   (* behavior *)
  po_pid   : WpPropId.prop_id ; (* goal target property *)
  po_gid   : string ;  (* goal identifier *) (** Uniquely Identify an object of type Wpo.t *)
  po_env   : string ;  (* goal environment identifier *)
  po_model : string ;  (* model identifier *)
  po_updater : Emitter.t ; (* property status updater *)
  po_name  : string ;  (* goal informal name *)
  po_dep   : Property.t list ; (* dependencies *)
  po_warn  : warning list ;    (* warnings *)
}

and warning = {
  wrn_loc : Lexing.position ;
  wrn_severe : bool ;
  wrn_source : string ;
  wrn_reason : string ;
  wrn_effect : string ;
}

(** Dynamically exported. *)
type prover =
  | Why of string (* Prover via WHY *)
  | AltErgo       (* Alt-Ergo *)
  | Coq           (* Coq and Coqide *)
  | WP            (* Simplifier *)

type language =
  | L_why
  | L_coq
  | L_altergo

type result =
  | Valid
  | Invalid
  | Unknown
  | Timeout
  | Computing
  | Failed of string

(** Dynamically exported
    @since Nitrogen-20111001
*)
val get_gid: t -> string

val clear : unit -> unit

val gid : context:string -> kf:kernel_function -> bhv:string option -> propid:WpPropId.prop_id -> string
val add : t -> unit
val new_env : context:string -> string (** Generates a fresh environment name. *)
val release_env : env:string -> unit (** Releases the last generated environment name. *)
val set_result : t -> prover -> result -> unit

(** Dynamically exported. *)
val get_result : t -> prover -> result option
val get_results : t -> (prover * result) list

(** [true] if the result is valid. Dynamically exported.
    @since Nitrogen-20111001
*)
val is_valid: result -> bool

val iter :
  ?on_environment:(string -> unit) ->
  ?on_function:(kernel_function -> unit) ->
  ?on_behavior:(kernel_function -> string option -> unit) ->
  ?on_goal:(t -> unit) ->
  unit -> unit

(** Dynamically exported.
    @since Nitrogen-20111001
*)
val iter_on_goals: (t -> unit) -> unit

val bar : string
val pp_warning : Format.formatter -> warning -> unit
val pp_depend : Format.formatter -> Property.t -> unit
val pp_dependency : Kernel_function.t -> Format.formatter -> Property.t -> unit
val pp_goal : Format.formatter -> t -> unit
val pp_environment : Format.formatter -> string -> unit
val pp_prover : Format.formatter -> prover -> unit
val pp_result : Format.formatter -> result -> unit
val pp_language : Format.formatter -> language -> unit

val pp_function : Format.formatter -> Kernel_function.t -> string option -> unit
val pp_goal_flow : Format.formatter -> t -> unit

(** {2 File access for WPO} *)

val file_for_ctxt  : env:string -> string
val file_for_head  : gid:string -> string
val file_for_body  : gid:string -> string
(** Dynamically exported. *)
val file_for_log_proof : gid:string -> prover -> string
val file_for_log_check : gid:string -> language -> string
val file_for_po    : gid:string -> language -> string
val file_for_goal  : gid:string -> language -> string
val file_for_env   : env:string -> language -> string
val file_for_model : model:string -> language -> string

val coq_for_env: env:string -> string
val coq_for_model : model:string -> string
val coqc_for_model : model:string -> string

val language_of_prover : prover -> language
val language_of_name : string -> language option
(** Dynamically exported. *)
val prover_of_name : string -> prover option
val language_of_prover_name: string -> language option
val is_interactive : string -> bool
val gui_provers : prover list

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

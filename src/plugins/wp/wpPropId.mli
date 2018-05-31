(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

open Cil_types
open LogicUsage

(** Beside the property identification, it can be found in different contexts
 * depending on which part of the computation is involved.
 * For instance, properties on loops are split in 2 parts : establishment and
 * preservation.
*)

(** Property.t information and kind of PO (establishment, preservation, etc) *)
type prop_id

(** returns the annotation which lead to the given PO.
    Dynamically exported.
*)
val property_of_id : prop_id -> Property.t

val source_of_id : prop_id -> Lexing.position

(*----------------------------------------------------------------------------*)

module PropId : Datatype.S with type t = prop_id

(*----------------------------------------------------------------------------*)

val compare_prop_id : prop_id -> prop_id -> int

val tactical : gid:string -> prop_id
val is_check : prop_id -> bool
val is_tactic : prop_id -> bool
val is_assigns : prop_id -> bool
val is_requires : Property.t -> bool
val is_loop_preservation : prop_id -> stmt option

(** test if the prop_id has to be selected for the asked name.
 * Also returns a debug message to explain then answer. *)
val select_by_name : string list -> prop_id -> bool

(** test if the prop_id has to be selected when we want to select the call
 * precondition the the [stmt] call (None means all the call preconditions).
 * Also returns a debug message to explain then answer. *)
val select_call_pre : stmt -> Property.t option -> prop_id -> bool

(*----------------------------------------------------------------------------*)

val prop_id_keys : prop_id -> string list * string list (* required , hints *)

val get_propid : prop_id -> string (** Unique identifier of [prop_id] *)
val pp_propid : Format.formatter -> prop_id -> unit (** Print unique id of [prop_id] *)

type prop_kind =
  | PKTactic      (** tactical sub-goal *)
  | PKCheck       (** internal check *)
  | PKProp        (** normal property *)
  | PKEstablished (** computation related to a loop property before the loop. *)
  | PKPreserved   (** computation related to a loop property inside the loop. *)
  | PKPropLoop    (** loop property used as hypothesis inside a loop. *)
  | PKVarDecr     (** computation related to the decreasing of a variant in a loop *)
  | PKVarPos      (** computation related to a loop variant being positive *)
  | PKAFctOut     (** computation related to the function assigns on normal termination *)
  | PKAFctExit    (** computation related to the function assigns on exit termination *)
  | PKPre of kernel_function * stmt * Property.t (** precondition for function
                                                     at stmt, property of the require. Many information that should come
                                                     from the p_prop part of the prop_id, but in the PKPre case,
                                                     it seems that it is hidden in a IPBlob property ! *)

val pretty : Format.formatter -> prop_id -> unit
val pretty_context : Description.kf -> Format.formatter -> prop_id -> unit
val pretty_local : Format.formatter -> prop_id -> unit

(** Short description of the kind of PO *)
val label_of_prop_id: prop_id -> string

(** TODO: should probably be somewhere else *)
val string_of_termination_kind : termination_kind -> string

val num_of_bhv_from : funbehavior -> from -> int
(*----------------------------------------------------------------------------*)

val mk_code_annot_ids : kernel_function -> stmt -> code_annotation -> prop_id list

val mk_assert_id : kernel_function -> stmt -> code_annotation -> prop_id

(** Invariant establishment and preservation *)
val mk_loop_inv_id : kernel_function -> stmt ->
  established:bool -> code_annotation -> prop_id

(** Invariant used as hypothesis *)
val mk_inv_hyp_id : kernel_function -> stmt -> code_annotation -> prop_id

(** Variant decrease *)
val mk_var_decr_id : kernel_function -> stmt -> code_annotation -> prop_id

(** Variant positive *)
val mk_var_pos_id : kernel_function -> stmt -> code_annotation -> prop_id

(** \from property of loop assigns. Must not be [FromAny] *)
val mk_loop_from_id : kernel_function -> stmt -> code_annotation ->
  from -> prop_id

(** \from property of function or statement behavior assigns.
    Must not be [FromAny] *)
val mk_bhv_from_id :
  kernel_function -> kinstr -> string list -> funbehavior ->
  from -> prop_id

(** \from property of function behavior assigns. Must not be [FromAny]. *)
val mk_fct_from_id : kernel_function -> funbehavior ->
  termination_kind -> from -> prop_id

(** disjoint behaviors property. 
    See {!Property.ip_of_disjoint} for more information
*)
val mk_disj_bhv_id :
  kernel_function * kinstr * string list * string list -> prop_id

(** complete behaviors property.
    See {!Property.ip_of_complete} for more information
 *)
val mk_compl_bhv_id :
  kernel_function * kinstr * string list * string list -> prop_id

val mk_decrease_id : kernel_function * kinstr * variant -> prop_id

(** axiom identification *)
val mk_lemma_id : logic_lemma -> prop_id

val mk_stmt_assigns_id :
  kernel_function -> stmt -> string list -> funbehavior ->
  from list -> prop_id option

val mk_loop_assigns_id : kernel_function -> stmt -> code_annotation ->
  from list -> prop_id option

(** function assigns *)
val mk_fct_assigns_id : kernel_function -> funbehavior ->
  termination_kind -> from list -> prop_id option

val mk_pre_id : kernel_function -> kinstr -> funbehavior ->
  identified_predicate -> prop_id

val mk_stmt_post_id : kernel_function -> stmt -> funbehavior ->
  termination_kind * identified_predicate -> prop_id

val mk_fct_post_id : kernel_function -> funbehavior ->
  termination_kind * identified_predicate -> prop_id

(** [mk_call_pre_id called_kf s_call called_pre] *)
val mk_call_pre_id : kernel_function -> stmt ->
  Property.t -> Property.t -> prop_id

val mk_property : Property.t -> prop_id

val mk_check : Property.t -> prop_id

(*----------------------------------------------------------------------------*)

type a_kind = LoopAssigns | StmtAssigns
type assigns_desc = private {
  a_label : Clabels.c_label ;
  a_stmt : Cil_types.stmt option ;
  a_kind : a_kind ;
  a_assigns : Cil_types.assigns ;
}
val pp_assigns_desc : Format.formatter -> assigns_desc -> unit

type effect_source = FromCode | FromCall | FromReturn
type assigns_info = prop_id * assigns_desc
val assigns_info_id : assigns_info -> prop_id

type assigns_full_info = private
    AssignsLocations of assigns_info
  | AssignsAny of assigns_desc
  | NoAssignsInfo

val empty_assigns_info : assigns_full_info

val mk_assigns_info : prop_id -> assigns_desc -> assigns_full_info
val mk_stmt_any_assigns_info : stmt -> assigns_full_info
val mk_kf_any_assigns_info : unit -> assigns_full_info
val mk_loop_any_assigns_info : stmt -> assigns_full_info

val pp_assign_info : string -> Format.formatter -> assigns_full_info -> unit
val merge_assign_info :
  assigns_full_info -> assigns_full_info -> assigns_full_info

val mk_loop_assigns_desc : stmt -> from list -> assigns_desc

val mk_stmt_assigns_desc : stmt -> from list -> assigns_desc

val mk_asm_assigns_desc : stmt -> assigns_desc

val mk_kf_assigns_desc : from list -> assigns_desc

val mk_init_assigns : assigns_desc

val is_call_assigns : assigns_desc -> bool

(*----------------------------------------------------------------------------*)

type axiom_info = prop_id * LogicUsage.logic_lemma

val mk_axiom_info : LogicUsage.logic_lemma -> axiom_info
val pp_axiom_info : Format.formatter -> axiom_info -> unit

type pred_info = (prop_id * Cil_types.predicate)

val mk_pred_info : prop_id -> Cil_types.predicate -> pred_info
val pred_info_id : pred_info -> prop_id
val pp_pred_of_pred_info : Format.formatter -> pred_info -> unit
val pp_pred_info : Format.formatter -> pred_info -> unit

(*----------------------------------------------------------------------------*)

(** [mk_part pid (k, n)] build the identification for the [k/n] part of [pid].*)
val mk_part : prop_id -> (int * int) -> prop_id

(** get the 'kind' information. *)
val kind_of_id : prop_id -> prop_kind

(** get the 'part' information. *)
val parts_of_id : prop_id -> (int * int) option

(** How many subproofs *)
val subproofs : prop_id -> int

(** subproof index of this propr_id *)
val subproof_idx : prop_id -> int

val get_induction : prop_id -> stmt option

(*----------------------------------------------------------------------------*)


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

open Cil_types

(* -------------------------------------------------------------------------- *)
(** This file provide all the functions to build a stategy that can then
 * be used by the main generic calculus.  *)
(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
(** {2 Annotations} *)
(* -------------------------------------------------------------------------- *)
   
(** a set of annotations to be added to a program point. *)
type t_annots

val empty_acc : t_annots

(** {3 How to use an annotation} *)

(** An annotation can be used for different purpose. *)
type annot_kind =
  | Ahyp  (** annotation is an hypothesis,
             but not a goal (see Aboth) : A => ...*)
  | Agoal (** annotation is a goal,
             but not an hypothesis (see Aboth): A /\ ...*)
  | Aboth of bool (** annotation can be used as both hypothesis and goal :
    - with true : considerer as both : A /\ A=>..
    - with false : we just want to use it as hyp right now. *)
  | AcutB of bool (** annotation is use as a cut :
    - with true (A is also a goal) -> A (+ proof obligation A => ...)
    - with false (A is an hyp only) -> True (+ proof obligation A => ...) *)
  | AcallHyp of kernel_function
       (** annotation is a called function property to consider as an Hyp.
       * The pre are not here but in AcallPre since they can also
       * be considered as goals. *)
  | AcallPre of bool * kernel_function
       (** annotation is a called function precondition :
           to be considered as hyp, and goal if bool=true *)

(** {3 Adding properties (predicates)} *)

(** generic function to add a predicate property after normalisation.
* All the [add_prop_xxx] functions below use this one. *)
val add_prop : t_annots -> annot_kind -> 
      NormAtLabels.label_mapping -> WpPropId.prop_id ->
      predicate named -> 
      t_annots

(** Add the predicate as a function precondition. 
* Add [assumes => pre] if [assumes] is given. *)
val add_prop_fct_pre : t_annots -> annot_kind ->
  kernel_function -> funbehavior -> 
  assumes: predicate named option -> identified_predicate -> t_annots

(** Add the preconditions of the behavior :
* if [impl_assumes], add [b_assumes => b_requires]
* else add both the [b_requires] and the [b_assumes] *)
val add_prop_fct_bhv_pre : t_annots -> annot_kind ->
  kernel_function -> funbehavior -> impl_assumes:bool -> t_annots

val add_prop_fct_post : t_annots -> annot_kind ->
  kernel_function -> funbehavior -> termination_kind -> identified_predicate
  -> t_annots

(** Add the predicate as a stmt precondition. 
* Add [assumes => pre] if [assumes] is given. *)
val add_prop_stmt_pre : t_annots -> annot_kind -> 
  kernel_function -> stmt -> funbehavior ->
  assumes: predicate named option -> identified_predicate -> t_annots

(** Add the predicate as a stmt precondition.
* Add [\old (assumes) => post] if [assumes] is given. *)
val add_prop_stmt_post :t_annots -> annot_kind ->
  kernel_function -> stmt -> funbehavior -> termination_kind ->
  logic_label option -> assumes:predicate named option -> identified_predicate 
  -> t_annots

(** Add all the [b_requires]. Add [b_assumes => b_requires] if [with_assumes] *)
val add_prop_stmt_bhv_requires : t_annots -> annot_kind -> 
  kernel_function -> stmt -> funbehavior -> with_assumes:bool -> t_annots

(** Process the stmt spec precondition as an hypothesis for external properties.
 * Add [assumes => requires] for all the behaviors. *)
val add_prop_stmt_spec_pre : t_annots -> annot_kind ->
  kernel_function -> stmt -> funspec -> t_annots

val add_prop_call_pre : t_annots -> annot_kind -> WpPropId.prop_id ->
  assumes:predicate named -> identified_predicate -> t_annots

(** Add a postcondition of a called function. Beware that [kf] and [bhv]
* are the called one. *)
val add_prop_call_post : t_annots -> annot_kind ->
  kernel_function -> funbehavior -> termination_kind ->
  assumes:predicate named -> identified_predicate -> t_annots

val add_prop_assert : t_annots -> annot_kind ->
  kernel_function -> stmt -> code_annotation -> predicate named -> t_annots

val add_prop_loop_inv : t_annots -> annot_kind -> 
  stmt -> WpPropId.prop_id -> predicate named -> t_annots

(** {3 Adding assigns properties} *)

(** generic function to add an assigns property. *)
val add_assigns : t_annots -> annot_kind ->
  WpPropId.prop_id -> WpPropId.assigns_desc -> t_annots

(** generic function to add a WriteAny assigns property. *)
val add_assigns_any : t_annots -> annot_kind ->
  WpPropId.assigns_full_info -> t_annots

(** shortcut to add a stmt spec assigns property as an hypothesis. *)
val add_stmt_spec_assigns_hyp : t_annots -> kernel_function -> stmt ->
  logic_label option -> funspec -> t_annots

(** short cut to add a dynamic call *)
val add_call_assigns_any : t_annots -> stmt -> t_annots

(** shortcut to add a call assigns property as an hypothesis. *)
val add_call_assigns_hyp : t_annots -> kernel_function -> stmt ->
  called_kf:kernel_function ->
  logic_label option -> funspec option -> t_annots

(** shortcut to add a loop assigns property as an hypothesis. *)
val add_loop_assigns_hyp : t_annots -> kernel_function -> stmt ->
 (code_annotation * identified_term from list) option -> t_annots

val add_fct_bhv_assigns_hyp : t_annots -> kernel_function -> termination_kind ->
  funbehavior -> t_annots

val assigns_upper_bound : 
                   funspec -> (funbehavior * identified_term from list) option

(** {3 Getting information from annotations} *)

val get_hyp_only : t_annots -> WpPropId.pred_info list
val get_goal_only : t_annots -> WpPropId.pred_info list
val get_both_hyp_goals : t_annots -> 
  WpPropId.pred_info list * WpPropId.pred_info list

(** the [bool] in [get_cut] results says if the property has to be
* considered as a both goal and hyp ([goal=true], or hyp only ([goal=false]) *)
val get_cut : t_annots -> (bool * WpPropId.pred_info) list

(** To be used as hypotheses arround a call, (the pre are in
 * [get_call_pre_goal]) *)
val get_call_hyp : t_annots -> kernel_function -> WpPropId.pred_info list

(** Preconditions of a called function to be considered as hyp and goal
* (similar to [get_both_hyp_goals]). *)
val get_call_pre : t_annots -> kernel_function -> WpPropId.pred_info list * WpPropId.pred_info list

val get_call_asgn : t_annots -> kernel_function option -> WpPropId.assigns_full_info


val get_asgn_hyp : t_annots -> WpPropId.assigns_full_info
val get_asgn_goal : t_annots -> WpPropId.assigns_full_info

(** {3 Printing} *)

val pp_annots : Format.formatter -> t_annots -> unit

(* -------------------------------------------------------------------------- *)
(** {2 Annotation table} *)
(* -------------------------------------------------------------------------- *)
   
type annots_tbl

val create_tbl : unit -> annots_tbl

val add_on_edges : annots_tbl -> t_annots -> Cil2cfg.edge list -> unit

(** [add_node_annots cfg annots v (before, (after, exits))]
* add the annotations for the node :
* @param before preconditions
* @param after postconditions
* @param exits \exits properties
*)
val add_node_annots : annots_tbl -> Cil2cfg.t -> Cil2cfg.node ->
  (t_annots * (t_annots * t_annots)) -> unit

val add_loop_annots : annots_tbl -> Cil2cfg.t -> Cil2cfg.node -> 
  entry:t_annots -> back:t_annots -> core:t_annots -> unit

val add_axiom : annots_tbl -> LogicUsage.logic_lemma -> unit

val add_all_axioms : annots_tbl -> unit

(* -------------------------------------------------------------------------- *)
(** {2 Strategy} *)
(* -------------------------------------------------------------------------- *)
 
type strategy

type strategy_for_froms = {
  get_pre : unit -> t_annots;
  more_vars : logic_var list
}

type strategy_kind =
  | SKannots (** normal mode for annotations *)
  | SKfroms of strategy_for_froms

val mk_strategy : string -> Cil2cfg.t -> string option -> bool ->
                  strategy_kind -> annots_tbl -> strategy

val get_annots : strategy -> Cil2cfg.edge -> t_annots
val new_loop_computation : strategy -> bool
val strategy_has_asgn_goal : strategy -> bool
val strategy_has_prop_goal : strategy -> bool
val strategy_kind : strategy -> strategy_kind
val global_axioms : strategy -> WpPropId.axiom_info list
val behavior_name_of_strategy : strategy -> string option
val is_default_behavior : strategy -> bool

val cfg_of_strategy : strategy -> Cil2cfg.t

val get_kf : strategy -> kernel_function
val get_bhv : strategy -> string option

val pp_info_of_strategy : Format.formatter -> strategy -> unit

(* -------------------------------------------------------------------------- *)
(** {2 Other useful things} *)
(* -------------------------------------------------------------------------- *)

(** The function is the main entry point AND it is not a lib entry *)
val is_main_init : Cil_types.kernel_function -> bool

(** apply [f_normal] on the [Normal] postconditions,
* [f_exits] on the [Exits] postconditions, and warn on the others. *)
val fold_bhv_post_cond : warn:bool ->
  ('n_acc -> Cil_types.identified_predicate -> 'n_acc) ->
  ('e_acc -> Cil_types.identified_predicate -> 'e_acc) ->
  'n_acc * 'e_acc -> funbehavior -> 'n_acc * 'e_acc

val mk_variant_properties : 
  kernel_function -> stmt -> code_annotation -> term ->
  (WpPropId.prop_id * predicate named)
  * (WpPropId.prop_id * predicate named)
(* -------------------------------------------------------------------------- *)

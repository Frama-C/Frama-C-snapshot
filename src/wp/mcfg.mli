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

type scope =
  | SC_Global
  | SC_Function_in    (* Just before the pre-state *)
  | SC_Function_frame (* Just after the introduction of formals *)
  | SC_Function_out   (* Post-state *)
  | SC_Block_in
  | SC_Block_out

module type Export =
sig
  type pred
  type decl
  val export_section : Format.formatter -> string -> unit
  val export_goal : Format.formatter -> string -> pred -> unit
  val export_decl : Format.formatter -> decl -> unit
end

module type Splitter =
sig
  type pred
  val simplify : pred -> pred
  val split : bool -> pred -> pred Bag.t
end

(**
 * This is what is really needed to propagate something through the CFG.
 * Usually, the propagated thing should be a predicate,
 * but it can be more sophisticated like lists of predicates,
 * or maybe a structure to keep hypotheses and goals separated.
 * Moreover, proof obligations may also need to be handeled.
 **)
module type S = sig

  type t_env
  type t_prop

  val pretty : Format.formatter -> t_prop -> unit
  val merge : t_env -> t_prop -> t_prop -> t_prop
  val empty : t_prop

  (** optionally init env with user logic variables *)
  val new_env : ?lvars:Cil_types.logic_var list -> kernel_function -> t_env

  val add_axiom : WpPropId.prop_id -> LogicUsage.logic_lemma -> unit
  val add_hyp  : t_env -> WpPropId.pred_info -> t_prop -> t_prop
  val add_goal : t_env -> WpPropId.pred_info -> t_prop -> t_prop

  val add_assigns : t_env -> WpPropId.assigns_info -> t_prop -> t_prop

  (** [use_assigns env hid kind assgn goal] performs the havoc on the goal.  
   * [hid] should be [None] iff [assgn] is [WritesAny], 
   * and tied to the corresponding identified_property otherwise.*)
  val use_assigns : t_env -> stmt option -> WpPropId.prop_id option ->
    WpPropId.assigns_desc -> t_prop -> t_prop

  val label  : t_env -> Clabels.c_label -> t_prop -> t_prop
  val assign : t_env -> stmt -> lval -> exp -> t_prop -> t_prop
  val return : t_env -> stmt -> exp option -> t_prop -> t_prop
  val test : t_env -> stmt -> exp -> t_prop -> t_prop -> t_prop
  val switch : t_env -> stmt -> exp -> (exp list * t_prop) list -> t_prop -> t_prop
  val init_value : t_env -> lval -> typ -> exp option -> t_prop -> t_prop
    (** init_value env lv t v_opt wp:
        put value of type t (or default if None) in lv *)
  val init_range : t_env -> lval -> typ -> int64 -> int64 -> t_prop -> t_prop
    (** init_range env lv t_elt a b wp :
        put default values of type t_elt in lv[k] with a <= k < b *)

  val loop_entry : t_prop -> t_prop
  val loop_step : t_prop -> t_prop

  (* -------------------------------------------------------------------------- *)
  (* --- Call Rules                                                         --- *)
  (* -------------------------------------------------------------------------- *)

  val call_dynamic : t_env -> stmt -> 
    WpPropId.prop_id -> exp -> (kernel_function * t_prop) list -> t_prop

  val call_goal_precond : t_env -> stmt ->
    kernel_function -> exp list ->
    pre: WpPropId.pred_info list ->
    t_prop -> t_prop

  val call : t_env -> stmt ->
    lval option -> kernel_function -> exp list ->
    pre:     WpPropId.pred_info list ->
    post:    WpPropId.pred_info list ->
    pexit:   WpPropId.pred_info list ->
    assigns: identified_term assigns ->
    p_post: t_prop ->
    p_exit: t_prop ->
    t_prop

  (* -------------------------------------------------------------------------- *)
  (* --- SCOPING RULES                                                      --- *)
  (* -------------------------------------------------------------------------- *)

  val scope : t_env -> varinfo list -> scope -> t_prop -> t_prop
  val close : t_env -> t_prop -> t_prop

  (* -------------------------------------------------------------------------- *)
  (* --- FROM                                                               --- *)
  (* -------------------------------------------------------------------------- *)

  (** build [p => alpha(p)] for functional dependencies verification. *)
  val build_prop_of_from : t_env -> WpPropId.pred_info list -> t_prop -> t_prop

end

(**************************************************************************)
(*                                                                        *)
(*  This file is part of Aorai plug-in of Frama-C.                        *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
(*    INSA  (Institut National des Sciences Appliquees)                   *)
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
open Promelaast

(** Given a transition a function and a function status (call or return)
    it returns if the cross condition can be satisfied
    with only function status.
 *)

val isCrossable:
  (typed_condition * action) trans -> kernel_function -> funcStatus -> bool

(** Given a transition and the main entry point it returns if
    the cross condition can be satisfied at the beginning of the program. *)
val isCrossableAtInit:
  (typed_condition * action) trans -> kernel_function -> bool

(** This function rewrites a cross condition into an ACSL expression.
    Moreover, by giving current operation name and its status (call or
    return) the generation simplifies the generated expression.
 *)
val crosscond_to_pred:
  typed_condition -> kernel_function -> funcStatus -> predicate

(** {b Globals management} *)

(** Copy the file pointer locally in the class in order to easiest globals management and initializes some tables. *)
val initFile : Cil_types.file -> unit

(** Given the name of the main function, this function computes all newly introduced globals (variables, enumeration structure, invariants, etc.) *)
val initGlobals : Cil_types.kernel_function -> bool -> unit

(* ************************************************************************* *)
(** {b Pre/post management} *)

(**{b Pre and post condition of C functions} In our point of view, the pre or
   the post condition of a C function are defined by the set of states
   authorized just before/after the call, as such as the set of crossable
   transitions. The following functions generates abstract pre and post-conditions
   by using only informations deduced from the buchi automata.
*)

(** base lhost corresponding to curState. *)
val host_state_term: unit -> Cil_types.term_lval

(** Returns the predicate saying that automaton is in 
    corresponding state. *)
val is_state_pred: state -> predicate

(** Returns the statement saying the state is affected *)
val is_state_stmt: state * Cil_types.varinfo -> location -> Cil_types.stmt

(** Returns the boolean expression saying the state is affected *)
val is_state_exp: state -> location -> Cil_types.exp

(** Returns the predicate saying that automaton is NOT
    in corresponding state. *)
val is_out_of_state_pred: state -> predicate

(** Returns the statement saying the automaton is not in the corresponding
    state.
    @raise AbortFatal in the deterministic case, as such an assignment is
    meaningless in this context: we only assign the state variable to be
    in the (unique by definition) state currently active
*)
val is_out_of_state_stmt:
  state * Cil_types.varinfo -> location -> Cil_types.stmt

(** Returns the expression testing that automaton is NOT
    in the corresponding state.*)
val is_out_of_state_exp: state -> location -> Cil_types.exp

(** returns assigns clause corresponding to updating automaton's state, and
    assigning auxiliary variable depending on the possible transitions made
    in the function.
    @since Nitrogen-20111001
    @since Neon-20140301 adds kf argument
 *)
val aorai_assigns:
  Data_for_aorai.state -> Cil_types.location -> Cil_types.assigns

(** returns the list of predicates expressing that for each current state
    the automaton currently is in, there is at least one transition that is
    crossed.
*)
val force_transition:
  Cil_types.location -> kernel_function -> Promelaast.funcStatus ->
  Data_for_aorai.state -> Cil_types.identified_predicate list

(** return list of preconditions for the given auxiliary function 
    (f_pre_func or f_post_func). *)
val auto_func_preconditions:
  Cil_types.location -> kernel_function -> Promelaast.funcStatus ->
  Data_for_aorai.state -> Cil_types.identified_predicate list

(** auto_func_behaviors f st (st_status, tr_status)
    generates behaviors corresponding to the transitions authorized by
    tr_status for function f in status st
    @since Nitrogen-20111001
*)
val auto_func_behaviors:
  Cil_types.location -> kernel_function -> Promelaast.funcStatus ->
  Data_for_aorai.state -> Cil_types.funbehavior list

(** [auto_func_block loc f status st res]
    generates the body of pre & post functions.
    res must be [None] for a pre-function and [Some v] for a post-func where
    [v] is the formal corresponding to the value returned by the original
    function. If the original function returns [Void], [res] must be [None].
    It also returns the local variables list declared in the body. *)
val auto_func_block:
  Cil_types.location -> kernel_function -> Promelaast.funcStatus ->
  Data_for_aorai.state -> Cil_types.varinfo option ->
  Cil_types.block * Cil_types.varinfo list

val get_preds_pre_wrt_params :  kernel_function -> predicate

val get_preds_post_bc_wrt_params : kernel_function -> predicate

(** Returns a list of predicate giving for each possible start state the
    disjunction of possible current states
*)
val possible_states_preds:
  Data_for_aorai.state -> predicate list

(** Possible values of the given auxiliary variable under the current path,
    [start]ing from the given point
    @since Neon-20140301 add logic_label argument
 *)
val update_to_pred:
  start: Cil_types.logic_label ->
  pre_state:Promelaast.state -> post_state:Promelaast.state ->
  Cil_types.term -> Data_for_aorai.Intervals.t -> predicate

(** for a given starting and ending state, returns the post-conditions
    related to the possible values of the auxiliary variables at current point
    the function, guarded by the fact that we have followed this path, from
    the given program point
    @modify Neon-20130301 add logic_label argument
 *)
val action_to_pred:
  start:Cil_types.logic_label ->
  pre_state:Promelaast.state -> post_state:Promelaast.state ->
  Data_for_aorai.Vals.t -> predicate list

(** All actions that might have been performed on aux variables from the
    given program point, guarded by the path followed.
    @modify Neon-20140301 add logic_label argument
 *)
val all_actions_preds: 
  Cil_types.logic_label ->
  Data_for_aorai.state -> predicate list

(** Return an integer constant term with the 0 value. *)
val zero_term : unit -> Cil_types.term

(** Given an lval term 'host' and an integer value 'off', it returns a lval term host[off]. *)
val mk_offseted_array : Cil_types.term_lval -> int -> Cil_types.term
val mk_offseted_array_states_as_enum :
  Cil_types.term_lval -> int -> Cil_types.term

(** Returns a term representing the given logic variable
    (usually a fresh quantified variable). *)
val mk_term_from_vi : Cil_types.varinfo -> Cil_types.term

val make_enum_states: unit -> unit

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

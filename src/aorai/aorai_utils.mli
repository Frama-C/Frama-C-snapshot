(**************************************************************************)
(*                                                                        *)
(*  This file is part of Aorai plug-in of Frama-C.                        *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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
open Spec_tools

(** Given a transition a function and a function status (call or return) 
    it returns if the cross condition can be statisfied 
    with only function status. 
 *)

val isCrossable: 
  (typed_condition * action) trans -> kernel_function -> funcStatus -> bool

(** Given a transition and the main entry point it returns if 
    the cross condition can be statisfied at the beginning of the program. *)
val isCrossableAtInit: 
  (typed_condition * action) trans -> kernel_function -> bool

(* ************************************************************************* *)
(** {b Abstract pre/post} *)
(** Given a function, its status (call or return) and an array of boolean 
    describing states status, it returns a couple of boolean array. 
    The first one describes the set of reachable states and 
    the second one is the set of crossable transitions. *)
val get_next : 
  kernel_function -> funcStatus -> bool array -> (bool array * bool array)

(** Given a function, its status (call or return) and an array of boolean 
    describing states status, it returns a couple of boolean array. 
    The first one describes the set of possible initial states and 
    the second one is the set of crossable transitions. *)
val get_prev : 
  kernel_function -> funcStatus -> 
  (bool array * bool array) -> (bool array * bool array)

(** given an event (func, status) and a state returns the predicate 
    that guards the transition to this state.
 *)
val make_prev_pred:
  kernel_function -> funcStatus -> Promelaast.state 
  -> (bool array * bool array) -> Cil_types.predicate Cil_types.named

(** given an event (func, status) and a state returns the 
    predicate that prevents transition to these states.
 *)
val make_prev_pred_neg:
  kernel_function -> funcStatus -> Promelaast.state list
  -> (bool array * bool array) -> Cil_types.predicate Cil_types.named

(* ************************************************************************* *)
(** {b Behaviored pre/post (bycase approach)} *)

(** Given a function name, is status (call or return) and an array of boolean describing states status, it returns a couple of boolean array. The first one describes the set of reachable states and the second one is the set of crossable transitions. *)
val get_next_bycase : 
  kernel_function -> funcStatus -> pre_post_bycase_t -> double_pre_post_bycase_t

(** Given a function name, is status (call or return) and an array of boolean describing states status, it returns a couple of boolean array. The first one describes the set of possible initial states and the second one is the set of crossable transitions. *)
val get_prev_bycase : 
  kernel_function -> funcStatus -> double_pre_post_bycase_t 
  -> double_pre_post_bycase_t

val mk_pre_or_post_bycase_from_pre_or_post : 
  (bool array * bool array) -> double_pre_post_bycase_t

(* ************************************************************************* *)
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

(** returns the predicate saying that automaton is in corresponding state. *)
val is_state_pred: state -> Cil_types.predicate Cil_types.named

(** returns the predicate saying that automaton is NOT 
    in corresponding state. *)
val is_out_of_state_pred: state -> Cil_types.predicate Cil_types.named

(** Given the buchi automata and the name of a function, it returns two arrays
    corresponding to the abstract pre-condition. *)
val mk_abstract_pre :
  typed_automaton -> Cil_types.kernel_function -> (bool array * bool array)

(** Given the buchi automata and the name of a function, it returns two arrays
    corresponding to the abstract post-condition. *)
val mk_abstract_post :
  typed_automaton -> Cil_types.kernel_function -> (bool array * bool array)

(** Generates a term representing the given pre or post condition, i.e.
    that the automaton is in one of the states mapped to [true]. *)
val pre_post_to_term : 
  (bool array * bool array) -> Cil_types.predicate Cil_types.named

(** Generates the negation of the given pre/post, i.e. 
    that the automaton is not in one of the states mapped to [true]. 
 *)
val pre_post_to_term_neg : 
  (bool array * bool array) -> Cil_types.predicate Cil_types.named

(** returns assigns clause corresponding to updating automaton's state. 
    @since Nitrogen-20111001
 *)
val aorai_assigns: 
  Cil_types.location -> Cil_types.identified_term Cil_types.assigns

(** returns the list of predicates expressing that for each current state
    the automaton currently is in, there is at least one transition that is
    crossed.
*)
val force_transition:
  Cil_types.location -> kernel_function -> Promelaast.funcStatus ->
  (bool array * bool array) -> Cil_types.identified_predicate list

(** auto_func_behaviors f st (st_status, tr_status)
    generates behaviors corresponding to the transitions authorized by 
    tr_status for function f in status st
    @since Nitrogen-20111001
*)
val auto_func_behaviors:
  Cil_types.location -> kernel_function -> Promelaast.funcStatus -> 
  (bool array * bool array) -> Cil_types.funbehavior list

val get_preds_pre_wrt_params :
  kernel_function -> Cil_types.predicate Cil_types.named option

val get_preds_post_bc_wrt_params : 
  kernel_function -> Cil_types.predicate Cil_types.named option

val update_to_pred: Promelaast.state -> 
  (term * (term * Data_for_aorai.range) list) -> predicate named

(** for a given kf, a starting and ending state, returns the post-conditions
    related to the possible values of the auxiliary variables at the exit of
    the function.
 *)
val action_to_pred:
  pre_state: Promelaast.state -> 
  post_state: Promelaast.state -> 
  kernel_function -> predicate named list

(** Return an integer constant term with the 0 value. *)
val zero_term : unit -> Cil_types.term

(** Given an lval term 'host' and an integer value 'off', it returns a lval term host[off]. *)
val mk_offseted_array : Cil_types.term_lval -> int -> Cil_types.term
val mk_offseted_array_states_as_enum : 
  Cil_types.term_lval -> int -> Cil_types.term

(** Returns a term representing the given logic variable 
    (usually a fresh quantified variable). *)
val mk_term_from_vi : Cil_types.varinfo -> Cil_types.term

val force_condition_to_predicate : 
  (bool array * bool array) -> (bool array * bool array) -> 
  Cil_types.predicate Cil_types.named

val get_global_loop_inv : Cil_types.stmt -> (bool array * bool array)

val get_restricted_int_pre_bc :
  Cil_types.stmt -> Cil_types.predicate Cil_types.named

val get_restricted_ext_pre_bc : 
  Cil_types.stmt -> Cil_types.predicate Cil_types.named

val get_restricted_int_post_bc : 
  Cil_types.stmt -> Cil_types.predicate Cil_types.named

val make_enum_states: unit -> unit
val debug_display_func_status: string -> unit
val display_operations_spec : unit -> unit
val display_operations_spec_bycase : unit -> unit
val display_operations_spec_sorted : unit -> unit
val display_operations_spec_sorted_bycase : unit -> unit
val debug_display_all_specs : unit -> unit
val debug_display_func_status_bycase  : string -> unit

val display_all_warnings_about_specs : unit -> unit

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

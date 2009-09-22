(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
(*    INSA  (Institut National des Sciences Appliquees)                   *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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

(* $Id: ltl_utils.mli,v 1.4 2008-12-19 15:30:56 uid588 Exp $ *)

open Promelaast
open Spec_tools


(** Given a transition a function name and a function status (call or return) it returns if the cross condition can be statisfied with only function status. *)

val isCrossable: trans -> string -> funcStatus -> bool

(** Given a transition a function name and a function status (call or return) it returns if the cross condition can be statisfied with only function status. *)
val isCrossableAtInit: trans -> string -> bool



(* ************************************************************************* *)
(** {b Abstract pre/post} *)



(** Given a function name, is status (call or return) and an array of boolean describing states status, it returns a couple of boolean array. The first one describes the set of reachable states and the second one is the set of crossable transitions. *)
val get_next : string -> funcStatus -> bool array -> (bool array * bool array) 

(** Given a function name, is status (call or return) and an array of boolean describing states status, it returns a couple of boolean array. The first one describes the set of possible initial states and the second one is the set of crossable transitions. *)
val get_prev : string -> funcStatus -> (bool array * bool array) -> (bool array * bool array) 


(*
(** Given two bool arrays with the same length, it returns a fresh bool array corresponding to a logical and between cells with same index from the two arrays.  *)
val bool_array_and        : bool array              -> bool array              -> bool array
val double_bool_array_and : (bool array*bool array) -> (bool array*bool array) -> (bool array*bool array) 
val quad_bool_array_and   : (bool array*bool array*bool array*bool array) -> (bool array*bool array*bool array*bool array) -> (bool array*bool array*bool array*bool array)
 
(** Given two bool arrays with the same length, it returns a fresh bool array corresponding to a logical AND between cells with same index from the two arrays.  *) 
val bool_array_or         : bool array              -> bool array              -> bool array 
val double_bool_array_or  : (bool array*bool array) -> (bool array*bool array) -> (bool array*bool array) 
val quad_bool_array_or    : (bool array*bool array*bool array*bool array) -> (bool array*bool array*bool array*bool array) -> (bool array*bool array*bool array*bool array)


(** Given two bool arrays with the same length, it returns true if and only if their cells are equal for each index. *)
val bool_array_eq         : bool array              -> bool array              -> bool
val double_bool_array_eq  : (bool array*bool array) -> (bool array*bool array) -> bool 
val quad_bool_array_eq    : (bool array*bool array*bool array*bool array) -> (bool array*bool array*bool array*bool array) -> bool
 

 
val mk_empty_pre_or_post  : unit -> (bool array*bool array)
val mk_empty_spec         : unit -> (bool array*bool array*bool array*bool array)
*)

(*val hash_specs_diff : 
  (Cil_types.stmt ref , (bool array*bool array*bool array*bool array)) Hashtbl.t  -> 
  (Cil_types.stmt ref , (bool array*bool array*bool array*bool array)) Hashtbl.t  
  -> bool  

val debug_display_func_status  : string -> unit 
val debug_display_spec         : (bool array*bool array*bool array*bool array) -> string -> unit
val debug_display_stmt_pre     : bool array -> string -> unit 
val debug_display_stmt_all_pre : (bool array*bool array) -> unit
*)



(* ************************************************************************* *)
(** {b Behaviored pre/post (bycase approach)} *)

(*
type pre_post_bycase_t = bool array array
type double_pre_post_bycase_t = (pre_post_bycase_t*pre_post_bycase_t)
type quad_pre_post_bycase_t = (pre_post_bycase_t*pre_post_bycase_t*pre_post_bycase_t*pre_post_bycase_t) 
*)

(** Given a function name, is status (call or return) and an array of boolean describing states status, it returns a couple of boolean array. The first one describes the set of reachable states and the second one is the set of crossable transitions. *)
val get_next_bycase : string -> funcStatus -> pre_post_bycase_t -> double_pre_post_bycase_t 

(** Given a function name, is status (call or return) and an array of boolean describing states status, it returns a couple of boolean array. The first one describes the set of possible initial states and the second one is the set of crossable transitions. *)
val get_prev_bycase : string -> funcStatus -> double_pre_post_bycase_t -> double_pre_post_bycase_t 


(*
(** Given two bool arrays with the same length, it returns a fresh bool array corresponding to a logical and between cells with same index from the two arrays.  *)
val bool_array_and_bycase        : pre_post_bycase_t        -> pre_post_bycase_t        -> pre_post_bycase_t 
val double_bool_array_and_bycase : double_pre_post_bycase_t -> double_pre_post_bycase_t -> double_pre_post_bycase_t 
val quad_bool_array_and_bycase   : quad_pre_post_bycase_t   -> quad_pre_post_bycase_t   -> quad_pre_post_bycase_t
 
(** Given two bool arrays with the same length, it returns a fresh bool array corresponding to a logical AND between cells with same index from the two arrays.  *)
val bool_array_or_bycase         : pre_post_bycase_t        -> pre_post_bycase_t        -> pre_post_bycase_t  
val double_bool_array_or_bycase  : double_pre_post_bycase_t -> double_pre_post_bycase_t -> double_pre_post_bycase_t
val quad_bool_array_or_bycase    : quad_pre_post_bycase_t   -> quad_pre_post_bycase_t   -> quad_pre_post_bycase_t 


(** Given two bool arrays with the same length, it returns true if and only if their cells are equal for each index. *) 
val bool_array_eq_bycase         : pre_post_bycase_t        -> pre_post_bycase_t        -> bool 
val double_bool_array_eq_bycase  : double_pre_post_bycase_t -> double_pre_post_bycase_t -> bool
val quad_bool_array_eq_bycase    : quad_pre_post_bycase_t   -> quad_pre_post_bycase_t   -> bool 
*)
 
val mk_pre_or_post_bycase_from_pre_or_post : (bool array * bool array) -> double_pre_post_bycase_t

(*
val mk_empty_pre_or_post_bycase  : unit -> double_pre_post_bycase_t
val mk_empty_spec_bycase         : unit -> quad_pre_post_bycase_t

val pre_flattening : (bool array array*bool array array) -> (bool array*bool array)
val post_pseudo_flattening : (bool array array*bool array array) -> (bool array array*bool array array)
*)

(** Given a set of states and the bycase post-condition of an operation 
    this function returns the new post-condition after the execution of the operation in the context of current_st.
*)
val mk_forward_composition : pre_post_bycase_t -> double_pre_post_bycase_t -> double_pre_post_bycase_t

(** Given a set of states and the bycases pre and post-conditions of an operation 
    this function returns the new pre-condition before the execution of the operation in the context of current_st.
*)
val mk_backward_composition: pre_post_bycase_t -> (bool array*bool array) -> double_pre_post_bycase_t -> double_pre_post_bycase_t


val debug_display_func_status_bycase  : string -> unit
(*val debug_display_spec_bycase         : quad_pre_post_bycase_t -> string -> unit
val debug_display_stmt_pre_bycase     : bool array array -> string  -> unit
val debug_display_stmt_all_pre_bycase : double_pre_post_bycase_t -> unit  

 
val is_empty_behavior : bool array -> bool
*)




(* ************************************************************************* *)
(** {b Expressions management} *)

(** This function rewrite a cross condition into a Cil expression.  
    Moreover, by giving current operation name and its status (call or return) the generation simplifies the generated expression. *)
(*val crosscond_to_exp : condition -> string -> Promelaast.funcStatus -> Cil_types.exp    *)



 
(* ************************************************************************* *)
(** {b Globals management} *) 

(** Copy the file pointer locally in the class in order to easiest globals management and initializes some tables. *)
val initFile : Cil_types.file -> unit
 
(** Given the name of the main function, this function computes all newly introduced globals (variables, enumeration structure, invariants, etc.) *)
val initGlobals : string -> bool -> unit
 






(* ************************************************************************* *)
(** {b Buchi automata and C code synchronisation } *)

(** This function returns the list of instructions that have to be introduced just before each call of function and each return of function. These instructions correspond to the synchronisation between C code and Buchi automata. The parameters are :
  + The buchi automata
  + the name of the function that is called or that returns
  + the status of this action (call or return)
  + the localisation associated to this generated code
  + the name of the caller (if any)
  + the stmt id of the call (if any)
*)
val synch_upd : buchautomata -> string -> funcStatus -> Cil_types.location -> string option -> int option -> Cil_types.instr list







(* ************************************************************************* *)
(** {b Pre/post management} *)

(**{b Pre and post condition of C functions} In our point of view, the pre or 
   the post condition of a C function are defined by the set of states 
   authorized just before/after the call, as such as the set of crossable 
   transitions. The following functions generates abstract pre and post-conditions
   by using only informations deduced from the buchi automata.
*)
(** Given the buchi automata and the name of a function, it returns two arrays 
    corresponding to the abstract pre-condition. *)
val mk_asbstract_pre : buchautomata -> string -> (bool array * bool array)

(** Given the buchi automata and the name of a function, it returns two arrays 
    corresponding to the abstract post-condition. *)
val mk_asbstract_post : buchautomata -> string -> (bool array * bool array)

(** Generates a term representing the given pre or post condition. 
    Transitions and states are rewrited into predicates in the same maner. The computation is then generalized 
    Conjunction of forbidden and disjunction of authorized are compute together. *)
val pre_post_to_term : (bool array * bool array) -> (Cil_types.predicate Cil_types.named) list









(** Given a NON EMPTY list of predicates, it returns a conjunction of these predicates. *)
val mk_conjunction_named : (Cil_types.predicate Cil_types.named) list -> (Cil_types.predicate Cil_types.named)
val mk_conjunction : (Cil_types.predicate) list -> (Cil_types.predicate )

(** Given a NON EMPTY list of predicates, it returns a disjunction of these predicates. *)
val mk_disjunction_named : (Cil_types.predicate Cil_types.named) list -> (Cil_types.predicate Cil_types.named)
val mk_disjunction : (Cil_types.predicate ) list -> (Cil_types.predicate )
val mk_expr_disjunction :  (Cil_types.exp) list -> (Cil_types.exp)



val mk_int_exp : int -> Cil_types.exp

(** Return an integer constant term with the 0 value. *)
val zero_term : unit -> Cil_types.term

(** Given an lval term 'host' and an integer value 'off', it returns a lval term host[off]. *)
val mk_offseted_array : Cil_types.term_lval -> int -> Cil_types.term

(** Returns a term representing the given logic variable (usually a fresh quantified variable). *)
val mk_term_from_vi : Cil_types.varinfo -> Cil_types.term


val force_condition_to_predicate : (bool array * bool array) -> (bool array * bool array) -> Cil_types.predicate




val display_operations_spec : unit -> unit
val display_operations_spec_bycase : unit -> unit
val display_operations_spec_sorted : unit -> unit
val display_operations_spec_sorted_bycase : unit -> unit
val debug_display_all_specs : unit -> unit

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)

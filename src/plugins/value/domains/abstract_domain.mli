(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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

(** Abstract domains of the analysis. *)

open Cil_types
open Eval


(** Lattice structure of a domain. *)
module type Lattice = sig
  type state

  val top: state
  (** Greatest element. *)
  val is_included: state -> state -> bool
  (** Inclusion test. *)
  val join: state -> state -> state
  (** Semi-lattice structure. *)
  val join_and_is_included: state -> state -> state * bool
  (** Do both operations simultaneously *)
  val widen: kernel_function -> stmt -> state -> state -> state
  (** [widen h t1 t2] is an over-approximation of [join t1 t2].
      Assumes [is_included t1 t2] *)
end


(** Queries for values stored by a domain about expressions or locations.
    Used in the evaluation of expressions and lvalues. *)
module type Queries = sig

  type state     (** Domain state. *)
  type value     (** Numerical values to which the expressions are evaluated. *)
  type location  (** Abstract memory locations associated to left values. *)

  (** The [origin] is used by the domain combiners to track the origin
       of a value. An abstract domain can always use a dummy type unit for
       origin, or use it to encode some specific information about a value. *)
  type origin

  (** Queries functions return a pair of:
      - the set of alarms that ensures the of the soundness of the evaluation
        of [exp]. [Alarmset.all] is always a sound over-approximation of these
        alarms.
      - a value for the expression, which can be:
        - `Bottom if its evaluation is infeasible;
        - `Value (v, o) where [v] is an over-approximation of the abstract
           value of the expression [exp], and [o] is the origin of the value. *)

  (** Query function for compound expressions:
      [eval oracle t exp] returns the known value of [exp] by the state [t].
      [oracle] is an evaluation function and can be used to find the answer
      by evaluating some others expressions, especially by relational domain.
      No recursive evaluation should be done by this function. *)
  val extract_expr :
    (exp -> value evaluated) ->
    state -> exp -> (value * origin) evaluated

  (** Query function for lvalues:
      [find oracle t lval typ loc] returns the known value stored at
      the location [loc] of the left value [lval] of type [typ]. *)
  val extract_lval :
    (exp -> value evaluated) ->
    state -> lval -> typ -> location -> (value * origin) evaluated

  (** [backward_location state lval typ loc v] reduces the location [loc] of the
      lvalue [lval] of type [typ], so that only the locations that may have value
      [v] are kept.
      The returned location must be included in [loc], but it is always sound
      to return [loc] itself.
      Also returns the value that may have the returned location, if not bottom. *)
  val backward_location :
    state -> lval -> typ -> location -> value -> (location * value) or_bottom

  (** Given a reduction [expr] = [value], provides more reductions that may
      be performed. *)
  val reduce_further : state -> exp -> value -> (exp * value) list

end


(** Transfer function of the domain. *)
module type Transfer = sig

  type state
  type summary
  type value
  type location
  type valuation

  (** [update valuation t] updates the state [t] by the values of expressions
      and the locations of lvalues cached in [valuation]. *)
  val update : valuation -> state -> state

  (** [assign lv expr v valuation state] is the transfer function for the
      assignment [lv = expr] in [state]. It must return the state where the
      assignment has been performed. [v] is the value corresponding to
      the already-evaluated expression [exp].
      [valuation] is a cache of all sub-expressions and locations computed
      for the evaluation of [lval] and [expr]; it can also be used to reduce
      the state. *)
  val assign :
    kinstr -> location left_value -> exp -> value assigned ->
    valuation -> state -> state or_bottom

  (** Transfer function for an assumption.
      [assume exp bool valuation state] returns a state in which the boolean
      expression [exp] evaluates to [bool].
      [valuation] is a cache of all sub-expressions and locations computed
      for the evaluation and the reduction of [expr]; it can also be used
      to reduce the state *)
  val assume : stmt -> exp -> bool -> valuation -> state -> state or_bottom

  (** Decision on a function call:
      [call_action stmt call valuation state] decides on the analysis of
      the called function; see {eval.mli} for details on the action type,
      which described the analysis.
      [stmt] is the statement of the call, [call] represents the call
      (the called function and the arguments) and [state] the state before
      the call.*)
  val call_action:
    stmt -> value call -> valuation -> state ->
    (state, summary, value) action

  (** [summarize kf stmt ~returned_lv state] returns a summary of the
      state [state] at the end of the function [kf]. This summary will be
      used at the call site for the analysis of the calling function.
      [stmt] is the return statement of [kf], and [returned_lv] is None if no
      value is returned, or represents the return lvalue, its type and
      its location (see {Eval.mli} for details). *)
  val summarize:
    kernel_function ->
    stmt ->
    returned:(location left_value * value copied) option ->
    state -> (summary * state) or_bottom

  (** [resolve_call stmt ~assigned_lv call valuation ~pre_state ~post_state]
      compute the state after the statement [stmt] where the lvalue
      [assigned] is assigned to the result of the call [call].
      - [stmt] is the statement of the call;
      - [assigned_lv] is None if no variable is assigned, or is the assigned
        lvalue, its type and its location (see {Eval.mli} for details);
      - [call] represents the function call and its arguments;
      - [pre_state] is the state before the call;
      - [post_state] is the state at the end of the called function, and
        the summary computed by the [summarize] function above. *)
  val resolve_call:
    stmt ->
    value call ->
    assigned:(location left_value * value copied) option ->
    valuation ->
    pre:state ->
    post:summary * state
    -> state or_bottom

  val default_call:
    stmt -> value call -> state ->
    (state, summary, value) call_result

end


(** Logic evaluation. Temporary API.
    TODO: factorization of these functions for generic abstract domain. *)
module type Logic = sig
  type state
  type eval_env (** Evaluation environment. *)
  val env_current_state: eval_env -> state or_bottom
  val env_annot: pre:state -> here:state -> unit -> eval_env
  val env_pre_f: pre:state -> unit -> eval_env
  val env_post_f: pre:state -> post:state -> result:varinfo option ->  unit -> eval_env
  val eval_predicate: eval_env -> predicate named -> Alarmset.status
  val reduce_by_predicate: eval_env -> bool -> predicate named -> eval_env
end

(** Results of an evaluation: the results of all intermediate calculation (the
    value of each expression and the location of each lvalue) are cached in a
    map. *)
module type Valuation = sig
  type t
  type value  (** Abstract value. *)
  type origin (** Origin of abstract values. *)
  type loc    (** Abstract memory location. *)
  val find : t -> exp -> (value, origin) record_val or_top
  val fold : (exp -> (value, origin) record_val -> 'a -> 'a) -> t -> 'a -> 'a
  val find_loc : t -> lval -> loc record_loc or_top
end

(** Signature for the abstract domains of the analysis. *)
module type S = sig
  type state
  include Datatype.S_with_collections with type t = state

  (** {3 Lattice Structure } *)
  include Lattice with type state := t

  (** {3 Queries } *)
  include Queries with type state := t

  (** Summary of the analysis of a function. Useful at a call site. *)
  type summary

  (** Datatypes *)
  module Summary : Datatype.S with type t = summary

  (** {3 Transfer Functions } *)

  (** Transfer functions from the result of evaluations.
      See {eval.mli} for more details about valuation. *)
  module Transfer
      (Valuation: Valuation with type value = value
                             and type origin = origin
                             and type loc = location)
    : Transfer with type state = t
                and type summary = summary
                and type value = value
                and type location = location
                and type valuation = Valuation.t


  (** {3 Logic } *)

  (* TODO: revise this signature. *)
  val compute_using_specification:
    kinstr -> kernel_function * funspec -> t ->
    (t, summary, value) call_result

  include Logic with type state := t

  (** {3 Miscellaneous } *)

  (** Scoping: abstract transformers for entering and exiting blocks.
      [fundec] is the englobing function, and [body] is true if the block is
      the body of [fundec]. (Otherwise, the block delimits an inner scope of
      [fundec].) The interpreter assumes the locals of the block enter or
      leave the scope when these two functions are called, and the variables
      [block.blocals] should be added or removed from the abstract state here.*)
  val open_block : fundec -> block -> body:bool -> t -> t
  val close_block: fundec -> block -> body:bool -> t -> t

  (** Initialization *)
  val empty: unit -> t
  val initialize_var: t -> lval -> location -> (value * bool) or_bottom -> t
  val initialize_var_using_type: t -> varinfo -> t
  val global_state: unit -> (t or_bottom) option

  (** Mem exec. *)
  val filter_by_bases: Base.Hptset.t -> t -> t
  val reuse: current_input:t -> previous_output:t -> t
end


(** Keys identify abstract domains through the type of their abstract values. *)
type 'a key = 'a Structure.Key_Domain.k

(** Describes the internal structure of a domain.
    Used internally to automatically generate efficient accessors from a
    generic abstract domain, which may be a combination of several domains,
    to a specific one. *)
type 'a structure = 'a Structure.Key_Domain.structure =
  | Void : 'a structure
  | Leaf : 'a key -> 'a structure
  | Node : 'a structure * 'b structure -> ('a * 'b) structure

(**
   For a simple 'leaf' domain, the structure should be:
     [let key = Structure.Key_Domain.create_key "name_of_the_domain";;
      let structure = Leaf key;;]

   Then, the key should be exported by the domain, to allow the use of the
   functions defined in the {!External} interface below.

   A compound domain may use the {!Node} constructor to provide a separate
   access to each of its parts.

   A domain can also use the {!Void} constructor to prevent access to itself.
*)

(** Internal implementation of a domain. *)
module type Internal = sig
  include S

  (** A structure matching the type of the domain. *)
  val structure : t structure
end

(** External interface of a domain, with accessors.
    Automatically built by the functor {!Structure.Open}.
    When a generic domain is a combination of several domains, these functions
    allow interacting with its subparts. Note that their behavior is undefined
    if the overall domain contains several times the same domain. *)
module type External = sig
  include S

  (** Tests whether a key belongs to the domain. *)
  val mem : 'a key -> bool

  (** For a key of type [k key]:
      - if the states of this domain (of type [t]) contain a part (of type [k])
        from the domain identified by the key,
        then [get key] returns an accessor for this part of a state.
      - otherwise, [get key] returns None. *)
  val get : 'a key -> (t -> 'a) option

  (** For a key of type [k key]:
      - if the states of this domain (of type [t]) contain a part (of type [k])
        from the domain identified by the key,
        then [set key d state] returns the state [state] in which this part
        has been replaced by [d].
      - otherwise, [set key _] is the identity function. *)
  val set : 'a key -> 'a -> t -> t
end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)

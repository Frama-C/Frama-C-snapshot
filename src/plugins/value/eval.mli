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

open Cil_types

(** Types and functions related to evaluations.
    Heavily used by abstract values and domains, evaluation of expressions,
    transfer functions of instructions and the dataflow analysis. *)


(* -------------------------------------------------------------------------- *)
(**                      {2 Lattice structure }                               *)
(* -------------------------------------------------------------------------- *)

include module type of Bottom.Type

(** For some functions, the special value top (denoting no information)
    is managed separately. *)
type 'a or_top    = [ `Value of 'a | `Top ]


(* -------------------------------------------------------------------------- *)
(**                    {2 Types for the evaluations }                         *)
(* -------------------------------------------------------------------------- *)

(** A type and a set of alarms. *)
type 't with_alarms = 't * Alarmset.t

(** Most forward evaluation functions return the set of alarms resulting from
    the operations, and a result which can be `Bottom, if the evaluation fails,
    or the expected value. *)
type 't evaluated = 't or_bottom with_alarms


(** This monad propagates the `Bottom value if needed, and join the alarms
    of each evaluation. *)
val (>>=) : 'a evaluated -> ('a -> 'b evaluated) -> 'b evaluated

(** Use this monad of the following function returns no alarms. *)
val (>>=.) : 'a evaluated -> ('a -> 'b or_bottom) -> 'b evaluated

(** Use this monad if the following function returns a simple value. *)
val (>>=:) : 'a evaluated -> ('a -> 'b) -> 'b evaluated


(** Most backward evaluation function returns `Bottom if the reduction leads to
    an invalid state, `Unreduced if no reduction can be performed, or the
    reduced value. *)
type 'a reduced = [ `Bottom | `Unreduced | `Value of 'a ]


(** Context for the evaluation of abstract value operators. *)

(** Evaluation of unary operator: unop e1 = e2. *)
type unop_context = exp * exp

(** Evaluation of binary operator: e1 binop e2 = e3. *)
type binop_context = exp * exp * exp * typ

(** Evaluation of an index:
    index, remaining, typ pointed, array size expression *)
type index_context =  exp * offset * typ * exp option


(* -------------------------------------------------------------------------- *)
(**                    {2 Cache for the evaluations }                         *)
(* -------------------------------------------------------------------------- *)


(** The evaluation of an expression stores in a cache the result of all
    intermediate computation. This cache is the outcome of the evaluation,
    and is used by abstract domains for transfer functions.
    It contains
    - the abstract value of each sub-expression, as well as its origin (see
      below), its reduction (see below), and the alarms produced by its
      evaluation.
    - the abstract location of each lvalue of the expression, as well as
      its type, and the alarms produced by its dereference.

    The evaluation queries the abstract domain the value of some sub-expressions.

    The origin of an abstract value is then provided by the abstract domain, and
    kept in the cache. The origin is None if the value has been internally
    computed without calling the domain.

    Also, a value provided by the domain may be reduced by the internal
    computation of the forward and backward evaluation. Such a reduction is
    tracked by the evaluator and reported to the domain, in the cache.
    States of reduction are:
    - Unreduced: the value provided by the domain could not be reduced;
    - Reduced: the value provided by the domain has been reduced;
    - Created: the domain has returned `Top for the given expression;
    - Dull: the domain was not queried for the given expresssion.
*)

(** State of reduction of an abstract value. *)
type reductness =
  | Unreduced  (** No reduction. *)
  | Reduced    (** A reduction has been performed for this expression. *)
  | Created    (** The abstract value has been created. *)
  | Dull       (** Reduction is pointless for this expression. *)

(** Right values with 'undefined' and 'escaping addresses' flags. *)
(* TODO: find a better name. *)
type 'a flagged_value = {
  v: 'a;
  initialized: bool;
  escaping: bool;
}

(** Data record associated to each evaluated expression. *)
type ('a, 'origin) record_val = {
  value: 'a or_bottom flagged_value;  (** The resulting abstract value *)
  origin: 'origin option;   (** The origin of the abstract value *)
  reductness : reductness;  (** The state of reduction. *)
  val_alarms : Alarmset.t   (** The emitted alarms during the evaluation. *)
}

(** Data record associated to each evaluated left-value. *)
type 'a record_loc = {
  loc: 'a;                  (** The location of the left-value. *)
  typ: typ;                 (** *)
  loc_alarms: Alarmset.t    (** The emitted alarms during the evaluation. *)
}

(** Results of an evaluation: the results of all intermediate calculation (the
    value of each expression and the location of each lvalue) are cached in a
    map. *)
module type Valuation = sig
  type t
  type value  (** Abstract value. *)
  type origin (** Origin of values. *)
  type loc    (** Abstract memory location. *)

  val empty : t
  val find : t -> exp -> (value, origin) record_val or_top
  val add : t -> exp -> (value, origin) record_val -> t
  val fold : (exp -> (value, origin) record_val -> 'a -> 'a) -> t -> 'a -> 'a
  val find_loc : t -> lval -> loc record_loc or_top
  val filter :
    (exp -> (value, origin) record_val -> bool) ->
    (lval -> loc record_loc -> bool) ->
    t -> t
end

module Clear_Valuation (Valuation: Valuation) : sig
  val clear_expr : Valuation.t -> exp -> Valuation.t
end


(* -------------------------------------------------------------------------- *)
(**                       {2 Types of assignments }                           *)
(* -------------------------------------------------------------------------- *)

type 'loc left_value = {
  lval: lval;
  lloc: 'loc;
  ltyp: typ;
}

(** Copy of values. *)
type 'value copied =
  | Determinate of 'value flagged_value
  (** Determinates the right value before the copy:
      the copied value is initialized, and without escaping addresses. *)
  | Exact of 'value or_bottom flagged_value
  (** Exact copy of the right value, with possible indeterminateness
      (then, the value can be bottom). *)

(** Assigned values. *)
type 'value assigned =
  | Assign of 'value
  (** Default assignment of a value. *)
  | Copy of lval * 'value copied
  (** Used when the right expression of an assignment is a left value.
      Copy the location of the lvalue [lval], that contains the value
      [value copied]. The copy can remove or not the possible indeterminateness
      of the value (according to the parameters of the analysis). *)

(* Extract the assigned value from a [value assigned]. *)
val value_assigned : 'value assigned -> 'value or_bottom

(* -------------------------------------------------------------------------- *)
(**                       {2 Interprocedural Analysis }                       *)
(* -------------------------------------------------------------------------- *)

type 'value argument = {
  formal: varinfo;
  concrete: exp;
  avalue: 'value assigned;
}

type 'value call = {
  kf: kernel_function;
  arguments: 'value argument list;
  rest: (exp * 'value assigned) list
}

type ('state, 'summary, 'value) return = {
  post_state: 'state;
  returned_value: 'value or_bottom flagged_value option;
  summary: 'summary;
}

type ('state, 'summary, 'value) call_result =
  ('state, 'summary, 'value) return list or_bottom

(** Initialization of a dataflow analysis, by definig the initial value of
    each statement. *)
type 't init =
  | Default
  (** The entry point is initialized to top, and all the others to bottom. *)
  | Continue of 't
  (** The entry point is initialized to the current value,
      and all the others to bottom. *)
  | Custom of (stmt * 't) list
  (** Custom association list for the initial values of statements. Other
      statements are initialized to bottom. *)

(** Action to perform on a call site. *)
type ('state, 'summary, 'value) action =
  | Compute of 'state init * bool
  (** Analyze the called function with the given initialization. If the summary
      of a previous analysis for this initialization has been cached, it will
      be used without re-computation.
      The second boolean indicates whether the result must be cached. *)
  | Recall of 'state init
  (** Do not run the analysis of the function, but use the summary for this
      initialization if it exists. Otherwise, [default_call] is called. *)
  | Result of ('state, 'summary, 'value) call_result * Value_types.cacheable
  (** Direct computation of the result. *)

exception InvalidCall


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

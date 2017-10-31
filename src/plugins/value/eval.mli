(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

type 'a or_top_or_bottom = [ `Value of 'a | `Top | `Bottom ]


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

(** Context for the evaluation of an unary operator: contains the involved
    expressions needed to create the appropriate alarms. *)
type unop_context = {
  operand: exp;
}

(** Context for the evaluation of a binary operator: contains the expressions
    of both operands and of the result, needed to create the appropriate
    alarms. *)
type binop_context = {
  left_operand: exp;
  right_operand: exp;
  binary_result: exp;
}


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
    - Dull: the domain was not queried for the given expression.
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
  v: 'a or_bottom;
  initialized: bool;
  escaping: bool;
}

module Flagged_Value : sig
  val bottom: 'a flagged_value
  val equal:
    ('a -> 'a -> bool) ->
    'a flagged_value -> 'a flagged_value -> bool
  val join:
    ('a -> 'a -> 'a) ->
    'a flagged_value -> 'a flagged_value -> 'a flagged_value
  val pretty:
    (Format.formatter -> 'a -> unit) ->
    Format.formatter -> 'a flagged_value -> unit
end

(** Data record associated to each evaluated expression. *)
type ('a, 'origin) record_val = {
  value: 'a flagged_value;  (** The resulting abstract value *)
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
  val remove : t -> exp -> t
  val remove_loc : t -> lval -> t
end

module Clear_Valuation (Valuation: Valuation) : sig
  (** Removes from the valuation all the subexpressions of [expr] that contain
      [subexpr], except [subexpr] itself. *)
  val clear_englobing_exprs :
    Valuation.t -> expr:exp -> subexpr:exp -> Valuation.t
end


(* -------------------------------------------------------------------------- *)
(**                       {2 Types of assignments }                           *)
(* -------------------------------------------------------------------------- *)

type 'loc left_value = {
  lval: lval;
  lloc: 'loc;
  ltyp: typ;
}

(** Assigned values. *)
type 'value assigned =
  | Assign of 'value
  (** Default assignment of a value. *)
  | Copy of lval * 'value flagged_value
  (** Copy of the location of the lvalue [lval], that contains the value
      [value copied]. The value is copied exactly, with possible
      indeterminateness. *)

(* Extract the assigned value from a [value assigned]. *)
val value_assigned : 'value assigned -> 'value or_bottom

(* -------------------------------------------------------------------------- *)
(**                       {2 Interprocedural Analysis }                       *)
(* -------------------------------------------------------------------------- *)


(** Argument of a function call. *)
type 'value argument = {
  formal: varinfo;          (** The formal argument of the called function. *)
  concrete: exp;            (** The concrete argument at the call site *)
  avalue: 'value assigned;  (** The value of the concrete argument. *)
}

(** A function call. *)
type 'value call = {
  kf: kernel_function;                (** The called function. *)
  arguments: 'value argument list;    (** The arguments of the call. *)
  rest: (exp * 'value assigned) list; (** Extra-arguments. *)
  return: varinfo option;             (** Fake varinfo to store the return value
                                          of the call. *)
  recursive: bool;
}

(** Action to perform on a call site. *)
type 'state call_action =
  | Compute of 'state
  (** Analyze the called function, starting with the given state at the
      entry-point. If the summary of a previous analysis for this initialization
      has been cached, it will be used without re-computation. *)
  | Result  of 'state list or_bottom * Value_types.cacheable
  (** Direct computation of the result. *)

exception InvalidCall


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

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
open Eval

(** Generic evaluation and reduction of expressions and left values. *)

module type S = sig

  type state  (** State of abstract domain. *)
  type value  (** Numeric values to which the expressions are evaluated. *)
  type origin (** Origin of values. *)
  type loc    (** Location of an lvalue. *)

  (** Results of an evaluation: the results of all intermediate calculation (the
      value of each expression and the location of each lvalue) are cached here.
      See {eval.mli} for more details. *)
  module Valuation : Valuation with type value = value
                                and type origin = origin
                                and type loc = loc

  (** [evaluate ~valuation state expr] evaluates the expression [expr] in the
      state [state], and returns the pair [result, alarms], where:
      - [alarms] are the set of alarms ensuring the soundness of the evaluation;
      - [result] is either `Bottom if the evaluation leads to an error,
        or `Value (valuation, value), where [value] is the numeric value computed
        for the expression [expr], and [valuation] contains all the intermediate
        results of the evaluation.
      The [valuation] argument is a cache of already computed expressions.
      It is empty by default.
      The [reduction] argument allows deactivating the backward reduction
      performed after the forward evaluation. *)
  val evaluate :
    ?valuation:Valuation.t -> ?reduction:bool ->
    state -> exp -> (Valuation.t * value) evaluated

  (** Computes the value of a lvalue, with possible indeterminateness: the
      returned value may be uninitialized, or contain escaping addresses.
      Also returns the alarms resulting of the evaluation of the lvalue location,
      and a valuation containing all the intermediate results of the evaluation.
      The [valuation] argument is a cache of already computed expressions.
      It is empty by default. *)
  val copy_lvalue :
    ?valuation:Valuation.t ->
    state -> lval -> (Valuation.t * value flagged_value) evaluated

  (** [lvaluate ~valuation ~for_writing state lval] evaluates the left value
      [lval] in the state [state]. Same general behavior as [evaluate] above
      but evaluates the lvalue into a location and its type.
      The boolean [for_writing] indicates whether the lvalue is evaluated to be
      read or written. It is useful for the emission of the alarms, and for the
      reduction of the location. *)
  val lvaluate :
    ?valuation:Valuation.t -> for_writing:bool ->
    state -> lval -> (Valuation.t * loc * typ) evaluated

  (** [reduce ~valuation state expr positive] evaluates the expression [expr]
      in the state [state], and then reduces the [valuation] such that
      the expression [expr] evaluates to a zero or a non-zero value, according
      to [positive]. By default, the empty valuation is used. *)
  val reduce:
    ?valuation:Valuation.t ->
    state -> exp -> bool -> Valuation.t evaluated

  (** [assume ~valuation state expr value] assumes in the [valuation] that
      the expression [expr] has the value [value] in the state [state],
      and backward propagates this information to the subterm of [expr].
      If [expr] has not been already evaluated in the [valuation], its forward
      evaluation takes place first, but the alarms are dismissed. By default,
      the empty valuation is used.
      The function returns the updated valuation, or bottom if it discovers
      a contradiction. *)
  val assume:
    ?valuation:Valuation.t ->
    state -> exp -> value -> Valuation.t or_bottom

  (* Sorts a list of states by the evaluation of an expression, according to
     a list of expected integer values.
     [split_by_evaluation expr expected_values states] returns two list
     (matched, tail) such as:
     - for each element (i, states, mess) of the first list [matched],
       i was in the list of integer [expected_values], [states] is the list of
       input states where [expr] evaluates to exactly [i], and [mess] is true
       if there was some other input state on which [expr] evaluates to a value
       including [i] (but not equal to [i]).
     - tail are the states on which [expr] does not evaluate to none of the
       [expected_values]. *)
  val split_by_evaluation:
    exp -> Integer.t list -> state list ->
    (Integer.t * state list * bool) list * state list

  val check_non_overlapping:
    state -> lval list -> lval list -> unit evaluated

  val eval_function_exp:
    exp -> state -> (Kernel_function.t * Valuation.t) list evaluated
    (** Evaluation of the function argument of a [Call] constructor *)

end

module type Value = sig
  include Abstract_value.External

  (** Inter-reduction of values. Useful when the value module is a reduced
      product of several abstraction.
      The value computed by the forward evaluation for each sub-expression or
      lvalue is reduced by this function. *)
  val reduce : t -> t
end

module type Queries = sig
  include Abstract_domain.Queries
  include Datatype.S with type t = state
end

(** Generic functor. *)
module Make
    (Value : Value)
    (Loc : Abstract_location.S with type value = Value.t)
    (Domain : Queries with type value = Value.t
                       and type location = Loc.location)
  : S with type state = Domain.state
       and type value = Value.t
       and type origin = Domain.origin
       and type loc = Loc.location


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)

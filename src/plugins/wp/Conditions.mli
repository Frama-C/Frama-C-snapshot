(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(* -------------------------------------------------------------------------- *)
(* --- Weakest Pre Accumulator                                            --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Lang
open Lang.F

(** Sequent *)

type step = private {
  mutable id : int ; (** See [index] *)
  size : int ;
  vars : Vars.t ;
  stmt : stmt option ;
  descr : string option ;
  deps : Property.t list ;
  warn : Warning.Set.t ;
  condition : condition ;
}

and condition =
  | Type of pred
  | Have of pred
  | When of pred
  | Core of pred
  | Init of pred
  | Branch of pred * sequence * sequence
  | Either of sequence list
  | State of Mstate.state

and sequence (** List of steps *)

type sequent = sequence * F.pred

val pretty : (Format.formatter -> sequent -> unit) ref

val step :
  ?descr:string ->
  ?stmt:stmt ->
  ?deps:Property.t list ->
  ?warn:Warning.Set.t ->
  condition -> step

(** Updates the condition of a step and merges [descr], [deps] and [warn] *)
val update_cond :
  ?descr:string ->
  ?deps:Property.t list ->
  ?warn:Warning.Set.t ->
  step ->
  condition -> step

val is_true : sequence -> bool (** Only true or empty steps *)
val is_empty : sequence -> bool (** No step at all *)
val vars_hyp : sequence -> Vars.t
val vars_seq : sequent -> Vars.t

val empty : sequence
val sequence : step list -> sequence
val seq_branch : ?stmt:stmt -> F.pred -> sequence -> sequence -> sequence

val append : sequence -> sequence -> sequence
val concat : sequence list -> sequence

(** Iterate only over the head steps of the sequence *)
val iter : (step -> unit) -> sequence -> unit

(** The internal list of steps *)
val list : sequence -> step list

val size : sequence -> int

val steps : sequence -> int
(** Attributes unique indices to every [step.id] in the sequence, starting from zero.
    Returns the number of steps in the sequence. *)

val index : sequent -> unit
(** Compute steps' id of sequent left hand-side.
    Same as [ignore (steps (fst s))]. *)

val step_at : sequence -> int -> step
(** Retrieve a step by [id] in the sequence.
    The [index] function {i must} have been called on the sequence before
    retrieving the index properly.
    @raise Not_found if the index is out of bounds. *)

val is_trivial : sequent -> bool

(** {2 Transformations} *)

val map_condition : (pred -> pred) -> condition -> condition
val map_step : (pred -> pred) -> step -> step
val map_sequence : (pred -> pred) -> sequence -> sequence
val map_sequent : (pred -> pred) -> sequent -> sequent

val insert : ?at:int -> step -> sequent -> sequent
(** Insert a step in the sequent immediately [at] the specified position.
    Parameter [at] can be [size] to insert at the end of the sequent (default).
    @raise Invalid_argument if the index is out of bounds. *)

val replace : at:int -> step -> sequent -> sequent
(** replace a step in the sequent, the one [at] the specified position.
    @raise Invalid_argument if the index is out of bounds. *)

val subst : (term -> term) -> sequent -> sequent
(** Apply the atomic substitution recursively using [Lang.F.p_subst f].
    Function [f] should only transform the head of the predicate, and can assume
    its sub-terms have been already substituted. The atomic substitution is also applied
    to predicates.
*)

val introduction : sequent -> sequent
(** Performs existential, universal and hypotheses introductions *)

val lemma : pred -> sequent
(** Performs existential, universal and hypotheses introductions *)

val head : step -> pred (** Predicate for Have and such, Condition for Branch, True for Either *)
val have : step -> pred (** Predicate for Have and such, True for any other *)

val condition : sequence -> pred (** With free variables kept. *)
val close : sequent -> pred (** With free variables {i quantified}. *)

val at_closure : (sequent -> sequent ) -> unit (** register a transformation applied just before close *)

(** {2 Bundles}

    Bundles are {i mergeable} pre-sequences.
    This the key structure for merging hypotheses with linear complexity
    during backward weakest pre-condition calculus.
*)

type bundle

type 'a attributed =
  ( ?descr:string ->
    ?stmt:stmt ->
    ?deps:Property.t list ->
    ?warn:Warning.Set.t ->
    'a )

val nil : bundle
val occurs : F.var -> bundle -> bool
val intersect : F.pred -> bundle -> bool
val merge : bundle list -> bundle
val domain : F.pred list -> bundle -> bundle
val intros : F.pred list -> bundle -> bundle
val state : ?descr:string -> ?stmt:stmt -> Mstate.state -> bundle -> bundle
val assume : (?init:bool -> F.pred -> bundle -> bundle) attributed
val branch : (F.pred -> bundle -> bundle -> bundle) attributed
val either : (bundle list -> bundle) attributed
val extract : bundle -> F.pred list
val bundle : bundle -> sequence

(** {2 Simplifier} *)

exception Contradiction

class type simplifier =
  object
    method name : string
    method copy : simplifier
    method assume : F.pred -> unit
    (** Assumes the hypothesis *)
    method target : F.pred -> unit
    (** Give the predicate that will be simplified later *)
    method fixpoint : unit
    (** Called after assuming hypothesis and knowing the goal *)
    method infer : F.pred list
    (** Add new hypotheses implied by the original hypothesis. *)

    method simplify_exp : F.term -> F.term
    (** Currently simplify an expression. *)
    method simplify_hyp : F.pred -> F.pred
    (** Currently simplify an hypothesis before assuming it. In any
        case must return a weaker formula. *)
    method simplify_branch : F.pred -> F.pred
    (** Currently simplify a branch condition. In any case must return an
        equivalent formula. *)
    method simplify_goal : F.pred -> F.pred
    (** Simplify the goal. In any case must return a stronger formula. *)
  end

val clean : sequent -> sequent
val filter : sequent -> sequent
val parasite : sequent -> sequent
val letify : ?solvers:simplifier list -> ?intros:int -> sequent -> sequent
val pruning : ?solvers:simplifier list -> sequent -> sequent

(* -------------------------------------------------------------------------- *)

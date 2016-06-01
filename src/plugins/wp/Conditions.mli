(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Weakest Pre Accumulator                                            --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Lang
open Lang.F

(** Sequent *)

type step = private {
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

and sequence (** List of steps *)
  
type sequent = sequence * F.pred

val step :
  ?descr:string ->
  ?stmt:stmt ->
  ?deps:Property.t list ->
  ?warn:Warning.Set.t ->
  condition -> step

val is_empty : sequence -> bool
val vars_hyp : sequence -> Vars.t
val vars_seq : sequent -> Vars.t

val empty : sequence
val seq_list : step list -> sequence
val seq_branch : ?stmt:stmt -> F.pred -> sequence -> sequence -> sequence

val append : sequence -> sequence -> sequence
val concat : sequence list -> sequence

val iter : (step -> unit) -> sequence -> unit
val iteri : ?from:int -> (int -> step -> unit) -> sequence -> unit

val condition : sequence -> F.pred (** With free variables kept. *)
val close : sequent -> F.pred (** With free variables {i quantified}. *)

(** {2 Bundles}
    
    Bundles are {i mergeable} pre-sequences.
    This the key structure for merging hypotheses with linerar complexity 
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
val assume : (?init:bool -> F.pred -> bundle -> bundle) attributed
val branch : (F.pred -> bundle -> bundle -> bundle) attributed
val either : (bundle list -> bundle) attributed
val extract : bundle -> F.pred list
val sequence : bundle -> sequence

(** {2 Simplifier} *)

exception Contradiction

class type simplifier =
  object
    method name : string
    method copy : simplifier
    method simplify_hyp : F.pred -> F.pred
    (** Currently simplify an hypothesis before assuming it. In any
        case must return a weaker formula. *)
    method assume : F.pred -> unit
    (** Assumes the hypothesis *)
    method target : F.pred -> unit
    (** Give the predicate that will be simplified later *)
    method fixpoint : unit
    (** Called after assuming hypothesis and knowing the goal *)
    method simplify_branch : F.pred -> F.pred
    (** Currently simplify a branch condition. In any case must return an
        equivalent formula. *)
    method infer : F.pred list
    (** Add new hypotheses implyed by the original hypothesis. *)
    method simplify_goal : F.pred -> F.pred
    (** Simplify the goal. In any case must return a stronger formula. *)
  end

val clean : sequent -> sequent
val filter : sequent -> sequent
val letify : ?solvers:simplifier list -> sequent -> sequent
val pruning : ?solvers:simplifier list -> sequent -> sequent


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

(* -------------------------------------------------------------------------- *)
(* --- Weakest Pre Accumulator                                            --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Lang

(** Bundles *)

type bundle
val dump : Format.formatter -> bundle -> unit

type 'a attributed = 
    ( ?descr:string ->
      ?stmt:stmt -> 
      ?deps:Property.t list ->
      ?warn:Warning.Set.t ->
      'a )

val empty : bundle
val occurs : F.var -> bundle -> bool
val intersect : F.pred -> bundle -> bool
val merge : bundle list -> bundle
val domain : F.pred list -> bundle -> bundle
val intros : F.pred list -> bundle -> bundle
val assume : (F.pred -> bundle -> bundle) attributed
val branch : (F.pred -> bundle -> bundle -> bundle) attributed
val either : (bundle list -> bundle) attributed
val extract : bundle -> F.pred list

(** Hypotheses *)

type hypotheses
val hypotheses : bundle -> hypotheses
  
type sequent = hypotheses * F.pred

(** Simplifier *)

exception Contradiction

class type simplifier =
  object
    method name : string
    method copy : simplifier
    method assume : F.pred -> unit
    method target : F.pred -> unit
    method fixpoint : unit
    method simplify : F.pred -> F.pred
    method infer : F.pred list
  end

val clean : sequent -> sequent
val letify : ?solvers:simplifier list -> sequent -> sequent
val pruning : ?solvers:simplifier list -> sequent -> sequent

val close : sequent -> F.pred

(** Pretty *)

type linker
type link = Lstmt of stmt | Lprop of Property.t
val linker : unit -> linker
val get_link : linker -> string -> link

val pretty : ?linker:linker -> Format.formatter -> sequent -> unit

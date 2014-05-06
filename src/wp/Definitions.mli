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

open LogicUsage
open Cil_types
open Ctypes
open Lang
open Lang.F

type cluster

val cluster : id:string -> ?title:string -> ?position:Lexing.position -> unit -> cluster
val axiomatic : axiomatic -> cluster
val section : logic_section -> cluster
val compinfo : compinfo -> cluster
val matrix : c_object -> cluster

val cluster_id : cluster -> string (** Unique *)
val cluster_title : cluster -> string
val cluster_position : cluster -> Lexing.position option
val cluster_age : cluster -> int
val cluster_compare : cluster -> cluster -> int
val pp_cluster : Format.formatter -> cluster -> unit

type trigger = (var,lfun) Qed.Engine.ftrigger
type typedef = (tau,field,lfun) Qed.Engine.ftypedef

type dlemma = {
  l_name  : string ;
  l_cluster : cluster ;
  l_assumed : bool ;
  l_types : int ;
  l_forall : var list ;
  l_triggers : trigger list list ; (** OR of AND-triggers *)
  l_lemma : pred ;
}

type definition =
  | Logic of tau
  | Value of tau * recursion * term
  | Predicate of recursion * pred
  | Inductive of dlemma list

and recursion = Def | Rec

type dfun = {
  d_lfun   : lfun ;
  d_cluster : cluster ;
  d_types  : int ;
  d_params : var list ;
  d_definition : definition ;
}

module Trigger :
sig
  val of_term : term -> trigger
  val of_pred : pred -> trigger
  val vars : trigger -> Vars.t
  val plug : trigger list list -> pred -> var list * pred
end

val define_symbol : dfun -> unit
val update_symbol : dfun -> unit
val find_lemma : logic_lemma -> dlemma (* raises Not_found *)
val compile_lemma  : (logic_lemma -> dlemma) -> logic_lemma -> unit
val define_lemma  : dlemma -> unit
val define_type   : cluster -> logic_type_info -> unit

val call_fun : lfun -> (lfun -> dfun) -> term list -> term
val call_pred : lfun -> (lfun -> dfun) -> term list -> pred

type axioms = cluster * logic_lemma list

class virtual visitor : cluster ->
object

  (** {2 Locality} *)

  method set_local : cluster -> unit
  method do_local : cluster -> bool

  (** {2 Visiting items} *)

  method vadt : ADT.t -> unit
  method vtype : logic_type_info -> unit
  method vcomp : compinfo -> unit
  method vfield : Field.t -> unit
  method vtau : tau -> unit
  method vparam : var -> unit
  method vterm : term -> unit
  method vpred : pred -> unit
  method vsymbol : lfun -> unit
  method vlemma : logic_lemma -> unit
  method vcluster : cluster -> unit
  method vlibrary : string -> unit
  method vgoal : axioms option -> F.pred -> unit
  method vself : unit

  (** {2 Visited definitions} *)

  method virtual section : string -> unit (** Comment *)
  method virtual on_library : string -> unit (** External library to import *)
  method virtual on_cluster : cluster -> unit (** Outer cluster to import *)
  method virtual on_type : logic_type_info -> typedef -> unit (** This local type must be defined *)
  method virtual on_comp : compinfo -> (field * tau) list -> unit (** This local compinfo must be defined *)
  method virtual on_dlemma : dlemma -> unit (** This local lemma must be defined *)
  method virtual on_dfun : dfun -> unit (** This local function must be defined *)

end





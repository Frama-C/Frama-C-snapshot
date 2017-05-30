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

(** {2 Term & Predicate Selection} *)

open Lang.F
open Conditions
open Tactical

val env : Conditions.sequent -> Repr.env

val occurs_x : var -> term -> bool
val occurs_y : var -> pred -> bool
val occurs_e : term -> term -> bool
val occurs_p : term -> pred -> bool
val occurs_q : pred -> pred -> bool

(** Lookup the first occurrence of term in the sequent and returns
    the associated selection. Returns [Empty] is not found.
    Goal is lookup first. *)
val select_e : sequent -> term -> selection

(** Same as [select_e] but for a predicate. *)
val select_p : sequent -> pred -> selection

(** {2 Strategy} *)

type argument = ARG: 'a field * 'a -> argument

type strategy = {
  priority : float ;
  tactical : tactical ;
  selection : selection ;
  arguments : argument list ;
}

class pool :
  object
    method add : strategy -> unit
    method sort : strategy array
  end

class type heuristic =
  object
    method id : string
    method title : string
    method descr : string
    method search : (strategy -> unit) -> sequent -> unit
  end

val register : #heuristic -> unit
val export : #heuristic -> heuristic
val lookup : id:string -> heuristic
val iter : (heuristic -> unit) -> unit

(** {2 Factory} *)

type t = strategy
val arg : 'a field -> 'a -> argument
val set_arg : tactical -> argument -> unit
val set_args : tactical -> argument list -> unit
val make : tactical -> ?priority:float -> selection -> strategy

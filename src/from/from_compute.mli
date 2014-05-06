(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

(** Module implementing the computation of functional dependencies *)

open Cil_types

(** Signature of the module explaining how to find the Froms for a given call
    during the analysis. *)
module type Froms_To_Use_Sig =
sig
  val get : kernel_function -> kinstr -> Function_Froms.t
end

(** Signature of the module explaining how to evaluatue some values during the
    analysis. This is typically Db.Value, or a specialized versions of
    Db.Value on more precise state. *)
module type Values_To_Use_Sig =
sig
  val lval_to_zone_with_deps :
    stmt ->
    deps:Locations.Zone.t option ->
    for_writing:bool ->
    lval ->
    Locations.Zone.t * Locations.Zone.t * bool

  val expr_to_kernel_function :
    stmt ->
    deps:Locations.Zone.t option ->
    exp -> Locations.Zone.t * Kernel_function.Hptset.t

  val get_stmt_state : stmt -> Db.Value.state

  val access_expr : stmt -> exp -> Db.Value.t
end

(** Module explaining how results should be recorded. *)
module type Recording_Sig =
sig
  val accept_base_in_lmap : kernel_function -> Base.t -> bool
  val final_cleanup :
    kernel_function -> Function_Froms.t -> Function_Froms.t
  val record_kf : kernel_function -> Function_Froms.t -> unit
end

(** Function that compute the Froms from a given prototype, called
    in the given state *)
val compute_using_prototype_for_state :
  Db.Value.state -> Kernel_function.t -> Function_Froms.froms


(** Direct computation of the dependencies on expressions, offsets and
    lvals. The state at the statement is taken from Values_To_Use *)
val find_deps_no_transitivity :
  Db.Value.state -> exp -> Function_Froms.Deps.t
val find_deps_lval_no_transitivity :
  Db.Value.state -> lval -> Function_Froms.Deps.t


(** Functor computing the functional dependencies, according to the three
    modules above. *)
module Make :
  functor (Values_To_Use : Values_To_Use_Sig) ->
  functor (Froms_To_Use : Froms_To_Use_Sig) ->
  functor (Recording_To_Do : Recording_Sig) ->
sig

  (** Compute the dependencies of the given function, and return them *)
  val compute_and_return : Kernel_function.t -> Function_Froms.t
  (** Compute the dependencies of the given function *)
  val compute : Kernel_function.t -> unit
end

(** Exception indicating that a given call statement was not evaluated. *)
exception Call_did_not_take_place

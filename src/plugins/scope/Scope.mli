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
open Cil_datatype

(** Scope analysis. *)

(** Interface for the Scope plugin.
    @see <index.html> internal documentation. *)
module Defs : sig
  val get_defs :
    Kernel_function.t -> stmt -> lval ->
     (Stmt.Hptset.t * Locations.Zone.t option) option
  (** @return the set of statements that define [lval] before [stmt] in [kf].
      Also returns the zone that is possibly not defined.
      Can return [None] when the information is not available (Pdg missing). *)

  val get_defs_with_type :
    Kernel_function.t -> stmt -> lval ->
     ((bool * bool) Stmt.Map.t * Locations.Zone.t option) option
(** @return a map from the statements that define [lval] before [stmt] in
        [kf]. The first boolean indicates the possibility of a direct
        modification at this statement, ie. [lval = ...] or [lval = f()].
        The second boolean indicates a possible indirect modification through
        a call.
        Also returns the zone that is possibly not defined.
        Can return [None] when the information is not available (Pdg missing).
 *)
end

module Datascope : sig
  val get_data_scope_at_stmt :
    Kernel_function.t -> stmt -> lval ->
    Stmt.Hptset.t * (Stmt.Hptset.t * Stmt.Hptset.t)
  (**  @raise Kernel_function.No_Definition if [kf] has no definition.
       @return 3 statement sets related to the value of [lval] before [stmt] :
       - the forward selection,
       - the both way selection,
       - the backward selection. *)

  val get_prop_scope_at_stmt :
    kernel_function -> stmt -> code_annotation ->
    Stmt.Hptset.t * code_annotation list
  (** compute the set of statements where the given annotation has the same
      value as before the given stmt. Also returns the eventual code annotations
      that are implied by the one given as argument. *)

  val check_asserts : unit -> code_annotation list
  (** Print how many assertions could be removed based on the previous
      analysis ([get_prop_scope_at_stmt]) and return the annotations
      that can be removed. *)

  val rm_asserts : unit -> unit
  (** Same analysis than [check_asserts] but mark the assertions as proven. *)
end

(** {3 Zones} *)

module Zones : sig
  type t_zones = Locations.Zone.t Stmt.Hashtbl.t
  val build_zones :
    kernel_function -> stmt -> lval -> Stmt.Hptset.t * t_zones
  val pretty_zones : Format.formatter -> t_zones -> unit
  val get_zones : t_zones ->  Cil_types.stmt -> Locations.Zone.t
end

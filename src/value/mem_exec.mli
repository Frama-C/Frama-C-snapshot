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

(** This module memorizes the analysis of entire calls to a function,
    so that those analyzes can be reused later on. *)

(** Counter that must be used each time a new call is analyzed, in order
    to refer to it later *)
val new_counter : unit -> int

(** Subtype of {!Value_types.call_res} *)
module ValueOutputs: Datatype.S with type t =
  (Cvalue.V_Offsetmap.t option * Cvalue.Model.t) list (** states *) *
  Base.SetLattice.t (** cloberred set for local variables *)


(** [store_computed_call (kf, ki) init_state actuals outputs] memoizes the fact
    that calling [kf] at statement [ki], with initial state [init_state]
    and arguments [actuals] resulted in the states [outputs]; the expressions
    in the actuals are not used. Those information are intended to be reused
    in subsequent calls *)
val store_computed_call :
  Value_types.call_site ->
  Cvalue.Model.t ->
  (Cil_types.exp * Cvalue.Model.offsetmap) list -> 
  Value_types.call_result ->
  unit

(** [reuse_previous_call (kf, ki) init_state] searches amongst the previous
    analyzes of [kf] one that matches the initial state [init_state]. If
    none is found, [None] is returned. Otherwise, the results of the analysis
    are returned, together with the index of the matching call. (This last
    information is intended to be used by the plugins that have registered
    Value callbacks.) *)
val reuse_previous_call :
  Value_types.call_site ->
  Cvalue.Model.t ->
  (Cil_types.exp * Cvalue.Model.offsetmap) list -> 
  (Value_types.call_result * int) option

(** Clean all previously stored results *)
val cleanup_results: unit -> unit


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

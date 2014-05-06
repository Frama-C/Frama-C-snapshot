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

(** Declarations that are useful for plugins written on top of the results
    of Value. *)

open Cil_types

(* TODO: These types are already defined in Value_util. *)
type call_site = kernel_function * kinstr
type callstack = call_site list
(** Value callstacks, as used e.g. in Db.Value hooks *)

module Callsite: Datatype.S_with_collections with type t = call_site
module Callstack: Datatype.S_with_collections with type t = callstack

type 'a callback_result =
  | Normal of 'a
  | NormalStore of 'a * int
  | Reuse of int

type cacheable =
  | Cacheable (** Functions whose result can be safely cached *)
  | NoCache (** Functions whose result should not be cached, but for
                which the caller can still be cached. Typically, functions
                printing something during the analysis. *)
  | NoCacheCallers (** Functions for which neither the call, neither the
                       callers, can be cached *)


(** Results of a a call to a function *)
type call_result = {
  c_values: (** Memory states after the call *)
    (Cvalue.V_Offsetmap.t option
       (** the value returned (ie. what is after the 'return' C keyword). *)
     * Cvalue.Model.t
       (** the memory state after the function has been executed *))
    list;

  c_clobbered: Base.SetLattice.t
    (** An over-approximation of the bases in which addresses of local
        variables might have been written *);

  c_cacheable: cacheable
    (** Is it possible to cache the result of this call? *);
}


(** Dependencies for the evaluation of a term or a predicate: for each
    program point involved, sets of zones that must be read *)
type logic_dependencies = Locations.Zone.t Cil_datatype.Logic_label.Map.t

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)


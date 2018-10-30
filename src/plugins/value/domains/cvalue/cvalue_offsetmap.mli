(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

(** Auxiliary functions on cvalue offsetmaps, used by the cvalue domain. *)

open Cil_types
open Cvalue

(** [warn_right_imprecision lval loc offsm] is used for the assignment of the
    lvalue [lval] pointing to the location [loc]; it warns if the offsetmap
    [offsm] contains a garbled mix. *)
val warn_right_imprecision:
  lval -> Locations.location -> V_Offsetmap.t -> unit

(** [offsetmap_of_lval state lval loc] extracts from state [state] the offsetmap
    at location [loc], corresponding to the lvalue [lval]. Warns if this
    offsetmap contains a garbled mix. *)
val offsetmap_of_lval:
  Model.t -> lval -> Precise_locs.precise_location -> V_Offsetmap.t

(** Computes the offsetmap for an assignment:
    - in case of a copy, extracts the offsetmap from the state;
    - otherwise, translates the value assigned into an offsetmap. *)
val offsetmap_of_assignment:
  Model.t -> exp -> (Precise_locs.precise_location, V.t) Eval.assigned ->
  V_Offsetmap.t

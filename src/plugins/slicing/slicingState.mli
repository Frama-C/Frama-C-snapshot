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

(** State of the slicing.
    @since Phosphorus-20170501-beta1 *)

val get: unit -> SlicingTypes.sl_project
(** Get the state of the slicing project.
    Assume it has already been initialized through {!Db.Slicing.reset_slice}. *)

val may: (unit -> unit) -> unit
(** apply the given closure if the slicing project has been initialized through
    {!Db.Slicing.reset_slice}. *)

val may_map: dft:'a -> (unit -> 'a) -> 'a
(** apply the given closure if the slicing project has been initialized through
    {!Db.Slicing.reset_slice}, or else return the default value.*)

val self: State.t
(** Internal state of the slicing tool from project viewpoints.
    @since Sulfur-20171101 *)

val reset_slicing: unit -> unit
(** Function that can be used for:
    - initializing the slicing tool before starting a slicing project;
    - removing all computed slices and all internal pending requests
      of the current slicing project. *)

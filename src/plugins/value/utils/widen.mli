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

(** Per-function computation of widening hints. *)

open Cil_types

(** [getWidenHints kf s] retrieves the set of widening hints related to
    function [kf] and statement [s]. *)
val getWidenHints: kernel_function -> stmt ->
  Base.Set.t * (Base.t -> Locations.Location_Bytes.generic_widen_hint)

(** Parses all widening hints defined via the widen_hint syntax extension.
    The result is memoized for subsequent calls. *)
val precompute_widen_hints: unit -> unit

(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)

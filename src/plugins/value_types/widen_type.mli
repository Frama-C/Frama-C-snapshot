(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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

(** Widening hints for the Value Analysis datastructures. *)

include Datatype.S

(** An empty set of hints *)
val empty : t

(** A default set of hints *)
val default : unit -> t

val join: t -> t -> t

(**  Pretty-prints a set of hints (for debug purposes only).
     @since Silicon-20161101 *)
val pretty : Format.formatter -> t -> unit

(** Define numeric hints for one or all variables ([None]),
    for a certain stmt or for all statements ([None]).  *)
val num_hints:
  Cil_types.stmt option -> Base.t option -> Ival.Widen_Hints.t -> t

(** Define a set of bases to widen in priority for a given statement. *)
val var_hints : Cil_types.stmt -> Base.Set.t -> t

(** Widen hints for a given statement, suitable for function
    {!Cvalue.Model.widen}. *)
val hints_from_keys :
  Cil_types.stmt -> t ->
  Base.Set.t * (Base.t -> Locations.Location_Bytes.generic_widen_hint)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

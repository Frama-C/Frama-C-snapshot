(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

(** Information computed by the From plugin for each function. *)

type froms = {
  deps_return : Lmap_bitwise.From_Model.LOffset.t
       (** Dependencies for the returned value *);
  deps_table : Lmap_bitwise.From_Model.t
    (** Dependencies on all the zones modified by the function *);
}

include Datatype.S with type t = froms

val join: froms -> froms -> froms

val top: froms

val pretty_with_type: Cil_types.typ -> froms Pretty_utils.formatter

(** Extract the left part of a from result, ie. the zones that are written *)
val outputs: froms -> Locations.Zone.t

(** Extract the right part of a from result, ie. the zones on which the
    written zones depend. If [include_self] is true, and the from is
    of the form [x FROM y (and SELF)], [x] is added to the result;
    default value is [false]. *)
val inputs: ?include_self:bool -> froms -> Locations.Zone.t


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

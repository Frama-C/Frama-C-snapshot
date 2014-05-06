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

type tt = {
  over_inputs: Locations.Zone.t;
  over_inputs_if_termination: Locations.Zone.t;
  under_outputs_if_termination: Locations.Zone.t;
  over_outputs: Locations.Zone.t;
  over_outputs_if_termination: Locations.Zone.t;
}

include Datatype.S with type t = tt

val pretty_operational_inputs: t Pretty_utils.formatter
(** Pretty-print the fields [over_inputs_if_termination], [over_inputs] and
    [under_outputs_if_termination] *)

val pretty_outputs: t Pretty_utils.formatter
(** Pretty-print the fields [over_outputs] and [over_outputs_if_termination]. *)

val map: (Locations.Zone.t -> Locations.Zone.t) -> t -> t

val bottom: tt
val join: tt -> tt -> tt


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

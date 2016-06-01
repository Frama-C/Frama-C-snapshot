(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(*                                                                        *)
(**************************************************************************)

type t = {
  over_inputs: Locations.Zone.t;
  over_inputs_if_termination: Locations.Zone.t;
  under_outputs_if_termination: Locations.Zone.t;
  over_outputs: Locations.Zone.t;
  over_outputs_if_termination: Locations.Zone.t;
}

include Datatype.S with type t := t

val pretty_operational_inputs: t Pretty_utils.formatter
(** Pretty-print the fields [over_inputs_if_termination], [over_inputs] and
    [under_outputs_if_termination] *)

val pretty_outputs: t Pretty_utils.formatter
(** Pretty-print the fields [over_outputs] and [over_outputs_if_termination]. *)

val map: (Locations.Zone.t -> Locations.Zone.t) -> t -> t

val bottom: t
val join: t -> t -> t


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

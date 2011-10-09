(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

type tt =
    { over_inputs_if_termination :  Locations.Zone.t ;
      under_outputs_if_termination : Locations.Zone.t ;
      over_inputs : Locations.Zone.t }

open Locations

include Datatype.Make
(struct
  include Datatype.Serializable_undefined
  type t = tt
  let structural_descr =
    Structural_descr.t_record
      [| Locations.Zone.packed_descr;
         Locations.Zone.packed_descr;
         Locations.Zone.packed_descr |]
  let reprs =
    List.map
      (fun z ->
        { over_inputs_if_termination = z;
          under_outputs_if_termination = z;
          over_inputs = z })
      Locations.Zone.reprs
  let name = "Inout_type"
  let hash
      { over_inputs_if_termination = a;
        under_outputs_if_termination = b;
        over_inputs = c} =
    Zone.tag a + 17 * Zone.tag b + 587 * Zone.tag c
  let equal
      { over_inputs_if_termination = a;
        under_outputs_if_termination = b;
        over_inputs = c}
      { over_inputs_if_termination = a';
        under_outputs_if_termination = b';
        over_inputs = c'} =
    Zone.equal a a' && Zone.equal b b' && Zone.equal c c'
  let mem_project = Datatype.never_any_project
 end)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

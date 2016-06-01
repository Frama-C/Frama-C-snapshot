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

let ok = false

module type S = Abstract_domain.Internal
  with type value = Main_values.Interval.t
   and type location = Precise_locs.precise_location

module U = Unit_domain.Make (Main_values.Interval) (Main_locations.PLoc)

module Octagon = U
module Box = U
module Polka_Loose = U
module Polka_Strict = U
module Polka_Equalities = U

let dummy_key = Structure.Key_Domain.create_key "dummy_apron"
let octagon_key = dummy_key
let box_key = dummy_key
let polka_loose_key = dummy_key
let polka_strict_key = dummy_key
let polka_equalities_key = dummy_key


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)

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

(** Main memory locations of EVA: *)

(** Abstract locations built over Precise_locs. *)
module PLoc : Abstract_location.Internal
  with type value = Cvalue.V.t
   and type location = Precise_locs.precise_location

(** Key for precise locs. *)
val ploc_key : PLoc.location Abstract_location.key


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)

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

(** Per-function computation of widening hints. *)

open Cil_types

val getWidenHints: kernel_function -> stmt ->
  Base.Set.t * (Base.t -> Locations.Location_Bytes.generic_widen_hint)


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)

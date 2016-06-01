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

(** Creation of the initial state for Value. *)

val initial_state_not_lib_entry: unit -> Cvalue.Model.t
val initial_state_lib_entry: unit -> Cvalue.Model.t

val initialize_var_using_type:
  Cil_types.varinfo -> Cvalue.Model.t -> Cvalue.Model.t

(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)

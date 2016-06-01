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

(** Security slicing. *)

open Cil_types

val get_direct_component: stmt -> stmt list
val get_indirect_backward_component: stmt -> stmt list
val get_forward_component: stmt -> stmt list
val impact_analysis: Kernel_function.t -> stmt -> stmt list
(*
val slice: bool -> Project.t
*)
(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

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

open Cil_types
open Cvalue

val get_retres_vi: kernel_function -> varinfo
(** Fake varinfo used by Value to store the result of functions without
    bodies. *)

val create_alloced_return : Cil_types.typ -> Kernel_function.t -> Base.t

val returned_value: kernel_function -> Model.t -> V.t * Model.t


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)

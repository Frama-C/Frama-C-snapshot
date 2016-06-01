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

(** Slicing API. *)

(** No function is directly exported: they are registered in {!Db.Slicing}. *)

(**/**)

(** Debugging purpose only.

    API used by the tests of slicing (see tests/slicing/libSelect.ml). *)

module PrintSlice: sig
  val print_fct_stmts:
    Format.formatter ->
    Db.Slicing.Project.t * Kernel_function.t ->
    unit
end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

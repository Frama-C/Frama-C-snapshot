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

(** Journalize the API of a callgraph *)

module Make
  (C: sig
    val name: string
    val dump: unit -> unit
    val compute: unit -> unit
    type t
    val ty: t Type.t
    val get: unit -> t
  end):
sig
  val dump: unit -> unit
  val compute: unit -> unit
  val get: unit -> C.t
end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

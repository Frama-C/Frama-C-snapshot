(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(* $Id: cg.mli,v 1.7 2008/04/01 09:25:20 uid568 Exp $ *)

(** Syntactic callgraph *)

open Callgraph

val get: unit -> Callgraph.callgraph
val dump: unit -> unit

module CallNodeComparable: sig
  type m = Nothing | Service of int | JustMet of int
  type t = { node : callnode; mutable mark : m; mutable visited : bool }
  val compare: t -> t -> int
  val hash: t -> int
  val equal: t -> t -> bool
end

module CallNodeSet: Set.S with type elt = CallNodeComparable.t

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)

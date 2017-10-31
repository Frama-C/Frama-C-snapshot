(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(** Subgraph from a given vertex *)
module Make
  (G: sig
    (** Graph datastructure *)
    include Graph.Sig.G
    val create: ?size:int -> unit -> t
    val add_edge_e: t -> E.t -> unit
  end)
  (D: Datatype.S with type t = G.t  (** Graph datatype *))
  (Info: sig
    (** additional information *)
    val self: State.t
    val name: string (** name of the state *)
    val get: unit -> G.t
    val vertex: Kernel_function.t -> G.V.t
  end) :
sig
  val get: unit -> G.t
  val self: State.t
end

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

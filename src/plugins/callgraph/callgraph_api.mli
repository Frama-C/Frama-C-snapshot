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

(** Callgraph API *)

module type Graph = sig

  module G: Graph.Sig.G
  (** The underlying graph datastructure *)

  val compute: unit -> unit
  (** Compute the graph *)

  val get: unit -> G.t
  (** Get the graph from the AST. *)

  module Subgraph: sig val get: unit -> G.t end
  (** Subgraph of [get ()] wrt [Options.Roots.get ()] *)

  val dump: unit -> unit
  (** Dump the (possibly sub-)graph in the file of the corresponding command
      line argument. *)

  val is_computed: unit -> bool
  (** Is the graph already built? *)

  val self: State.t

end

(** Signature for a callgraph. Each edge is labeled by the callsite. Its source
    is the caller, while the destination is the callee. *)
module type S = Graph with type G.V.t = Kernel_function.t
                      and type G.E.label = Cil_types.stmt

(** Signature for a graph of services *)
module type Services = sig

  include Graph with type G.V.t = Kernel_function.t Service_graph.vertex
                and type G.E.label = Service_graph.edge

  val entry_point: unit -> G.V.t option
  val is_root: Kernel_function.t -> bool

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

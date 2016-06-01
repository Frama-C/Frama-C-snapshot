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

(** Callgraph API *)

module type Graph = sig

  module G: Graph.Sig.G
  (** The underlying graph datastructure *)

  val compute: unit -> unit
  (** Compute the graph *)

  val get: unit -> G.t
  (** Get the graph from the AST. *)

  val dump: unit -> unit
  (** Dump the graph in the file of the corresponding command line argument. *)

  val is_computed: unit -> bool
  (** Is the graph already built? *)

  val self: State.t

end

(** Signature for a callgraph. Each edge is labeled by the callsite. Its source
    is the caller, while the destination is the callee. *)
module type S = Graph with type G.V.t = Kernel_function.t
                      and type G.E.label = Cil_types.stmt

(** Signature for a graph of services *)
module type Services =
  Graph with type G.V.t = Kernel_function.t Service_graph.vertex
        and type G.E.label = Service_graph.edge

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

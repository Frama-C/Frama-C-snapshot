(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

(** Compute services from a callgraph. *)

val frama_c_display: bool -> unit
(** must be set to [false] before output the graph in dot format
    and must be set to [true] in order to display the graph in the Frama-C GUI.
    @since Oxygen-20120901 *)

type 'a vertex = private
    { node: 'a; mutable is_root: bool; mutable root: 'a vertex }

type edge = private Inter_services | Inter_functions | Both

(** Output signature for services. *)
module type S = sig

  type node
  type graph

  module Service_graph: sig
    include Graph.Sig.G with type V.t = node vertex and type E.label = edge
    module Datatype: Datatype.S with type t = t
  end

  val compute: graph -> Datatype.String.Set.t -> Service_graph.t
  val output_graph: out_channel -> Service_graph.t -> unit

  val entry_point: unit -> Service_graph.V.t option
  (** [compute] must be called before
      @since Carbon-20101201
      @modify Nitrogen-20111001 return an option type *)

  module TP: Graph.Graphviz.GraphWithDotAttrs
    with type t = Service_graph.t
    and type V.t = node vertex
    and type E.t = Service_graph.E.t
(** @since Beryllium-20090902 *)

end

(** Generic functor implementing the services algorithm according to a graph
    implementation. *)
module Make
  (G: sig
     type t
     module V: sig
       (** @modify Oxygen-20120901 require [compare] *)
       include Graph.Sig.COMPARABLE
       val id: t -> int
         (** assume [id >= 0] and unique for each vertices of the graph *)
       val name: t -> string
       val attributes: t -> Graph.Graphviz.DotAttributes.vertex list
       val entry_point: unit -> t option
     (** @modify Nitrogen-20111001 return an option*)
     end
     val iter_vertex : (V.t -> unit) -> t -> unit
     val iter_succ : (V.t -> unit) -> t -> V.t -> unit
     val iter_pred : (V.t -> unit) -> t -> V.t -> unit
     val fold_pred : (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
     val datatype_name: string
   end) :
  S with type node = G.V.t and type graph = G.t

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

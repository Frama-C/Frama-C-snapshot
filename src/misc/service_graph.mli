(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

(** Generic functor implementing the services algorithm according to a graph
    implementation. *)
module Make
  (G: sig
     type t
     module V: sig
       include Graph.Sig.HASHABLE
       val id: t -> int
         (** assume is >= 0 and unique for each vertices of the graph *)
       val name: t -> string
       val attributes: t -> Graph.Graphviz.DotAttributes.vertex list
       val entry_point: unit -> t option
     (** @modify Nitrogen-20111001 return an option*)
     end
     val iter_vertex : (V.t -> unit) -> t -> unit
     val iter_succ : (V.t -> unit) -> t -> V.t -> unit
     val iter_pred : (V.t -> unit) -> t -> V.t -> unit
     val fold_pred : (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
     val in_degree: t -> V.t -> int
     val datatype_name: string
   end) :
sig

  type vertex = private
      { node: G.V.t; mutable is_root: bool; mutable root: vertex }

  type edge = private Inter_services | Inter_functions | Both

  module CallG: sig
    include Graph.Sig.G with type V.t = vertex and type E.label = edge
    module Datatype: Datatype.S with type t = t
  end

  val compute: G.t -> Datatype.String.Set.t -> CallG.t
  val output_graph: out_channel -> CallG.t -> unit

  val entry_point: unit -> CallG.V.t option
  (** [compute] must be called before
      @since Carbon-20101201
      @modify Nitrogen-20111001 return an option type *)

  module TP: Graph.Graphviz.GraphWithDotAttrs
    with type t = CallG.t
    and type V.t = vertex
    and type E.t = CallG.E.t
(** @since Beryllium-20090902 *)

(*
(** Graph of services *)

  module SS: Set.S with type elt = G.V.t
  type service_vertex = private
  { service: int; mutable root: G.V.t; mutable nodes: SS.t }
(** @since Beryllium-20090901 *)

(** @since Beryllium-20090901 *)
  module SG : sig
  include Graph.Sig.G with type V.t = service_vertex
  type tt = t
  module Datatype: Project.Datatype.S with type t = tt
  end

  val compute_services: CallG.t -> SG.t
(** @since Beryllium-20090901 *)

  val output_services: out_channel -> SG.t -> unit
(** @since Beryllium-20090901 *)
 *)
end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

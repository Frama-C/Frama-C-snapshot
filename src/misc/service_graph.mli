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

module Make
  (G: sig
     type t
     module V: sig
       type t
       val id: t -> int (* assume is >= 0 and unique for each vertices of the graph *)
       val name: t -> string
       val attributes: t -> Graph.Graphviz.DotAttributes.vertex list
     end
     val iter_vertex: (V.t -> unit) -> t -> unit
     val callees: t -> V.t -> V.t list
     val callers: t -> V.t -> V.t list
     val name: string
   end) : sig

    type m = private Nothing | Service of int | JustMet of int
    type vertex = private { node : G.V.t; 
                            mutable mark : m; 
                            mutable visited : bool;
                            is_service : bool}

    module CallG: sig
      include Graph.Sig.I with type V.t = vertex
      type tt = t
      module Datatype: Project.Datatype.S with type t = tt
    end

    val output_graph: out_channel -> CallG.t -> unit

    val compute: G.t -> Cilutil.StringSet.t -> CallG.t
  end

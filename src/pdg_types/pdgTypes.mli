(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

(** This module defines the types that are used to store the PDG of a
    function.
    @plugin development guide *)

(** [Dpd] stands for 'dependence'. This object is used as a label on the edges
 * of the PDG. There are three kinds of dependencies :
 * - control dependency,
 * - address dependency,
 * - data dependency.
 * An edge can carry one or several kinds.
 * A bottom edge means that there are no relation.
 *)
module Dpd :
  sig
    type t

    type td = Ctrl | Addr | Data

    val make : ?a:bool -> ?d:bool -> ?c:bool -> unit -> t
    val top : t
    val bottom : t

    val is_addr : t -> bool
    val is_ctrl : t -> bool
    val is_data : t -> bool
    val adc_value : t -> bool * bool * bool
    val is_dpd : td -> t -> bool
    val is_bottom : t -> bool
    val is_included : t -> t -> bool

    val compare : t -> t -> int
    val equal : t -> t -> bool

    val combine : t -> t -> t
    val add : t -> td -> t
    val inter : t -> t -> t
    val intersect : t -> t -> bool
    val minus : t -> t -> t

    val pretty_td : Format.formatter -> td -> unit
    val pretty : Format.formatter -> t -> unit
  end

(** A node of the PDG : includes some information to know where it comes
    from. *)
module Node : sig
  include Datatype.S_with_collections
  val id : t -> int
  val elem_key : t -> PdgIndex.Key.t
  val stmt : t ->  Cil_types.stmt option
  (*val equivalent : t -> PdgIndex.Key.t -> bool*)
  val pretty_list : Format.formatter -> t list -> unit
  val pretty_with_part :
    Format.formatter -> (t * Locations.Zone.t option) -> unit
  val pretty_node: Format.formatter -> t -> unit
end

module NodeSet : Hptset.S with type elt = Node.t

(** Program dependence graph main part : the nodes of the graph represent
   computations, and the edges represent the dependencies between these
   computations. Only a few functions are exported, to build the graph
    in [pdg/build.ml]. Iterating over the PDG should be done using the
    functions in module [Pdg] below *)
module G : sig
  type t
  module E : sig
    type t
    type label
    val src : t -> Node.t
    val dst : t -> Node.t
    val label : t -> label
  end

  val create : unit -> t

  val add_elem : t -> PdgIndex.Key.t -> Node.t
  val add_dpd : 
    t -> Node.t -> Dpd.td -> Locations.Zone.t option -> Node.t -> unit
end

module NodeSetLattice : sig
  include Lattice_type.Lattice_Set with type O.elt=Node.t
  val default : Base.t -> Abstract_interp.Int.t -> Abstract_interp.Int.t -> t
  val defaultall : Base.t -> t
end

module LocInfo :
  Lmap_bitwise.Location_map_bitwise with type y = NodeSetLattice.t

(** a [data_state] object is associated with a program point
    and provides a mapping between a location and some nodes in the PDG
    that are used to compute the location value at that point. *)
type data_state =
  { loc_info : LocInfo.t ; under_outputs : Locations.Zone.t }

module Pdg : sig

  exception Top
    (** can be raised by most of the functions when called with a Top PDG.
        Top means that we were not abled to compute the PDG for this
        function. *)

  exception Bottom
    (** exception raised when requiring the PDG of a function that is never
        called. *)

  include Datatype.S

  (** @param name of the function associated with that PDG *)
  val top : Kernel_function.t -> t
  val bottom : Kernel_function.t -> t

  val is_top : t -> bool
  val is_bottom : t -> bool

  val get_kf : t -> Kernel_function.t

  val iter_nodes : (Node.t -> unit) -> t -> unit

  val fold_call_nodes :
    ('a -> Node.t -> 'a) -> 'a -> t -> Cil_types.stmt -> 'a

  val iter_direct_dpds : t -> (Node.t -> unit) -> Node.t -> unit
  val iter_direct_codpds : t -> (Node.t -> unit) -> Node.t -> unit

  (** a dependency to another node. The dependency can be restricted to a zone.
  * (None means no restriction ie. total dependency) *)
  type dpd_info = (Node.t * Locations.Zone.t option)

  val get_all_direct_dpds : t -> Node.t -> dpd_info list
  val get_x_direct_dpds : Dpd.td -> t -> Node.t -> dpd_info list

  val get_all_direct_codpds : t -> Node.t -> dpd_info list
  val get_x_direct_codpds : Dpd.td -> t -> Node.t -> dpd_info list

  val fold_direct_dpds : t ->
      ('a -> Dpd.t * Locations.Zone.t option -> Node.t -> 'a) ->
      'a -> Node.t -> 'a

  val fold_direct_codpds : t ->
      ('a -> Dpd.t * Locations.Zone.t option -> Node.t -> 'a) ->
      'a -> Node.t -> 'a

  val pretty_bw : ?bw:bool -> Format.formatter -> t -> unit
  val pretty_graph : ?bw:bool -> Format.formatter -> G.t -> unit

  type fi = (Node.t, unit) PdgIndex.FctIndex.t

  val get_index : t -> fi

  (** [make fundec graph states index] *)
  val make :
    Kernel_function.t -> G.t -> data_state Cil_datatype.Stmt.Hashtbl.t -> fi -> t
  val get_states : t -> data_state Cil_datatype.Stmt.Hashtbl.t

  (** build the PDG .dot file and put it in [filename].  *)
  val build_dot: string -> t -> unit

  module Printer : sig
    val iter_vertex : (Node.t -> unit) -> t -> unit
    val iter_edges_e : (G.E.t * bool -> unit) -> t -> unit
    val graph_attributes : t -> Graph.Graphviz.DotAttributes.graph list
    val default_vertex_attributes : t -> Graph.Graphviz.DotAttributes.vertex list
    val vertex_name : Node.t -> string
    val vertex_attributes : Node.t -> Graph.Graphviz.DotAttributes.vertex list
    val get_subgraph : Node.t -> Graph.Graphviz.DotAttributes.subgraph option
    val default_edge_attributes : 'a -> Graph.Graphviz.DotAttributes.edge list
    val edge_attributes : G.E.t * bool -> Graph.Graphviz.DotAttributes.edge list
  end

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

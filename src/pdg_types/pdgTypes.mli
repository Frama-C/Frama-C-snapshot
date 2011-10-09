(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
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
    val minus : t -> t -> t

    val pretty_td : Format.formatter -> td -> unit
    val pretty : Format.formatter -> t -> unit
  end

(** A node of the PDG : includes some information to know where it comes
    from. *)
module Node : sig
  include Datatype.S
  val elem_id : t -> int
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
 * computations, and the edges represent the dependencies between these
 * computations. *)
module G : sig
(*  include Datatype.S*)
  type t
  module V : sig type t = Node.t end
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

  (* val replace_dpd : t -> E.t -> Dpd.t -> unit *)
  (* val find_dpd : t -> Node.t -> Node.t -> E.t * Dpd.t *)

  val succ : t -> Node.t -> Node.t list
  val pred : t -> Node.t -> Node.t list

  val iter_vertex : (Node.t -> unit) -> t -> unit
  val iter_edges_e : (E.t -> unit) -> t -> unit
  val iter_succ_e : (E.t -> unit) -> t -> Node.t -> unit
  val fold_succ_e : (E.t -> 'a -> 'a) -> t -> Node.t -> 'a -> 'a
  val fold_succ : (Node.t -> 'a -> 'a) -> t -> Node.t -> 'a -> 'a
  val iter_pred_e : (E.t -> unit) -> t -> Node.t -> unit
  val fold_pred : (Node.t -> 'a -> 'a) -> t -> Node.t -> 'a -> 'a
    
  val edge_dpd : E.t -> Dpd.t * Locations.Zone.t option
  val pretty_edge_label : Format.formatter -> E.label -> unit
end

module NodeSetLattice : sig
  include Abstract_interp.Lattice_Set with type O.elt=Node.t
  val default : Base.t -> Abstract_interp.Int.t -> Abstract_interp.Int.t -> t
  val defaultall : Base.t -> t
end

module LocInfo :
  Lmap_bitwise.Location_map_bitwise with type y = NodeSetLattice.t

(** a [DataState] object is associated with a program point
 * and provides a mapping between a location and some nodes in the PDG
 * that are used to compute the location value at that point. *)
type t_data_state =
  { loc_info : LocInfo.t ; under_outputs : Locations.Zone.t }

module Pdg : sig

  exception Top
    (** can be raised by most of the functions when called with a Top PDG.
        Top means that we were not abled to compute the PDG for this
        function. *)

  exception Bottom

  include Datatype.S
  (** @plugin development guide *)

  (** @param name of the function associated with that PDG *)
  val top : Kernel_function.t -> t
  val bottom : Kernel_function.t -> t

  val is_top : t -> bool
  val is_bottom : t -> bool

  val get_kf : t -> Kernel_function.t

  val iter_nodes : (Node.t -> unit) -> t -> unit

  val fold_call_nodes :
    ('a -> Node.t -> 'a) -> 'a -> t -> Cil_types.stmt -> 'a

  (** a dependency to another node. The dependency can be restricted to a zone.
  * (None means no restriction ie. total dependency) *)
  type dpd_info = (Node.t * Locations.Zone.t option)

  val get_all_direct_dpds : t -> Node.t -> dpd_info list
  val get_x_direct_dpds : Dpd.td -> t -> Node.t -> dpd_info list

  val get_all_direct_codpds : t -> Node.t -> dpd_info list
  val get_x_direct_codpds : Dpd.td -> t -> Node.t -> dpd_info list

  val pretty_bw : ?bw:bool -> Format.formatter -> t -> unit
  val pretty_graph : ?bw:bool -> Format.formatter -> G.t -> unit

  type t_fi = (Node.t, unit) PdgIndex.FctIndex.t

  val get_index : t -> t_fi

  (** [make fundec graph states index] *)
  val make :
    Kernel_function.t -> G.t -> t_data_state Inthash.t -> t_fi -> t
  val get_states : t -> t_data_state Inthash.t

  val build_dot: string -> t -> unit

  module Printer : sig
    val iter_vertex : (G.V.t -> unit) -> t -> unit
    val iter_edges_e : (G.E.t * bool -> unit) -> t -> unit
    val graph_attributes : t -> Graph.Graphviz.DotAttributes.graph list
    val default_vertex_attributes : t -> Graph.Graphviz.DotAttributes.vertex list
    val vertex_name : G.V.t -> string
    val vertex_attributes : G.V.t -> Graph.Graphviz.DotAttributes.vertex list
    val get_subgraph : G.V.t -> Graph.Graphviz.DotAttributes.subgraph option
    val default_edge_attributes : 'a -> Graph.Graphviz.DotAttributes.edge list
    val edge_attributes : G.E.t * bool -> Graph.Graphviz.DotAttributes.edge list
  end

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

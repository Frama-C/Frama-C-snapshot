(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA   (Commissariat à l'Énergie Atomique)                           *)
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

(* $Id: pdgTypes.mli,v 1.35 2008/04/11 09:06:05 uid530 Exp $*)

(** This module defines the types that are used to store the PDG of a
    function. *)

exception Pdg_Internal_Error of string

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
    val make_adc : bool -> bool -> bool -> t
    val make : td -> t
    val make_addr : t
    val make_ctrl : t
    val make_data : t
    val bottom : t
    val top : t
    val default : t
    val adc_value : t -> bool * bool * bool
    val is_addr : t -> bool
    val is_ctrl : t -> bool
    val is_data : t -> bool
    val is_dpd : td -> t -> bool
    val is_bottom : t -> bool
    val compare : t -> t -> int
    val is_included : t -> t -> bool
    val combine : t -> t -> t
    val add : t -> td -> t
    val inter : t -> t -> t
    val minus : t -> t -> t
    val pretty : Format.formatter -> t -> unit
  end

(** A node of the PDG : includes some information to know where it comes
    from. *) 
module Node :
  sig
    type t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val hash : t -> int
    val elem_id : t -> int
    val elem_key : t -> PdgIndex.Key.t
    val stmt : t ->  Cil_types.stmt option
    val equivalent : t -> PdgIndex.Key.t -> bool
    val pretty : Format.formatter -> t -> unit
    val pretty_list : Format.formatter -> t list -> unit
    module Datatype: Project.Datatype.OUTPUT with type t = t
  end

module NodeSet : sig
  include Set.S with type elt = Node.t
  val add_list : ?set:t -> Node.t list -> t
end

(** Program dependence graph main part : the nodes of the graph represent
 * computations, and the edges represent the dependencies between these
 * computations. *)
module G :
  sig
    type t
    module V : sig type t = Node.t end
    module E :
      sig
        type t 
        val src : t -> Node.t
        val dst : t -> Node.t
        val label : t -> Dpd.t
      end
    type edge = E.t

    val create : unit -> t

    val add_elem : t -> PdgIndex.Key.t -> Node.t
    val add_dpd : t -> Node.t -> Dpd.td -> Node.t -> unit
    (* val replace_dpd : t -> edge -> Dpd.t -> unit *)
    val find_dpd : t -> Node.t -> Node.t -> edge * Dpd.t

    val succ : t -> Node.t -> Node.t list
    val pred : t -> Node.t -> Node.t list

    val iter_vertex : (Node.t -> unit) -> t -> unit
    val iter_edges_e : (edge -> unit) -> t -> unit
    val iter_succ_e : (edge -> unit) -> t -> Node.t -> unit
    val fold_succ_e : (edge -> 'a -> 'a) -> t -> Node.t -> 'a -> 'a
    val fold_succ : (Node.t -> 'a -> 'a) -> t -> Node.t -> 'a -> 'a
    val iter_pred_e : (edge -> unit) -> t -> Node.t -> unit
    val fold_pred : (Node.t -> 'a -> 'a) -> t -> Node.t -> 'a -> 'a
  end

module NodeSetLattice : sig
  include Abstract_interp.Lattice_Set with type O.elt=Node.t
  exception NoNodeForZone of Locations.Zone.t
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

module Pdg :
  sig
    (** can be raised by most of the functions when called with a Top PDG.
     * Top means that we were not abled to compute the PDG for this fonction. *)
    exception Top

    exception Bottom

    type t
    module Datatype : Project.Datatype.OUTPUT with type t = t

    (** @param name of the function associated with that PDG *)
    val top : string -> t
    val bottom : string -> t

    val is_top : t -> bool
    val is_bottom : t -> bool

    val get_var_fct : t -> Cil_types.varinfo
    val get_fct_name : t -> string

    val iter_nodes : (Node.t -> unit) -> t -> unit
    val iter_dynamic_dpds : (G.E.t -> unit) -> t -> unit

    val fold_call_nodes : 
        ('a -> Node.t -> 'a) -> 'a -> t -> Cil_types.stmt -> 'a

    val get_all_direct_dpds : t -> Node.t -> Node.t list
    val get_x_direct_dpds : Dpd.td -> t -> Node.t -> Node.t list

    val get_all_direct_codpds : t -> Node.t -> Node.t list
    val get_x_direct_codpds : Dpd.td -> t -> Node.t -> Node.t list

    val add_dynamic_dpds : t -> 
      ?data:Node.t list -> ?addr: Node.t list -> ?ctrl:Node.t list -> 
      Node.t -> unit
    val clear_dynamic_dpds : t -> unit
  end

module InternalPdg :
  sig
    type t = Pdg.t
    type t_index = (Node.t, unit) PdgIndex.FctIndex.t
    val get_index : t -> t_index

    (** [make fundec graph states index] *)
    val make :
      Cil_types.varinfo -> G.t -> t_data_state Inthash.t -> t_index -> t

    val get_states : t -> t_data_state Inthash.t 

    (** Be careful: this function shouldn't be used because it doesn't take
	the dynamic dependencies into account. *)
    val get_graph : t -> G.t
  end

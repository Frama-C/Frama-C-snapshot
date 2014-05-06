(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(* -------------------------------------------------------------------------- *)
(* --- Control Flow Graphs                                                --- *)
(* -------------------------------------------------------------------------- *)

module type Transition =
sig
  type 'a transition
  val empty : 'a transition
  val iter : ('a -> unit) -> 'a transition -> unit
end

module type Cfg = 
sig

  type cfg
  type node

  module T : Transition

  type transition = node T.transition

  val nil : node
  val is_nil : node -> bool

  val create : unit -> cfg
  val node : cfg -> node
  val size : cfg -> int
  val set : cfg -> node -> transition -> unit
  val add : cfg -> transition -> node

  val next : cfg -> node -> transition
  val succ : cfg -> node -> node list (** Reversed with repetitions *)
  val pred : cfg -> node -> node list (** Reversed With repetitions *)
  val iter_succ : cfg -> (node -> unit) -> node -> unit (** Iterate over [succ] *)
  val iter_pred : cfg -> (node -> unit) -> node -> unit (** Iterate over [pred] *)
  val iter : (node -> transition -> unit) -> cfg -> unit

  val id : node -> int
  val nid : int -> node

  type marks (** Markers for CFG exploration. *)

  val marks : cfg -> marks 
  (** Create new markers *)

  val once : marks -> node -> bool 
  (** Return [true] only if the node is not yet marked, then mark it. *)

  type dotter
  val pp_node : dotter -> node -> ('a,Format.formatter,unit) format -> 'a
  (** Print the attributes of the node in the [.dot] file. 
      Typically : [G.pp_node n "label=\"Root\", color=red" ;] *)
    
  val pp_edge : dotter -> node -> node -> ('a,Format.formatter,unit) format -> 'a
  (** Print the attributes of the edge in the [.dot] file. 
      Typically : [G.pp_node n "style=dotted" ;] *)
    
  type pp_cfg = dotter -> node -> transition -> unit

  val dot : cfg -> pp_cfg -> string -> unit
  (** Dump the graph into provided file.  If file extension is
      different from ".dot", then command "dot -T<format>" is executed on the
      generated output to produce the expected format. *)

end

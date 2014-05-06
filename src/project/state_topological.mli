(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for ocaml                         *)
(*  Copyright (C) 2004-2012                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliâtre and Julien Signoles        *)
(*                                                                        *)
(*  This library is free software; you can redistribute it and/or         *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, as published by the Free Software Foundation.    *)
(*                                                                        *)
(*  This library is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU Library General Public License version 2.1 for more       *)
(*  details (enclosed in the file licences/LGPLv2.1).                     *)
(*                                                                        *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux        *)
(*                        énergies alternatives).                         *)
(*                                                                        *)
(**************************************************************************)

(** Topological ordering over states.

    This functor provides functions which allow iterating over a <b>state</b>
    graph in topological order.

    That is the module [Topological] from OcamlGraph, but it takes into account
    state clusters. *)

(** Minimal graph signature to provide.
    Sub-signature of {!Sig.G}. *)
module type G = sig
  type t
  val iter_vertex : (State.t -> unit) -> t -> unit
  val iter_succ : (State.t -> unit) -> t -> State.t -> unit
  val in_degree : t -> State.t -> int
end

(** Functor providing topological iterators over a graph. *)
module Make(G: G) : sig

  val fold : (State.t -> 'a -> 'a) -> G.t -> 'a -> 'a
    (** [fold action g seed] allows iterating over the graph [g]
      in topological order. [action node accu] is called repeatedly,
      where [node] is the node being visited, and [accu] is the result of
      the [action]'s previous invocation, if any, and [seed] otherwise.
      If [g] contains cycles, the order is unspecified inside the cycles and
      every node in the cycles will be presented exactly once. *)

  val iter : (State.t -> unit) -> G.t -> unit
    (** [iter action] calls [action node] repeatedly. Nodes are (again)
        presented to [action] in topological order.
        The order is the same as for [fold]. *)

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

open Sigs
open Cil_types
open Lang

(** {2 Control Flow Graphs}

    The semantics of a {i cfg} is a collection of execution traces.  We
    introduce the notion of {i node} which represent a program point.
    In case of loop unrolling of function inlining, a node
    generalize the notion of [stmt] : two distinct nodes may refer to the same
    instruction at different memory states.

    We introduce an interpretation I as a partial mapping from nodes [n:node] to
    memory states [s:M.sigma], denoted I(n). The notation I(n) seen as a predicate
    indicates if `n` is in the partial mapping.

    Given a cfg, a node can be associated to {i assumptions} to filter
    interpretation against the memory state at this point.

    Effects and predicates are defined {i wrt} some fresh memory states, and can
    be duplicated at different nodes, each instance being mapped to different
    memory states.

*)

type mode = [
  | `Tree
  | `Bool_Backward
  | `Bool_Forward
]

module type Cfg =
sig

  (** The memory model used. *)
  module S : Sigma

  (** Program point along a trace. *)
  module Node : sig
    type t
    module Map : Qed.Idxmap.S with type key = t
    module Set : Qed.Idxset.S with type elt = t
    module Hashtbl : Hashtbl.S with type key = t
    val pp: Format.formatter -> t -> unit
    val create: unit -> t
    val equal: t -> t -> bool
  end

  type node = Node.t

  (** fresh node *)
  val node : unit -> node

  (** {2 Relocatable Formulae}
      Can be created once with fresh environment, and used several
      times on different memory states. *)

  (** Relocatable condition *)
  module C :
  sig
    type t

    val equal : t -> t -> bool

    (** Bundle an equation with the sigma sequence that created it. *)
    val create : S.t -> F.pred -> t
    val get : t -> F.pred
    val reads : t -> S.domain
    val relocate : S.t -> t -> t
  end

  (** Relocatable predicate *)
  module P :
  sig
    type t
    val pretty : Format.formatter -> t -> unit

    (** Bundle an equation with the sigma sequence that created it.
        [| create m p |] = [| p |]
    *)
    val create : S.t Node.Map.t -> F.pred -> t
    val get: t -> F.pred
    val reads : t -> S.domain Node.Map.t
    val nodes : t -> Node.Set.t
    val relocate : S.t Node.Map.t -> t -> t
    (** [| relocate m' (create m p) |] = [| p{ } |] *)

    val to_condition: t -> (C.t * Node.t option) option
  end

  (** Relocatable term *)
  module T :
  sig
    type t
    val pretty : Format.formatter -> t -> unit

    (** Bundle a term with the sigma sequence that created it. *)
    val create : S.t Node.Map.t -> F.term -> t
    val get: t -> F.term
    val reads : t -> S.domain Node.Map.t
    val relocate : S.t Node.Map.t -> t -> t
    val init  : Node.Set.t -> (S.t Node.Map.t -> F.term) -> t
    val init' : Node.t -> (S.t -> F.term) -> t
  end


  (** Relocatable effect (a predicate that depend on two states). *)
  module E : sig
    type t
    val pretty: Format.formatter -> t -> unit

    (** Bundle an equation with the sigma sequence that created it *)
    val create : S.t sequence -> F.pred -> t
    val get : t -> F.pred
    val reads : t -> S.domain
    val writes : t -> S.domain
    (** as defined by S.writes *)
    val relocate : S.t sequence -> t -> t
  end

  type cfg (** Structured collection of traces. *)

  val dump_env: name:string -> cfg -> unit
  val output_dot: out_channel -> ?checks:P.t Bag.t -> cfg -> unit

  val nop : cfg
  (** Structurally, [nop] is an empty execution trace.
      Hence, [nop] actually denotes all possible execution traces.
      This is the neutral element of [concat].

      Formally: all interpretations I verify nop: [| nop |]_I
  *)

  val add_tmpnode: node -> cfg
  (** Set a node as temporary. Information about its path predicate or
      sigma can be discarded during compilation *)

  val concat : cfg -> cfg -> cfg
  (** The concatenation is the intersection of all
      possible collection of traces from each cfg.

      [concat] is associative, commutative,
      has [nop] as neutral element.

      Formally: [| concat g1 g2 |]_I iff [| g1 |]_I and [| g2 |]_I
  *)

  val meta : ?stmt:stmt -> ?descr:string -> node -> cfg
  (** Attach meta informations to a node.
      Formally, it is equivalent to [nop]. *)

  val goto : node -> node -> cfg
  (** Represents all execution traces [T] such that, if [T] contains node [a],
      [T] also contains node [b] and memory states at [a] and [b] are equal.

      Formally: [| goto a b |]_I iff (I(a) iff I(b))
  *)

  val branch : node -> C.t -> node -> node -> cfg
  (** Structurally corresponds to an if-then-else control-flow.
      The predicate [P] shall reads only memory state at label [Here].

      Formally: [| branch n P a b |]_I iff (   (I(n) iff (I(a) \/ I(b)))
                                            /\ (I(n) implies (if P(I(n)) then I(a) else I(b)))  )
  *)

  val guard : node -> C.t -> node -> cfg
  (** Structurally corresponds to an assume control-flow.
      The predicate [P] shall reads only memory state at label [Here].

      Formally: [| guard n P a |]_I iff (   (I(n) iff I(a))
                                            /\ (I(n) implies [| P |]_I  ) )
  *)

  val guard' : node -> C.t -> node -> cfg
  (** Same than guard but the condition is negated *)

  val either : node -> node list -> cfg
  (** Structurally corresponds to an arbitrary choice among the different
      possible executions.

      [either] is associative and commutative. [either a []] is
      very special, since it denotes a cfg with {i no} trace. Technically,
      it is equivalent to attaching an [assert \false] annotation to node [a].

      Formally: [| either n [a_1;...;a_n] } |]_I iff ( I(n) iff (I(a_1) \/ ... I(a_n)))
  *)


  val implies : node -> (C.t * node) list -> cfg
  (**
     implies is the dual of either. Instead of being a non-deterministic choice,
     it takes the choices that verify its predicate.

      Formally: [| either n [P_1,a_1;...;P_n,a_n] } |]_I iff ( I(n) iff (I(a_1) \/ ... I(a_n))
                                                              /\  I(n) implies [| P_k |]_I implies I(a_k)
  *)


  val effect : node -> E.t -> node -> cfg
  (** Represents all execution trace [T] such that, if [T] contains node [a],
      then [T] also contains [b] with the given effect on corresponding
      memory states.

      Formally: [| effect a e b |]_I iff (( I(a) iff I(b) ) /\ [| e |]_I )
  *)

  val assume : P.t -> cfg
  (** Represents execution traces [T] such that, if [T] contains
      every node points in the label-map, then the condition holds over the
      corresponding memory states. If the node-map is empty,
      the condition must hold over all possible execution path.

      Formally: [| assume P |]_I iff [| P |]_I
  *)

  val havoc : node -> effects:node sequence -> node -> cfg
  (** Inserts an assigns effect between nodes [a] and [b], correspondings
      to all the written memory chunks accessible in execution paths delimited
      by the [effects] sequence of nodes.

      Formally: [| havoc a s b |]_I is verified if there is no path between s.pre and s.path,
      otherwise if (I(a) iff I(b) and if I(a) is defined then I(a) and I(b) are equal
      for all the chunks that are not in the written domain of an effect that can be found
      between [s.pre] to [s.post].

      Note: the effects are collected in the {i final} control-flow,
      when {!compile} is invoked. The portion of the sub-graph in the sequence
      shall be concatenated to the [cfg] before compiling-it, otherwize it would be
      considered empty and [havoc] would be a nop (no connection between a and b).
  *)

  (** {2 Path-Predicates}

      The compilation of cfg control-flow into path predicate
      is performed by allocating fresh environments with optimized variable
      allocation. Only the relevant path between the nodes
      is extracted. Other paths in the cfg are pruned out.
  *)

  (** Extract the nodes that are between the start node and the final
      nodes and returns how to observe a collection of states indexed
      by nodes. The returned maps gives, for each reachable node, a
      predicate representing paths that reach the node and the memory
      state at this node.

      Nodes absent from the map are unreachable. Whenever possible,
      predicate [F.ptrue] is returned for inconditionally accessible
      nodes.

      ~name: identifier used for debugging

  *)

  val compile : ?name:string -> ?mode:mode -> node -> Node.Set.t -> S.domain Node.Map.t ->
    cfg -> F.pred Node.Map.t * S.t Node.Map.t * Conditions.sequence

end

module Cfg(S:Sigma) : Cfg with module S = S

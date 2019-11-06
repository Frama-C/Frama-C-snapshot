(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

(** Traces domain *)
open Cil_types

module Node : Datatype.S

module GraphShape : sig type 'value t end

type node = Node.t

type transition =
  | Assign of kinstr * lval * typ * exp
  | Assume of stmt * exp * bool
  | EnterScope of kernel_function * varinfo list
  | LeaveScope of kernel_function * varinfo list
  (** For call of functions without definition *)
  | CallDeclared of kernel_function * exp list * lval option
  | Loop of stmt * node (** start *) * edge list GraphShape.t
  | Msg of string

and edge = {
  edge_trans : transition;
  edge_dst : node;
}

module Edge : Datatype.S with type t = edge

module Graph : sig
  include Hptmap_sig.S with type key = Node.t
                        and type v = edge list
                        and type 'a shape = 'a GraphShape.t

  val join : t -> t -> t
end

(** stack of open loops *)
type loops =
  | Base of Node.t * Graph.t (* current last *)
  | OpenLoop of Cil_types.stmt * Node.t (* start node *) * Graph.t (* last iteration *) * Node.t (** current *) * Graph.t * loops
  | UnrollLoop of Cil_types.stmt * loops

module Loops : sig
  type t = loops
end

type state

val start: state -> Node.t
val current: state -> loops
val globals: state -> Cil_types.varinfo list
val entry_formals: state -> Cil_types.varinfo list

module D: Abstract_domain.Leaf
  with type value = Cvalue.V.t
   and type location = Precise_locs.precise_location
   and type state = state

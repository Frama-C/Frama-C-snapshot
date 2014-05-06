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

module type S = sig
  type param
  type result
  val extend: (param -> result) -> unit
  val extend_once: (param -> result) -> unit
  val apply: param -> result
  val is_empty: unit -> bool
  val clear: unit -> unit
  val length: unit -> int
end

module type Comparable = sig
  type t
  val equal: t -> t -> bool
  val hash: t -> int
  val compare: t -> t -> int
end

module type S_ordered = sig
    include S
    type key
    type id (** identifier of the hook *)
    val register_key: key -> id
    val extend: id -> (param->result)->unit
    val extend_once: id -> (param->result) -> unit
    val add_dependency: id -> id -> unit
end

module type Iter_hook = S with type result = unit

let add_once v queue =
  let already = Queue.fold (fun b v' -> b || v' == v) false queue in
  if not already then Queue.add v queue

module Build(P:sig type t end) = struct
  type param = P.t
  type result = unit
  let hooks = Queue.create ()
  let extend f = Queue.add f hooks
  let extend_once f = add_once f hooks

  let apply arg = Queue.iter (fun f -> f arg) hooks
    (* [JS 06 October 2008] the following code iter in reverse order without
       changing the order of the queue itself.

       let list = ref [] in
       Queue.iter (fun f -> list := f :: !list) hooks;
       List.iter (fun f -> f arg) !list *)

  let is_empty () = Queue.is_empty hooks
  let clear () = Queue.clear hooks
  let length () = Queue.length hooks
end

module Fold(P:sig type t end) = struct
  type param = P.t
  type result = P.t
  let hooks = Queue.create ()
  let extend f = Queue.add f hooks
  let extend_once f = add_once f hooks
  let apply arg = Queue.fold (fun arg f -> f arg) arg hooks
  let is_empty () = Queue.is_empty hooks
  let clear () = Queue.clear hooks
  let length () = Queue.length hooks
end

module Make(X:sig end) = Build(struct type t = unit end)

module Make_graph
  (P: sig module Id:Comparable type param type result end)
  =
struct
  type key = P.Id.t
  type param = P.param
  type result = P.result

  module Nodes = struct
    type t = key * (param -> result) Queue.t
    let equal (id1,_) (id2,_) = P.Id.equal id1 id2
    let hash (id,_) = P.Id.hash id
    let compare (id1,_) (id2,_) = P.Id.compare id1 id2
  end

  module Hooks = Graph.Imperative.Digraph.Concrete(Nodes)

  type id = Hooks.V.t

  let hooks = Hooks.create ()

  (* No find in OCamlgraph API... *)
  let find_vertex v1 =
    let module F = struct exception Found of Nodes.t end in
    try
      Hooks.iter_vertex
        (fun v2 -> if Nodes.equal v1 v2 then raise (F.Found v2)) hooks;
      raise Not_found
    with F.Found v -> v

  let register_key k =
    let empty_node = k, Queue.create() in
    try find_vertex empty_node
    with Not_found -> Hooks.add_vertex hooks empty_node; empty_node

  module Apply = Graph.Topological.Make(Hooks)

  let extend (_,q) f = Queue.add f q

  let extend_once (_,q) f = add_once f q

  let add_dependency v1 v2 =
    Hooks.add_edge hooks v1 v2

  let empty_nodes () =
    let module F = struct exception Full end in
    let empty_node (_,q) = if not (Queue.is_empty q) then raise F.Full in
    try Hooks.iter_vertex empty_node hooks; true with F.Full -> false

  let is_empty () = Hooks.is_empty hooks || empty_nodes ()

  let clear () = Hooks.clear hooks

  let length () = 
    Hooks.fold_vertex (fun (_,q) l -> Queue.length q + l) hooks 0
end

module Build_ordered (P: sig module Id:Comparable type t end): 
  S_ordered with type key = P.Id.t and type param = P.t and type result = unit =
struct
  include Make_graph(
    struct module Id = P.Id type param = P.t type result = unit end)

  let apply v =
    let apply_queue (_,q) = Queue.iter (fun f -> f v) q in
    Apply.iter apply_queue hooks  
end

module Make_ordered(P: sig module Id:Comparable end) =
  Build_ordered(struct include P type t = unit end)

module Fold_ordered(P: sig module Id:Comparable type t end) =
struct
  include Make_graph(
    struct module Id = P.Id type param = P.t type result = P.t end)

  let apply v =
    let apply_queue (_,q) v = Queue.fold (fun v f -> f v) v q in
    Apply.fold apply_queue hooks v
end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

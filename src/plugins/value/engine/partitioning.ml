(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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

open Eval

module type StateSet = sig
  type state
  type t

  val empty: t
  val is_empty: t -> bool
  val singleton: state -> t
  val singleton': state or_bottom -> t
  val uncheck_add: state -> t -> t
  val add: state -> t -> t
  val add': state or_bottom -> t -> t

  val length: t -> int

  val merge: into:t -> t -> t * bool
  val join: ?into:state or_bottom -> t -> state or_bottom
  val fold: (state -> 'a -> 'a) -> t -> 'a -> 'a
  val iter: (state -> unit) -> t -> unit
  val map: (state -> state) -> t -> t

  val reorder: t -> t
  val of_list: state list -> t
  val to_list: t -> state list

  val pretty : Format.formatter -> t -> unit
end

module type Partition = sig
  type state
  type state_set
  type t

  val empty: unit -> t
  val fold: (state -> 'a -> 'a) -> t -> 'a -> 'a
  val merge_set_return_new: state_set -> t -> state_set
  val join: t -> state or_bottom
  val to_set: t -> state_set
  val to_list: t -> state list
  val pretty : Format.formatter -> t -> unit
end


(** Set of states, propagated through the edges by the dataflow analysis. *)
module Make_Set (Domain : Abstract_domain.S) = struct

  type state = Domain.t
  type t = Domain.t list

  let empty = []
  let is_empty = function [] -> true | _ -> false

  let singleton s = [s]
  let singleton' s = match s with `Bottom -> [] | `Value s -> [s]
  let uncheck_add s states = s :: states

  let length = List.length

  let join ?(into=`Bottom) states =
    List.fold_left
      (fun acc v -> Bottom.join Domain.join acc (`Value v))
      into states

  let fold f states acc = List.fold_left (fun acc s -> f s acc) acc states
  let iter = List.iter
  let map = List.map

  let of_list l = l
  let to_list l = l

  exception Unchanged
  let add_exn v s =
    if (List.exists (fun e -> Domain.is_included v e) s)
    then raise Unchanged;
    v :: s

  let add v s =
    try add_exn v s
    with Unchanged -> s

  let add' v s = match v with
    | `Bottom  -> s
    | `Value v -> add v s

  let merge ~into set =
    let f e (acc, unchanged) =
      try  add_exn e acc, false
      with Unchanged -> acc, unchanged
    in
    fold f set (into, true)

  let reorder = List.rev

  let pretty fmt state =
    iter (fun s -> Format.fprintf fmt "set contains %a@\n" Domain.pretty s) state
end


(** Partition of the abstract states, computed for each node by the
    dataflow analysis. *)
module Make_Partition
    (Domain : Abstract_domain.External)
    (States : StateSet with type state = Domain.t)
= struct

  type state = Domain.t
  type state_set = States.t
  module Index = Hashtbl.Make (Cvalue_domain.Subpart)

  type t = {
    mutable states : Domain.t Index.t;             (* Indexed states. *)
    mutable prefix : Cvalue_domain.prefix option;  (* Prefix for the index. *)
    mutable others : Domain.t list                 (* States not indexed. *)
  }

  let sentinel = Index.create 1
  let empty () = { states = sentinel ; prefix = None ; others = [] }

  let fold f {states; others} acc =
    let acc = Index.fold (fun _k s acc -> f s acc) states acc in
    List.fold_left (fun acc s -> f s acc) acc others

  (* Optimizations relying on specific features of the cvalue domain. *)

  let distinct_subpart = match Domain.get Cvalue_domain.key with
    | None -> fun _ _ -> None
    | Some get ->
      fun s1 s2 -> Cvalue_domain.distinct_subpart (get s1) (get s2)

  let find_subpart = match Domain.get Cvalue_domain.key with
    | None -> fun _ _ -> None
    | Some get ->
      fun state prefix -> Cvalue_domain.find_subpart (get state) prefix

  let add state partition =
    let {states; prefix; others} = partition in
    match prefix with
    | None -> begin match others with
        | []        -> partition.others <- [state]; true
        | s :: tail ->
          if List.exists (fun s -> Domain.is_included state s) others
          then false
          else match distinct_subpart state s with
            | None -> partition.others <- state :: others; true
            | Some (prefix, part1, part2) ->
              let states = Index.create 13 in
              Index.add states part1 state;
              Index.add states part2 s;
              let others =
                List.fold_left
                  (fun acc s -> match find_subpart s prefix with
                     | None -> s :: acc
                     | Some part -> Index.add states part s; acc)
                  [] tail
              in
              partition.states <- states;
              partition.prefix <- Some prefix;
              partition.others <- others;
              true
      end
    | Some prefix ->
      match find_subpart state prefix with
      | None ->
        if List.exists (fun s -> Domain.is_included state s) others
        then false
        else (partition.others <- state :: others; true)
      | Some prefix ->
        let candidates = Index.find_all states prefix in
        if List.exists (fun s -> Domain.is_included state s) candidates
        then false
        else (Index.add states prefix state; true)

  let merge_set_return_new states partition =
    let f state acc =
      let added = add state partition in
      if added then States.uncheck_add state acc else acc
    in
    States.fold f states States.empty

  let join partition =
    fold (fun v acc -> Bottom.join Domain.join (`Value v) acc) partition `Bottom

  let to_list p = Index.fold (fun _k v a -> v :: a) p.states p.others

  let to_set partition = States.of_list (to_list partition)

  let iter f { states; others } =
    Index.iter (fun _k v -> f v) states;
    List.iter f others

  let pretty fmt s =
    iter
      (fun state ->
         Format.fprintf fmt "set contains %a@\n"
           Domain.pretty state)
      s
end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)

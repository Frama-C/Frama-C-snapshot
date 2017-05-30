(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

module type Domain = sig
  include Abstract_domain.Lattice
  include Datatype.S_with_collections with type t = state
  include Abstract_domain.Interface with type t := state
end

module type S = sig
  type state
  type state_set
  type t

  val empty: unit -> t
  val merge_set_return_new: state_set -> t -> state_set
  val join: t -> state or_bottom
  val to_set: t -> state_set
  val to_list: t -> state list
  val pretty : Format.formatter -> t -> unit
end


(** Partition of the abstract states, computed for each node by the
    dataflow analysis. *)
module Make
    (Domain : Domain)
    (States : Powerset.S with type state = Domain.t)
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

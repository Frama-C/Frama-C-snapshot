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
end

module type S = sig
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
  val map_or_bottom: (state -> state or_bottom) -> t -> t

  val reorder: t -> t
  val of_list: state list -> t
  val to_list: t -> state list

  val pretty : Format.formatter -> t -> unit
end


(** Set of states, propagated through the edges by the dataflow analysis. *)
module Make (Domain : Domain) = struct

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
  let map_or_bottom f l =
    let aux l d =
      match f d with
      | `Bottom -> l
      | `Value d' -> d' :: l
    in
    let l = List.fold_left aux [] l in
    List.rev l (* preserve original order *)

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

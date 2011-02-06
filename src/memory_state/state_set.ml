(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

module Sindexed = 
  Hashtbl.Make
    (struct
      type t = Cvalue_type.Model.subtree
      let hash = Cvalue_type.Model.hash_subtree
      let equal = Cvalue_type.Model.equal_subtree
    end)

let sentinel = Sindexed.create 1

type t = 
    { t : Relations_type.Model.t Sindexed.t ;
      o : Relations_type.Model.t list }

let fold f acc { t = t ; o = o } = 
  List.fold_left f (Sindexed.fold (fun _k v a -> f a v) t acc) o

let of_list l = { t = sentinel ; o = l }

let iter f { t = t ; o = o } = 
  Sindexed.iter (fun _k v -> f v) t;
  List.iter f o
  
exception Found

let empty = { t = sentinel ; o = [] }

let is_empty t = t.t == sentinel && t.o = []

let exists f s = 
  try
    iter (fun v -> if f v then raise Found) s;
    false
  with Found -> true

let length s = List.length s.o + Sindexed.length s.t

exception Unchanged
let pretty fmt s =
  iter
    (fun state ->
      Format.fprintf fmt "set contains %a@\n"
	Relations_type.Model.pretty state)
    s

let add_to_list v s =
  if 
    List.exists
      (fun e -> Relations_type.Model.is_included v e)
      s
  then raise Unchanged;
(*  let nl, ns =
    filter
      (fun e -> not (Relations_type.Model.is_included e v))
      w
  in *)
  v :: s

let add_exn v s = 
  if not (Relations_type.Model.is_reachable v)
  then raise Unchanged;
  { s with o = add_to_list v s.o }

let merge_into sa sb = 
  let unchanged = ref true in
  let f acc e =
    try
      let r = add_exn e acc in
      unchanged := false;
      r
    with Unchanged ->
      acc
  in
  let result = fold f sb sa in
  if !unchanged then raise Unchanged;
  result


let add v s =
  try
    add_exn v s
  with Unchanged -> s

let unsafe_add v s = { s with o = v :: s.o }

let singleton v = add v empty

let join s =
  fold
    Relations_type.Model.join 
    Relations_type.Model.bottom 
    s

let join_dropping_relations s =
  Relations_type.Model.inject
    (fold
	(fun x y -> 
          snd (Cvalue_type.Model.join (Relations_type.Model.value_state y) x))
	Cvalue_type.Model.bottom
	s)

let fold f acc s = fold (fun acc v -> f v acc) s acc


(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j 4"
End:
*)


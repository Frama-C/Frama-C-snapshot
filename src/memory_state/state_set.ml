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

type t = Cvalue.Model.t list

let obviously_terminates = false

let fold = List.fold_left

let of_list l = l

let iter = List.iter 

exception Found

let empty = []

let is_empty t = t = empty

let exists = List.exists

let length = List.length

exception Unchanged
let pretty fmt s =
  iter
    (fun state ->
      Format.fprintf fmt "set contains %a@\n"
        Cvalue.Model.pretty state)
    s

let add_to_list  v s =
  if (not (Cvalue.Model.is_reachable v))
    || ((not obviously_terminates) && 
	   (List.exists
	       (fun e -> Cvalue.Model.is_included v e)
	       s))
  then raise Unchanged;
  v :: s

let add_exn v s = add_to_list v s

let merge_into sa sb =
  let unchanged = ref true in
  let f acc e =
    try
      let r = add_exn  e acc in
      unchanged := false;
      r
    with Unchanged ->
      acc
  in
  let result = fold f sb sa in
  if !unchanged then raise Unchanged;
  result


let add  v s =
  try
    add_exn  v s
  with Unchanged -> s

let unsafe_add v s = v :: s

let singleton v = add v empty

let join s =
  fold
    Cvalue.Model.join
    Cvalue.Model.bottom
    s

let join_dropping_relations = join

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

(* $Id: state_set.ml,v 1.8 2009-02-24 17:53:39 uid527 Exp $ *)

type t = Relations_type.Model.t list
let fold = List.fold_right
let iter = List.iter
let exists = List.exists
let for_all = List.for_all
let filter = List.filter
let is_empty l = l =[]
let empty = []
let elements e = e
let length s = List.length s

exception Unchanged
let pretty fmt set =
  List.iter
    (fun state ->
       Format.fprintf fmt "set contains %a@\n"
	 Relations_type.Model.pretty state)
    set

let rec length_at_most_n l n =
  if n < 0 
  then false
  else match l with
    [] -> true
  | _ :: t -> length_at_most_n t (pred n)
      
let add_exn v s =
(*  let len = List.length s in if len >= 3 then
      Format.printf "State_set:%4d@." (List.length s); *)
  if length_at_most_n s 50
  then begin
      if (not (Relations_type.Model.is_reachable v)) ||
	List.exists
	(fun e -> Relations_type.Model.is_included v e)
	s
      then raise Unchanged
      else
	let s =
	  List.filter
	    (fun e -> not (Relations_type.Model.is_included e v))
	    s
	in
	v::s
    end
  else begin
      if Relations_type.Model.is_reachable v then v :: s else raise Unchanged 
    end



let merge_into a b =
  let unchanged = ref true in
  let f acc e =
    try
      let r = add_exn e acc in
      unchanged := false;
      r
    with Unchanged ->
      acc
  in
  let result = List.fold_left f b a in
  if !unchanged then raise Unchanged;
  result

let add v s =
  try
    add_exn v s
  with Unchanged -> s

let singleton v = add v []

let cardinal = List.length

let join l =
  List.fold_left
    Relations_type.Model.join 
    Relations_type.Model.bottom 
    l

let join_dropping_relations l =
  Relations_type.Model.inject
    (List.fold_left
	(fun x y -> 
	  snd (Cvalue_type.Model.join 
		  x 
		  (Relations_type.Model.value_state y)))
	  Cvalue_type.Model.bottom 
	  l)


(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)


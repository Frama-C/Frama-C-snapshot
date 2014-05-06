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

(* ********************************************************************** *)
(** {2 Type declarations} *)
(* ********************************************************************** *)

type single_pack = Unmarshal.t

type t = Unknown | Abstract | Structure of structure | T_pack of single_pack
and structure = Sum of pack array array | Array of pack
and pack = Nopack | Pack of single_pack | Recursive of recursive
and recursive = t ref

(* ********************************************************************** *)
(** {2 Injection into Unmarshal} *)
(* ********************************************************************** *)

module Recursive = struct

  let create () = ref Unknown
  let update x t = x := t

  (* internals *)

  module Tbl =
    Hashtbl.Make
      (struct
        type t = recursive
        let equal = (==)
        let hash = Hashtbl.hash
       end)

  let positions = Tbl.create 7
  let arrays = Tbl.create 7

  let add_position r i j = Tbl.add positions r (i, j)

  let add_array a =
    Tbl.iter (fun r p -> Tbl.add arrays r (a, p)) positions;
    Tbl.clear positions

  let finalize t u =
    (* there are not so many mutually recursive values: linear time is ok *)
    Tbl.iter (fun r (a, (i, j)) -> if !r == t then a.(i).(j) <- u) arrays;
    Tbl.clear arrays

end

exception Cannot_pack

let pack_to_unmarshal i j = function
  | Nopack  -> raise Cannot_pack
  | Pack d -> d
  | Recursive r ->
    Recursive.add_position r i j;
    Unmarshal.Abstract (* will be updated later *)

let structure_to_unmarshal = function
  | Sum arr ->
    let a = Array.mapi (fun i -> Array.mapi (pack_to_unmarshal i)) arr in
    Recursive.add_array a;
    Unmarshal.Sum a
  | Array d -> Unmarshal.Array (pack_to_unmarshal 0 0 d)

let to_unmarshal = function
  | Unknown -> raise Cannot_pack
  | Abstract -> Unmarshal.Abstract
  | Structure s as x ->
    let y = Unmarshal.Structure (structure_to_unmarshal s) in
    Recursive.finalize x y;
    y
  | T_pack p -> p

let pack d = try Pack (to_unmarshal d) with Cannot_pack -> Nopack

let pack_from_unmarshal d = Pack d
let unsafe_pack = pack_from_unmarshal

let of_pack p = T_pack p

let structure_from_unmarshal = function
  | Unmarshal.Sum arr -> Sum (Array.map (Array.map pack_from_unmarshal) arr)
  | Unmarshal.Dependent_pair _ -> assert false (* not structural *)
  | Unmarshal.Array d -> Array (pack_from_unmarshal d)

let from_unmarshal = function
  | Unmarshal.Abstract -> Abstract
  | Unmarshal.Structure s -> Structure (structure_from_unmarshal s)
  | Unmarshal.Transform _ | Unmarshal.Return _ | Unmarshal.Dynamic _ ->
    assert false (* not structural *)

let recursive_pack r = Recursive r

(* ********************************************************************** *)
(** {2 Predefined values} *)
(* ********************************************************************** *)

let p_abstract = unsafe_pack Unmarshal.Abstract

let p_unit = unsafe_pack Unmarshal.t_unit
let p_int = unsafe_pack Unmarshal.t_int
let p_string = unsafe_pack Unmarshal.t_string
let p_float = unsafe_pack Unmarshal.t_float
let p_bool = unsafe_pack Unmarshal.t_bool
let p_int32 = unsafe_pack Unmarshal.t_int32
let p_int64 = unsafe_pack Unmarshal.t_int64
let p_nativeint = unsafe_pack Unmarshal.t_nativeint

let t_abstract = Abstract
let t_unknown = Unknown

let t_unit = from_unmarshal Unmarshal.t_unit
let t_int = from_unmarshal Unmarshal.t_int
let t_string = from_unmarshal Unmarshal.t_string
let t_float = from_unmarshal Unmarshal.t_float
let t_bool = from_unmarshal Unmarshal.t_bool
let t_int32 = from_unmarshal Unmarshal.t_int32
let t_int64 = from_unmarshal Unmarshal.t_int64
let t_nativeint = from_unmarshal Unmarshal.t_nativeint

let poly f = function
  | Abstract -> Abstract
  | Unknown -> Unknown
  | Structure _ | T_pack _ as a ->
    try from_unmarshal (f (to_unmarshal a)) with Cannot_pack -> Unknown

(* would be better to put it in Extlib, but no access to this library here *)
let array_for_all f a =
  try
    Array.iter (fun x -> if not (f x) then raise Exit) a;
    true
  with Exit ->
    false

let is_abstract_array a = 
  array_for_all (fun x -> x = Pack Unmarshal.Abstract) a

let poly_arr f a = 
  if is_abstract_array a then Abstract
  else
    try
      let d = f (Array.mapi (pack_to_unmarshal 0) a) in
      from_unmarshal d
    with Cannot_pack ->
      Unknown

let t_record = poly_arr Unmarshal.t_record
let t_tuple = poly_arr Unmarshal.t_tuple
let t_list = poly Unmarshal.t_list
let t_ref = poly Unmarshal.t_ref
let t_option = poly Unmarshal.t_option
let t_array = poly Unmarshal.t_array
let t_queue = poly Unmarshal.t_queue
let t_set_unchanged_compares = poly Unmarshal.t_set_unchangedcompares

let poly2 f a b = match a, b with
  | Abstract, Abstract -> Abstract
  | _, _ ->
    (* no special case for [Unknown]: sometimes, even if one part of the
       container is unknown, it can be unmarshaled. *)
    try from_unmarshal (f (to_unmarshal a) (to_unmarshal b))
    with Cannot_pack -> Unknown

let t_map_unchanged_compares = poly2 Unmarshal.t_map_unchangedcompares
let t_hashtbl_unchanged_hashs = poly2 (Unmarshal.t_hashtbl_unchangedhashs)

let t_sum a = 
  if array_for_all (is_abstract_array) a then Abstract 
  else Structure (Sum a)

(* ********************************************************************** *)
(** {2 Internals} *)
(* ********************************************************************** *)

(* ********************************************************************** *)
(* {3 cleanup} *)
(* ********************************************************************** *)

module Unmarshal_tbl =
  Hashtbl.Make
    (struct
      type t = Unmarshal.t
      let equal = (==)
      let hash = Hashtbl.hash
     end)

let unmarshal_visited = Unmarshal_tbl.create 7

module Tbl =
  Hashtbl.Make
    (struct
      type u = t
      type t = u
      let equal = (==)
      let hash = Hashtbl.hash
     end)

let visited = Tbl.create 7

let rec cleanup_unmarshal_structure = function
  | Unmarshal.Sum arr ->
    Unmarshal.Sum (Array.map (Array.map cleanup_unmarshal) arr)
  | Unmarshal.Array p -> Unmarshal.Array (cleanup_unmarshal p)
  | Unmarshal.Dependent_pair _ -> assert false

and cleanup_unmarshal = function
  | Unmarshal.Abstract as x -> x
  | Unmarshal.Transform(x, _) | Unmarshal.Return(x, _) -> cleanup_unmarshal x
  | Unmarshal.Structure s as x ->
    if Unmarshal_tbl.mem unmarshal_visited x then
      Unmarshal.Abstract (* not so good, but so much simpler *)
    else begin
      Unmarshal_tbl.add unmarshal_visited x ();
      Unmarshal.Structure (cleanup_unmarshal_structure s)
    end
  | Unmarshal.Dynamic _ -> assert false

let rec cleanup_pack = function
  | Nopack as x -> x
  | Recursive r ->
    let x = ref Unknown in
    Tbl.add visited !r x;
    Recursive x
  | Pack p -> Pack (cleanup_unmarshal p)

and cleanup_structure = function
  | Sum arr -> Sum (Array.map (Array.map cleanup_pack) arr)
  | Array p -> Array (cleanup_pack p)

and cleanup_aux = function
  | Unknown | Abstract as x -> x
  | Structure s as x ->
    let x' = Structure (cleanup_structure s) in
    (try
       let r = Tbl.find visited x in
       r := x'
     with Not_found ->
       ());
    x'
  | T_pack p -> T_pack (cleanup_unmarshal p)

let cleanup x =
  assert (Unmarshal_tbl.length unmarshal_visited = 0 && Tbl.length visited = 0);
  let x = cleanup_aux x in
  Unmarshal_tbl.clear unmarshal_visited;
  Tbl.clear visited;
  x

(* ********************************************************************** *)
(* {3 are_consistent} *)
(* ********************************************************************** *)

let unmarshal_consistent_visited = Unmarshal_tbl.create 7
let consistent_visited = Tbl.create 7

let rec are_consistent_unmarshal_structures s1 s2 = match s1, s2 with
  | Unmarshal.Sum arr1, Unmarshal.Sum arr2 ->
    (try
       for i = 0 to Array.length arr1 - 1 do
         let arr1_i = arr1.(i) in
         for j = 0 to Array.length arr1_i - 1 do
           if not (are_consistent_unmarshal arr1_i.(j) arr2.(i).(j)) then
             raise Exit
         done
       done;
       true
     with Invalid_argument _ | Exit ->
       false)
  | Unmarshal.Array d1, Unmarshal.Array d2
  | Unmarshal.Dependent_pair(d1, _), Unmarshal.Dependent_pair(d2, _)
  | Unmarshal.Dependent_pair(d1, _), Unmarshal.Sum [| [| d2; _ |] |]
  | Unmarshal.Sum [| [| d1; _ |] |], Unmarshal.Dependent_pair(d2, _)  ->
    are_consistent_unmarshal d1 d2
  | Unmarshal.Sum _, Unmarshal.Array _
  | Unmarshal.Array _, Unmarshal.Sum _
  | (Unmarshal.Array _ | Unmarshal.Sum _), Unmarshal.Dependent_pair _
  | Unmarshal.Dependent_pair _, (Unmarshal.Array _ | Unmarshal.Sum _) ->
    false

and  are_consistent_unmarshal d1 d2 = match d1, d2 with
  | Unmarshal.Abstract, Unmarshal.Abstract
  | Unmarshal.Dynamic _, _ | _, Unmarshal.Dynamic _ ->
    true
  | Unmarshal.Return(d1, _), d2 | d1, Unmarshal.Return(d2, _)
  | Unmarshal.Transform(d1, _), d2 | d1, Unmarshal.Transform(d2, _) ->
    are_consistent_unmarshal d1 d2
  | Unmarshal.Structure s1, Unmarshal.Structure s2 ->
    (try
       let d2' = Unmarshal_tbl.find unmarshal_consistent_visited d1 in
       d2 == d2'
     with Not_found ->
       (* Keep already visited terms in order to prevent looping when visiting
	  recursive terms. However, remove them from the table after visiting in
	  order to not pollute it when visiting cousins: fixed bts #1277.
	  Would be better to use a persistent table instead of a mutable one,
	  but not possible to provide a (terminating) comparison. *)
       Unmarshal_tbl.add unmarshal_consistent_visited d1 d2;
       let b = are_consistent_unmarshal_structures s1 s2 in
       Unmarshal_tbl.remove unmarshal_consistent_visited d1;
       b)
  | Unmarshal.Abstract, Unmarshal.Structure _ -> 
    true (* we provide a more precise version: accept it *)
  | _, _ ->
    false

let are_consistent_pack p1 p2 = match p1, p2 with
  | Nopack, Nopack -> true
  | Pack s1, Pack s2 -> are_consistent_unmarshal s1 s2
  | Recursive _, _ | _, Recursive _ ->
    invalid_arg "unbound recursive structural descriptors"
  | Nopack, Pack _ | Pack _, Nopack -> false

let rec are_consistent_structures s1 s2 = match s1, s2 with
  | Sum arr1, Sum arr2 ->
    (try
       for i = 0 to Array.length arr1 - 1 do
         let arr1_i = arr1.(i) in
         for j = 0 to Array.length arr1_i - 1 do
           if not (are_consistent_pack arr1_i.(j) arr2.(i).(j)) then raise Exit
         done
       done;
       true
     with Invalid_argument _ | Exit ->
       false)
  | Array d1, Array d2 -> are_consistent_pack d1 d2
  | Sum _, Array _ | Array _, Sum _ -> false

and are_consistent_aux d1 d2 = match d1, d2 with
  | Unknown, Unknown | Abstract, Abstract -> true
  | Structure s1, Structure s2 ->
    (try
       let d2' = Tbl.find consistent_visited d1 in
       d2 == d2'
     with Not_found ->
       Tbl.add consistent_visited d1 d2;
       are_consistent_structures s1 s2)
  | d, T_pack s | T_pack s, d -> are_consistent_unmarshal (to_unmarshal d) s
  | Abstract, Structure _ -> 
    true  (* we provide a more precise version: accept it *)
  | Structure _, Abstract -> false
  | _, _ -> false

let are_consistent d1 d2 =
  assert (Unmarshal_tbl.length unmarshal_consistent_visited = 0
         && Tbl.length consistent_visited = 0);
  let b = are_consistent_aux d1 d2 in
  Unmarshal_tbl.clear unmarshal_consistent_visited;
  Tbl.clear consistent_visited;
  b

(*
  Local Variables:
  compile-command: "make -C ../.."
  End:
 *)

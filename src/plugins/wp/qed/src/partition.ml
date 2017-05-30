(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

module type Elt =
sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module type Set =
sig
  type t
  type elt
  val singleton : elt -> t
  val iter : (elt -> unit) -> t -> unit
  val union : t -> t -> t
  val inter : t -> t -> t
end

module type Map =
sig
  type 'a t
  type key
  val empty : 'a t
  val is_empty : 'a t -> bool
  val find : key -> 'a t -> 'a
  val add : key -> 'a -> 'a t -> 'a t
  val remove : key -> 'a t -> 'a t
  val iter : (key -> 'a -> unit) -> 'a t -> unit
end

module Make(E : Elt)
    (S : Set with type elt = E.t)
    (M : Map with type key = E.t) =
struct

  type elt = E.t
  type set = S.t

  type t = {
    mutable dag : E.t M.t ;
    members : S.t M.t ;
    size : int ;
  }

  let empty = { size = 0 ; dag = M.empty ; members = M.empty }

  let rec lookup p a =
    try
      let a0 = M.find a p.dag in
      let a1 = lookup p a0 in
      p.dag <- M.add a a1 p.dag ; a1
    with Not_found -> a

  let equal t a b = E.equal (lookup t a) (lookup t b)
  let members p e =
    try M.find e p.members with Not_found -> S.singleton e

  let merge p a b =
    let a = lookup p a in
    let b = lookup p b in
    let cmp = E.compare a b in
    if cmp = 0 then p else
      let c = S.union (members p a) (members p b) in
      let size = succ p.size in
      if cmp < 0 then {
        size ; dag = M.add b a p.dag ;
        members = M.add a c (M.remove b p.members) ;
      } else {
        size ; dag = M.add a b p.dag ;
        members = M.add b c (M.remove a p.members) ;
      }

  let rec merge_with p e = function
    | [] -> p
    | e'::es -> merge_with (merge p e e') e es

  let merge_list p = function
    | [] -> p
    | e::es -> merge_with p e es

  let merge_set p s =
    let p = ref p in
    let w = ref None in
    S.iter
      (fun e ->
         match !w with
         | None -> w := Some e
         | Some u -> p := merge !p u e
      ) s ;
    !p

  let iter f p = M.iter f p.members
  let unstable_iter f p = M.iter f p.dag

  let map f p =
    let r = ref empty in
    M.iter (fun a b -> r := merge !r (f a) (f b)) p.dag ; !r

  let merge_dag p dag =
    let r = ref p in
    M.iter (fun a b -> r := merge !r a b) dag ; !r

  let union p q =
    if p.size < q.size
    then merge_dag q p.dag
    else merge_dag p q.dag

  let inter p q =
    let r = ref empty in
    M.iter (fun _ ca ->
        M.iter (fun _ cb ->
            r := merge_set !r (S.inter ca cb)
          ) q.members
      ) p.members ;
    !r

  let is_empty p = M.is_empty p.dag

end

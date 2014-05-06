(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

(* -------------------------------------------------------------------------- *)
(* --- Merging Map Functor                                                --- *)
(* -------------------------------------------------------------------------- *)

module type Key =
sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Make(K : Key) =
struct

  module Lmap = Listmap.Make(K)

  type key = K.t

  type 'a t = 'a Lmap.t Intmap.t (* sorted collisions *)

  let is_empty m = 
    try Intmap.iteri (fun _ m -> if m<>[] then raise Exit) m ; true
    with Exit -> false

  let empty = Intmap.empty

  let _nonempty     = function [] -> None | l -> Some l
  let _nonempty_inv = function None -> [] | Some l -> l

  (* good sharing *) 
  let insert f k v m =
    let h = K.hash k in
    Intmap.insert (fun _h kv old -> 
        match kv with
        | [k,v] -> Lmap.insert f k v old 
        | _ -> assert false) h [k,v] m

  (* good sharing *) 
  (*  val change : (key -> 'b -> 'a option -> 'a option) -> key -> 'b -> 'a t -> 'a t*)
  let change (f:key -> 'b -> 'a option -> 'a option) (k:key) (v:'b) (m:'a t) =
    let h = K.hash k in
    Intmap.change (fun _h (k,v) -> function
        | None -> (match f k v None with | None -> None | Some w -> Some [k,w])
        | Some old -> _nonempty (Lmap.change f k v old)) h (k,v) m

  (* good sharing *) 
  let add k v =
    insert (fun _k x _old -> x) k v

  let find k m = Lmap.find k (Intmap.find (K.hash k) m)
  let findk k m = Lmap.findk k (Intmap.find (K.hash k) m)

  let mem k m = try ignore (find k m) ; true with Not_found -> false

  let map f m = Intmap.map (Lmap.map f) m
  let mapi f m = Intmap.map (Lmap.mapi f) m

  let mapf f = Intmap.mapf (fun _h w -> _nonempty (Lmap.mapf f w))

  (* good sharing *) 
  let mapq f = Intmap.mapq (fun _h w -> _nonempty (Lmap.mapq f w))

  (* good sharing *) 
  let filter f = Intmap.mapq (fun _k w -> _nonempty (Lmap.filter f w))

  (* good sharing *) 
  let remove k m = 
    let h = K.hash k in
    Intmap.change (fun _h k ->  function
        | None -> None
        | Some old -> _nonempty (Lmap.remove k old)) h k m

  let iter f m = Intmap.iter (Lmap.iter f) m
  let iter_sorted f m =
    let xs = 
      Intmap.fold (List.merge (fun a b -> K.compare (fst a) (fst b))) m []
    in
    List.iter (fun (k,v) -> f k v) xs

  let fold f m a = Intmap.fold (Lmap.fold f) m a
  let fold_sorted f m a =
    let xs = 
      Intmap.fold (List.merge (fun a b -> K.compare (fst a) (fst b))) m []
    in
    List.fold_left (fun acc (k,v) -> f k v acc) a xs

  let size m = fold (fun _ _ w -> succ w) m 0

  (* good sharing *) 
  let partition p =
    Intmap.partition_split (fun _k w ->
        let u,v = Lmap.partition p w in
        (_nonempty u), (_nonempty v))

  (* good sharing *) 
  let union f = Intmap.union (fun _h -> Lmap.union f)

  let inter f = Intmap.inter (fun _h -> Lmap.inter f)
  let interf f = Intmap.interf (fun _h a b -> _nonempty (Lmap.interf f a b))

  (* good sharing *) 
  let interq f = Intmap.interq (fun _h a b -> _nonempty (Lmap.interq f a b))

  (* good sharing *) 
  let diffq f = Intmap.diffq (fun _h a b -> _nonempty (Lmap.diffq f a b))

  let subset f = Intmap.subset (fun _h -> Lmap.subset f)
  let equal eq m1 m2 = Intmap.equal (Lmap.equal eq) m1 m2

  let iterk f = Intmap.iterk (fun _h -> Lmap.iterk f)

  let iter2 f = Intmap.iter2 (fun _h u1 u2 -> 
      Lmap.iter2 f (_nonempty_inv u1) (_nonempty_inv u2))

  (* good sharing *) 
  let merge f = Intmap.merge (fun _h u1 u2 -> 
      _nonempty (Lmap.merge f (_nonempty_inv u1) (_nonempty_inv u2)))

end

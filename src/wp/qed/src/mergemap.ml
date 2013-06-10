(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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
	    
  let add k v m =
    let h = K.hash k in
    let w = try Lmap.add k v (Intmap.find h m) with Not_found -> [k,v] in
    Intmap.add h w m

  let find k m = Lmap.find k (Intmap.find (K.hash k) m)
  let findk k m = Lmap.findk k (Intmap.find (K.hash k) m)

  let mem k m = try ignore (find k m) ; true with Not_found -> false

  let map f m = Intmap.map (Lmap.map f) m
  let mapi f m = Intmap.map (Lmap.mapi f) m

  let select phi m = Intmap.mapf 
    (fun _h w -> match phi w with [] -> None | w -> Some w) m

  let mapf f = select (Lmap.mapf f)
  let filter f = select (Lmap.filter f)
  let remove k = select (Lmap.remove k)

  let iter f m = Intmap.iter (Lmap.iter f) m
  let iter_sorted f m =
    let xs = ref [] in
    Intmap.iter 
      (fun w -> 
	 xs := List.merge 
	   (fun a b -> K.compare (fst a) (fst b)) 
	   w !xs
      ) m ;
    List.iter (fun (k,v) -> f k v) !xs

  let fold f m a = Intmap.fold (Lmap.fold f) m a
  let fold_sorted f m a =
    let xs = 
      Intmap.fold (List.merge (fun a b -> K.compare (fst a) (fst b))) m []
    in
    List.fold_left (fun acc (k,v) -> f k v acc) a xs

  let union f = Intmap.union (fun _h -> Lmap.union f)
  let inter f = Intmap.inter (fun _h -> Lmap.inter f)
  let subset f = Intmap.subset (fun _h -> Lmap.subset f)
  let equal eq m1 m2 = Intmap.equal (Lmap.equal eq) m1 m2

  let iterk f = Intmap.iterk (fun _h -> Lmap.iterk f)

  let iter2 f m1 m2 =
    Intmap.iter2
      (fun _h u1 u2 ->
	 let w1 = match u1 with None -> [] | Some w -> w in
	 let w2 = match u2 with None -> [] | Some w -> w in
	 Lmap.iter2 f w1 w2) m1 m2

  let merge f m1 m2 =
    Intmap.merge
      (fun _h u1 u2 ->
	 let w1 = match u1 with None -> [] | Some w -> w in
	 let w2 = match u2 with None -> [] | Some w -> w in
	 match Lmap.merge f w1 w2 with
	   | [] -> None
	   | w -> Some w) m1 m2

end

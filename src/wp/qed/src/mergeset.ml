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
(* --- Merging Set Functor                                                --- *)
(* -------------------------------------------------------------------------- *)

module type Elt =
sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Make(E : Elt) =
struct

  module Lset = Listset.Make(E)

  type elt = E.t

  type t = E.t list Intmap.t

  let is_empty es = 
    try 
      Intmap.iteri (fun _ s -> if s <> [] then raise Exit) es ; 
      true
    with Exit -> false
  let empty = Intmap.empty

  let add e m =
    let h = E.hash e in
    let w = try Lset.add e (Intmap.find h m) with Not_found -> [e] in
    Intmap.add h w m

  let singleton e = 
    let h = E.hash e in
    Intmap.add h [e] Intmap.empty

  let mem e m =
    try Lset.mem e (Intmap.find (E.hash e) m)
    with Not_found -> false

  let elements m =
    Intmap.fold (fun w xs -> List.merge E.compare w xs) m []

  let iter_sorted f m =
    List.iter f (elements m)

  let fold_sorted f m a =
    List.fold_left (fun acc x -> f x acc) a (elements m)

  let nonempty = function [] -> None | l -> Some l

  let filter f m = 
    Intmap.mapf (fun _ l -> nonempty (Lset.filter f l)) m

  let partition f m =
    let m0 = Intmap.map (Lset.partition f) m in
    Intmap.mapf (fun _ (p,_) -> nonempty p) m0 ,
    Intmap.mapf (fun _ (_,q) -> nonempty q) m0
      
  exception BREAK

  let iter f = Intmap.iter (Lset.iter f)
  let fold f = Intmap.fold (Lset.fold f)

  let for_all f m = 
    try iter (fun x -> if not (f x) then raise BREAK) m ; true
    with BREAK -> false

  let exists f m = 
    try iter (fun x -> if f x then raise BREAK) m ; false
    with BREAK -> true

  let union = Intmap.union (fun _h -> Lset.union)
  let inter = Intmap.inter (fun _h -> Lset.union)
  let subset = Intmap.subset (fun _h -> Lset.subset)

  let intersect m1 m2 =
    try
      Intmap.iter2
	(fun _h xs ys ->
	   match xs , ys with
	     | None , _ | _ , None -> ()
	     | Some w1 , Some w2 -> if Lset.intersect w1 w2 then raise Exit
	) m1 m2 ; false
    with Exit -> true

  let equal = Intmap.equal E.equal

end

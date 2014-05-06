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

module type S =
sig
  type elt
  type t
  val empty : t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val find : elt -> t -> elt
  val add : elt -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val subset : t -> t -> bool
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val filter : (elt -> bool) -> t -> t
  val partition : (elt -> bool) -> t -> t * t
  val cardinal : t -> int
  val elements : t -> elt list
  val map : (elt -> elt) -> t -> t
  val mapf : (elt -> elt option) -> t -> t
  val intersect : t -> t -> bool
end

module type IndexedElements =
sig
  type t
  val id : t -> int (* unique per t *)
end

module Make(E : IndexedElements) =
struct
  type t = E.t Intmap.t
  type elt = E.t
  let empty = Intmap.empty
  let singleton x = Intmap.singleton (E.id x) x

  (* good sharing *) 
  let add x = Intmap.add (E.id x) x

  (* good sharing *) 
  let remove x = Intmap.remove (E.id x)
  let is_empty = Intmap.is_empty
  let mem x = Intmap.mem (E.id x)
  let find x = Intmap.find (E.id x)
  let cardinal = Intmap.size
  let compare = Intmap.compare (fun _ _ -> 0)
  let equal = Intmap.equal (fun _ _ -> true)

  let _keep _ x _ = x
  let _keepq _ x _ = Some x
  let _same _ _ _ = true

  (* good sharing *) 
  let union = Intmap.union _keep

  (* good sharing *) 
  let inter = Intmap.interq _keepq

  (* good sharing *) 
  let diff = Intmap.diffq _keepq
  let subset = Intmap.subset _same
  let intersect = Intmap.intersectf _same

  (* increasing order on id *)
  let iter f = Intmap.iteri (fun _i x -> f x)

  (* increasing order on id *)
  let fold f = Intmap.foldi (fun _i x e -> f x e)

  (* good sharing *) 
  let filter f = Intmap.filter (fun _i x -> f x)

  (* good sharing *) 
  let partition f = Intmap.partition (fun _i x -> f x)

  let for_all f = Intmap.for_all (fun _i x -> f x)
  let exists f = Intmap.exists (fun _i x -> f x)

  (* increasing order on id *)
  let elements = Intmap.mapl (fun _i x -> x)

  (* good sharing *) 
  let mapf f= Intmap.mapq (fun _i x -> f x)

  (* good sharing *) 
  let map f = Intmap.mapq (fun _i x -> Some (f x))
end

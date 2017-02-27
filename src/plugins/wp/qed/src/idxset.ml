(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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
  let mem x m = Intmap.mem (E.id x) m
  let find x m = Intmap.find (E.id x) m
  let cardinal = Intmap.size
  let compare m1 m2 = Intmap.compare (fun _ _ -> 0) m1 m2
  let equal m1 m2 = Intmap.equal (fun _ _ -> true) m1 m2

  let _keep _ x _ = x
  let _keepq _ x _ = Some x
  let _same _ _ _ = true

  (* good sharing *)
  let union m1 m2 = Intmap.union _keep m1 m2

  (* good sharing *)
  let inter m1 m2 = Intmap.interq _keepq m1 m2

  (* good sharing *)
  let diff m1 m2 = Intmap.diffq _keepq m1 m2
  let subset m1 m2 = Intmap.subset _same m1 m2
  let intersect m1 m2 = Intmap.intersectf _same m1 m2

  (* increasing order on id *)
  let iter f m = Intmap.iteri (fun _i x -> f x) m

  (* increasing order on id *)
  let fold f m i = Intmap.foldi (fun _i x e -> f x e) m i

  (* good sharing *)
  let filter f m = Intmap.filter (fun _i x -> f x) m

  (* good sharing *)
  let partition f m = Intmap.partition (fun _i x -> f x) m

  let for_all f m = Intmap.for_all (fun _i x -> f x) m
  let exists f m = Intmap.exists (fun _i x -> f x) m

  (* increasing order on id *)
  let elements m = Intmap.mapl (fun _i x -> x) m

  (* good sharing *)
  let mapf f m = Intmap.mapq (fun _i x -> f x) m

  (* good sharing *)
  let map f m = Intmap.mapq (fun _i x -> Some (f x)) m
end

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

module type S =
sig
  type key
  type 'a t
  val is_empty : 'a t -> bool
  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val mem : key -> 'a t -> bool
  val find : key -> 'a t -> 'a
  val remove : key -> 'a t -> 'a t
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val map : (key -> 'a -> 'b) -> 'a t -> 'b t
  val mapf : (key -> 'a -> 'b option) -> 'a t -> 'b t
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val union : (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val inter : (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val merge : (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t 
end

module type IndexedKey =
sig
  type t
  val id : t -> int (** unique per t *)
end

module Make( K : IndexedKey ) =
struct
  type key = K.t
  type 'a t = (key * 'a) Intmap.t
  let is_empty = Intmap.is_empty
  let empty = Intmap.empty
  let add k x m = Intmap.add (K.id k) (k,x) m
  let mem k m = Intmap.mem (K.id k) m
  let find k m = snd (Intmap.find (K.id k) m)
  let remove k m = Intmap.remove (K.id k) m
  let compare f m1 m2 = Intmap.compare (fun (_,a) (_,b) -> f a b) m1 m2
  let equal f m1 m2 = Intmap.equal (fun (_,a) (_,b) -> f a b) m1 m2
  let iter f m = Intmap.iter (fun (k,v) -> f k v) m
  let map f m = Intmap.map (fun (k,v) -> k,f k v) m
  let pack k = function None -> None | Some v -> Some (k,v)
  let mapf f m = Intmap.mapf (fun _ (k,v) -> pack k (f k v)) m
  let filter f m = Intmap.filter (fun _ (k,v) -> f k v) m
  let fold f m w = Intmap.fold (fun (k,v) w -> f k v w) m w
  let union f a b = Intmap.union (fun _ (k,v) (_,v') -> k,f k v v') a b
  let inter f a b = Intmap.inter (fun _ (k,v) (_,v') -> k,f k v v') a b
  let pack k = function None -> None | Some v -> Some (k,v)
  let merge f a b = Intmap.merge
    (fun _ u v ->
       match u , v with
	 | None , None -> None
	 | Some(k,v) , None -> pack k (f k (Some v) None)
	 | None , Some(k,v) -> pack k (f k None (Some v))
	 | Some(k,v) , Some(_,v') -> pack k (f k (Some v) (Some v'))
    ) a b
end


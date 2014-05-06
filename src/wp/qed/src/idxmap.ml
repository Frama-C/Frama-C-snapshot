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
  val mapq : (key -> 'a -> 'a option) -> 'a t -> 'a t
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val union : (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val inter : (key -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val interf : (key -> 'a -> 'b -> 'c option) -> 'a t -> 'b t -> 'c t
  val interq : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val diffq : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val merge : (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t 

  (** [insert (fun key v old -> ...) key v map] *)
  val insert : (key -> 'a -> 'a -> 'a) -> key -> 'a -> 'a t -> 'a t

  val change : (key -> 'b -> 'a option -> 'a option) -> key -> 'b -> 'a t -> 'a t

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

  (* good sharing *) 
  let add k x m = Intmap.add (K.id k) (k,x) m

  let _pack k = function None -> None | Some v -> Some (k,v)
  let _packold ((k,old) as o) w = if w==old then o else k,w
  let _oldpack o = function None -> None | Some w -> Some (_packold o w)

  (* good sharing *) 
  let insert f k v m = Intmap.insert (fun _k (k,v) ((_,old) as o) -> _packold o (f k v old)) (K.id k) (k,v) m

  (* good sharing *) 
  let change f k v m = Intmap.change (fun _k (k,v) -> function
      | None -> _pack k (f k v None)
      | Some ((_,old) as o) -> _oldpack o (f k v (Some old))) (K.id k) (k,v) m

  let mem k m = Intmap.mem (K.id k) m
  let find k m = snd (Intmap.find (K.id k) m)

  let compare f m1 m2 = Intmap.compare (fun (_,a) (_,b) -> f a b) m1 m2
  let equal f m1 m2 = Intmap.equal (fun (_,a) (_,b) -> f a b) m1 m2
  let iter f m = Intmap.iter (fun (k,v) -> f k v) m
  let fold f m w = Intmap.fold (fun (k,v) w -> f k v w) m w
  let map f m = Intmap.map (fun (k,v) -> k,f k v) m
  let mapf f m = Intmap.mapf (fun _ (k,v) -> _pack k (f k v)) m

  (* good sharing *) 
  let mapq f = Intmap.mapq (fun _ ((k,old) as o) -> _oldpack o (f k old))

  (* good sharing *) 
  let partition f = Intmap.partition (fun _ (k,v) -> f k v)

  (* good sharing *) 
  let remove k = Intmap.remove (K.id k)

  (* good sharing *) 
  let filter f = Intmap.filter (fun _ (k,v) -> f k v)

  (* good sharing *) 
  let union f = Intmap.union (fun _ ((k,v) as x) ((_,v') as y) -> 
      let w = f k v v' in if w==v then x else if w==v then y else k,w )

  let inter f = Intmap.inter (fun _ (k,v) (_,v') -> k,f k v v')
  let interf f = Intmap.interf (fun _ (k,v) (_,v') -> _pack k (f k v v'))

  (* good sharing *) 
  let interq f = Intmap.interq (fun _ ((k,v) as x) ((_,v') as y) -> match f k v v' with None -> None | Some w -> 
        Some (if w==v then x else if w==v then y else k,w))

  (* good sharing *) 
  let diffq f = Intmap.diffq (fun _ ((k,v) as x) ((_,v') as y) -> match f k v v' with None -> None | Some w -> 
        Some (if w==v then x else if w==v then y else k,w))

  let merge f a b = Intmap.merge
      (fun _ u v ->
         match u , v with
         | None , None -> None
         | Some(k,v) , None -> _pack k (f k (Some v) None)
         | None , Some(k,v) -> _pack k (f k None (Some v))
         | Some(k,v) , Some(_,v') -> _pack k (f k (Some v) (Some v'))
      ) a b
end


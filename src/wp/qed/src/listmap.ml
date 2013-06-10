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
(* --- Merging List-map Functor                                           --- *)
(* -------------------------------------------------------------------------- *)

module type Key =
sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Make(K : Key) =
struct

  type key = K.t

  type 'a t = (key * 'a) list

  let empty = []

  let rec add k v = function
    | [] -> [k,v]
    | (a::next) as w ->
	let c = K.compare k (fst a) in
	if c < 0 then (k,v) :: w else
	  if c = 0 then (k,v) :: next else
	    (* c > 0 *) a :: add k v next
      
  let rec findk k = function
    | [] -> raise Not_found
    | ((k0,_) as e) :: next ->
	let c = K.compare k k0 in
	if c < 0 then raise Not_found else
	  if c > 0 then findk k next else
	    e

  let find k m = snd (findk k m)

  let mem k m = try ignore (find k m) ; true with Not_found -> false

  let map f m = List.map (fun (k,v) -> k,f v) m
  let mapi f m = List.map (fun (k,v) -> k,f k v) m
  let iter f m = List.iter (fun (k,v) -> f k v) m

  let remove k m = List.filter (fun (k0,_) -> K.compare k k0 <> 0) m
  let filter f m = List.filter (fun (k,x) -> f k x) m

  let rec mapf f = function
    | [] -> []
    | (k,x)::m ->
	match f k x with
	  | Some y -> (k,y)::mapf f m
	  | None -> mapf f m

  let rec fold f m a = match m with
    | (k,v)::w -> f k v (fold f w a)
    | [] -> a

  let rec union f w1 w2 =
    match w1 , w2 with
      | [] , w | w , [] -> w
      | ((k1,v1) as a1)::r1 , ((k2,v2) as a2)::r2 ->
	  let c = K.compare k1 k2 in
	  if c < 0 then a1 :: union f r1 w2 else
	    if c > 0 then a2 :: union f w1 r2 else
	      (k1,f k1 v1 v2) :: union f r1 r2

  let rec inter f w1 w2 =
    match w1 , w2 with
      | [] , _ | _ , [] -> []
      | (k1,v1)::r1 , (k2,v2)::r2 ->
	  let c = K.compare k1 k2 in
	  if c < 0 then inter f r1 w2 else
	    if c > 0 then inter f w1 r2 else
	      (k1,f k1 v1 v2) :: inter f r1 r2

  let rec subset f w1 w2 =
    match w1 , w2 with
      | [] , _ -> true
      | _::_ , [] -> false
      | (k1,v1)::r1 , (k2,v2)::r2 ->
	  let c = K.compare k1 k2 in
	  if c < 0 then false else
	    if c > 0 then subset f w1 r2 else
	      (f k1 v1 v2 && subset f r1 r2)

  let rec equal eq w1 w2 =
    match w1 , w2 with
      | [] , [] -> true
      | [] , _::_ | _::_ , [] -> false
      | (k1,v1)::r1 , (k2,v2)::r2 ->
	  K.equal k1 k2 && eq v1 v2 && equal eq r1 r2

  let rec iterk
      (f : K.t -> 'a -> 'b -> unit)
      (w1 : (K.t * 'a) list) 
      (w2 : (K.t * 'b) list) 
      =
    match w1 , w2 with
      | [] , _  | _  , [] -> ()
      | (k1,v1)::r1 , (k2,v2)::r2 ->
	  let c = K.compare k1 k2 in
	  if c < 0 then iterk f r1 w2 else
	    if c > 0 then iterk f w1 r2 else
	      (f k1 v1 v2 ; iterk f r1 r2)

  let rec iter2 
      (f : K.t -> 'a option -> 'b option -> unit)
      (w1 : (K.t * 'a) list) 
      (w2 : (K.t * 'b) list) 
      =
    match w1 , w2 with
      | [] , [] -> ()
      | _  , [] -> List.iter (fun (k1,v1) -> f k1 (Some v1) None) w1
      | [] , _  -> List.iter (fun (k2,v2) -> f k2 None (Some v2)) w2
      | (k1,v1)::r1 , (k2,v2)::r2 ->
	  let c = K.compare k1 k2 in
	  if c < 0 then (f k1 (Some v1) None ; iter2 f r1 w2) else
	    if c > 0 then (f k2 None (Some v2) ; iter2 f w1 r2) else
	      (f k1 (Some v1) (Some v2) ; iter2 f r1 r2)

  let cons k v w =
    match v with
      | None -> w
      | Some x -> (k,x) :: w

  let rec merge
      (f : K.t -> 'a option -> 'b option -> 'c option)
      w1 w2 =
    match w1 , w2 with
      | [] , [] -> []
      | _ , [] -> mapf (fun k1 v1 -> f k1 (Some v1) None) w1
      | [] , _ -> mapf (fun k2 v2 -> f k2 None (Some v2)) w2
      | (k1,v1)::r1 , (k2,v2)::r2 ->
	  let c = K.compare k1 k2 in
	  if c < 0 then cons k1 (f k1 (Some v1) None) (merge f r1 w2) else
	    if c > 0 then cons k2 (f k2 None (Some v2)) (merge f w1 r2) else
	      cons k1 (f k1 (Some v1) (Some v2)) (merge f r1 r2)

end

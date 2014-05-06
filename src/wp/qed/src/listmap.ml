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
  (* Should be implemented using listset *)

  type key = K.t

  type 'a t = (key * 'a) list

  let compare cmp = Hcons.compare_list (fun (i,x) (j,y) -> let r = K.compare i j in if r != 0 then r else cmp x y)
  let equal eq = Hcons.equal_list (fun (i,x) (j,y) -> K.equal i j && eq x y)

  let empty = []

  (* used for better sharing between a list and a modified list *)
  let rev_append_until i l1 l2 = 
    let rec aux acc = function
      | [] -> acc
      | i'::_ when i'==i -> acc
      | i'::l -> aux (i'::acc) l
    in aux l2 l1

  (* used for better sharing between a list and a modified list *)
  let append_until i l1 l2 = 
    List.rev_append (rev_append_until i l1 []) l2

  (* good sharing *) 
  let mapq f l =
    let rec aux ((res,rest) as acc) = function
      | [] -> List.rev_append res rest
      | ((k,v) as i) :: resti -> 
          (match f k v with
           | None -> (* remove *) aux ((rev_append_until i rest res),resti) resti
           | Some v' -> 
               if v' = v then (* add idem *) aux acc resti 
               else (* add new *) aux (((k,v')::(rev_append_until i rest res)),resti) resti)
    in aux ([],l) l

  (* good sharing *) 
  (* idem List.filter, but returns l if no element is removed. *) 
  let filter f l =
    let rec aux ((res,rest) as acc) = function
      | [] -> List.rev_append res rest
      | i :: resti -> 
          if f i then (* add idem *) aux acc resti 
          else (* remove *) aux ((rev_append_until i rest res),resti) resti
    in aux ([],l) l

  (* good sharing *) 
  (* idem List.partition, better sharing. *) 
  let partition f l =
    let rec aux ((res,rest) as acc) ((res',rest') as acc') = function
      | [] -> (List.rev_append res rest), (List.rev_append res' rest')
      | ((k,v) as i) :: resti -> 
          if f k v then aux acc ((rev_append_until i rest' res'),resti) resti 
          else aux ((rev_append_until i rest res),resti) acc' resti
    in aux ([],l) ([],l) l

  (* good sharing *) 
  let change f k v l =
    let rec aux = function
      | [] -> (match f k v None with None -> l | Some w -> l @ [k,w])
      | ((k',v') as a)::next->
          let c = K.compare k k' in
          if c < 0 then l
          else if c = 0 then 
            match f k v (Some v') with
            | None -> append_until a l next
            | Some w -> if w==v' then l
                else append_until a l ((k, w) :: next)
          else (* c > 0 *) aux next
    in aux l

  (* good sharing *) 
  let insert f k v l = 
    let rec aux = function
      | [] -> l @ [k,v]
      | (((k',v') as a)::next) as w ->
          let c = K.compare k k' in
          if c < 0 then append_until a l ((k,v) :: w)
          else if c = 0 then 
            let w = f k v v' 
            in if w==v' then l
            else append_until a l ((k, w) :: next)
          else (* c > 0 *) aux next
    in aux l

  (* good sharing *) 
  let add k x = insert (fun _k x _old -> x) k x

  let rec findk k = function
    | [] -> raise Not_found
    | ((k0,_) as e) :: next ->
        let c = K.compare k k0 in
        if c < 0 then raise Not_found else
        if c > 0 then findk k next else
          e

  let find k m = snd (findk k m)

  let mem k m = try ignore (find k m) ; true with Not_found -> false

  let mapi f = List.map (fun (k,v) -> k,f k v)
  let map f = mapi (fun _k v -> f v)

  let iter f = List.iter (fun (k,v) -> f k v)

  (* good sharing *) 
  let remove k m = change (fun _ _ _ -> None) k () m

  (* good sharing *) 
  let filter f m = filter (fun (k,x) -> f k x) m

  let rec mapf f = function
    | [] -> []
    | (k,x)::m ->
        match f k x with
        | Some y -> (k,y)::mapf f m
        | None -> mapf f m

  let fold f m a = List.fold_left (fun a (k,v) -> f k v a) a m

  let rec inter f w1 w2 =
    match w1 , w2 with
    | [] , _ | _ , [] -> []
    | (k1,v1)::r1 , (k2,v2)::r2 ->
        let c = K.compare k1 k2 in
        if c < 0 then inter f r1 w2 else
        if c > 0 then inter f w1 r2 else
          (k1,f k1 v1 v2) :: inter f r1 r2

  let rec interf f w1 w2 =
    match w1 , w2 with
    | [] , _ | _ , [] -> []
    | (k1,v1)::r1 , (k2,v2)::r2 ->
        let c = K.compare k1 k2 in
        if c < 0 then interf f r1 w2 else
        if c > 0 then interf f w1 r2 else
          match f k1 v1 v2 with
          | None -> interf f r1 r2
          | Some v12 -> (k1,v12) :: interf f r1 r2

  (* good sharing with w1 *) 
  let interq f w1 w2 =
    let rec aux ((res,o1) as acc) w1 w2 =
      match w1 , w2 with
      | [] , _ ->    (* no addition *) List.rev_append res o1
      | a1::_, [] -> (* no addition *) List.rev_append res (List.rev (rev_append_until a1 o1 []))
      | ((k1,v1) as a1)::r1 , (k2,v2)::r2 ->
          let c = K.compare k1 k2 in
          if c < 0 then (* remove a1 *) aux ((rev_append_until a1 o1 res),r1) r1 w2
          else if c > 0 then (* remove a2 *) aux acc w1 r2
          else match f k1 v1 v2 with
            | None -> (* remove a1 *) aux ((rev_append_until a1 o1 res),r1) r1 r2
            | Some w -> if w==v1 then (* adding a1 *) aux acc r1 r2 
                else (* adding w *) aux (((k1,w)::(rev_append_until a1 o1 res)), r1) r1 r2
    in aux ([],w1) w1 w2

  (* good sharing with w1 *) 
  let diffq f w1 w2 =
    let rec aux ((res,o1) as acc) w1 w2 =
      match w1 , w2 with
      | [] , _ -> (* no addition *) List.rev_append res o1
      | _ , [] -> (* adding w1 *) List.rev_append res o1
      | ((k1,v1) as a1)::r1 , (k2,v2)::r2 ->
          let c = K.compare k1 k2 in
          if c < 0 then (* adding a1 *) aux acc r1 w2
          else if c > 0 then (* skip *) aux acc w1 r2
          else match f k1 v1 v2 with
            | None -> (* remove a1 *) aux ((rev_append_until a1 o1 res),r1) r1 r2
            | Some w -> if w==v1 then (* adding a1 *) aux acc r1 r2 
                else (* adding w *) aux (((k1,w)::(rev_append_until a1 o1 res)), r1) r1 r2
    in aux ([],w1) w1 w2

  (* good sharing with w1 *) 
  let union f w1 w2 = 
    let rec aux ((res,o1) as acc) w1 w2 =
      match w1 , w2 with
      | [] , _ -> (* adding w2 *) List.rev_append res (List.append o1 w2)
      | _ , [] -> (* adding w1 *) List.rev_append res o1
      | ((k1,v1) as a1)::r1 , ((k2,v2) as a2)::r2 ->
          let c = K.compare k1 k2 in
          if c < 0 then (* adding a1 *) aux acc r1 w2
          else if c = 0 then let w = f k1 v1 v2 in 
            if w==v1 then (* adding a1 *) aux acc r1 r2 
            else (* adding w *) aux (((k1,w)::(rev_append_until a1 o1 res)), r1) r1 r2
          else (* c > 0 *) (* adding a2 *) aux ((a2::(rev_append_until a1 o1 res)),w1) w1 r2
    in aux ([],w1) w1 w2

  let rec subset f w1 w2 =
    match w1 , w2 with
    | [] , _ -> true
    | _::_ , [] -> false
    | (k1,v1)::r1 , (k2,v2)::r2 ->
        let c = K.compare k1 k2 in
        if c < 0 then false else
        if c > 0 then subset f w1 r2 else
          f k1 v1 v2 && subset f r1 r2

  let rec equal eq w1 w2 =
    match w1 , w2 with
    | [] , [] -> true
    | [] , _::_ | _::_ , [] -> false
    | (k1,v1)::r1 , (k2,v2)::r2 ->
        w1==w2 || (K.equal k1 k2 && eq v1 v2 && equal eq r1 r2)

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

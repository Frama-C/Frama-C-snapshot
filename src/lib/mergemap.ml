(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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

(* $Id: mergemap.ml,v 1.3 2008-11-04 10:05:05 uid568 Exp $ *)

module type S =
  sig
    type key
    type +'a t
    val empty: 'a t
    val is_empty: 'a t -> bool
    val add: key -> 'a -> 'a t -> 'a t
    val find: key -> 'a t -> 'a
    val remove: key -> 'a t -> 'a t
    val mem:  key -> 'a t -> bool
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    val map: ('a -> 'b) -> 'a t -> 'b t
    val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val fold2: ?skip:('a t -> 'b t -> bool) -> 
      ('a option -> 'b option -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c
  end

module Make(Ord: Map.OrderedType) = struct

    type key = Ord.t

    type 'a t =
        Empty
      | Node of 'a t * key * 'a * 'a t * int

    let height = function
        Empty -> 0
      | Node(_,_,_,_,h) -> h

    let create l x d r =
      let hl = height l and hr = height r in
      Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

    let bal l x d r =
      let hl = match l with Empty -> 0 | Node(_,_,_,_,h) -> h in
      let hr = match r with Empty -> 0 | Node(_,_,_,_,h) -> h in
      if hl > hr + 2 then begin
        match l with
          Empty -> invalid_arg "Map.bal"
        | Node(ll, lv, ld, lr, _) ->
            if height ll >= height lr then
              create ll lv ld (create lr x d r)
            else begin
              match lr with
                Empty -> invalid_arg "Map.bal"
              | Node(lrl, lrv, lrd, lrr, _)->
                  create (create ll lv ld lrl) lrv lrd (create lrr x d r)
            end
      end else if hr > hl + 2 then begin
        match r with
          Empty -> invalid_arg "Map.bal"
        | Node(rl, rv, rd, rr, _) ->
            if height rr >= height rl then
              create (create l x d rl) rv rd rr
            else begin
              match rl with
                Empty -> invalid_arg "Map.bal"
              | Node(rll, rlv, rld, rlr, _) ->
                  create (create l x d rll) rlv rld (create rlr rv rd rr)
            end
      end else
        Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

    let empty = Empty

    let is_empty = function Empty -> true | _ -> false

    let rec add x data = function
        Empty ->
          Node(Empty, x, data, Empty, 1)
      | Node(l, v, d, r, h) as _t ->
          let c = Ord.compare x v in
          if c = 0 then
            Node(l, x, data, r, h)
          else if c < 0 then
            bal (add x data l) v d r
          else
            bal l v d (add x data r)

    let rec find x = function
        Empty ->
          raise Not_found
      | Node(l, v, d, r, _) ->
          let c = Ord.compare x v in
          if c = 0 then d
          else find x (if c < 0 then l else r)

    let rec mem x = function
        Empty ->
          false
      | Node(l, v, _d, r, _) ->
          let c = Ord.compare x v in
          c = 0 || mem x (if c < 0 then l else r)

    let rec min_binding = function
        Empty -> raise Not_found
      | Node(Empty, x, d, _r, _) -> (x, d)
      | Node(l, _x, _d, _r, _) -> min_binding l

    let rec remove_min_binding = function
        Empty -> invalid_arg "Map.remove_min_elt"
      | Node(Empty, _x, _d, r, _) -> r
      | Node(l, x, d, r, _) -> bal (remove_min_binding l) x d r

    let merge t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) ->
          let (x, d) = min_binding t2 in
          bal t1 x d (remove_min_binding t2)

    let rec remove x = function
        Empty ->
          Empty
      | Node(l, v, d, r, _h) as _t ->
          let c = Ord.compare x v in
          if c = 0 then
            merge l r
          else if c < 0 then
            bal (remove x l) v d r
          else
            bal l v d (remove x r)

    let rec iter f = function
        Empty -> ()
      | Node(l, v, d, r, _) ->
          iter f l; f v d; iter f r

    let rec map f = function
        Empty               -> Empty
      | Node(l, v, d, r, h) -> Node(map f l, v, f d, map f r, h)

    let rec mapi f = function
        Empty               -> Empty
      | Node(l, v, d, r, h) -> Node(mapi f l, v, f v d, mapi f r, h)

    (* Modified from Caml library : fold applies [f] to bindings in order. *)
    let rec fold f m accu =
      match m with
        Empty -> accu
      | Node(l, v, d, r, _) ->
          fold f r (f v d (fold f l accu))

 
    let fold2 ?(skip=fun _m1 _m2 -> false) f m1 m2 accu =
      (* Find the list of nodes to process to reach the min binding. *)
      let rec min_binding_zipper acc node = match node with
      | Empty -> acc
      | Node(l, _x, _d, _r, _) -> min_binding_zipper (node::acc) l
      in
      let rec min_binding_zipper_2 node1 acc1 node2 acc2 = 
          match node1,node2 with
        | Empty, _ -> acc1,min_binding_zipper acc2 node2
        | _, Empty -> min_binding_zipper acc1 node1,acc2
        | Node _, Node _ when skip node1 node2 -> acc1,acc2
        | Node(l1, x1, _d1, _r1, _), Node(l2, x2, _d2, _r2, _) ->
            let c = Ord.compare x1 x2 in
	    if c = 0
	    then min_binding_zipper_2 l1 (node1::acc1) l2 (node2::acc2)
	    else if c > 0 then
              min_binding_zipper_2 l1 (node1::acc1) node2 acc2
            else (* [c < 0] *) 
              min_binding_zipper_2 node1 acc1 l2 (node2::acc2)
      in
      let zip1,zip2 = min_binding_zipper_2 m1 [] m2 [] in
      let rec aux zip1 zip2 accu = 
        match zip1,zip2 with
        | [],[] -> accu
        | [], _::_ -> 
            let f acc n = match n with Node(_,_v,d,r,_)-> 
              let new_accu = f None (Some d) acc in
              fold 
                (fun _ d acc -> f None (Some d) acc)
                r 
                new_accu
            | _ -> assert false
            in 
            List.fold_left f accu zip2
        | _::_, [] -> 
            let f acc n = match n with 
            | Node(_,_v,d,r,_) -> 
                let new_accu = f (Some d) None acc in
                fold 
                  (fun _ d -> f (Some d) None)
                  r 
                  new_accu
            | _ -> assert false
            in
            List.fold_left f accu zip1
        | (Node(_l1, _v1, _d1, _r1, _) as m1)::zip1', 
            (Node(_l2, _v2, _d2, _r2, _) as m2)::zip2' when skip m1 m2 ->
            aux zip1' zip2' accu
        | (Node(_l1, v1, d1, r1, _) as _m1)::zip1', 
              (Node(_l2, v2, d2, r2, _) as _m2)::zip2' ->
            let c = Ord.compare v1 v2 in
	    if c = 0
	    then let newzip1,newzip2 = min_binding_zipper_2 r1 zip1' r2 zip2' in
            aux newzip1 newzip2 (f (Some d1) (Some d2) accu)
	    else if c > 0 then
              let newzip1 = zip1 in
              let newzip2 = min_binding_zipper zip2' r2 in
              aux newzip1 newzip2 (f None (Some d2) accu)
            else (* [c < 0] *) 
              let newzip2 = zip2 in
              let newzip1 = min_binding_zipper zip1' r1 in
              aux newzip1 newzip2 (f (Some d1) None accu)
        | Empty::_, _::_ | _::_, Empty::_ -> assert false
      in
      aux zip1 zip2 accu
          
    type 'a enumeration = End | More of key * 'a * 'a t * 'a enumeration

    let rec cons_enum m e =
      match m with
        Empty -> e
      | Node(l, v, d, r, _) -> cons_enum l (More(v, d, r, e))

    exception Value of int
    let mone = Value (-1)
    let one = Value 1
    let compare cmp m1 m2 =
(*      let aux x y acc = match x,y with
      | None, None -> assert false
      | Some _, None -> raise mone
      | None, Some _ -> raise one
      | Some d1, Some d2 -> 
          let c = cmp d1 d2 in
          if c<>0 then raise (Value c)
      in
      try fold2 ~skip:(==) aux m1 m2 (); 0
      with Value c -> c
*)        
      let rec compare_aux e1 e2 =
          match (e1, e2) with
          (End, End) -> 0
        | (End, _)  -> -1
        | (_, End) -> 1
        | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
            let c = Ord.compare v1 v2 in
            if c <> 0 then c else
            let c = cmp d1 d2 in
            if c <> 0 then c else
            compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
      in compare_aux (cons_enum m1 End) (cons_enum m2 End)
      

    let equal cmp m1 m2 =
      let rec equal_aux e1 e2 =
          match (e1, e2) with
          (End, End) -> true
        | (End, _)  -> false
        | (_, End) -> false
        | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
            Ord.compare v1 v2 = 0 && cmp d1 d2 &&
            equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
      in equal_aux (cons_enum m1 End) (cons_enum m2 End)
           
    (* let pretty fmt n = match n with
    | Empty -> Format.fprintf fmt "EMPTY"
    | Node (_,k,_,_,_) ->  
        Format.fprintf fmt "KEY:%d" (Obj.magic k)  *)
end


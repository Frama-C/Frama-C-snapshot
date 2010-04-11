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

(* modified by CEA *)

(* $Id: rangemap.ml,v 1.3 2008-11-04 10:05:05 uid568 Exp $ *)

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
    val hash: t -> int
    val descr: Unmarshal.t
  end

module type ValueType =
  sig
    type t
    val hash: t -> int
    val descr: Unmarshal.t
  end

type fuzzy_order = Above | Below | Match

module Make(Ord: OrderedType)(Value:ValueType) = struct

    type key = Ord.t

    type t =
        Empty
      | Node of t * key * Value.t * t * int * int
	  (* the last two are height and hash in this order *)

    let descr =
      let rec t_map =
	Unmarshal.Structure
	  (Unmarshal.Sum
	     [| [| t_map; Ord.descr; Value.descr; t_map;
		   Unmarshal.Abstract; Unmarshal.Abstract |] |] )
      in
      t_map

    let height = function
        Empty -> 0
      | Node(_,_,_,_,h,_) -> h

    let hash = function
	Empty -> 13
      | Node(_,_,_,_,_,h) -> h

    let create l x d r =
      let hl = height l and hr = height r in
      let hashl = hash l and hashr = hash r in
      let hashbinding = Hashtbl.hash (Ord.hash x, Value.hash d) in
      let hashtree = 289 (* =17*17 *) * hashl + 17 * hashbinding + hashr in
      Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1), hashtree)


    let bal l x d r =
      let hl = match l with Empty -> 0 | Node(_,_,_,_,h,_) -> h in
      let hr = match r with Empty -> 0 | Node(_,_,_,_,h,_) -> h in
      if hl > hr + 2 then begin
        match l with
          Empty -> invalid_arg "Rangemap.bal"
        | Node(ll, lv, ld, lr, _, _) ->
            if height ll >= height lr then
              create ll lv ld (create lr x d r)
            else begin
              match lr with
                Empty -> invalid_arg "Rangemap.bal"
              | Node(lrl, lrv, lrd, lrr, _, _)->
                  create (create ll lv ld lrl) lrv lrd (create lrr x d r)
            end
      end else if hr > hl + 2 then begin
        match r with
          Empty -> invalid_arg "Rangemap.bal"
        | Node(rl, rv, rd, rr, _, _) ->
            if height rr >= height rl then
              create (create l x d rl) rv rd rr
            else begin
              match rl with
                Empty -> invalid_arg "Rangemap.bal"
              | Node(rll, rlv, rld, rlr, _, _) ->
                  create (create l x d rll) rlv rld (create rlr rv rd rr)
            end
      end else
        create l x d r

    let empty = Empty

    let is_empty = function Empty -> true | _ -> false

    let rec add x data = function
        Empty ->
          create Empty x data Empty
      | Node(l, v, d, r, _, _) ->
          let c = Ord.compare x v in
          if c = 0 then
	    create l x data r
          else if c < 0 then
            bal (add x data l) v d r
          else
            bal l v d (add x data r)

   let rec find x = function
        Empty ->
          raise Not_found
      | Node(l, v, d, r, _, _) ->
          let c = Ord.compare x v in
          if c = 0 then d
          else find x (if c < 0 then l else r)

    let rec mem x = function
        Empty ->
          false
      | Node(l, v, _d, r, _, _) ->
          let c = Ord.compare x v in
          c = 0 || mem x (if c < 0 then l else r)

    let rec min_binding = function
        Empty -> raise Not_found
      | Node(Empty, x, d, _r, _, _) -> (x, d)
      | Node(l, _x, _d, _r, _, _) -> min_binding l

    let rec remove_min_binding = function
        Empty -> invalid_arg "Rangemap.remove_min_elt"
      | Node(Empty, _x, _d, r, _, _) -> r
      | Node(l, x, d, r, _, _) -> bal (remove_min_binding l) x d r

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
      | Node(l, v, d, r, _, _h) ->
          let c = Ord.compare x v in
          if c = 0 then
            merge l r
          else if c < 0 then
            bal (remove x l) v d r
          else
            bal l v d (remove x r)

    let rec iter f = function
        Empty -> ()
      | Node(l, v, d, r, _, _) ->
          iter f l; f v d; iter f r

    let rec map f = function
        Empty               -> Empty
      | Node(l, v, d, r, _, _h) -> create (map f l) v (f d) (map f r)
    let rec mapi f = function
        Empty               -> Empty
      | Node(l, v, d, r, _, _h) -> create (mapi f l) v (f v d) (mapi f r)

    let rec mapii f = function
      | Empty -> Empty
      | Node(l, v, d, r, _, _) ->
	  let new_v, new_d = f v d in
	  create (mapii f l) new_v new_d (mapii f r)

    (* Modified from Caml library : fold applies [f] to bindings in order. *)
    let rec fold f m accu =
      match m with
        Empty -> accu
      | Node(l, v, d, r, _, _) ->
          fold f r (f v d (fold f l accu))

    type enumeration = End | More of key * Value.t * t * enumeration

    let rec cons_enum m e =
      match m with
        Empty -> e
      | Node(l, v, d, r, _, _) -> cons_enum l (More(v, d, r, e))

    let compare cmp m1 m2 =
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

    let rec fold_range o f m accu =
      match m with
        Empty -> accu
      | Node(l, v, d, r, _, _) ->
	  let compar = o v in
	  let accu1 =
	    if compar = Match || compar = Above
	    then fold_range o f l accu
	    else accu
	  in
	  let accu2 =
	    if compar = Match
	    then f v d accu1
	    else accu1
	  in
	  if compar = Match || compar = Below
	  then fold_range o f r accu2
	  else accu2

    let cons k v l = (k,v) :: l

    let concerned_intervals fuzzy_order i m =
      fold_range (fuzzy_order i) cons m []

    let remove_whole fuzzy_order i m =
      fold_range (fuzzy_order i) (fun k _v acc -> remove k acc) m m

    let add_whole fuzzy_order i v m =
      let removed = remove_whole fuzzy_order i m
      in
      add i v removed

    exception Empty_rangemap

    let rec lowest_binding m =
      match m with
	Node(Empty,k,v,_,_, _) -> k,v
      | Node(t,_,_,_,_, _) -> lowest_binding t
      | Empty -> raise Empty_rangemap

    exception No_such_binding

    let rec lowest_binding_above o m =
      match m with
      | Node(l,k,v,r,_, _) ->
	  if o k
	  then begin
	    try
	      lowest_binding_above o l
	    with No_such_binding -> k,v
	  end
	  else lowest_binding_above o r
      | Empty -> raise No_such_binding

    let merge m1 m2 =
      fold (fun k v acc -> add k v acc) m1 m2

end

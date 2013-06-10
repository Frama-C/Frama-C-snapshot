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
(* --- Patricia Trees By L. Correnson                                     --- *)
(* -------------------------------------------------------------------------- *)

type 'a t = 
  | Empty
  | Lf of int * 'a
  | Br of int * int * 'a t * 'a t
      (* prefix , mask , 
	 sub-tree with prefix & mask = 0 , sub-tree with prefix & mask = 1 *)

(* -------------------------------------------------------------------------- *)
(* --- Debug                                                              --- *)
(* -------------------------------------------------------------------------- *)

let pp_mask m fmt p =
  begin
    let bits = Array.create 63 false in
    let last = ref 0 in
    for i = 0 to 62 do 
      let u = 1 lsl i in
      if u land p <> 0 then 
	bits.(i) <- true ;
      if u == m then last := i ;
    done ;
    Format.pp_print_char fmt '*' ;
    for i = !last - 1 downto 0 do
      Format.pp_print_char fmt (if bits.(i) then '1' else '0') ;
    done ;
  end

let pp_bits fmt k =
  begin
    let bits = Array.create 63 false in
    let last = ref 0 in
    for i = 0 to 62 do 
      if (1 lsl i) land k <> 0 then 
	( bits.(i) <- true ;
	  if i > !last then last := i ) ;
    done ;
    for i = !last downto 0 do
      Format.pp_print_char fmt (if bits.(i) then '1' else '0') ;
    done ;
  end

let rec pp_tree tab fmt = function
  | Empty -> ()
  | Lf(k,_) -> 
      Format.fprintf fmt "%sL%a@\n" tab pp_bits k
  | Br(p,m,l,r) ->
      let next = tab ^ "   " in
      pp_tree next fmt l ;
      Format.fprintf fmt "%s@@%a@\n" tab (pp_mask m) p ;
      pp_tree next fmt r

(* -------------------------------------------------------------------------- *)
(* --- Bit utilities                                                      --- *)
(* -------------------------------------------------------------------------- *)

let zero_bit k m = (k land m) == 0
let mask p m = p land (m-1)
let match_prefix k p m = (mask k m) == p

let lowest_bit x = x land (-x)
let branching_bit p0 p1 = lowest_bit (p0 lxor p1)

let included_mask m n = 
  (* m mask is strictly included into n *)
  (* can not use (m < n) when n is (1 lsl 62) = min_int < 0 *)
  (* must use (0 < (n-m) instead *)
  0 < ( n - m )

let included_prefix p m q n =
  (* prefix p1 with mask m1 in included into prefix p2 with mask m2 *)
  included_mask m n && match_prefix q p m

(* -------------------------------------------------------------------------- *)
(* --- Smart Constructors                                                 --- *)
(* -------------------------------------------------------------------------- *)

let empty = Empty

let lf k = function None -> Empty | Some x -> Lf(k,x)

let br p m t0 t1 = match t0 , t1 with
  | Empty,t | t,Empty -> t
  | _ -> Br(p,m,t0,t1)

let join p t0 q t1 =
  let m = branching_bit p q in
  let r = mask p m in
  if zero_bit p m then
    Br(r,m,t0,t1)
  else
    Br(r,m,t1,t0)

(* t0 and t1 has different prefix, but best common prefix is unknown *)
let glue t0 t1 = 
  match t0 , t1 with
    | Empty,t | t,Empty -> t
    | (Lf(p,_) | Br(p,_,_,_)) , (Lf(q,_) | Br(q,_,_,_)) -> join p t0 q t1
	
(* -------------------------------------------------------------------------- *)
(* --- Access API                                                         --- *)
(* -------------------------------------------------------------------------- *)

let is_empty = function
  | Empty -> true
  | Lf _ | Br _ -> false

let size t =
  let rec walk n = function
    | Empty -> n
    | Lf _ -> succ n
    | Br(_,_,a,b) -> walk (walk n a) b
  in walk 0 t

let rec mem k = function
  | Empty -> false
  | Lf(i,_) -> i=k
  | Br(p,m,t0,t1) -> 
      match_prefix k p m && mem k (if zero_bit k m then t0 else t1)

let rec find k = function
  | Empty -> raise Not_found
  | Lf(i,x) -> if k = i then x else raise Not_found
  | Br(p,m,t0,t1) ->
      if match_prefix k p m then
	find k (if zero_bit k m then t0 else t1)
      else
	raise Not_found

(* -------------------------------------------------------------------------- *)
(* --- Comparison                                                         --- *)
(* -------------------------------------------------------------------------- *)

let rec compare cmp s t =
  if s == t then 0 else
    match s , t with
      | Empty , Empty -> 0
      | Empty , _ -> (-1)
      | _ , Empty -> 1
      | Lf(i,x) , Lf(j,y) ->
	  let ck = Pervasives.compare i j in
	  if ck = 0 then cmp x y else ck
      | Lf _ , _ -> (-1)
      | _ , Lf _ -> 1
      | Br(p,m,s0,s1) , Br(q,n,t0,t1) ->
	  let cm = Pervasives.compare m n in
	  if cm <> 0 then cm else
	    let cp = Pervasives.compare p q in
	    if cp <> 0 then cp else
	      let c0 = compare cmp s0 t0 in
	      if c0 <> 0 then c0 else
		compare cmp s1 t1

let rec equal eq s t =
  if s == t then true else
    match s , t with
      | Empty , Empty -> true
      | Empty , _ -> false
      | _ , Empty -> false
      | Lf(i,x) , Lf(j,y) -> i == j && eq x y
      | Lf _ , _ -> false
      | _ , Lf _ -> false
      | Br(p,m,s0,s1) , Br(q,n,t0,t1) ->
	  p==q && m==n && equal eq s0 t0 && equal eq s1 t1

(* -------------------------------------------------------------------------- *)
(* --- Addition                                                           --- *)
(* -------------------------------------------------------------------------- *)

let rec add k x = function
  | Empty -> Lf(k,x)
  | Lf(i,_) as t -> 
      let s = Lf(k,x) in
      if k = i then s else join k s i t
  | Br(p,m,t0,t1) as t ->
      if match_prefix k p m then
	(* k belongs to tree *)
	if zero_bit k m 
	then Br(p,m,add k x t0,t1) (* k is in t0 *)
	else Br(p,m,t0,add k x t1) (* k is in t1 *)
      else
	(* k is disjoint from tree *)
	join k (Lf(k,x)) p t

(* -------------------------------------------------------------------------- *)
(* --- Remove                                                             --- *)
(* -------------------------------------------------------------------------- *)

let rec remove k = function
  | Empty -> Empty
  | Lf(i,_) as t -> if i = k then Empty else t
  | Br(p,m,t0,t1) as t ->
      if match_prefix k p m then
	(* k belongs to tree *)
	if zero_bit k m
	then br p m (remove k t0) t1 (* k is in t0 *)
	else br p m t0 (remove k t1) (* k is in t1 *)
      else t
	(* k disjoint from the tree *)

(* -------------------------------------------------------------------------- *)
(* --- Insert                                                             --- *)
(* -------------------------------------------------------------------------- *)

let rec insert phi k x = function
  | Empty -> Lf(k,x)
  | Lf(i,y) as t ->
      if i = k then Lf(i,phi i x y)
      else
	let s = Lf(k,x) in
	join k s i t
  | Br(p,m,t0,t1) as t ->
      if match_prefix k p m then
	(* k belongs to tree *)
	if zero_bit k m 
	then br p m (insert phi k x t0) t1 (* k is in t0 *)
	else br p m t0 (insert phi k x t1) (* k is in t1 *)
      else
	(* k is disjoint from tree *)
	let s = Lf(k,x) in
	join k s p t

(* -------------------------------------------------------------------------- *)
(* --- Map                                                                --- *)
(* -------------------------------------------------------------------------- *)

let rec map phi = function
  | Empty -> Empty
  | Lf(k,x) -> Lf(k,phi x)
  | Br(p,m,t0,t1) -> Br(p,m,map phi t0,map phi t1)

let rec mapi phi = function
  | Empty -> Empty
  | Lf(k,x) -> Lf(k,phi k x)
  | Br(p,m,t0,t1) -> Br(p,m,mapi phi t0,mapi phi t1)

let rec mapf phi = function
  | Empty -> Empty
  | Lf(k,x) -> lf k (phi k x)
  | Br(_,_,t0,t1) -> glue (mapf phi t0) (mapf phi t1)

let rec filter phi = function
  | Empty -> Empty
  | Lf(i,x) as t -> if phi i x then t else Empty
  | Br(_,_,t0,t1) -> glue (filter phi t0) (filter phi t1)

(* -------------------------------------------------------------------------- *)
(* --- Iter                                                               --- *)
(* -------------------------------------------------------------------------- *)

let rec iter phi = function
  | Empty -> ()
  | Lf(_,x) -> phi x
  | Br(_,_,t0,t1) -> iter phi t0 ; iter phi t1

let rec iteri phi = function
  | Empty -> ()
  | Lf(k,x) -> phi k x
  | Br(_,_,t0,t1) -> iteri phi t0 ; iteri phi t1

let rec fold phi t e = match t with
  | Empty -> e
  | Lf(_,x) -> phi x e
  | Br(_,_,t0,t1) -> fold phi t1 (fold phi t0 e)

let rec foldi phi t e = match t with
  | Empty -> e
  | Lf(i,x) -> phi i x e
  | Br(_,_,t0,t1) -> foldi phi t1 (foldi phi t0 e)

(* -------------------------------------------------------------------------- *)
(* --- Intersects                                                         --- *)
(* -------------------------------------------------------------------------- *)

let rec intersect s t =
  match s , t with
    | Empty , _ -> false
    | _ , Empty -> false
    | Lf(i,_) , Lf(j,_) -> i=j
    | Lf(i,_) , Br _ -> mem i t
    | Br _ , Lf(j,_) -> mem j s
    | Br(p,m,s0,s1) , Br(q,n,t0,t1) ->
	if p == q && m == n then
	  (* prefixes agree *)
	  (intersect s0 t0) || (intersect s1 t1)
	else if included_prefix p m q n then
	  (* q contains p. Intersect t with a subtree of s *)
	  if zero_bit q m 
	  then intersect s0 t (* t has bit m = 0 => t is inside s0 *)
	  else intersect s1 t (* t has bit m = 1 => t is inside s1 *)
	else if included_prefix q n p m then
	  (* p contains q. Intersect s with a subtree of t *)
	  if zero_bit p n
	  then intersect s t0 (* s has bit n = 0 => s is inside t0 *)
	  else intersect s t1 (* t has bit n = 1 => s is inside t1 *)
	else
	  (* prefix disagree *)
	  false

(* -------------------------------------------------------------------------- *)
(* --- Inter                                                              --- *)
(* -------------------------------------------------------------------------- *)

let occur i t = try Some (find i t) with Not_found -> None

let rec inter phi s t =
  match s , t with
    | Empty , _ -> Empty
    | _ , Empty -> Empty
    | Lf(i,x) , Lf(j,y) ->
	if i = j 
	then Lf(i,phi i x y) 
	else Empty
    | Lf(i,x) , Br _ -> 
	(match occur i t with None -> Empty | Some y -> Lf(i,phi i x y))
    | Br _ , Lf(j,y) ->
	(match occur j s with None -> Empty | Some x -> Lf(j,phi j x y))
    | Br(p,m,s0,s1) , Br(q,n,t0,t1) ->
	if p == q && m == n then
	  (* prefixes agree *)
	  glue (inter phi s0 t0) (inter phi s1 t1)
	else if included_prefix p m q n then
	  (* q contains p. Intersect t with a subtree of s *)
	  if zero_bit q m 
	  then inter phi s0 t (* t has bit m = 0 => t is inside s0 *)
	  else inter phi s1 t (* t has bit m = 1 => t is inside s1 *)
	else if included_prefix q n p m then
	  (* p contains q. Intersect s with a subtree of t *)
	  if zero_bit p n
	  then inter phi s t0 (* s has bit n = 0 => s is inside t0 *)
	  else inter phi s t1 (* t has bit n = 1 => s is inside t1 *)
	else
	  (* prefix disagree *)
	  Empty

(* -------------------------------------------------------------------------- *)
(* --- Union                                                              --- *)
(* -------------------------------------------------------------------------- *)

let rec union phi s t =
  match s , t with
    | Empty , _ -> t
    | _ , Empty -> s
    | Lf(i,x) , Lf(j,y) ->
	if i = j 
	then Lf(i,phi i x y) 
	else join i s j t
    | Lf(i,x) , Br _ -> insert phi i x t
    | Br _ , Lf(j,y) -> insert (fun j y x -> phi j x y) j y s
    | Br(p,m,s0,s1) , Br(q,n,t0,t1) ->
	if p == q && m == n then
	  (* prefixes agree *)
	  Br(p,m, union phi s0 t0 , union phi s1 t1 )
	else if included_prefix p m q n then
	  (* q contains p. Merge t with a subtree of s *)
	  if zero_bit q m 
	  then Br(p,m,union phi s0 t,s1) (* t has bit m = 0 => t is inside s0 *)
	  else Br(p,m,s0,union phi s1 t) (* t has bit m = 1 => t is inside s1 *)
	else if included_prefix q n p m then
	  (* p contains q. Merge s with a subtree of t *)
	  if zero_bit p n
	  then Br(q,n,union phi s t0,t1) (* s has bit n = 0 => s is inside t0 *)
	  else Br(q,n,t0,union phi s t1) (* t has bit n = 1 => s is inside t1 *)
	else
	  (* prefix disagree *)
	  join p s q t

(* -------------------------------------------------------------------------- *)
(* --- Merge                                                              --- *)
(* -------------------------------------------------------------------------- *)

let map1 phi s = mapf (fun i x -> phi i (Some x) None) s
let map2 phi t = mapf (fun j y -> phi j None (Some y)) t

let rec merge phi s t =
  match s , t with
    | Empty , _ -> map2 phi t
    | _ , Empty -> map1 phi s
    | Lf(i,x) , Lf(j,y) ->
	if i = j then lf i (phi i (Some x) (Some y))
	else
	  let a = lf i (phi i (Some x) None) in
	  let b = lf j (phi j None (Some y)) in
	  glue a b

    | Lf(i,x) , Br(q,n,t0,t1) ->
	if match_prefix i q n then
	  (* leaf i is in tree t *)
	  if zero_bit i n 
	  then glue (merge phi s t0) (map2 phi t1) (* s=i is in t0 *)
	  else glue (map2 phi t0) (merge phi s t1) (* s=i is in t1 *)
	else
	  (* leaf i does not appear in t *)
	  glue (lf i (phi i (Some x) None)) (map2 phi t)

    | Br(p,m,s0,s1) , Lf(j,y) ->
	if match_prefix j p m then
	  (* leaf j is in tree s *)
	  if zero_bit j m
	  then glue (merge phi s0 t) (map1 phi s1) (* t=j is in s0 *)
	  else glue (map1 phi s0) (merge phi s1 t) (* t=j is in s1 *)
	else
	  (* leaf j does not appear in s *)
	  glue (map1 phi s) (lf j (phi j None (Some y)))

    | Br(p,m,s0,s1) , Br(q,n,t0,t1) ->
	if p == q && m == n then
	  (* prefixes agree *)
	  glue (merge phi s0 t0) (merge phi s1 t1)
	else if included_prefix p m q n then
	  (* q contains p. Merge t with a subtree of s *)
	  if zero_bit q m
	  then (* t has bit m = 0 => t is inside s0 *)
	    glue (merge phi s0 t) (map1 phi s1) 
	  else (* t has bit m = 1 => t is inside s1 *)
	    glue (map1 phi s0) (merge phi s1 t)
	else if included_prefix q n p m then
	  (* p contains q. Merge s with a subtree of t *)
	  if zero_bit p n
	  then (* s has bit n = 0 => s is inside t0 *)
	    glue (merge phi s t0) (map2 phi t1)
	  else (* s has bit n = 1 => s is inside t1 *)
	    glue (map2 phi t0) (merge phi s t1)
	else
	  glue (map1 phi s) (map2 phi t)

(* -------------------------------------------------------------------------- *)
(* --- Iter Kernel                                                        --- *)
(* -------------------------------------------------------------------------- *)

let rec iterk phi s t =
  match s , t with
    | Empty , _ | _ , Empty -> ()
    | Lf(i,x) , Lf(j,y) -> if i = j then phi i x y
    | Lf(i,x) , Br _ -> 
	(match occur i t with None -> () | Some y -> phi i x y)
    | Br _ , Lf(j,y) ->
	(match occur j s with None -> () | Some x -> phi j x y)
    | Br(p,m,s0,s1) , Br(q,n,t0,t1) ->
	if p == q && m == n then
	  (* prefixes agree *)
	  (iterk phi s0 t0 ; iterk phi s1 t1)
	else if included_prefix p m q n then
	  (* q contains p. Intersect t with a subtree of s *)
	  if zero_bit q m 
	  then iterk phi s0 t (* t has bit m = 0 => t is inside s0 *)
	  else iterk phi s1 t (* t has bit m = 1 => t is inside s1 *)
	else if included_prefix q n p m then
	  (* p contains q. Intersect s with a subtree of t *)
	  if zero_bit p n
	  then iterk phi s t0 (* s has bit n = 0 => s is inside t0 *)
	  else iterk phi s t1 (* t has bit n = 1 => s is inside t1 *)
	else
	  (* prefix disagree *)
	  ()

(* -------------------------------------------------------------------------- *)
(* --- Iter2                                                              --- *)
(* -------------------------------------------------------------------------- *)

let iter21 phi s = iteri (fun i x -> phi i (Some x) None) s
let iter22 phi t = iteri (fun j y -> phi j None (Some y)) t

let rec iter2 phi s t =
  match s , t with
    | Empty , _ -> iter22 phi t
    | _ , Empty -> iter21 phi s
    | Lf(i,x) , Lf(j,y) ->
	if i = j then phi i (Some x) (Some y)
	else ( phi i (Some x) None ; phi j None (Some y) )

    | Lf(i,x) , Br(q,n,t0,t1) ->
	if match_prefix i q n then
	  (* leaf i is in tree t *)
	  if zero_bit i n 
	  then (iter2 phi s t0 ; iter22 phi t1) (* s=i is in t0 *)
	  else (iter22 phi t0 ; iter2 phi s t1) (* s=i is in t1 *)
	else
	  (* leaf i does not appear in t *)
	  (phi i (Some x) None ; iter22 phi t)

    | Br(p,m,s0,s1) , Lf(j,y) ->
	if match_prefix j p m then
	  (* leaf j is in tree s *)
	  if zero_bit j m
	  then (iter2 phi s0 t ; iter21 phi s1) (* t=j is in s0 *)
	  else (iter21 phi s0 ; iter2 phi s1 t) (* t=j is in s1 *)
	else
	  (* leaf j does not appear in s *)
	  (iter21 phi s ; phi j None (Some y))

    | Br(p,m,s0,s1) , Br(q,n,t0,t1) ->
	if p == q && m == n then
	  (* prefixes agree *)
	  (iter2 phi s0 t0 ; iter2 phi s1 t1)
	else if included_prefix p m q n then
	  (* q contains p. Merge t with a subtree of s *)
	  if zero_bit q m
	  then (* t has bit m = 0 => t is inside s0 *)
	    (iter2 phi s0 t ; iter21 phi s1) 
	  else (* t has bit m = 1 => t is inside s1 *)
	    (iter21 phi s0 ; iter2 phi s1 t)
	else if included_prefix q n p m then
	  (* p contains q. Merge s with a subtree of t *)
	  if zero_bit p n
	  then (* s has bit n = 0 => s is inside t0 *)
	    (iter2 phi s t0 ; iter22 phi t1)
	  else (* s has bit n = 1 => s is inside t1 *)
	    (iter22 phi t0 ; iter2 phi s t1)
	else
	  (iter21 phi s ; iter22 phi t)

(* -------------------------------------------------------------------------- *)
(* --- Subset                                                             --- *)
(* -------------------------------------------------------------------------- *)

let rec subset phi s t =
  match s , t with
    | Empty , _ -> true
    | _ , Empty -> false
    | Lf(i,x) , Lf(j,y) -> if i = j then phi i x y else false
    | Lf(i,x) , Br _ -> 
	(match occur i t with None -> false | Some y -> phi i x y)
    | Br _ , Lf _ -> false
    | Br(p,m,s0,s1) , Br(q,n,t0,t1) ->
	if p == q && m == n then
	  (* prefixes agree *)
	  (subset phi s0 t0 && subset phi s1 t1)
	else if included_prefix p m q n then
	  (* q contains p: t is included in a (strict) subtree of s *)
	  false
	else if included_prefix q n p m then
	  (* p contains q: s is included in a subtree of t *)
	  if zero_bit p n
	  then subset phi s t0 (* s has bit n = 0 => s is inside t0 *)
	  else subset phi s t1 (* t has bit n = 1 => s is inside t1 *)
	else
	  (* prefix disagree *)
	  false

(* -------------------------------------------------------------------------- *)

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
  include Set.S
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

  type t =
    | Empty
    | Leaf of int * E.t
    | Branch of int * int * t * t

  type elt = E.t
	
  let empty = Empty
    
  let is_empty = function Empty -> true | _ -> false
    
  let singleton e = Leaf( E.id e , e )

  let zero_bit k m = (k land m) == 0
    
  let rec mem_k k = function
    | Empty -> false
    | Leaf(j,_) -> k == j
    | Branch (_, m, l, r) -> mem_k k (if zero_bit k m then l else r)

  let mem e s = mem_k (E.id e) s

  let lowest_bit x = x land (-x)
    
  let branching_bit p0 p1 = lowest_bit (p0 lxor p1)
    
  let mask p m = p land (m-1)
    
  let join p0 t0 p1 t1 =
    let m = branching_bit p0 p1 in
    if zero_bit p0 m then 
      Branch (mask p0 m, m, t0, t1)
    else 
      Branch (mask p0 m, m, t1, t0)

  let match_prefix k p m = (mask k m) == p

  let rec insert k e = function
    | Empty -> Leaf(k,e)
    | Leaf(j,_) as t -> 
	if j == k then t else join k (Leaf(k,e)) j t
    | Branch (p,m,t0,t1) as t ->
	if match_prefix k p m then
	  if zero_bit k m then 
	      Branch (p, m, insert k e t0, t1)
	  else
	    Branch (p, m, t0, insert k e t1)
	else
	  join k (Leaf(k,e)) p t
    
  let add e t = insert (E.id e) e t

  let branch p m t0 t1 =
    match t0,t1 with 
      | Empty,t | t,Empty -> t
      | _ -> Branch (p,m,t0,t1)

  let rec remove_k k = function
    | Empty -> Empty
    | Leaf(j,_) as t -> if k == j then Empty else t
    | Branch (p,m,t0,t1) as t -> 
	if match_prefix k p m then
	  if zero_bit k m then
	    branch p m (remove_k k t0) t1
	  else
	    branch p m t0 (remove_k k t1)
	else
	  t
	    
  let remove e t = remove_k (E.id e) t

  let rec union a b =
    match a,b with
      | Empty, t  -> t
      | t, Empty  -> t
      | Leaf(k,e), t -> insert k e t
      | t, Leaf(k,e) -> insert k e t
      | (Branch (p,m,s0,s1) as s), (Branch (q,n,t0,t1) as t) ->
	  if m == n && match_prefix q p m then
	    (* The trees have the same prefix. Merge the subtrees. *)
	    Branch (p, m, union s0 t0, union s1 t1)
	  else if m < n && match_prefix q p m then
	    (* [q] contains [p]. Merge [t] with a subtree of [s]. *)
	    if zero_bit q m then 
	      Branch (p, m, union s0 t, s1)
            else 
	      Branch (p, m, s0, union s1 t)
	  else if m > n && match_prefix p q n then
	    (* [p] contains [q]. Merge [s] with a subtree of [t]. *)
	    if zero_bit p n then
	      Branch (q, n, union s t0, t1)
	    else
	      Branch (q, n, t0, union s t1)
	  else
	    (* The prefixes disagree. *)
	    join p s q t

  let rec subset s1 s2 = match (s1,s2) with
    | Empty, _ -> true
    | _, Empty -> false
    | Leaf(k1,_), _ -> mem_k k1 s2
    | Branch _, Leaf _ -> false
    | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	if m1 == m2 && p1 == p2 then
	  subset l1 l2 && subset r1 r2
	else if m1 > m2 && match_prefix p1 p2 m2 then
	  if zero_bit p1 m2 then 
	    subset l1 l2 && subset r1 l2
	  else 
	    subset l1 r2 && subset r1 r2
	else
	  false

  let rec inter s1 s2 = 
    match (s1,s2) with
      | Empty, _ -> Empty
      | _, Empty -> Empty
      | Leaf(k1,_), _ -> if mem_k k1 s2 then s1 else Empty
      | _, Leaf(k2,_) -> if mem_k k2 s1 then s2 else Empty
      | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	  if m1 == m2 && p1 == p2 then 
	    union (inter l1 l2) (inter r1 r2)
	  else if m1 < m2 && match_prefix p2 p1 m1 then
	    inter (if zero_bit p2 m1 then l1 else r1) s2
	  else if m1 > m2 && match_prefix p1 p2 m2 then
	    inter s1 (if zero_bit p1 m2 then l2 else r2)
	  else
	    Empty
	      
  let rec diff s1 s2 = 
    match (s1,s2) with
      | Empty, _ -> Empty
      | _, Empty -> s1
      | Leaf(k1,_), _ -> if mem_k k1 s2 then Empty else s1
      | _, Leaf(k2,_) -> remove_k k2 s1
      | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	  if m1 == m2 && p1 == p2 then
	    union (diff l1 l2) (diff r1 r2)
	  else if m1 < m2 && match_prefix p2 p1 m1 then
	    if zero_bit p2 m1 then 
	      union (diff l1 s2) r1
	    else 
	      union l1 (diff r1 s2)
	  else if m1 > m2 && match_prefix p1 p2 m2 then
	    if zero_bit p1 m2 then diff s1 l2 else diff s1 r2
	  else
	    s1

  let rec cardinal = function
    | Empty -> 0
    | Leaf _ -> 1
    | Branch (_,_,t0,t1) -> cardinal t0 + cardinal t1

  let rec iter f = function
    | Empty -> ()
    | Leaf(_,e) -> f e
    | Branch (_,_,t0,t1) -> iter f t0; iter f t1
      
  let rec iteri f = function
    | Empty -> ()
    | Leaf(k,e) -> f k e
    | Branch (_,_,t0,t1) -> iteri f t0; iteri f t1

  let rec fold f s accu = match s with
    | Empty -> accu
    | Leaf(_,e) -> f e accu
    | Branch (_,_,t0,t1) -> fold f t0 (fold f t1 accu)

  let map f s = fold (fun e s -> add (f e) s) s Empty
  let mapf f s = fold (fun e s -> match f e with None -> s | Some e -> add e s) s Empty

  let rec for_all p = function
    | Empty -> true
    | Leaf(_,e) -> p e
    | Branch (_,_,t0,t1) -> for_all p t0 && for_all p t1

  let rec exists p = function
    | Empty -> false
    | Leaf(_,e) -> p e
    | Branch (_,_,t0,t1) -> exists p t0 || exists p t1

  let rec filter pr = function
    | Empty -> Empty
    | Leaf(_,e) as t -> if pr e then t else Empty
    | Branch (p,m,t0,t1) -> branch p m (filter pr t0) (filter pr t1)

  let partition p s =
    let t = ref Empty in
    let f = ref Empty in
    iteri 
      (fun k e -> 
	 let r = if p e then t else f in
	 r := insert k e !r) s ;
    !t , !f

  let rec choose = function
    | Empty -> raise Not_found
    | Leaf(_,e) -> e
    | Branch (_, _,t0,_) -> choose t0   (* we know that [t0] is non-empty *)
	
  let e_compare a b = Pervasives.compare (E.id a) (E.id b)
  let e_min a b = if e_compare a b <=0 then a else b
  let e_max a b = if e_compare a b <=0 then b else a

  let elements s =
    let rec elements_aux acc = function
    | Empty -> acc
    | Leaf(_,e) -> e :: acc
    | Branch (_,_,l,r) -> elements_aux (elements_aux acc l) r
    in
    (* unfortunately there is no easy way to get the elements in ascending
       order with little-endian Patricia trees *)
    List.sort e_compare (elements_aux [] s)

  let split x s =
    let coll k (l, b, r) =
      if k < x then add k l, b, r
      else if k > x then l, b, add k r
      else l, true, r 
    in
    fold coll s (Empty, false, Empty)

  (*s There is no way to give an efficient implementation of [min_elt]
    and [max_elt], as with binary search trees.  The following
    implementation is a traversal of all elements, barely more
    efficient than [fold min t (choose t)] (resp. [fold max t (choose
    t)]). Note that we use the fact that there is no constructor
    [Empty] under [Branch] and therefore always a minimal
    (resp. maximal) element there. *)
      
  let rec min_elt = function
    | Empty -> raise Not_found
    | Leaf(_,e) -> e
    | Branch (_,_,s,t) -> e_min (min_elt s) (min_elt t)
	
  let rec max_elt = function
    | Empty -> raise Not_found
    | Leaf(_,e) -> e
    | Branch (_,_,s,t) -> e_max (max_elt s) (max_elt t)

  let rec equal a b = 
    match a,b with
      | Empty , Empty -> true
      | Empty , _ | _ , Empty -> false
      | Leaf(i,_) , Leaf(j,_) -> i=j
      | Leaf _ , _ | _ , Leaf _ -> false
      | Branch(p1,m1,l1,r1) , Branch(p2,m2,l2,r2) ->
	  p1 = p2 && m1 = m2 && equal l1 l2 && equal r1 r2
    
  let rec compare a b =
    match a,b with
      | Empty , Empty -> 0
      | Empty , _ -> (-1)
      | _ , Empty -> 1
      | Leaf(i,_) , Leaf(j,_) -> Pervasives.compare i j
      | Leaf _ , _ -> (-1)
      | _ , Leaf _ -> 1
      | Branch(p1,m1,l1,r1) , Branch(p2,m2,l2,r2) ->
	  let c = Pervasives.compare p1 p2 in
	  if c<>0 then c else
	    let c = Pervasives.compare m1 m2 in
	    if c<>0 then c else
	      let c = compare l1 l2 in
	      if c<>0 then c else
		compare r1 r2

  let rec intersect s1 s2 = match (s1,s2) with
    | Empty, _ -> false
    | _, Empty -> false
    | Leaf(k1,_), _ -> mem_k k1 s2
    | _, Leaf(k2,_) -> mem_k k2 s1
    | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	if m1 == m2 && p1 == p2 then
          intersect l1 l2 || intersect r1 r2
	else if m1 < m2 && match_prefix p2 p1 m1 then
          intersect (if zero_bit p2 m1 then l1 else r1) s2
	else if m1 > m2 && match_prefix p1 p2 m2 then
          intersect s1 (if zero_bit p1 m2 then l2 else r2)
	else
          false

end


module Positive(E : IndexedElements) =
struct

  type t =
    | Empty
    | Leaf of int * E.t
    | Branch of int * int * t * t

  type elt = E.t
	
  let empty = Empty
    
  let is_empty = function Empty -> true | _ -> false
    
  let singleton e = Leaf( E.id e , e )

  let index e = let k = E.id e in assert (k >= 0) ; k
    
  let singleton e = Leaf( index e , e )

  let zero_bit k m = (k land m) == 0

  let rec mem_k k = function
    | Empty -> false
    | Leaf(j,_) -> k == j
    | Branch (p, _, l, r) -> if k <= p then mem_k k l else mem_k k r

  let rec min_elt = function
    | Empty -> raise Not_found
    | Leaf(_,e) -> e
    | Branch (_,_,s,_) -> min_elt s

  let rec max_elt = function
    | Empty -> raise Not_found
    | Leaf(_,e) -> e
    | Branch (_,_,_,t) -> max_elt t

  let mem e s = mem_k (index e) s

  let mask k m  = (k lor (m-1)) land (lnot m)

  (* we first write a naive implementation of [highest_bit] 
     only has to work for bytes *)
  let naive_highest_bit x = 
    assert (x < 256);
    let rec loop i = 
      if i = 0 then 1 else if x lsr i = 1 then 1 lsl i else loop (i-1)
    in
    loop 7

  (* then we build a table giving the highest bit for bytes *)
  let hbit = Array.init 256 naive_highest_bit
  
  (* to determine the highest bit of [x] we split it into bytes *)
  let highest_bit_32 x =
    let n = x lsr 24 in if n != 0 then hbit.(n) lsl 24
    else let n = x lsr 16 in if n != 0 then hbit.(n) lsl 16
    else let n = x lsr 8 in if n != 0 then hbit.(n) lsl 8
    else hbit.(x)

  let highest_bit_64 x =
    let n = x lsr 32 in if n != 0 then (highest_bit_32 n) lsl 32
    else highest_bit_32 x

  let highest_bit = match Sys.word_size with
    | 32 -> highest_bit_32
    | 64 -> highest_bit_64
    | _ -> assert false

  let branching_bit p0 p1 = highest_bit (p0 lxor p1)

  let join p0 t0 p1 t1 =
    let m = branching_bit p0 p1 in
    if zero_bit p0 m then 
      Branch (mask p0 m, m, t0, t1)
    else 
      Branch (mask p0 m, m, t1, t0)
    
  let match_prefix k p m = (mask k m) == p

  let rec insert k e = function
    | Empty -> Leaf(k,e)
    | Leaf(j,_) as t -> 
	if j == k then t else join k (Leaf(k,e)) j t
    | Branch (p,m,t0,t1) as t ->
	if match_prefix k p m then
	  if zero_bit k m then 
	    Branch (p, m, insert k e t0, t1)
	  else
	    Branch (p, m, t0, insert k e t1)
	else
	  join k (Leaf(k,e)) p t
		    
  let add e t = insert (index e) e t

  let branch p m t0 t1 =
    match t0,t1 with 
      | Empty,t | t,Empty -> t
      | _ -> Branch (p,m,t0,t1)
      
  let rec remove_k k = function
    | Empty -> Empty
    | Leaf(j,_) as t -> if k == j then Empty else t
    | Branch (p,m,t0,t1) as t -> 
	if match_prefix k p m then
	  if zero_bit k m then
	    branch p m (remove_k k t0) t1
	  else
	    branch p m t0 (remove_k k t1)
	else
	  t
	    
  let remove e t = remove_k (index e) t 
      
  let rec union a b =
    match a,b with
      | Empty, t  -> t
      | t, Empty  -> t
      | Leaf(k,e), t -> insert k e t
      | t, Leaf(k,e) -> insert k e t
      | (Branch (p,m,s0,s1) as s), (Branch (q,n,t0,t1) as t) ->
	  if m == n && match_prefix q p m then
	    (* The trees have the same prefix. Merge the subtrees. *)
	    Branch (p, m, union s0 t0, union s1 t1)
	  else if m > n && match_prefix q p m then
	    (* [q] contains [p]. Merge [t] with a subtree of [s]. *)
	    if zero_bit q m then 
	      Branch (p, m, union s0 t, s1)
            else 
	      Branch (p, m, s0, union s1 t)
	  else if m < n && match_prefix p q n then
	    (* [p] contains [q]. Merge [s] with a subtree of [t]. *)
	    if zero_bit p n then
	      Branch (q, n, union s t0, t1)
	    else
	      Branch (q, n, t0, union s t1)
	  else
	    (* The prefixes disagree. *)
	    join p s q t

  let rec subset s1 s2 = match (s1,s2) with
    | Empty, _ -> true
    | _, Empty -> false
    | Leaf(k1,_), _ -> mem_k k1 s2
    | Branch _, Leaf _ -> false
    | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	if m1 == m2 && p1 == p2 then
	  subset l1 l2 && subset r1 r2
	else if m1 < m2 && match_prefix p1 p2 m2 then
	  if zero_bit p1 m2 then 
	    subset l1 l2 && subset r1 l2
	  else 
	    subset l1 r2 && subset r1 r2
	else
	  false

  let rec inter s1 s2 = match (s1,s2) with
    | Empty, _ -> Empty
    | _, Empty -> Empty
    | Leaf(k1,_), _ -> if mem_k k1 s2 then s1 else Empty
    | _, Leaf(k2,_) -> if mem_k k2 s1 then s2 else Empty
    | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	if m1 == m2 && p1 == p2 then 
	  union (inter l1 l2) (inter r1 r2)
	else if m1 > m2 && match_prefix p2 p1 m1 then
	  inter (if zero_bit p2 m1 then l1 else r1) s2
	else if m1 < m2 && match_prefix p1 p2 m2 then
	  inter s1 (if zero_bit p1 m2 then l2 else r2)
	else
	  Empty
	    
  let rec diff s1 s2 = match (s1,s2) with
    | Empty, _ -> Empty
    | _, Empty -> s1
    | Leaf(k1,_), _ -> if mem_k k1 s2 then Empty else s1
    | _, Leaf(k2,_) -> remove_k k2 s1
    | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	if m1 == m2 && p1 == p2 then
	  union (diff l1 l2) (diff r1 r2)
	else if m1 > m2 && match_prefix p2 p1 m1 then
	  if zero_bit p2 m1 then 
	    union (diff l1 s2) r1
	  else 
	    union l1 (diff r1 s2)
	else if m1 < m2 && match_prefix p1 p2 m2 then
	  if zero_bit p1 m2 then diff s1 l2 else diff s1 r2
	else
	  s1

  let rec cardinal = function
    | Empty -> 0
    | Leaf _ -> 1
    | Branch (_,_,t0,t1) -> cardinal t0 + cardinal t1

  let rec equal a b = 
    match a,b with
      | Empty , Empty -> true
      | Empty , _ | _ , Empty -> false
      | Leaf(i,_) , Leaf(j,_) -> i=j
      | Leaf _ , _ | _ , Leaf _ -> false
      | Branch(p1,m1,l1,r1) , Branch(p2,m2,l2,r2) ->
	  p1 = p2 && m1 = m2 && equal l1 l2 && equal r1 r2
    
  let rec compare a b =
    match a,b with
      | Empty , Empty -> 0
      | Empty , _ -> (-1)
      | _ , Empty -> 1
      | Leaf(i,_) , Leaf(j,_) -> Pervasives.compare i j
      | Leaf _ , _ -> (-1)
      | _ , Leaf _ -> 1
      | Branch(p1,m1,l1,r1) , Branch(p2,m2,l2,r2) ->
	  let c = Pervasives.compare p1 p2 in
	  if c<>0 then c else
	    let c = Pervasives.compare m1 m2 in
	    if c<>0 then c else
	      let c = compare l1 l2 in
	      if c<>0 then c else
		compare r1 r2

  let rec iter f = function
    | Empty -> ()
    | Leaf(_,e) -> f e
    | Branch (_,_,t0,t1) -> iter f t0; iter f t1
      
  let rec iteri f = function
    | Empty -> ()
    | Leaf(k,e) -> f k e
    | Branch (_,_,t0,t1) -> iteri f t0; iteri f t1

  let rec fold f s accu = match s with
    | Empty -> accu
    | Leaf(_,e) -> f e accu
    | Branch (_,_,t0,t1) -> fold f t0 (fold f t1 accu)

  let map f s = fold (fun e s -> add (f e) s) s Empty
  let mapf f s = fold (fun e s -> match f e with None -> s | Some e -> add e s) s Empty

  let rec for_all p = function
    | Empty -> true
    | Leaf(_,e) -> p e
    | Branch (_,_,t0,t1) -> for_all p t0 && for_all p t1

  let rec exists p = function
    | Empty -> false
    | Leaf(_,e) -> p e
    | Branch (_,_,t0,t1) -> exists p t0 || exists p t1

  let rec filter pr = function
    | Empty -> Empty
    | Leaf(_,e) as t -> if pr e then t else Empty
    | Branch (p,m,t0,t1) -> branch p m (filter pr t0) (filter pr t1)

  let partition p s =
    let t = ref Empty in
    let f = ref Empty in
    iteri 
      (fun k e -> 
	 let r = if p e then t else f in
	 r := insert k e !r) s ;
    !t , !f

  let rec choose = function
    | Empty -> raise Not_found
    | Leaf(_,e) -> e
    | Branch (_, _,t0,_) -> choose t0   (* we know that [t0] is non-empty *)

  let elements s =
    let rec elements_aux acc = function
      | Empty -> acc
      | Leaf(_,e) -> e :: acc
      | Branch (_,_,l,r) -> elements_aux (elements_aux acc r) l
    in
    elements_aux [] s

  let split x s =
    let coll k (l, b, r) =
      if k < x then add k l, b, r
      else if k > x then l, b, add k r
      else l, true, r 
    in
    fold coll s (Empty, false, Empty)

  let rec intersect s1 s2 = match (s1,s2) with
    | Empty, _ -> false
    | _, Empty -> false
    | Leaf(k1,_) , _ -> mem_k k1 s2
    | _ , Leaf(k2,_) -> mem_k k2 s1
    | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	if m1 == m2 && p1 == p2 then
          intersect l1 l2 || intersect r1 r2
	else if m1 > m2 && match_prefix p2 p1 m1 then
          intersect (if zero_bit p2 m1 then l1 else r1) s2
	else if m1 < m2 && match_prefix p1 p2 m2 then
          intersect s1 (if zero_bit p1 m2 then l2 else r2)
	else
          false

end

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

(* ---------------------------------------------------------------------- *)
(* --- Patricia Trees By L. Correnson & P. Baudin                     --- *)
(* ---------------------------------------------------------------------- *)

type 'a t = 
  | Empty
  | Lf of int * 'a
  | Br of int * 'a t * 'a t

(* -------------------------------------------------------------------------- *)
(* --- Bit library                                                        --- *)
(* -------------------------------------------------------------------------- *)

let hsb =
  let hsb p = if p land 2 != 0 then 1 else 0
  in let hsb p = let n = p lsr  2 in if n != 0 then 2 + hsb n else hsb p  
  in let hsb p = let n = p lsr  4 in if n != 0 then 4 + hsb n else hsb p 
  in let hsb = Array.init 256 hsb
  in let hsb p = let n = p lsr  8 in if n != 0 then  8 + hsb.(n) else hsb.(p)
  in let hsb p = let n = p lsr 16 in if n != 0 then 16 + hsb n else hsb p
  in match Sys.word_size with
  | 32 -> hsb
  | 64 -> (function p -> let n = p lsr 32 in if n != 0 then 32 + hsb n else hsb p)
  | _ -> assert false

let highest_bit x = 1 lsl (hsb x)
let lowest_bit x = x land (-x)

let decode_mask p = lowest_bit (lnot p)

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
      Format.fprintf fmt "%sL%a=%d@\n" tab pp_bits k k
  | Br(p,l,r) ->
      let next = tab ^ "   " in
      pp_tree next fmt l ;
      Format.fprintf fmt "%s@@%a@\n" tab (pp_mask (decode_mask p)) p ;
      pp_tree next fmt r

(* -------------------------------------------------------------------------- *)
(* --- Bit utilities                                                      --- *)
(* -------------------------------------------------------------------------- *)
let decode_mask p = lowest_bit (lnot p)

let branching_bit p0 p1 = highest_bit (p0 lxor p1)
let mask p m = (p lor (m-1)) land (lnot m)

let zero_bit_int k m = (k land m) == 0
let zero_bit k p = zero_bit_int k (decode_mask p)

let match_prefix_int k p m = (mask k m) == p
let match_prefix k p = match_prefix_int k p (decode_mask p)

let included_mask_int m n = 
  (* m mask is strictly included into n *)
  (* can not use (m < n) when n is (1 lsl 62) = min_int < 0 *)
  (* must use (0 < (n-m) instead *)
  0 > n - m
let included_mask p q = included_mask_int (decode_mask p) (decode_mask q) 

let included_prefix p q =
  let m = decode_mask p in
  let n = decode_mask q in
  included_mask_int m n && match_prefix_int q p m

(* -------------------------------------------------------------------------- *)
(* --- Smart Constructors                                                 --- *)
(* -------------------------------------------------------------------------- *)

let empty = Empty

let singleton k x = Lf(k,x)

let lf k = function None -> Empty | Some x -> Lf(k,x)

let br p t0 t1 = match t0 , t1 with
  | Empty,t | t,Empty -> t
  | _ -> Br(p,t0,t1)

(* good sharing *) 
let lf0 k x' t' = function None -> Empty | Some x -> if x == x' then t' else Lf(k,x)

(* good sharing *) 
let br0 p t0' t1' t' = function
  | Empty -> t1'
  | t0 -> if t0' == t0 then t' else Br(p,t0,t1')

(* good sharing *) 
let br1 p t0' t1' t' = function
  | Empty -> t0'
  | t1 -> if t1' == t1 then t' else Br(p,t0',t1)

let join p t0 q t1 =
  let m = branching_bit p q in
  let r = mask p m in
  if zero_bit p r
  then Br(r,t0,t1)
  else Br(r,t1,t0)

(* t0 and t1 has different prefix, but best common prefix is unknown *)
let glue t0 t1 = 
  match t0 , t1 with
  | Empty,t | t,Empty -> t
  | (Lf(p,_) | Br(p,_,_)) , (Lf(q,_) | Br(q,_,_)) -> join p t0 q t1

let glue0 t0 t0' t1' t' = 
  if t0 == t0' then t' else glue t0 t1'

let glue1 t1 t0' t1' t' = 
  if t1 == t1' then t' else glue t0' t1

let glue01 t0 t1 t0' t1' t' = 
  if t0 == t0' && t1 == t1' then t' else glue t0 t1

let glue2 t0 t1 t0' t1' t' s0' s1' s' = 
  if t0 == s0' && t1 == s1' then s' else
  if t0 == t0' && t1 == t1' then t' else glue t0 t1

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
    | Br(_,a,b) -> walk (walk n a) b
  in walk 0 t

let rec mem k = function
  | Empty -> false
  | Lf(i,_) -> i=k
  | Br(p,t0,t1) -> 
      match_prefix k p && mem k (if zero_bit k p then t0 else t1)

let rec findq k = function
  | Empty -> raise Not_found
  | Lf(i,x) as t -> if k = i then (x,t) else raise Not_found
  | Br(p,t0,t1) ->
      if match_prefix k p then
        findq k (if zero_bit k p then t0 else t1)
      else
        raise Not_found

let find k m = fst (findq k m)

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
    | Br(p,s0,s1) , Br(q,t0,t1) ->
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
    | Br(p,s0,s1) , Br(q,t0,t1) ->
        p==q && equal eq s0 t0 && equal eq s1 t1

(* -------------------------------------------------------------------------- *)
(* --- Addition, Insert, Change, Remove                                   --- *)
(* -------------------------------------------------------------------------- *)

(* good sharing *) 
let rec change phi k x = function
  | Empty as t -> (match phi k x None with
      | None -> t
      | Some w -> Lf(k,w))
  | Lf(i,y) as t ->
      if i = k then 
        lf0 k y t (phi k x (Some y))
      else
        (match phi k x None with
         | None -> t
         | Some w -> let s = Lf(k,w) in
             join k s i t)
  | Br(p,t0,t1) as t ->
      if match_prefix k p then
        (* k belongs to tree *)
        if zero_bit k p
        then br0 p t0 t1 t (change phi k x t0) (* k is in t0 *)
        else br1 p t0 t1 t (change phi k x t1) (* k is in t1 *)
      else
        (* k is disjoint from tree *)
        (match phi k x None with
         | None -> t
         | Some w -> let s = Lf(k,w) in
             join k s p t)

(* good sharing *) 
let insert f k x = change (fun _k x -> function
    | None -> Some x 
    | Some old -> Some (f k x old)) k x

(* good sharing *) 
let add k x = change (fun _k x _old -> Some x) k x

(* good sharing *) 
let remove k = change (fun _k () _old -> None) k ()

(* -------------------------------------------------------------------------- *)
(* --- Map                                                                --- *)
(* -------------------------------------------------------------------------- *)

let mapi phi =
  let rec mapi phi = function
    | Empty   -> Empty
    | Lf(k,x) -> Lf(k,phi k x)
    | Br(p,t0,t1) -> 
        let t0 = mapi phi t0 in
        let t1 = mapi phi t1 in
        Br(p,t0,t1)
  in function (* to be sorted *)
    | Empty   -> Empty
    | Lf(k,x) -> Lf(k,phi k x)
    | Br(p,t0,t1) when p = max_int -> let t1 = mapi phi t1 in 
        let t0 = mapi phi t0 in Br(p,t0,t1)
    | Br(p,t0,t1)                  -> let t0 = mapi phi t0 in 
        let t1 = mapi phi t1 in Br(p,t0,t1)
let map phi = mapi (fun _ x -> phi x)

let mapf phi =
  let rec mapf phi = function
    | Empty   -> Empty
    | Lf(k,x) -> lf k (phi k x)
    | Br(_,t0,t1) -> glue (mapf phi t0) (mapf phi t1)
  in function (* to be sorted *)
    | Empty   -> Empty
    | Lf(k,x) -> lf k (phi k x)
    | Br(p,t0,t1) when p = max_int -> let t1 = mapf phi t1 in 
        let t0 = mapf phi t0 in glue t0 t1
    | Br(_,t0,t1)                  -> let t0 = mapf phi t0 in 
        let t1 = mapf phi t1 in glue t0 t1

(* good sharing *) 
let mapq phi =
  let rec mapq phi = function
    | Empty as t -> t
    | Lf(k,x) as t -> lf0 k x t (phi k x)
    | Br(_,t0,t1) as t-> 
        let t0' = mapq phi t0 in
        let t1' = mapq phi t1 in
        glue01 t0' t1' t0 t1 t
  in function (* to be sorted *)
    | Empty as t -> t
    | Lf(k,x) as t -> lf0 k x t (phi k x)
    | Br(p,t0,t1) as t when p = max_int -> 
        let t1' = mapq phi t1 in
        let t0' = mapq phi t0 in
        glue01 t0' t1' t0 t1 t
    | Br(_,t0,t1) as t-> 
        let t0' = mapq phi t0 in
        let t1' = mapq phi t1 in
        glue01 t0' t1' t0 t1 t

(* good sharing *) 
let filter f m = mapq (fun k v -> if f k v then Some v else None) m

(* good sharing *) 
let rec partition p = function
  | Empty as t -> (t,t)
  | Lf(k,x) as t -> if p k x then t,Empty else Empty,t
  | Br(_,t0,t1) as t-> 
      let (t0',u0') = partition p t0 in
      let (t1',u1') = partition p t1 in
      if t0'==t0 && t1'==t1 then (t, u0') (* u0' and u1' are empty *)
      else if u0'==t0 && u1'==t1 then (t0', t) (* t0' and t1' are empty *)
      else (glue t0' t1'),(glue u0' u1')

(* good sharing *) 
let rec partition_split p = function
  | Empty as t -> (t,t)
  | Lf(k,x) as t -> let u,v = p k x in (lf0 k x t u), (lf0 k x t v)
  | Br(_,t0,t1) as t-> 
      let t0',u0' = partition_split p t0 in
      let t1',u1' = partition_split p t1 in
      if t0'==t0 && t1'==t1 then (t, u0') (* u0' and u1' are empty *)
      else if u0'==t0 && u1'==t1 then (t0', t) (* t0' and t1' are empty *)
      else (glue t0' t1'),(glue u0' u1')

(* -------------------------------------------------------------------------- *)
(* --- Iter                                                               --- *)
(* -------------------------------------------------------------------------- *)

let iteri phi =
  let rec aux = function
    | Empty -> ()
    | Lf(k,x) -> phi k x
    | Br(_,t0,t1) -> aux t0 ; aux t1
  in function (* to be sorted *)
    | Empty -> ()
    | Lf(k,x) -> phi k x
    | Br(p,t0,t1) when p = max_int -> aux t1 ; aux t0
    | Br(_,t0,t1)                  -> aux t0 ; aux t1

let iter phi = iteri (fun _ x -> phi x)

let foldi phi t e = (* increasing order *)
  let rec aux t e = match t with
    | Empty -> e
    | Lf(i,x) -> phi i x e
    | Br(_,t0,t1) -> aux t1 (aux t0 e)
  in match t with (* to be sorted *)
  | Empty -> e
  | Lf(i,x) -> phi i x e
  | Br(p,t0,t1) when p = max_int -> aux t0 (aux t1 e)
  | Br(_,t0,t1)                  -> aux t1 (aux t0 e)

let fold phi = foldi (fun _ x e -> phi x e)

let foldd phi t e = (* decreasing order *)
  let rec aux t e = match t with
    | Empty -> e
    | Lf(i,x) -> phi i x e
    | Br(_,t0,t1) -> aux t0 (aux t1 e)
  in match t with (* to be sorted *)
  | Empty -> e
  | Lf(i,x) -> phi i x e
  | Br(p,t0,t1) when p = max_int -> aux t1 (aux t0 e)
  | Br(_,t0,t1)                  -> aux t0 (aux t1 e)

(* decreasing order on f to have the list in increasing order *)
let mapl f m = foldd (fun k v a -> (f k v)::a) m []

let for_all phi = (* increasing order *)
  let rec aux = function
    | Empty -> true
    | Lf(k,x) -> phi k x
    | Br(_,t0,t1) -> aux t0 && aux t1
  in function (* to be sorted *)
    | Empty -> true
    | Lf(k,x) -> phi k x
    | Br(p,t0,t1) when p = max_int -> aux t1 && aux t0
    | Br(_,t0,t1)                  -> aux t0 && aux t1

let exists phi = (* increasing order *)
  let rec aux = function
    | Empty -> false
    | Lf(k,x) -> phi k x
    | Br(_,t0,t1) -> aux t0 || aux t1
  in function (* to be sorted *)
    | Empty -> false
    | Lf(k,x) -> phi k x
    | Br(p,t0,t1) when p = max_int -> aux t1 || aux t0
    | Br(_,t0,t1)                  -> aux t0 || aux t1

(* -------------------------------------------------------------------------- *)
(* --- Inter                                                              --- *)
(* -------------------------------------------------------------------------- *)

let occur i t = try Some (find i t) with Not_found -> None

let rec interi lf_phi s t =
  match s , t with
  | Empty , _ -> Empty
  | _ , Empty -> Empty
  | Lf(i,x) , Lf(j,y) ->
      if i = j 
      then lf_phi i x y 
      else Empty
  | Lf(i,x) , Br _ -> 
      (match occur i t with None -> Empty | Some y -> lf_phi i x y)
  | Br _ , Lf(j,y) ->
      (match occur j s with None -> Empty | Some x -> lf_phi j x y)
  | Br(p,s0,s1) , Br(q,t0,t1) ->
      if p == q then
        (* prefixes agree *)
        glue (interi lf_phi s0 t0) (interi lf_phi s1 t1)
      else if included_prefix p q then
        (* q contains p. Intersect t with a subtree of s *)
        if zero_bit q p
        then interi lf_phi s0 t (* t has bit m = 0 => t is inside s0 *)
        else interi lf_phi s1 t (* t has bit m = 1 => t is inside s1 *)
      else if included_prefix q p then
        (* p contains q. Intersect s with a subtree of t *)
        if zero_bit p q
        then interi lf_phi s t0 (* s has bit n = 0 => s is inside t0 *)
        else interi lf_phi s t1 (* t has bit n = 1 => s is inside t1 *)
      else
        (* prefix disagree *)
        Empty

let inter phi = interi (fun i x y -> Lf(i,phi i x y))
let interf phi = interi (fun i x y -> lf i (phi i x y))

(* good sharing with s  *)
let lfq phi i x y s t = match phi i x y with None -> Empty | Some w -> if w == x then s else if w == y then t else Lf(i,w)
let occur0 phi i x s t = try let (y,t) = findq i t in lfq phi i x y s t with Not_found -> Empty
let occur1 phi j y s t = try let (x,s) = findq j s in lfq phi j x y s t with Not_found -> Empty

(* good sharing with s *) 
let rec interq phi s t =
  match s , t with
  | Empty , _ -> s
  | _ , Empty -> t
  | Lf(i,x) , Lf(j,y) ->
      if i = j 
      then lfq phi i x y s t
      else Empty
  | Lf(i,x) , Br _ -> occur0 phi i x s t
  | Br _ , Lf(j,y) -> occur1 phi j y s t
  | Br(p,s0,s1) , Br(q,t0,t1) ->
      if p == q then
        (* prefixes agree *)
        glue2 (interq phi s0 t0) (interq phi s1 t1) s0 s1 s t0 t1 t
      else if included_prefix p q then
        (* q contains p. Intersect t with a subtree of s *)
        if zero_bit q p
        then interq phi s0 t (* t has bit m = 0 => t is inside s0 *)
        else interq phi s1 t (* t has bit m = 1 => t is inside s1 *)
      else if included_prefix q p then
        (* p contains q. Intersect s with a subtree of t *)
        if zero_bit p q
        then interq phi s t0 (* s has bit n = 0 => s is inside t0 *)
        else interq phi s t1 (* t has bit n = 1 => s is inside t1 *)
      else
        (* prefix disagree *)
        Empty

(* -------------------------------------------------------------------------- *)
(* --- Union                                                              --- *)
(* -------------------------------------------------------------------------- *)

(* good sharing with s *)
let br2u p s0' s1' s' t0' t1' t' t0 t1=
  if s0'==t0 && s1'== t1 then s' else 
  if t0'==t0 && t1'== t1 then t' else 
    Br(p, t0, t1)

(* good sharing with s *)
let br0u p t0' t1' t' t0 = if t0'==t0 then t' else Br(p, t0, t1')
let br1u p t0' t1' t' t1 = if t1'==t1 then t' else Br(p, t0', t1)

(* good sharing with s *) 
let rec union phi s t =
  match s , t with
  | Empty , _ -> t
  | _ , Empty -> s
  | Lf(i,x) , Lf(j,y) ->
      if i = j 
      then let w = phi i x y in
        if w == x then s else if w == y then t else Lf(i,w) 
      else join i s j t
  | Lf(i,x) , Br _ -> insert phi i x t
  | Br _ , Lf(j,y) -> insert (fun j y x -> phi j x y) j y s
  | Br(p,s0,s1) , Br(q,t0,t1) ->
      if p == q then
        (* prefixes agree *)
        br2u p s0 s1 s t0 t1 t (union phi s0 t0) (union phi s1 t1)
      else if included_prefix p q then
        (* q contains p. Merge t with a subtree of s *)
        if zero_bit q p
        then br0u p s0 s1 s (union phi s0 t) (* t has bit m = 0 => t is inside s0 *)
        else br1u p s0 s1 s (union phi s1 t) (* t has bit m = 1 => t is inside s1 *)
      else if included_prefix q p then
        (* p contains q. Merge s with a subtree of t *)
        if zero_bit p q
        then br0u q t0 t1 t (union phi s t0) (* s has bit n = 0 => s is inside t0 *)
        else br1u q t0 t1 t (union phi s t1) (* t has bit n = 1 => s is inside t1 *)
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

  | Lf(i,x) , Br(q,t0,t1) ->
      if match_prefix i q then
        (* leaf i is in tree t *)
        if zero_bit i q
        then glue (merge phi s t0) (map2 phi t1) (* s=i is in t0 *)
        else glue (map2 phi t0) (merge phi s t1) (* s=i is in t1 *)
      else
        (* leaf i does not appear in t *)
        glue (lf i (phi i (Some x) None)) (map2 phi t)

  | Br(p,s0,s1) , Lf(j,y) ->
      if match_prefix j p then
        (* leaf j is in tree s *)
        if zero_bit j p
        then glue (merge phi s0 t) (map1 phi s1) (* t=j is in s0 *)
        else glue (map1 phi s0) (merge phi s1 t) (* t=j is in s1 *)
      else
        (* leaf j does not appear in s *)
        glue (map1 phi s) (lf j (phi j None (Some y)))

  | Br(p,s0,s1) , Br(q,t0,t1) ->
      if p == q then
        (* prefixes agree *)
        glue (merge phi s0 t0) (merge phi s1 t1)
      else if included_prefix p q then
        (* q contains p. Merge t with a subtree of s *)
        if zero_bit q p
        then (* t has bit m = 0 => t is inside s0 *)
          glue (merge phi s0 t) (map1 phi s1) 
        else (* t has bit m = 1 => t is inside s1 *)
          glue (map1 phi s0) (merge phi s1 t)
      else if included_prefix q p then
        (* p contains q. Merge s with a subtree of t *)
        if zero_bit p q
        then (* s has bit n = 0 => s is inside t0 *)
          glue (merge phi s t0) (map2 phi t1)
        else (* s has bit n = 1 => s is inside t1 *)
          glue (map2 phi t0) (merge phi s t1)
      else
        glue (map1 phi s) (map2 phi t)

(* good sharing with s *) 
let rec diffq phi s t =
  match s , t with
  | Empty , _ -> s
  | _ , Empty -> s
  | Lf(i,x) , Lf(j,y) ->
      if i = j 
      then lfq phi i x y s t
      else s
  | Lf(i,x) , Br _ ->
      (match occur i t with None -> s | Some y -> lfq phi i x y s t)
  | Br _ , Lf(j,y) -> change (fun j y x -> match x with None -> None | Some x -> phi j x y) j y s
  | Br(p,s0,s1) , Br(q,t0,t1) ->
      if p == q then
        (* prefixes agree *)
        let t0' = (diffq phi s0 t0) in
        let t1' = (diffq phi s1 t1) in
        glue01 t0' t1' s0 s1 s
      else if included_prefix p q then
        (* q contains p. *)
        if zero_bit q p
        then (* t has bit m = 0 => t is inside s0 *)
          let s0' = (diffq phi s0 t) in
          glue0 s0' s0 s1 s
        else (* t has bit m = 1 => t is inside s1 *)
          let s1' = (diffq phi s1 t) in
          glue1 s1' s0 s1 s
      else if included_prefix q p then
        (* p contains q. *)
        if zero_bit p q
        then diffq phi s t0 (* s has bit n = 0 => s is inside t0 *)
        else diffq phi s t1 (* t has bit n = 1 => s is inside t1 *)
      else
        (* prefix disagree *)
        s

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
  | Br(p,s0,s1) , Br(q,t0,t1) ->
      if p == q then
        (* prefixes agree *)
        (iterk phi s0 t0 ; iterk phi s1 t1)
      else if included_prefix p q then
        (* q contains p. Intersect t with a subtree of s *)
        if zero_bit q p
        then iterk phi s0 t (* t has bit m = 0 => t is inside s0 *)
        else iterk phi s1 t (* t has bit m = 1 => t is inside s1 *)
      else if included_prefix q p then
        (* p contains q. Intersect s with a subtree of t *)
        if zero_bit p q
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

  | Lf(i,x) , Br(q,t0,t1) ->
      if match_prefix i q then
        (* leaf i is in tree t *)
        if zero_bit i q
        then (iter2 phi s t0 ; iter22 phi t1) (* s=i is in t0 *)
        else (iter22 phi t0 ; iter2 phi s t1) (* s=i is in t1 *)
      else
        (* leaf i does not appear in t *)
        (phi i (Some x) None ; iter22 phi t)

  | Br(p,s0,s1) , Lf(j,y) ->
      if match_prefix j p then
        (* leaf j is in tree s *)
        if zero_bit j p
        then (iter2 phi s0 t ; iter21 phi s1) (* t=j is in s0 *)
        else (iter21 phi s0 ; iter2 phi s1 t) (* t=j is in s1 *)
      else
        (* leaf j does not appear in s *)
        (iter21 phi s ; phi j None (Some y))

  | Br(p,s0,s1) , Br(q,t0,t1) ->
      if p == q then
        (* prefixes agree *)
        (iter2 phi s0 t0 ; iter2 phi s1 t1)
      else if included_prefix p q then
        (* q contains p. Merge t with a subtree of s *)
        if zero_bit q p
        then (* t has bit m = 0 => t is inside s0 *)
          (iter2 phi s0 t ; iter21 phi s1) 
        else (* t has bit m = 1 => t is inside s1 *)
          (iter21 phi s0 ; iter2 phi s1 t)
      else if included_prefix q p then
        (* p contains q. Merge s with a subtree of t *)
        if zero_bit p q
        then (* s has bit n = 0 => s is inside t0 *)
          (iter2 phi s t0 ; iter22 phi t1)
        else (* s has bit n = 1 => s is inside t1 *)
          (iter22 phi t0 ; iter2 phi s t1)
      else
        (iter21 phi s ; iter22 phi t)

(* -------------------------------------------------------------------------- *)
(* --- Intersects                                                         --- *)
(* -------------------------------------------------------------------------- *)

let rec intersectf phi s t =
  match s , t with
  | Empty , _ -> false
  | _ , Empty -> false
  | Lf(i,x) , Lf(j,y) -> if i = j then phi i x y else false
  | Lf(i,x) , Br _ -> (match occur i t with None -> false | Some y -> phi i x y)
  | Br _ , Lf(j,y) -> (match occur j s with None -> false | Some x -> phi j x y)
  | Br(p,s0,s1) , Br(q,t0,t1) ->
      if p == q then
        (* prefixes agree *)
        (intersectf phi s0 t0) || (intersectf phi s1 t1)
      else if included_prefix p q then
        (* q contains p. Intersect t with a subtree of s *)
        if zero_bit q p
        then intersectf phi s0 t (* t has bit m = 0 => t is inside s0 *)
        else intersectf phi s1 t (* t has bit m = 1 => t is inside s1 *)
      else if included_prefix q p then
        (* p contains q. Intersect s with a subtree of t *)
        if zero_bit p q
        then intersectf phi s t0 (* s has bit n = 0 => s is inside t0 *)
        else intersectf phi s t1 (* t has bit n = 1 => s is inside t1 *)
      else
        (* prefix disagree *)
        false

let intersect s t = intersectf (fun _i _x _y -> true) s t
(* -------------------------------------------------------------------------- *)
(* --- Subset                                                             --- *)
(* -------------------------------------------------------------------------- *)

let rec subsetf phi s t =
  match s , t with
  | Empty , _ -> true
  | _ , Empty -> false
  | Lf(i,x) , Lf(j,y) -> if i = j then phi i x y else false
  | Lf(i,x) , Br _ -> 
      (match occur i t with None -> false | Some y -> phi i x y)
  | Br _ , Lf _ -> false
  | Br(p,s0,s1) , Br(q,t0,t1) ->
      if p == q then
        (* prefixes agree *)
        (subsetf phi s0 t0 && subsetf phi s1 t1)
      else if included_prefix p q then
        (* q contains p: t is included in a (strict) subtree of s *)
        false
      else if included_prefix q p then
        (* p contains q: s is included in a subtree of t *)
        if zero_bit p q
        then subsetf phi s t0 (* s has bit n = 0 => s is inside t0 *)
        else subsetf phi s t1 (* t has bit n = 1 => s is inside t1 *)
      else
        (* prefix disagree *)
        false

let subset = subsetf
let subsetk s t = subsetf (fun _i _x _y -> true) s t
(* -------------------------------------------------------------------------- *)

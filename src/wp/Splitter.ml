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
(* --- Base Type for Splitting                                            --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Cil_datatype

type tag =
  | MARK of stmt
  | THEN of stmt
  | ELSE of stmt
  | CALL of stmt * kernel_function
  | CASE of stmt * int64 list
  | DEFAULT of stmt
  | ASSERT of identified_predicate * int * int (* part *)

let pretty fmt = function
  | MARK _ -> Format.fprintf fmt "Stmt"
  | THEN _ -> Format.fprintf fmt "Then"
  | ELSE _ -> Format.fprintf fmt "Else"
  | CASE(_,[]) -> Format.fprintf fmt "Case(s)"
  | CASE(_,[k]) -> Format.fprintf fmt "Case %s" (Int64.to_string k)
  | CASE(_,k::ks) -> 
      Format.fprintf fmt "@[Cases %s" (Int64.to_string k) ;
      List.iter (fun k -> Format.fprintf fmt ",@,%s" (Int64.to_string k)) ks ;
      Format.fprintf fmt "@]"
  | CALL(_,kf) -> Format.fprintf fmt "Call %a" Kernel_function.pretty kf
  | DEFAULT _ -> Format.fprintf fmt "Default"
  | ASSERT(_,k,n) -> Format.fprintf fmt "Disjunction (%d/%d)" k n

let loc = function
  | THEN s | ELSE s | MARK s | CASE(s,_) | CALL(s,_) | DEFAULT s -> Stmt.loc s
  | ASSERT(p,_,_) -> p.ip_loc

let compare p q =
  if p == q then 0 else
    match p,q with
      | MARK s , MARK t -> Stmt.compare s t
      | MARK _ , _ -> (-1)
      | _ , MARK _ -> 1
      | THEN s , THEN t -> Stmt.compare s t
      | THEN _ , _ -> (-1)
      | _ , THEN _ -> 1
      | ELSE s , ELSE t -> Stmt.compare s t
      | ELSE _ , _ -> (-1)
      | _ , ELSE _ -> 1
      | CASE(s1,k1) , CASE(s2,k2) -> 
	  let c = Stmt.compare s1 s2 in
	  if c = 0 then Pervasives.compare k1 k2 else c
      | CASE _ , _ -> (-1)
      | _ , CASE _ -> 1
      | DEFAULT s , DEFAULT t -> Stmt.compare s t
      | DEFAULT _ , _ -> (-1)
      | _ , DEFAULT _ -> 1
      | CALL(s1,f1) , CALL(s2,f2) ->
	  let c = Stmt.compare s1 s2 in
	  if c = 0 then Kernel_function.compare f1 f2 else c
      | CALL _ , _ -> (-1)
      | _ , CALL _ -> 1
      | ASSERT(ip1,k1,_) , ASSERT(ip2,k2,_) -> 
	  let c = Pervasives.compare ip1.ip_id ip2.ip_id in
	  if c = 0 then k1 - k2 else c

(* -------------------------------------------------------------------------- *)
(* --- Assertion Disjunction                                              --- *)
(* -------------------------------------------------------------------------- *)

let rec disjunction p =
 try unwrap p 
 with Exit -> [p]

and unwrap p =
  match p.content with
    | Por(a,b) -> disjunction a @ disjunction b
    | Plet(f,a) -> 
	List.map 
	  (fun q -> { p with content = Plet(f,q) }) 
	  (unwrap a)
    | Pexists(qs,p) ->
	List.map
	  (fun q -> { p with content = Pexists(qs,q) })
	  (unwrap p)
    | Pat(p,l) ->
	List.map
	  (fun q -> { p with content = Pat(q,l) })
	  (unwrap p)
    | _ -> raise Exit

let predicate ip = 
  { name = ip.ip_name ; loc = ip.ip_loc ; content = ip.ip_content }

let rec enumerate ip k n = function
  | [] -> []
  | p::ps -> (ASSERT(ip,k,n),p) :: enumerate ip (succ k) n ps
  
let cases ip =
  try 
    let ps = unwrap (predicate ip) in
    Some (enumerate ip 1 (List.length ps) ps)
  with Exit -> None

(* -------------------------------------------------------------------------- *)
(* --- Switch Cases                                                       --- *)
(* -------------------------------------------------------------------------- *)

let switch_cases stmt ks = CASE(stmt,ks)
let switch_default stmt = DEFAULT stmt
let if_then stmt = THEN stmt
let if_else stmt = ELSE stmt
let mark stmt = MARK stmt
let call stmt kf = CALL(stmt,kf)

(* -------------------------------------------------------------------------- *)
(* --- Switch Cases                                                       --- *)
(* -------------------------------------------------------------------------- *)

module Tags = Qed.Listset.Make
  (struct 
     type t = tag 
     let compare = compare 
     let equal x y = (compare x y = 0)
   end)
module M = Qed.Listmap.Make(Tags)
module I = FCMap.Make(Tags)

type 'a t = 'a M.t

let rec compact merge = function
  | ([] | [_]) as m -> m
  | ( (k1,v1) as e )::(( (k2,v2)::r ) as m) ->
      if Tags.compare k1 k2 = 0 then
	collect merge k1 [v2;v1] r
      else
	e :: compact merge m
and collect merge k vs = function
  | [] -> [k,merge vs]
  | ((k',v')::r) as m ->
      if Tags.compare k k' = 0 then
	collect merge k (v'::vs) r
      else
	(k,merge vs) :: compact merge m

let bytags (k,_) (k',_) = Tags.compare k k'

let group tag merge m = 
  let compaction = ref false in
  let m = List.sort bytags 
    (List.map 
       (fun (tgs,v) -> 
	  if not !compaction && Tags.mem tag tgs then compaction := true ;
	  Tags.add tag tgs , v) m) 
  in if !compaction then compact merge m else m

(* let filter phi m = M.filter (fun key _ -> phi key) m *)

let length = List.length
let empty = []
let singleton e = [[],e]
let union merge m1 m2 = M.union (fun _ -> merge) m1 m2

let rec merge ~left ~both ~right m1 m2 =
  match m1 , m2 with
    | [],[] -> []
    | _,[] -> List.map (fun (k,v) -> k , left v) m1
    | [],_ -> List.map (fun (k,v) -> k , right v) m2
    | (k1,v1)::w1 , (k2,v2)::w2 ->
	let cmp = Tags.compare k1 k2 in
	if cmp < 0 then
	  (k1 , left v1) :: merge ~left ~both ~right w1 m2
	else if cmp > 0 then
	  (k2 , right v2) :: merge ~left ~both ~right m1 w2
	else
	  (k1 , both v1 v2) :: merge ~left ~both ~right w1 w2

let merge_all merge = function
  | [] -> []
  | [m] -> m
  | [m1;m2] -> M.union (fun _ u v -> merge [u;v]) m1 m2
  | ms ->
      let t = ref I.empty in
      List.iter 
	(List.iter 
	   (fun (k,v) -> 
	      try 
		let r = (I.find k !t) in r := v :: !r
	      with Not_found ->
		t := I.add k (ref [v]) !t))
	ms ;
      I.fold
	(fun k r m -> match !r with
	   | [] -> m
	   | [v] -> (k,v)::m
	   | vs -> (k,merge vs)::m)
	!t []

let map = M.map
let iter = M.iter
let fold = M.fold

let exists f xs = List.exists (fun (_,x) -> f x) xs
let for_all f xs = List.for_all (fun (_,x) -> f x) xs
let filter f xs = List.filter (fun (_,x) -> f x) xs


(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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

let lift_keep op x y = match x , y with 
  | None , z | z , None -> z
  | Some u , Some v -> Some (op u v)
      
let lift_none op x y = match x , y with
  | None , _ | _ , None -> None
  | Some u , Some v -> Some (op u v)

let lift op = function None -> None | Some x -> Some (op x)

type t = Bot | Range of Z.t option * Z.t option 

let pretty fmt = function
  | Bot -> Format.pp_print_string fmt "[empty]"
  | Range(None,None) -> Format.pp_print_string fmt "[..]"
  | Range(Some a,None) -> Format.fprintf fmt "[%a..]" Z.pretty a
  | Range(None,Some a) -> Format.fprintf fmt "[..%a]" Z.pretty a
  | Range(Some a,Some b) -> 
      if Z.equal a b then 
	Format.fprintf fmt "[%a]" Z.pretty a 
      else
	Format.fprintf fmt "[%a..%a]" Z.pretty a Z.pretty b
  
let top = Range(None,None)
let bot = Bot
let value a = let u = Some a in Range(u,u)
let range a b = if Z.leq a b then Range (Some a , Some b) else Bot
let drange a b = 
  match a,b with
    | None,_ | _,None -> Range(a,b)
    | Some x,Some y -> if Z.leq x y then Range(a,b) else Bot
	
let inf_to a = Range(None,Some a)
let sup_to a = Range(Some a,None)

let lower = function Bot -> Bot | Range(a,_) -> Range(a,None)
let upper = function Bot -> Bot | Range(_,a) -> Range(None,a)

let union r1 r2 =
  match r1 , r2 with
    | Bot , r | r , Bot -> r
    | Range(a1,b1) , Range(a2,b2) -> 
	let a = lift_none Z.min a1 a2 in
	let b = lift_none Z.max b1 b2 in
	drange a b
	  
let inter r1 r2 =
  match r1 , r2 with
    | Bot , _ | _ , Bot -> Bot
    | Range(a1,b1) , Range(a2,b2) ->
	let a = lift_keep Z.max a1 a2 in
	let b = lift_keep Z.min b1 b2 in
	drange a b

(* None is -oo *)
let inf_leq x y = match x , y with
  | None , _ -> true
  | Some _ , None -> false
  | Some x , Some y -> Z.leq x y 

(* None is +oo *)
let sup_leq x y = match x , y with
  | _ , None -> true
  | None , Some _ -> false
  | Some x , Some y -> Z.leq x y

let subset r1 r2 =
  match r1 , r2 with
    | Bot , _ -> true
    | Range _ , Bot -> false
    | Range(a1,b1) , Range(a2,b2) -> inf_leq a2 a1 && sup_leq b1 b2

let leq r1 r2 =
  match r1 , r2 with
    | Bot , _ | _ , Bot -> true
    | Range(_,Some a) , Range(Some b,_) -> Z.leq a b
    | _ -> false	

let lt r1 r2 =
  match r1 , r2 with
    | Bot , _ | _ , Bot -> true
    | Range(_,Some a) , Range(Some b,_) -> Z.lt a b
    | _ -> false
    
let add r1 r2 =
  match r1 , r2 with
      | Bot , _ | _ , Bot -> Bot
      | Range(a1,b1) , Range(a2,b2) ->
	  let a = lift_none Z.add a1 a2 in
	  let b = lift_none Z.add b1 b2 in
	  drange a b

let sub r1 r2 =
  match r1 , r2 with
      | Bot , _ | _ , Bot -> Bot
      | Range(a1,b1) , Range(a2,b2) ->
	  let a = lift_none Z.sub a1 b2 in
	  let b = lift_none Z.sub b1 a2 in
	  drange a b
	    
let opp = function Bot -> Bot | Range(a,b) -> Range(lift Z.opp b,lift Z.opp a)

let shift k = function
  | Bot -> Bot
  | Range(a,b) -> let f = lift (Z.add k) in Range(f a,f b)

exception Empty

let sup = function Range(_,u) -> u | Bot -> raise Empty
let inf = function Range(u,_) -> u | Bot -> raise Empty

let singleton = function 
  | Bot -> raise Empty
  | Range(Some x,Some y) when Z.equal x y -> Some x
  | _ -> None

let is_bot = function Bot -> true | _ -> false
let is_top = function Range(None,None) -> true | _ -> false
let mem x = function
  | Bot -> false
  | Range(None,None) -> true
  | Range(Some a,None) -> Z.leq a x
  | Range(None,Some b) -> Z.leq x b
  | Range(Some a,Some b) -> Z.leq a x && Z.leq x b


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
(* --- Variables Cleaning                                                 --- *)
(* -------------------------------------------------------------------------- *)

open Qed.Logic
open Lang
open Lang.F

(* -------------------------------------------------------------------------- *)
(* --- Latice                                                             --- *)
(* -------------------------------------------------------------------------- *)

type 'a occur = 
  | TOP
  | TRUE
  | FALSE
  | EQ of 'a

let cup eq a y = match a with
  | EQ x when eq x y -> a
  | _ -> TOP

let cup_true = function
  | TRUE -> TRUE
  | _ -> TOP

let cup_false = function
  | FALSE -> FALSE
  | _ -> TOP

let set_top m p = Vars.fold (fun x m -> Vmap.add x TOP m) (F.varsp p) m
let add eq x d m = Vmap.add x (try cup eq (Vmap.find x m) d with Not_found -> EQ d) m
let add_true m x = Vmap.add x (try cup_true (Vmap.find x m) with Not_found -> TRUE) m
let add_false m x = Vmap.add x (try cup_false (Vmap.find x m) with Not_found -> FALSE) m
let add_var = add Var.equal
let add_fun = add Fun.equal

(* -------------------------------------------------------------------------- *)
(* --- Collector                                                          --- *)
(* -------------------------------------------------------------------------- *)

let rec add_pred m p =
  match F.pred p with
    | And ps -> List.fold_left add_pred m ps
    | If(e,a,b) -> add_pred (add_pred (set_top m e) a) b
    | Eq(a,b) -> 
	begin
	  match F.pred a , F.pred b with
	    | Var x , Var y -> add_var x y (add_var y x m)
	    | _ -> set_top m p
	end
    | Var x -> add_true m x
    | Not p ->
	begin
	  match F.pred p with
	    | Var x -> add_false m x
	    | _ -> set_top m p
	end
    | _ -> set_top m p

let rec add_type m p =
  match F.pred p with
    | And ps -> List.fold_left add_type m ps
    | Fun(f,[e]) ->
	begin
	  match F.pred e with
	    | Var x -> add_fun x f m
	    | _ -> set_top m p
	end
    | _ -> set_top m p

(* -------------------------------------------------------------------------- *)
(* --- Usage                                                              --- *)
(* -------------------------------------------------------------------------- *)

type usage = {
  mutable eq_var : var occur Vmap.t ;
  mutable eq_fun : lfun occur Vmap.t ;
}

let create () = { eq_var = Vmap.empty ; eq_fun = Vmap.empty }
let as_atom m p = m.eq_var <- set_top m.eq_var p
let as_have m p = m.eq_var <- add_pred m.eq_var p
let as_type m p = m.eq_fun <- add_type m.eq_fun p

(* -------------------------------------------------------------------------- *)
(* --- Extraction                                                         --- *)
(* -------------------------------------------------------------------------- *)

let get x m = try Some (Vmap.find x m) with Not_found -> None

let is_true x m = 
  try match Vmap.find x m with TRUE -> true | _ -> false 
  with Not_found -> false

let is_false x m = 
  try match Vmap.find x m with FALSE -> true | _ -> false 
  with Not_found -> false

let is_var x m = 
  try match Vmap.find x m.eq_var with 
    | EQ y -> 
	begin
	  match get x m.eq_fun , get y m.eq_fun with
	    | None , _ -> true  (* we eliminate x, which has no guard... *)
	    | Some (EQ f) , Some (EQ g) -> Fun.equal f g
	    | _ -> false
	end
    | _ -> false
  with Not_found -> false

(* -------------------------------------------------------------------------- *)
(* --- Filtering                                                          --- *)
(* -------------------------------------------------------------------------- *)

let rec filter_pred m p =
  match F.pred p with
    | And ps -> F.p_all (filter_pred m) ps
    | If(e,a,b) -> p_if e (filter_pred m a) (filter_pred m b)
    | Eq(a,b) -> 
	begin
	  match F.pred a , F.pred b with
	    | Var x , Var y when is_var x m || is_var y m -> p_true
	    | _ -> p
	end
    | Var x when is_true x m.eq_var -> p_true
    | Not q ->
	begin
	  match F.pred q with
	    | Var x when is_false x m.eq_var -> p_true
	    | _ -> p
	end
    | _ -> p

let rec filter_type m p =
  match F.pred p with
    | And ps -> F.p_all (filter_type m) ps
    | Fun(_,[e]) ->
	begin
	  match F.pred e with
	    | Var x when is_var x m -> p_true
	    | _ -> p
	end
    | _ -> p


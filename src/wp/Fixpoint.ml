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
(* --- Generic Fixpoint Computation over a Domain                         --- *)
(* -------------------------------------------------------------------------- *)

module type Domain =
sig
  type t
  val bot : t
  val leq : t -> t -> bool
  val cap : t -> t -> t
  val cup : t -> t -> t
  val wide : t -> t -> t
  val pretty : Format.formatter -> t -> unit
end

module Make(D : Domain) =
struct

  type var = int
  type sem = 
    | F0 of D.t
    | F1 of int * (D.t -> D.t)
    | F2 of int * int * (D.t -> D.t -> D.t)
    | Fn of int list * (D.t list -> D.t)

  type system = rules Vector.t
  and rules = {
    mutable join : var list ;
    mutable fct : sem list ;
  }

  type f1 = D.t -> D.t
  type f2 = D.t -> D.t -> D.t
  type fn = D.t list -> D.t

  let rec add x = function [] -> [] | (y::ys) as w ->
    if x < y then x::w else if y < x then y :: add x ys else w

  let map f xs = 
    let rec walk ys f = function [] -> ys | x::xs -> walk (add (f x) ys) f xs
    in walk [] f xs

  let create () = Vector.create ()
  let var s = Vector.addi s { join=[] ; fct=[] }

  let add s x y =
    let r = Vector.get s x in
    r.join <- add y r.join

  let adds s x phi =
    let r = Vector.get s x in
    r.fct <- phi::r.fct

  let add0 s x d = adds s x (F0 d)
  let add1 s x f y = adds s x (F1(y,f))
  let add2 s x f y z = adds s x (F2(y,z,f))
  let addn s x f xs = adds s x (Fn(xs,f))

  type strategy = {
    root : int ;
    sys : system ;
    var : int array ;
    dom : D.t array ;
  }

  let rec visit sys var x = visit_r sys var x (Vector.get sys x)
  and visit_k sys var x r = ignore (visit_r sys var x r)
  and visit_r sys var x r =
    let y = var.(x) in
    if y != 0 then y else
      begin
	var.(x) <- x ;
	r.join <- map (visit sys var) r.join ;
	if r.fct = [] then
	  match r.join with [e] -> var.(x) <- e ; e | _ -> x
	else x
      end

  let rec id var x =
    let y = var.(x) in
    if x == y then x else
      let y = id var y in var.(x) <- y ; y

  let depend var deps x y = 
    let y = id var y in
    deps.(y) <- x :: deps.(y) ; y

  let fmap f = function
    | F0 _ as s -> s
    | F1(y,s) -> F1(f y,s)
    | F2(y,z,s) -> F2(f y,f z,s)
    | Fn(ys,s) -> Fn(List.map f ys,s)

  let sem d = function
    | F0 u -> u
    | F1(x,s) -> s d.(x)
    | F2(x,y,s) -> s d.(x) d.(y)
    | Fn(xs,s) -> s (List.map (Array.get d) xs)

  let update (sys,d,_) x = 
    let r = Vector.get sys x in
    let a = List.fold_left (fun w y -> D.cup w d.(y)) D.bot r.join in
    let b = List.fold_left (fun w s -> D.cup w (sem d s)) a r.fct in
    d.(x) <- b

  let widen ((_,d,tm) as job) ~level x =
    let a = d.(x) in
    update job x ;
    if level > tm then d.(x) <- D.wide a d.(x) ;
    D.leq d.(x) a

  type fixpoint = D.t array

  let get = Array.get
      
  let fixpoint ~system ~root ~timeout =
    let size = Vector.size system in
    let var = Array.create size 0 in
    Vector.iteri (visit_k system var) system ;
    let deps = Array.create size [] in
    for x = 0 to size-1 do
      if var.(x) == x then
	let r = Vector.get system x in
	let depx = depend var deps x in
	r.join <- List.map depx r.join ;
	r.fct <- List.map (fmap depx) r.fct ;
    done ;
    let succ f e = List.iter f deps.(e) in
    let root = id var root in
    let order = WTO.partition ~size ~succ ~root in
    let domain = Array.create size D.bot in
    let job = (system,domain,timeout) in
    WTO.fixpoint (widen job) (update job) order ;
    Array.iteri
      (fun x y -> if x!=y then domain.(x) <- domain.(y))
      var ; 
    domain
    
end

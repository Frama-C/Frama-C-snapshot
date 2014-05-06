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
(* --- Hierarchical Strongly Connected Components                         --- *)
(* -------------------------------------------------------------------------- *)

type partition =
  | Nil
  | Node of int * partition
  | Component of partition * partition

type succ = (int -> unit) -> int -> unit

type scc = {
  succ : (int -> unit) -> int -> unit ;
  stack : int Stack.t ;
  dfn : int array ;
  mutable num : int ;
}

type visit = {
  mutable loop : bool ;
  mutable head : int ;
}

let rec pretty fmt = function
  | Nil -> ()
  | Node(k,Nil) -> Format.fprintf fmt "%d" k
  | Node(k,e) -> Format.fprintf fmt "%d@ " k ; pretty fmt e
  | Component(a,Nil) ->
    Format.fprintf fmt "@[<hov 1>(%a)@]" pretty a
  | Component(a,b) ->
    Format.fprintf fmt "@[<hov 1>(%a)@]@ " pretty a ; 
    pretty fmt b

let rec visit scc vertex acc =
  begin
    Stack.push vertex scc.stack ;
    let n = succ scc.num in
    scc.num <- n ;
    scc.dfn.(vertex) <- n ;
    let w = { loop = false ; head = n } in
    scc.succ
      (fun succ ->
	let min = 
	  let d = scc.dfn.(succ) in
	  if d = 0 then visit scc succ acc else d
	in
	if min <= w.head then
	  ( w.head <- min ; w.loop <- true )
      ) vertex ;
    if w.head = scc.dfn.(vertex) then
      begin
	scc.dfn.(vertex) <- max_int ;
	let e = Stack.pop scc.stack in
	if w.loop then
	  begin
	    let rec unstack scc e vertex =
	      if e <> vertex then 
		( scc.dfn.(e) <- 0 ; 
		  let e = Stack.pop scc.stack in
		  unstack scc e vertex )
	    in unstack scc e vertex ; 
	    acc := Component(component scc vertex, !acc)
	  end
	else
	  acc := Node(vertex,!acc) ;
      end ;
    w.head
  end

and component scc vertex =
  begin
    let p = ref Nil in
    scc.succ
      (fun succ ->
	if scc.dfn.(succ) = 0 then
	  ignore (visit scc succ p)
      ) vertex ;
    Node(vertex,!p)
  end

let partition ~size ~succ ~root =
  let stack = Stack.create () in
  let dfn = Array.create size 0 in
  let scc = { succ ; stack ; dfn ; num = 0 } in
  let acc = ref Nil in
  ignore (visit scc root acc) ; 
  !acc

let rec fix widen level = function
  | Nil -> true
  | Node(e,_) -> widen ~level e
  | Component(a,_) -> fix widen level a
      
let rec fixpoint widen update = function
  | Nil -> ()
  | Node(e,w) -> update e ; fixpoint widen update w
  | Component(a,w) -> loop widen update a 0 ; fixpoint widen update w

and loop widen update wto n =
  fixpoint widen update wto ;
  if not (fix widen n wto) then 
    loop widen update wto (succ n)

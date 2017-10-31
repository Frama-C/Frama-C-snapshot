(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

open Lang
open Qed.Logic

type pattern =
  | INT of Integer.t
  | MUL_K of Integer.t * pattern
  | DIV_K of pattern * Integer.t
  | DIV of pattern * pattern
  | VAR of F.term

let rec pattern e =
  match F.repr e with
  | Kint n -> INT n
  | Times(k,e) -> MUL_K(k,pattern e)
  | Div(a,b) ->
      begin match F.repr b with
        | Kint k ->
            if Integer.(equal k zero) then raise Not_found ;
            DIV_K(pattern a,k)
        | _ ->
            DIV(pattern a,pattern b)
      end
  | _ ->
      if F.is_int e then VAR e else raise Not_found
          
let rec to_term = function
  | INT n -> F.e_zint n
  | MUL_K(k,a) -> F.e_times k (to_term a)
  | DIV_K(a,k) -> F.e_div (to_term a) (F.e_zint k)
  | DIV(a,b) -> F.e_div (to_term a) (to_term b)
  | VAR e -> e

let pdiv a b = INT(Integer.c_div a b)

let nzero x = F.p_neq F.e_zero x
let positive x = F.p_lt F.e_zero x
let negative x = F.p_lt x F.e_zero

type cmp = LEQ | LT

let icmp cmp a b = match cmp with
  | LEQ -> Integer.le a b
  | LT -> Integer.lt a b

let fcmp cmp a b = match cmp with
  | LEQ -> F.p_leq a b
  | LT -> F.p_lt a b

let ratio cmp a u b v =
  let x = F.e_mul a v in
  let y = F.e_mul b v in
  let pu = positive u in
  let nu = negative u in
  let pv = positive v in
  let nv = negative v in
  F.p_conj [ nzero u ; nzero v ;
             F.p_hyps [pu;pv] (fcmp cmp x y) ;
             F.p_hyps [nu;pv] (fcmp cmp y x) ;
             F.p_hyps [pu;nv] (fcmp cmp y x) ;
             F.p_hyps [nu;nv] (fcmp cmp x y) ]

let rec compare cmp a b =
  match a, b with
  | MUL_K( k,a ) , INT n ->
      if Integer.(lt zero k) then compare cmp a (pdiv n k) else
      if Integer.(lt k zero) then compare cmp (pdiv n k) a else
      if icmp cmp Integer.zero n then F.p_true else F.p_false
  | INT n , MUL_K( k,a ) ->
      if Integer.(lt zero k) then compare cmp (pdiv n k) a else
      if Integer.(lt k zero) then compare cmp a (pdiv n k) else
      if icmp cmp Integer.zero n then F.p_true else F.p_false
  | DIV_K( a,k ) , _ ->
      if Integer.(lt zero k) then
        let c = F.e_times k (F.e_add (to_term b) F.e_one) in
        compare cmp a (pattern c)
      else
      if Integer.(lt k zero) then
        let c = F.e_times k (F.e_sub (to_term b) F.e_one) in
        compare cmp (pattern c) a
      else
        raise Not_found
  | _ , DIV_K( b,k ) ->
      if Integer.(lt zero k) then
        let c = F.e_times k (F.e_sub (to_term a) F.e_one) in
        compare cmp (pattern c) b
      else
      if Integer.(lt k zero) then
        let c = F.e_times k (F.e_add (to_term a) F.e_one) in
        compare cmp b (pattern c)
      else
        raise Not_found
  | DIV(a,u) , DIV(b,v) ->
      ratio cmp (to_term a) (to_term u) (to_term b) (to_term v)
  | DIV(a,u) , b ->
      ratio cmp (to_term a) (to_term u) (to_term b) F.e_one
  | a , DIV(b,v) ->
      ratio cmp (to_term a) F.e_one (to_term b) (to_term v)
  | _ -> fcmp cmp (to_term a) (to_term b)

let rec equal a b =
  match a , b with
  | MUL_K( k,a ) , INT n
  | INT n , MUL_K( k,a ) ->
      let r = Integer.c_rem k n in
      if Integer.equal r Integer.zero then
        equal a (pdiv n k)
      else
        F.p_false
  | MUL_K( k,a ) , MUL_K( k',b ) ->
      let r = Integer.pgcd k k' in
      F.p_equal
        (F.e_times (Integer.c_div k r) (to_term a))
        (F.e_times (Integer.c_div k' r) (to_term b))
  | DIV_K(a,u) , DIV_K(b,v) ->
      let r = Integer.pgcd u v in
      F.p_equal
        (F.e_div (to_term a) (F.e_zint (Integer.c_div u r)))
        (F.e_div (to_term b) (F.e_zint (Integer.c_div v r)))
  | _ -> F.p_equal (to_term a) (to_term b)

let select goal =
  match F.repr (F.e_prop goal) with
  | Leq(a,b) -> compare LEQ (pattern a) (pattern b)
  | Lt(a,b) -> compare LT (pattern a) (pattern b)
  | Eq(a,b) -> equal (pattern a) (pattern b)
  | Neq(a,b) -> F.p_not (equal (pattern a) (pattern b))
  | _ -> raise Not_found

class ratio =
  object
    inherit Tactical.make
        ~id:"Wp.ratio"
        ~title:"Ratio"
        ~descr:"Compare Products and Divisions"
        ~params:[]
    
    method select _feedback = function
      | Tactical.Clause(Tactical.Goal p) ->
          let q = select p in
          if q != p
          then Tactical.Applicable(fun seq -> ["ratio" , (fst seq , q)])
          else Tactical.Not_applicable
      | _ -> Tactical.Not_applicable

  end

let tactical = Tactical.export (new ratio)

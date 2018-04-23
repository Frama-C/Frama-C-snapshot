(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

(* Only integer patterns *)
type pattern =
  | IMUL_K of Integer.t * F.term
  | IDIV_K of F.term * Integer.t
  | QDIV of F.term * F.term
  | Ival of F.term * Integer.t option
  | Rval of F.term

let pattern e =
  match F.repr e with
  | Kint n -> Ival(e,Some n)
  | Times(k,e) when F.is_int e -> IMUL_K(k,e)
  | Div(a,b) when not (F.is_int e) -> QDIV(a,b)
  | Div(a,b) when F.is_int e ->
      begin match F.repr b with
        | Kint k ->
            if Integer.(equal k zero) then raise Not_found ;
            IDIV_K(a,k)
        | _ -> Ival(e,None)
      end
  | _ ->
      if F.is_int e then Ival(e,None) else
      if F.is_real e then Rval e else
        raise Not_found
(*
let pp_pattern fmt = function
  | Ival(_,Some z) -> Format.fprintf fmt "(%s : constant)" (Integer.to_string z)
  | Ival(e,None) -> Format.fprintf fmt "@[<hov 2>(%a : int)@]" F.pp_term e
  | Rval e -> Format.fprintf fmt "@[<hov 2>(%a : real)@]" F.pp_term e
  | IMUL_K(k,e) -> Format.fprintf fmt "@[<hov 2>%s.(%a : int)@]" (Integer.to_string k) F.pp_term e
  | IDIV_K(e,k) -> Format.fprintf fmt "@[<hov 2>(%a : int)/%s@]" F.pp_term e (Integer.to_string k)
  | QDIV(a,b) -> Format.fprintf fmt "@[<hov 2>(%a : real)@,/(%a : real)@]" F.pp_term a F.pp_term b
*)
          
let to_term = function
  | IMUL_K(k,a) -> F.e_times k a
  | IDIV_K(a,k) -> F.e_div a (F.e_zint k)
  | QDIV(a,b) -> F.e_div a b
  | Ival(e,_) | Rval e -> e

let pdiv a b = let k = Integer.c_div a b in Ival(F.e_zint k,Some k)

let nzero x = F.p_neq F.e_zero x
let positive x = F.p_lt F.e_zero x
let negative x = F.p_lt x F.e_zero

type cmp = LEQ | LT | EQ

let icmp cmp a b = match cmp with
  | LEQ -> Integer.le a b
  | LT -> Integer.lt a b
  | EQ -> Integer.equal a b

let fcmp cmp a b = match cmp with
  | LEQ -> F.p_leq a b
  | LT -> F.p_lt a b
  | EQ -> F.p_equal a b

let compare_ratio cmp a u b v =
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

let compare_div cmp a b g =
  let ra = F.e_mod a g in
  let rb = F.e_mod b g in
  fcmp cmp (F.e_sub a ra) (F.e_sub b rb)

let rec compare cmp a b =
  match a, b with
  | IMUL_K( k,a ) , Ival(_,Some n) ->
      if Integer.(lt zero k) then compare cmp (pattern a) (pdiv n k) else
      if Integer.(lt k zero) then compare cmp (pdiv n k) (pattern a) else
      if icmp cmp Integer.zero n then F.p_true else F.p_false
  | Ival(_,Some n) , IMUL_K( k,a ) ->
      if Integer.(lt zero k) then compare cmp (pdiv n k) (pattern a) else
      if Integer.(lt k zero) then compare cmp (pattern a) (pdiv n k) else
      if icmp cmp Integer.zero n then F.p_true else F.p_false
  | IDIV_K( a,k ) , Ival(b,_) ->
      if Integer.(lt zero k) then
        let c = F.e_times k (F.e_add b F.e_one) in
        fcmp cmp a c
      else
      if Integer.(lt k zero) then
        let c = F.e_times k (F.e_sub b F.e_one) in
        fcmp cmp c a
      else
        raise Not_found
  | Ival(a,_) , IDIV_K( b,k ) ->
      if Integer.(lt zero k) then
        let c = F.e_times k (F.e_sub a F.e_one) in
        fcmp cmp c b
      else
      if Integer.(lt k zero) then
        let c = F.e_times k (F.e_add a F.e_one) in
        fcmp cmp b c
      else
        raise Not_found
  | IDIV_K( a,p ) , IDIV_K( b,q ) when
      not Integer.(equal p zero) &&
      not Integer.(equal q zero) ->
      let g = Integer.pgcd (Integer.abs p) (Integer.abs q) in
      let ka = Integer.div p g in
      let kb = Integer.div q g in
      compare_div cmp (F.e_times ka a) (F.e_times kb b) (F.e_zint g)
        
  | QDIV(a,u) , QDIV(b,v) -> compare_ratio cmp a u b v
  | QDIV(a,u) , (Ival(b,_) | Rval b) -> compare_ratio cmp a u b F.e_one
  | (Ival(a,_) | Rval a) , QDIV(b,v) -> compare_ratio cmp a F.e_one b v
  | _ ->
      raise Not_found

let eq_ratio eq a u b v =
  F.p_conj [ nzero u ; nzero v ; eq (F.e_mul a v) (F.e_mul b u) ]

let rec equal eq a b =
  match a , b with
  | IMUL_K( k,a ) , Ival(_,Some n)
  | Ival(_,Some n) , IMUL_K( k,a ) ->
      let r = Integer.c_rem k n in
      if Integer.equal r Integer.zero then
        equal eq (pattern a) (pdiv n k)
      else
        eq F.e_one F.e_zero
  | IMUL_K( k,a ) , IMUL_K( k',b ) ->
      let r = Integer.pgcd k k' in
      eq (F.e_times (Integer.c_div k r) a)
        (F.e_times (Integer.c_div k' r) b)

  | IDIV_K( a,p ) , IDIV_K( b,q ) when
      not Integer.(equal p zero) &&
      not Integer.(equal q zero) ->
      let g = Integer.pgcd (Integer.abs p) (Integer.abs q) in
      let ka = Integer.div p g in
      let kb = Integer.div q g in
      compare_div EQ (F.e_times ka a) (F.e_times kb b) (F.e_zint g)

  | QDIV(a,u) , QDIV(b,v) -> eq_ratio eq a u b v
  | QDIV(a,u) , (Ival(b,_) | Rval b) -> eq_ratio eq a u b F.e_one
  | (Ival(a,_) | Rval a) , QDIV(b,v) -> eq_ratio eq a F.e_one b v
  | _ -> eq (to_term a) (to_term b)

let select goal =
  match F.repr (F.e_prop goal) with
  | Leq(a,b) -> compare LEQ (pattern a) (pattern b)
  | Lt(a,b) -> compare LT (pattern a) (pattern b)
  | Eq(a,b) -> equal F.p_equal (pattern a) (pattern b)
  | Neq(a,b) -> equal F.p_neq (pattern a) (pattern b)
  | _ -> raise Not_found

class congruence =
  object
    inherit Tactical.make
        ~id:"Wp.congruence"
        ~title:"Congruence"
        ~descr:"Euclidian Comparisons"
        ~params:[]
    
    method select _feedback = function
      | Tactical.Clause(Tactical.Goal p) ->
          let q = select p in
          if q != p
          then Tactical.Applicable(fun seq -> ["congruence" , (fst seq , q)])
          else Tactical.Not_applicable
      | _ -> Tactical.Not_applicable

  end

let tactical = Tactical.export (new congruence)
let strategy = Strategy.make tactical ~arguments:[]

(* -------------------------------------------------------------------------- *)
(* --- Auto Congruence                                                    --- *)
(* -------------------------------------------------------------------------- *)

class autodiv =
  object

    method id = "wp:congruence"
    method title = "Auto Congruence"
    method descr = "Resolve Divisions and Multiplications"
    method search push (seq : Conditions.sequent) =
      try
        let p = snd seq in
        let q = select p in
        if q != p then push (strategy Tactical.(Clause (Goal p)))
      with Not_found -> ()

  end

let () = Strategy.register (new autodiv)

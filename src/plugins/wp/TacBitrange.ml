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

type log = Exact of int | Upper of int

(* Requires 2^i < n && 0 <= i < j *)
let rec log2m i j n =
  let b = Integer.two_power_of_int j in
  if Integer.lt b n then log2m j (2*j) n else
    (* 2^i < n <= 2^j *)
  if Integer.equal b n then Exact j else
    (* 2^i < n < 2^j *)
    log2d i j n

(* Requires 2^i < n < 2 ^j && 0 <= i < j *)
and log2d i j n =
  if succ i = j then Upper j else
    let k = (i+j)/2 in
    let a = Integer.two_power_of_int k in
    let c = Integer.compare a n in
    if c > 0 then log2d i k n else (* a=2^k > n *)
    if c < 0 then log2d k j n else (* a=2^k < n *)
      Exact k

(* Theorem:
   
   exists i, 0 <= x_i   
   forall i, x_i <= 2^p
   -----------------------------
   0 <= land(x_1,...,x_n) <= 2^p

*)

let is_positive e = F.p_leq F.e_zero e

let land_leq es n =
  if Integer.(lt zero n) then
    let p = match log2m 0 1 n with Exact p -> p | Upper p -> p-1 in
    (* Use theorem with 2^p <= n: land es <= 2^p <= n *)
    let a = F.e_zint (Integer.two_power_of_int p) in
    let positive = F.p_any is_positive es in
    F.p_conj (positive :: List.map (fun e -> F.p_leq e a) es)
  else if Integer.(equal zero n) then
    F.p_all (fun e -> F.p_equal e F.e_zero) es
  else raise Not_found

let leq_land n es =
  if Integer.(le n zero) then
    (* Use theorem with maximal p: n <= 0 <= land es *)
    F.p_any is_positive es
  else raise Not_found

(* Theorem:
   
   forall i, 0 <= x_i < 2^p
   -----------------------------
   0 <= lor(x_1,...,x_n) <= 2^p

*)

let lor_leq es n =
  if Integer.(le n zero) then
    let p = match log2m 0 1 n with Exact p -> p | Upper p -> p-1 in
    (* Use theorem with 2^p <= n: lor es <= 2^p <= n *)
    let a = F.e_zint (Integer.two_power_of_int p) in
    F.p_all (fun e -> F.p_and (is_positive e) (F.p_lt e a)) es
  else if Integer.(equal n zero) then
    F.p_any (fun e -> F.p_leq e F.e_zero) es
  else raise Not_found

let leq_lor n es =
  if Integer.(le n zero) then
    (* Use theorem with maximal p: n <= 0 <= lor es *)
    F.p_all is_positive es
  else raise Not_found

(* -------------------------------------------------------------------------- *)
(* --- Patterns                                                           --- *)
(* -------------------------------------------------------------------------- *)

type pattern =
  | LEQ of pattern * pattern
  | LT of pattern * pattern
  | INT
  | LAND
  | LOR

type sigma = {
  mutable bound : Integer.t ;
  mutable terms : F.term list ;
}

let rec pmatch s p e =
  let open Qed.Logic in
  match p , F.repr e with
  | LEQ(p,q) , Leq(a,b)
  | LT(p,q) , Lt(a,b)
    -> pmatch s p a ; pmatch s q b
  | INT , Kint n -> s.bound <- n
  | LAND , Fun(f,es) when f == Cint.f_land -> s.terms <- es
  | LOR , Fun(f,es) when f == Cint.f_lor -> s.terms <- es
  | _ -> raise Exit

let matches s p e = try pmatch s p e ; true with Exit -> false

let patterns : (pattern * (sigma -> F.pred)) list =
  [
    LEQ(INT,LAND) , (fun s -> leq_land s.bound s.terms) ;
    LT(INT,LAND) , (fun s -> leq_land (Integer.succ s.bound) s.terms) ;
    LEQ(LAND,INT) , (fun s -> land_leq s.terms s.bound) ;
    LT(LAND,INT) , (fun s -> land_leq s.terms (Integer.pred s.bound)) ;
    LEQ(INT,LOR) , (fun s -> leq_lor s.bound s.terms) ;
    LT(INT,LOR) , (fun s -> leq_lor (Integer.succ s.bound) s.terms) ;
    LEQ(LOR,INT) , (fun s -> lor_leq s.terms s.bound) ;
    LT(LOR,INT) , (fun s -> lor_leq s.terms (Integer.pred s.bound)) ;
  ]

let select_goal g =
  try
    let s = { bound = Integer.zero ; terms = [] } in
    let (_,f) = List.find (fun (p,_) -> matches s p g) patterns in
    Some (f s)
  with Not_found -> None

let rec split_goals others ranges = function
  | [] -> List.rev others , List.rev ranges
  | g::gs ->
      begin
        match select_goal g with
        | None -> split_goals (F.p_bool g::others) ranges gs
        | Some g' -> split_goals others (g'::ranges) gs
      end

let range_goal g' (hs,_) = ["bit-range" , (hs,g')]
let range_goals gs' (hs,_) = List.map (fun g' -> "bit-range" , (hs,g')) gs'
let other_goals ps (hs,_) = List.map (fun p -> "split" , (hs,p)) ps

open Tactical

class bitrange =
  object
    inherit Tactical.make
        ~id:"Wp.bitrange"
        ~title:"Bit Range"
        ~descr:"Bounds of Bitwise Operators"
        ~params:[]
    
    method select feedback = function
      | Clause(Goal p) ->
          begin
            let goals =
              let e = F.e_prop p in
              match F.repr e with
              | Qed.Logic.And es -> es
              | Qed.Logic.Leq _ | Qed.Logic.Lt _ -> [e]
              | _ -> raise Not_found
            in
            let others,ranges = split_goals [] [] goals in
            if ranges = [] then Tactical.Not_applicable else
              begin
                if others = [] then
                  feedback#set_title "Split & Bit Range(s)"
                else
                  feedback#set_title "Bit Range(s)" ;
                Tactical.Applicable
                  (fun seq -> other_goals others seq @
                              range_goals ranges seq)
              end
          end
      | Inside(Goal p,e) ->
          begin
            let g = F.e_prop p in
            match F.repr g with
            | Qed.Logic.And es when List.memq e es ->
                begin match select_goal g with
                  | Some g' -> Tactical.Applicable(range_goal g')
                  | None -> Tactical.Not_applicable
                end
            | _ -> Tactical.Not_applicable
          end
      | _ -> Tactical.Not_applicable
  end

let tactical = Tactical.export (new bitrange)

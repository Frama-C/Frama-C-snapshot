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

(* Helpers *)
let is_positive e = F.p_leq F.e_zero e (* 0 <= n *)
let is_negative e = F.p_lt e F.e_zero  (* n < 0 *)

(* Requires 2^i < n && 0 <= i < j *)
let rec log2m i j n =
  let b = Integer.two_power_of_int j in
  if Integer.lt b n then log2m j (2*j) n else
    (* 2^i < n <= 2^j *)
  if Integer.equal b n then j else
    (* 2^i < n < 2^j *)
    log2d i j n

(* Requires 2^i < n < 2 ^j && 0 <= i < j *)
and log2d i j n =
  if succ i = j then i else
    let k = (i+j)/2 in
    let a = Integer.two_power_of_int k in
    let c = Integer.compare a n in
    if c > 0 then log2d i k n else (* a=2^k > n *)
    if c < 0 then log2d k j n else (* a=2^k < n *)
      k

(* Theorem LAND-1: derived from Cbits.uint_land_range
   exists i, 0 <= e_i <= n
   -----------------------------
   0 <= land(e_1,...,e_n) <= n

   Theorem LAND-2: partially derived from Cbits.sint_land_inf
   forall i, -2^p <= e_i <= n < 0
   -------------------------------------------
   -2^p <= land(e_1,...,e_n) <= e_i <= n < 0
*)

let land_leq ~positive es n = (* land(e_1,...,e_n) <= n *)
  if Integer.(le zero n) then
    (* From theorem LAND-1 when 0<=n:
       (exist i, 0 <= e_i <= n) |- 0 <= land(e_1,...,e_n) <= n *)
    let a = F.e_zint n in
    let case1 = F.p_any (fun e -> F.p_and (is_positive e) (F.p_leq e a)) es in
    if positive then case1 else
      (* From theorem LAND-2: when 0 <= n
         (forall i, e_i < 0) && -1 <= 0 <= n |- land(e_1,...e_n) <= -1 <= 0 <= n *)
      let case2 = F.p_any is_negative es in
      F.p_or case1 case2
  else if positive then raise Not_found else
    (* From theorem LAND-2 when n<0:
       (forall i, e_i <= n < 0) |- land(e_1,...,e_n) <= n < 0*)
    let a = F.e_zint n in
    let case1 = F.p_any (fun e -> F.p_leq e a) es in
    if Integer.(lt n minus_one) then case1 else
      (* From theorem LAND-2: when -1 == n
         (forall i, e_i < 0) && -1 <= 0 <= n |- land(e_1,...e_n) <= -1 <= 0 <= n *)
      let case2 = F.p_any is_negative es in
      F.p_or case1 case2

let leq_land ~positive n es = (* n <= land(e_1,...,e_n) *)
  if Integer.(le n zero) then
    (* From theorem LAND-1 when n<=0:
       (exist i, n <= 0 <= e_i) |- n <= 0 <= land(e_1,...,e_n) *)
    F.p_any is_positive es
  else
  if positive then raise Not_found else
    let p = log2m 0 1 (Integer.neg n) in
    (* Have n <= -2^p < 0
       From theorem LAND-2: when n <= -2^p < 0
       (forall i, n <= -2^p <= e_i < 0) |- n <= land(e_1,...e_n) < 0 *)
    let a = F.e_zint Integer.(neg (two_power_of_int p)) in
    F.p_all (fun e -> F.p_and (is_negative e) (F.p_lt a e)) es

(* Theorem LOR-1: partially derived from Cbits.uint_lor_inf
   forall i, 0 <= e_i <= 2^p-1
   -----------------------------
   forall i, 0 <= e_i <= lor(e_1,...,e_n) <= 2^p-1

   Theorem LOR-2: derived from Cbits.sint_lor_range
   exist i, e_i <= n < 0
   -----------------------------
   n <= lor(e_1,...,e_n) < 0
*)

let lor_leq ~positive es n = (* lor(e_1,...,e_n) <= n *)
  if Integer.(le zero n) then
    let p = log2m 0 1 (Integer.succ n) in
    (* Have 0 <= 2^p <= n+1, hence 0 <= 2^p-1 <= n.
       From theorem LOR-1 when 0 <= 2^p-1 <= n
       (forall i, 0<= e_i <= 2^p-1 <=n) ==> 0<=lor(e_1,...,e_n) <= 2^p-1 <=n *)
    let a = F.e_zint (Integer.two_power_of_int p) in
    let case1 = F.p_all (fun e -> F.p_and (is_positive e) (F.p_lt e a)) es in
    if positive then case1 else
      (* From theorem LOR-2 when 0<=n:
         (exist i, e_i < 0 <= n) |- lor(e_1,...,e_n) < 0 <= n*)
      let case2 = F.p_any is_negative es in
      F.p_or case1 case2
  else
    raise Not_found

let leq_lor ~positive n es = (* n <= lor(e_1,...,e_n) *)
  if Integer.(le zero n) then
    (* From theorem LOR-1 when 0<=n:
       (forall i, 0 <= n <= e_i) |- 0 <= n <= lor(e_1,...,e_n) *)
    let a = F.e_zint n in
    F.p_all (fun e -> F.p_leq a e) es
  else
  if positive then raise Not_found
  else
    (* From theorem LOR-1 when n<0:
       (forall i, n < 0 <= e_i) |- n < 0 <= lor(e_1,...,e_n) *)
    let case1 = F.p_all is_positive es in
    (* From theorem LOR-2 when n<0:
         (exist i, n <= e_i < 0) |- n <= lor(e_1,...,e_n) < 0 *)
    let a = F.e_zint n in
    let case2 = F.p_any (fun e -> F.p_and (F.p_leq a e) (is_negative e)) es in
    F.p_or case1 case2

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
  plor : bool ;
  pland : bool ;
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
    LEQ(INT,LAND) , (fun s -> leq_land ~positive:s.pland s.bound s.terms) ;
    LT(INT,LAND) , (fun s -> leq_land ~positive:s.pland (Integer.succ s.bound) s.terms) ;
    LEQ(LAND,INT) , (fun s -> land_leq ~positive:s.pland s.terms s.bound) ;
    LT(LAND,INT) , (fun s -> land_leq ~positive:s.pland s.terms (Integer.pred s.bound)) ;
    LEQ(INT,LOR) , (fun s -> leq_lor ~positive:s.plor s.bound s.terms) ;
    LT(INT,LOR) , (fun s -> leq_lor ~positive:s.plor (Integer.succ s.bound) s.terms) ;
    LEQ(LOR,INT) , (fun s -> lor_leq ~positive:s.plor s.terms s.bound) ;
    LT(LOR,INT) , (fun s -> lor_leq ~positive:s.plor s.terms (Integer.pred s.bound)) ;
  ]

let select_goal ~pland ~plor g =
  try
    let s = { pland ; plor ; bound = Integer.zero ; terms = [] } in
    let (_,f) = List.find (fun (p,_) -> matches s p g) patterns in
    Some (f s)
  with Not_found -> None

let rec split_goals ~pland ~plor others ranges = function
  | [] -> List.rev others , List.rev ranges
  | g::gs ->
      begin
        match select_goal ~pland ~plor g with
        | None -> split_goals ~pland ~plor (F.p_bool g::others) ranges gs
        | Some g' -> split_goals ~pland ~plor others (g'::ranges) gs
      end

let range_goal g' (hs,_) = ["bit-range" , (hs,g')]
let range_goals gs' (hs,_) = List.map (fun g' -> "bit-range" , (hs,g')) gs'
let other_goals ps (hs,_) = List.map (fun p -> "split" , (hs,p)) ps

open Tactical

let positive_land =
  Tactical.checkbox
    ~id:"positive-land"
    ~title:"Force positive logical-and"
    ~descr:"Requires to obtain a result from (at least one) positive operands"
    ~default:true ()

let positive_lor =
  Tactical.checkbox
    ~id:"positive-lor"
    ~title:"Force negative logical-or"
    ~descr:"Restrict to obtain a positive result from (all) positive operands"
    ~default:true ()

class bitrange =
  object(self)
    inherit Tactical.make
        ~id:"Wp.bitrange"
        ~title:"Bit Range"
        ~descr:"Bounds of Bitwise Operators"
        ~params:[snd positive_land;snd positive_lor]

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
            let pland = self#get_field (fst positive_land) in
            let plor = self#get_field (fst positive_lor) in
            let others,ranges = split_goals ~pland ~plor [] [] goals in
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
                begin
                  let pland = self#get_field (fst positive_land) in
                  let plor = self#get_field (fst positive_lor) in
                  match select_goal ~pland ~plor g with
                  | Some g' -> Tactical.Applicable(range_goal g')
                  | None -> Tactical.Not_applicable
                end
            | _ -> Tactical.Not_applicable
          end
      | _ -> Tactical.Not_applicable
  end

let tactical = Tactical.export (new bitrange)
let strategy = Strategy.make tactical ~arguments:[]

(* -------------------------------------------------------------------------- *)
(* --- Auto Bitrange                                                      --- *)
(* -------------------------------------------------------------------------- *)

let is_bitwised e =
  let open Qed.Logic in
  match F.repr e with
  | Fun(f,_) -> List.memq f Cint.f_bitwised
  | _ -> false

class autobitrange =
  object

    method id = "wp:bitrange"
    method title = "Auto Bit-Range"
    method descr = "Apply Bit-Range on comparison with bitwised operations"

    method search push (seq : Conditions.sequent) =
      let goal = snd seq in
      let open Qed.Logic in
      match F.e_expr goal with
      | Lt(x,y) | Leq(x,y) when is_bitwised x || is_bitwised y ->
          push (strategy Tactical.(Clause (Goal goal)))
      | _ -> ()
  end

let () = Strategy.register (new autobitrange)

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

let range a n =
  let vmax = Integer.two_power_of_int n in
  F.p_and (F.p_leq F.e_zero a) (F.p_lt a (F.e_zint vmax))

(* starts from 0 *)
let bit_test x k = Cint.l_and x (F.e_int (1 lsl k))

(* from n downto 0 *)
let rec bitwise_eqs a b n =
  if n >= 0 then
    F.e_eq (bit_test a n) (bit_test b n) ::
    bitwise_eqs a b (n-1)
  else []

(* bitwise eq on n bits *)
let bitwise_eq a b n = F.e_and (bitwise_eqs a b (n-1))

let rewrite descr u v = Tactical.rewrite [ descr , F.p_true , u , v ]

let vrange,prange = Tactical.spinner ~id:"Wp.bitwised.range"
    ~vmin:0 ~vmax:64 ~default:32
    ~title:"Bits" ~descr:"Number of bits for bitwise equality" ()

class bitcase =
  object(self)
    inherit Tactical.make
        ~id:"Wp.bitwised"
        ~title:"Bitwise Eq."
        ~descr:"Decompose Bitwise Equality"
        ~params:[prange]

    method select feedback selection =
      let e = Tactical.selected selection in
      let open Qed.Logic in
      match F.repr e with
      | Eq(a,b) when F.is_int a && F.is_int b ->
          let n = self#get_field vrange in
          feedback#set_title "Bitwise Eq. (%d bits)" n ;
          (*   range:(0 <= a < 2^n && 0 <= b < 2^n)
               && bitwise:(forall k; 0 <= k <= n ==> (bit(a,k) <==> bit(b,k)))
               |- a <= b *)
          let inrange = F.p_and (range a n) (range b n) in
          let bitwise = bitwise_eq a b n in
          Tactical.Applicable
            (fun seq ->
               ("range" , (fst seq , inrange)) ::
               rewrite "bitwise" e bitwise seq)
      | _ -> Tactical.Not_applicable

  end

let tactical = Tactical.export (new bitcase)
let strategy ?(priority=1.0) selection ~nbits =
  Strategy.{
    priority ; tactical ; selection ;
    arguments = [ arg vrange nbits ] ;
  }

(* -------------------------------------------------------------------------- *)
(* --- Auto Bitwise                                                       --- *)
(* -------------------------------------------------------------------------- *)

let is_bitwised e =
  let open Qed.Logic in
  match F.repr e with
  | Fun(f,_) -> List.memq f Cint.f_bitwised
  | _ -> false

let rec lookup push clause ~nbits ~priority p =
  let open Qed.Logic in
  match F.repr p with
  | And ps | Or ps ->
      List.iter (lookup push clause ~priority ~nbits) ps
  | Imply(hs,p) ->
      List.iter (lookup push clause ~priority ~nbits) (p::hs)
  | Eq(x,y) when F.is_int x && F.is_int y ->
      let bx = is_bitwised x in
      let by = is_bitwised y in
      if bx || by then
        let priority = if bx && by then priority else priority *. 0.8 in
        push (strategy ~priority ~nbits Tactical.(Inside(clause,p)))
  | _ -> ()

class autobitwise =
  object(self)

    method private nbits = Ctypes.i_bits (Ctypes.c_ptr ())

    method id = "wp:bitwised"
    method title =
      Printf.sprintf "Auto Bitwise Eq. (%d)" self#nbits

    method descr =
      Printf.sprintf "Apply Bitwise Equality on wordsize bits (%d)" self#nbits

    method search push (seq : Conditions.sequent) =
      let goal = snd seq in
      let nbits = self#nbits in
      lookup push (Tactical.Goal goal) ~nbits ~priority:1.0 (F.e_prop goal) ;
      Conditions.iter
        (fun step ->
           let p = Conditions.head step |> F.e_prop in
           lookup push (Tactical.Step step) ~nbits ~priority:0.5 p
        ) (fst seq)

  end

let () = Strategy.register (new autobitwise)

(* -------------------------------------------------------------------------- *)

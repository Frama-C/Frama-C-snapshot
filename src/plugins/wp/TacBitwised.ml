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

let range a n = F.p_and (F.p_leq F.e_zero a) (F.p_lt a (F.e_int (1 lsl n)))

let bit_test x k = Cint.l_and x (F.e_int (1 lsl k))

let rec bitwise_eqs a b n =
  if n >= 0 then
    F.e_eq (bit_test a n) (bit_test b n) ::
    bitwise_eqs a b (n-1)
  else []

let bitwise_eq a b n = F.e_and (bitwise_eqs a b n)
let rewrite descr u v = Tactical.rewrite [ descr , F.p_true , u , v ]

let vrange,prange = Tactical.spinner ~id:"Wp.bitwised.range"
    ~vmin:0 ~vmax:64 ~default:8
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
          let inrange = F.p_and (range a n) (range b n) in
          let bitwise = bitwise_eq a b n in
          Tactical.Applicable
            (fun seq ->
               ("range" , (fst seq , inrange)) ::
               rewrite "bitwise" e bitwise seq)
      | _ -> Tactical.Not_applicable

  end

let tactical = Tactical.export (new bitcase)

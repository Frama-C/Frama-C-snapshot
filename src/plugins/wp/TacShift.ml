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

let select_op f =
  if f == Cint.f_lsl then F.e_mul else
  if f == Cint.f_lsr then F.e_div else
    raise Not_found

let select_int n =
  match F.repr n with
  | Qed.Logic.Kint n ->
      (try Integer.to_int n with Integer.Too_big -> raise Not_found)
  | _ -> raise Not_found

let rewrite descr u v = Tactical.rewrite [ descr , F.p_true , u , v ]

class shift =
  object
    inherit Tactical.make
        ~id:"Wp.shift"
        ~title:"Logical Shift"
        ~descr:"Transform Shifting into Arithmetics"
        ~params:[]
    
    method select feedback selection =
      let e = Tactical.selected selection in
      let open Qed.Logic in
      match F.repr e with
      | Fun( f , [a;n] ) ->
          begin
            let op = select_op f in
            let n = select_int n in
            if n > 64 then feedback#set_error "Too large shift (64 max.)" ;
            if n < 0 then feedback#set_error "Negative shift (0 min.)" ;
            let b = op a (F.e_int (1 lsl n)) in
            Tactical.Applicable
              (fun seq ->
                 ("positive" , (fst seq , F.p_leq F.e_zero a)) ::
                 rewrite "shift" e b seq)
          end
      | _ -> Tactical.Not_applicable

  end

let tactical = Tactical.export (new shift)

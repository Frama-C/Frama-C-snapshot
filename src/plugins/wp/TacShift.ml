(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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
  let rewrite descr u v = Tactical.rewrite [ descr , F.p_true , u , v ] in
  let rewrite_lsl e a n =
    (* from selection e='a<<n', rewrites the sequent 'Hs |- G' into:
       - Hs[e := a*2^n] |- G[e := a*2^n)] *)
    let b = F.e_mul a (F.e_int (1 lsl n)) in
    rewrite "shift" e b
  in
  let rewrite_lsr e a n =
    (* from selection e='a>>n', rewrites the sequent 'Hs |- G' into:
       - Hs |- 0<=a
       - Hs[e := a*2^n] |- G[e := a*2^n] *)
    let b = F.e_div a (F.e_int (1 lsl n)) in
    (fun seq -> ("positive" , (fst seq , F.p_leq F.e_zero a)) ::
                rewrite "shift" e b seq)
  in
  if f == Cint.f_lsl then rewrite_lsl else
  if f == Cint.f_lsr then rewrite_lsr else
    raise Not_found

let select_int n =
  match F.repr n with
  | Qed.Logic.Kint n ->
      (try Integer.to_int n with Z.Overflow -> raise Not_found)
  | _ -> raise Not_found

class shift =
  object
    inherit Tactical.make
        ~id:"Wp.shift"
        ~title:"Logical Shift"
        ~descr:"Transform Shifts into Div/Mult"
        ~params:[]

    method select feedback selection =
      let e = Tactical.selected selection in
      let open Qed.Logic in
      match F.repr e with
      | Fun( f , [a;n] ) ->
          begin
            let rewrite_shift = select_op f in
            let n = select_int n in
            if n > 64 then feedback#set_error "Too large shift (64 max.)" ;
            if n < 0 then feedback#set_error "Negative shift (0 min.)" ;
            Tactical.Applicable (rewrite_shift e a n)
          end
      | _ -> Tactical.Not_applicable

  end

let tactical = Tactical.export (new shift)
let strategy = Strategy.make tactical ~arguments:[]

(* -------------------------------------------------------------------------- *)
(* --- Auto Shift                                                         --- *)
(* -------------------------------------------------------------------------- *)

let is_shift e =
  try
    let open Qed.Logic in
    match F.repr e with
    | Fun( f , [_;n] ) ->
        let _ignore = select_op f in
        let _ = select_int n in
        true
    | _ -> false
  with Not_found -> false

let rec scan m f e =
  if not (F.Tset.mem e !m) then
    begin
      m := F.Tset.add e !m ;
      if is_shift e then f e else
      if F.lc_closed e then
        F.lc_iter (scan m f) e
    end

class autoshift =
  object

    method id = "wp:bitshift"
    method title = "Auto Bit-Shift"
    method descr = "Apply Bit-Shift in Goal"

    method search push (seq : Conditions.sequent) =
      let goal = snd seq in
      let apply e =
        let selection = Tactical.(Inside(Goal goal,e)) in
        push (strategy ~priority:0.5 selection)
      in
      scan (ref F.Tset.empty) apply (F.e_prop goal)

  end

let () = Strategy.register (new autoshift)

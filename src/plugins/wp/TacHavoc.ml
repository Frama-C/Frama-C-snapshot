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
open Tactical
open Conditions

module L = Qed.Logic

(* -------------------------------------------------------------------------- *)
(* --- Cut Tactical                                                       --- *)
(* -------------------------------------------------------------------------- *)

let field,parameter =
  Tactical.composer
    ~id:"address"
    ~title:"Address"
    ~descr:"Access Outside the Assigned Range"
    ()

let has_type t e =
  try F.Tau.equal t (F.typeof e)
  with Not_found -> false

class havoc =
  object(self)
    inherit Tactical.make ~id:"Wp.havoc"
        ~title:"Havoc"
        ~descr:"Go Through Assigns"
        ~params:[parameter]

    method select feedback sel =
      match sel with
      | Clause(Step s) ->
          begin
            match s.condition with
            | Have p | When p ->
                begin match F.e_expr p with
                  | L.Fun( f , [m;m';a;n] ) when f == MemTyped.p_havoc ->
                      (try
                         let tp = F.typeof a in
                         feedback#update_field ~filter:(has_type tp) field ;
                         let sel = self#get_field field in
                         if not (Tactical.is_empty sel) then
                           let ptr = Tactical.selected sel in
                           if has_type tp ptr then
                             let separated =
                               F.p_call MemTyped.p_separated
                                 [ ptr ; F.e_int 1 ; a ; n ] in
                             let equal =
                               F.p_equal (F.e_get m ptr) (F.e_get m' ptr) in
                             let process = Tactical.insert ~at:s.id
                                 [ "Havoc",F.p_imply separated equal ] in
                             Applicable process
                           else
                             ( feedback#set_error "Not a pointer type" ;
                               Not_configured )
                         else Not_configured
                       with Not_found -> Not_applicable)
                  | _ -> Not_applicable
                end
            | _ -> Not_applicable
          end
      | _ -> Not_applicable
  end

let separated ?at property =
  match F.e_expr property with
  | L.Fun( f , [p;n;q;m] ) when f == MemTyped.p_separated ->
      let base_p = MemTyped.a_base p in
      let ofs_p = MemTyped.a_offset p in
      let base_q = MemTyped.a_base q in
      let ofs_q = MemTyped.a_offset q in
      let eq_base = F.p_equal base_p base_q in
      let on_left = F.p_leq (F.e_add ofs_p n) ofs_q in
      let on_right = F.p_leq (F.e_add ofs_q m) ofs_p in
      let overlap = F.p_not (F.p_and on_left on_right) in
      let pattern = F.e_prop property in
      let cases =
        [ "WrongBase" , F.p_neq base_p base_q , pattern , F.e_true ;
          "OnLeft" , F.p_and eq_base on_left , pattern , F.e_true ;
          "OnRight" , F.p_and eq_base on_right , pattern , F.e_true ;
          "OverLap" , F.p_and eq_base overlap , pattern , F.e_false ]
      in
      Applicable (Tactical.rewrite ?at cases)
  | _ -> Not_applicable

class separated =
  object
    inherit Tactical.make ~id:"Wp.separated"
        ~title:"Separated"
        ~descr:"Expand Separation Cases"
        ~params:[]

    method select _feedback sel =
      match sel with
      | Clause (Goal p) -> separated p
      | Clause (Step s) -> separated ~at:s.id (Conditions.head s)
      | Inside (_,p) when F.is_prop p ->
          separated ?at:(Tactical.at sel) (F.p_bool p)
      | _ -> Not_applicable
  end

module Havoc =
struct
  let field = field
  let tactical = Tactical.export (new havoc)
  let strategy ?(priority=1.0) ~havoc ~addr =
    Strategy.{
      priority ;
      tactical ;
      selection = havoc ;
      arguments = [ arg field addr ] ;
    }
end

module Separated =
struct
  let tactical = Tactical.export (new separated)
  let strategy = Strategy.make tactical
end

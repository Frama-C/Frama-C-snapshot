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
open Tactical
open Conditions

module L = Qed.Logic

(* -------------------------------------------------------------------------- *)
(* --- Havoc                                                              --- *)
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

let match_havoc =
  let havoc m1 = function
    | L.Fun( f , [m_undef;m0;a;n] ) when f == MemTyped.f_havoc -> m1,(m_undef,m0,a,n)
    | _ -> raise Not_found
  in function
    | L.Eq (m,m') -> (try havoc m' (F.repr m) with | Not_found -> havoc m (F.repr m'))
    | _ -> raise Not_found

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
                let m1,(m_undef,m0,a,n) = match_havoc (F.e_expr p) in
                let tp = F.typeof a in
                feedback#update_field ~filter:(has_type tp) field ;
                let sel = self#get_field field in
                if not (Tactical.is_empty sel) then
                  let ptr = Tactical.selected sel in
                  if has_type tp ptr then
                    let separated =
                      F.p_call MemTyped.p_separated
                        [ ptr ; F.e_int 1 ; a ; n ] in
                    let equal_unassigned =
                      F.p_equal (F.e_get m1 ptr) (F.e_get m0 ptr) in
                    let equal_assigned =
                      F.p_equal (F.e_get m1 ptr) (F.e_get m_undef ptr) in
                    let process = Tactical.insert ~at:s.id
                        [ "Havoc",F.p_if separated equal_unassigned equal_assigned  ] in
                    Applicable process
                  else
                    ( feedback#set_error "Not a pointer type" ;
                      Not_configured )
                else Not_configured
            | _ -> Not_applicable
          end
      | _ -> Not_applicable
  end

(* -------------------------------------------------------------------------- *)
(* --- Separated                                                          --- *)
(* -------------------------------------------------------------------------- *)

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

(* -------------------------------------------------------------------------- *)
(* --- Included, Validity, Invalidity                                     --- *)
(* -------------------------------------------------------------------------- *)

let invalid m p n =
  let base = MemTyped.a_base p in
  let offset = MemTyped.a_offset p in
  let malloc = F.e_get m base in
  "Invalid",
  F.p_imply
    (F.p_lt F.e_zero n)
    (F.p_or
       (F.p_leq malloc offset)
       (F.p_leq (F.e_add offset n) F.e_zero))

let valid_rd m p n =
  let base = MemTyped.a_base p in
  let offset = MemTyped.a_offset p in
  let malloc = F.e_get m base in
  "Valid (Read)",
  F.p_imply
    (F.p_lt F.e_zero n)
    (F.p_and
       (F.p_leq F.e_zero offset)
       (F.p_leq (F.e_add offset n) malloc))

let valid_rw m p n =
  let base = MemTyped.a_base p in
  let offset = MemTyped.a_offset p in
  let malloc = F.e_get m base in
  "Valid (Read & Write)",
  F.p_imply
    (F.p_lt F.e_zero n)
    (F.p_conj [
        F.p_lt F.e_zero base ;
        F.p_leq F.e_zero offset ;
        F.p_leq (F.e_add offset n) malloc ;
      ])

let included p a q b =
  let p_base = MemTyped.a_base p in
  let q_base = MemTyped.a_base q in
  let p_offset = MemTyped.a_offset p in
  let q_offset = MemTyped.a_offset q in
  "Included",
  F.p_imply
    (F.p_lt F.e_zero a)
    (F.p_imply
       (F.p_leq F.e_zero b)
       (F.p_conj [
           F.p_equal p_base q_base ;
           F.p_leq q_offset p_offset ;
           F.p_leq (F.e_add p_offset a) (F.e_add q_offset b) ;
         ]))

let lookup f = function
  | [p;a;q;b] when f == MemTyped.p_included -> included p a q b
  | [m;p;n] when f == MemTyped.p_invalid -> invalid m p n
  | [m;p;n] when f == MemTyped.p_valid_rd -> valid_rd m p n
  | [m;p;n] when f == MemTyped.p_valid_rw -> valid_rw m p n
  | _ -> raise Not_found

let unfold ?at e f es =
  let descr,q = lookup f es in
  Applicable (Tactical.rewrite ?at [descr,F.p_true,e,F.e_prop q])

class validity =
  object

    inherit Tactical.make ~id:"Wp.valid"
        ~title:"Validity Range"
        ~descr:"Unfold validity and range definitions"
        ~params:[]

    method select _feedback (s : Tactical.selection) =
      let at = Tactical.at s in
      let e = Tactical.selected s in
      match F.repr e with
      | Qed.Logic.Fun(f,es) -> unfold ?at e f es
      | _ -> Not_applicable

  end


(* -------------------------------------------------------------------------- *)
(* --- Exported API                                                       --- *)
(* -------------------------------------------------------------------------- *)

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
  let strategy = Strategy.make tactical ~arguments:[]
end

module Validity =
struct
  let tactical = Tactical.export (new validity)
  let strategy = Strategy.make tactical ~arguments:[]
end

(* -------------------------------------------------------------------------- *)

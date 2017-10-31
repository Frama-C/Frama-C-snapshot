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

(* -------------------------------------------------------------------------- *)
(* --- Unfold Definition Tactical                                         --- *)
(* -------------------------------------------------------------------------- *)

open Definitions

let definition f es =
  let d = find_symbol f in
  match d.d_definition with
  | Function(_,_,u) ->
      Subst.(e_apply (sigma d.d_params es) u)
  | Predicate(_,p) ->
      F.e_prop (Subst.(p_apply (sigma d.d_params es) p))
  | _ ->
      raise Not_found

let range f es =
  let a,b = Ctypes.bounds (Cint.is_cint f) in
  let range e = F.p_and
      (F.p_leq (F.e_zint a) e)
      (F.p_leq e (F.e_zint b)) in
  F.e_prop (F.p_all range es)

let rec applicable ?at e f es = function
  | phi::others ->
      begin
        try
          let v = phi f es in
          let d = Pretty_utils.sfprintf "Unfold '%a'" Lang.Fun.pretty f in
          Applicable (Tactical.rewrite ?at [d,F.p_true,e,v])
        with Not_found | Invalid_argument _ ->
          applicable ?at e f es others
      end
  | [] ->
      Not_applicable

class unfold =
  object
    inherit Tactical.make ~id:"Wp.unfold"
        ~title:"Definition"
        ~descr:"Unfold predicate and logic function definition"
        ~params:[]

    method select _feedback (s : Tactical.selection) =
      let at = Tactical.at s in
      let e = Tactical.selected s in
      match F.repr e with
      | Qed.Logic.Fun(f,es) ->
          applicable ?at e f es [ definition ; range ]
      | _ -> Not_applicable

  end

let tactical = Tactical.export (new unfold)
let strategy = Strategy.make tactical

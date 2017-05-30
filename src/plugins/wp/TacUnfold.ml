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

let definition ?at e f es =
  let d = find_symbol f in
  match d.d_definition with
  | Function(_,_,u) ->
      let a = Pretty_utils.sfprintf "Unfold '%a'" Lang.Fun.pretty f in
      let v = Subst.(e_apply (sigma d.d_params es) u) in
      Tactical.rewrite ?at [a,F.p_true,e,v]
  | Predicate(_,p) ->
      let a = Pretty_utils.sfprintf "Unfold '%a'" Lang.Fun.pretty f in
      let v = Subst.(p_apply (sigma d.d_params es) p) in
      Tactical.rewrite ?at [a,F.p_true,e,F.e_prop v]
  | _ ->
      raise Not_found

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
          begin
            try Applicable (definition ?at e f es)
            with Not_found | Invalid_argument _ -> Not_applicable
          end
      | _ -> Not_applicable

  end

let tactical = Tactical.export (new unfold)
let strategy = Strategy.make tactical

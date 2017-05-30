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
(* --- Array Tactical                                                     --- *)
(* -------------------------------------------------------------------------- *)

(* Detects a[i->e][j] pattern *)
let access_update_pattern e =
  let open Qed.Logic in
  match F.repr e with
  | Aget(u,j) ->
      begin match F.repr u with
        | Aset(a,i,e) -> Some(a,i,e,j)
        | _ -> None
      end
  | _ -> None

class array =
  object
    inherit Tactical.make ~id:"Wp.array"
        ~title:"Array"
        ~descr:"Decompose array access-update patterns"
        ~params:[]

    method select feedback (s : Tactical.selection) =
      let e = Tactical.selected s in
      match access_update_pattern e with
      | None -> Not_applicable
      | Some(a,i,v,j) ->
          ignore feedback ;
          let at = Tactical.at s in
          let cases = [
            "Same Indices" , F.p_equal i j , e , v ;
            "Diff Indices" , F.p_neq i j , e , F.e_get a j ;
          ] in
          Applicable (Tactical.rewrite ?at cases)

  end

let tactical = Tactical.export (new array)
let strategy = Strategy.make tactical

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

(* -------------------------------------------------------------------------- *)
(* --- Passive Forms                                                      --- *)
(* -------------------------------------------------------------------------- *)

open Lang
open Lang.F

type binding =
  | Bind of var * var (* fresh , bound *)
  | Join of var * var (* left, right *)
type t = binding list

let empty = []
let is_empty n = n = []
let union = List.append

let bind ~fresh ~bound bs = Bind(fresh,bound) :: bs
let join x y bs =
  if Var.equal x y then bs else Join(x,y) :: bs

let eq x y = F.p_equal (e_var x) (e_var y)

let rec collect phi hs = function
  | [] -> hs
  | Bind(x,y)::bs -> collect phi (if phi y then eq x y :: hs else hs) bs
  | Join(x,y)::bs -> collect phi (if phi x || phi y then eq x y :: hs else hs) bs

let apply bindings p =
  let xs = varsp p in
  let hs = collect (fun x -> Vars.mem x xs) [] bindings in
  p_conj (p::hs)

let conditions bindings phi = collect phi [] bindings

let iter = List.iter

let pretty fmt =
  List.iter
    begin function
      | Bind(x,y) ->
          Format.fprintf fmt "@ @[%a:=%a@]"
            F.pp_var x F.pp_var y
      | Join(x,y) ->
          Format.fprintf fmt "@ @[%a==%a@]"
            F.pp_var x F.pp_var y
    end

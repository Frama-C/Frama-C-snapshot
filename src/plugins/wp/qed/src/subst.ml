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

(* -------------------------------------------------------------------------- *)
(* --- Generalized Substitution by Congruence Closure                     --- *)
(* -------------------------------------------------------------------------- *)

module Make(T : Logic.Term) =
struct

  type term = T.term
  module Tmap = T.Tmap

  type sigma = {
    mutable ker : term Tmap.t ;
    mutable img : term Tmap.t ;
  }

  let create () = {
    ker = Tmap.empty ;
    img = Tmap.empty ;
  }

  let copy s = { ker = s.ker ; img = s.img }

  (* --- Union Find --- *)

  let rec find s a =
    try Tmap.find a s.img
    with Not_found ->
      let b = T.lc_map (find s) a in
      s.img <- Tmap.add a b s.img ;
      let c = find s b in
      s.img <- Tmap.add a c s.img ; c

  (* --- Incremental Rewrite with Queue of delayed equalities --- *)

  let rewrite q s a b =
    begin
      let ker = Tmap.add a b Tmap.empty in
      let sigma0 = { ker ; img = ker } in
      s.ker <-
        Tmap.filter
          (fun u v ->
             let u' = find sigma0 u in
             let v' = find sigma0 v in
             if T.equal u u' && T.equal v v' then true
             else ( q := (u',v') :: !q ; false ))
          s.ker ;
      s.ker <- Tmap.add a b s.ker ;
      s.img <- s.ker ;
    end

  (* --- Fixpoint & Sub-Term Based Orientation --- *)

  let rec fixpoint q s a b =
    let a = find s a in
    let b = find s b in
    let cmp = T.compare a b in
    if cmp <> 0 then
      begin
        if T.is_subterm b a then rewrite q s a b else
        if T.is_subterm a b then rewrite q s b a else
        if cmp < 0 then rewrite q s b a else
        if cmp > 0 then rewrite q s a b
      end ;
    flush q s

  and flush q s =
    match !q with
    | [] -> ()
    | (a,b)::w -> q := w ; fixpoint q s a b

  let merge s a b = fixpoint (ref []) s a b

end

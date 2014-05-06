(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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
(* ---     First Order Logic                                              --- *)
(* -------------------------------------------------------------------------- *)

open Hcons
open Logic

module type Type =
sig
  type t
  val dummy : t
  val equal : t -> t -> bool
end

module Make(T : Type) =
struct

  type var = {
    vid : int ;
    vbase : string ;
    vrank : int ;
    vtau : T.t ;
  }

  let hash_var x = Hcons.hash_pair x.vrank (Hashtbl.hash x.vbase)

  let pretty fmt x = Format.fprintf fmt "%s_%d" x.vbase x.vrank

  (* HASHCONSING *)

  module W = Weak.Make
      (struct
        type t = var
        let hash = hash_var
        let equal x y = 
          x.vbase = y.vbase && x.vrank = y.vrank && T.equal x.vtau y.vtau
      end)

  let kid = ref 0
  let hmap = W.create 32993 (* 3-th Leyland Prime number *)

  let insert base rank tau =
    let x0 = {
      vid = 0 ;
      vbase = base ;
      vrank = rank ;
      vtau = tau ;
    } in
    try W.find hmap x0
    with Not_found ->
        let k = let i = !kid in (assert (i <> -1) ; incr kid ; i) in
        let x = { x0 with vid = k } in W.add hmap x ; x

  let dummy = insert "" 0 T.dummy

  let hash x = x.vid
  let equal = (==)
  let compare x y = 
    let cmp = String.compare x.vbase y.vbase in
    if cmp <> 0 then cmp else 
      let cmp = Pervasives.compare x.vrank y.vrank in
      if cmp <> 0 then cmp else
        Pervasives.compare x.vid y.vid

  (* POOL *)

  type pool = (string,int ref) Hashtbl.t
  let create ?copy () = match copy with
    | None -> Hashtbl.create 131
    | Some pool -> Hashtbl.copy pool

  let counter pool base = 
    try Hashtbl.find pool base
    with Not_found ->
        let c = ref 0 in Hashtbl.add pool base c ; c

  let add pool x =
    let c = counter pool x.vbase in
    if !c <= x.vrank then c := succ x.vrank

  let next pool base =
    let c = counter pool base in
    let k = !c in incr c ; k

  let fresh pool base tau =
    let rank = next pool base in
    insert base rank tau

  let alpha pool x = fresh pool x.vbase x.vtau

end

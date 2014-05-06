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
(* --- Relations                                                          --- *)
(* -------------------------------------------------------------------------- *)

module type S =
sig
  type t
  type elt
  val empty : t
  val add : elt -> elt -> t -> t
  val mem : elt -> elt -> t -> bool
  val union : t -> t -> t
  val inter : t -> t -> t
  val filter : (elt -> elt -> bool) -> t -> t
  val iter : (elt -> elt -> unit) -> t -> unit
  val map : (elt -> elt) -> t -> t
end

module type Elt =
sig
  type t
  val compare : t -> t -> int
  val pretty : Format.formatter -> t -> unit
end

module type Pair =
sig
  type elt
  type t = elt * elt
  val pair : elt -> elt -> t
  val compare : t -> t -> int
  val pretty : Format.formatter -> t -> unit
end

module PAIR(E : Elt) : Pair with type elt = E.t =
struct
  type elt = E.t
  type t = E.t * E.t (* x << y *)
  let pair x y = (x,y)
  let compare (a,b) (c,d) =
    let cmp = E.compare a c in
    if cmp <> 0 then cmp else E.compare b d
  let pretty fmt (x,y) = Format.fprintf fmt "(%a,%a)" E.pretty x E.pretty y
end  

module PSYM(E : Elt) : Pair with type elt = E.t =
struct
  include PAIR(E)
  let pair x y = if E.compare x y > 0 then (y,x) else (x,y)
end

module REL(P : Pair) : S with type elt = P.elt =
struct
  module S = Set.Make(P)
  type elt = P.elt
  type t = S.t
  let empty = S.empty
  let union = S.union
  let inter = S.inter
  let mem x y s = S.mem (P.pair x y) s
  let add x y s = S.add (P.pair x y) s
  let lift f (x,y) = f x y
  let filter f = S.filter (lift f)
  let iter f = S.iter (lift f)
  let map f s = S.fold (fun (x,y) s -> add (f x) (f y) s) s empty
end

module Rel(E:Elt) = REL(PAIR(E))
module Sym(E:Elt) = REL(PSYM(E))

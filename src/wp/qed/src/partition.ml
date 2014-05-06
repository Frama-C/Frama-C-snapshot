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
(* --- Partition based on union-find                                      --- *)
(* -------------------------------------------------------------------------- *)

module type Explain =
sig
  type t
  val bot : t 
  val cup : t -> t -> t
end

module Unit =
struct
  type t = unit
  let bot = ()
  let cup () () = ()
end

module type S =
sig
  type t
  type elt
  type explain
  val empty : t
  val join : ?explain:explain -> elt -> elt -> t -> t
  val class_of : t -> elt -> elt
  val is_equal : t -> elt -> elt -> bool
  val members : t -> elt -> elt list 
  val repr : t -> elt -> elt * explain
  val equal : t -> elt -> elt -> explain option 
  val explain : t -> elt -> elt -> explain
  val iter : (elt -> elt list -> unit) -> t -> unit
  val map : (elt -> elt) -> t -> t
end

module MakeExplain(A : Map.OrderedType)(E : Explain) =
struct

  module M = Map.Make(A)
  type elt = A.t
  type explain = E.t

  type t = {
    mutable color : (A.t * E.t) M.t ;
    members : A.t list M.t ; (* sorted *)
  }

  let empty = { color = M.empty ; members = M.empty }

  let rec union ca cb = 
    match ca , cb with
    | [] , r | r , [] -> r
    | a :: ra , b :: rb ->
        let cmp = A.compare a b in
        if cmp < 0 then a :: union ra cb else
        if cmp > 0 then b :: union ca rb else
          a :: union ra rb

  let rec lookup p a =
    let ((a0,e0) as w0) = M.find a p.color in
    try 
      let (a1,e1) = lookup p a0 in
      let w = (a1,E.cup e0 e1) in
      p.color <- M.add a w p.color ; w
    with Not_found -> w0

  let repr p a =
    try lookup p a
    with Not_found -> a,E.bot

  let class_of p a =
    try fst (lookup p a)
    with Not_found -> a

  let equal t a b =
    let (a,u) = repr t a in
    let (b,v) = repr t b in
    if A.compare a b = 0 then Some (E.cup u v) else None

  let explain t a b =
    let (a,u) = repr t a in
    let (b,v) = repr t b in
    if A.compare a b = 0 then E.cup u v else E.bot

  let is_equal t a b = A.compare (class_of t a) (class_of t b) = 0
  let k_members p e = try M.find e p.members with Not_found -> [e]
  let members p e = k_members p (class_of p e)

  let join ?(explain=E.bot) a b p =
    let a = class_of p a in
    let b = class_of p b in
    let cmp = A.compare a b in
    if cmp = 0 then p else
      let c = union (k_members p a) (k_members p b) in
      if cmp < 0 then {
        color = M.add b (a,explain) p.color ; 
        members = M.add a c (M.remove b p.members) ;
      } else {
        color = M.add a (b,explain) p.color ;
        members = M.add b c (M.remove a p.members) ;
      }

  let iter f p = M.iter f p.members

  let map f p = 
    M.fold
      (fun a (b,explain) p -> join ~explain (f a) (f b) p) 
      p.color empty

end

module Make(A : Map.OrderedType) = MakeExplain(A)(Unit)

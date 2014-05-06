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

open Logic

module type S =
sig
  type t
  type term
  type explain
  exception Contradiction of explain
  val empty : t
  val assume : ?explain:explain -> term -> t -> t 
  val rewrite : ?explain:explain -> term -> term -> t -> t
  val reduce : t -> term -> term * explain
  val is_true : t -> term -> explain option
  val is_false : t -> term -> explain option
  val iter : (term -> term -> explain -> unit) -> t -> unit
end

module type Explain =
sig
  type t
  val bot : t 
  val cup : t -> t -> t
end

module Make(T : Logic.Term)(E : Explain) = 
struct

  type term = T.term
  type explain = E.t
  exception Contradiction of explain

  open T

  type t = {
    domain : Vars.t ;
    def : (term * explain) Tmap.t ; (* core facts *)
    mutable mem : (term * explain) Tmap.t ; (* memoization *)
  }

  let empty = {
    domain = Vars.empty ;
    def = Tmap.empty ;
    mem = Tmap.empty ;
  }

  let iter f s = Tmap.iter (fun a (b,e) -> f a b e) s.def

  let rec apply s xs a =
    let ys = T.vars a in
    if Vars.intersect xs ys 
    then (* Filter out bound variables *) apply_sub s xs a
    else
    if not (Vars.intersect s.domain ys)
    then (* Filter out-of-scope *) a , E.bot
    else
      (* Memoization *)
      try Tmap.find a s.mem
      with Not_found ->
          let w = apply_sub s xs a in
          s.mem <- Tmap.add a w s.mem ; w

  and apply_sub s xs a =
    let w = ref E.bot in
    let b = T.f_map 
        (fun xs a -> let (a,e) = apply s xs a in w := E.cup !w e ; a) xs a in
    b , !w

  let reduce s a = apply s Vars.empty a

  let is_true s a =
    let r,e = reduce s a in if r == e_true then Some e else None

  let is_false s a =
    let r,e = reduce s a in if r == e_false then Some e else None

  let add_def e a b s =
    try
      let b0,e0 = Tmap.find a s.mem in
      match T.are_equal b b0 with
      | No -> raise (Contradiction(E.cup e0 e))
      | Yes -> s (* nothing to do *)
      | Maybe -> raise Not_found
    with Not_found ->
        let def = Tmap.add a (b,e) s.def in
        { domain = Vars.union (T.vars a) s.domain ; mem = def ; def }

  let add_lit e a s = add_def e a e_true (add_def e (e_not a) e_false s)

  let rec add_pred e p s = match T.repr p with
    | True -> s
    | False -> raise (Contradiction e)
    | And ps -> add_all e ps s
    | Fun _ | Not _ | Neq _ -> add_lit e p s
    | Lt(x,y) -> add_lit e p (add_lit e (e_leq x y) (add_lit e (e_neq x y) s))
    | Leq(x,y) ->
        begin
          match is_true s (e_leq y x) with
          | Some e0 -> add_pred (E.cup e e0) (e_eq x y) s
          | None -> add_lit e p s
        end
    | Eq(x,y) ->
        begin
          match T.is_closed x , T.is_closed y with
          | true , false -> add_def e y x s
          | false , true -> add_def e x y s
          | _ -> add_lit e p s
        end
    | _ -> s

  and add_all e ps s = match ps with 
    | [] -> s | p::ps -> add_all e ps (add_pred e p s)

  let assume ?(explain=E.bot) p s = add_pred explain p s
  let rewrite ?(explain=E.bot) a b s = add_def explain a b s

end

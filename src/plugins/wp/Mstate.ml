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

open Lang.F
open Memory

type (_,_) eq = Equal : 'a -> ('a,'a) eq | NotEqual : ('a,'b) eq

module Ident :
sig
  type 'a t
  val create : 'a -> 'a t
  val get : 'a t -> 'a
  val eq : 'a t -> 'b t -> ('a,'b) eq
end =
struct
  let k = ref 0
  type 'a t = int * 'a
  let get = snd
  let create s = incr k ; (!k,s)
  let eq (k,s) (k',_) = if k = k' then Obj.magic (Equal s) else NotEqual
end

(* -------------------------------------------------------------------------- *)
(* --- L-Val Utility                                                      --- *)
(* -------------------------------------------------------------------------- *)

let index (host,ofs) k = host , ofs @ [Mindex k]
let field (host,ofs) f = host , ofs @ [Mfield f]

let host_eq a b = match a,b with
  | Mvar x , Mvar y -> Cil_datatype.Varinfo.equal x y
  | Mmem a , Mmem b -> a == b
  | _ -> false

let ofs_eq a b = match a,b with
  | Mindex i , Mindex j -> i = j
  | Mfield f , Mfield g -> Cil_datatype.Fieldinfo.equal f g
  | _ -> false

let rec offset_eq p q = match p,q with
  | [],[] -> true
  | a :: p , b :: q -> ofs_eq a b && offset_eq p q
  | _ -> false

let equal a b = a == b || (host_eq (fst a) (fst b) && offset_eq (snd a) (snd b))

(* -------------------------------------------------------------------------- *)
(* --- Memory State Pretty Printing Information                           --- *)
(* -------------------------------------------------------------------------- *)

type 'a operations = {
  apply : (term -> term) -> 'a -> 'a ;
  lookup : 'a -> term -> mval ;
  updates : 'a sequence -> Vars.t -> update Bag.t ;
  iter : (mval -> term -> unit) -> 'a -> unit ;
}

type 'a model = MODEL : 'a operations Ident.t * ('b -> 'a) -> 'b model

let create (type s) (module M : Memory.Model with type Sigma.t = s) =
  let op = {
    apply = M.apply ;
    lookup = M.lookup ;
    updates = M.updates ;
    iter = M.iter ;
  } in
  MODEL( Ident.create op , M.state )

type state = STATE : 'a operations Ident.t * 'a -> state

let state model sigma = match model with MODEL(op,state) -> STATE(op,state sigma)

let iter f = function STATE(m,s) -> (Ident.get m).iter f s
let apply f = function STATE(m,s) -> STATE(m,(Ident.get m).apply f s)
let lookup s e = match s with STATE(m,s) -> (Ident.get m).lookup s e
let updates seq vars =
  match seq.pre , seq.post with STATE(p,u) , STATE(q,v) ->
  match Ident.eq p q with
  | Equal s -> s.updates { pre = u ; post = v } vars
  | NotEqual -> Bag.empty (* assert false *)

(* -------------------------------------------------------------------------- *)

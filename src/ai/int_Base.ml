(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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

open Abstract_interp

type i = Top | Value of Integer.t

let equal i1 i2 = match i1, i2 with
  | Top, Top -> true
  | Value i1, Value i2 -> Integer.equal i1 i2
  | Top, Value _ | Value _, Top -> false

let compare i1 i2 = match i1, i2 with
  | Top, Top -> 0
  | Value i1, Value i2 -> Integer.compare i1 i2
  | Top, Value _ -> -1
  | Value _, Top -> 1

let hash = function
  | Top -> 37
  | Value i -> Integer.hash i

let pretty fmt = function
  | Top -> Format.fprintf fmt "Top"
  | Value i -> Format.fprintf fmt "<%a>" Int.pretty i

include Datatype.Make
(struct
  type t = i (*= Top | Value of Integer.t *)
  let name = "Int_Base.t"
  let structural_descr =
    Structural_descr.t_sum [| [| Datatype.Big_int.packed_descr |] |]
  let reprs = Top :: List.map (fun v -> Value v) Datatype.Big_int.reprs
  let equal = equal
  let compare = compare
  let hash = hash
  let rehash = Datatype.identity
  let copy = Extlib.id
  let internal_pretty_code = Datatype.undefined
  let pretty = pretty
  let varname = Datatype.undefined
  let mem_project = Datatype.never_any_project
 end)

let minus_one = Value Int.minus_one
let one = Value Int.one
let zero = Value Int.zero
let is_zero x = equal x zero
let top = Top
let is_top v = (v = Top)
let neg x =
  match x with
    | Value v -> Value (Int.neg v)
    | Top -> x
let inject i = Value i

exception Error_Top

let project = function
  | Top -> raise Error_Top
  | Value i -> i

let cardinal_zero_or_one = function
  | Top -> false
  | Value _ -> true

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

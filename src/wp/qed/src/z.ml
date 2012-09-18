(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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

open Big_int

type t = big_int

let zero = zero_big_int
let one = unit_big_int
let minus_one = minus_big_int one
let int = big_int_of_int

let add = add_big_int
let sub = sub_big_int
let mul = mult_big_int
let opp = minus_big_int
let div a b = 
  let sb = sign_big_int b in
  if sb = 0 then failwith "Division by zero" ;
  let sa = sign_big_int a in
  if sa = 0 then zero else
    let a = abs_big_int a in
    let b = abs_big_int b in
    let q = div_big_int a b in
    if sa * sb > 0 then q else minus_big_int q

let remainder a b =
  let sb = sign_big_int b in
  if sb = 0 then failwith "Division by zero" ;
  let sa = sign_big_int a in
  if sa = 0 then zero else
    let a = abs_big_int a in
    let b = abs_big_int b in
    let r = mod_big_int a b in
    if sa > 0 then r else minus_big_int r

let euclidian a b =
  let sb = sign_big_int b in
  if sb = 0 then failwith "Division by zero" ;
  let sa = sign_big_int a in
  if sa = 0 then zero , zero else
    let a = abs_big_int a in
    let b = abs_big_int b in
    let q,r = quomod_big_int a b in
    ( (if sa * sb > 0 then q else minus_big_int q) ,
      (if sa > 0 then r else minus_big_int r) )

let to_string = string_of_big_int
let of_string = big_int_of_string

let pretty fmt x = Format.pp_print_string fmt (string_of_big_int x)

let to_int x = try Some(int_of_big_int x) with _ -> None
let to_big_int x = x
let of_big_int x = x

let hash x = Hashtbl.hash (to_string x)
let compare = compare_big_int
let equal = eq_big_int
let not_equal x y = compare x y <> 0
let leq = le_big_int
let lt = lt_big_int
let max x y = if leq x y then y else x
let min x y = if leq x y then x else y
let positive x = sign_big_int x >= 0
let negative x = sign_big_int x <= 0
let null x = sign_big_int x = 0
let lt_zero x = sign_big_int x < 0
let gt_zero x = sign_big_int x > 0

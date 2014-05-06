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
(* --- Hash Consing Utilities                                             --- *)
(* -------------------------------------------------------------------------- *)

let primes = [| 2 ; 3 ; 5 ; 7 ; 11 ; 13 ; 17 ; 19 ; 23 ; 29 ; 
                31 ; 37 ; 41 ; 43 ; 47 ; 53 ; 59 ; 61 ; 67 ; 71 ; 
                73 ; 79 ; 83 ; 89 ; 97 ; 101 ; 103 ; 107 ; 109 ; 113 ; 
                127 ; 131 ; 137 ; 139 ; 149 ; 151 ; 157 ; 163 ; 167 ; 173 ; 
                179 ; 181 ; 191 ; 193 ; 197 ; 199 ; 211 ; 223 ; 227 ; 229 ; 
                233 ; 239 ; 241 ; 251 ; 257 ; 263 ; 269 ; 271 ; 277 ; 281 |]
let n_primes = Array.length primes

let hash_int t = if t < n_primes then primes.(t) else 1
let hash_tag x = hash_int (Obj.tag (Obj.repr x))
let hash_pair x y = x * 599 + y * 799
let hash_triple x y z = x * 281 + y * 599 + z * 799
let rec hash_list f h = function
  | [] -> h
  | x::xs -> hash_list f (h * 599 + f x) xs

let hash_opt f h = function
  | None -> h
  | Some x -> h * 281 + f x

let hash_array f h xs =
  let rec collect h xs i =
    if i < Array.length xs then
      collect (h * 599 + f xs.(i)) xs (succ i)
    else h
  in collect h xs 0

let rec compare_list cmp xs ys =
  match xs , ys with
  | [] , [] -> 0
  | [] , _ :: _ -> -1
  | _ :: _ , [] -> 1
  | x::xs , y::ys ->
      let c = cmp x y in
      if c = 0 then compare_list cmp xs ys else c

let rec equal_list eq xs ys =
  match xs , ys with
  | [] , [] -> true
  | [] , _ :: _ | _ :: _ , [] -> false
  | x::xs , y::ys -> eq x y && equal_list eq xs ys

let equal_array eq xs ys =
  let n = Array.length xs in
  let m = Array.length ys in
  n = m && 
  begin
    try
      for i=0 to n-1 do
        if not (eq xs.(i) ys.(i)) then raise Exit
      done ; true
    with Exit -> false
  end

let exists_array f xs =
  try
    for i=0 to Array.length xs - 1 do
      if f xs.(i) then raise Exit
    done ; false
  with Exit -> true

let forall_array f xs =
  try
    for i=0 to Array.length xs - 1 do
      if not (f xs.(i)) then raise Exit
    done ; true
  with Exit -> false

let rec eq_list xs ys =
  match xs, ys with
  | [] , [] -> true
  | [] , _::_ | _::_ , [] -> false
  | x::xs , y::ys -> x==y && eq_list xs ys

let eq_array xs ys =
  let n = Array.length xs in
  let m = Array.length ys in
  n = m && 
  begin
    try
      for i=0 to n-1 do
        if not (xs.(i) == ys.(i)) then raise Exit
      done ; true
    with Exit -> false
  end

let rec fold_list op f a = function
  | [] -> a
  | x::xs -> fold_list op f (op a (f x)) xs

let fold_array op f a xs =
  let rec collect op f a xs i =
    if i < Array.length xs then
      collect op f (op a (f xs.(i))) xs (succ i)
    else a
  in collect op f a xs 0

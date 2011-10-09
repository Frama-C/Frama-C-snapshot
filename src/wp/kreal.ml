(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

(* -------------------------------------------------------------------------- *)
(* --- Coq Real Constants                                                 --- *)
(* -------------------------------------------------------------------------- *)

let error () = raise (Invalid_argument "invalid real constant")

type sign = Positive | Negative
type state = Integral | Fraction | Exponent

type env = {
  mantiss  : Buffer.t ;
  exponent : Buffer.t ;
  mutable sign  : sign ;
  mutable coma  : int ;  (* number of digits afer '.' *)
  mutable state : state ;
}

type token =
  | Digit
  | Plus
  | Minus
  | Exp
  | Dot

let token = function
  | ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') -> Digit
  | '.' -> Dot
  | '-' -> Minus
  | '+' -> Plus
  | 'e' | 'E' -> Exp
  | _ -> error ()

let trail m =
  let n = String.length m in
  let rec first k = if k < n && m.[k] = '0' then first (succ k) else k in
  let rec last k = if k >= 0 && m.[k] = '0' then last (pred k) else k in
  let a = first 0 in
  let b = last (n-1) in
  if a <= b then Some(a , n-b-1) else None

let convert r =
  let e = {
    mantiss  = Buffer.create 64 ; sign = Positive ;
    exponent = Buffer.create 64 ;
    coma = 0 ; state = Integral ;
  } in
  String.iter
    (fun c ->
       let tk = token c in
       match e.state , tk with
         | _ , Dot -> e.state <- Fraction ;
         | _ , Exp -> e.state <- Exponent ;
         | (Integral|Fraction) , Plus -> e.sign <- Positive
         | (Integral|Fraction) , Minus -> e.sign <- Negative
         | Integral , Digit -> Buffer.add_char e.mantiss c
         | Fraction , Digit -> Buffer.add_char e.mantiss c ; e.coma <- succ e.coma
         | Exponent , (Plus|Minus|Digit) -> Buffer.add_char e.exponent c
    ) r ;
  let m = Buffer.contents e.mantiss in
  begin
    match trail m with
      | None -> "0"
      | Some(a,b) ->
          let digits = String.sub m a (String.length m - a - b) in
          let exp =
            let ex = Buffer.contents e.exponent in
            if ex = "" then b - e.coma else int_of_string ex + b - e.coma
          in
          let size = 4 + String.length m + abs exp in
          let buffer = Buffer.create size in
          let parent = e.sign = Negative || exp <> 0 in
          if parent then Buffer.add_char buffer '(' ;
          if e.sign = Negative then Buffer.add_char buffer '-' ;
          Buffer.add_string buffer digits ;
          if exp > 0 then Buffer.add_string buffer (String.make exp '0') ;
          if exp < 0 then
            (Buffer.add_string buffer "/1" ;
             Buffer.add_string buffer (String.make (-exp) '0')) ;
          if parent then Buffer.add_char buffer ')' ;
          Buffer.contents buffer
  end

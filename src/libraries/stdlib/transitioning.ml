(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

[@@@ warning "-3"]

module String = struct
  let uppercase_ascii = String.uppercase
  let capitalize_ascii = String.capitalize
  let uncapitalize_ascii = String.uncapitalize
  let lowercase_ascii = String.lowercase
end

module Char = struct
  let uppercase_ascii = Char.uppercase
  let lowercase_ascii = Char.lowercase
end

module Q = struct

  let round_to_float x exact =
    let m = Z.to_int64 x in
    (* Unless the fractional part is exactly 0, round m to an odd integer *)
    let m = if exact then m else Int64.logor m 1L in
    (* Then convert m to float, with the current rounding mode. *)
    Int64.to_float m


  let to_float x =
    match Q.classify x with
    | Q.ZERO -> 0.0
    | Q.INF  -> infinity
    | Q.MINF -> neg_infinity
    | Q.UNDEF -> nan
    | Q.NZERO ->
      let p = x.Q.num and q = x.Q.den in
      let np = Z.numbits p and nq = Z.numbits q in
      if np <= 53 && nq <= 53 then
        (* p and q convert to floats exactly; use FP division to get the
           correctly-rounded result. *)
        Int64.to_float (Z.to_int64 p) /. Int64.to_float (Z.to_int64 q)
      else begin
        (* |p| is in [2^(np-1), 2^np)
           q is in [2^(nq-1), 2^nq)
           hence |p/q| is in (2^(np-nq-1), 2^(np-nq+1)).
           We define n such that |p/q*2^n| is in [2^54, 2^56).
           >= 2^54 so that the round to odd technique applies.
           < 2^56 so that the integral part is representable as an int64. *)
        let n = 55 - (np - nq) in
        (* Scaling p/q by 2^n *)
        let (p', q') =
          if n >= 0
          then (Z.shift_left p n, q)
          else (p, Z.shift_left q (-n)) in
        (* Euclidean division of p' by q' *)
        let (quo, rem) = Z.ediv_rem p' q' in
        (* quo is the integral part of p/q*2^n
           rem/q' is the fractional part. *)
        (* Round quo to float *)
        let f = round_to_float quo (Z.sign rem = 0) in
        (* Apply exponent *)
        ldexp f (-n)
      end
end

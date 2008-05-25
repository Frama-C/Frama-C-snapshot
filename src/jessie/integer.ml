(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(**************************************************************************)

(* $Id: integer.ml,v 1.7 2008/04/10 15:48:06 uid562 Exp $ *)

let zero = Int64.zero
let one = Int64.one
let minus_one = Int64.minus_one

let (+) = Int64.add
let (-) = Int64.sub
let ( * ) = Int64.mul
let (/) = Int64.div
let (mod) = Int64.rem
let (~-) = Int64.neg

let max_int = Int64.max_int
let min_int = Int64.min_int

let (land) = Int64.logand
let (lor) = Int64.logor
let (lxor) = Int64.logxor
let (lsl) = Int64.shift_left
let (asr) = Int64.shift_right
let (lsr) = Int64.shift_right_logical

let negative c = c < 0
let nonpositive c = c <= 0
let (<=) i1 i2 = nonpositive (Int64.compare i1 i2)
let (<) i1 i2 = negative (Int64.compare i1 i2)
let (>=) i1 i2 = i2 <= i1
let (>) i1 i2 = i2 < i1

let power_of_two i = 
  assert (i >= 0L && i < 63L);
  1L lsl (Int64.to_int i)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)

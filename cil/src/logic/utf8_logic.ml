(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
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
(*                                                                        *)
(**************************************************************************)

let from_unichar n =
  let rec log64 n =
    if n = 0 then 0 else
      1 + log64 (n lsr 5)
  in
  let utf8_storage_len n =
    if n < 0x80 then 1 else
      log64 (n lsr 1)
  in
  (* this function is not exported, so it's OK to do a few 'unsafe' things *)
  let write_unichar s ~pos c =
    let len = utf8_storage_len c in
    if len = 1 then
      String.unsafe_set s pos (Char.unsafe_chr c)
    else begin
      String.unsafe_set s pos (Char.unsafe_chr (((1 lsl len - 1) lsl (8-len)) lor (c lsr ((len-1)*6))));
      for i = 1 to len-1 do
	String.unsafe_set s (pos+i)
	  (Char.unsafe_chr (((c lsr ((len-1-i)*6)) land 0x3f) lor 0x80))
      done ;
    end ;
    len
  in
  let s = String.create 6 in
  let len = write_unichar s ~pos:0 n in
  String.sub s 0 len


let forall =  from_unichar 0x2200
let exists =  from_unichar 0x2203
let eq =  from_unichar (*0x2263*) (*0x2250*) 0x2261
let neq =  from_unichar 0x2262
let le =  from_unichar 0x2264
let ge =  from_unichar 0x2265
let minus = from_unichar 0x2212

let implies = from_unichar 0x21D2
let iff = from_unichar 0x21D4
let conj = from_unichar 0x2227
let disj = from_unichar 0x2228
let neg = from_unichar 0x00AC
let x_or =  from_unichar 0x22BB
let inset = from_unichar 0x2208

let boolean = from_unichar 0x1D539
let integer = from_unichar 0x2124
let real = from_unichar 0x211D
(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

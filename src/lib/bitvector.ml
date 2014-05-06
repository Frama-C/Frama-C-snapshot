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

(* ------------------------------------------------------------------------ *)
(* ---  Bit Vector Library                                              --- *)
(* ------------------------------------------------------------------------ *)

type t = string

let max_size = 1 lsl 20

let create n =
  let s = n lsr 3 in
  if s > max_size then raise (Invalid_argument "Bitvector.create") ;
  let r = n land 7 in
  String.make (if r > 0 then succ s else s) '\000'

let pp_bits fmt x =
  for k=7 downto 0 do
    Format.pp_print_char fmt (if x land (1 lsl k) > 0 then '1' else '0')
  done

let pp_elts fmt x =
  for k=0 to 7 do
    Format.pp_print_char fmt (if x land (1 lsl k) > 0 then '1' else '0')
  done

let pretty fmt s =
  for i=0 to String.length s - 1 do
    if i > 0 then Format.pp_print_space fmt () ;
    pp_elts fmt (int_of_char s.[i]) ;
  done

let is_empty s =
  try
    for i=0 to String.length s - 1 do
      if s.[i] <> '\000' then raise Exit ;
    done ; true
  with Exit -> false

let set s k =
  let p = k lsr 3 in
  if p >= String.length s then
    raise (Invalid_argument "Bitvector.set") ;
  let r = k land 7 in
  let b = int_of_char s.[p] lor (1 lsl r) in
  s.[p] <- char_of_int b

let clear s k =
  let p = k lsr 3 in
  if p >= String.length s then
    raise (Invalid_argument "Bitvector.clear") ;
  let r = k land 7 in
  let b = int_of_char s.[p] land (lnot (1 lsl r)) in
  s.[p] <- char_of_int b

let mem s k =
  let p = k lsr 3 in
  if p >= String.length s then
    raise (Invalid_argument "Bitvector.mem") ;
  let r = k land 7 in
  int_of_char s.[p] land (1 lsl r) <> 0

let once s k =
  let p = k lsr 3 in
  if p >= String.length s then
    raise (Invalid_argument "Bitvector.once") ;
  let r = k land 7 in
  let b0 = int_of_char s.[p] in
  let b1 = b0 lor (1 lsl r) in
  if b0 = b1 then false else (s.[p] <- char_of_int b1 ; true)

let iter_true f s =
  for p = 0 to String.length s - 1 do
    let x = int_of_char s.[p] in
    if x <> 0 then
      let q = p lsl 3 in
      for r = 0 to 7 do
	if x land (1 lsl r) <> 0 then f (q+r)
      done
  done

let fold_true f init s =
  let r = ref init in
  iter_true (fun i -> r := f !r i) s;
  !r

exception Result of int

let find_next_true s k =
  let p = k lsr 3 in
  if p >= String.length s then
    raise Not_found;
  let x = int_of_char s.[p] in
  let r = k land 7 in
  try
    begin
      for r' = r to 7 do
	if x land (1 lsl r') <> 0
	then raise (Result ((p lsl 3) lor r'))
      done;
      for p' = (p+1) to (String.length s - 1) do
	let x = int_of_char s.[p'] in
	if x <> 0 then
	  for r' = 0 to 7 do
	    if x land (1 lsl r') <> 0
	    then raise (Result ((p' lsl 3) lor r'))
	  done
      done;
      raise Not_found
    end
  with Result res -> res
;;

let low = [|
  0b00000001 ; (* 0: bits 0..0 *)
  0b00000011 ; (* 1: bits 0..1 *)
  0b00000111 ; (* 2: bits 0..2 *)
  0b00001111 ; (* 3: bits 0..3 *)
  0b00011111 ; (* 4: bits 0..4 *)
  0b00111111 ; (* 5: bits 0..5 *)
  0b01111111 ; (* 6: bits 0..6 *)
|]

let high = [|
  0b11111110 ; (* 0: bits 1..7 *)
  0b11111100 ; (* 1: bits 2..7 *)
  0b11111000 ; (* 2: bits 3..7 *)
  0b11110000 ; (* 3: bits 4..7 *)
  0b11100000 ; (* 4: bits 5..7 *)
  0b11000000 ; (* 5: bits 6..7 *)
  0b10000000 ; (* 6: bits 7..7 *)
|]

let set_range s a b =
  if b-a < 8 then
    for i=a to b do set s i done
  else
    let p =
      let i = a land 7 in
      let p0 = a lsr 3 in
      if i=0 then p0 else
        (* Sets bits i..7 of p0 *)
        let x = int_of_char s.[p0] lor high.(i-1) in
        s.[p0] <- char_of_int x ; succ p0
    in
    let q =
      let j = b land 7 in
      let q0 = b lsr 3 in
      if j=7 then q0 else
        (* Sets bits 0..j of q0 *)
        let x = int_of_char s.[q0] lor low.(j) in
        s.[q0] <- char_of_int x ; pred q0
    in
    for i=p to q do s.[i] <- '\255' done

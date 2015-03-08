(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

(* Notes:
   - Bits are counted from 0, in string order, then from least to 
     most significant. For instance the value of bit 11 is tested
     with (s.[1] land (1 lsl 3) == 0)
   - Strings can store more bits than the bitvector they represent;
     for instance a bitvector of size 11 is stored in a 2-bytes
     string. We (currently) do not store the actual size of the
     bitvector, which has to be provided in some informations (such as
     concat). We rely on the invariant that the extra bits are set to
     0 (this is important e.g. for equality testing). An alternative
     design could have been not to explicitely ignore these extra bits 
     in operations that are sensitive to them, but this seems more 
     error-prone. *)

type t = string

let max_size = 1 lsl 20

let ( <-< ) a b = char_of_int (((int_of_char a) lsl b) land 255);;
let ( >-> ) a b = char_of_int ((int_of_char a) lsr b);;
let ( ||| ) a b = char_of_int ((int_of_char a) lor (int_of_char b));;
let ( &&& ) a m = char_of_int ((int_of_char a) land m);;

(* Imperatively unset the extra trailing bits *)
let clean_trail size bv =
  let last = (size + 7) / 8 - 1 in
  assert (last < String.length bv);
  let r = size land 7 in
  if r > 0 then
    (let mask = 1 lsl r - 1 in
     bv.[last] <- bv.[last] &&& mask) ;
  for i = last + 1 to String.length bv - 1 do
    bv.[i] <- '\000' ;
  done ;
  bv
;;

let capacity s = String.length s * 8

let create n =
  let s = (n + 7) lsr 3 in              (* rounded-up division *)
  if s > max_size then raise (Invalid_argument "Bitvector.create") ;
  String.make s '\000'

let resize n s =
  let u = create n in
  String.blit s 0 u 0 (min (String.length s) (String.length u)) ;
  clean_trail n u

let create_set n =
  let s = (n + 7) lsr 3 in              (* rounded-up division *)
  if s > max_size then raise (Invalid_argument "Bitvector.create") ;
  let copy = String.make s (char_of_int 255) in
  let r = n land 7 in
  (* Set only the last r bits in the last byte. *)
  if r != 0 
  then copy.[s-1] <- char_of_int ((1 lsl r) - 1);
  copy
;;

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

let bnot size s =
  let len = (size + 7) / 8 in
  let copy = String.make (String.length s) '\000' in
  for i = 0 to len-1 do
    copy.[i] <- char_of_int ((lnot (int_of_char s.[i])) land 255)
  done;
  clean_trail size copy
;;


(* Internal; this function does not clean the trail for operations
   that do not need it. *)
let bitwise_bop bop a b =
  assert ((String.length a) = (String.length b));
  let copy = String.make (String.length a) '\000' in
  for i = 0 to (String.length a) - 1 do
    copy.[i] <- char_of_int (255 land (bop (int_of_char a.[i]) (int_of_char b.[i])));
  done;
  copy

let band _ = bitwise_bop (land);;
let bor _  = bitwise_bop (lor);;
let bxor _ = bitwise_bop (lxor);;
let beq size a b =
  let bv = bitwise_bop (fun x y -> lnot (x lxor y)) a b in
  clean_trail size bv
;;

let bitwise_op2 size op2 a b =
  let len = String.length a in
  assert (len = (String.length b));
  let copy = String.make len '\000' in
  for i = 0 to len - 1 do
    copy.[i] <- char_of_int (255 land (op2
					 (int_of_char a.[i])
					 (int_of_char b.[i])))
  done;
  clean_trail size copy
;;



let bitwise_op3 size op3 a b c =
  let len = String.length a in
  assert (len = (String.length b));
  assert (len = (String.length c));
  let copy = String.make len '\000' in
  for i = 0 to len - 1 do
    copy.[i] <- char_of_int (255 land (op3
					 (int_of_char a.[i])
					 (int_of_char b.[i])
					 (int_of_char c.[i])));
  done;
  clean_trail size copy
;;

let bitwise_op4 size op4 a b c d =
  let len = String.length a in
  assert (len = (String.length b));
  assert (len = (String.length c));
  assert (len = (String.length d));
  let copy = String.make len '\000' in
  for i = 0 to len - 1 do
    copy.[i] <- char_of_int (255 land
			       (op4
				  (int_of_char a.[i]) (int_of_char b.[i])
				  (int_of_char c.[i]) (int_of_char d.[i])));
  done;
  clean_trail size copy
;;


let equal = (=);;          (* String equality. *)
let compare = Pervasives.compare
let hash = Hashtbl.hash

let concat bv1 size1 bv2 size2 =
  let len1 = size1 / 8 in
  let str1 = (size1 + 7) / 8 in
  let str2 = (size2 + 7) / 8 in
  assert (str1 <= String.length bv1);
  assert (str2 <= String.length bv2);
  let newlen = (size1 + size2 + 7) / 8 in
  let copy = String.create newlen in
  String.blit bv1 0 copy 0 len1 ;
  let fst_bits = size1 land 7 in
  let snd_bits = 8 - fst_bits in

  (* Byte-aligned case. *)
  if fst_bits = 0 then
    (String.blit bv2 0 copy len1 str2;
     copy)
    
  (* Not aligned. *)
  else
    let rec loop prev_byte i =
      let j = len1 + i in
      if i <= str2 - 1
      then
	(copy.[j] <- prev_byte ||| (bv2.[i] <-< fst_bits);
	 loop (bv2.[i] >-> snd_bits) (i+1))
      else
	if j < newlen
	then copy.[j] <- (bv2.[str2-1] >-> snd_bits)
	else ()
    in
    loop bv1.[len1] 0;
    clean_trail (size1+size2) copy;;

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
;;

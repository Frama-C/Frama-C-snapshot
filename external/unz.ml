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

open Unmarshal;;

let readz ch =
  let sign = read8u ch in
  let charlen = read32u ch in
  let str = String.create charlen in 
  readblock ch (Obj.repr str) 0 charlen;
(* My beautiful string reversing code; 
   now useless :( 
  let max = pred charlen in
  for i = 0 to (pred max) / 2 do
    let c = str.[i] in
    str.[i] <- str.[max - i] ;
    str.[max - i] <- c
  done;
*)
  let n = Z.of_bits str in
  let z = if sign = 0 then n else Z.neg n in
  Obj.repr z
;;

register_custom "_z" readz;;

(*
  #load "zarith.cma" ;;
  let f = open_out "test" ;;
  let i = ref (-10000000000000000L) ;;

  while !i <= 10000000000000000L do
  output_value f (Z.of_int64 (!i)) ;
  i := Int64.add !i 100000000000L ; done
  ;;


  ocamlc -custom zarith.cma unmarshal.ml unz.ml 
*)

(*
let f = open_in "test" ;;

let i = ref (-10000000000000000L) ;;

while !i <= 10000000000000000L do
  let z = input_val f Abstract in
  let r = Z.to_int64 z in
  if (r <> !i) 
  then begin
      Format.printf "read: %Ld expected: %Ld@."
	r !i;
      assert false
    end;
  i := Int64.add !i 100000000000L ; 
done
;;
*)




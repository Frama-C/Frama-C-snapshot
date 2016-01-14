(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2009-2012 INRIA                                         *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*                                                                        *)
(*  Redistribution and use in source and binary forms, with or without    *)
(*  modification, are permitted provided that the following conditions    *)
(*  are met:                                                              *)
(*    * Redistributions of source code must retain the above copyright    *)
(*      notice, this list of conditions and the following disclaimer.     *)
(*    * Redistributions in binary form must reproduce the above           *)
(*      copyright notice, this list of conditions and the following       *)
(*      disclaimer in the documentation and/or other materials provided   *)
(*      with the distribution.                                            *)
(*    * Neither the name of the <organization> nor the names of its       *)
(*      contributors may be used to endorse or promote products derived   *)
(*      from this software without specific prior written permission.     *)
(*                                                                        *)
(*  THIS SOFTWARE IS PROVIDED BY <INRIA> ''AS IS'' AND ANY                *)
(*  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE     *)
(*  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR    *)
(*  PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL <copyright holder> BE       *)
(*  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR   *)
(*  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT     *)
(*  OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR    *)
(*  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF            *)
(*  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT             *)
(*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE     *)
(*  USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH      *)
(*  DAMAGE.                                                               *)
(*                                                                        *)
(**************************************************************************)

(* caml_unmarshal by Ineffable Casters *)

(* Version 3.11.1.8 *)


(* Basic testing only. *)

open Printf;;
open Unmarshal;;


(* 0. Identification. *)

printf "Testing: ";;
if arch_sixtyfour
then printf "64-bit "
else printf "32-bit "
;;
if arch_bigendian
then printf "big-endian "
else printf "little-endian "
;;
match (Obj.magic 1.23530711838574823e-307 : string).[1] with
| '1' -> printf "(floats are little-endian)...\n"
| '6' -> printf "(floats are big-endian)...\n"
| '5' -> printf "(floats are ARM-style mixed-endian)...\n"
| _ -> printf "(floats have unknown endianness)...\n"
;;
flush stdout;;


(* 1. Testing without transformation function. *)

let wrt v t =
  let oc = open_out_bin "test-file" in
  Marshal.to_channel oc v [Marshal.Closures];
  close_out oc;
  let ic = open_in_bin "test-file" in
  let result = input_val ic t in
  close_in ic;
  result
;;

let wr v = wrt v Abstract;;

let check cond msg =
  if not cond then failwith (sprintf "test failed (%s)" msg)
;;

let counter = ref 0;;

let test v =
  incr counter;
  check (wr v = v) (sprintf "wr%d" !counter);
;;


(* SMALL_INT, INT8, INT16, INT32, INT64 *)
for i = -130 to 130 do test i; done;;
for i = -32780 to -32750 do test i; done;;
for i = 32750 to -32780 do test i; done;;
test (-1_000_000);;
test 1_000_000_000;;
test (1 lsl 60);;
test (-1 lsl 60);;
test max_int;;
test min_int;;

(* SMALL_STRING, STRING8, STRING32 *)
test "short";;
for i = 0 to 40 do test (String.create i) done;;
for i = 250 to 260 do test (String.create i) done;;
test (String.create 1255);;

(* DOUBLE_*, DOUBLE_ARRAY8_*, DOUBLE_ARRAY32_* *)
test 0.0;;
test 1.0;;
test infinity;;
test (-. infinity);;
test 1.234e-225;;
for i = 0 to 300 do test (Array.init i float_of_int) done;;

(* SMALL_BLOCK, BLOCK32 *)
test [1; 2; 3];;
type t0 =
  | C01 of int
  | C02 of int * int
  | C03 of int * int * int
  | C04 of int * int * int * int
  | C05 of int * int * int * int * int
  | C06 of int * int * int * int * int * int
  | C07 of int * int * int * int * int * int * int
  | C08 of int
  | C09 of int * int * int * int * int * int * int
  | C10 of int * int * int * int * int * int
  | C11 of int * int * int * int * int
  | C12 of int * int * int * int
  | C13 of int * int * int
  | C14 of int * int
  | C15 of int
  | C16 of int * int * int * int * int * int * int * int
;;
test [
  C01 (1);
  C02 (1, 2);
  C03 (1, 2, 3);
  C04 (1, 2, 3, 4);
  C05 (1, 2, 3, 4, 5);
  C06 (1, 2, 3, 4, 5, 6);
  C07 (1, 2, 3, 4, 5, 6, 7);
  C08 (1);
  C09 (1, 2, 3, 4, 5, 6, 7);
  C10 (1, 2, 3, 4, 5, 6);
  C11 (1, 2, 3, 4, 5);
  C12 (1, 2, 3, 4);
  C13 (1, 2, 3);
  C14 (1, 2);
  C15 (1);
  C16 (1, 2, 3, 4, 5, 6, 7, 8);
];;
type t1 =
  | A
  | B of int
  | C of float
  | D of bool
  | E
  | F
  | G
  | H
  | I
  | J
;;
test [A; B 10; C 100.; D false; E; F; G; H; I];;

(* SHARED8 *)
let rec l =
  J :: I :: H :: G :: F :: E :: D true :: C 1e100 :: B (-1000) :: A :: l
in
let v = wr l in
for i = 0 to 9; do
  check (List.nth l i = List.nth v i) "share1";
  check (List.nth v i == List.nth v (i + 10)) "share2";
done;;
let a = ref 0;;
let b = ref 1;;
let x = Array.make 1_000_000 a;;
for i = 1 to 499_999 do x.(2 * i) <- b done;;
let v = (wr x : int ref array);;
check (v.(0) == v.(1)) "share3";;
check (v.(1) == v.(999_999)) "share4";;
check (v.(2) == v.(400_000)) "share5";;
check (v.(2) == v.(999_998)) "share6";;

(* SHARED8, SHARED16, SHARED32 *)
for i = 1 to 499_999 do x.(2 * i) <- ref i done;;
let v = (wr x : int ref array);;
v.(0) := -1;;
for i = 1 to 499_999 do
  check (!(v.(2 * i)) = i) "share7";
  check (v.(2 * i + 1) == v.(0)) "share8";
done;;

(* CODEPOINTER *)


let raw_value x =
  let result = Obj.dup (Obj.repr 0L) in
  let foo = (Obj.obj result : Int64.t) in
  Obj.set_field result 1 (Obj.repr x);
  foo
;;

let value_raw x = Obj.field (Obj.repr x) 1;;



let x = fun x -> (x + 1);;
let v = (wr x : int -> int);;
check (v 0 = 1) "code1";;
let x =
  let a = 1 in
  let b = 2 in
  fun x -> (x + a, x + b)
;;
let v = (wr x : int -> int * int);;
check (fst (v 10) = 11) "code2";;
check (snd (v 10) = 12) "code3";;

(* INFIXPOINTER *)
let rec f x = if x = 0 then g x else x + 10
and g x = if x <> 0 then f x else x + 20
;;
let v = (wr f : int -> int);;
check (v 0 = 20) "infix0";;
check (v 5 = 15) "infix1";;
let w = (wr g : int -> int);;
check (w 0 = 20) "infix2";;
check (w 5 = 15) "infix3";;

(* CUSTOM *)

test 0l;;
test 1l;;
test 0x7FFFFFFFl;;
test 0x80000000l;;

test 0L;;
test (-1L);;
test 0x7fffffffffffffffL;;
test 0x8000000000000000L;;

test 0n;;
test 1n;;
test 10n;;
test 0x7fffffffn;;
test 0x80000000n;;

open Num;;

ignore Unmarshal_nums.t_num;;

let test v =
  incr counter;
  check (string_of_num (wr (num_of_string v)) = v) (sprintf "num%d" !counter);
;;

test "0";;
test "1";;
test "-1";;
test "100000000000000000000000000000000";;
test "77777777777777777777777777777/2222222222222222222222";;
test "-314159265358979/2718281828";;


(* 2. Testing with transformation functions. *)

let v = [1; 2; 3; 4; 5; 12847];;

let double x = let x = (Obj.obj x : int) in Obj.repr (x + x);;
let t_list2 = t_list (Transform (t_int, double));;

let test v =
  incr counter;
  let w = wrt v t_list2 in
  let f x y = check (x + x = y) (sprintf "list2-%d" !counter) in
  List.iter2 f v w;
;;

test v;;

let t_list3 = t_list (Return (t_int, fun () -> (Obj.repr 1)));;

let test v ty =
  incr counter;
  let w = wrt v ty in
  let f x y = check (1 = y) (sprintf "list3-%d" !counter) in
  List.iter2 f v w;
;;
test v t_list3;;

let t_list4 = Dynamic (fun () -> t_list3);;
test v t_list4;;

(* 3. Testing multi-allocated constructors. *)

type t = A of int * int | B of int
let l = [ A (3, 4); B 5 ]
let t_l = 
  t_list (Structure (Sum [| [| Abstract; Abstract |]; [| Abstract |] |]));;

let test v ty =
  incr counter;
  let w = wrt v ty in
  check (v = w) (sprintf "list3-%d" !counter)
;;
test l t_l;;


(* 4. Conclusion. *)

printf "All tests passed.\n";;

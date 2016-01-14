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


(* Warning:

   If you are new to OCaml, don't take this as an example of good code.

*)

open Unmarshal;;

let readnat_big32 ch =
  let len = read32u ch in
  let v = Obj.repr (Nat.create_nat len) in
  readblock ch v 4 (len * 4);
  v
;;

let readnat_little32 ch =
  let len = read32u ch in
  let v = Obj.repr (Nat.create_nat len) in
  for i = 1 to len do readblock_rev ch v (i * 4) 4 done;
  v
;;

let readnat_little64 ch =
  let len = read32u ch in
  let size = (len + 1) / 2 in
  let v = Nat.create_nat size in
  Nat.set_digit_nat v (size - 1) 0;
  let v = Obj.repr v in
  for i = 2 to len + 1 do readblock_rev ch v (i * 4) 4 done;
  v
;;

let readnat_big64 ch =
  let len = read32u ch in
  let size = (len + 1) / 2 in
  let v = Nat.create_nat size in
  Nat.set_digit_nat v (size - 1) 0;
  let v = Obj.repr v in
  let rec loop i =
    if i < len then begin
      readblock ch v (12 + i * 4) 4;
      if i + 1 < len then begin
        readblock ch v (8 + i * 4) 4;
        loop (i + 2);
      end
    end
  in loop 0;
  v
;;

let readnat =
  if arch_sixtyfour
  then if arch_bigendian then readnat_big64 else readnat_little64
  else if arch_bigendian then readnat_big32 else readnat_little32
;;

register_custom "_nat" readnat;;

let t_nat = Abstract;;
let t_big_int = Abstract;;
let t_ratio = Abstract;;
let t_num = Abstract;;

(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

(* -------------------------------------------------------------------------- *)
(* --- Sanitizer                                                          --- *)
(* -------------------------------------------------------------------------- *)

(*
   Keeps only alphanumerical characters,
   remove consecutive, trailing and leading `_`
*)

type state = START | SEP | CHAR | TRUNCATE

type buffer = {
  content : Buffer.t ;
  truncate : int ;
  mutable lastsep : int ;
  mutable state : state ;
}

let create ?(truncate=false) n = {
  content = Buffer.create n ;
  truncate = if truncate then n else max_int ;
  lastsep = 0 ;
  state = START ;
}

let clear buffer =
  begin
    Buffer.clear buffer.content ;
    buffer.state <- START ;
    buffer.lastsep <- 0 ;
  end

let add_sep buffer =
  if buffer.state = CHAR then
    let offset = Buffer.length buffer.content in
    if offset < buffer.truncate then
      begin
        buffer.state <- SEP ;
        buffer.lastsep <- offset ;
      end
    else
      begin
        buffer.state <- TRUNCATE ;
        (* TODO [OCaml 4.05] Buffer.truncate buffer.content buffer.lastsep ; *)
      end

let add_char buffer = function
  | ('a'..'z' | 'A'..'Z' | '0'..'9') as c ->
    begin
      match buffer.state with
      | START ->
        Buffer.add_char buffer.content c ;
        buffer.state <- CHAR
      | SEP ->
        Buffer.add_char buffer.content '_' ;
        Buffer.add_char buffer.content c ;
        buffer.state <- CHAR
      | CHAR ->
        Buffer.add_char buffer.content c
      | TRUNCATE -> ()
    end
  | '_' | '-' | ' ' | '\t' | ',' | ';' | '.' | '/' | '\\' | ':' ->
    add_sep buffer
  | _ -> ()

let add_string buffer s = String.iter (add_char buffer) s

let rec add_list buffer = function
  | [] -> ()
  | p::ps -> add_string buffer p ; add_sep buffer ; add_list buffer ps

let contents buffer =
  (* TODO [OCaml 4.05] simply buffer contents if using Buffer.truncate *)
  let s = Buffer.contents buffer.content in
  if buffer.state = TRUNCATE then
    String.sub s 0 buffer.lastsep
  else s

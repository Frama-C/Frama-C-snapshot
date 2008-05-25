(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU General Public                   *)
(*  License version 2, as published by the Free Software Foundation.      *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU General Public License version 2 for more details         *)
(*  (enclosed in the file GPL).                                           *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: loc.ml,v 1.25 2008/02/05 12:10:49 marche Exp $ i*)

let join (b,_) (_,e) = (b,e)

(*s Error locations. *)

let finally ff f x =
  let y = try f x with e -> ff (); raise e in ff (); y

(***
let linenum f b =
  let cin = open_in f in
  let rec lookup n l cl =
    if n = b then 
      (l,cl)
    else 
      let c = input_char cin in
      lookup (succ n) (if c == '\n' then succ l else l) 
	(if c == '\n' then 0 else succ cl)
  in
  try let r = lookup 0 1 0 in close_in cin; r with e -> close_in cin; raise e

let safe_linenum f b = try linenum f b with _ -> (1,1)
***)

open Format
open Lexing

(*s Line number *)

let report_line fmt l = fprintf fmt "%s:%d:" l.pos_fname l.pos_lnum

(* Lexing positions *)

type position = Lexing.position * Lexing.position

exception Located of position * exn

let dummy_position = Lexing.dummy_pos, Lexing.dummy_pos

let gen_report_line fmt (f,l,b,e) = 
  fprintf fmt "File \"%s\", " f;
  fprintf fmt "line %d, characters %d-%d" l b e

type floc = string * int * int * int

let dummy_floc = ("",0,0,0)

let extract (b,e) = 
  let f = b.pos_fname in
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol in
  let lc = e.pos_cnum - b.pos_bol in
  (f,l,fc,lc)

let gen_report_position fmt loc = 
  gen_report_line fmt (extract loc)

let report_position fmt pos = 
  fprintf fmt "%a:@\n" gen_report_position pos

let string =
  let buf = Buffer.create 1024 in
  fun loc ->
    let fmt = Format.formatter_of_buffer buf in
    Format.fprintf fmt "%a@?" gen_report_position loc;
    let s = Buffer.contents buf in
    Buffer.reset buf;
    s

let parse s =
  Scanf.sscanf s "File %S, line %d, characters %d-%d"
    (fun f l c1 c2 -> 
       (*Format.eprintf "Loc.parse %S %d %d %d@." f l c1 c2;*)
       let p = 
	 { Lexing.dummy_pos with pos_fname = f; pos_lnum = l; pos_bol = 0 }
       in
       { p with pos_cnum = c1 }, { p with pos_cnum = c2 })

let report_obligation_position fmt loc =
  let (f,l,b,e) = loc in
  fprintf fmt "Why obligation from file \"%s\", " f;
  fprintf fmt "line %d, characters %d-%d:" l b e

let current_offset = ref 0
let reloc p = { p with pos_cnum = p.pos_cnum + !current_offset }

(* Identifiers localization *)

let ident_t = Hashtbl.create 97
let add_ident = Hashtbl.add ident_t
let ident = Hashtbl.find ident_t

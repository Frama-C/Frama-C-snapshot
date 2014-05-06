(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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
(* --- Fast Report for WP                                                 --- *)
(* -------------------------------------------------------------------------- *)

{
	
  (* -------------------------------------------------------------------------- *)
  (* --- Time Utilities                                                     --- *)
  (* -------------------------------------------------------------------------- *)

  let epsilon = 0.0005

  let get_time ladder t =
    let rec dicho ladder t i j =
      let k = (i+j)/2 in
      if i=k then j else
	let d = ladder.(k) in
	if t < d then dicho ladder t i k else
	  if t > d then dicho ladder t k j else k 
    in
    if t <= ladder.(0) then 0 else
      let n = Array.length ladder in
      if t > ladder.(n-1) then n else
	dicho ladder t 0 (n-1)
	  
  let rdiv t n =
    let d = floor (t /. n) in
    let r = t -. d *. n in
    d , r
    
  let pp_time fmt t =
    if t < 1.0 then Format.fprintf fmt "%dms" (truncate (t *. 1000.0 +. 0.5)) else
      if t < 60.0 then 
	let dt = t -. floor t in
	if dt < 0.1 
	then Format.fprintf fmt "%.0fs" t
	else Format.fprintf fmt "%.1fs" t 
      else
	if t < 3600.0 then
	  let minutes,seconds = rdiv t 60.0 in
	  if seconds < 1.0 then
	    Format.fprintf fmt "%d'" (truncate minutes)
	  else
	    Format.fprintf fmt "%d'%ds" (truncate minutes) (truncate seconds)
	else
	  let hours,seconds = rdiv t 3600.0 in
	  let minutes,_ = rdiv seconds 60.0 in
	  if minutes < 1.0 then
	    Format.fprintf fmt "%dh" (truncate hours)
	  else
	    Format.fprintf fmt "%dh%d'" (truncate hours) (truncate minutes)

  let pp_time_range ladder fmt t =
    let k = get_time ladder t in
    let n = Array.length ladder in
    if k > n then Format.fprintf fmt ">%a" pp_time ladder.(n-1)
    else pp_time fmt ladder.(k)
	
  (* -------------------------------------------------------------------------- *)
  (* --- Formatters Syntax                                                  --- *)
  (* -------------------------------------------------------------------------- *)
	
  type command =
    | CMD of string
    | ARG of string * string
    | TEXT

  type console = {
    env : (Format.formatter -> string -> string -> unit) ;
    line : Buffer.t ;
    mutable spaces : int ;
    fline : Format.formatter ;
    foutput : Format.formatter ;
  }

  let spaces = String.make 80 ' '

  let rec add_spaces buffer n =
    if n > 0 then
      if n < 80 then Buffer.add_substring buffer spaces 0 n 
      else ( Buffer.add_string buffer spaces ; add_spaces buffer (n-80) )

  let spaces console =
    begin
      Format.pp_print_flush console.fline () ;
      if console.spaces > 0 then 
	( add_spaces console.line console.spaces ;
	  console.spaces <- 0 ) ;
    end

  let flush console = 
    begin
      spaces console ;
      Format.pp_print_string console.foutput (Buffer.contents console.line) ;
      Buffer.clear console.line ;
    end

  let write console text = spaces console ; Buffer.add_string console.line text
  let env console cmd arg = spaces console ; console.env console.fline cmd arg

}

let blank = [ ' ' '\t' ]
let number = [ '0'-'9' ]+
let ident = [ 'a'-'z' 'A'-'Z' '-' '0'-'9' ]+ 

rule word console = 
    parse
	eof
	  { flush console }

      | '\n'
	  { flush console ; Format.pp_print_newline console.foutput () ; word console lexbuf }
	    
      | ' '
          { console.spaces <- succ console.spaces ; word console lexbuf }

      | "&&" { write console "&" ; word console lexbuf }
      | "%%" { write console "%" ; word console lexbuf }

      | '&' (number as arg) ':'
	  {
	    Format.pp_print_flush console.fline () ;
	    add_spaces console.line (int_of_string arg - Buffer.length console.line) ;
	    console.spaces <- 0 ;
	    word console lexbuf
	  }

      | "%{" (ident as cmd) ':' (ident as arg) '}'      
      | '%' (ident as cmd) ':' (ident as arg)
 
	  { env console cmd arg ; word console lexbuf }

      | "%{" (ident as cmd) "}"
      | '%' (ident as cmd) 

	  { env console cmd "" ; word console lexbuf }

      | _ { write console (Lexing.lexeme lexbuf) ; word console lexbuf }

and command = parse
  | blank* '@' (ident as cmd) blank* { CMD cmd }
  | blank* '@' (ident as cmd) blank+ '"' ([^ '"']* as arg) '"' blank* { ARG(cmd,arg) }
  | _ { TEXT }
	  
{

  let pretty env fmt msg =
    let lexbuf = Lexing.from_string msg in
    let line = Buffer.create 80 in
    word {
      line = line ;
      fline = Format.formatter_of_buffer line ;
      foutput = fmt ;
      env = env ;
      spaces = 0 ;
    } lexbuf

  let command msg =
    let lexbuf = Lexing.from_string msg in
    command lexbuf

}

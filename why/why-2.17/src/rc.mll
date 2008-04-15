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
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

{

  open Lexing

type rc_value =
  | RCint of int
  | RCbool of bool
  | RCfloat of float
  | RCstring of string
  | RCident of string

let buf = Buffer.create 17

let current_rec = ref ""
let current_list = ref []
let current = ref []

let push_field key value =
  current_list := (key,value) :: !current_list

let push_record () =
  if !current_list <> [] then
    current := (!current_rec,List.rev !current_list) :: !current;
  current_rec := "";
  current_list := []

}

let space = [' ' '\t' '\r' '\n']+
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let ident = (letter | '_') (letter | digit | '_' | '-') * 
let sign = '-' | '+' 
let integer = sign? digit+
let mantissa = ['e''E'] sign? digit+
let real = sign? digit* '.' digit* mantissa?
let escape = ['\\''"''n''t''r']

rule record = parse
  | space 
      { record lexbuf }
  | '[' (ident as key) ']'  
      { push_record();
	current_rec := key;
	record lexbuf 
      }
  | eof 
      { push_record () }
  | (ident as key) space* '=' space* 
      { value key lexbuf }
  | _ as c
      { failwith ("Rc: invalid keyval pair starting with " ^ String.make 1 c) }

and value key = parse
  | integer as i
      { push_field key (RCint (int_of_string i));
        record lexbuf }
  | real as r
      { push_field key (RCfloat (float_of_string r));
        record lexbuf }
  | '"' 
      { Buffer.clear buf;
	string_val key lexbuf } 
  | "true"
      { push_field key (RCbool true);
        record lexbuf }
  | "false"
      { push_field key (RCbool false);
        record lexbuf }
  | ident as id
      { push_field key (RCident (*kind_of_ident*) id);
        record lexbuf }
  | _ as c
      { failwith ("Rc: invalid value starting with " ^ String.make 1 c) }
  | eof
      { failwith "Rc: unterminated keyval pair" }

and string_val key = parse 
  | '"' 
      { push_field key (RCstring (Buffer.contents buf));
	record lexbuf
      }
  | [^ '\\' '"'] as c
      { Buffer.add_char buf c;
        string_val key lexbuf }
  | '\\' (['\\''\"'] as c)   
      { Buffer.add_char buf c;
        string_val key lexbuf }
  | '\\' 'n'
      { Buffer.add_char buf '\n';
        string_val key lexbuf }
  | '\\' (_ as c)
      { Buffer.add_char buf '\\';
        Buffer.add_char buf c;
        string_val key lexbuf }
  | eof
      { failwith "Rc: unterminated string" }


{

  let from_file f =
      let c = 
	try open_in f 
	with Sys_error _ -> raise Not_found
(*
	  Format.eprintf "Cannot open file %s@." f;
	  exit 1
*)
      in
      current := [];
      let lb = from_channel c in
      record lb;
      close_in c;
      List.rev !current

}

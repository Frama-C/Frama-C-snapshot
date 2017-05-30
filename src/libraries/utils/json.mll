(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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
(* --- Json Parser/Lexer                                                  --- *)
(* -------------------------------------------------------------------------- *)

{

type t =
  | Null
  | True | False
  | String of string
  | Number of string
  | Int of int
  | Float of float
  | Array of t list
  | Assoc of (string * t) list

let equal = (=)
let compare = Pervasives.compare

type token = EOF | TRUE | FALSE | NULL | KEY of char
           | STR of string | INT of string | DEC of string
}

rule token = parse
 '\n' { Lexing.new_line lexbuf ; token lexbuf }
| [ ' ' '\t' '\r' ] { token lexbuf }
| '"' { let buffer = Buffer.create 80 in
        string buffer lexbuf ;
        STR(Buffer.contents buffer) }
| '-'? [ '0'-'9' ]+ { INT(Lexing.lexeme lexbuf) }
| '-'? [ '0'-'9' ]* '.' ['0'-'9']* ( ['e' 'E'] ['-' '+']? ['0'-'9']+ )?
    { DEC(Lexing.lexeme lexbuf) }
| [ '[' ']' '{' '}' ':' ',' ] as c { KEY c }
| "true" { TRUE }
| "false" { FALSE }
| "null" { NULL }
| eof { EOF }
| _ { failwith "un-recognised token" }

and string buffer = parse
  | '"' { () }
  | "\\\\" { Buffer.add_char buffer '\\' ; string buffer lexbuf }
  | "\\n" { Buffer.add_char buffer '\n' ; string buffer lexbuf }
  | "\\t" { Buffer.add_char buffer '\t' ; string buffer lexbuf }
  | "\\r" { Buffer.add_char buffer '\r' ; string buffer lexbuf }
  | "\\\"" { Buffer.add_char buffer '"' ; string buffer lexbuf }
  | '\n' | eof { failwith "non-terminated string" }
  | _ as c { Buffer.add_char buffer c ; string buffer lexbuf }


{

type input = {
  lexbuf : Lexing.lexbuf ;
  mutable token : token ;
}

let skip input =
  if input.token <> EOF then input.token <- token input.lexbuf

(* Termination hints:
   - unless EOF, parse_value always eat a token
   - parse_array always eat a token or call parse_value with non-EOF input
   - parse_object always eat a token
   - parse_entry always eat a token or call parse_value with non-EOF input
*)
let rec parse_value input =
  match input.token with
  | EOF -> Null
  | TRUE -> skip input ; True
  | FALSE -> skip input ; False
  | NULL -> skip input ; Null
  | STR a -> skip input ; String a
  | INT a -> skip input ; (try Int(int_of_string a) with _ -> Number a)
  | DEC a -> skip input ; (try Float(float_of_string a) with _ -> Number a)
  | KEY '[' -> skip input ; Array (parse_array [] input)
  | KEY '{' -> skip input ; Assoc (parse_object [] input)
  | _ -> failwith "unexpected token"

and parse_array es input =
  match input.token with
  | EOF -> failwith "non-terminated array"
  | KEY ']' -> skip input ; List.rev es
  | KEY ',' -> skip input ; parse_array es input
  | _ -> let e = parse_value input in parse_array (e::es) input

and parse_object es input =
  match input.token with
  | EOF -> failwith "non-terminated record"
  | KEY '}' -> skip input ; List.rev es
  | KEY ',' -> skip input ; parse_object es input
  | STR a -> skip input ; let e = parse_entry a input in parse_object (e::es) input
  | _ -> failwith "missing name"

and parse_entry a input =
  match input.token with
  | EOF -> failwith "non-terminated record"
  | KEY ':' -> skip input ; parse_entry a input
  | _ -> a , parse_value input

let parse_file input =
  let content = parse_value input in
  if input.token <> EOF then failwith "unexpected end-of-file" ;
  content

exception Error of string * int * string

let error lexbuf msg =
  let open Lexing in
  let position = Lexing.lexeme_start_p lexbuf in
  let token = Lexing.lexeme lexbuf in
  Error(position.pos_fname,position.pos_lnum,
        Printf.sprintf "%s (at %S)" msg token)

let load_lexbuf lexbuf =
  try
    let token = token lexbuf in
    parse_file { lexbuf ; token }
  with Failure msg -> raise (error lexbuf msg)

let load_string text = load_lexbuf (Lexing.from_string text)
let load_channel inc = load_lexbuf (Lexing.from_channel inc)

let load_file file =
  let inc = open_in file in
  try
    let content = load_channel inc in
    close_in inc ; content
  with e ->
    close_in inc ; raise e

let rec pp fmt v = let open Format in
  match v with
  | Null -> pp_print_string fmt "null"
  | True -> pp_print_string fmt "true"
  | False -> pp_print_string fmt "false"
  | String s -> fprintf fmt "%S" s
  | Number s -> pp_print_string fmt s
  | Int a -> pp_print_int fmt a
  | Float f -> pp_print_float fmt f
  | Array [] -> pp_print_string fmt "[]"
  | Array (e::es) ->
      Format.fprintf fmt "@[<hov 2>[ %a" pp e ;
      List.iter (fun e -> Format.fprintf fmt ",@ %a" pp e) es ;
      Format.fprintf fmt " ]@]"
  | Assoc [] -> pp_print_string fmt "{}"
  | Assoc (e::es) ->
      Format.fprintf fmt "@[<hov 2>{ %a" pp_entry e ;
      List.iter (fun e -> Format.fprintf fmt ",@ %a" pp_entry e) es ;
      Format.fprintf fmt " }@]"

and pp_entry fmt (a,v) = Format.fprintf fmt "@[<hov 2>%S: %a@]" a pp v

let dump_string f s =
  let quote = "\"" in
  f quote ; f (String.escaped s) ; f quote

let rec dump f = function
  | Null -> f "null"
  | True -> f "true"
  | False -> f "false"
  | String s -> dump_string f s
  | Number s -> f s
  | Int a -> f (string_of_int a)
  | Float x -> f (string_of_float x)
  | Array [] -> f "[]"
  | Array (e::es) ->
      f "[" ; dump f e ;
      List.iter (fun e -> f "," ; dump f e) es ;
      f "]"
  | Assoc [] -> f "{}"
  | Assoc (e::es) ->
      f "{" ;
      dump_entry f e ;
      List.iter (fun e -> f "," ; dump_entry f e) es ;
      f "}"

and dump_entry f (a,v) =
  dump_string f a ; f ":" ; dump f v

let save_buffer ?(pretty=true) buffer v =
  if pretty then
    let fmt = Format.formatter_of_buffer buffer in
    pp fmt v ; Format.pp_print_flush fmt ()
  else
    dump (Buffer.add_string buffer) v

let save_channel ?(pretty=true) out v =
  if pretty then
    let fmt = Format.formatter_of_out_channel out in
    pp fmt v ; Format.pp_print_flush fmt ()
  else
    dump (output_string out) v ; flush out

let save_string ?(pretty=true) v =
  let buffer = Buffer.create 80 in
  save_buffer ~pretty buffer v ;
  Buffer.contents buffer

let save_file ?(pretty=true) file v =
  let out = open_out file in
  try
    save_channel ~pretty out v ;
    close_out out
  with e ->
    close_out out ; raise e

let invalid name = raise (Invalid_argument ("Json." ^ name))

let bool = function
  | True -> true
  | False -> false
  | _ -> invalid "bool"

let int = function
  | Null -> 0
  | Int n -> n
  | Float f -> (try int_of_float f with _ -> invalid "int")
  | Number s | String s -> (try int_of_string s with _ -> invalid "int")
  | _ -> invalid "int"

let float = function
  | Null -> 0.0
  | Float f -> f
  | Int n -> (try float_of_int n with _ -> invalid "float")
  | Number s | String s -> (try float_of_string s with _ -> invalid "float")
  | _ -> invalid "float"

let string = function
  | Null -> ""
  | Int n -> string_of_int n
  | Float f -> string_of_float f
  | Number s | String s -> s
  | _ -> invalid "string"

let list = function
  | Null -> []
  | Array es -> es
  | _ -> invalid "list"

let array = function
  | Null -> [| |]
  | Array es -> Array.of_list es
  | _ -> invalid "array"

let field f = function
  | Null -> raise Not_found
  | Assoc fs -> List.assoc f fs
  | _ -> invalid "field"

let fold f v w = match v with
  | Null -> w
  | Assoc fs -> List.fold_left (fun w (e,v) -> f e v w) w fs
  | _ -> invalid "fold"

let of_bool b = if b then True else False
let of_int k = Int k
let of_string s = String s
let of_float f = Float f
let of_list xs = Array xs
let of_array xs = Array (Array.to_list xs)
let of_fields m = Assoc m

}

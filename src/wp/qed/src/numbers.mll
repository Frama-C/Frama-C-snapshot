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
(* --- Parsing for Constants                                              --- *)
(* -------------------------------------------------------------------------- *)

{
  
  type base = Dec | Hex
  type sign = Pos | Neg
      
  type cst = {
    base : base ;
    sign : sign ;
    man  : string ;
    com  : string ;
    exp  : int ;
  }

  let error m s = raise (Invalid_argument (Printf.sprintf "%s %S" m s))

  let sign = function
    | "-" -> Neg
    | "+" -> Pos
    | "" -> Pos
    | s -> error "Unexpected sign" s

  let exp e = 
    (* first char: exponent, second char: optional sign *)
    let n = String.length e in
    if n > 1 then
      let k = if e.[1] = '+' then 2 else 1 in
      int_of_string (String.sub e k (String.length e - k))
    else 0

  let rec trail s k = if k >= 0 && s.[k] = '0' then trail s (pred k) else k
  let rec head s k = if k < String.length s && s.[k] = '0' then head s (succ k) else k

  let compile sign base man com exp =
    let man =
      let k = head man 0 in
      let n = String.length man in
      if k < n then String.sub man k (n-k) else "" in
    let com =
      let n = String.length com in
      let k = trail com (n-1) in
      if k > 0 then String.sub com 1 k else "" in
    { base ; sign ; man ; com ; exp }

}

let zero = '0'*
let sign = ['-' '+']
let digit = ['0'-'9']
let exp = sign? digit+
let hex = ['a'-'f' 'A'-'F' '0'-'9']

rule token = parse

    (sign? as s) "0d"? (digit* as m) (("." digit*)? as c) ((['e' 'E'] exp)? as e) eof
      { compile (sign s) Dec m c (exp e) }

  | (sign? as s) "0x"? (hex* as m) (("." hex*)? as c) ((['p' 'P'] exp)? as e) eof
      { compile (sign s) Hex m c (exp e) }

  | _ { raise Not_found }

and token_dec = parse

    (sign? as s) "0d"? (digit* as m) (("." digit*)? as c) ((['e' 'E'] exp)? as e) eof
      { compile (sign s) Dec m c (exp e) }

  | _ { raise Not_found }
    
and token_hex = parse
    
    (sign? as s) "0x"? (hex* as m) (("." hex*)? as c) ((['p' 'P'] exp)? as e) eof
      { compile (sign s) Hex m c (exp e) }

  | _ { raise Not_found }

{

  let parse ?base s = 
    try
      let lexbuf = Lexing.from_string s in
      match base with
	| None -> token lexbuf
	| Some Dec -> token_dec lexbuf
	| Some Hex -> token_hex lexbuf
    with Not_found -> error "Unrecognized constant" s

  let pretty fmt cst =
    begin
      if cst.sign = Neg then Format.pp_print_char fmt '-' ;
      Format.pp_print_char fmt '.' ;
      Format.pp_print_string fmt cst.man ;
      Format.pp_print_string fmt cst.com ;
      Format.pp_print_char fmt (match cst.base with Dec -> 'e' | Hex -> 'p') ;
      Format.pp_print_int fmt cst.exp ;
    end

  let is_zero cst = cst.man = "" && cst.com = ""

  let hex c = 
    let d = match c with
      | '0' .. '9' -> int_of_char '0'
      | 'a' .. 'f' -> int_of_char 'a' - 10
      | 'A' .. 'F' -> int_of_char 'A' - 10
      | _ -> 
	  let e = "?" in e.[0] <- c ; 
	  error "Incorrect hex-digit" e 
    in int_of_char c - d

  open Big_int

  let rec add_hex s a i =
    if i < String.length s then
      let d = hex s.[i] in
      let a = shift_left_big_int a 4 in
      let a = add_int_big_int d a in
      add_hex s a (succ i)
    else a

  let of_hex s (* non empty *) =
    let u = hex s.[0] in
    let a = big_int_of_int u in
    add_hex s a 1

  let big_int_of_hex s =
    if String.length s > 0 
    then of_hex s
    else zero_big_int

  let dec_of_hex s =
    if String.length s > 0 
    then string_of_big_int (of_hex s)
    else ""

  let power_of_two e =
    if e < 0 then raise (Invalid_argument "negative power") ;
    let a = shift_left_big_int unit_big_int e in
    string_of_big_int a

  let power_of_ten e =
    if e < 0 then raise (Invalid_argument "negative power") ;
    let s = String.make (succ e) '0' in
    s.[0] <- '1' ; s

  let significant cst = 
    let digits = cst.man ^ cst.com in
    let coma = String.length cst.com in
    let exp = match cst.base with
      | Dec -> cst.exp - coma
      | Hex -> cst.exp - 4 * coma
    in digits , exp

}

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

open Format_types
open Format_pprint

exception Invalid_format


let warn f = Options.Self.warning ~current:true f

(* ************************************************************************ *)
(* printf format verification                                               *)
(* ************************************************************************ *)

(* true = valid and useful,
   false = valid but useless and
   Invalid_format = invalid *)
let check_flag spec flag =
  let cs = spec.f_conversion_specifier in
  match flag, cs with
  | FSharp, #has_alternative_form -> true
  | FZero, #integer_specifier when Extlib.has_some spec.f_precision ->
      warn "Flag 0 is ignored when a precision is specified"; false
  | FZero, #numeric_specifier when List.mem FMinus spec.f_flags ->
      warn "Flag 0 is ignored when flag - is also specified."; false
  | FZero, #numeric_specifier -> true
  | FMinus, cs when cs <> `n -> true
  | FSpace, #signed_specifier when List.mem FPlus spec.f_flags ->
      warn "Flag ' ' is ignored when flag + is also specified."; false
  | FSpace, #signed_specifier -> true
  | FPlus, (#signed_specifier | #float_specifier) -> true
  | _ -> 
      warn "Flag %a and conversion specififer %a are not compatibles."
        pp_flag flag
        pp_cs (spec.f_conversion_specifier,spec.f_capitalize);
      raise Invalid_format

let check_cs_compatibility cs capitalized has_field_width has_precision =
  match cs with
  | (`n | `c | `p) as cs when has_precision ->
      warn "Conversion specifier %a does not expect a precision."
        pp_cs (cs, capitalized) ;
      raise Invalid_format
  | `n when has_field_width ->
      warn "Conversion specifier n does not expect a field width.";
      raise Invalid_format
  | _ -> ()

let rec make_flags_unique = function
  | [] -> []
  | f :: l ->
      if List.mem f l then (
        warn "Multiple usage of flag '%a'." pp_flag f;
        make_flags_unique l
      ) else
        f :: make_flags_unique l

(* When checking, we don't really care which type are returned but only if
   it can be returned *)
let find_typedef : Format_typer.typdef_finder =
  fun _namespace _name -> Cil.voidType

let check_f_specification spec =
  (* Check the correctness of precision and field width fields *)
  check_cs_compatibility spec.f_conversion_specifier spec.f_capitalize
    (spec.f_precision <> None) (spec.f_field_width <> None);
  (* Check the combination of conversion specifier and length modifier *)
  begin
    try ignore (Format_typer.type_f_specifier ~find_typedef spec)
    with Format_typer.Invalid_specifier ->
      warn "Length modifier %a and conversion specifier %a \
            are not compatibles."
        (Pretty_utils.pp_opt pp_lm) spec.f_length_modifier
        pp_cs (spec.f_conversion_specifier,spec.f_capitalize);
      raise Invalid_format
  end;
  (* Check and filter flags *)
  let flags = make_flags_unique spec.f_flags in
  let flags = List.filter (check_flag spec) flags in
  { spec with f_flags = flags}

let check_s_specification spec =
  (* Check the correctness of field width *)
  check_cs_compatibility spec.s_conversion_specifier false
    false (spec.s_field_width <> None);
  (* Check the combination of conversion specifier and length modifier *)
  begin
    try ignore (Format_typer.type_s_specifier ~find_typedef spec)
    with Format_typer.Invalid_specifier ->
      warn "Length modifier %a and conversion specifier %a \
            are not compatibles."
        (Pretty_utils.pp_opt pp_lm) spec.s_length_modifier
        pp_cs (spec.s_conversion_specifier,false);
      raise Invalid_format
  end;
  spec

let check_token f = function
  | Char _ as c -> c
  | Specification s -> Specification (f s)

let check_f_format format =
  List.map (check_token check_f_specification) format

let check_s_format format =
  List.map (check_token check_s_specification) format

let check_format = function
  | FFormat f -> FFormat (check_f_format f)
  | SFormat s -> SFormat (check_s_format s)


(* ************************************************************************ *)
(* Buffers                                                                  *)
(* ************************************************************************ *)

module Buffer =
struct
  type t = Format_string.t * int ref

  let create (s : Format_string.t) : t = (s,ref 0)

  let consume (_s,i : t) : unit = incr i

  let back (_s,i : t) : unit = decr i

  let get (s,i : t) : char =
    try let c = Format_string.get_char s !i in incr i; c
    with Format_string.OutOfBounds -> '\000'
      |  Format_string.NotAscii _ -> '\026'

  let last (s,i : t) : char =
    try Format_string.get_char s (!i - 1)
    with Format_string.OutOfBounds -> '\000'
      |  Format_string.NotAscii _ -> '\026'

  let peek (s,i : t) : char =
    try Format_string.get_char s !i
    with Format_string.OutOfBounds -> '\000'
      |  Format_string.NotAscii _ -> '\026'

  let getall (f : char -> bool) (s,i as b : t) : string =
    let start = !i in
    let len = ref 0 in
    begin try
      while f (get b) do
        incr len;
      done;
      back b; (* last char has not been matched *)
    with _ -> ()
    end;
    Format_string.sub_string s start !len
end


(* ************************************************************************ *)
(* Parsing                                                                  *)
(* ************************************************************************ *)

let is_uppercase = function
  | 'A'..'Z' -> true
  | _ -> false

let rec parse_negative b =
  match Buffer.peek b with
  | '-' -> Buffer.consume b; not (parse_negative b)
  | _ -> false

let parse_int b =
  let neg = parse_negative b in
  let s = Buffer.getall (function '0'..'9' -> true | _ -> false) b in
  let i =
    try int_of_string s
    with Failure _ -> warn "Invalid integer in format."; raise Invalid_format
  in
  if neg then -i else i

let parse_assignement_suppression b =
  match Buffer.peek b with
  | '*' -> Buffer.consume b; true
  | _ -> false

let rec parse_flags b =
  match Buffer.get b with
  | '-' -> FMinus :: parse_flags b
  | '+' -> FPlus :: parse_flags b 
  | ' ' -> FSpace :: parse_flags b 
  | '#' -> FSharp :: parse_flags b 
  | '0' -> FZero :: parse_flags b 
  | _ -> Buffer.back b; []

let parse_f_fw b =
  match Buffer.peek b with
  | '*' -> Buffer.consume b; Some `FWStar
  | '0'..'9' -> Some (`FWInt (parse_int b))
  | _ -> None

let parse_s_fw b =
  match Buffer.peek b with
  | '0'..'9' ->  Some (`FWInt (parse_int b))
  | _ -> None

let parse_precision b =
  match Buffer.peek b with
  | '.' ->  Buffer.consume b; Some
    begin match Buffer.peek b with
    | '*' -> Buffer.consume b; PStar
    | '-' | '0'..'9'-> PInt (parse_int b)
    | _ -> PInt 0
    end
  | _ -> None

let parse_lm b =
  match Buffer.get b, Buffer.peek b with
  | 'h', 'h' -> Buffer.consume b; Some `hh
  | 'h', _   -> Some `h
  | 'l', 'l' -> Buffer.consume b; Some `ll
  | 'l', _   -> Some `l
  | 'j', _   -> Some `j
  | 'z', _   -> Some `z
  | 't', _   -> Some `t
  | 'L', _   -> Some `L
  | _        -> Buffer.back b; None

let parse_brackets_interior b =
  let first = ref true and circ = ref false in
  let matching = function 
  | ']' when not !first -> false
  | '^' when !first && not !circ -> circ := true; true
  | '\000' -> warn "Unterminated brackets."; raise Invalid_format
  | _ -> first := false; true
  in
  let s = Buffer.getall matching b in
  Buffer.consume b;
  s

let parse_f_cs b =
  match Buffer.get b with
  | 'd' -> `d
  | 'i' -> `i
  | 'o' -> `o
  | 'u' -> `u
  | 'c' -> `c
  | 's' -> `s
  | 'p' -> `p
  | 'n' -> `n
  | 'x' | 'X' -> `x
  | 'f' | 'F' -> `f
  | 'e' | 'E' -> `e
  | 'g' | 'G' -> `g
  | 'a' | 'A' -> `a
  | '\000' ->
     warn "Missing conversion specifier at the end of format.";
     raise Invalid_format
  | '\026' ->
     warn "Conversion specifiers must be ascii characters.";
     raise Invalid_format
  | c ->
     warn "Unknown conversion specifier %c." c;
     raise Invalid_format

let parse_s_cs b =
  match Buffer.peek b with
  | '[' -> Buffer.consume b; `Brackets (parse_brackets_interior b)
  | _ -> parse_f_cs b

let parse_f_spec b =
  let f_flags = parse_flags b in
  let f_field_width = parse_f_fw b in
  let f_precision = parse_precision b in
  let f_length_modifier = parse_lm b in
  let f_conversion_specifier = parse_f_cs b in
  let f_capitalize = is_uppercase (Buffer.last b) in
  check_f_specification {
    f_flags; f_field_width; f_precision; f_length_modifier;
    f_conversion_specifier; f_capitalize
  }

let parse_s_spec b =
  let s_assignment_suppression = parse_assignement_suppression b in
  let s_field_width = parse_s_fw b in
  let s_length_modifier = parse_lm b in
  let s_conversion_specifier = parse_s_cs b in
  check_s_specification {
    s_assignment_suppression; s_field_width; s_length_modifier;
    s_conversion_specifier
  }

let rec parse_aux f b =
  match Buffer.get b, Buffer.peek b with
  | '%', '%' -> Buffer.consume b; (Char '%') :: parse_aux f b
  | '%', _ -> let spec = f b in Specification spec :: parse_aux f b
  | '\000', _ -> []
  | c, _ -> Char c :: parse_aux f b

let parse_f_format s = parse_aux parse_f_spec (Buffer.create s)

let parse_s_format s = parse_aux parse_s_spec (Buffer.create s)

let parse_format typ s = match typ with
  | PrintfLike -> FFormat (parse_f_format s)
  | ScanfLike -> SFormat (parse_s_format s)


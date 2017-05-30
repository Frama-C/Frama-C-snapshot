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

let string_of_flag = function
  | FMinus -> "-"
  | FPlus -> "+"
  | FSpace -> "' '"
  | FSharp -> "#"
  | FZero -> "0"

let string_of_flags fl =
  let rec aux accu fl = match fl with
    | f::fl -> aux (accu ^ string_of_flag f) fl
    | [] -> accu in
  aux "" fl

let pp_flag ff f = Format.fprintf ff "%s" (string_of_flag f)

let pp_flags ff fl = Pretty_utils.pp_list ~sep:", " pp_flag ff fl

let string_of_fw = function
  | `FWStar -> "*"
  | `FWInt i -> string_of_int i

let pp_fw ff fw = Format.fprintf ff "%s" (string_of_fw fw)

let string_of_precision = function
  | PStar -> "*"
  | PInt i -> string_of_int i

let pp_precision ff p = Format.fprintf ff ".%s" (string_of_precision p)

let string_of_lm = function
  | `hh -> "hh"
  | `h -> "h"
  | `l -> "l"
  | `ll -> "ll"
  | `j -> "j"
  | `z -> "z"
  | `t -> "t"
  | `L -> "L"

let pp_lm ff lm = Format.fprintf ff "%s" (string_of_lm lm)

let string_of_cs = function
  | `d -> "d"
  | `i -> "i"
  | `o -> "o"
  | `u -> "u"
  | `x -> "x"
  | `f -> "f"
  | `e -> "e"
  | `g -> "g"
  | `a -> "a"
  | `c -> "c"
  | `s -> "s"
  | `p -> "p"
  | `n -> "n"
  | `Brackets b -> "[" ^ b ^ "]"

let pp_cs ff (cs,capitalize) =
  let s = string_of_cs cs in
  let s = if capitalize then Transitioning.String.capitalize_ascii s else s in
  Format.fprintf ff "%s" s

let string_of_option ?pre:(pre="") ?suf:(suf="") f = function
  | Some o ->  pre ^ (f o) ^ suf
  | None -> ""

let pp_f_specification ff spec =
  let suf = "; " in
  Format.fprintf ff "<";

  if List.length spec.f_flags <> 0 then
    (Format.fprintf ff "Flags: "; pp_flags ff spec.f_flags;
     Format.fprintf ff "%s" suf);

  Format.fprintf ff "%s%s%s"
    (string_of_option ~pre:"Field width: " ~suf:suf
       string_of_fw spec.f_field_width)
    (string_of_option ~pre:"Precision: " ~suf:suf
       string_of_precision spec.f_precision)
    (string_of_option ~pre:"Length modifier: " ~suf:suf
       string_of_lm spec.f_length_modifier);

  Format.fprintf ff "Conversion specifier: %s>"
    (string_of_cs spec.f_conversion_specifier)

let pp_s_specification ff (spec: s_conversion_specification) =
  let suf = "; " in
  Format.fprintf ff "<%s; %s%s"
    ("Assignment: " ^ (string_of_bool (not spec.s_assignment_suppression)))
    (string_of_option ~pre:"Field width: " ~suf:suf
       string_of_fw spec.s_field_width)
    (string_of_option ~pre:"Length modifier: " ~suf:suf
       string_of_lm spec.s_length_modifier);
  Format.fprintf ff "Conversion specifier: %s>"
    (string_of_cs spec.s_conversion_specifier)

let pp_f_format ff fl =
  let fl = Extends.List.filter_map
    (function | Specification s -> Some s | _ -> None) fl in
  Pretty_utils.pp_list ~sep:"@." (fun ff s -> pp_f_specification ff s) ff fl

let pp_s_format ff (fl: s_format) =
  let fl = Extends.List.filter_map
    (function | Specification s -> Some s | _ -> None) fl in
  Pretty_utils.pp_list ~sep:"@." (fun ff s -> pp_s_specification ff s) ff fl

let pp_format ff = function
  | FFormat s -> pp_f_format ff s
  | SFormat s -> pp_s_format ff s

let rec f_format_to_cstring fl =
  let aux spec =
    "%"
    ^ (string_of_flags spec.f_flags)
    ^ (string_of_option string_of_fw spec.f_field_width)
    ^ (string_of_option ~pre: "." string_of_precision spec.f_precision)
    ^ (string_of_option string_of_lm spec.f_length_modifier)
    ^ (string_of_cs spec.f_conversion_specifier) in
  match fl with
  | [] -> ""
  | Char '%' :: fl -> "%%" ^ f_format_to_cstring fl
  | Char c :: fl -> (String.make 1 c) ^ f_format_to_cstring fl
  | Specification s :: fl -> (aux s) ^ f_format_to_cstring fl

let rec s_format_to_cstring fl =
  let aux spec =
    "%"
    ^ (if spec.s_assignment_suppression then "*" else "")
    ^ (string_of_option string_of_fw spec.s_field_width)
    ^ (string_of_option string_of_lm spec.s_length_modifier)
    ^ (string_of_cs spec.s_conversion_specifier) in
  match fl with
  | [] -> ""
  | Char '%' :: fl -> "%%" ^ s_format_to_cstring fl
  | Char c :: fl -> (String.make 1 c) ^ s_format_to_cstring fl
  | Specification s :: fl -> (aux s) ^ s_format_to_cstring fl

let format_to_cstring = function
  | FFormat s -> f_format_to_cstring s
  | SFormat s -> s_format_to_cstring s

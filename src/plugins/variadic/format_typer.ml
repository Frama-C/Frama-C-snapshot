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
open Cil_types

exception Type_not_found of string
exception Invalid_specifier

type arg_dir = [ `ArgIn
               | `ArgInArray of precision option (* for '%.*s' or '%.42s' *)
               | `ArgOut
               | `ArgOutArray ]

type typdef_finder = Logic_typing.type_namespace -> string -> Cil_types.typ


let get_typedef ?(find_typedef = Globals.Types.find_type) s =
  try 
    find_typedef Logic_typing.Typedef s
  with Not_found ->
    raise (Type_not_found s)

let ptr typ = TPtr (typ, [])


let type_f_specifier ?find_typedef spec =
  match spec.f_conversion_specifier, spec.f_length_modifier with
  | #signed_specifier, None     -> Cil.intType
  | #signed_specifier, Some `hh -> Cil.scharType
  | #signed_specifier, Some `h  -> Extends.Cil.shortType
  | #signed_specifier, Some `l  -> Cil.longType
  | #signed_specifier, Some `ll -> Cil.longLongType
  | #signed_specifier, Some `j  -> get_typedef ?find_typedef "intmax_t"
  | #signed_specifier, Some `z  -> get_typedef ?find_typedef "size_t"
  | #signed_specifier, Some `t  -> get_typedef ?find_typedef "ptrdiff_t"
  | #unsigned_specifier, None     -> Cil.uintType
  | #unsigned_specifier, Some `hh -> Cil.ucharType
  | #unsigned_specifier, Some `h  -> Extends.Cil.ushortType
  | #unsigned_specifier, Some `l  -> Cil.ulongType
  | #unsigned_specifier, Some `ll -> Cil.ulongLongType
  | #unsigned_specifier, Some `j  -> get_typedef ?find_typedef "uintmax_t"
  | #unsigned_specifier, Some `z  -> get_typedef ?find_typedef "size_t"
  | #unsigned_specifier, Some `t  -> get_typedef ?find_typedef "ptrdiff_t"
  | #float_specifier, None    -> Cil.doubleType
  | #float_specifier, Some `l -> Cil.doubleType
  | #float_specifier, Some `L -> Cil.longDoubleType
  | `c, None    -> Cil.intType
  | `c, Some `l -> get_typedef ?find_typedef "intmax_t"
  | `s, None    -> Cil.charPtrType
  | `s, Some `l -> ptr (get_typedef ?find_typedef "wchar_t")
  | `p, None    -> Cil.voidPtrType
  | `n, None     -> ptr Cil.intType
  | `n, Some `hh -> ptr Cil.scharType
  | `n, Some `h  -> ptr Extends.Cil.shortType
  | `n, Some `l  -> ptr Cil.longType
  | `n, Some `ll -> ptr Cil.longLongType
  | `n, Some `j  -> ptr (get_typedef ?find_typedef "intmax_t")
  | `n, Some `z  -> ptr (get_typedef ?find_typedef "size_t")
  | `n, Some `t  -> ptr (get_typedef ?find_typedef "ptrdiff_t")
  | _, _ -> raise Invalid_specifier

let type_s_specifier ?find_typedef spec =
  match spec.s_conversion_specifier, spec.s_length_modifier with
  | #signed_specifier, None     -> ptr Cil.intType
  | #signed_specifier, Some `hh -> ptr Cil.scharType
  | #signed_specifier, Some `h  -> ptr Extends.Cil.shortType
  | #signed_specifier, Some `l  -> ptr Cil.longType
  | #signed_specifier, Some `ll -> ptr Cil.longLongType
  | #signed_specifier, Some `j  -> ptr (get_typedef ?find_typedef "intmax_t")
  | #signed_specifier, Some `z  -> ptr (get_typedef ?find_typedef "size_t")
  | #signed_specifier, Some `t  -> ptr (get_typedef ?find_typedef "ptrdiff_t")
  | #unsigned_specifier, None     -> ptr Cil.uintType
  | #unsigned_specifier, Some `hh -> ptr Cil.ucharType
  | #unsigned_specifier, Some `h  -> ptr Extends.Cil.ushortType
  | #unsigned_specifier, Some `l  -> ptr Cil.ulongType
  | #unsigned_specifier, Some `ll -> ptr Cil.ulongLongType
  | #unsigned_specifier, Some `j  -> ptr (get_typedef ?find_typedef "uintmax_t")
  | #unsigned_specifier, Some `z  -> ptr (get_typedef ?find_typedef "size_t")
  | #unsigned_specifier, Some `t  -> ptr (get_typedef ?find_typedef "ptrdiff_t")
  | #float_specifier, None    -> ptr (Cil.floatType)
  | #float_specifier, Some `l -> ptr (Cil.doubleType)
  | #float_specifier, Some `L -> ptr (Cil.longDoubleType)
  | (`c | `s | `Brackets _), None    -> Cil.charPtrType
  | (`c | `s | `Brackets _), Some `l -> ptr (get_typedef ?find_typedef "wchar_t")
  | `p, None    -> ptr (Cil.voidPtrType)
  | `n, None     -> ptr Cil.intType
  | `n, Some `hh -> ptr Cil.scharType
  | `n, Some `h  -> ptr Extends.Cil.shortType
  | `n, Some `l  -> ptr Cil.longType
  | `n, Some `ll -> ptr Cil.longLongType
  | `n, Some `j  -> ptr (get_typedef ?find_typedef "intmax_t")
  | `n, Some `z  -> ptr (get_typedef ?find_typedef "size_t")
  | `n, Some `t  -> ptr (get_typedef ?find_typedef "ptrdiff_t")
  | _, _ -> raise Invalid_specifier


let type_f_format ?find_typedef format =
  let r = ref [] in
  let add_types spec =
    match spec with
    | Char _ -> ()
    | Specification s -> 
        if s.f_field_width = Some `FWStar then
          r := (Cil.intType, `ArgIn) :: !r;
        if s.f_precision = Some PStar then
          r := (Cil.intType, `ArgIn) :: !r;
        let dir = match s.f_conversion_specifier with
          | `s -> `ArgInArray s.f_precision
          | `n -> `ArgOut
          | _ ->  `ArgIn
         in
        r := (type_f_specifier ?find_typedef s, dir) :: !r;
  in
  List.iter add_types format;
  List.rev !r

let type_s_format ?find_typedef format =
  let r = ref [] in
  let add_types spec =
    match spec with
    | Char _ -> ()
    | Specification s -> 
        let dir = match s.s_conversion_specifier with
          | `s -> `ArgOutArray
          | _ -> `ArgOut
        in
        if not s.s_assignment_suppression then
          r := (type_s_specifier ?find_typedef s, dir) :: !r;
  in
  List.iter add_types format;
  List.rev !r


let type_format ?find_typedef = function
  | FFormat f -> type_f_format ?find_typedef f
  | SFormat s -> type_s_format ?find_typedef s


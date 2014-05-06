(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

module Dictionary =
  State_builder.Hashtbl
    (Obfuscator_kind.Hashtbl)
    (Datatype.String.Hashtbl.Make(Datatype.String))
    (struct
      let name = "Obfuscator.Dictionary"
      let size = 97
      let dependencies = [ Ast.self ]
     end)

module Literal_strings =
  State_builder.Hashtbl
    (Datatype.String.Hashtbl)
    (Datatype.String)
    (struct
      let name = "Obfuscator.Literal_strings"
      let size = 17
      let dependencies = [ Dictionary.self ]
     end)

let fresh kind name =
  let h = Dictionary.memo (fun _ -> Datatype.String.Hashtbl.create 17) kind in
  let idx = Datatype.String.Hashtbl.length h + 1 in
  let fresh = Obfuscator_kind.prefix kind ^ string_of_int idx in
  Datatype.String.Hashtbl.add h fresh name;
  if kind = Obfuscator_kind.Literal_string && not (Literal_strings.mem name) 
  then Literal_strings.add name fresh;
  fresh

let id_of_literal_string = Literal_strings.find

let iter_sorted_kind f k h =
  if Datatype.String.Hashtbl.length h > 0 then
    let f = f k in
    Datatype.String.Hashtbl.iter_sorted f h

let iter_sorted f = 
  let cmp k1 k2 = 
    Datatype.String.compare
      (Obfuscator_kind.prefix k1)
      (Obfuscator_kind.prefix k2)
  in
  Dictionary.iter_sorted ~cmp (iter_sorted_kind f)

let pretty_entry fmt k =
  Format.fprintf fmt "// %as@\n" Obfuscator_kind.pretty k;
  let quote = k = Obfuscator_kind.Literal_string in
  fun new_ old -> 
    if quote then Format.fprintf fmt "#define %s %S@\n" new_ old
    else Format.fprintf fmt "#define %s %s@\n" new_ old

let pretty_kind fmt k =
  try
    let h = Dictionary.find k in
    iter_sorted_kind (pretty_entry fmt) k h
  with Not_found -> 
    ()

let pretty fmt =
    Format.fprintf fmt "\
/* *********************************** */@\n\
/* start of dictionary for obfuscation */@\n\
/* *********************************** */@\n";
  iter_sorted
    (fun k -> 
      if k = Obfuscator_kind.Literal_string then fun _ _ -> ()
      else pretty_entry fmt k);
  Format.fprintf fmt "\
/*********************************** */@\n\
/* end of dictionary for obfuscation */@\n\
/*********************************** */@\n@\n"

let mark_as_computed () = Dictionary.mark_as_computed ()
let is_computed () = Dictionary.is_computed ()

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

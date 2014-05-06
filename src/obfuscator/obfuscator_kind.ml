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

type k =
  | Behavior
  | Enum
  | Field
  | Formal_var
  | Function
  | Global_var
  | Label
  | Literal_string
  | Local_var
  | Logic_var
  | Predicate
  | Type
      
let name_of_kind = function
  | Behavior -> "behavior"
  | Enum -> "enum"
  | Field -> "field"
  | Formal_var -> "formal variable"
  | Function -> "function"
  | Global_var -> "global variable"
  | Label -> "label"
  | Literal_string -> "literal string"
  | Local_var -> "local variable"
  | Logic_var -> "logic variable"
  | Predicate -> "predicate"
  | Type -> "type"

let prefix = function
  | Behavior -> "B"
  | Enum -> "E"
  | Field -> "M"
  | Formal_var -> "f"
  | Function -> "F"
  | Global_var -> "G"
  | Label -> "L"
  | Literal_string -> "LS"
  | Local_var -> "V"
  | Logic_var -> "LV"
  | Predicate -> "P"
  | Type -> "T"

include Datatype.Make_with_collections
(struct
  type t = k
  let name = "Obfuscator.kind"
  let reprs = [ Global_var ]
  let hash (k:k) = Hashtbl.hash k
  let equal (k1:k) k2 = k1 = k2
  let compare (k1:k) k2 = Pervasives.compare k1 k2
  let varname _ = "k"
  let internal_pretty_code = Datatype.undefined
  let copy = Datatype.identity
  let structural_descr = Structural_descr.t_abstract
  let rehash = Datatype.identity
  let mem_project = Datatype.never_any_project
  let pretty fmt k = Format.fprintf fmt "%s" (name_of_kind k)
 end)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(* ------------------------------------------------------------------------ *)
(* ---  Exception Handling in WP                                        --- *)
(* ------------------------------------------------------------------------ *)

exception Error of string * string

let current = ref "wp"
let set_model m = current := m

let unsupported ?(model= !current) fmt =
  let b = Buffer.create 80 in
  Buffer.add_string b "unsupported " ;
  let kf fmt =
    Format.pp_print_flush fmt () ;
    raise (Error(model,Buffer.contents b))
  in Format.kfprintf kf (Format.formatter_of_buffer b) fmt

let not_yet_implemented ?(model= !current) fmt =
  let b = Buffer.create 80 in
  let kf fmt =
    Format.pp_print_string fmt " not yet implemented" ;
    Format.pp_print_flush fmt () ;
    raise (Error(model,Buffer.contents b))
  in Format.kfprintf kf (Format.formatter_of_buffer b) fmt

open Cil_types

let pp_logic_label fmt label =
  match label with
  | BuiltinLabel l -> Printer.pp_logic_builtin_label fmt l
  | FormalLabel s -> Format.pp_print_string fmt s
  | StmtLabel {contents=stmt} ->
      Format.pp_print_string fmt
        (let rec pickLabel = function
            | [] -> Printf.sprintf "__unknown_label_%d" stmt.sid
            | Label (l, _, _) :: _ -> l
            | _ :: rest -> pickLabel rest
         in pickLabel stmt.labels)

let pp_assigns fmt asgns =
  match asgns with
  | WritesAny -> Format.fprintf fmt "<undef>"
  | _ -> Format.fprintf fmt "@[<hov 2>%a@]" (Printer.pp_full_assigns "") asgns

let pp_string_list ?(sep=format_of_string "@ ") ~empty fmt l =
  match l with [] ->  Format.fprintf fmt "%s" empty
             | _ -> Format.fprintf fmt "%a"
                      (Pretty_utils.pp_list ~sep Format.pp_print_string) l

let name = function
  | [] -> ""
  | [x] -> x
  | x::xs ->
      let buffer = Buffer.create 80 in
      Buffer.add_string buffer x ;
      List.iter
        (fun y -> if y <> "" then
            ( Buffer.add_char buffer '-' ;
              Buffer.add_string buffer y )) xs ;
      Buffer.contents buffer

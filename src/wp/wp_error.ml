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
  let name = match label with
    | LogicLabel (_,l) -> l
    | StmtLabel {contents=stmt} ->
        let rec pickLabel = function
          | [] -> Printf.sprintf "__unknown_label_%d" stmt.sid
          | Label (l, _, _) :: _ -> l
          | _ :: rest -> pickLabel rest
        in pickLabel stmt.labels
  in Format.pp_print_string fmt name

let pp_assigns fmt asgns =
  match asgns with
  | WritesAny -> Format.fprintf fmt "<undef>"
  | _ -> Format.fprintf fmt "@[<hov 2>%a@]" (Printer.pp_full_assigns "") asgns

let pp_string_list ?(sep=format_of_string "@ ") ~empty fmt l =
  match l with [] ->  Format.fprintf fmt "%s" empty
  | _ -> Format.fprintf fmt "%a"
           (Pretty_utils.pp_list ~sep Format.pp_print_string) l


type 'a cc =
  | Result of 'a
  | Warning of string * string (* model , message *)

let protected = function
  | Error (model, msg) ->
      Some(model , msg)
  | Log.FeatureRequest (plugin,msg) ->
      Some(plugin , Printf.sprintf "%s not yet implemented" msg)
  | Log.AbortError msg ->
      Some("user error" , msg)
  | _ -> None

let protect exn =
  match protected exn with
    | Some(plugin,reason) -> plugin , reason
    | None -> raise exn

let protect_warning exn =
  match protected exn with
    | Some(src,reason) -> Warning(src,reason)
    | None -> raise exn

let protect_function f x =
  try Result (f x)
  with e -> protect_warning e

let protect_translation f x y =
  try Result (f x y)
  with e -> protect_warning e

let protect_translation3 f x y z =
  try Result (f x y z)
  with e -> protect_warning e

let protect_translation4 f x y z t =
  try Result (f x y z t)
  with e -> protect_warning e

let protect_translation5 f x y z t u =
  try Result (f x y z t u)
  with e -> protect_warning e

let rec protect_map f = function
  | [] -> Result []
  | x::xs ->
      match f x with
        | Result y ->
            ( match protect_map f xs with
                | Result ys -> Result (y :: ys)
                | Warning _ as w -> w )
        | Warning(m,p) -> Warning(m,p)

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

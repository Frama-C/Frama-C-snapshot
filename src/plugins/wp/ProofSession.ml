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

open Wpo

let files = Hashtbl.create 32

let filename wpo =
  let m = Model.get_id wpo.po_model in
  let d = Wp_parameters.get_session_dir m in
  Printf.sprintf "%s/%s.json" d wpo.po_gid

let pretty fmt wpo = Format.pp_print_string fmt (filename wpo)

let exists wpo =
  let f = filename wpo in
  try Hashtbl.find files f
  with Not_found ->
    let e = Sys.file_exists f in
    Hashtbl.add files f e ; e

let load wpo =
  let f = filename wpo in
  if Sys.file_exists f then Json.load_file f else Json.Null

let save wpo js =
  let f = filename wpo in
  let empty =
    match js with
    | Json.Null | Json.Array [] | Json.Assoc [] -> true
    | _ -> false
  in
  ( if empty
    then Extlib.safe_remove f
    else Json.save_file f js ) ;
  Hashtbl.replace files f (not empty)

let remove wpo =
  let f = filename wpo in
  Extlib.safe_remove f ;
  Hashtbl.replace files f false

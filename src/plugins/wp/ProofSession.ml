(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

type status =
  | NoScript
  | Script of string
  | Deprecated of string

let files : (string,status) Hashtbl.t = Hashtbl.create 32

let filename ?(legacy=false) wpo =
  let m = Model.get_id wpo.po_model in
  let d = Wp_parameters.get_session_dir m in
  Printf.sprintf "%s/%s.json" d (if legacy then wpo.po_leg else wpo.po_gid)

let pretty fmt wpo = Format.pp_print_string fmt (filename wpo)

let status wpo =
  let f = filename wpo in
  try Hashtbl.find files f
  with Not_found ->
    let status =
      if Sys.file_exists f then Script f else
        let f' = filename ~legacy:true wpo in
        if Sys.file_exists f' then
          ( Wp_parameters.warning ~current:false
              "Deprecated script for '%s'" wpo.po_sid ;
            Deprecated f' )
        else NoScript in
    Hashtbl.add files f status ; status

let exists wpo =
  match status wpo with NoScript -> false | Script _ | Deprecated _ -> true

let load wpo =
  match status wpo with
  | NoScript -> `Null
  | Script f | Deprecated f ->
      if Sys.file_exists f then Json.load_file f else `Null

let remove wpo =
  match status wpo with
  | NoScript -> ()
  | Script f ->
      begin
        Extlib.safe_remove f ;
        Hashtbl.replace files f NoScript ;
      end
  | Deprecated f0 ->
      begin
        Wp_parameters.feedback
          "Removed deprecated script for '%s'" wpo.po_sid ;
        Extlib.safe_remove f0 ;
        Hashtbl.replace files (filename wpo) NoScript ;
      end

let save wpo js =
  let empty =
    match js with
    | `Null | `List [] | `Assoc [] -> true
    | _ -> false in
  if empty then remove wpo else
    match status wpo with
    | Script f -> Json.save_file f js
    | NoScript ->
        begin
          let f = filename wpo in
          Json.save_file f js ;
          Hashtbl.replace files f (Script f) ;
        end
    | Deprecated f0 ->
        begin
          Wp_parameters.feedback
            "Upgraded script for '%s'" wpo.po_sid ;
          Extlib.safe_remove f0 ;
          let f = filename wpo in
          Json.save_file f js ;
          Hashtbl.replace files f (Script f) ;
        end

(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

(* Only Compiled when package Zmq is installed *)
(* No interface, registered via side-effects   *)

(* -------------------------------------------------------------------------- *)
(* --- ZeroMQ Server Options                                              --- *)
(* -------------------------------------------------------------------------- *)

module Senv = Server_parameters

let batch_group = Senv.add_group "Protocol BATCH"

let () = Parameter_customize.set_group batch_group
module Batch = Senv.String_list
    (struct
      let option_name = "-server-batch"
      let arg_name = "file.json,..."
      let help =
        "Executes all requests in each <file.json>, and save the \
         associated results in <file.out.json>."
    end)

let () = Parameter_customize.set_group batch_group
let () = Parameter_customize.do_not_save ()
module BatchOutputDir = Senv.Empty_string
    (struct
      let option_name = "-server-batch-output-dir"
      let arg_name = "path"
      let help =
        "Outputs the results of -server-batch in <path> instead of the input \
         directory."
    end)

let _ = Doc.page `Protocol ~title:"Batch Protocol" ~filename:"server_batch.md"


(* -------------------------------------------------------------------------- *)
(* --- Execute JSON                                                       --- *)
(* -------------------------------------------------------------------------- *)

module Js = Yojson.Basic
module Ju = Yojson.Basic.Util

let pretty = Js.pretty_print ~std:false

let execute_command js =
  let request = Ju.member "request" js |> Ju.to_string in
  let id = Ju.member "id" js in
  let data = Ju.member "data" js in
  match Main.find request with
  | None ->
    Senv.error "[batch] %a: request %S not found" pretty id request ;
    `Assoc [ "id" , id ; "error" , `String "request not found" ]
  | Some (kind,handler) ->
    try
      Senv.feedback "[%a] %s" Main.pp_kind kind request ;
      `Assoc [ "id" , id ; "data" , handler data ]
    with Data.InputError(msg) ->
      Senv.error "[%s] %s@." request msg ;
      `Assoc [ "id" , id ; "error" , `String msg ; "at" , js ]

let rec execute_batch js =
  match js with
  | `Null -> `Null
  | `List js -> `List (List.map execute_batch js)
  | js ->
    try execute_command js
    with Ju.Type_error(msg,js) ->
      Senv.error "[batch] incorrect encoding:@\n%s@\n@[<hov 2>At: %a@]@."
        msg pretty js ;
      `Null

(* -------------------------------------------------------------------------- *)
(* --- Execute the Scripts                                                --- *)
(* -------------------------------------------------------------------------- *)

let execute () =
  begin
    let files = Batch.get () in
    Batch.clear () ; (* clear in any case *)
    List.iter
      begin fun file ->
        Senv.feedback "Script %S" file ;
        let response = execute_batch (Js.from_file file) in
        let output = Filename.remove_extension file ^ ".out.json" in
        let output = match BatchOutputDir.get () with
          | "" -> output
          | dir -> Filename.(dir ^ dir_sep ^ basename output)
        in
        Senv.feedback "Output %S" output ;
        let out = open_out output in
        Js.pretty_to_channel out response ;
        close_out out
      end files
  end

(* -------------------------------------------------------------------------- *)
(* --- Run the Server from the Command line                               --- *)
(* -------------------------------------------------------------------------- *)

let () = Db.Main.extend execute

(* -------------------------------------------------------------------------- *)

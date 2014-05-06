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

let binary = ref false

let open_out file = if !binary then open_out_bin file else open_out file
let open_in file = if !binary then open_in_bin file else open_in file

(* -------------------------------------------------------------------------- *)
(* --- Monadic                                                            --- *)
(* -------------------------------------------------------------------------- *)

let try_finally ~finally f x =
  try let r = f x in finally x ; r with e -> finally x ; raise e

let on_inc file job = try_finally ~finally:close_in job (open_in file)
let on_out file job = try_finally ~finally:close_out job (open_out file)

(* -------------------------------------------------------------------------- *)
(* --- Sys-like Commands                                                  --- *)
(* -------------------------------------------------------------------------- *)

let hardcopy inc out =
  begin
    let buffer = String.create 1024 in
    let n = ref 0 in
    while (n := Pervasives.input inc buffer 0 1024 ; !n > 0) do
      Pervasives.output out buffer 0 !n 
    done ;
    flush out ; 
  end

let copy src tgt = on_inc src (fun inc -> on_out tgt (hardcopy inc))

let is_dir path = Sys.file_exists path && Sys.is_directory path

let rec mkdir path =
  if not (Sys.file_exists path) then
    begin
      mkdir (Filename.dirname path) ;
      Unix.mkdir path 0o755 ;
    end
  else if not (Sys.is_directory path) then
    failwith (Printf.sprintf "Directory %S is not a directory" path)

let parse_mode (m : string) : Unix.file_perm =
  try int_of_string m
  with _ ->
    failwith (Printf.sprintf "Invalid file permissions %S" m)

(* -------------------------------------------------------------------------- *)
(* --- Install Utility                                                    --- *)
(* -------------------------------------------------------------------------- *)

let verbose = ref false
let warning = ref true
let path = ref false
let mode = ref "default"
let input = ref "."
let output = ref "#undefined#"
let count = ref 0
let summary = ref false

let do_mkdir tgt =
  if not (Sys.file_exists tgt) then 
    begin
      if not !path then 
        failwith (Printf.sprintf "Directory %S not found." tgt) ;
      if !verbose then Format.printf "[install] mkdir %S@." tgt ;
      mkdir tgt ;
    end
  else
  if not (Sys.is_directory tgt) then
    failwith (Printf.sprintf "File %S is not a directory." tgt)

let do_copy src tgt =
  begin
    if !verbose then Format.printf "[install] cp %S %S@." src tgt ;
    copy src tgt ;
    incr count ;
  end

let do_chmod tgt =
  begin
    if !mode <> "default" then
      let perm = parse_mode !mode in
      if !verbose then Format.printf "[install] chmod %s %S@." !mode tgt ;
      Unix.chmod tgt perm ;
  end

let do_install file = 
  try
    let src = Printf.sprintf "%s/%s" !input file in
    if Sys.file_exists src then
      begin
        let tgt = Printf.sprintf "%s/%s" !output file in
        do_mkdir (Filename.dirname tgt) ;
        do_copy src tgt ;
        do_chmod tgt ;
      end
    else if !warning then
      Format.printf "[install] File %S not found@." src
  with
  | Failure msg | Sys_error msg -> 
      Format.printf "[install] %s@." msg ; 
      exit 1
  | Unix.Unix_error (e,_,_) ->
      let msg = Unix.error_message e in
      Format.printf "[install] Error: %s@." msg ; 
      exit 2
  | e ->
      let msg = Printexc.to_string e in
      Format.printf "[install] Error: %s@." msg ; 
      exit 2

let () = 
  Arg.parse [
    "-v" , Arg.Set verbose , "verbose mode" ;
    "-q" , Arg.Clear verbose , "quiet mode (default)" ;
    "-p" , Arg.Set path , "create output directories" ;
    "-f" , Arg.Clear warning , "ignore warnings" ;
    "-i" , Arg.Set_string input , "<dir> set input directory (defaults to '.')" ;
    "-d" , Arg.Set_string output , "<dir> set output directory (mandatory)" ;
    "-m" , Arg.Set_string mode , "<mode> change mode of copied files (use \"default\" to reset)" ;
    "-s" , Arg.Set summary , "print number of installed files." ;
    "-b", Arg.Set binary, "set binary mode for copying files.";
  ] do_install
    "Usage: install [options|files]\n\n\
    Copy all files from input directory to output directory.\n\
    Files must be given with their relative paths to input/output.\n\
    Options and files are processed in order (each option only\n\
    apply to subsequent files).\n"

let () = 
  if !summary then match !count with
    | 0 -> Format.printf "No file installed.@."
    | 1 -> Format.printf "One single file installed.@."
    | n -> Format.printf "%d files installed.@." n

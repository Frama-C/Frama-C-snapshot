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

open Sysutil

let symbolic_dirs = ref []

let add_symbolic_dir name dir =
  let regex = Str.regexp ("^" ^ Str.quote dir) in
  symbolic_dirs:=(regex,name)::!symbolic_dirs

let replace_symbolic filename (regex, name) =
  Str.replace_first regex name filename

let replace_symbols filename =
  List.fold_left replace_symbolic filename !symbolic_dirs

let base_dir = Sys.getcwd ()

(* Normalize a filename: make it relative if it is "close" to the
   current directory and results in a shorter path. *)
let pretty filename =
  (** if filename is relative things can be messy *)
  let absfilename = absolutize_filename base_dir filename in
  let newfilename = replace_symbols absfilename in
  (* if we have a symbolic replacement, don't go further *)
  if newfilename <> absfilename then newfilename
  else begin
    let newfilename = relativize_filename base_dir newfilename in
    let newfilename =
      match Extlib.string_del_prefix ~strict:true "./" newfilename with
        | Some f -> f
        | None -> newfilename in
    if String.length newfilename < String.length filename
    then newfilename
    else filename
  end
;;

let normalize filename = absolutize_filename base_dir filename

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

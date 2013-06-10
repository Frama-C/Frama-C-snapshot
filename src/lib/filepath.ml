(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

let base_dir = Sys.getcwd ()

(* Normalize a filename: make it relative if it is "close" to the
   current directory and results in a shorter path. *)
let normalize filename =
  (** if filename is relative things can be messy *)
  let absfilename = absolutize_filename base_dir filename in
  let newfilename = relativize_filename base_dir absfilename in
  let newfilename =
    match Extlib.string_del_prefix ~strict:true "./" newfilename with
    | Some f -> f
    | None -> newfilename in
  if String.length newfilename < String.length filename
  then newfilename
  else filename
;;

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

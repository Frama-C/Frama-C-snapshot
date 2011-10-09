(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

(* Implementation of [Dynlink_common_interface] compatible with
   ocamlc < 3.11 *)

module type OldDynlink = sig
  val loadfile : string -> unit
  val allow_unsafe_modules : bool -> unit
  val init : unit -> unit
  val digest_interface : string -> string list -> Digest.t
end

include (Dynlink : OldDynlink)

exception Unsupported_Feature of string

let is_native = false
let adapt_filename x = x

type linking_error =
  | Undefined_global of string
  | Unavailable_primitive of string
  | Uninitialized_global of string

type error =
    Not_a_bytecode_file of string
  | Inconsistent_import of string
  | Unavailable_unit of string
  | Unsafe_file
  | Linking_error of string * linking_error
  | Corrupted_interface of string
  | File_not_found of string
  | Cannot_open_dll of string
  | Inconsistent_implementation of string

exception Error of error

let to_dynlink_linking_error = function
  | Undefined_global s -> Dynlink.Undefined_global s
  | Unavailable_primitive s -> Dynlink.Unavailable_primitive s
  | Uninitialized_global s -> Dynlink.Uninitialized_global s

let to_dynlink_error = function
  | Not_a_bytecode_file s -> Dynlink.Not_a_bytecode_file s
  | Inconsistent_import s -> Dynlink.Inconsistent_import s
  | Unavailable_unit s -> Dynlink.Unavailable_unit s
  | Unsafe_file -> Dynlink.Unsafe_file
  | Linking_error(s, l) -> Dynlink.Linking_error(s, to_dynlink_linking_error l)
  | Corrupted_interface s -> Dynlink.Corrupted_interface s
  | File_not_found s -> Dynlink.File_not_found s
  | Cannot_open_dll s -> Dynlink.Cannot_open_dll s
  | Inconsistent_implementation _ -> assert false

let from_dynlink_linking_error = function
  | Dynlink.Undefined_global s -> Undefined_global s
  | Dynlink.Unavailable_primitive s -> Unavailable_primitive s
  | Dynlink.Uninitialized_global s -> Uninitialized_global s

let from_dynlink_error = function
  | Dynlink.Not_a_bytecode_file s -> Not_a_bytecode_file s
  | Dynlink.Inconsistent_import s -> Inconsistent_import s
  | Dynlink.Unavailable_unit s -> Unavailable_unit s
  | Dynlink.Unsafe_file -> Unsafe_file
  | Dynlink.Linking_error(s, l) ->
      Linking_error(s, from_dynlink_linking_error l)
  | Dynlink.Corrupted_interface s -> Corrupted_interface s
  | Dynlink.File_not_found s -> File_not_found s
  | Dynlink.Cannot_open_dll s -> Cannot_open_dll s

let stub_error f x =
  try f x with Dynlink.Error e -> raise (Error (from_dynlink_error e))

let init = stub_error init
let loadfile = stub_error loadfile

let error_message e = Dynlink.error_message (to_dynlink_error e)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)

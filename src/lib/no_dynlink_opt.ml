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

(* Implementation of [Dynlink_common_interface] when no dynlink
   is available *)

module type OldDynlink = sig
  val loadfile : string -> unit
  val allow_unsafe_modules : bool -> unit
  val init : unit -> unit
  val add_interfaces: string list -> string list -> unit
  val digest_interface : string -> string list -> Digest.t
end

exception Unsupported_Feature of string
let fail s = fun _ -> raise (Unsupported_Feature s)

let is_native = true
let adapt_filename = fail "adapt_filename"

let loadfile = fail "loadfile"
let allow_unsafe_modules = fail "allow_unsafe_modules"
let init = fail "init"
let add_interfaces = fail "add_interfaces"

type linking_error =
    Undefined_global of string
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

let error_message = fail "error_message"
let digest_interface = fail "digest_interface"

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)

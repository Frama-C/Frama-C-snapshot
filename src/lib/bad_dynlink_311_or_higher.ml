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

(* Should not be in this module, but must happen very early in the boot
   process *)
let () = Printexc.record_backtrace true

(* Implementation of [Dynlink_common_interface] compatible with OCaml >=3.11
   whenever [Dynlink] **does not** correctly work. *)

module type OldDynlink = sig
  val loadfile : string -> unit
  val allow_unsafe_modules : bool -> unit
  val init : unit -> unit
  val add_interfaces: string list -> string list -> unit
  val digest_interface : string -> string list -> Digest.t
end

exception Unsupported_Feature of string
let fail s = fun _ -> raise (Unsupported_Feature s)

let is_native = Dynlink.is_native

let adapt_filename =
  if is_native then fail "adapt_filename" else Dynlink.adapt_filename

let loadfile = if is_native then fail "loadfile" else Dynlink.loadfile

let loadfile_private =
  if is_native then fail "loadfile_private" else Dynlink.loadfile_private

let allow_unsafe_modules =
  if is_native then fail "allow_unsafe_modules"
  else Dynlink.allow_unsafe_modules

let init = if is_native then fail "init" else Dynlink.init

let clear_available_units =
  if is_native then fail "clear_available_units"
  else Dynlink.clear_available_units

let add_available_units =
  if is_native then fail "add_available_units" else Dynlink.add_available_units

let add_interfaces =
  if is_native then fail "add_interfaces" else Dynlink.add_interfaces

let default_available_units =
  if is_native then fail "default_available_units"
  else Dynlink.default_available_units

let prohibit = if is_native then fail "prohibit" else Dynlink.prohibit
let allow_only = if is_native then fail "allow_only" else Dynlink.allow_only

type linking_error = Dynlink.linking_error =
    Undefined_global of string
  | Unavailable_primitive of string
  | Uninitialized_global of string

type error = Dynlink.error =
    Not_a_bytecode_file of string
  | Inconsistent_import of string
  | Unavailable_unit of string
  | Unsafe_file
  | Linking_error of string * linking_error
  | Corrupted_interface of string
  | File_not_found of string
  | Cannot_open_dll of string
  | Inconsistent_implementation of string

exception Error = Dynlink.Error

let error_message =
  if is_native then fail "error_message" else Dynlink.error_message

let digest_interface =
  if is_native then fail "digest_interface" else Dynlink.digest_interface

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)

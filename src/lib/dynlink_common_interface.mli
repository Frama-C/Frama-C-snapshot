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

(** Wrapper for [Dynlink] compatible with all OCaml versions. *)

module type OldDynlink = sig

  (** {6 Dynamic loading of compiled files} *)

val loadfile : string -> unit
(** In bytecode: load the given bytecode object file ([.cmo] file) or
    bytecode library file ([.cma] file), and link it with the running
    program. In native code: load the given OCaml plugin file (usually
    [.cmxs]), and link it with the running
    program.
    All toplevel expressions in the loaded compilation units
    are evaluated. No facilities are provided to
    access value names defined by the unit. Therefore, the unit
    must register itself its entry points with the main program,
    e.g. by modifying tables of functions. *)

(** {6 Access control} *)

val allow_unsafe_modules : bool -> unit
(** Govern whether unsafe object files are allowed to be
    dynamically linked. A compilation unit is ``unsafe'' if it contains
    declarations of external functions, which can break type safety.
    By default, dynamic linking of unsafe object files is
    not allowed. In native code, this function does nothing; object files
    with external functions are always allowed to be dynamically linked. *)

(** {6 Deprecated, initialization} *)

val init : unit -> unit
(** @deprecated Initialize the [Dynlink] library. This function is called
    automatically when needed. *)

val add_interfaces: string list -> string list -> unit
(**/**)

(** {6 Internal functions} *)

val digest_interface : string -> string list -> Digest.t

end

include OldDynlink

exception Unsupported_Feature of string

(** Dynamic loading of object files. *)

val is_native: bool
(** [true] if the program is native,
    [false] if the program is bytecode. *)

(** {6 Error reporting} *)

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
(** Errors in dynamic linking are reported by raising the [Error]
    exception with a description of the error. *)

val error_message : error -> string
(** Convert an error description to a printable message. *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

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
   whenever [Dynlink] does correctly work. *)

module type OldDynlink = sig
  val loadfile : string -> unit
  val allow_unsafe_modules : bool -> unit
  val init : unit -> unit
  val add_interfaces: string list -> string list -> unit
  val digest_interface : string -> string list -> Digest.t
end

exception Unsupported_Feature of string
include Dynlink

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)

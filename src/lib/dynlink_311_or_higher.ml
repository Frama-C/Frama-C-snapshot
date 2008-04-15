(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(* $Id: dynlink_311_or_higher.ml,v 1.2 2008/08/28 09:22:49 uid528 Exp $ *)

module type OldDynlink = sig
  val loadfile : string -> unit
  val allow_unsafe_modules : bool -> unit
  val init : unit -> unit
  type linking_error = 
      Dynlink.linking_error 
      =
    | Undefined_global of string
    | Unavailable_primitive of string
    | Uninitialized_global of string

  val digest_interface : string -> string list -> Digest.t
end

exception Unsupported_Feature of string
include Dynlink

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)

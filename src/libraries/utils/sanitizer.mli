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

(** {2 Sanitizer}

    Keeps only alpha-numerical characters.
    Separator ['_'] is allowed, but leading, trailing and consecutive
    separators are removed.
*)

type buffer

val create : ?truncate:bool -> int -> buffer
val clear : buffer -> unit

val add_sep : buffer -> unit (** Adds ['_'] character *)
val add_char : buffer -> char -> unit
val add_string : buffer -> string -> unit
val add_list : buffer -> string list -> unit (** Separated with ['_'] *)

val contents : buffer -> string

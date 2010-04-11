(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

(** Namespace for projects. *)

(** Build a new namespace. *)
module Make(X:sig end) : sig

  type t (* = private string *)

  exception AlreadyExists of string
  val make: string -> t
    (** Make a new name.
	@raise AlreadyExists if it already exists. *)

  val extend: string -> string -> t
    (** Build a new name from 2 strings.
	No verification is performed for the existence of names. *)

  val extend2: string -> string -> string -> t
    (** Build a new name from 3 strings.
	No verification is performed for the existence of names. *)

  val extend3: string -> string -> string -> string -> t
    (** Build a new name from 4 strings.
	No verification is performed for the existence of names. *)

  val get: t -> string
    (** @return the name *)
    (* [JS 03 October 2008] remove this function when private abbreviation
       types will be allowed. *)

end

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)

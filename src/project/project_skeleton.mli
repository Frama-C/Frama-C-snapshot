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

(** This module should not be used outside of the Project library.
    @since Carbon-20101201 *)

(* ************************************************************************** *)
(** {2 Logging machinery} *)
(* ************************************************************************** *)

(** @since Carbon-20101201 *)
module Output : sig
  include Log.Messages
  val dkey: Log.category
(** @since Oxygen-20121001+dev *)
end

(* ************************************************************************** *)
(** {2 Type declaration} *)
(* ************************************************************************** *)

type t = private
    { pid: int; mutable name: string; mutable unique_name: string }
(** @since Carbon-20101201
    @plugin development guide *)

type project = t
(** @since Carbon-20101201 *)

(* ************************************************************************** *)
(** {2 Constructor} *)
(* ************************************************************************** *)

val dummy: t
(** @since Carbon-20101201 *)

(** @since Carbon-20101201 *)
module Make_setter(X: sig val mem: string -> bool end) : sig

  val make_unique_name: string -> string
  (** @return a fresh name from the given string according to [X.mem].
      @since Nitrogen-20111001 *)

  val make: string -> t
  (** @since Carbon-20101201 *)

  val set_name: t -> string -> unit
(** @since Carbon-20101201 *)

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

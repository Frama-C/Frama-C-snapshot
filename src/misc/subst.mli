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

(** Substitution of varinfos by exps.
    @deprecated Carbon-20101201 *)

open Cil_types

type t
  (** Type of the substitution. *)

val empty: t
  (** The empty substitution. *)

val add: varinfo -> exp -> t -> t
  (** Add a new couple to the substitution. *)

val remove: varinfo -> t -> t
  (** Do not substitute the varinfo anymore. *)

val expr: ?trans:bool -> exp -> t -> exp * bool
  (** Apply the substitution to an expression.
      If [trans], the substitution is transitively applied. Default is [true].
      For example, with subst = \{ x -> &y; y -> b \} and exp = x, the result
      is &b by default and &y if trans is false.
      The returned boolean flag is true is a substitution occured. *)

val lval: ?trans:bool -> lval -> t -> exp * bool
  (** Apply the substitution to a lvalue. *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

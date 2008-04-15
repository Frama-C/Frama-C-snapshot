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

(* $Id: extlib.mli,v 1.14 2008/11/18 12:13:41 uid568 Exp $ *)

(** Useful operations.

    This module does not depend of any of frama-c module.
    @plugin development guide *)

exception NotYetImplemented of string
  (** @plugin development guide *)

val not_yet_implemented: string -> 'a
  (** @raise NotYetImplemented with the given string. *)

val mk_fun: string -> ('a -> 'b) ref
  (** build a reference to an unitialized function (which raises
      [NotYetImplemented] if it is called).
      @plugin development guide *)

val deprecated: string -> ('a -> 'b) -> ('a -> 'b)
  (** Indicate that the use of the given function with the given name is
      deprecated.
      @return the given function itself 
      @since Lithium-20081002+beta1+dev *)

val nop: 'a -> unit
  (** Do nothing. *)

val find_or_none: ('a -> 'b) -> 'a -> 'b option

val adapt_filename: string -> string
  (** Ensure that the given filename has the extension "cmo" in bytecode
      and "cmxs" in native *)
(* ************************************************************************** *)
(** {2 Function combinators} *)
(* ************************************************************************** *)

val ($) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
  (** Composition. *)

val swap: ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
  (** Swap arguments. *)

(* ************************************************************************** *)
(** {2 Lists} *)
(* ************************************************************************** *)

val as_singleton: 'a list -> 'a
  (** returns the unique element of a singleton list.
      @raise Invalid_argument on a non singleton list. *)

val filter_out: ('a -> bool) -> 'a list -> 'a list
  (** Filter out elements that pass the test *)

(* ************************************************************************** *)
(** {2 Options} *)
(* ************************************************************************** *)

val may: ('a -> unit) -> 'a option -> unit

val may_map: ('a -> 'b) -> ?dft:'b -> 'a option -> 'b
  (** [may_map f ?dft x] applies [f] to the value of [x] if exists. Otherwise
      returns the default value [dft].
      Assume that either [x] or [dft] is defined. *)

val opt_map: ('a -> 'b) -> 'a option -> 'b option

val opt_filter: ('a -> bool) -> 'a option -> 'a option

val the: 'a option -> 'a
  (** @raise Invalid_argument if the value is none.
      @plugin development guide *)

external getperfcount: unit -> int = "getperfcount"

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

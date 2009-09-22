(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
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

(* $Id: extlib.mli,v 1.16 2009-02-13 07:59:29 uid562 Exp $ *)

(** Useful operations.

    This module does not depend of any of frama-c module.
    @plugin development guide *)

val nop: 'a -> unit
  (** Do nothing. *)

val find_or_none: ('a -> 'b) -> 'a -> 'b option

val adapt_filename: string -> string
  (** Ensure that the given filename has the extension "cmo" in bytecode
      and "cmxs" in native *)

(* [max_cpt t1 t2] returns the maximum of [t1] and [t2] wrt the total ordering
   induced by tags creation. This ordering is defined as follow:
   forall tags t1 t2,
   t1 <= t2 iff
   t1 is before t2 in the finite sequence
   [0; 1; ..; max_int; min_int; min_int-1; -1] *)
val max_cpt: int -> int -> int

(* ************************************************************************* *)
(** {2 Function builders} *)
(* ************************************************************************* *)

exception NotYetImplemented of string
  (** @plugin development guide *)

val not_yet_implemented: string -> 'a
  (** @raise NotYetImplemented with the given string. *)

val mk_fun: string -> ('a -> 'b) ref
  (** build a reference to an unitialized function (which raises
      [NotYetImplemented] if it is called).
      @deprecated since Beryllium-20090601-beta1+dev *)

(* ************************************************************************* *)
(** {2 Function combinators} *)
(* ************************************************************************* *)

val ($) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
  (** Composition. *)

val swap: ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
  (** Swap arguments. *)

(* ************************************************************************* *)
(** {2 Lists} *)
(* ************************************************************************* *)

val as_singleton: 'a list -> 'a
  (** returns the unique element of a singleton list.
      @raise Invalid_argument on a non singleton list. *)

val filter_out: ('a -> bool) -> 'a list -> 'a list
  (** Filter out elements that pass the test *)

val product_fold: ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
(** [product f acc l1 l2] is similar to [fold_left f acc l12] with l12 the
    list of all pairs of an elt of [l1] and an elt of [l2]
*)

val product: ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  (** [product f l1 l2] applies [f] to all the pairs of an elt of [l1] and
      an element of [l2].
   *)

(* ************************************************************************* *)
(** {2 Options} *)
(* ************************************************************************* *)

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
external getperfcount1024: unit -> int = "getperfcount1024"

external address_of_value: 'a -> int = "address_of_value"

(* ************************************************************************* *)
(** {2 Exception catcher} *)
(* ************************************************************************* *)

val try_finally: finally:(unit -> unit) -> ('a -> 'b) -> 'a -> 'b

(* ************************************************************************* *)
(** {2 Launching an external process} *)
(* ************************************************************************* *)

val full_command :
  string -> string array
  -> stdin:Unix.file_descr
  -> stdout:Unix.file_descr
  -> stderr:Unix.file_descr
  -> int
  (** Same arguments as {Unix.create_process} but returns only when
      execution is complete. It returns the exit code of the executed
      process. *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

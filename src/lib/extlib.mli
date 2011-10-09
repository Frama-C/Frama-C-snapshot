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

(** Useful operations.

    This module does not depend of any of frama-c module.
    @plugin development guide *)

val nop: 'a -> unit
  (** Do nothing. *)

val adapt_filename: string -> string
  (** Ensure that the given filename has the extension "cmo" in bytecode
      and "cmxs" in native *)

val max_cpt: int -> int -> int
(** [max_cpt t1 t2] returns the maximum of [t1] and [t2] wrt the total ordering
    induced by tags creation. This ordering is defined as follow: forall tags t1
    t2, t1 <= t2 iff t1 is before t2 in the finite sequence [0; 1; ..; max_int;
    min_int; min_int-1; -1] *)

val number_to_color: int -> int

(* ************************************************************************* *)
(** {2 Function builders} *)
(* ************************************************************************* *)

exception NotYetImplemented of string
(** Use function {!not_yet_implemented} to raise this exception.
    Do never catch it yourself: let the kernel do the job.
    @plugin development guide *)

val not_yet_implemented: string -> 'a
  (** @raise NotYetImplemented with the given string. *)

val mk_fun: string -> ('a -> 'b) ref
  (** build a reference to an unitialized function (which raises
      [NotYetImplemented] if it is called).
      @deprecated since Beryllium-20090901 *)

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

val last: 'a list -> 'a
  (** returns the last element of a list.
      @raise Invalid_argument on an empty list
      @since Nitrogen-20111001
   *)


val filter_out: ('a -> bool) -> 'a list -> 'a list
  (** Filter out elements that pass the test *)

val filter_map: ('a -> bool) -> ('a -> 'b) -> 'a list -> 'b list
val filter_map': ('a -> 'b) -> ('b -> bool) -> 'a list -> 'b list
  (** Combines [filter] and [map]. *)

val product_fold: ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
(** [product f acc l1 l2] is similar to [fold_left f acc l12] with l12 the
    list of all pairs of an elt of [l1] and an elt of [l2]
*)

val product: ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  (** [product f l1 l2] applies [f] to all the pairs of an elt of [l1] and
      an element of [l2].
   *)

val find_index: ('a -> bool) -> 'a list -> int
  (** returns the index (starting at 0) of the first element verifying the
      condition
      @raise Not_found if no element in the list matches the condition
   *)

val list_compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int
  (** Generic list comparison function, where the elements are compared
      with the specified function
      @since Boron-20100401 *)

val list_of_opt: 'a option -> 'a list
  (** converts an option into a list with 0 or 1 elt.
      @since Carbon-20111201-beta2+dev
   *)

val find_opt : ('a -> 'b option) -> 'a list -> 'b
  (** [find_option p l] returns the value [p e], [e] being the first
      element of [l] such that [p e] is not [None]. Raise [Not_found] if there
      is no such value the list l.

      @since Nitrogen-20111001
  *)

val iteri: (int -> 'a -> unit) -> 'a list -> unit
  (** Same as iter, but the function to be applied take also as argument the
      index of the element (starting from 0)
      @since Nitrogen-20111001
   *)


(* ************************************************************************* *)
(** {2 Options} *)
(* ************************************************************************* *)

(** [true] iff its argument is [Some x] 
    @since Nitrogen-20111001
*)
val has_some: 'a option -> bool

val may: ('a -> unit) -> 'a option -> unit

val may_map: ('a -> 'b) -> ?dft:'b -> 'a option -> 'b
  (** [may_map f ?dft x] applies [f] to the value of [x] if exists. Otherwise
      returns the default value [dft].
      Assume that either [x] or [dft] is defined. *)

val opt_map: ('a -> 'b) -> 'a option -> 'b option

(** [opt_bind f x] returns [None] if [x] is [None] and [f y] if is [Some y]
    (monadic bind)
    @since Nitrogen-20111001
*)
val opt_bind: ('a -> 'b option) -> 'a option -> 'b option

val opt_filter: ('a -> bool) -> 'a option -> 'a option

val the: 'a option -> 'a
  (** @raise Invalid_argument if the value is none.
      @plugin development guide *)

val find_or_none: ('a -> 'b) -> 'a -> 'b option

val opt_equal : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool

val opt_compare : ('a -> 'a -> int) -> 'a option -> 'a option -> int
  (** @since Boron-20100401 *)

(* ************************************************************************* *)
(** {2 Strings} *)
(* ************************************************************************* *)

val string_prefix: ?strict:bool -> string -> string -> bool
  (** [string_prefix ~strict p s] returns [true] if and only if [p] is a
      prefix of the string [s]. If [strict] is true, the prefix must be strict
      (that is, [s] must moreover be strictly longer than [p]. [strict]
      is false by default.

      @since Boron-20100401 *)

(* ************************************************************************* *)
(** {2 Performance} *)
(* ************************************************************************* *)

external getperfcount: unit -> int = "getperfcount"
external getperfcount1024: unit -> int = "getperfcount1024"

val time: ?msg:string -> ('a -> 'b) -> 'a -> 'b
val time1024: ?msg:string -> ('a -> 'b) -> 'a -> 'b

external address_of_value: 'a -> int = "address_of_value"

(* ************************************************************************* *)
(** {2 Exception catcher} *)
(* ************************************************************************* *)

val try_finally: finally:(unit -> unit) -> ('a -> 'b) -> 'a -> 'b

(* ************************************************************************* *)
(** System commands *)
(* ************************************************************************* *)

val cleanup_at_exit: string -> unit
  (** [cleanup_at_exit file] indicates that [file] must be removed when the
      program exits (except if exit is caused by a signal).
      If [file] does not exist, nothing happens. *)

exception Temp_file_error of string

val temp_file_cleanup_at_exit: string -> string -> string
  (** Similar to [Filename.temp_file] except that the temporary file will be
      deleted at the end of the execution (see above).
      @raise Temp_file_error if the temp file cannot be created.
      @modify Nitrogen-20111001 may now raise Temp_file_error *)

val temp_dir_cleanup_at_exit: string -> string
(** @raise Temp_file_error if the temp dir cannot be created.
    @modify Nitrogen-20111001 may now raise Temp_file_error *)

val safe_remove: string -> unit
  (** Tries to delete a file and never fails. *)

val safe_remove_dir: string -> unit

val terminate_process: int -> unit
  (** Terminate a process id. *)

val usleep: int -> unit
  (** Unix function that sleep for [n] microseconds.
      See [man usleep] for details.
      Should not be used under Win32. *)

(* ************************************************************************* *)
(** Comparison functions *)
(* ************************************************************************* *)

(** Use this function instead of [Pervasives.compare], as this makes
    it easier to find incorrect uses of the latter *)
external compare_basic: 'a -> 'a -> int = "%compare"

(* ************************************************************************* *)
(** Printing Lexing.position *)
(* ************************************************************************* *)

val pretty_position: Format.formatter -> Lexing.position -> unit

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

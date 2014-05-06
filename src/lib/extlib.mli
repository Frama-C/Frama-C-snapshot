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

(** Useful operations.

    This module does not depend of any of frama-c module.
    @plugin development guide *)

val nop: 'a -> unit
  (** Do nothing. *)

external id: 'a -> 'a = "%identity"
  (** identity function.
      @since Oxygen-20120901
   *)


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

exception Unregistered_function of string
(** Never catch it yourself: let the kernel do the job.
    @since Oxygen-20120901 *)

val mk_labeled_fun: string -> 'a
(** To be used to initialized a reference over a labeled function.
    @since Oxygen-20120901
    @raise Unregistered_function when not properly initialized *)

val mk_fun: string -> ('a -> 'b) ref
  (** Build a reference to an unitialized function
      @raise Unregistered_function when not properly initialized *)

(* ************************************************************************* *)
(** {2 Function combinators} *)
(* ************************************************************************* *)

val ($) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
  (** Composition. *)

val swap: ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
  (** Swap arguments. *)

val iter_uncurry2:
  (('a -> 'b -> unit) -> 'c -> unit) ->
  (('a * 'b -> unit) -> 'c -> unit)

(* ************************************************************************* *)
(** {2 Lists} *)
(* ************************************************************************* *)

val as_singleton: 'a list -> 'a
  (** returns the unique element of a singleton list.
      @raise Invalid_argument on a non singleton list. *)

val last: 'a list -> 'a
  (** returns the last element of a list.
      @raise Invalid_argument on an empty list
      @since Nitrogen-20111001 *)

val filter_out: ('a -> bool) -> 'a list -> 'a list
  (** Filter out elements that pass the test *)

val replace: ('a -> 'a -> bool) -> 'a -> 'a list -> 'a list
  (** [replace cmp x l] replaces the first element [y] of [l] such that
      [cmp x y] is true by [x]. If no such element exists, [x] is added
      at the tail of [l].
      @since Neon-20130301 
   *)

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
      @since Carbon-20111201-beta2+dev *)

val opt_of_list: 'a list -> 'a option
  (** converts a list with 0 or 1 element into an option.
      @raise Invalid_argument on lists with more than one argument
      @since Oxygen-20120901 *)

val find_opt : ('a -> 'b option) -> 'a list -> 'b
  (** [find_option p l] returns the value [p e], [e] being the first
      element of [l] such that [p e] is not [None]. Raise [Not_found] if there
      is no such value the list l.

      @since Nitrogen-20111001 *)

val iteri: (int -> 'a -> unit) -> 'a list -> unit
  (** Same as iter, but the function to be applied take also as argument the
      index of the element (starting from 0). Tail-recursive
      @since Nitrogen-20111001 *)

val mapi: (int -> 'a -> 'b) -> 'a list -> 'b list
  (** Same as map, but the function to be applied take also as argument the
      index of the element (starting from 0). Tail-recursive
      @since Oxygen-20120901 *)

(* ************************************************************************* *)
(** {2 Options} *)
(* ************************************************************************* *)

(** [true] iff its argument is [Some x] 
    @since Nitrogen-20111001 *)
val has_some: 'a option -> bool

val may: ('a -> unit) -> 'a option -> unit
  (** [may f v] applies [f] to [x] if [v = Some(x)] *)

val opt_conv: 'a -> 'a option -> 'a
  (** [opt_conv default v] returns [default] if [v] is [None] and [a] if
      [v] is [Some a] *)

val may_map: ('a -> 'b) -> ?dft:'b -> 'a option -> 'b
  (** [may_map f ?dft x] applies [f] to the value of [x] if exists. Otherwise
      returns the default value [dft].
      Assume that either [x] or [dft] is defined. *)

val opt_map: ('a -> 'b) -> 'a option -> 'b option

val opt_fold: ('a -> 'b -> 'b) -> 'a option -> 'b -> 'b
(** @since Oxygen-20120901 *)

(** [merge f k a b]  returns
    - [None] if both [a] and [b] are [None]
    - [Some a'] (resp. [b'] if [b] (resp [a]) is [None] 
      and [a] (resp. [b]) is [Some]
    - [f k a' b'] if both [a] and [b] are [Some]
    
    It is mainly intended to be used with Map.merge
    
    @since Oxygen-20120901
*)
val merge_opt:
  ('a -> 'b -> 'b -> 'b) -> 'a -> 'b option -> 'b option -> 'b option

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
(** {2 Booleans} *)
(* ************************************************************************* *)

val xor: bool -> bool -> bool
(** exclusive-or. @since Oxygen-20120901 *)

(* ************************************************************************* *)
(** {2 Strings} *)
(* ************************************************************************* *)

val string_prefix: ?strict:bool -> string -> string -> bool
  (** [string_prefix ~strict p s] returns [true] if and only if [p] is a
      prefix of the string [s]. If [strict] is true, the prefix must be strict
      (that is, [s] must moreover be strictly longer than [p]. [strict]
      is false by default.
      @since Boron-20100401 *)

val string_del_prefix: ?strict:bool -> string -> string -> string option
  (** [string_del_prefix ~strict p s] returns [None] if [p] is not a prefix of
      [s] and Some [s1] iff [s=p^s1].
      @since Oxygen-20120901 *)

val string_split: string -> int -> string * string
(** [string_split s i] returns the beginning of [s] up to char [i-1] and the 
    end of [s] starting from char [i+1]
    @raise Invalid_argument if [i] is not in the range [[0,(length s -1)]]
    @since Oxygen-20120901 *)

val make_unique_name: 
  (string -> bool) -> ?sep:string -> ?start:int -> string -> int*string
  (** [make_unique_name mem s] returns [(0, s)] when [(mem s)=false]
      otherwise returns [(n,new_string)] such that [new_string] is 
      derived from [(s,sep,start)] and [(mem new_string)=false] and [n<>0] 
      @since Oxygen-20120901 *)

(* ************************************************************************* *)
(** {2 Performance} *)
(* ************************************************************************* *)

external getperfcount: unit -> int = "getperfcount" "noalloc"
external getperfcount1024: unit -> int = "getperfcount1024" "noalloc"

val time: ?msg:string -> ('a -> 'b) -> 'a -> 'b
val time1024: ?msg:string -> ('a -> 'b) -> 'a -> 'b

external address_of_value: 'a -> int = "address_of_value" "noalloc"

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

val temp_file_cleanup_at_exit: ?debug:bool -> string -> string -> string
  (** Similar to [Filename.temp_file] except that the temporary file will be
      deleted at the end of the execution (see above), unless [debug] is set
      to true, in which case a message with the name of the kept file will be
      printed.
      @raise Temp_file_error if the temp file cannot be created.
      @modify Nitrogen-20111001 may now raise Temp_file_error
      @modify Oxygen-20120901 optional debug argument
*)

val temp_dir_cleanup_at_exit: ?debug:bool -> string -> string
(** @raise Temp_file_error if the temp dir cannot be created.
    @modify Nitrogen-20111001 may now raise Temp_file_error
    @modify Neon-20130301 add optional debug flag *)

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

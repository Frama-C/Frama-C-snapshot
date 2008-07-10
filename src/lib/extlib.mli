(* $Id: extlib.mli,v 1.6 2008/05/30 08:29:49 uid568 Exp $ *)

(** Useful operations. 
    
    This module does not depend of any of frama-c module. 
    @plugin developer guide *)

exception NotYetImplemented of string
  (** @plugin developer guide *)

val not_yet_implemented: string -> 'a
  (** @raise NotYetImplemented with the given string. *)

val mk_fun: string -> ('a -> 'b) ref
  (** build a reference to an unitialized function (which raises
      [NotYetImplemented] if it is called). 
      @plugin developer guide *)

val nop: 'a -> unit
  (** Do nothing. *)

val find_or_none: ('a -> 'b) -> 'a -> 'b option

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

val last: 'a list -> 'a
  (** Returns the last element of a list; O(N), tail recursive.
      @raise Invalid_argument on an empty list. *)

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

val the: 'a option -> 'a
  (** @raise Invalid_argument if the value is none.
      @plugin developer guide *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

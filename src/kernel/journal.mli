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

(** Journalization of functions.
    @plugin development guide *)

(* ****************************************************************************)
(** {2 Journalization} *)
(* ****************************************************************************)

val register:
  string ->
  'a Type.t ->
  ?comment:(Format.formatter -> unit) ->
  ?is_dyn:bool ->
  'a ->
  'a
    (** [register name ty ~comment ~is_dyn v] journalizes the value [v]
        of type [ty] with the name [name]. [name] must exactly match the caml
        long name of the value (i.e. "List.iter" and not "iter" even though the
        module List is already opened). Journalisation of anonymous value is
        not possible.

        If the [comment] argument is set, the given pretty printer will be
        applied in an OCaml comment when the function is journalized.

        Set [is_dyn] to [true] to journalize a dynamic function. *)

val never_write: string -> 'a -> 'a
  (** [never_write name f] returns a closure [g] observationaly equal to [f]
      except that trying to write a call to [g] in the journal is an error. If
      [f] is not a closure, then [never_write name f] raises
      [Invalid_argument]. *)

val prevent: ('a -> 'b) -> 'a -> 'b
  (** [prevent f x] applies [x] to [f] without printing anything in the
      journal, even if [f] is journalized. *)

module Binding: sig
  val add: 'a Type.t -> 'a -> string -> unit
    (** [add ty v var] binds the value [v] to the variable name [var].  Thus,
        [pp ty v] prints [var] and not use the standard pretty printer.  Very
        useful to pretty print values with no associated pretty printer. *)
  exception Name_already_exists of string
  val add_once: 'a Type.t -> 'a -> string -> unit
    (** Same as function [add] above but raise the exception [Already_exists]
        if the binding previously exists *)
end

(* JS 2012/02/07: useful only for BM introspection testing ;-) *)
module Reverse_binding: sig
  
  (* Raised by [find] *)
  exception Unbound_value of string
  exception Incompatible_type of string

  val fill: unit -> unit
  val find: string -> 'a Type.t -> 'a
  val iter: (string -> 'a Type.t -> 'a -> unit) -> unit
  val pretty: Format.formatter -> unit -> unit
end

(* ****************************************************************************)
(** {2 Journal management} *)
(* ****************************************************************************)

val get_name: unit -> string
  (** @return the filename which the journal will be written into. *)

val set_name: string -> unit
  (** [set_name name] changes the filename into the journal is generated. *)

val write: unit -> unit
  (** [write ()] writes the content of the journal into the file set by
      [set_name] (or in "frama_c_journal.ml" by default);
      without clearing the journal. *)

val save: unit -> unit
  (** Save the current state of the journal for future restauration.
      @since Beryllium-20090901 *)

val restore: unit -> unit
  (** Restore a previously saved journal.
      @since Beryllium-20090901 *)

(* ****************************************************************************)
(** {2 Internal use only} *)
(* ****************************************************************************)

val keep_file: string -> unit
  (** This function has not to be used explictely. Only offers functions
      retrieving when running a journal file. *)

val get_session_file: (string -> string) ref

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

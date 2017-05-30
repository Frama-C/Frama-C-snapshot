(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(* -------------------------------------------------------------------------- *)
(** Tactical *)
(* -------------------------------------------------------------------------- *)

open Lang.F
open Conditions

(** {2 Tactical Selection} *)

type clause = Goal of pred | Step of step
type process = sequent -> (string * sequent) list
type status =
  | Not_applicable
  | Not_configured
  | Applicable of process

type selection =
  | Empty
  | Clause of clause
  | Inside of clause * term
  | Compose of compose

and compose = private
  | Cint of Integer.t
  | Range of int * int
  | Code of term * string * selection list

val int : int -> selection
val cint : Integer.t -> selection
val range : int -> int -> selection
val compose : string -> selection list -> selection
val destruct : selection -> selection list

val head : clause -> pred
val is_empty : selection -> bool
val selected : selection -> term
val subclause : clause -> pred -> bool
(** When [subclause clause p], we have [clause = Step H] and [H -> p],
    or [clause = Goal G] and [p -> G]. *)

(** Debug only *)
val pp_clause : Format.formatter -> clause -> unit

(** Debug only *)
val pp_selection : Format.formatter -> selection -> unit

(** {2 Tactical Parameters} *)

type 'a field

module Fmap :
sig
  type t
  val create : unit -> t
  val get : t -> 'a field -> 'a (** raises Not_found if absent *)
  val set : t -> 'a field -> 'a -> unit
end

(** {2 Tactical Parameter Editors} *)

type 'a named = { title : string ; descr : string ; vid : string ; value : 'a }
type 'a range = { vmin : 'a option ; vmax : 'a option ; vstep : 'a }
type 'a browser = ('a named -> unit) -> selection -> unit

type parameter =
  | Checkbox of bool field
  | Spinner  of int field * int range
  | Composer of selection field * (Lang.F.term -> bool)
  | Selector : 'a field * 'a named list * ('a -> 'a -> bool) -> parameter
  | Search : 'a named option field * 'a browser * (string -> 'a) -> parameter

val ident : 'a field -> string
val default : 'a field -> 'a
val signature : 'a field -> 'a named

val checkbox :
  id:string -> title:string -> descr:string ->
  ?default:bool ->
  unit -> bool field * parameter
(** Unless specified, default is [false]. *)

val spinner :
  id:string -> title:string -> descr:string ->
  ?default:int ->
  ?vmin:int -> ?vmax:int -> ?vstep:int ->
  unit -> int field * parameter
(** Unless specified, default is [vmin] or [0] or [vmax], whichever fits.
    Range must be non-empty, and default shall fit in. *)

val selector :
  id:string -> title:string -> descr:string ->
  ?default:'a ->
  options:'a named list ->
  ?equal:('a -> 'a -> bool) ->
  unit -> 'a field * parameter
(** Unless specified, default is head option.
    Default equality is [(=)].
    Options must be non-empty. *)

val composer :
  id:string -> title:string -> descr:string ->
  ?default:selection ->
  ?filter:(Lang.F.term -> bool) ->
  unit -> selection field * parameter
(** Unless specified, default is Empty selection. *)

val search :
  id:string -> title:string -> descr:string ->
  browse:('a browser) ->
  find:(string -> 'a) ->
  unit -> 'a named option field * parameter
(** Search field.
     - [browse s n] is the lookup function, used in the GUI only.
       Shall returns at most [n] results applying to selection [s].
     - [find n] is used at script replay, and shall retrieve the
       selected item's [id] later on. *)

type 'a formatter = ('a,Format.formatter,unit) format -> 'a

class type feedback =
  object
    (** Interactive mode.
        If [false] the GUI is not activated.
        Hence, detailed feedback is not reported to the user. *)
    method interactive : bool

    method get_title : string
    (** Retrieve the title *)

    method has_error : bool
    (** Retrieve the errors *)

    method set_title : 'a. 'a formatter
    (** Update the title {i wrt} current selection & tuning *)

    method set_descr : 'a. 'a formatter
    (** Add a short description {i wrt} current selection & tuning *)

    method set_error : 'a. 'a formatter
    (** Mark the current configuration as invalid *)

    method update_field :
      'a. ?enabled:bool -> ?title:string -> ?tooltip:string ->
          ?range:bool -> ?vmin:int -> ?vmax:int ->
          ?filter:(Lang.F.term -> bool) -> 'a field -> unit
    (** Update field parameters *)

  end

(** {2 Tactical Utilities} *)

val at : selection -> int option
val mapi : (int -> int -> 'a -> 'b) -> 'a list -> 'b list
val insert : ?at:int -> (string * pred) list -> process
val replace : at:int -> (string * condition) list -> process
val split : (string * pred) list -> process
val rewrite : ?at:int -> (string * pred * term * term) list -> process
(** For each pattern [(descr,guard,src,tgt)] replace [src] with [tgt]
    under condition [guard], inserted in position [at]. *)

(** {2 Tactical Plug-in} *)

class type tactical =
  object
    method id : string
    method title : string
    method descr : string
    method params : parameter list
    method reset : unit
    method get_field : 'a. 'a field -> 'a
    method set_field : 'a. 'a field -> 'a -> unit
    method select : feedback -> selection -> status
  end

class virtual make :
  id:string -> title:string -> descr:string -> params:parameter list ->
  object
    method id : string
    method reset : unit
    method get_field : 'a. 'a field -> 'a
    method set_field : 'a. 'a field -> 'a -> unit

    method title : string
    method descr : string
    method params : parameter list

    method reset : unit
    (** Reset all parameters to default *)

    method virtual select : feedback -> selection -> status
    (** Shall return [Applicable] or [Not_configured]
        if the tactic might apply to the selection.
        Hints can be provided here, if appropriate.

        The continuation [f] returned with [Applicable f] shall generates
        sub-goals {i wrt} to the given selection and current field values.

        @raise Exit,Not_found is like returning Not_applicable. *)
  end

(** {2 Composer Factory} *)

class type composer =
  object
    method id : string
    method group : string
    method title : string
    method descr : string
    method arity : int
    method filter : term list -> bool
    method compute : term list -> term
  end

(** {2 Global Registry} *)

type t = tactical

val register : #tactical -> unit
val export : #tactical -> tactical (** Register and returns the tactical *)
val lookup : id:string -> tactical
val iter : (tactical -> unit) -> unit

val add_composer : #composer -> unit
val iter_composer : (composer -> unit) -> unit

(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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
(** Logic Database *)
(* -------------------------------------------------------------------------- *)

open LogicId
open LogicTau
open LogicLang

(** {3 Model Registration} *)

val register : name:string -> pointer:tau -> unit
val on_model : string -> ('a -> 'b) -> 'a -> 'b

(** {3 Declarations} *)

type item =
  | TYPE of int
  | RECORD of field list
  | FUNCTION of var list * tau * term option
  | PREDICATE of var list * pred option
  | AXIOM of pred

type description = {
  t_source : Lexing.position ;
  t_short : string ;
  t_descr : string ;
}

type declaration = {
  d_name : id ;
  d_item : item ;
  d_descr : description ;
}

val declare : declaration -> unit (** Simple declaration. *)

(** {3 Recursive Compilations} *)

val fixpoint : id -> (id -> unit) -> item
  (** Retrieve the definition of [f] if already defined.
      Otherwise compile it with the provided compiler.
      
      The compiler should set an initial value with [default] before
      any recursive call to [fixpoint].  Then, the compiler should
      updates item for [f] or any other mutually-recursive symbol with
      [f] by using [update]. It is a fatal-error to call [declare] on
      a symbol currently compiled by [fixpoint].

      The mutually recursive calls to [fixpoint] are detected, and
      associated compilers are run until stabilisation. All
      mutually-recursive symbols are finally declared and defined with
      their last updates. *)

val default : id -> item -> unit
val update : id -> item -> description -> unit

(** {3 Retrieving Definitions} *)

val get_item : id -> item
  (** Raise [Not_found] if the symbol is undefined. *)

val get_description : id -> description
  (** Raise [Not_found] if the symbol is undefined. *)

val get_declaration : id -> declaration
  (** Raise [Not_found] if the symbol is undefined. *)

(* -------------------------------------------------------------------------- *)
(* --- History Management                                                 --- *)
(* -------------------------------------------------------------------------- *)

val mark_history : unit -> unit
val model_age : unit -> int

val export_items : (declaration list -> unit) -> id list -> unit
val export_goal  : (declaration list -> unit) -> pred -> unit


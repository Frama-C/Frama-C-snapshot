(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(**************************************************************************)

(** Logic expressions *)

open Logic

module Make
    ( Z : Arith.Z )
    ( ADT : Logic.Data )
    ( Field : Logic.Field )
    ( Fun : Logic.Function )
  :
  sig
    (** Logic API *)
    include Logic.Term with module Z = Z
                        and module ADT = ADT
                        and module Field = Field
                        and module Fun = Fun

    (** Prints term in debug mode. *)
    val debug : Format.formatter -> term -> unit

    (** {2 Global State}
        One given [term] has valid meaning only for one particular state. *)

    type state (** Hash-consing, cache, rewriting rules, etc. *)
    val create : unit -> state
    (** Create a new fresh state. Local state is not modified. *)

    val get_state : unit -> state (** Return local state. *)
    val set_state : state -> unit (** Update local state. *)
    val clr_state : state -> unit (** Clear local state. *)

    (** Register a constant in the global state. *)
    val constant : term -> term

    (** {2 Registered Checks} *)

    val check : repr -> term -> term
    val check_unit : qed:term -> raw:term -> term
    val iter_checks : (qed:term -> raw:term -> unit) -> unit

    (** {2 Context Release} *)

    val release : unit -> unit
    (** Clear caches and checks. Global builtins are kept. *)

  end

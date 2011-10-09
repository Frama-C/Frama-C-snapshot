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

(** ACSL comparable property.
    @since Carbon-20101201 *)

open Cil_types

(**************************************************************************)
(** {2 Type declarations} *)
(**************************************************************************)

(* [JS 20110607] TODO: redesigned the type below in order to:
   - use private records instead of tuples whenever possible
   - extend identified_property to any possible annotations
   - design more consistent type
   For instance,
   - why code annotations are represented so differently?
   - why type [behavior_or_loop] does not contain "assigns" somewhere in its
   name?
   - why this last type cannot be private? *)

(** assigns can belong either to a contract or a loop annotation *)
type behavior_or_loop = (* private *)
  | Id_behavior of funbehavior
  | Id_code_annot of code_annotation

type identified_complete = kernel_function * kinstr * string list
type identified_disjoint = identified_complete

(** Only AAssert, AInvariant, or APragma. Other code annotations are
    dispatched as identified_property of their own. *)
type identified_code_annotation = kernel_function * stmt * code_annotation

type identified_assigns =
    kernel_function * kinstr * behavior_or_loop * identified_term from list

type identified_from =
    kernel_function
    * kinstr
    * behavior_or_loop
    * (identified_term from (* * identified_term list *) )

type identified_decrease =
    kernel_function * kinstr * code_annotation option * term variant
(** code_annotation is None for decreases and [Some { AVariant }] for
    loop variant. *)

type identified_behavior = kernel_function * kinstr * funbehavior

type predicate_kind = private
  | PKRequires of funbehavior
  | PKAssumes of funbehavior
  | PKEnsures of funbehavior * termination_kind
  | PKTerminates

type identified_predicate =
    predicate_kind * kernel_function * kinstr * Cil_types.identified_predicate

type identified_unreachable =
  | UStmt of kernel_function * stmt
  | UProperty of identified_property

and identified_axiomatic = string * identified_property list

and identified_property = private
  | IPPredicate of identified_predicate
  | IPAxiom of string
  | IPAxiomatic of identified_axiomatic
  | IPLemma of string
  | IPBehavior of identified_behavior
  | IPComplete of identified_complete
  | IPDisjoint of identified_disjoint
  | IPCodeAnnot of identified_code_annotation
  | IPAssigns of identified_assigns
  | IPFrom of identified_from
  | IPDecrease of identified_decrease
  | IPUnreachable of identified_unreachable
  | IPOther of string * kernel_function option * kinstr

include Datatype.S_with_collections with type t = identified_property

(* [JS 2011/08/04] seem to be unused *)
(*val short_pretty: Format.formatter -> t -> unit*)

(**************************************************************************)
(** {2 Smart constructors} *)
(**************************************************************************)

val ip_other: string -> kernel_function option -> kinstr -> identified_property
(** Create a non-standard property.
    @since Nitrogen-20111001 *)

val ip_unreachable_stmt: kernel_function -> stmt -> identified_property
(** @since Carbon-20110201 *)

val ip_unreachable_ppt: identified_property -> identified_property
(** @since Carbon-20110201 *)

(** IPPredicate of a single requires.
    @since Carbon-20110201 *)
val ip_of_requires:
  kernel_function -> kinstr -> funbehavior ->
  Cil_types.identified_predicate -> identified_property

(** Builds the IPPredicate corresponding to requires of a behavior.
    @since Carbon-20110201 *)
val ip_requires_of_behavior:
  kernel_function -> kinstr -> funbehavior -> identified_property list

(** IPPredicate of a single assumes.
    @since Carbon-20110201 *)
val ip_of_assumes:
  kernel_function -> kinstr -> funbehavior ->
  Cil_types.identified_predicate -> identified_property

(** Builds the IPPredicate corresponding to assumes of a behavior.
    @since Carbon-20110201 *)
val ip_assumes_of_behavior:
  kernel_function -> kinstr -> funbehavior -> identified_property list

(** IPPredicate of single ensures.
    @since Carbon-20110201 *)
val ip_of_ensures:
  kernel_function -> kinstr -> funbehavior ->
  (termination_kind * Cil_types.identified_predicate) -> identified_property

(** Builds the IPPredicate PKEnsures corresponding to a behavior.
    @since Carbon-20110201 *)
val ip_ensures_of_behavior:
  kernel_function -> kinstr -> funbehavior -> identified_property list

(** Builds the corresponding IPAssigns.
    @since Carbon-20110201 *)
val ip_of_assigns:
  kernel_function -> kinstr ->
  behavior_or_loop -> identified_term assigns -> identified_property option

(** Builds IPAssigns for a contract (if not WritesAny)
    @since Carbon-20110201 *)
val ip_assigns_of_behavior:
  kernel_function -> kinstr -> funbehavior -> identified_property option

(** Builds the corresponding IPFrom.
    @since Carbon-20110201 *)
val ip_of_from:
  kernel_function -> kinstr ->
  behavior_or_loop -> identified_term from -> identified_property

(** Builds IPFrom for a contract (if not ReadsAny)
    @since Carbon-20110201 *)
val ip_from_of_behavior:
  kernel_function -> kinstr -> funbehavior -> identified_property list

(** Builds IPAssigns for a loop annotation (if not WritesAny)
    @since Carbon-20110201 *)
val ip_assigns_of_code_annot:
  kernel_function -> kinstr -> code_annotation -> identified_property option

(** Builds IPFrom for a loop annotation(if not ReadsAny)
    @since Carbon-20110201 *)
val ip_from_of_code_annot:
  kernel_function -> kinstr -> code_annotation -> identified_property list

(** Builds all IP related to the post-conditions (including assigns and from)
    @since Carbon-20110201 *)
val ip_post_cond_of_behavior:
  kernel_function -> kinstr -> funbehavior -> identified_property list

(** Builds an IP for the post-conditions (including assigns and from)
    of the behavior.
    @since Carbon-20110201 *)
val ip_of_behavior:
  kernel_function -> kinstr -> funbehavior -> identified_property

(** Builds all IP related to a behavior.
    @since Carbon-20110201 *)
val ip_all_of_behavior:
  kernel_function -> kinstr -> funbehavior -> identified_property list

(** Builds IPComplete.
    @since Carbon-20110201 *)
val ip_of_complete:
  kernel_function -> kinstr -> string list -> identified_property

(** Builds IPComplete of a given spec.
    @since Carbon-20110201 *)
val ip_complete_of_spec:
  kernel_function -> kinstr -> funspec -> identified_property list

(** Builds IPDisjoint.
    @since Carbon-20110201 *)
val ip_of_disjoint:
  kernel_function -> kinstr -> string list -> identified_property

(** Builds IPDisjoint of a given spec.
    @since Carbon-20110201 *)
val ip_disjoint_of_spec:
  kernel_function -> kinstr -> funspec -> identified_property list

val ip_of_terminates:
  kernel_function -> kinstr ->
  Cil_types.identified_predicate -> identified_property

(** Builds IPTerminates of a given spec.
    @since Carbon-20110201 *)
val ip_terminates_of_spec:
  kernel_function -> kinstr -> funspec -> identified_property option

(** Builds IPDecrease
    @since Carbon-20110201 *)
val ip_of_decreases:
  kernel_function -> kinstr -> term variant -> identified_property

(** Builds IPDecrease of a given spec.
    @since Carbon-20110201 *)
val ip_decreases_of_spec:
  kernel_function -> kinstr -> funspec -> identified_property option

(** Builds all IP of post-conditions related to a spec.
    @since Carbon-20110201 *)
val ip_post_cond_of_spec:
  kernel_function -> kinstr -> funspec -> identified_property list

(** Builds all IP related to a spec.
    @since Carbon-20110201 *)
val ip_of_spec:
  kernel_function -> kinstr -> funspec -> identified_property list

(** Builds an IPAxiom.
    @since Carbon-20110201 *)
val ip_axiom: string -> identified_property

val ip_lemma: string -> identified_property
(** Build an IPLemma.
    @since Nitrogen-20111001 *)

(** Builds all IP related to a given code annotation.
    @since Carbon-20110201 *)
val ip_of_code_annot:
  kernel_function -> stmt -> code_annotation -> identified_property list

(** Builds the IP related to the code annotation.
    should be used only on code annotations returning a single ip, i.e.
    assert, invariant, variant, pragma.
    @raise Invalid_argument if the resulting code annotation has an empty set
    of identified property
    @since Carbon-20110201 *)
val ip_of_code_annot_single:
  kernel_function -> stmt -> code_annotation -> identified_property

val ip_of_global_annotation: global_annotation -> identified_property list
(** @since Nitrogen-20111001 *)

val ip_of_global_annotation_single: 
  global_annotation -> identified_property option
(** @since Nitrogen-20111001 *)

(**************************************************************************)
(** {2 getters} *)
(**************************************************************************)

val get_kinstr: identified_property -> kinstr
val get_kf: identified_property -> kernel_function option
val get_behavior: identified_property -> funbehavior option

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

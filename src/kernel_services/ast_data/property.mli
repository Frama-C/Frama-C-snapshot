(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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
    @since Carbon-20101201
    @plugin development guide *)

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
  | Id_contract of Datatype.String.Set.t * funbehavior
  (** in case of statement contract, we can have different contracts
      based on different sets of active behaviors. *)
  | Id_loop of code_annotation

(** Only AAssert, AInvariant, or APragma. Other code annotations are
    dispatched as identified_property of their own. *)
type identified_code_annotation = {
  ica_kf : kernel_function;
  ica_stmt : stmt;
  ica_ca : code_annotation
}

type identified_assigns = {
  ias_kf : kernel_function;
  ias_kinstr : kinstr;
  ias_bhv : behavior_or_loop;
  ias_froms : from list
}

type identified_allocation = {
  ial_kf : kernel_function;
  ial_kinstr : kinstr;
  ial_bhv : behavior_or_loop;
  ial_allocs : identified_term list * identified_term list
}

type identified_from = {
  if_kf : kernel_function;
  if_kinstr : kinstr;
  if_bhv : behavior_or_loop;
  if_from : from
}

type identified_decrease = {
  id_kf : kernel_function;
  id_kinstr : kinstr;
  id_ca : code_annotation option;
  id_variant : variant
}
(** code_annotation is None for decreases and [Some { AVariant }] for
    loop variant. *)

type identified_behavior = {
  ib_kf : kernel_function;
  ib_kinstr : kinstr;
  ib_active : Datatype.String.Set.t;
  ib_bhv : funbehavior
}
(** for statement contract, the set of parent behavior for which the
    contract is active is part of its identification. If the set is empty,
    the contract is active for all parent behaviors.
*)

type identified_complete = {
  ic_kf : kernel_function;
  ic_kinstr : kinstr;
  ic_active : Datatype.String.Set.t;
  ic_bhvs : string list
}
(** Same as for {!identified_behavior}. *)

type identified_disjoint = identified_complete

type predicate_kind = private
  | PKRequires of funbehavior
  | PKAssumes of funbehavior
  | PKEnsures of funbehavior * termination_kind
  | PKTerminates

type identified_predicate = {
  ip_kind : predicate_kind;
  ip_kf : kernel_function;
  ip_kinstr : kinstr;
  ip_pred : Cil_types.identified_predicate
}

type program_point = Before | After

type identified_reachable = {
  ir_kf : kernel_function option;
  ir_kinstr : kinstr;
  ir_program_point : program_point
}
(** [None, Kglobal] --> global property
    [None, Some ki] --> impossible
    [Some kf, Kglobal] --> property of a function without code
    [Some kf, Kstmt stmt] --> reachability of the given stmt (and the attached
    properties) *)

type other_loc =
  | OLContract of kernel_function
  | OLStmt of kernel_function * stmt
  | OLGlob of location

type extended_loc =
  | ELContract of kernel_function
  | ELStmt of kernel_function * stmt
  | ELGlob

type identified_extended = {
  ie_loc : extended_loc;
  ie_ext : Cil_types.acsl_extension
}

and identified_axiomatic = {
  iax_name : string;
  iax_props : identified_property list
}

and identified_lemma = {
  il_name : string;
  il_labels : logic_label list;
  il_args : string list;
  il_pred : predicate;
  il_loc : location
}

and identified_axiom = identified_lemma

(** Specialization of a property at a given point, identified by a statement
    and a function, along with the predicate transposed at this point (if it
    can be) and the original property. *)
and identified_instance = {
  ii_kf : kernel_function;
  ii_stmt : stmt;
  ii_pred : Cil_types.identified_predicate option;
  ii_ip : identified_property
}

and identified_type_invariant = {
  iti_name : string;
  iti_type : typ;
  iti_pred : predicate;
  iti_loc : location
}

and identified_global_invariant = {
  igi_name : string;
  igi_pred : predicate;
  igi_loc : location
}

and identified_other = {
  io_name : string;
  io_loc : other_loc
}

and identified_property = private
  | IPPredicate of identified_predicate
  | IPExtended of identified_extended
  | IPAxiom of identified_axiom
  | IPAxiomatic of identified_axiomatic
  | IPLemma of identified_lemma
  | IPBehavior of identified_behavior
  | IPComplete of identified_complete
  | IPDisjoint of identified_disjoint
  | IPCodeAnnot of identified_code_annotation
  | IPAllocation of identified_allocation
  | IPAssigns of identified_assigns
  | IPFrom of identified_from
  | IPDecrease of identified_decrease
  | IPReachable of identified_reachable
  | IPPropertyInstance of identified_instance
  | IPTypeInvariant of identified_type_invariant
  | IPGlobalInvariant of identified_global_invariant
  | IPOther of identified_other

include Datatype.S_with_collections with type t = identified_property

val short_pretty: Format.formatter -> t -> unit
(** output a meaningful name for the property (e.g. the name of the
    corresponding identified predicate when available)
    reverting back to the full ACSL formula if it can't find one.
    The name is not meant to uniquely identify the property.
    @since Neon-20140301
*)

(** @since Oxygen-20120901 *)
val pretty_predicate_kind: Format.formatter -> predicate_kind -> unit

val pretty_debug: Format.formatter -> identified_property -> unit
(** Internal use only.
    @since 18.0-Argon *)

(** create a Loc_contract or Loc_stmt depending on the kinstr.
    @since 18.0-Argon
*)
val e_loc_of_stmt: kernel_function -> kinstr -> extended_loc

(** create a Loc_contract or Loc_stmt depending on the kinstr.
    @since 18.0-Argon
*)
val o_loc_of_stmt: kernel_function -> kinstr -> other_loc


(**************************************************************************)
(** {2 Smart constructors} *)
(**************************************************************************)

val ip_other: string -> other_loc -> identified_property
(** Create a non-standard property.
    @since Nitrogen-20111001
    @modify 18.0-Argon Refine localisation argument
*)

val ip_reachable_stmt: kernel_function -> stmt -> identified_property
(** @since Oxygen-20120901 *)

val ip_reachable_ppt: identified_property -> identified_property
(** @since Oxygen-20120901 *)

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

(** Extended property.
    @since Chlorine-20180501
    @modify 18.0-Argon refine localisation argument
*)
val ip_of_extended: extended_loc -> acsl_extension -> identified_property

(** Builds the IPPredicate PKEnsures corresponding to a behavior.
    @since Carbon-20110201 *)
val ip_ensures_of_behavior:
  kernel_function -> kinstr -> funbehavior -> identified_property list

(** Builds the corresponding IPAllocation.
    @since Oxygen-20120901 *)
val ip_of_allocation:
  kernel_function -> kinstr -> behavior_or_loop
  -> allocation -> identified_property option

(** [ip_allocation_of_behavior kf ki active bhv] builds IPAllocation for
    behavior [bhv], in the spec in function [kf], at statement [ki], under
    active behaviors [active]
    @since Oxygen-20120901
    @modify Aluminium-20160501 added active argument
*)
val ip_allocation_of_behavior:
  kernel_function -> kinstr -> active:string list ->
  funbehavior -> identified_property option

(** Builds the corresponding IPAssigns.
    @since Carbon-20110201 *)
val ip_of_assigns:
  kernel_function -> kinstr ->
  behavior_or_loop -> assigns -> identified_property option

(** [ip_assigns_of_behavior kf ki active bhv]
    builds IPAssigns for a contract (if not WritesAny).
    See {!ip_allocation_of_behavior} for signification of [active].
    @since Carbon-20110201
    @modify Aluminium-20160501 added active argument
*)
val ip_assigns_of_behavior:
  kernel_function -> kinstr -> active:string list ->
  funbehavior -> identified_property option

(** Builds the corresponding IPFrom (if not FromAny)
    @since Carbon-20110201
    @modify Aluminium-20160501 returns an option. *)
val ip_of_from:
  kernel_function -> kinstr ->
  behavior_or_loop -> from -> identified_property option

(** [ip_from_of_behavior kf ki active bhv]
    builds IPFrom for a behavior (if not ReadsAny).
    See {!ip_from_of_behavior} for signification of [active]
    @since Carbon-20110201
    @modify Aluminium-20160501 added active argument
*)
val ip_from_of_behavior:
  kernel_function -> kinstr -> active:string list ->
  funbehavior -> identified_property list

(** Builds IPAssigns for a loop annotation (if not WritesAny)
    @since Carbon-20110201 *)
val ip_assigns_of_code_annot:
  kernel_function -> kinstr -> code_annotation -> identified_property option

(** Builds IPFrom for a loop annotation(if not ReadsAny)
    @since Carbon-20110201 *)
val ip_from_of_code_annot:
  kernel_function -> kinstr -> code_annotation -> identified_property list

(** [ip_post_cond_of_behavior kf ki active bhv]
    builds all IP related to the post-conditions (including allocates, frees,
    assigns and from). See {!ip_allocation_of_behavior} for the signification
    of the [active] argument.
    @since Carbon-20110201
    @modify Aluminium-20160501 added active argument
*)
val ip_post_cond_of_behavior:
  kernel_function -> kinstr -> active:string list ->
  funbehavior -> identified_property list

(** [ip_of_behavior kf ki activd bhv] builds the IP corresponding
    to the behavior itself.
    See {!ip_allocation_of_behavior} for signification of [active]
    @since Carbon-20110201
    @modify Aluminium-20160501 added active argument
*)
val ip_of_behavior:
  kernel_function -> kinstr -> active:string list ->
  funbehavior -> identified_property

(** [ip_all_of_behavior kf ki active bhv] builds all IP related to a behavior.
    See {!ip_allocation_of_behavior} for signification of [active]
    @since Carbon-20110201
    @modify Aluminium-20160501 added active argument
*)
val ip_all_of_behavior:
  kernel_function -> kinstr -> active:string list ->
  funbehavior -> identified_property list

(** [ip_of_complete kf ki active complete] builds IPComplete.
    See {!ip_allocation_of_behavior} for signification of [active]
    @since Carbon-20110201
    @modify Aluminium-20160501 added active argument
*)
val ip_of_complete:
  kernel_function -> kinstr -> active:string list ->
  string list -> identified_property

(** [ip_complete_of_spec kf ki active spec] builds IPComplete of a given spec.
    See {!ip_allocation_of_behavior} for signification of [active]
    @since Carbon-20110201
    @modify Aluminium-20160501 added active argument
*)
val ip_complete_of_spec:
  kernel_function -> kinstr -> active:string list ->
  funspec -> identified_property list

(** [ip_of_disjoint kf ki active disjoint] builds IPDisjoint.
    See {!ip_allocation_of_behavior} for signification of [active]
    @since Carbon-20110201
    @modify Aluminium-20160501 added active argument
*)
val ip_of_disjoint:
  kernel_function -> kinstr -> active:string list ->
  string list -> identified_property

(** [ip_disjoint_of_spec kf ki active spec] builds IPDisjoint of a given spec.
    See {!ip_allocation_of_behavior} for signification of [active]
    @since Carbon-20110201
    @modify Aluminium-20160501 added active argument
*)
val ip_disjoint_of_spec:
  kernel_function -> kinstr -> active:string list ->
  funspec -> identified_property list

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
  kernel_function -> kinstr -> variant -> identified_property

(** Builds IPDecrease of a given spec.
    @since Carbon-20110201 *)
val ip_decreases_of_spec:
  kernel_function -> kinstr -> funspec -> identified_property option

(** [ip_post_cond_of_spec kf ki active spec]
    builds all IP of post-conditions related to a spec.
    See {!ip_post_cond_of_behavior} for more information.
    @since Carbon-20110201
    @modify Aluminium-20160501 added active argument
*)
val ip_post_cond_of_spec:
  kernel_function -> kinstr -> active:string list ->
  funspec -> identified_property list

(** [ip_of_spec kf ki active spec] builds all IP related to a spec.
    See {!ip_allocation_of_behavior} for signification of [active]
    @since Carbon-20110201
    @modify Aluminium-20160501 added active argument
*)
val ip_of_spec:
  kernel_function -> kinstr -> active:string list ->
  funspec -> identified_property list

(** Build a specialization of the given property at the given function and
    stmt. The predicate is the property predicate transposed at the given
    statement, or None if it can't be. *)
val ip_property_instance:
  kernel_function -> stmt -> Cil_types.identified_predicate option ->
  identified_property -> identified_property

(** Builds an IPAxiom.
    @since Carbon-20110201
    @modify Oxygen-20120901 takes an identified_axiom instead of a string
*)
val ip_axiom: identified_axiom -> identified_property

(** Build an IPLemma.
    @since Nitrogen-20111001
    @modify Oxygen-20120901 takes an identified_lemma instead of a string
*)
val ip_lemma: identified_lemma -> identified_property

(** Build an IPTypeInvariant. *)
val ip_type_invariant: identified_type_invariant -> identified_property

(** Build an IPGlobalInvariant. *)
val ip_global_invariant: identified_global_invariant -> identified_property

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

val has_status: identified_property -> bool
(** Does the property has a logical status (which may be Never_tried)?
    False for pragma, assumes clauses and some ACSL extensions.
    @since 19.0-Potassium *)

val get_kinstr: identified_property -> kinstr
val get_kf: identified_property -> kernel_function option
val get_behavior: identified_property -> funbehavior option

val location: identified_property -> location
(** returns the location of the property.
    @since Oxygen-20120901 *)

val source: identified_property -> Filepath.position option
(** returns the location of the property, if not unknown.
    @since Chlorine-20180501 *)

(**************************************************************************)
(** {2 names} *)
(**************************************************************************)


(** @since 19.0-Potassium deprecated old naming scheme,
    to be removed in future versions. *)
module LegacyNames :
sig
  val self: State.t
  val get_prop_basename: identified_property -> string
  val get_prop_name_id: identified_property -> string
end

(** @since Oxygen-20120901 *)
module Names :
sig

  val self: State.t

  val get_prop_name_id: identified_property -> string
  (** returns a unique name identifying the property.
      This name is built from the basename of the property.
      @modify 19.0-Potassium new naming scheme, Cf. LegacyNames
  *)

  val get_prop_basename: ?truncate:int -> identified_property -> string
  (** returns the basename of the property.
      @modify 19.0-Potassium additional truncation parameter
  *)

end

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

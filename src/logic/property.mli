(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(** ACSL comparable property.
    @since Carbon-20101201 *)

open Cil_types
open Db_types

(** assigns can belong either to a contract or a loop annotation *)
type behavior_or_loop =
    Id_behavior of funbehavior
  | Id_code_annot of code_annotation

type identified_complete = kernel_function * kinstr * string list
type identified_disjoint =  identified_complete

(** Only AAssert, AInvariant, or APragma. Other code annotations are
    dispatched as identified_property of their own.
*)
type identified_code_annotation = 
    kernel_function * stmt * code_annotation

type identified_assigns =
    kernel_function
    * kinstr
    * behavior_or_loop
    * identified_term from list

type identified_from =
    kernel_function
    * kinstr
    * behavior_or_loop
    * (identified_term * identified_term list)

type identified_decrease = 
    kernel_function * kinstr * code_annotation option * term variant
(** code_annotation is None for decreases and [Some { AVariant }] for
    loop variant.
 *)

type identified_behavior = kernel_function * kinstr * funbehavior

type predicate_kind =
  | PKRequires of funbehavior
  | PKAssumes of funbehavior
  | PKEnsures of funbehavior * termination_kind
  | PKTerminates

type identified_predicate =
    predicate_kind * kernel_function * kinstr * Cil_types.identified_predicate

type identified_spec = kernel_function * kinstr * funspec

type identified_property = private
  | IPBlob of State.t (* an unidentified property *)
  | IPPredicate of identified_predicate
  | IPAxiom of string
  | IPComplete of identified_complete
  | IPDisjoint of identified_disjoint
  | IPCodeAnnot of identified_code_annotation
  | IPBehavior of identified_behavior
  | IPAssigns of identified_assigns
  | IPFrom of identified_from
  | IPDecrease of identified_decrease

include Datatype.S_with_collections with type t = identified_property

(** {2 Builders} *)

(** Builds an IPBehavior. 
    @since Carbon-20101201-beta2+dev
*)
val ip_of_behavior: 
  kernel_function -> kinstr -> funbehavior -> identified_property

(** IPPredicate of a single requires.
    @since Carbon-20101201-beta2+dev
*)
val ip_of_requires:
  kernel_function -> kinstr -> funbehavior -> 
  Cil_types.identified_predicate -> identified_property

(** Builds the IPPredicate corresponding to requires of a behavior. 
    @since Carbon-20101201-beta2+dev
*)
val ip_requires_of_behavior:
  kernel_function -> kinstr -> funbehavior -> identified_property list

(** IPPredicate of a single assumes.
    @since Carbon-20101201-beta2+dev
*)
val ip_of_assumes:
  kernel_function -> kinstr -> funbehavior -> 
  Cil_types.identified_predicate -> identified_property

(** Builds the IPPredicate corresponding to assumes of a behavior. 
    @since Carbon-20101201-beta2+dev
*)
val ip_assumes_of_behavior:
  kernel_function -> kinstr -> funbehavior -> identified_property list

(** IPPredicate of single ensures. 
    @since Carbon-20101201-beta2+dev
*)
val ip_of_ensures:
  kernel_function -> kinstr -> funbehavior -> 
  (termination_kind * Cil_types.identified_predicate) -> identified_property

(** Builds the IPPredicate PKEnsures corresponding to a behavior. 
    @since Carbon-20101201-beta2+dev
*)
val ip_ensures_of_behavior: 
  kernel_function -> kinstr -> funbehavior -> identified_property list

(** Builds the corresponding IPAssigns. 
    @since Carbon-20101201-beta2+dev
*)
val ip_of_assigns:
  kernel_function -> kinstr -> 
  behavior_or_loop -> identified_term assigns -> identified_property option

(** Builds IPAssigns for a contract (if not WritesAny) 
    @since Carbon-20101201-beta2+dev
*)
val ip_assigns_of_behavior:
  kernel_function -> kinstr -> funbehavior -> identified_property option

(** Builds the corresponding IPFrom. 
    @since Carbon-20101201-beta2+dev
*)
val ip_of_from:
  kernel_function -> kinstr -> 
  behavior_or_loop -> identified_term from -> identified_property option

(** Builds IPFrom for a contract (if not ReadsAny) 
    @since Carbon-20101201-beta2+dev
*)
val ip_from_of_behavior:
  kernel_function -> kinstr -> funbehavior -> identified_property list

(** Builds IPAssigns for a loop annotation (if not WritesAny) 
    @since Carbon-20101201-beta2+dev
*)
val ip_assigns_of_code_annot:
  kernel_function -> kinstr -> code_annotation -> identified_property option

(** Builds IPFrom for a loop annotation(if not ReadsAny) 
    @since Carbon-20101201-beta2+dev
*)
val ip_from_of_code_annot:
  kernel_function -> kinstr -> code_annotation -> identified_property list

(** Builds all IP related to the post-conditions (including assigns and from)
    @since Carbon-20101201-beta2+dev
*)
val ip_post_cond_of_behavior:
  kernel_function -> kinstr -> funbehavior -> identified_property list

(** Builds all IP related to a behavior. 
    @since Carbon-20101201-beta2+dev
*)
val ip_all_of_behavior:
  kernel_function -> kinstr -> funbehavior -> identified_property list

(** Builds IPComplete. 
    @since Carbon-20101201-beta2+dev
*)
val ip_of_complete:
  kernel_function -> kinstr -> string list -> identified_property

(** Builds IPComplete of a given spec. 
    @since Carbon-20101201-beta2+dev
*)
val ip_complete_of_spec:
  kernel_function -> kinstr -> funspec -> identified_property list

(** Builds IPDisjoint. 
    @since Carbon-20101201-beta2+dev
*)
val ip_of_disjoint:
  kernel_function -> kinstr -> string list -> identified_property

(** Builds IPDisjoint of a given spec. 
    @since Carbon-20101201-beta2+dev
*)
val ip_disjoint_of_spec:
  kernel_function -> kinstr -> funspec -> identified_property list

val ip_of_terminates:
  kernel_function -> kinstr -> 
  Cil_types.identified_predicate -> identified_property

(** Builds IPTerminates of a given spec. 
    @since Carbon-20101201-beta2+dev
*)
val ip_terminates_of_spec:
  kernel_function -> kinstr -> funspec -> identified_property option

(** Builds IPDecrease
    @since Carbon-20101201-beta2+dev
*)
val ip_of_decreases:
  kernel_function -> kinstr -> term variant -> identified_property

(** Builds IPDecrease of a given spec. 
    @since Carbon-20101201-beta2+dev
*)
val ip_decreases_of_spec:
  kernel_function -> kinstr -> funspec -> identified_property option


(** Builds all IP of post-conditions related to a spec. 
    @since Carbon-20101201-beta2+dev
*)
val ip_post_cond_of_spec: 
  kernel_function -> kinstr -> funspec -> identified_property list

(** Builds all IP related to a spec. 
    @since Carbon-20101201-beta2+dev
*)
val ip_of_spec:
  kernel_function -> kinstr -> funspec -> identified_property list

(** Builds an IPAxiom. 
    @since Carbon-20101201-beta2+dev
*)
val ip_axiom: string -> identified_property

(** Builds an IPBlob. 
    @since Carbon-20101201-beta2+dev
*)
val ip_blob: State.t -> identified_property

(** Builds all IP related to a given code annotation. \
    @since Carbon-20101201-beta2+dev
*)
val ip_of_code_annot:
  kernel_function -> stmt -> code_annotation -> identified_property list

(** Builds the IP related to the code annotation.
    should be used only on code annotations returning a single ip, i.e.
    assert, invariant, variant, pragma.
    @raise Invalid_argument if the resulting code annotation has an empty set
    of identified property
    @since Carbon-20101201-beta2+dev
*)
val ip_of_code_annot_single:
  kernel_function -> stmt -> code_annotation -> identified_property

(** {2 getters} *)

val get_kinstr: identified_property -> kinstr
val get_kf: identified_property -> kernel_function option
val get_behavior: identified_property -> funbehavior option

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

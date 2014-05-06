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

(** Status of properties.
    @since Nitrogen-20111001
    @plugin development guide *)

(* ************************************************************************ *)
(** {2 Local status} 

    A local status (shortly, a status) of a property is a status directly set 
    by an emitter. Thus a property may have several distinct status according to
    who attempts the verification. *)
(* ************************************************************************ *)

(* ************************************************************************ *)
(** {3 Emitting a status} *)
(* ************************************************************************ *)

(** Type of status emitted by analyzers. Each Property is attached to a program
    point [s] and implicitely depends on an execution path from the program
    entry point to [s]. It also depends on an explicit set of hypotheses [H]
    indicating when emitting the property (see function {!emit}). *)
type emitted_status = 
  | True (** for each execution path [ep] from the program entry point to [s],
	     the formula (/\_{h in H} h) ==> P(ep) is true *)
  | False_if_reachable (** for each execution path [ep] from the program entry
			   point to [s], the formula (/\_{h in H} h) ==> P(ep)
			   is false *) 
  | False_and_reachable (** it exists an execution path [ep] from the program
			    entry point to [s] such that the formula (/\_{h in
			    H} h) ==> P(ep) is false *)
  | Dont_know (** any other case *)

module Emitted_status: Datatype.S with type t = emitted_status

exception Inconsistent_emitted_status of emitted_status * emitted_status

val emit: 
  Emitter.t -> hyps:Property.t list -> Property.t -> ?distinct:bool -> 
  emitted_status -> unit
(** [emit e ~hyps p s] indicates that the status of [p] is [s], is emitted by
    [e], and is based on the list of hypothesis [hyps].  If [e] previously
    emitted another status [s'], it must be emitted with the same hypotheses and
    a consistency check is performed between [s] and [s'] and the best (by
    default the strongest) status is kept. If [distinct] is [true] (default is
    [false]), then we consider than the given status actually merges several
    statuses coming from distinct execution paths. The strategy for computing
    the best status is changed accordingly. One example when [~distinct:true]
    may be required is when emitting a status for a pre-condition of a function
    [f] since the status associated to a pre-condition [p] merges all statuses
    of [p] at each callsite of the function [f].  @return the kept status.
    @raise Inconsistent_emitted_status when emiting False after emiting True or
    conversely *)

val emit_and_get:
  Emitter.t -> hyps:Property.t list -> Property.t -> ?distinct:bool ->
  emitted_status -> emitted_status
(** Like {!emit} but also returns the computed status. *)

val logical_consequence: Emitter.t -> Property.t -> Property.t list -> unit
(** [logical_consequence e ppt list] indicates that the emitter [e] considers
    that [ppt] is a logical consequence of the conjunction of properties
    [list]. Thus it lets the kernel automatically computes it: [e] must not call
    functions [emit*] itself on this property, but the kernel ensures that the
    status will be up-to-date when getting it. *)

val legal_dependency_cycle: Emitter.t -> Property.Set.t -> unit
(** The given properties may define a legal dependency cycle for the given
    emitter. 
    @since Oxygen-20120901 *)

val self: State.t
(** The state which stores the computed status. *)

(* ************************************************************************ *)
(** {3 Getting a (local) status} *)
(* ************************************************************************ *)

type emitter_with_properties = private
    { emitter: Emitter.Usable_emitter.t; 
      mutable properties: Property.t list;
      logical_consequence: bool (** Is the emitted status automatically
				    infered? *) }

type inconsistent = private
    { valid: emitter_with_properties list; 
      invalid: emitter_with_properties list }

(** Type of known precise status of a property. *)
type status = private
  | Never_tried (** Nobody tries to verify the property *)
  | Best of 
      emitted_status (** The know precise status *)
    * emitter_with_properties list (** who attempt the verification 
				       under which hypotheses *)
  | Inconsistent of inconsistent (** someone says the property is valid and
				     someone else says it is invalid. *)

include Datatype.S with type t = status

val get: Property.t -> status
(** @return the most precise status and all its emitters. *)

(* ************************************************************************ *)
(** {2 Consolidated status} *)
(* ************************************************************************ *)

(** Consolidation of a property status according to the (consolidated) status of
    the hypotheses of the property. *)
module Consolidation: sig

  (** who do the job and, for each of them, who find which issues. *)
  type pending =
      Property.Set.t Emitter.Usable_emitter.Map.t Emitter.Usable_emitter.Map.t

  type consolidated_status = private
    | Never_tried
    (** Nobody tries to verify the property. 
	The argument is for internal use only *)

    | Considered_valid 
    (** Nobody succeeds to verifiy the property, but it is expected to be
	verified by another way (manual review, ...) *)

    | Valid of Emitter.Usable_emitter.Set.t
    (** The verification of this property is fully done. No work to
	do anymore for this property. The argument is the emitters who did the
	job. *)
	
    | Valid_under_hyp of pending
    (** The verification of this property is locally done, but it remains
	properties to verify in order to close the
	work. *)

    | Unknown of pending
    (** The verification of this property is not finished: the property itself
	remains to verify and it may also remain other pending properties. 
	NB: the pendings contains the property itself. *)

    | Invalid of Emitter.Usable_emitter.Set.t
    (** The verification of this property is fully done. All its hypotheses have
	been verified, but it is false: that is a true bug. *)

    | Invalid_under_hyp of pending
    (** This property is locally false, but it remains properties to verify in
	order to be sure that is a bug. *)

    | Invalid_but_dead of pending
    (** This property is locally false, but there is other bugs in hypotheses *)

    | Valid_but_dead of pending
    (** This property is locally true, but there is bugs in hypotheses *)

    | Unknown_but_dead of pending
    (** This property is locally unknown, but there is other bugs in 
	hypotheses *)

    | Inconsistent of string
  (** Inconsistency detected when computing the consolidated status.
      The string explains what is the issue for the end-user. *)
    
  include Datatype.S with type t = consolidated_status

  val get: Property.t -> t
  val get_conjunction: Property.t list -> t

end

(** Lighter version than Consolidation *)
module Feedback: sig
    
  (** Same constructor than Consolidation.t, without argument. *)
  type t =
    | Never_tried
    | Considered_valid 
    | Valid
    | Valid_under_hyp
    | Unknown
    | Invalid
    | Invalid_under_hyp
    | Invalid_but_dead
    | Valid_but_dead
    | Unknown_but_dead
    | Inconsistent

  val get: Property.t -> t
  val get_conjunction: Property.t list -> t

end
 
(** See the consolidated status of a property in a graph, which all its
    dependencies and their consolidated status. *)
module Consolidation_graph: sig
  type t
  val get: Property.t -> t
  val dump: t -> Format.formatter -> unit
end
  
(* ************************************************************************* *)
(** {2 Access to the registered properties} *)
(* ************************************************************************* *)

val iter: (Property.t -> unit) -> unit
val fold: (Property.t -> 'a -> 'a) -> 'a -> 'a

(* ************************************************************************* *)
(** {2 API not for casual users} *)
(* ************************************************************************* *)

val register: Property.t -> unit
(** Register the given property. It must not be already registered. *)

val register_property_add_hook: (Property.t -> unit) -> unit
(** add an hook that will be called for any newly registred property
    @since Neon-20130301 *)

val remove: Property.t -> unit
(** Remove the property deeply. Must be called only when removing the
    corresponding annotation. *)

val register_property_remove_hook: (Property.t -> unit) -> unit
(** Add and hook that will be called each time a property is removed.
    @since Neon-20130301 *)

val merge: old:Property.t list  -> Property.t list -> unit
(** [merge old new] registers properties in [new] which are not in [old] and
    removes properties in [old] which are not in [new]. *)

val automatically_proven: Property.t -> bool
(** Is the status of the given property only automatically handled by the 
    kernel? *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

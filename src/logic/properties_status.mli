(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

(** Status of properties.
    @since Boron-20100401 *)

open Cil_types
open Db_types

(*
  type dependencies
  val get_consolidated : dependency -> validity * (string * dependencies) list

  val S.dependencies : t -> dependencies
  val S.get_all: t -> (string * annotation_status * dependencies) list
  val Updater.set: t -> annotation_status -> dependencies list -> unit
  val Updater.update: t -> 
  (annotation_status * dependencies -> annotation_status * dependencies list) 
  -> annotation_status

*)

(** Signature for getting status of properties.
    @since Boron-20100401 *)
module type S = sig 
  
  type t 
    (** Type of considered annotations.
	@since Boron-20100401 *)

  val get_all: 
    ?who:Project.Computation.t list -> t -> 
    (annotation_status * Project.Computation.t) list
      (** For a given annotation, get all the status set by each plug-in.
	  @since Boron-20100401 *)    

  val get_all_status: 
    ?who:Project.Computation.t list -> t -> annotation_status list
    (** For a given annotation, get all the status set by each plug-in.
	@since Boron-20100401 *)    

  val get_all_states: 
    ?who:Project.Computation.t list -> t -> Project.Computation.t list
      (** For a given annotation, get all the states set by each plug-in.
	  @since Boron-20100401 *)    

  val strongest: 
    ?who:Project.Computation.t list -> t -> 
    annotation_status * Project.Computation.t
    (** Checks status consistency according to the following
        partial order: [Unknown < Maybe < True] and [Maybe < False]
	@return the most precise status available for the
        property according to the above partial order. 
        In case of consistent multiple status, the most recent 
        is returned.
	@since Boron-20100401 *)

  val pretty_all: Format.formatter -> t -> unit
    (** Pretty print all the status of a given annotation.
	@since Boron-20100401 *)

  val compare: t -> t -> int
    (** Compare two annotations.
	@since Boron-20100401 *)

  val equal: t -> t -> bool
    (** Equality between annotations.
	@since Boron-20100401 *)

  val hash: t -> int
    (** Hash for annotations.
	@since Boron-20100401 *)
    
  val add_dependency: Project.Computation.t -> Project.Computation.t -> unit
    (** see {!Computation.DASHTBL_OUTPUT.add_dependency}
	@since Boron-20100401 *)

  val self: Project.Computation.t
    (** @since Boron-20100401 *)

end    

(** Getting status of {!code_annotation}.
    @since Boron-20100401 *)
module CodeAnnotation: S with type t = code_annotation

(** Getting status of behaviors. It is computed as the disjunction of the status
    of its postconditions and assigns.
    @since Boron-20100401 *)
module Behavior: S with type t = kernel_function * kinstr * funbehavior

(** Getting status of predicates. 
    @since Boron-20100401 *)
module Predicate: S with type t = identified_predicate 

(** Getting status of assigns.
    @since Boron-20100401 *)
module Assigns: S with type t = 
  kernel_function * kinstr * funbehavior option * identified_term assigns list

(** @since Boron-20100401 *)
module Complete: S with type t = kernel_function * kinstr * string list

(** @since Boron-20100401 *)
module Disjoint: S with type t = kernel_function * kinstr * string list

(** Apply this functor in order to be able to modify status of annotations
    within a plug-in.
    @since Boron-20100401 *)
module Make_updater
  (P: sig 
     val name: string (** Plug-in name. *)
     val dependencies: Project.Computation.t list 
       (** Dependencies of the status modified by the plug-in. 
	   What are the states of the analysers which modify some status of
	   some properties? *)
   end) : 
sig

  (** @since Boron-20100401 *)
  module type S_ReadOnly = sig

    include S

    val get: t -> annotation_status
      (** Get the status of the given annotation set by this plug-in.
	  @since Boron-20100401 *)
  end
  
  (** @since Boron-20100401 *)
  module type S = sig

    include S_ReadOnly

    val set: t -> annotation_status -> unit
      (** Set the status of an annotation. 
	  @since Boron-20100401 *)

    val update: t -> (annotation_status -> annotation_status) -> 
      annotation_status
	(** Update the status of a given annotation according to the old
	    status.  
	    @since Boron-20100401 *)

  end

  (** Getting and modifying status of {!code_annotation}.
      @since Boron-20100401 *)
  module CodeAnnotation: S with type t = code_annotation

  (** Getting status of behaviors.
      @since Boron-20100401 *)
  module Behavior: S_ReadOnly with type t = 
    kernel_function *kinstr * funbehavior

  (** Getting and modifying status of predicates.
      @since Boron-20100401 *)
  module Predicate: S with type t = identified_predicate 

  (** Getting and modifying status of assigns clause.
      @since Boron-20100401 *)
  module Assigns: S with type t = 
    kernel_function * kinstr * funbehavior option * identified_term assigns list

  (** Getting and modifying status of complete behaviors clause.
      @since Boron-20100401 *)
  module Complete: S with type t = kernel_function * kinstr * string list

  (** Getting and modifying status of disjoint behaviors clause.
      @since Boron-20100401 *)
  module Disjoint: S with type t = kernel_function * kinstr * string list

end

(** @since Boron-20100401 *)
module type Generated = sig

  val get: kernel_function -> bool
    (** @since Boron-20100401 *)

  val set: kernel_function -> bool -> unit
    (** @since Boron-20100401 *)

  val get_state : kernel_function -> Project.Computation.t
    (** @since Boron-20100401 *)

  val self: Project.Computation.t
    (** @since Boron-20100401 *)

end

(** @since Boron-20100401 *)
module RTE_Status_Proxy : sig

  val self : Project.Computation.t

  val get_state : kernel_function -> Project.Computation.t

end

(** @since Boron-20100401 *)
module RTE_Signed_Generated : Generated

(** @since Boron-20100401 *)
module RTE_MemAccess_Generated : Generated

(** @since Boron-20100401 *)
module RTE_DivMod_Generated : Generated

(** @since Boron-20100401 *)
module RTE_DownCast_Generated : Generated

val get_all_status : 
  unit -> 
  (Project.Computation.t * 
     (kernel_function -> Project.Computation.t) *
     (kernel_function -> bool)) 
    list
  (** @since Boron-20100401 *)

(** @since Boron-20100401 *)
module Called_Precond_Generated : Generated

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

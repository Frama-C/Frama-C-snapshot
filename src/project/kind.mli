(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(* $Id: kind.mli,v 1.12 2009-01-23 15:02:59 uid568 Exp $ *)

(** Kind (roughly speaking, a type used as first-class-value for Project). 

    A kind may depend of others kinds and there are selections of kinds, i.e. a
    set of kinds dealing with their dependencies. *)

(** How to select the dependencies when a kind is added to a selection. 
    @plugin development guide *)
type how =
  | Do_Not_Select_Dependencies
      (** Only select the kind, and not its dependencies. *)
  | Select_Dependencies
      (** Select both the kind and its dependencies. *)
  | Only_Select_Dependencies 
      (** Select the kind's dependencies but not the kind itself. *)
      
(** Signature of a selection of kinds. *)
module type SELECTION = sig

  type kind
    (** Type of a kind. *)

  type t
    (** Type of a selection of kinds. *)

  val ty: t Type.t
    (** Type value of a selection. *)

  val empty : t
    (** The empty selection. *)

  val is_empty : t -> bool
    (** Whether the selection is empty. 
	@since Beryllium-20090601-beta1 *)

  val add: kind -> how -> t -> t
    (** [add k h s] adds [k] to [s]. [h] is used to specify how to deal with
	the dependencies of [k]. *)

  val singleton : kind -> how -> t
    (** [singleton k h] is equivalent to [add k h empty]. 
	@plugin development guide *)

  val remove: kind -> t -> t
    (** [remove k s] removes [k] of [s]. Each dependency [d] of [k] is also
	removed if [d] was not added using [add] (but only selected when [k]
	was added). *)

  val iter: (kind -> how -> unit) -> t -> unit
  val fold: (kind -> how -> 'a -> 'a) -> t -> 'a -> 'a

end

val version: string ref

(** Kind generator. *)
module Make
  (T: sig 
     type t (** Type used to build the first-class-value type. *)
     val dummy: t (** A dummy value. *)
     val name: string
       (** Name of the kind to generate (debugging purpose only). *)
     val kind_name: t -> string
       (** Name of a value of type [t]. *)
   end) : 
sig

  type t
    (** Type of kinds (roughly speaking, a second-order type) which the values
	have type [T.t] . *)

  val dummy: t
    (** A dummy kind. *)

  module Selection : SELECTION with type kind = t
    (** Selection for this type of kinds. *)

  val create: T.t -> t list -> t
    (** Create a new kind from a value with some dependencies. [create] is a
	bijection. 
	If ocaml assertions are checked, some verifications about the
	well-foundness of the dependency graph extended with this kind are
	performed (using the name of the kind, given by [T.kind_name]). *) 

  val value: t -> T.t
    (** Inverse of [create]. *)

  exception DependencyAlreadyExists of string * string
    (** May be raised by [add_dependency]. *)
   
  val add_dependency: t -> t -> unit
    (** [add_dependency k1 k2] indicates that the kind [k1] depends on the kind
	[k2], that is an action of the kind [k2] must be done before one of the
	kind [k1].
	@raise DependencyAlreadyExists if such a dependency already exists. *)

  val iter: (T.t -> 'a -> unit) -> 'a -> unit
    (** [iter f x] applies [f k x] for each kind [k] of type [t]. Order of
	applications is not specified. *)

  val apply_in_order:
    Selection.t -> Selection.t -> (t -> 'a -> 'a) -> 'a -> 'a
    (** [apply_in_order only except f x] folds [f] for of each kind of type [t] 
	(or for each kind specified by [only] and [except] if one of them is 
	non-empty), begining to [acc] and following a topological order of 
	kinds dependencies. *)

  val iter_in_order: 
    Selection.t -> Selection.t -> (T.t -> 'a -> unit) -> 'a -> unit
    (** [iter_in_order only except f x] applies [f v x] for each kind value [v] 
	of type [T.t] (or for each kind specified by [only] and 
	[except] if one of them is non-empty), following the same order as 
	apply_in_order. *)

  val fold_in_order:
    Selection.t -> Selection.t -> (T.t -> 'a -> 'a) -> 'a -> 'a
    (** [fold_in_order only except f acc] folds [f v x] for each kind value [v]
	of type [T.t] (or for each kind specified by [only] and 
	[except] if one of them is non-empty), begining to [acc] and following 
	the same order as apply_in_order. *)

  val number_of_applicants: Selection.t -> Selection.t -> int option
    (** [number_of_applicants only except] computes how many states would be
	impact by a folding. Return [None] if all states are impacted. *)

  val digest: unit -> Digest.t
    (** Checksum of kinds of type [t]. *)

  val dump_dependencies: 
    ?only:Selection.t -> ?except:Selection.t -> string -> unit
    (** Dump the dependencies of kinds of type [t] (or those of kinds specified
	by [only] and [except] if any) in a file (in dot format). *)

end

(*
  Local Variables:
  compile-command: "LC_ALL=C make -C ../.. -j"
  End:
*)

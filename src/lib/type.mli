(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
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

(** Types representation.

    @plugin development guide *)

type 'a t
  (** The representation of a type. 
      @plugin development guide *)

val id: 'a t -> int
  (** @return an unique identifier for the given type. *)

val name: 'a t -> string
  (** @return the name corresponding to the given type. *)

val name_of_id: int -> string
  (** @return the name of the given type id.
      @raise Not_found if the given id does not correspond to any type. *)

(** Types are ordered. *)

val equal: 'a t -> 'b t -> bool
val compare: 'a t -> 'b t -> int

(** Signature of an instance checker *)
module type INSTANCE_CHECKER = sig
  
  val is_instance_of: 'a t -> bool
    (** @return true if it's a subset of the polymorph type, false otherwise. *)

end

(* ****************************************************************************)
(** {2 Basic types} *)
(* ****************************************************************************)

exception AlreadyExists of string

val make: string -> 'a -> 'a t
  (** [make name repr] makes a new type from [name] and the
      representative [repr].
      @raise AlreadyExists if the given name is already used by another
      type. *)

val unit: unit t (** @plugin development guide *)
val bool: bool t
val int: int t
val float: float t
val char: char t 
val string: string t (** @plugin development guide *)

module Couple : sig
  
  include INSTANCE_CHECKER

  val realize: 'a t -> 'b t -> ('a * 'b) t
    (** @return the type of the couple [('a * 'b)]. *)

end

val couple: 'a t -> 'b t -> ('a * 'b) t
  (** Alias of [Couple.realize]. *)

module Option : sig

  include INSTANCE_CHECKER

  val realize: 'a t -> 'a option t
    (** @return the type of ['a option]. *)

end

val option: 'a t -> 'a option t
  (** Alias of [Option.realize] *)

(* ****************************************************************************)
(** {2 Functional types} *)
(* ****************************************************************************)

module Function : sig

  include INSTANCE_CHECKER

  val realize: 'a t -> 'b t -> ('a -> 'b) t
    (** @return the type of the function ['a -> 'b] *)

end

val func: 'a t -> 'b t -> ('a -> 'b) t
  (** Alias of {!Function.realize}.
      @plugin development guide *)

exception Not_functional

val split: 'a t -> 'b t * 'c t
  (** [split ty] gives the couple composed of input and ouput types of the 
      function type [ty].
      
      @raise Not_functional if [split ty] is applied despite of [ty] does not
      represent the type of a function. *)

(* ****************************************************************************)
(** {2 Polymorph types} *)
(* ****************************************************************************)

module type POLY_OUTPUT = sig
  
  include INSTANCE_CHECKER

  type 'a poly
    (** A polymorph type. *)
    
  val realize: 'a t -> 'a poly t
    (** @return the realization of the polymorph type applied to the given
	one. *)

  val elem_type: 'a poly t -> 'a t
    (** @return the type of elements contained in the polymorphic container. *)
    
end

module Polymorphic(D:sig type 'a t val make_name: string -> string end)
  : POLY_OUTPUT with type 'a poly = 'a D.t

module type POLY2_OUTPUT = sig
  
  include INSTANCE_CHECKER

  type ('a, 'b) poly2
    (** A doubly polymorph type. *)

  val realize: 'a t -> 'b t -> ('a, 'b) poly2 t
    (** @return the type of the polymporph one applied to both given types
	@raise AlreadyExists if the given name is already registered. *)

  val elem_types: ('a, 'b) poly2 t -> 'a t * 'b t
    (** @return the types of elements contained in the doubly poly polymorphic
	container. *)

end

module Polymorphic2
  (D:sig 
     type ('a, 'b) t 
     val make_name: string -> string -> string 
   end) : POLY2_OUTPUT with type ('a, 'b) poly2 = ('a, 'b) D.t

(* ****************************************************************************)
(** {2 Lists} *)
(* ****************************************************************************)

module List : POLY_OUTPUT with type 'a poly = 'a list

val list : 'a t -> 'a list t
  (** Alias of [List.realize]. *)
(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)

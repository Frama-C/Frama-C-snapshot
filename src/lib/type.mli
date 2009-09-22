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

(** Type value. A type value is a value representing a static ML monomorphic
    type.

    @plugin development guide *)

type 'a t
  (** Type of type values. For each (static) monomorphic type [ty], 
      a value of type [ty t] dynamically represents the type [ty]. Such a value
      is called a type value and should be unique for each static monomorphic
      type.
      @plugin development guide *)

(** Precedences used for generating the minimal number of parenthesis in
    combination with function {!par} below. *)
type precedence =
  | Basic
  | Call
  | Couple
  | List
  | NoPar

(** [par context myself fmt pp] is used to put parenthesis around the verbatim
    prints by [pp] according to the precedence [myself] of the verbatim and to
    the precedence [context] of the caller of the pretty printer. [fmt] is the
    output formatter.

    The typical use is the following:
    [let pretty_print p_caller fmt x =
    let pp fmt = Format.fprintf "..." ... x ... in
    let myself = Call in
    par p_caller Call fmt pp] *)
val par: 
  precedence -> precedence -> Format.formatter -> (Format.formatter -> unit) -> 
  unit

exception AlreadyExists of string
val register: 
  name:string -> value_name:string option ->
  ?pp:(precedence -> Format.formatter -> 'a -> unit) -> 
  ?varname:('a -> string) -> 
  'a -> 'a t
  (** [register ~name ~value_name abstract pp repr] registers a new type
      value from a value [repr] representing this type.
      - [repr] should not be a polymorphic value. For registering
      polymorphic types, use one of the dedicated functors. See module type
      {!POLYMORPHIC}.
      - [name] is a string representing the ML type
      (e.g. "int" or "int -> bool")
      - If [value_name = Some s], then [s] is a string denoting the type value
      itself (e.g. "Type.int" or "Type.func Type.int Type.bool").
      - If [value_name = None], then the type is a dynamically-registered
      type. In such a case, function {!get_dynamic} may be used.
      - [pp] is a pretty printer for values of this type value. In this pretty
      printer, each '@' must be preceded by another: '@@'.
      - [varname] is the prefix for the generated variable names of this
      type value (no generable variable by default).
      @raise AlreadyExists if the given name is already used by another
      type. *)

val name: 'a t -> string
  (** @return the name used to register the given type value. *)

val pp_value_name: 'a t -> precedence -> Format.formatter -> unit
  (** pretty print the value_name used to register the given type value. *)

val register_pp: 'a t -> (precedence -> Format.formatter -> 'a -> unit) -> unit
  (** [register_pp ty pp] sets the pretty printer of [ty] with [pp]. *)

exception NoPrinter of string
val pp: 'a t -> precedence -> Format.formatter -> 'a -> unit
  (** [pp ty fmt v] prints [v] on [fmt] according to the printer
      registered for the type value [ty]. 
      This function must be called from other pretty printers.
      However, the printed verbatim contains formatting materials and so should
      not be used for a direct output. Consider to use {!use_pp} instead.
      @raise NoPrinter if there is no printer registered for [ty]. *)

val varname: 'a t -> ('a -> string) option
  (** @return the prefix to use for generating variable names of this type
      value (as register with optional argument [varname] of
      {!register}). Return None if there is no known prefix. *)

val use_pp: 'a t -> Format.formatter -> 'a -> unit
  (** [use_pp ty fmt v] prints [v] on [fmt] according to the printer
      registered for the type value [ty] and to the formatting materials that
      it contains.
      @raise NoPrinter if there is no printer registered for [ty]. *)

exception Not_dynamic of string
type ty
val get_dynamic: string -> ty t
  (** @return the dynamic type value corresponding to the given name.
      @raise Not_dynamic if no dynamic type value was registered with this
      name. *)

val is_dynamic: 'a t -> bool
  (** @return true iff the type value was registered as abstract. *)

(* ****************************************************************************)
(** {2 Type values are comparable} *)
(* ****************************************************************************)

val equal: 'a t -> 'b t -> bool
val compare: 'a t -> 'b t -> int
val hash: 'a t -> int

(* ****************************************************************************)
(** {2 Basic type values} *)
(* ****************************************************************************)

val unit: unit t
  (** The type value corresponding to [unit]. 
      @plugin development guide*)

val bool: bool t
  (** The type value corresponding to [bool]. *)

val int: int t
  (** The type value corresponding to [int]. *)

val int32: int32 t
  (** The type value corresponding to [in32t]. 
      @since Beryllium-20090901 *)

val int64: int64 t
  (** The type value corresponding to [int64].
      @since Beryllium-20090901 *)

val nativeint: nativeint t
  (** The type value corresponding to [nativeint].
      @since Beryllium-20090901 *)

val float: float t
  (** The type value corresponding to [float]. *)

val char: char t 
  (** The type value corresponding to [char]. *)

val string: string t 
  (** The type value corresponding to [string].
      @plugin development guide *)

val formatter: Format.formatter t
  (** The type value corresponding to [Format.formatter]. *)

(* ****************************************************************************)
(** {2 Polymorphic type values} *)
(* ****************************************************************************)

val no_pp: precedence -> Format.formatter -> 'a -> unit
  (** @raise Assert_failure if fully applied. *)

(** For a polymorphic type value with one type variable, you must use an
    implementation of this signature. *)
module type POLYMORPHIC = sig

  type 'a poly
    (** Type of the polymorphic type. It must be instantiated before used. See
	function [instantiate] below. *)

  val instantiate: 'a t -> 'a poly t
    (** @return the monomorphic instantiation of the polymorph type with the
	given type value. For instance, if ['a poly = 'a list], then
	[instantiate int] returns the type value [int list]. *)
    
  val is_instance_of: 'a t -> bool
    (** @return [true] iff the given type value has been created from
	function [instantiate] above.
	For instance, [is_instance_of (instantiate int)] always returns [true]
	but [is_instance_of int] always returns [false]. *)

  val get_instance: 'a poly t -> 'a t
    (** [get_instance ty] returns the type value used to create the given
	monomorphic instantiation. *)

end

(** Generic implementation of polymorphic type value. *)
module Polymorphic
  (D:sig 
     val name: 'a t -> string
       (** How to build a name for each monomorphic instance of the type
	   value from the underlying type. *)
     val value_name: string
       (** The name of the built module. *)
     type 'a t 
       (** Static polymorphic type corresponding to its dynamic counterpart to
	   register. *)
     val repr: 'a -> 'a t
       (** How to make the representant of each monomorphic instance of the
	   polymorphic type value from an underlying representant. *)
     val pp: 
       (precedence -> Format.formatter -> 'a -> unit) ->
       precedence -> Format.formatter -> 'a t -> unit
       (** How to pretty print values with the help of a pretty printer for the
	   underlying type. 
	   Use {!no_pp} if you don't want to register a pretty printer. *)
   end)
  : POLYMORPHIC with type 'a poly = 'a D.t

(** Same as [POLYMORPHIC] for types with two type variables. *)
module type POLYMORPHIC2 = sig
  type ('a, 'b) poly
  val instantiate: 'a t -> 'b t -> ('a, 'b) poly t
  val is_instance_of: 'a t -> bool
  val get_instance: ('a, 'b) poly t -> 'a t * 'b t
end

module Polymorphic2
  (D:sig 
     val name: 'a t -> 'b t -> string 
     val value_name: string 
     type ('a, 'b) t 
     val repr: 'a -> 'b -> ('a, 'b) t
     val pp: 
       (precedence -> Format.formatter -> 'a -> unit) -> 
       (precedence -> Format.formatter -> 'b -> unit) ->
       precedence -> Format.formatter -> ('a,'b) t -> unit
   end) 
  : POLYMORPHIC2 with type ('a, 'b) poly = ('a, 'b) D.t

(* ****************************************************************************)
(** {2 Prebuilt polymorphic type values} *)
(* ****************************************************************************)

(** @since Beryllium-20090901 *)
module Ref : POLYMORPHIC with type 'a poly = 'a ref
val t_ref: 'a t -> 'a ref t 
  (** Alias of {!Parameters.instantiate}. 
      @since Beryllium-20090901 *)

module Option : POLYMORPHIC with type 'a poly = 'a option
val option: 'a t -> 'a option t (** Alias of {!Parameters.instantiate}. *)

module List : POLYMORPHIC with type 'a poly = 'a list
val list : 'a t -> 'a list t (** Alias of {!List.instantiate}. *)

module Couple : POLYMORPHIC2 with type ('a, 'b) poly = 'a * 'b
val couple: 'a t -> 'b t -> ('a * 'b) t (** Alias of {!Couple.instantiate}. *)

(** Same signature than {!POLYMORPHIC2} with possibility to specify a label for
    the function parameter. *)
module Function : sig
  type ('a, 'b) poly
  val instantiate: 
    ?label:string * (unit -> 'a) option -> 'a t -> 'b t -> ('a -> 'b) t
    (** Possibility to add a label for the parameter. 
	 - [~label:(p,None)] for a mandatory labelized parameter [p];
         - [~label:(p,Some f)] for an optional labelized parameter [p], 
           with default value [f ()]. *)
  val is_instance_of: 'a t -> bool
  val get_instance: 
    ('a, 'b) poly t -> 'a t * 'b t * (string * (unit -> 'a) option) option
end
val func: ?label:string * (unit -> 'a) option -> 'a t -> 'b t -> ('a -> 'b) t
  (** Alias of {!Function.instantiate}.
      @plugin development guide *)

val optlabel_func: string -> (unit -> 'a) -> 'a t -> 'b t -> ('a -> 'b) t
  (** [optlabel_func lab dft ty1 ty2] is an alias for 
      [func ~label:(lab, Some dft) ty1 ty2] *)

(* ****************************************************************************)
(** {2 Heterogeneous Tables} 
    These tables are safe to use but nevertheless not for casual users. *)
(* ****************************************************************************)

(** Heterogeneous tables indexed by string *)
module StringTbl : sig

  type 'a ty = 'a t
  type t 
    (** Type of heterogeneous (hash)tables indexed by string. 
	Type values ensure type safety. *)

  val create: int -> t
    (** [create n] creates a new table of initial size [n]. *)

  val add: t -> string -> 'a ty -> 'a -> 'a
    (** [add tbl s ty v] binds [s] to the value [v] in the table [tbl]. 
	@return the exact value stored in the table which is observationally
	equal to [v] but it deals better with dynamic types.
	@raise AlreadyExists if [s] is already bound in [tbl]. *)

  exception Unbound_value of string
  exception Incompatible_type of string
    (** @plugin development guide *)

  val find: t -> string -> 'a ty -> 'a
    (** [find tbl s ty] returns the binding of [s] in the table [tbl].
	@raise Unbound_value if [s] is not bound in [tbl].
	@raise Incompatible_Type if [ty] was not the type value used to add
	the binding of [s] in [tbl]. *)

end

(** Heterogeneous tables indexed by type. 
    Roughly the same signature that [Hashtbl.S]. *)
module TyTbl : sig
  type 'a ty = 'a t
  type 'a t
  val create: int -> 'a t
  val add: 'a t -> 'b ty -> 'a -> unit
  val find: 'a t -> 'b ty -> 'a
end

module Binding: sig
  val add: 'a t -> 'a -> string -> unit
    (** [add ty v var] binds the value [v] to the variable name [var].  Thus,
	[pp ty v] prints [var] and not use the standard pretty printer.  Very
	useful to pretty print values with no associated pretty printer. *)
  exception Name_already_exists of string
  val add_once: 'a t -> 'a -> string -> unit
    (** Same as function [add] above but raise the exception [Already_exists]
	if the binding previously exists *)
end

val no_obj: unit -> unit
  (** Deactivate all the black magic.
      Roughly, in this mode, nothing is done by this module. *)

(*
  Local Variables:
  compile-command: "LC_ALL=C make -C ../.."
  End:
*)

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

(** A datatype provides useful values for types. It is a high-level API on top
    of module {!Type}.
    @since Carbon-20101201
    @plugin development guide *)

(* ********************************************************************** *)
(** {2 Type declarations} *)
(* ********************************************************************** *)

(** Values associated to each datatype.
    Some others are provided directly in module {!Type}. *)
type 'a t = private
    { equal: 'a -> 'a -> bool;
      compare: 'a -> 'a -> int;
      hash: 'a -> int;
      copy: 'a -> 'a;
      internal_pretty_code: Type.precedence -> Format.formatter -> 'a -> unit;
      pretty_code: Format.formatter -> 'a -> unit;
      pretty: Format.formatter -> 'a -> unit;
      varname: 'a -> string;
      mem_project: (Project_skeleton.t -> bool) -> 'a -> bool }

(** A type with its type value. *)
module type Ty = sig
  type t
  val ty: t Type.t
end

(** All values associated to a datatype, excepted [copy]. *)
module type S_no_copy = sig

  include Ty

  val name: string
  (** Unique name of the datatype. *)

  val descr: t Descr.t
  (** Datatype descriptor. *)

  val packed_descr: Structural_descr.pack
  (** Packed version of the descriptor. *)

  val reprs: t list
  (** List of representents of the descriptor. *)

  val equal: t -> t -> bool
  (** Equality: same spec than [Pervasives.(=)]. *)

  val compare: t -> t -> int
  (** Comparison: same spec than [Pervasives.compare]. *)

  val hash: t -> int
  (** Hash function: same spec than [Hashtbl.hash]. *)

  val pretty_code: Format.formatter -> t -> unit
  (** Pretty print each value in an ML-like style: the result must be a valid
      OCaml expression. Only useful for journalisation. *)

  val internal_pretty_code: Type.precedence -> Format.formatter -> t -> unit
  (** Same spec than [pretty_code], but must take care of the precedence of the
      context in order to put parenthesis if required. See {!Type.par}. *)

  val pretty: Format.formatter -> t -> unit
  (** Pretty print each value in an user-friendly way. *)

  val varname: t -> string
  (** A good prefix name to use for an OCaml variable of this type. Only useful
      for journalisation. *)

  val mem_project: (Project_skeleton.t -> bool) -> t -> bool
(** [mem_project f x] must return [true] iff there is a value [p] of type
    [Project.t] in [x] such that [f p] returns [true]. *)

end

(** All values associated to a datatype. *)
module type S = sig
  include S_no_copy
  val copy: t -> t
(** Deep copy: no possible sharing between [x] and [copy x]. *)
end

(* ********************************************************************** *)
(** {2 Getters from a type value} *)
(* ********************************************************************** *)

val info: 'a Type.t -> 'a t
val equal: 'a Type.t -> 'a -> 'a -> bool
val compare: 'a Type.t -> 'a -> 'a -> int
val hash: 'a Type.t -> 'a -> int
val copy: 'a Type.t -> 'a -> 'a
val internal_pretty_code:
  'a Type.t -> Type.precedence -> Format.formatter -> 'a -> unit
val pretty_code: 'a Type.t -> Format.formatter -> 'a -> unit
val pretty: 'a Type.t -> Format.formatter -> 'a -> unit
val varname: 'a Type.t -> 'a -> string
val mem_project: 'a Type.t -> (Project_skeleton.t -> bool) -> 'a -> bool

(* ********************************************************************** *)
(** {2 Easy builders} *)
(* ********************************************************************** *)

val undefined: 'a -> 'b
(** Must be used if you don't want to implement a required function. 
    @plugin development guide *)

val identity: 'a -> 'a
(** Must be used if you want to implement a required function by [fun x ->
    x]. Only useful for implementing [rehash] and [copy]. 
    @plugin development guide *)

val from_compare: 'a -> 'a -> bool
(** Must be used for [equal] in order to implement it by [compare x y = 0]
    (with your own [compare] function). *)

val from_pretty_code: Format.formatter -> 'a -> unit
(** Must be used for [pretty] in order to implement it by [pretty_code]
    provided by the datatype from your own [internal_pretty_code] function. *)

val never_any_project: (Project_skeleton.t -> bool) -> 'a -> bool
(** Must be used for [mem_project] if values of your type does never contain
    any project. 
    @plugin development guide *)

val pp_fail: Type.precedence -> Format.formatter -> 'a -> unit
(** Must be used for [internal_pretty_code] if this pretty-printer must
    fail only when called. 
    @plugin development guide *)

(** Sub-signature of {!S}.
  @plugin development guide *)
module type Undefined = sig
  val structural_descr: Structural_descr.t
  val equal: 'a -> 'a -> bool
  val compare: 'a -> 'a -> int
  val hash: 'a -> int
  val rehash: 'a -> 'a
  val copy: 'a -> 'a
  val internal_pretty_code: Type.precedence -> Format.formatter -> 'a -> unit
  val pretty: Format.formatter -> 'a -> unit
  val varname: 'a -> string
  val mem_project: (Project_skeleton.t -> bool) -> 'a -> bool
end

(** Each values in these modules are undefined. The usual way to use it is:
    [module X: Datatype.S = struct
    include Undefined
    type t = ...
    let reprs = ...
    let name = ...
    let mem_project = ... (* Usually, Datatype.never_any_project *)
(* define only useful functions for this datatype *)
    end] *)
module Undefined: Undefined

(** Same as {!Undefined}, but the type is supposed to be marshalable by the
    standard OCaml way (in particular, no hash-consing or projects inside
    the type). 
    @plugin development guide *)
module Serializable_undefined: Undefined

(* ********************************************************************** *)
(** {2 Generic builders} *)
(* ********************************************************************** *)

(** Input signature of {!Make} and {!Make_with_collections}.
    Values to implement in order to get a datatype.
    Feel free to use easy builders (see above) for easy implementation. *)
module type Make_input = sig

  type t (** Type for this datatype *)

  val name: string
  (** Unique name for this datatype.
      If the name is a valid ocaml module name, then it must really corresponds
      to the module name you are defining by applying the functor.
      Otherwise, put the name you want as long as it does not clash with any
      other datatype name. *)

  val rehash: t -> t
  (** How to rehashconsed values. Must be {!identity} if you do not use
      hashconsing. Only useful for unmarshaling (use {!undefined} for
      unmarshable type). *)

  (** All the above operations have the same semantics than the corresponding
      value specified in module type {!S}. *)

  val structural_descr: Structural_descr.t
  val reprs: t list (** Must be non-empty.*)
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val hash: t -> int
  val copy: t -> t
  val internal_pretty_code: Type.precedence -> Format.formatter -> t -> unit
  val pretty: Format.formatter -> t -> unit
  val varname: t -> string
  val mem_project: (Project_skeleton.t -> bool) -> t -> bool

end

(** Generic datatype builder. 
    @plugin development guide *)
module Make(X: Make_input): S with type t = X.t

(** Additional info for building [Set], [Map] and [Hashtbl]. *)
module type Functor_info = sig
  val module_name: string
(** Must be a valid OCaml module name corresponding to the module name you are
    defining by applying the functor. *)
end

(** A standard OCaml set signature extended with datatype operations. *)
module type Set = sig
  include FCSet.S
  include S with type t := t
end

(** A standard OCaml map signature extended with datatype operations. *)
module type Map = sig

  include FCMap.S

  module Key: S with type t = key
  (** Datatype for the keys of the map. *)

  module Make(Data: S) : S with type t = Data.t t
(** Build a datatype of the map according to the datatype of values in the
    map. *)

end

(** Marshallable collectors with hashtbl-like interface. *)
module type Hashtbl_with_descr = sig
  include FCHashtbl.S
  val structural_descr: Structural_descr.t -> Structural_descr.t
end

(** A standard OCaml hashtbl signature extended with datatype operations. *)
module type Hashtbl = sig

  include Hashtbl_with_descr

  val make_type: 'a Type.t -> 'a t Type.t
  (** @since Fluorine-20130401 *)

  val memo: 'a t -> key -> (key -> 'a) -> 'a
  (** [memo tbl k f] returns the binding of [k] in [tbl]. If there is
      no binding, add the binding [f k] associated to [k] in [tbl] and return
      it.
      @since Nitrogen-20111001 *)

  module Key: S with type t = key
  (** Datatype for the keys of the hashtbl. *)

  module Make(Data: S) : S with type t = Data.t t
(** Build a datatype of the hashtbl according to the datatype of values in the
    hashtbl. *)

end

(** A datatype for a type [t] extended with predefined set, map and hashtbl
    over [t]. *)
module type S_with_collections = sig
  include S
  module Set: Set with type elt = t
  module Map: Map with type key = t
  module Hashtbl: Hashtbl with type key = t
end

(** Generic comparable datatype builder: functions [equal], [compare] and
    [hash] must not be {!undefined}. *)
module Make_with_collections(X: Make_input):
  S_with_collections with type t = X.t

(** Add sets, maps and hashtables modules to an existing datatype, provided the
    [equal], [compare] and [hash] functions are not {!undefined}.
    @since Oxygen-20120901 *)
module With_collections(X: S)(Info: Functor_info):
  S_with_collections with type t = X.t

(* ****************************************************************************)
(** {2 Predefined datatype} *)
(* ****************************************************************************)

module Unit: S_with_collections with type t = unit
val unit: unit Type.t
(** @plugin development guide *)

(** @plugin development guide *)
module Bool: S_with_collections with type t = bool
val bool: bool Type.t
(** @plugin development guide *)

(** @plugin development guide *)
module Int: S_with_collections with type t = int
val int: int Type.t
(** @plugin development guide *)

module Int32: S_with_collections with type t = int32
val int32: int32 Type.t

module Int64: S_with_collections with type t = int64
val int64: int64 Type.t

module Nativeint: S_with_collections with type t = nativeint
val nativeint: nativeint Type.t

module Float: S_with_collections with type t = float
val float: float Type.t

module Char: S_with_collections with type t = char
val char: char Type.t
(** @plugin development guide *)

(** @plugin development guide *)
module String: S_with_collections with type t = string
val string: string Type.t
(** @plugin development guide *)

module Formatter: S with type t = Format.formatter
val formatter: Format.formatter Type.t

module Big_int: S_with_collections with type t = Integer.t
val big_int: Big_int.t Type.t

(* ****************************************************************************)
(** {2 Generic functors for polymorphic types} *)
(* ****************************************************************************)

(** Output signature of {!Polymorphic}. *)
module type Polymorphic = sig
  include Type.Polymorphic
  module Make(T: S) : S with type t = T.t poly
(** Create a datatype for a monomorphic instance of the polymorphic type. *)
end

(** Functor for polymorphic types with only 1 type variable.
    @plugin development guide *)
module Polymorphic
  (P: sig
    include Type.Polymorphic_input
    val mk_equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val mk_compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val mk_hash: ('a -> int) -> 'a t -> int
    val map: ('a -> 'a) -> 'a t -> 'a t
    val mk_internal_pretty_code:
      (Type.precedence -> Format.formatter -> 'a -> unit) ->
      Type.precedence -> Format.formatter -> 'a t -> unit
    val mk_pretty:
      (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
    val mk_varname: ('a -> string) -> 'a t -> string
    val mk_mem_project:
      ((Project_skeleton.t -> bool) -> 'a -> bool) ->
      (Project_skeleton.t -> bool) -> 'a t -> bool
  end) :
  Polymorphic with type 'a poly = 'a P.t

(** Output signature of {!Polymorphic2}. *)
module type Polymorphic2 = sig
  include Type.Polymorphic2
  module Make(T1: S)(T2: S) : S with type t = (T1.t, T2.t) poly
end

(** Functor for polymorphic types with 2 type variables. 
    @plugin development guide *)
module Polymorphic2
  (P: sig
    include Type.Polymorphic2_input
    val mk_equal:
      ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t ->
      bool
    val mk_compare:
      ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('a, 'b) t -> ('a, 'b) t -> int
    val mk_hash: ('a -> int) -> ('b -> int) -> ('a, 'b) t -> int
    val map: ('a -> 'a) -> ('b -> 'b) -> ('a, 'b) t -> ('a, 'b) t
    val mk_internal_pretty_code:
      (Type.precedence -> Format.formatter -> 'a -> unit) ->
      (Type.precedence -> Format.formatter -> 'b -> unit) ->
      Type.precedence -> Format.formatter -> ('a, 'b) t -> unit
    val mk_pretty:
      (Format.formatter -> 'a -> unit) -> (Format.formatter -> 'b -> unit) ->
      Format.formatter -> ('a, 'b) t -> unit
    val mk_varname: ('a -> string) -> ('b -> string) -> ('a, 'b) t -> string
    val mk_mem_project:
      ((Project_skeleton.t -> bool) -> 'a -> bool) ->
      ((Project_skeleton.t -> bool) -> 'b -> bool) ->
      (Project_skeleton.t -> bool) -> ('a, 'b) t -> bool
  end) :
  Polymorphic2 with type ('a, 'b) poly = ('a, 'b) P.t

(** Output signature of {!Polymorphic3}. 
    @since Oxygen-20120901 *)
module type Polymorphic3 = sig
  include Type.Polymorphic3
  module Make(T1:S)(T2:S)(T3:S) : S with type t = (T1.t, T2.t, T3.t) poly
end

(** Functor for polymorphic types with 3 type variables. 
    @since Oxygen-20120901 *)
module Polymorphic3
  (P: sig
    include Type.Polymorphic3_input
    val mk_equal:
      ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('c -> 'c -> bool) ->
      ('a, 'b, 'c) t -> ('a, 'b, 'c) t ->
      bool
    val mk_compare:
      ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('c -> 'c -> int) ->
      ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> int
    val mk_hash: 
      ('a -> int) -> ('b -> int) -> ('c -> int) -> ('a, 'b, 'c) t -> int
    val map: 
      ('a -> 'a) -> ('b -> 'b) -> ('c -> 'c) -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
    val mk_internal_pretty_code:
      (Type.precedence -> Format.formatter -> 'a -> unit) ->
      (Type.precedence -> Format.formatter -> 'b -> unit) ->
      (Type.precedence -> Format.formatter -> 'c -> unit) ->
      Type.precedence -> Format.formatter -> ('a, 'b, 'c) t -> unit
    val mk_pretty:
      (Format.formatter -> 'a -> unit) -> 
      (Format.formatter -> 'b -> unit) ->
      (Format.formatter -> 'c -> unit) ->
      Format.formatter -> ('a, 'b, 'c) t -> unit
    val mk_varname: 
      ('a -> string) -> ('b -> string) -> ('c -> string) -> 
      ('a, 'b, 'c) t -> string
    val mk_mem_project:
      ((Project_skeleton.t -> bool) -> 'a -> bool) ->
      ((Project_skeleton.t -> bool) -> 'b -> bool) ->
      ((Project_skeleton.t -> bool) -> 'c -> bool) ->
      (Project_skeleton.t -> bool) -> ('a, 'b, 'c) t -> bool
  end) :
  Polymorphic3 with type ('a, 'b, 'c) poly = ('a, 'b, 'c) P.t

(** Output signature of {!Polymorphic4}. 
    @since Oxygen-20120901 *)
module type Polymorphic4 = sig
  include Type.Polymorphic4
  module Make(T1:S)(T2:S)(T3:S)(T4:S) 
    : S with type t = (T1.t, T2.t, T3.t, T4.t) poly
end

(** Functor for polymorphic types with 4 type variables. 
    @since Oxygen-20120901 *)
module Polymorphic4
  (P: sig
    include Type.Polymorphic4_input
    val mk_equal:
      ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> 
      ('c -> 'c -> bool) -> ('d -> 'd -> bool) ->
      ('a, 'b, 'c, 'd) t -> ('a, 'b, 'c, 'd) t ->
      bool
    val mk_compare:
      ('a -> 'a -> int) -> ('b -> 'b -> int) -> 
      ('c -> 'c -> int) -> ('d -> 'd -> int) ->
      ('a, 'b, 'c, 'd) t -> ('a, 'b, 'c, 'd) t -> int
    val mk_hash: 
      ('a -> int) -> ('b -> int) -> ('c -> int) -> ('d -> int) -> 
      ('a, 'b, 'c, 'd) t -> int
    val map: 
      ('a -> 'a) -> ('b -> 'b) -> ('c -> 'c) -> ('d -> 'd) ->
      ('a, 'b, 'c, 'd) t -> ('a, 'b, 'c, 'd) t
    val mk_internal_pretty_code:
      (Type.precedence -> Format.formatter -> 'a -> unit) ->
      (Type.precedence -> Format.formatter -> 'b -> unit) ->
      (Type.precedence -> Format.formatter -> 'c -> unit) ->
      (Type.precedence -> Format.formatter -> 'd -> unit) ->
      Type.precedence -> Format.formatter -> ('a, 'b, 'c, 'd) t -> unit
    val mk_pretty:
      (Format.formatter -> 'a -> unit) -> 
      (Format.formatter -> 'b -> unit) ->
      (Format.formatter -> 'c -> unit) ->
      (Format.formatter -> 'd -> unit) ->
      Format.formatter -> ('a, 'b, 'c, 'd) t -> unit
    val mk_varname: 
      ('a -> string) -> ('b -> string) -> ('c -> string) -> ('d -> string) -> 
      ('a, 'b, 'c, 'd) t -> string
    val mk_mem_project:
      ((Project_skeleton.t -> bool) -> 'a -> bool) ->
      ((Project_skeleton.t -> bool) -> 'b -> bool) ->
      ((Project_skeleton.t -> bool) -> 'c -> bool) ->
      ((Project_skeleton.t -> bool) -> 'd -> bool) ->
      (Project_skeleton.t -> bool) -> ('a, 'b, 'c, 'd) t -> bool
  end) :
  Polymorphic4 with type ('a, 'b, 'c, 'd) poly = ('a, 'b, 'c, 'd) P.t

(* ****************************************************************************)
(** {2 Predefined functors for polymorphic types} *)
(* ****************************************************************************)

module Poly_pair: Polymorphic2 with type ('a, 'b) poly = 'a * 'b

(** @plugin development guide *)
module Pair(T1: S)(T2: S): S with type t = T1.t * T2.t
module Pair_with_collections(T1: S)(T2: S)(Info: Functor_info):
  S_with_collections with type t = T1.t * T2.t
val pair: 'a Type.t -> 'b Type.t -> ('a * 'b) Type.t

module Poly_ref: Polymorphic with type 'a poly = 'a ref

(** @plugin development guide *)
module Ref(T: S) : S with type t = T.t ref
val t_ref: 'a Type.t -> 'a ref Type.t

module Poly_option: Polymorphic with type 'a poly = 'a option
module Option(T: S) : S with type t = T.t option

(** @since Nitrogen-20111001 *)
module Option_with_collections(T:S)(Info: Functor_info):
  S_with_collections with type t = T.t option

val option: 'a Type.t -> 'a option Type.t

module Poly_list: Polymorphic with type 'a poly = 'a list

(** @plugin development guide *)
module List(T: S) : S with type t = T.t list

module List_with_collections(T:S)(Info:Functor_info):
  S_with_collections with type t = T.t list
(** @since Fluorine-20130401 *)

val list: 'a Type.t -> 'a list Type.t
(** @plugin development guide *)

module Poly_array: Polymorphic with type 'a poly = 'a array
(** @since Neon-20130301 *)

module Array(T: S) : S with type t = T.t array
(** @since Neon-20130301 *)

module Array_with_collections(T:S)(Info:Functor_info):
  S_with_collections with type t = T.t array
(** @since Neon-20130301 *)

val array: 'a Type.t -> 'a array Type.t
(** @since Neon-20130301 *)


module Poly_queue: Polymorphic with type 'a poly = 'a Queue.t
val queue: 'a Type.t -> 'a Queue.t Type.t
module Queue(T: S) : S with type t = T.t Queue.t

module Triple(T1: S)(T2: S)(T3: S): S with type t = T1.t * T2.t * T3.t
val triple: 'a Type.t -> 'b Type.t -> 'c Type.t -> ('a * 'b * 'c) Type.t
(** @since Fluorine-20130401 *)

module Triple_with_collections(T1: S)(T2: S)(T3: S)(Info: Functor_info):
  S_with_collections with type t = T1.t * T2.t * T3.t

(** @since Nitrogen-20111001 *)
module Quadruple(T1: S)(T2: S)(T3: S)(T4:S): 
  S with type t = T1.t * T2.t * T3.t * T4.t
val quadruple: 
  'a Type.t -> 'b Type.t -> 'c Type.t -> 'd Type.t -> ('a * 'b * 'c * 'd) Type.t
(** @since Fluorine-20130401 *)

(** @since Nitrogen-20111001 *)
module Quadruple_with_collections
  (T1: S)(T2: S)(T3: S)(T4:S)(Info: Functor_info):
  S_with_collections with type t = T1.t * T2.t * T3.t * T4.t

(** @plugin development guide *)
module Function
  (T1: sig include S val label: (string * (unit -> t) option) option end)
  (T2: S)
  : S with type t = T1.t -> T2.t

val func:
  ?label:string * (unit -> 'a) option -> 'a Type.t ->
  'b Type.t ->
  ('a -> 'b) Type.t
(** @plugin development guide *)

val optlabel_func:
  string -> (unit -> 'a) -> 'a Type.t -> 'b Type.t -> ('a -> 'b) Type.t
  (** [optlabel_func lab dft ty1 ty2] is equivalent to
      [func ~label:(lab, Some dft) ty1 ty2] *)

val func2:
  ?label1:string * (unit -> 'a) option -> 'a Type.t ->
  ?label2:string * (unit -> 'b) option -> 'b Type.t ->
  'c Type.t ->
  ('a -> 'b -> 'c) Type.t
(** @plugin development guide *)

val func3:
  ?label1:string * (unit -> 'a) option -> 'a Type.t ->
  ?label2:string * (unit -> 'b) option -> 'b Type.t ->
  ?label3:string * (unit -> 'c) option -> 'c Type.t ->
  'd Type.t ->
  ('a -> 'b -> 'c -> 'd) Type.t
(** @plugin development guide *)

val func4:
  ?label1:string * (unit -> 'a) option -> 'a Type.t ->
  ?label2:string * (unit -> 'b) option -> 'b Type.t ->
  ?label3:string * (unit -> 'c) option -> 'c Type.t ->
  ?label4:string * (unit -> 'd) option -> 'd Type.t ->
  'e Type.t ->
  ('a -> 'b -> 'c -> 'd -> 'e) Type.t

module Set
  (S: FCSet.S)(E: S with type t = S.elt)(Info : Functor_info):
  Set with type t = S.t and type elt = E.t

module Map
  (M: FCMap.S)(Key: S with type t = M.key)(Info: Functor_info) :
  Map with type 'a t = 'a M.t and type key = M.key and module Key = Key

module Hashtbl
  (H: Hashtbl_with_descr)(Key: S with type t = H.key)(Info : Functor_info):
  Hashtbl with type 'a t = 'a H.t and type key = H.key and module Key = Key

module type Sub_caml_weak_hashtbl = sig
  type data
  type t
  val create: int -> t
  val add: t -> data -> unit
end

module Caml_weak_hashtbl(D: S): sig
  include Weak.S with type t = Weak.Make(D).t and type data = D.t
  module Datatype: S with type t = t
end

module Weak(W: Sub_caml_weak_hashtbl)(D: S with type t = W.data) :
  S with type t = W.t

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

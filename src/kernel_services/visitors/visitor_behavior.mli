(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(** Operations on visitor behaviors.
    @since 20.0-Calcium.
*)

open Cil_types

type t
(** How the visitor should behave in front of mutable fields: in
    place modification or copy of the structure. This type is abstract.
    Use one of the two values below in your classes.
    @plugin development guide *)

val inplace: unit -> t
(** In-place modification. Behavior of the original cil visitor.
    @plugin development guide *)

val copy: Project.t -> t
(** Makes fresh copies of the mutable structures.
    - preserves sharing for varinfo.
    - makes fresh copy of varinfo only for declarations. Variables that are
      only used in the visited AST are thus still shared with the original
      AST. This allows for instance to copy a function with its
      formals and local variables, and to keep the references to other
      globals in the function's body.
      @plugin development guide *)

val refresh: Project.t -> t
(** Makes fresh copies of the mutable structures and provides fresh id
    for the structures that have ids. Note that as for {!copy_visit}, only
    varinfo that are declared in the scope of the visit will be copied and
    provided with a new id.
*)

(** true iff the behavior provides fresh id for copied structs with id.
    Always [false] for an inplace visitor.
*)
val is_fresh: t -> bool

(** true iff the behavior is a copy behavior. *)
val is_copy: t -> bool

val get_project: t -> Project.t option

(** Reset operations on behaviors, allows to reset the tables associated to a given
    kind of AST elements. If you use fresh instances of visitor for each round of
    transformation, you should not need this module. In place modifications
    do not need this at all.

    [Reset.ast_element vis] resets the tables associated to the considered type of
    AST elements in [vis]. For example for {!Cil_types.varinfo}: [Reset.varinfo vis].

    @since 20.0-Calcium
    @plugin development guide
*)
module Reset: sig
  val varinfo: t -> unit
  val compinfo: t -> unit
  val enuminfo: t -> unit
  val enumitem: t -> unit
  val typeinfo: t -> unit
  val stmt: t -> unit
  val logic_info: t -> unit
  val logic_type_info: t -> unit
  val fieldinfo: t -> unit
  val model_info: t -> unit
  val logic_var: t -> unit
  val kernel_function: t -> unit
  val fundec: t -> unit
end

module type Get = sig
  val varinfo: t -> varinfo -> varinfo
  val compinfo: t -> compinfo -> compinfo
  val enuminfo: t -> enuminfo -> enuminfo
  val enumitem: t -> enumitem -> enumitem
  val typeinfo: t -> typeinfo -> typeinfo
  val stmt: t -> stmt -> stmt
  val logic_info: t -> logic_info -> logic_info
  val logic_type_info: t -> logic_type_info -> logic_type_info
  val fieldinfo: t -> fieldinfo -> fieldinfo
  val model_info: t -> model_info -> model_info
  val logic_var: t -> logic_var -> logic_var
  val kernel_function: t -> kernel_function -> kernel_function
  val fundec: t -> fundec -> fundec
end

(** Get operations on behaviors, allows to get the representative of an
    AST element in the current state of the visitor.

    [Get.ast_element vis e] with [e] of type [ast_element] gets the
    representative of [e] in [vis]. For example for {!Cil_types.varinfo}:
    [Get.varinfo vis vi].

    @since 20.0-Calcium
    @plugin development guide
*)
module Get: Get

(** Get operations on behaviors, allows to get the original representative of
    an element of the {b new} AST in the curent state of the visitor.

    [Get_orig.ast_element vis new_e] with [new_e] of type [ast_element] gets the
    original representative of [new_e] in [vis]. For example for
    {!Cil_types.varinfo}: [Get_orig.varinfo vis new_vi].

    @since 20.0-Calcium
    @plugin development guide
*)
module Get_orig: Get

(** Memo operations on behaviors, allows to get a binding in the new project
    for the given AST element, creating one if it does not already exists.

    [Memo.ast_element vis e] with [e] of type [ast_element] tries to find a
    binding to a [e] in the new project created using [vis] in the current
    state, if it does not exist this binding is created. For example for
    {!Cil_types.varinfo}: [Memo.varinfo vis vi].

    @since 20.0-Calcium
    @plugin development guide
*)
module Memo: Get

module type Set = sig
  val varinfo: t -> varinfo -> varinfo -> unit
  val compinfo: t -> compinfo -> compinfo -> unit
  val enuminfo: t -> enuminfo -> enuminfo -> unit
  val enumitem: t -> enumitem -> enumitem -> unit
  val typeinfo: t -> typeinfo -> typeinfo -> unit
  val stmt: t -> stmt -> stmt -> unit

  val logic_info: t -> logic_info -> logic_info -> unit
  val logic_type_info:
    t -> logic_type_info -> logic_type_info -> unit
  val fieldinfo: t -> fieldinfo -> fieldinfo -> unit
  val model_info: t -> model_info -> model_info -> unit
  val logic_var: t -> logic_var -> logic_var -> unit
  val kernel_function:
    t -> kernel_function -> kernel_function -> unit
  val fundec: t -> fundec -> fundec -> unit
end

(** Set operations on behaviors, allows to change the representative of a
    given AST element in the current state of the visitor. Use with care
    (i.e. makes sure that the old one is not referenced anywhere in the
    AST, or sharing will be lost).

    [Set.ast_element vis e s] with [e] and [s] of type [ast_element] changes
    the representative of [e] to [s] in [vis]. For example, for
    {!Cil_types.varinfo}: [Set.varinfo vis vi new_representative].

    @since 20.0-Calcium
    @plugin development guide
*)
module Set: Set

(** Set operations on behaviors related to original representatives, allows to
    change the reference of an element of the {b new} AST in the current state
    of the visitor. Use with care.

    [Set.ast_element vis e s] with [e] and [s] of type [ast_element] changes
    the original representative of [e] to [s] in [vis]. For example, for
    {!Cil_types.varinfo}: [Set_orig.varinfo vis vi new_original_repr].

    @since 20.0-Calcium
*)
module Set_orig: Set

module type Unset = sig
  val varinfo: t -> varinfo -> unit
  val compinfo: t -> compinfo -> unit
  val enuminfo: t -> enuminfo -> unit
  val enumitem: t -> enumitem -> unit
  val typeinfo: t -> typeinfo -> unit
  val stmt: t -> stmt -> unit

  val logic_info: t -> logic_info -> unit
  val logic_type_info: t -> logic_type_info -> unit
  val fieldinfo: t -> fieldinfo -> unit
  val model_info: t -> model_info -> unit
  val logic_var: t -> logic_var -> unit
  val kernel_function: t -> kernel_function -> unit
  val fundec: t -> fundec -> unit
end


(** Operations to remove the entry associated to a given AST element in the
    current state of the visitor. Use with care (i.e. make sure that you will
    never visit again this element in the same visiting context).

    [Unset.ast_element vis e] with [e] of type [ast_element] removes the
    representative of [e] in the [ast_element] table of [vis]. For example,
    for {!Cil_types.varinfo}: [Unset.varinfo vis vi].

    @since 20.0-Calcium
*)
module Unset: Unset

(** Operations to remove the entry associated to a given element of the {b new}
    AST in the current state of the visitor. Use with care.

    [Unset_orig.ast_element vis e] with [e] of type [ast_element] removes the
    original representative of [e] in the [ast_element] table of [vis]. For
    example, for {!Cil_types.varinfo}: [Unset_orig.varinfo vis vi].

    @since 20.0-Calcium
*)
module Unset_orig: Unset

(** Iter operations on the table of a given type of AST elements.

    [Iter.ast_element vis f], iterates [f] over each pair of [ast_element]
    registered in [vis]. The [ast_element] in the old AST is presented to [f]
    first (that is, [f] looks like: [let f old_e new_e = ...]. For example for
    {!Cil_types.varinfo}: [Iter.varinfo vis (fun old_vi new_vi -> ())].

    @since 20.0-Calcium
*)
module Iter: sig
  val varinfo:
    t -> (varinfo -> varinfo -> unit) -> unit
  val compinfo:
    t -> (compinfo -> compinfo -> unit) -> unit
  val enuminfo:
    t -> (enuminfo -> enuminfo -> unit) -> unit
  val enumitem:
    t -> (enumitem -> enumitem -> unit) -> unit
  val typeinfo:
    t -> (typeinfo -> typeinfo -> unit) -> unit
  val stmt:
    t -> (stmt -> stmt -> unit) -> unit
  val logic_info:
    t -> (logic_info -> logic_info -> unit) -> unit
  val logic_type_info:
    t -> (logic_type_info -> logic_type_info -> unit) -> unit
  val fieldinfo:
    t -> (fieldinfo -> fieldinfo -> unit) -> unit
  val model_info:
    t -> (model_info -> model_info -> unit) -> unit
  val logic_var:
    t -> (logic_var -> logic_var -> unit) -> unit
  val kernel_function:
    t -> (kernel_function -> kernel_function -> unit) -> unit
  val fundec:
    t -> (fundec -> fundec -> unit) -> unit
end

(** Fold operations on table of a given type of AST elements.

    [Fold.ast_element vis f], folds [f] over each pair of [ast_element]
    registered in [vis]. The [ast_element] in the old AST is presented to [f]
    first (that is, [f] looks like: [let f old_e new_e acc = ...]. For example
    for {!Cil_types.varinfo}:
    [Fold.varinfo vis (fun old_vi new_vi acc -> ... )].

    @since 20.0-Calcium
*)
module Fold: sig
  val varinfo:
    t -> (varinfo -> varinfo -> 'a -> 'a) -> 'a -> 'a
  val compinfo:
    t -> (compinfo -> compinfo -> 'a -> 'a) -> 'a -> 'a
  val enuminfo:
    t -> (enuminfo -> enuminfo -> 'a -> 'a) -> 'a -> 'a
  val enumitem:
    t -> (enumitem -> enumitem -> 'a -> 'a) -> 'a -> 'a
  val typeinfo:
    t -> (typeinfo -> typeinfo -> 'a -> 'a) -> 'a -> 'a
  val stmt:
    t -> (stmt -> stmt -> 'a -> 'a) -> 'a -> 'a
  val logic_info:
    t -> (logic_info -> logic_info -> 'a -> 'a) -> 'a -> 'a
  val logic_type_info:
    t ->
    (logic_type_info -> logic_type_info -> 'a -> 'a) -> 'a -> 'a
  val fieldinfo:
    t -> (fieldinfo -> fieldinfo -> 'a -> 'a) -> 'a -> 'a
  val model_info:
    t -> (model_info -> model_info -> 'a -> 'a) -> 'a -> 'a
  val logic_var:
    t -> (logic_var -> logic_var -> 'a -> 'a) -> 'a -> 'a
  val kernel_function:
    t ->
    (kernel_function -> kernel_function -> 'a -> 'a) -> 'a -> 'a
  val fundec:
    t -> (fundec -> fundec -> 'a -> 'a) -> 'a -> 'a
end


(**/**)

(** For INTERNAL USE only *)

val cfile: t -> file -> file
val cinitinfo: t -> initinfo -> initinfo
val cblock: t -> block -> block
val cfunspec: t -> funspec -> funspec
val cfunbehavior: t -> funbehavior -> funbehavior
val cidentified_term: t -> identified_term -> identified_term
val cidentified_predicate: t -> identified_predicate -> identified_predicate
val cexpr: t -> exp -> exp
val ccode_annotation: t -> code_annotation -> code_annotation

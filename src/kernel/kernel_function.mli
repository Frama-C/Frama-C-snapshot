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

(** Operations to get info from a kernel function. This module does
    not give access to information about the set of all the registered kernel
    functions (like iterators over kernel functions). This kind of operations is
    stored in module {!Globals.Functions}.

    @plugin development guide *)

open Cil_types

(* ************************************************************************* *)
(** {2 Kernel functions are comparable and hashable} *)
(* ************************************************************************* *)

include Datatype.S_with_collections with type t = kernel_function
val id: t -> int
val auxiliary_kf_stmt_state: State.t

(* ************************************************************************* *)
(** {2 Searching} *)
(* ************************************************************************* *)

exception No_Statement
val find_first_stmt : t -> stmt
  (** Find the first statement in a kernel function.
      @raise No_Statement if there is no first statement for the given
      function. *)

val find_return : t -> stmt
  (** Find the return statement of a kernel function.
      @raise No_Statement is there is no return statement for the given
      function.
      @modify Nitrogen-20111001 may raise No_Statement*)

val find_label : t -> string -> stmt ref
  (** Find a given label in a kernel function.
      @raise Not_found if the label does not exist in the given function. *)

val clear_sid_info: unit -> unit
(** removes any information related to statements in kernel functions.
    ({i.e.} the table used by the function below).
    - Must be called when the Ast has silently changed
    (e.g. with an in-place visitor) before calling one of
    the functions below
    - Use with caution, as it is very expensive to re-populate the table. *)

val find_from_sid : int -> stmt * t
  (** @return the stmt and its kernel function from its identifier.
      Complexity: the first call to this function is linear in the size of
      the cil file.
      @raise Not_found if there is no statement with such an identifier. *)

val find_englobing_kf : stmt -> t
  (** @return the function to which the statement belongs. Same
      complexity as [find_from_sid] 
      @raise Not_found if the given statement is not correctly registered *)

val find_enclosing_block: stmt -> block
  (** @return the innermost block to which the given statement belongs. *)

val find_all_enclosing_blocks: stmt -> block list
  (** same as above, but returns all enclosing blocks, starting with the
      innermost one. *)

val blocks_closed_by_edge: stmt -> stmt -> block list
  (** [blocks_closed_by_edge s1 s2] returns the (possibly empty)
      list of blocks that are closed when going from [s1] to [s2].
      @raise Invalid_argument if the statements do not belong to the
      same function or [s2] is not a successor of [s1] in the cfg.
      @since Carbon-20101201 *)

val stmt_in_loop: t -> stmt -> bool
  (** [stmt_in_loop kf stmt] is [true] iff [stmt] strictly 
      occurs in a loop of [kf].
      @since Oxygen-20120901 *)

val find_enclosing_loop: t -> stmt -> stmt
  (** [find_enclosing_loop kf stmt] returns the statement corresponding
      to the innermost loop containing [stmt] in [kf]. If [stmt] itself is
      a loop, returns [stmt]
      @raise Not_found if [stmt] is not part of a loop of [kf]
      @since Oxygen-20120901 *)

val find_syntactic_callsites : t -> (t * stmt) list
  (** [callsites f] collect the statements where [f] is called.  Same
      complexity as [find_from_sid].
      @return a list of [f',s] where function [f'] calls [f] at statement
      [stmt].
      @since Carbon-20110201 *)

(* ************************************************************************* *)
(** {2 Checkers} *)
(* ************************************************************************* *)

val is_definition : t -> bool
val returns_void : t -> bool

(* ************************************************************************* *)
(** {2 Getters} *)
(* ************************************************************************* *)

val dummy: unit -> t
(** @plugin development guide *)

val get_vi : t -> varinfo
val get_id: t -> int
val get_name : t -> string
val get_type : t -> typ
val get_return_type : t -> typ
val get_location: t -> Cil_types.location
val get_global : t -> global
val get_formals : t -> varinfo list
val get_locals : t -> varinfo list

exception No_Definition
val get_definition : t -> fundec
  (** @raise No_Definition if the given function is not a definition.
      @plugin development guide *)

(* ************************************************************************* *)
(** {2 Membership of variables} *)
(* ************************************************************************* *)

val is_formal: varinfo -> t -> bool
  (** @return [true] if the given varinfo is a formal parameter of the given
      function. If possible, use this function instead of
      {!Ast_info.Function.is_formal}. *)

val get_formal_position: varinfo -> t -> int
(** [get_formal_position v kf] is the position of [v] as parameter of [kf].
    @raise Not_found if [v] is not a formal of [kf]. *)

val is_local : varinfo -> t -> bool
(** @return [true] if the given varinfo is a local variable of the given
    function. If possible, use this function instead of
    {!Ast_info.Function.is_local}. *)

val is_formal_or_local: varinfo -> t -> bool
  (** @return [true] if the given varinfo is a formal parameter or a local
      variable of the given function.
      If possible, use this function instead of
      {!Ast_info.Function.is_formal_or_local}. *)

val get_called : exp -> t option
  (** Returns the static call to function [expr], if any.
      [None] means a dynamic call through function pointer. *)

(* ************************************************************************* *)
(** {2 Collections} *)
(* ************************************************************************* *)

(** Hashtable indexed by kernel functions and dealing with project.
    @plugin development guide *)
module Make_Table(Data: Datatype.S)(Info: State_builder.Info_with_size):
  State_builder.Hashtbl with type key = t and type data = Data.t

(** Set of kernel functions. *)
module Hptset : Hptset.S
  with type elt = kernel_function
  and type 'a shape = 'a Hptmap.Shape(Cil_datatype.Kf).t


(* ************************************************************************* *)
(** {2 Setters}

    Use carefully the following functions. *)
(* ************************************************************************* *)

val register_stmt: t -> stmt -> block list -> unit
  (** Register a new statement in a kernel function, with the list of
      blocks that contain the statement (innermost first). *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

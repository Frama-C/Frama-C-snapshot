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

(* $Id: kernel_function.mli,v 1.19 2009-02-23 12:52:19 uid562 Exp $ *)

(** Operations on kernel function.
    @plugin development guide *)

open Cil_types
open Db_types

(* ************************************************************************* *)
(** {2 Kernel functions are comparable and hashable} *)
(* ************************************************************************* *)

type t = kernel_function
val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int

(** Datatype for a kernel function.
    @plugin development guide *)
module Datatype: Project.Datatype.S with type t = kernel_function

(* ************************************************************************* *)
(** {2 Searching} *)
(* ************************************************************************* *)

val find_from_sid : int -> stmt * t
  (** Return the stmt and its kernel function from its identifier.
      Complexity: the first call to this function is linear in the size of
      the cil file.
      @raise Not_found if there is no statement with such an identifier.
      @plugin development guide *)

exception No_Statement
val find_first_stmt : t -> stmt
  (** Find the first statement in a kernel function.
      @raise No_Statement if there is no first statement for the given
      function. *)

val find_return : t -> stmt
  (** Find the return statement of a kernel function. *)

val find_label : t -> string -> stmt ref
  (** Find a given label in a kernel function.
      @raise Not_found if the label does not exist in the given function. *)

val find_enclosing_block: stmt -> block
  (** returns the innermost block to which the given statement belongs. *)

val find_all_enclosing_blocks: stmt -> block list
  (** same as above, but returns all enclosing blocks, starting with the
      innermost one.
   *)

val blocks_closed_by_edge: stmt -> stmt -> block list
  (** [edge_exits_block kf s1 s2] returns the (possibly empty)
      list of blocks that are closed when going from [s1] to [s2].
      @raise Invalid_argument if the statements do not belong to the
      same function or are not adjacent in the cfg.
   *)

(* ************************************************************************* *)
(** {2 Checkers} *)
(* ************************************************************************* *)

val is_definition : t -> bool
val returns_void : t -> bool

(* ************************************************************************* *)
(** {2 Getters} *)
(* ************************************************************************* *)

val dummy: unit -> t
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
  (** @raise No_Definition if the given function is not a definition. *)

(* ************************************************************************* *)
(** {2 Membership of variables} *)
(* ************************************************************************* *)

val is_formal: varinfo -> t -> bool
  (** Return [true] if the given varinfo is a formal parameter of the given
      function. If possible, use this function instead of
      {!Ast_info.Function.is_formal}. *)

val is_local : varinfo -> t -> bool
  (** Return [true] if the given varinfo is a local variable of the given
      function. If possible, use this function instead of
      {!Ast_info.Function.is_local}. *)

val is_formal_or_local: varinfo -> t -> bool
  (** Return [true] if the given varinfo is a formal parameter or a local
      variable of the given function.
      If possible, use this function instead of
      {!Ast_info.Function.is_formal_or_local}. *)

(* ************************************************************************* *)
(** {2 Specifications} *)
(* ************************************************************************* *)

val get_spec: t -> funspec
val postcondition : t -> predicate named
val precondition: t -> predicate named

val populate_spec: (t -> unit) ref
  (** Should not be used by casual users. *)

(* ************************************************************************* *)
(** {2 Collections} *)
(* ************************************************************************* *)

(** Hashtable indexed by kernel functions and dealing with project.
    @plugin development guide *)
module Make_Table(Data:Project.Datatype.S)(Info:Signature.NAME_SIZE_DPDS):
  Computation.HASHTBL_OUTPUT with type key = t and type data = Data.t

(** Set of kernel functions. *)
module Set : sig

  include Set.S with type elt = t

  module Datatype : Project.Datatype.S with type t = t
    (** Datatype corresponding to a set of kernel functions. *)

  val pretty : Format.formatter -> t -> unit
    (** Pretty print a set of kernel functions. *)

end

(** Datatype for a queue of kernel functions. *)
module Queue: sig
  module Datatype: Project.Datatype.S with type t = kernel_function Queue.t
end

(* ************************************************************************* *)
(** {2 Setters}

    Use carefully the following functions. *)
(* ************************************************************************* *)

val register_stmt: t -> stmt -> block list -> unit
  (** Register a new statement in a kernel function, with the list of
      blocks that contain the statement (innermost first)
   *)

(* ************************************************************************* *)
(** {2 Pretty printer} *)
(* ************************************************************************* *)

val pretty_name : Format.formatter -> t -> unit
  (** Print the name of a kernel function. *)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)

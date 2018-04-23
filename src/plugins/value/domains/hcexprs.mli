(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

(** Hash-consed expressions and lvalues. *)

open Cil_types

type unhashconsed_exprs = private E of exp | LV of lval
(** lvalues are never stored under a constructor [E], only [LV] *)

(** Raised when the replacement of an lvalue in an expression is impossible. *)
exception NonExchangeable

(** Reason of the replacement of an lvalue [lval]: [Modified] means that the
    value of [lval] has been modified (in which case &lval is unchanged), and
    [Deleted] means that [lval] is no longer in scope (in which case &lval
    raises the NonExchangeable error). *)
type kill_type = Modified | Deleted

module E: Datatype.S with type t = unhashconsed_exprs

(** Datatype + utilities functions for hashconsed exprsessions. *)
module HCE: sig
  include Datatype.S_with_collections
  val self: State.t
  val pretty_debug: t Pretty_utils.formatter

  val id: t -> int

  (** Conversions between type [t] and Cil lvalues and expressions. *)

  val of_lval: lval -> t
  val of_exp: exp -> t

  val get: t -> unhashconsed_exprs
  val to_exp: t -> exp
  val to_lval: t -> lval option
  val is_lval: t -> bool


  (** Replaces all occurrences of the lvalue [late] by the expression [heir].
      @raise NonExchangeable if the replacement is not feasible. *)
  val replace: kill_type -> late:lval -> heir:exp -> t -> t
end


(** Hashconsed sets of symbolic expressions. *)
module HCESet: Hptset.S with type elt = HCE.t

(* Sets of lvalues that appear in an expression. The [addr] field gathers the
   lvalues [lv] appearing as addresses &lv, while the [read] field gathers the
   lvalues whose value is read during the evaluation of the expression.  *)
type lvalues = {
  read : HCESet.t;
  addr : HCESet.t;
}

(* Empty sets of lvalues. *)
val empty_lvalues: lvalues

(** [syntactic_lvalues e] returns the set of lvalues that appear in the
    expression [e]. This is used by the equality domain: the expression [e] will
    be removed from an equality if a lvalue from [syntactic_lvalues e] is
    removed. This function only computes the first lvalues of the expression,
    and does not go through the lvalues (for the expression t[i]+1, only the
    lvalue t[i] is returned). *)
val syntactic_lvalues: Cil_types.exp -> lvalues


(** Maps from symbolic expressions to their memory dependencies, expressed as a
    {!Locations.Zone.t}. *)
module HCEToZone: sig
  include Datatype.S_with_collections

  val empty: t

  val add: HCE.t -> Locations.Zone.t -> t -> t
  val remove: HCE.t -> t -> t

  val union: t -> t -> t
  val inter: t -> t -> t
  val is_included: t -> t -> bool

  val find: HCE.t -> t -> Locations.Zone.t
  (** @raise Not_found if the symbolic expression is not in the map. *)

  val find_default: HCE.t -> t -> Locations.Zone.t
  (** returns the empty set when the key is not bound *)

end


(** Maps froms {!Base.t} to set of {!HCE.t}. *)
module BaseToHCESet: sig
  include Datatype.S_with_collections

  val empty: t

  val add: Base.t -> HCESet.t -> t -> t
  val remove: Base.t -> t -> t

  val union: t -> t -> t
  val inter: t -> t -> t

  val find: Base.t -> t -> HCESet.t
  (** @raise Not_found if the base is not in the map. *)

  val find_default: Base.t -> t -> HCESet.t
  (** returns the empty set when the key is not bound *)
end

(**************************************************************************)
(*                                                                        *)
(*  This file is part of the Frama-C's E-ACSL plug-in.                    *)
(*                                                                        *)
(*  Copyright (C) 2012-2019                                               *)
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

(** Type system which computes the smallest C type that may contain all the
    possible values of a given integer term or predicate. Also compute the
    required casts. It is based on interval inference of module {!Interval}.

    It implement Figure 4 of J. Signoles' JFLA'15 paper "Rester statique pour
    devenir plus rapide, plus précis et plus mince".

    Example: consider a variable [x] of type [int] and a variable [y] of type
    char on a (strange) architecture in which values of type [int] belongs to
    the interval [[-128;127]] and values of type [char] belongs to the interval
    [[-32;31]], while there are no other integral types. Then here are some
    information computed from the term [1+(x+1)/(y-64)] by the type system:
    1. [x+1] must be a GMP (because of the potential overflow)
    2. consequently [x], which is an [int], must be coerced into a GMP and the
    same for the number 1 in this addition.
    3. [y-64] can be computed in an [int] (because the result belongs to the
    interval [[-96;-33]]).
    4. [(x+1)/(y-64)] must be a GMP operation because the numerator is a
    GMP (see 1.). Consequently [y-64] must be coerced into a GMP too. However,
    the result belongs to the interval [[-3;3]] and thus can be safely coerced
    to an [int].
    5. Consequently the addition of the toplevel term [1+(x+1)/(y-64)] can
    safely be computed in [int]: its result belongs to [[-2;4]]. *)

open Cil_types

(******************************************************************************)
(** {2 Datatypes} *)
(******************************************************************************)

(** Possible types infered by the system. *)

type number_ty = private
  | C_integer of ikind
  | C_float of fkind
  | Gmpz
  | Rational
  | Real
  | Nan

module Datatype: Datatype.S_with_collections with type t = number_ty

(** {3 Smart constructors} *)

val c_int: number_ty
val ikind: ikind -> number_ty
val fkind: fkind -> number_ty
val gmpz: number_ty
val rational: number_ty
val nan: number_ty

(** {3 Useful operations over {!number_ty}} *)

exception Not_a_number
val typ_of_number_ty: number_ty -> typ
(** @return the C type corresponding to an {!number_ty}. That is [Gmp.z_t ()]
    for [Gmpz], [Real.t ()] for [Real] and [TInt(ik, [[]])] for [Ctype ik].
    @raise Not_a_number in case of [Nan]. *)

val number_ty_of_typ: typ -> number_ty
(** Reverse of [typ_of_number_ty] *)

val join: number_ty -> number_ty -> number_ty
(** {!number_ty} is a join-semi-lattice if you do not consider [Other]. If
    there is no [Other] in argument, this function computes the join of this
    semi-lattice. If one of the argument is {!Other}, the function assumes that
    the other argument is also {!Other}. In this case, the result is [Other]. *)

(******************************************************************************)
(** {2 Typing} *)
(******************************************************************************)

val type_term: use_gmp_opt:bool -> ?ctx:number_ty -> term -> unit
(** Compute the type of each subterm of the given term in the given context. If
    [use_gmp_opt] is false, then the conversion to the given context is done
    even if -e-acsl-gmp-only is set. *)

val type_named_predicate: ?must_clear:bool -> predicate -> unit
(** Compute the type of each term of the given predicate.
    Set {!must_clear} to false in order to not reset the environment. *)

val clear: unit -> unit
(** Remove all the previously computed types. *)

(** {3 Getters}

    Below, the functions assume that either {!type_term} or
    {!type_named_predicate} has been previously computed for the given term or
    predicate. *)

val get_number_ty: term -> number_ty
(** @return the infered type for the given term. *)

val get_integer_op: term -> number_ty
(** @return the infered type for the top operation of the given term.
    It is meaningless to call this function over a non-arithmetical/logical
    operator. *)

val get_integer_op_of_predicate: predicate -> number_ty
(** @return the infered type for the top operation of the given predicate. *)

val get_typ: term -> typ
(** Get the type which the given term must be generated to. *)

val get_op: term -> typ
(** Get the type which the operation on top of the given term must be generated
    to. *)

val get_cast: term -> typ option
(** Get the type which the given term must be converted to (if any). *)

val get_cast_of_predicate: predicate -> typ option
(** Like {!get_cast}, but for predicates. *)

val unsafe_set: term -> ?ctx:number_ty -> number_ty -> unit
(** Register that the given term has the given type in the given context (if
    any). No verification is done. *)

(*****************************************************************************)
(** {2 Typing/types-related utils} *)
(*****************************************************************************)

val ty_of_interv: ?ctx:number_ty -> Interval.t -> number_ty
(* Compute the smallest type (bigger than [int]) which can contain the whole
   interval. It is the \theta operator of the JFLA's paper. *)

val typ_of_lty: logic_type -> typ
(** @return the C type that correponds to the given logic type. *)

(******************************************************************************)
(** {2 Internal stuff} *)
(******************************************************************************)

val compute_quantif_guards_ref
    : (predicate -> logic_var list -> predicate ->
       (term * relation * logic_var * relation * term) list) ref
(** Forward reference. *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

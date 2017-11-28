(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(** Interval inference for terms.

    Compute the smallest interval that contains all the possible values of a
    given integer term. The interval of C variables is directly infered from
    their C type. The interval of logic variables must be registered from
    outside before computing the interval of a term containing such variables
    (see module {!Interval.Env}).

    It implement Figure 3 of J. Signoles' JFLA'15 paper "Rester statique pour
    devenir plus rapide, plus précis et plus mince".

    Example: consider a variable [x] of type [int] on a (strange) architecture
    in which values of type [int] belongs to the interval \[-128;127\] and a
    logic variable [y] which was registered in the environment with an interval
    \[-32;31\]. Then here are the intervals computed from the term
    [1+(x+1)/(y-64)]:
    1. x in \[128;127\];
    2. x+1 in \[129;128\];
    3. y in \[-32;31\];
    4. y-64 in \[-96;-33\];
    5. (x+1)/(y-64) in \[-3;3\];
    6. 1+(x+1)/(y-64) in \[-2;4\]

    Note: this is a partial wrapper on top of [Ival.t], to which most
    functions are delegated. *)

(* ************************************************************************** *)
(** {3 Useful operations on intervals} *)
(* ************************************************************************** *)

exception Not_an_integer

val interv_of_typ: Cil_types.typ -> Ival.t
(** @return the smallest interval which contains the given C type.
    @raise Not_an_integer if the given type is not an integral type. *)

(* ************************************************************************** *)
(** {3 Environment for interval computations} *)
(* ************************************************************************** *)

(** Environment which maps logic variables to intervals. This environment must
    be extended from outside. *)
module Env: sig
  val clear: unit -> unit
  val add: Cil_types.logic_var -> Ival.t -> unit
end

(* ************************************************************************** *)
(** {3 Inference system} *)
(* ************************************************************************** *)

val infer: Cil_types.term -> Ival.t
(** [infer t] infers the smallest possible integer interval which the values
    of the term can fit in.
    @raise Not_an_integer if the type of the term is not a subtype of
    [Linteger]. *)

(*
Local Variables:
compile-command: "make"
End:
*)

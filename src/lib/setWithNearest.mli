(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            *)
(*                                                                        *)
(*  Copyright 2002 Institut National de Recherche en Informatique et      *)
(*  en Automatique.  All rights reserved.  This file is distributed       *)
(*  under the terms of the GNU Library General Public License, with       *)
(*  the special exception on linking described in file ../LICENSE.        *)
(*                                                                        *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux        *)
(*                        énergies alternatives).                         *)
(**************************************************************************)

(** Set equiped with operations to find nearest element greater or less
    than the required value *)

module type S = sig
  include Datatype.Set
  val nearest_elt_le: elt -> t -> elt
  val nearest_elt_ge: elt -> t -> elt
end
(** Output signature of the functor {!SetWithNearest.Make}. *)

module Make (Ord : Datatype.S) : S with type elt = Ord.t
(** Functor building an implementation of the set structure
   given a totally ordered type. *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

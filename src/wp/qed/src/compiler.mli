(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(* -------------------------------------------------------------------------- *)
(** Qed Compiler to Ground                                                    *)
(* -------------------------------------------------------------------------- *)

open Logic

module Make(T:Term) :
sig

  type symbol =
    | Fun of T.signature * T.Fun.t
    | Val of T.tau * T.term

  type lookup = {
    make_field : Syntax.id -> sort -> T.Field.t ;
    lookup_field : Syntax.id -> T.Field.t -> bool ;
    lookup_typedef : Syntax.id -> T.tau ;
    lookup_datatype : T.ADT.t -> T.tau option ;
    lookup_symbol  : Syntax.id -> symbol ;
  }

  val cc_tau : lookup -> Syntax.id list -> Syntax.t -> T.tau
  val cc_sig : lookup -> Syntax.t list -> Syntax.t -> T.signature
  val cc_def : lookup -> Syntax.arg list -> Syntax.t option -> Syntax.e -> T.signature * T.term
  val cc_exp : lookup -> Syntax.e -> T.tau * T.term

end


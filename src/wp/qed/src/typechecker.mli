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
(** Qed Syntax Typechecker                                                    *)
(* -------------------------------------------------------------------------- *)

open Logic

module Make(ADT:Data)(Field:Field) :
sig

  type t (* Not yet generalized type *)
  type env (* Typing environment *)

  type tau = (Field.t,ADT.t) datatype
  type signature = (Field.t,ADT.t) funtype

  type lookup = {
    make_field : Syntax.id -> sort -> Field.t ;
    lookup_field : Syntax.id -> Field.t -> bool ;
    lookup_typedef : Syntax.id -> tau ;
    lookup_datatype : ADT.t -> tau option ;
    lookup_signature : Syntax.id -> signature ;
  }

  val signature : lookup -> Syntax.t list -> Syntax.t -> signature

  val create : lookup -> Syntax.arg list -> env
  val typecheck : env -> Syntax.e -> Syntax.t option -> tau

  val final_degree : env -> int
  val final_type   : env -> t -> tau
  val final_node   : env -> int -> tau
  val final_fields : env -> int -> Field.t list

end

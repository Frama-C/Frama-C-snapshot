(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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

(** Logic Declarations for ACSL *)

open Ctypes
open Cil_types
open LogicId
open LogicTau
open LogicLang

(* -------------------------------------------------------------------------- *)
(** {2 Types} *)
(* -------------------------------------------------------------------------- *)
  
val record_of : compinfo -> id
val field_of : fieldinfo -> field

val tau_of_ctype : typ -> tau
val tau_of_object : c_object -> tau
val tau_of_logic_type : logic_type -> tau

(* -------------------------------------------------------------------------- *)
(** {2 Sub-Types} *)
(* -------------------------------------------------------------------------- *)

val is_int : c_int -> id
val to_int : c_int -> id

val has_ctype : typ -> term -> pred
val has_object : c_object -> term -> pred

(* -------------------------------------------------------------------------- *)
(** {2 Equality} *)
(* -------------------------------------------------------------------------- *)

val eq_ctype : typ -> term -> term -> pred
val eq_object : c_object -> term -> term -> pred
val eq_logic_type : logic_type -> term -> term -> pred

(* -------------------------------------------------------------------------- *)
(** {2 Declarator Helper} *)
(* -------------------------------------------------------------------------- *)

module type Compiler =
sig
  type key
  type data
  val name : string
  val reprs : data list (* Project+Datatype requirement *)
  val compile : key -> data
end

module Register(H : Datatype.Hashtbl)(C : Compiler with type key = H.key) :
sig

  val obtain : C.key -> C.data
    (** Projectified and memoized [C.compile].  The compiler can not
	be recursive.  An exception during the compilation is
	re-raised each time its value is requested. *)

end

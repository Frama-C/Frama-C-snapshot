(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU General Public                   *)
(*  License version 2, as published by the Free Software Foundation.      *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU General Public License version 2 for more details         *)
(*  (enclosed in the file GPL).                                           *)
(*                                                                        *)
(**************************************************************************)

(* $Id: jc_envset.mli,v 1.19 2008/03/20 16:05:13 moy Exp $ *)

module type OrderedHashedType =
sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
end

open Jc_env

module StringSet : Set.S with type elt = string

module StringMap : Map.S with type key = string 

val get_unique_name : ?local_names:StringSet.t -> string -> string

val is_pointer_type : Jc_env.jc_type -> bool

val is_embedded_field : Jc_env.field_info -> bool

module VarSet : Set.S with type elt = var_info

module StructSet : Set.S with type elt = struct_info

module StructMap : Map.S with type key = struct_info

module VariantSet : Set.S with type elt = variant_info

module VariantMap : Map.S with type key = variant_info

module ExceptionSet : Set.S with type elt = exception_info

module ExceptionMap : Map.S with type key = exception_info

module StructOrd : OrderedHashedType with type t = struct_info

module VariantOrd : OrderedHashedType with type t = variant_info

module FieldOrd : OrderedHashedType with type t = field_info

module FieldSet : Set.S with type elt = field_info

module FieldMap : Map.S with type key = field_info

module FieldTable : Hashtbl.S with type key = field_info

module FieldOrVariantOrd : OrderedHashedType with type t = field_or_variant_info

module LogicLabelSet : Set.S with type elt = logic_label

(*
Local Variables: 
compile-command: "make -C .. bin/jessie.byte"
End: 
*)


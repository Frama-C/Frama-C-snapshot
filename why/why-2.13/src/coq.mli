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

(*i $Id: coq.mli,v 1.28 2008/02/05 12:10:49 marche Exp $ i*)

open Cc
open Vcg

val reset : unit -> unit

val push_decl : Logic_decl.t -> unit

val push_validation : string -> cc_type -> validation -> unit

val push_program : string -> cc_type -> cc_functional_program -> unit

val push_parameter : string -> cc_type -> unit

val output_file : string -> unit

(* exported for the GUI *)

val prefix_id : Ident.t -> string
val pprefix_id : Ident.t -> string
val infix_relation : Ident.t -> string


val print_predicate_v8 : Format.formatter -> Logic.predicate -> unit
val print_cc_type_v8 : Format.formatter -> Cc.cc_type -> unit
val print_binder_v8 : Format.formatter -> Cc.cc_binder -> unit
val print_binder_type_v8 : Format.formatter -> Cc.cc_binder -> unit
val print_term_v8 : Format.formatter -> Logic.term -> unit

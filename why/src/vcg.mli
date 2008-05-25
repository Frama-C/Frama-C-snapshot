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

(*s Verification Conditions Generator. *)

open Types
open Logic
open Cc

(***
val vcg : 
  string -> (Loc.position * predicate) cc_term -> obligation list * validation
***)

val logs : Log.t ref
val log_print_function : (Format.formatter -> sequent -> unit) ref

(* obligations from the WP *)

val vcg_from_wp : 
  (* fun loc, fun ids fun name, fun behavior, wp *)
  Loc.floc -> string -> string -> string -> Ast.assertion -> Logic_decl.obligation list * proof

(* functions to be reused in module [Coq] *)

val annotated_if : Ident.t -> cc_binder list -> bool
val annotation_if : cc_binder list -> Ident.t * predicate


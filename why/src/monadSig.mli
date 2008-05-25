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

open Logic
open Types
open Ast
open Cc
open Env

module type S = 
sig 

  (*s Translation of types *)
  
  val trad_scheme_v : Rename.t -> local_env -> type_v scheme -> cc_type
  val trad_type_v : Rename.t -> local_env -> type_v -> cc_type
  val trad_type_c : Rename.t -> local_env -> type_c -> cc_type
  val trad_imp_type  : Rename.t -> local_env -> type_v -> cc_type
  val trad_type_in_env : Rename.t -> local_env -> Ident.t -> cc_type
    
  val exn_arg_type : Ident.t -> cc_type
    
    (*s Basic monadic operators. They operate over values of type [interp] i.e.
      functions building a [cc_term] given a renaming structure. *)
    
  type interp = Rename.t -> (Loc.position * predicate) cc_term
    
    (*s The [unit] operator encapsulates a term in a computation. *)
    
  type result = 
    | Value of term
    | Exn of Ident.t * term option
	
  val unit : typing_info -> result -> interp
    
    (*s [compose k1 e1 e2] evaluates computation [e1] of type [k1] and 
      passes its result (actually, a variable naming this result) to a
      second computation [e2]. [apply] is a variant of [compose] where
      [e2] is abstracted (typically, a function) and thus must be applied
      to its input. *)

  val compose : 
    typing_info -> interp -> typing_info -> (Ident.t -> interp) -> interp
  val apply   : 
    typing_info -> interp -> typing_info -> (Ident.t -> interp) -> interp

    (*s Monadic operator to raise and handle exceptions *)

  val exn : typing_info -> Ident.t -> term option -> interp

  val handle : 
    typing_info -> interp -> typing_info -> 
    (exn_pattern * (Ident.t -> interp)) list -> interp

    (*s Other monadic operators. *)

  val cross_label : string -> interp -> interp

  val insert_pre : local_env -> precondition -> interp -> interp

  val insert_many_pre : local_env -> precondition list -> interp -> interp

  val abstraction : typing_info -> precondition list -> interp -> interp

  val fresh : Ident.t -> (Ident.t -> interp) -> interp

    (*s Well-founded recursion. *)

  val wfrec : 
    variant -> precondition list -> typing_info -> (interp -> interp) -> interp

  val wfrec_with_binders : 
    cc_binder list -> 
    variant -> precondition list -> typing_info -> (interp -> interp) -> interp

    (*s Table for recursive functions' interpretation *)

  val is_rec : Ident.t -> bool
  val find_rec : Ident.t -> interp
  val with_rec : Ident.t -> interp -> ('a -> 'b) -> 'a -> 'b
end

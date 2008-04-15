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
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: typing.mli,v 1.24 2008/11/05 14:03:18 filliatr Exp $ i*)

(*s This module realizes type and effect inference *)

open Types
open Ptree
open Ast
open Env

val typef : Label.t -> local_env -> parsed_program -> typed_expr

val check_for_not_mutable : Loc.position -> type_v -> unit

val is_pure_type : type_v -> bool
val is_pure_type_v : type_v -> bool

val type_c_of_typing_info : assertion list -> typing_info -> type_c
val typing_info_of_type_c : 
  Loc.position -> local_env -> label -> type_c -> typing_info

(*
val gmake_node : 
  Loc.position ->
    local_env ->
    label -> (* userlabel *)
    label ->
    ?post:postcondition option ->
    typing_info t_desc ->
    type_v -> Effect.t -> typed_expr
*)

val conj : postcondition option -> postcondition option -> postcondition option



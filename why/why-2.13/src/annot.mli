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

(*i $Id: annot.mli,v 1.12 2008/02/05 12:10:49 marche Exp $ i*)

open Env
open Types
open Logic
open Ast

(*s Maximum of two postconditions. [sup q q'] is made of postconditions
    from [q], when not the default postcondition, and from [q'] otherwise. *)

val sup : postcondition option -> postcondition option -> postcondition option

(*s automatic postcondition for a loop body, i.e. [I and var < var@L] *)

val while_post_block :
  local_env -> predicate asst option -> variant -> typed_program -> 
  postcondition

(*s [normalise p] inserts some automatic annotation on the outermost
    construct of [p] *)

val normalize : typed_program -> typed_program


(*s Useful functions to change the program tree or its annotations,
    to be reused in [Wp] *)




val purify : typed_program -> typed_program

val is_result_eq : predicate -> term option


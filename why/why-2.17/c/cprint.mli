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

(*i $Id: cprint.mli,v 1.13 2008/11/05 14:03:13 filliatr Exp $ i*)

(* Pretty-printer for normalized AST *)

open Format
open Clogic
open Cast

val term_unop : Clogic.term_unop -> string

val term_binop : Clogic.term_binop -> string

val relation : Clogic.relation -> string

val nexpr : formatter -> nexpr -> unit

val nstatement : formatter -> nstatement -> unit

val ndecl : formatter -> ndecl located -> unit

val nfile : formatter -> nfile -> unit

val npredicate :  formatter -> npredicate -> unit

val nterm : formatter -> nterm -> unit

val nfunctions : formatter -> unit

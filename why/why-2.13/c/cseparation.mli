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

open Cast
open Info


val not_alias : Loc.position ->
  Cast.nctype Clogic.nterm -> Cast.nctype Clogic.nterm -> Cast.npredicate 

val file : nfile -> unit

val valid_for_type : 
  ?fresh:bool -> Loc.position -> string -> 
    nterm -> npredicate

val predicate : Info.why_type -> Cast.nctype Clogic.npredicate -> unit

val separation :
  Loc.position -> 
  Info.var_info -> Info.var_info -> Ctypes.ctype Clogic.npredicate

val in_struct :  nterm -> Info.var_info -> nterm 


val assoctype : why_type -> (zone *zone) list -> why_type

val funct : fun_info list -> unit

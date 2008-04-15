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

(*i $Id: rename.mli,v 1.11 2008/11/05 14:03:18 filliatr Exp $ i*)

(* Abstract type for renamings 
 * 
 * Records the names of the mutables objets (ref, arrays) at the different
 * moments of the evaluation, called dates
 *)

type t

type date = string


val empty_ren : t
val update    : t -> date -> Ident.t list -> t
    (* assign new names for the given variables, associated to a new date *)
val next      : t -> Ident.t list -> t
    (* assign new names for the given variables, associated to a new
     * date which is generated from an internal counter *)
val push_date : t -> date -> t
    (* put a new date on top of the stack *)

val valid_date   : date -> t -> bool
val current_date : t -> date
val all_dates    : t -> date list

val current_var  : t -> Ident.t      -> Ident.t
val current_vars : t -> Ident.t list -> (Ident.t * Ident.t) list
    (* gives the current names of some variables *)

val avoid : t -> Ident.set -> t
val fresh : t -> Ident.t -> Ident.t * t
val fresh_many : t -> Ident.t list -> (Ident.t * Ident.t) list * t
    (* introduces new names to avoid and renames some given variables *)

val var_at_date  : t -> date -> Ident.t -> Ident.t
    (* gives the name of a variable at a given date *)
val vars_at_date : t -> date -> Ident.t list
                 -> (Ident.t * Ident.t) list
    (* idem for a list of variables *)  

(* pretty-printers *)

open Format

val print : formatter -> t -> unit

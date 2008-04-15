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

(*i $Id: effect.mli,v 1.21 2008/11/05 14:03:17 filliatr Exp $ i*)

(*s The abstract type of effects. *)

type t

val bottom : t

val add_read  : Ident.t -> t -> t
val add_reads : Ident.set -> t -> t
val add_write : Ident.t -> t -> t
val add_writes : Ident.set -> t -> t
val add_exn : Ident.t -> t -> t
val add_exns : Ident.set -> t -> t
val add_nontermination : t -> t

val get_reads : t -> Ident.t list
val get_writes : t -> Ident.t list
val get_exns : t -> Ident.t list
val get_repr : t -> Ident.t list * Ident.t list * Ident.t list * bool

val is_read  : t -> Ident.t -> bool    (* read-only *)
val is_write : t -> Ident.t -> bool    (* read-write *)
val is_exn : t -> Ident.t -> bool
val is_nonterminating : t -> bool

val union : t -> t -> t

val remove : Ident.t -> t -> t
val remove_exn : Ident.t -> t -> t
val remove_nontermination : t -> t

val keep_writes : t -> t
val erase_exns : t -> t

val occur : Ident.t -> t -> bool

val subst : Logic.var_substitution -> t -> t

open Format

val print : formatter -> t -> unit


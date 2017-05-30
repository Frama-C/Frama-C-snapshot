(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Lang.F

(** Term manipulation for Tacticals *)

val s_bool:term -> term list

val s_cnf_ite: term -> term -> term -> term list 
val s_dnf_ite: term -> term -> term -> term list 
val s_cnf_iff: term -> term -> term list 
val s_dnf_iff: term -> term -> term list
val s_cnf_xor: term -> term -> term list 
val s_dnf_xor: term -> term -> term list 

(* Is the term into a Conjunctive Normal Form *)
val is_cnf: term -> bool

(* returns the Conjunctive Normal Form of a term *)
val e_cnf: ?depth:int -> term -> term 

(*Is the term into a Conjunctive Normal Form *)
val is_dnf: term -> bool

(* returns the Disjunctive Normal Form of a term *)
val e_dnf: ?depth:int -> term -> term 

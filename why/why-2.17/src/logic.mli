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

(*i $Id: logic.mli,v 1.48 2008/11/05 14:03:17 filliatr Exp $ i*)

(*s Logic. *)

type real_constant = string * string * string (* int / frac / exp *)

type constant =
  | ConstInt of string
  | ConstBool of bool
  | ConstUnit
  | ConstFloat of real_constant

(*s Pure types. *)

type pure_type =
  | PTint
  | PTbool
  | PTreal
  | PTunit
  | PTvar of type_var
  | PTexternal of pure_type list * Ident.t

and type_var =
  { tag : int; 
    user : bool;
    mutable type_val : pure_type option }

type instance = pure_type list

type term_label = User of string | Internal of int

type term =
  | Tconst of constant
  | Tvar of Ident.t
  | Tderef of Ident.t
  | Tapp of Ident.t * term list * instance
  | Tnamed of term_label * term

type substitution = term Ident.map
type var_substitution = Ident.t Ident.map

type is_wp = bool
type is_sym = bool

type predicate =
  | Pvar of Ident.t
  | Papp of Ident.t * term list * instance
  | Ptrue
  | Pfalse
  | Pimplies of is_wp * predicate * predicate
  | Pif of term * predicate * predicate
  | Pand of is_wp * is_sym * predicate * predicate
  | Por of predicate * predicate
  | Piff of predicate * predicate
  | Pnot of predicate
  | Forall of is_wp * Ident.t * Ident.t * pure_type * triggers * predicate
  | Forallb of is_wp * predicate * predicate
  | Exists of Ident.t * Ident.t * pure_type * predicate
  | Pfpi of term * real_constant * real_constant
  | Pnamed of term_label * predicate
and pattern = TPat of term | PPat of predicate
and trigger = pattern list
and triggers = trigger list


type logic_type =
  | Predicate of pure_type list
  | Function of pure_type list * pure_type

type predicate_def = (Ident.t * pure_type) list * predicate

type inductive_def = pure_type list * (Ident.t * predicate) list

type function_def = (Ident.t * pure_type) list * pure_type * term



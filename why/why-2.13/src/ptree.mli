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

(*i $Id: ptree.mli,v 1.39 2008/05/28 13:51:26 marche Exp $ i*)

(*s Parse trees. *)

open Logic
open Types

type loc = Loc.position

(*s Logical expressions (for both terms and predicates) *)

type pp_infix = 
  PPand | PPor | PPimplies | PPiff |
  PPlt | PPle | PPgt | PPge | PPeq | PPneq |
  PPadd | PPsub | PPmul | PPdiv | PPmod

type pp_prefix = 
  PPneg | PPnot

type ppure_type =
  | PPTint
  | PPTbool
  | PPTreal
  | PPTunit
  | PPTvarid of Ident.t * Loc.position
  | PPTexternal of ppure_type list * Ident.t * Loc.position

type lexpr = 
  { pp_loc : loc; pp_desc : pp_desc }

and pp_desc =
  | PPvar of Ident.t
  | PPapp of Ident.t * lexpr list
  | PPtrue
  | PPfalse
  | PPconst of constant
  | PPinfix of lexpr * pp_infix * lexpr
  | PPprefix of pp_prefix * lexpr
  | PPif of lexpr * lexpr * lexpr
  | PPforall of Ident.t * ppure_type * lexpr list list * lexpr
  | PPexists of Ident.t * ppure_type * lexpr
  | PPfpi of lexpr * real_constant * real_constant
  | PPnamed of string * lexpr

type assertion = { 
  pa_name : Ident.name; 
  pa_value : lexpr; 
  pa_loc : Loc.position;
}

type postcondition = assertion * (Ident.t * assertion) list

(*s Parsed types *)

type peffect = { 
  pe_reads : Ident.t list;
  pe_writes : Ident.t list;
  pe_raises : Ident.t list
}

type ptype_v =
  | PVpure  of ppure_type
  | PVref   of ppure_type
  | PVarrow of ptype_v binder list * ptype_c

and ptype_c =
  { pc_result_name : Ident.t;
    pc_result_type : ptype_v;
    pc_effect : peffect;
    pc_pre    : assertion list;
    pc_post   : postcondition option }

(*s Parsed program. *)

type variable = Ident.t

type label = string

type variant = lexpr * variable

type exn_pattern = variable * variable option

type t = 
  { pdesc : t_desc;
    ploc : loc }

and t_desc =
  | Svar of variable
  | Sderef of variable
  | Sloop of assertion option * variant option * t
  | Sif of t * t * t
  | Sapp of t * t
  | Sletref of variable * t * t
  | Sletin of variable * t * t
  | Sseq of t * t
  | Slam of ptype_v binder list * assertion list * t
  | Srec of 
      variable * ptype_v binder list * ptype_v * variant option * 
	assertion list * t
  | Sraise of variable * t option * ptype_v option
  | Stry of t * (exn_pattern * t) list
  | Sconst of constant
  | Sabsurd of ptype_v option
  | Sany of ptype_c
  | Slabel of label * t
  | Sassert of assertion list * t
  | Spost of t * postcondition * transp

type parsed_program = t

(*s Declarations. *)

type external_ = bool

type plogic_type =
  | PPredicate of ppure_type list
  | PFunction of ppure_type list * ppure_type

type decl = 
  | Program of loc * Ident.t * parsed_program
  | Parameter of loc * external_ * Ident.t list * ptype_v
  | Exception of loc * Ident.t * ppure_type option
  | Logic of loc * external_ * Ident.t list * plogic_type
  | Predicate_def of loc * Ident.t * (loc * Ident.t * ppure_type) list * lexpr
  | Function_def 
      of loc * Ident.t * (loc * Ident.t * ppure_type) list * ppure_type * lexpr
  | Axiom of loc * Ident.t * lexpr
  | Goal of loc * Ident.t * lexpr
  | TypeDecl of loc * external_ * Ident.t list * Ident.t

type file = decl list

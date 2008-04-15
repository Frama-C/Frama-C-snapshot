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

(*i $Id: clogic.mli,v 1.72 2008/11/05 14:03:13 filliatr Exp $ i*)

(* AST for C annotations *)

type signed = bool

type logic_type = 
  | LTvoid
  | LTchar of signed
  | LTshort of signed
  | LTint of signed
  | LTlong of signed
  | LTlonglong of signed
  | LTinteger
  | LTfloat
  | LTdouble
  | LTlongdouble
  | LTreal
  | LTarray of logic_type
  | LTpointer of logic_type
  | LTvar of string

(* parsed terms and predicates *)

type constant = 
  | IntConstant of string 
  | RealConstant of string

type term_binop = 
  | Badd | Bsub | Bmul | Bdiv | Bmod | Bpow_real
  | Bbw_and | Bbw_xor | Bbw_or | Bshift_left | Bshift_right

type term_unop = 
  | Uminus | Utilde | Ustar | Uamp |Uplus
  | Uabs_real | Usqrt_real | Uround_error | Utotal_error
  | Uexact | Umodel | Unot
  (* introduced during typing *)
  | Ufloat_of_int | Uint_of_float | Ufloat_conversion | Uint_conversion

type 'ctype quantifiers = ('ctype * string) list
type 'ctype typed_quantifiers = ('ctype * Info.var_info) list

type relation = Lt | Gt | Le | Ge | Eq | Neq

type lexpr = {
  lexpr_node : lexpr_node;
  lexpr_loc : Loc.position
}

and lexpr_node = 
  (* both terms and predicates *)
  | PLvar of Info.var_info
  | PLapp of Info.logic_info * lexpr list
  (* terms *)
  | PLconstant of constant
  | PLunop of term_unop * lexpr
  | PLbinop of lexpr * term_binop * lexpr
  | PLdot of lexpr * string
  | PLarrow of lexpr * string
  | PLarrget of lexpr * lexpr
  | PLold of lexpr
  | PLat of lexpr * string
  | PLbase_addr of lexpr
  | PLoffset of lexpr
  | PLblock_length of lexpr
  | PLarrlen of lexpr
  | PLstrlen of lexpr
  | PLmin of lexpr * lexpr
  | PLmax of lexpr * lexpr
  | PLminint of logic_type
  | PLmaxint of logic_type
  | PLresult
  | PLnull
  | PLcast of logic_type * lexpr
  | PLrange of lexpr * lexpr option * lexpr option
  (* predicates *)
  | PLfalse
  | PLtrue
  | PLrel of lexpr * relation * lexpr
  | PLand of lexpr * lexpr
  | PLor of lexpr * lexpr
  | PLimplies of lexpr * lexpr
  | PLiff of lexpr * lexpr
  | PLnot of lexpr
  | PLif of lexpr * lexpr * lexpr
  | PLforall of logic_type quantifiers * lexpr
  | PLexists of logic_type quantifiers * lexpr
  | PLvalid of lexpr
  | PLseparated of lexpr * lexpr
  | PLbound_separated of lexpr * lexpr * lexpr * lexpr
  | PLfull_separated of lexpr * lexpr
  | PLfullseparated of lexpr * lexpr
  | PLvalid_index of lexpr * lexpr
  | PLvalid_range of lexpr * lexpr * lexpr
  | PLfresh of lexpr
  | PLnamed of string * lexpr

(* typed terms *)

type 'ctype term = {
  term_node : 'ctype term_node;
  term_loc : Loc.position;
  term_type : 'ctype;
}

and 'ctype term_node =
  | Tconstant of constant
  | Tstring_literal of string
  | Tvar of Info.var_info
  | Tapp of Info.logic_info * 'ctype term list
  | Tunop of term_unop * 'ctype term
  | Tbinop of 'ctype term * term_binop * 'ctype term
  | Tdot of 'ctype term * Info.var_info
  | Tarrow of 'ctype term * Info.var_info
  | Tarrget of 'ctype term * 'ctype term
  | Tif of 'ctype term * 'ctype term * 'ctype term
  | Told of 'ctype term
  | Tat of 'ctype term * string
  | Tbase_addr of 'ctype term
  | Toffset of 'ctype term
  | Tblock_length of 'ctype term
  | Tarrlen of 'ctype term
  | Tstrlen of 'ctype term
  | Tmin of 'ctype term * 'ctype term
  | Tmax of 'ctype term * 'ctype term
  | Tminint of 'ctype
  | Tmaxint of 'ctype
(*
  | Tresult of Info.env_info
  | Tnull
*)
  | Tcast of 'ctype * 'ctype term
  | Trange of 'ctype term * 'ctype term option * 'ctype term option

(* typed predicates *)

type 'ctype predicate = {
  pred_node : 'ctype predicate_node;
  pred_loc : Loc.position;
}

and 'ctype predicate_node = 
  | Pfalse
  | Ptrue
  | Papp of Info.logic_info * 'ctype term list
  | Prel of 'ctype term * relation * 'ctype term
  | Pand of 'ctype predicate * 'ctype predicate
  | Por of 'ctype predicate * 'ctype predicate
  | Pimplies of 'ctype predicate * 'ctype predicate
  | Piff of 'ctype predicate * 'ctype predicate
  | Pnot of 'ctype predicate
  | Pif of 'ctype term * 'ctype predicate * 'ctype predicate
  | Pforall of 'ctype typed_quantifiers * 'ctype predicate
  | Pexists of 'ctype typed_quantifiers * 'ctype predicate
  | Pold of 'ctype predicate
  | Pat of 'ctype predicate * string
  | Pseparated of 'ctype term * 'ctype term  
  | Pbound_separated of 'ctype term * 'ctype term * 'ctype term * 'ctype term  
  | Pfull_separated of 'ctype term * 'ctype term
  | Pfullseparated of 'ctype term * 'ctype term
  | Pvalid of 'ctype term 
  | Pvalid_index of 'ctype term * 'ctype term
  | Pvalid_range of 'ctype term * 'ctype term * 'ctype term
  | Pfresh of 'ctype term 
  | Pnamed of string * 'ctype predicate

type 'term location = 'term

type 'term variant = 'term * string option

type ('term,'pred) spec = { 
  mutable requires : 'pred option;
  mutable assigns : (Loc.position * 'term location list) option;    
  mutable ensures : 'pred option;
  mutable decreases : 'term variant option
}

type ('term,'pred) loop_annot = {
  invariant : 'pred option;
    (* part of the invariant already proved by program analysis *)
  assume_invariant : 'pred option;
  loop_assigns : (Loc.position * 'term location list) option;
  variant : 'term variant option
}

type ('term,'ctype) logic_symbol =
  | Predicate_reads of (Info.var_info * 'ctype) list * 'term location list
  | Predicate_def of (Info.var_info * 'ctype) list * 'ctype predicate 
  | Function of (Info.var_info * 'ctype) list * 'ctype * 'term location list
  | Function_def of (Info.var_info * 'ctype) list * 'ctype * 'term

(*

normalized AST

*)

type 'ctype nterm = {
  nterm_node : 'ctype nterm_node;
  nterm_loc : Loc.position;
  nterm_type : 'ctype;
}

and 'ctype nterm_node =
  | NTconstant of constant
  | NTstring_literal of string
  | NTvar of Info.var_info
  | NTapp of 'ctype napp
  | NTunop of term_unop * 'ctype nterm
  | NTbinop of 'ctype nterm * term_binop * 'ctype nterm
  | NTarrow of 'ctype nterm * Info.zone * Info.var_info
  | NTif of 'ctype nterm * 'ctype nterm * 'ctype nterm
  | NTold of 'ctype nterm
  | NTat of 'ctype nterm * string
  | NTbase_addr of 'ctype nterm
  | NToffset of 'ctype nterm
  | NTblock_length of 'ctype nterm
  | NTarrlen of 'ctype nterm
      (* [strlen(p)] depends on the value pointed to by [p] *)
  | NTstrlen of 'ctype nterm * Info.zone * Info.var_info
  | NTmin of 'ctype nterm * 'ctype nterm
  | NTmax of 'ctype nterm * 'ctype nterm
  | NTminint of 'ctype
  | NTmaxint of 'ctype
  | NTcast of 'ctype * 'ctype nterm
  | NTrange of 'ctype nterm * 'ctype nterm option * 'ctype nterm option 
      * Info.zone * Info.var_info

and 'ctype napp = { 
  napp_pred : Info.logic_info;
  napp_args : 'ctype nterm list;
  mutable napp_zones_assoc : (Info.zone * Info.zone) list;
}

type 'ctype npredicate = {
  npred_node : 'ctype npredicate_node;
  npred_loc : Loc.position
}

and 'ctype npredicate_node =
  | NPfalse
  | NPtrue
  | NPapp of 'ctype napp
  | NPrel of 'ctype nterm * relation * 'ctype nterm
  | NPand of 'ctype npredicate * 'ctype npredicate
  | NPor of 'ctype npredicate * 'ctype npredicate
  | NPimplies of 'ctype npredicate * 'ctype npredicate
  | NPiff of 'ctype npredicate * 'ctype npredicate
  | NPnot of 'ctype npredicate
  | NPif of 'ctype nterm * 'ctype npredicate * 'ctype npredicate
  | NPforall of 'ctype typed_quantifiers * 'ctype npredicate
  | NPexists of 'ctype typed_quantifiers * 'ctype npredicate
  | NPold of 'ctype npredicate
  | NPat of 'ctype npredicate * string
  | NPvalid of 'ctype nterm 
  | NPvalid_index of 'ctype nterm * 'ctype nterm
  | NPvalid_range of 'ctype nterm * 'ctype nterm * 'ctype nterm
  | NPfresh of 'ctype nterm 
  | NPnamed of string * 'ctype npredicate
  | NPseparated of 'ctype nterm * 'ctype nterm
  | NPfull_separated of 'ctype nterm * 'ctype nterm
  | NPbound_separated of 
      'ctype nterm * 'ctype nterm * 'ctype nterm * 'ctype nterm

type ('term,'ctype) nlogic_symbol =
  | NPredicate_reads of (Info.var_info * 'ctype) list * 'term location list
  | NPredicate_def of (Info.var_info * 'ctype) list * 'ctype npredicate 
  | NFunction of (Info.var_info * 'ctype) list * 'ctype * 'term location list
  | NFunction_def of (Info.var_info * 'ctype) list * 'ctype * 'term



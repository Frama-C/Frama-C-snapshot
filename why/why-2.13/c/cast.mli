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

(*i $Id: cast.mli,v 1.82 2008/02/05 12:10:47 marche Exp $ i*)

(*s C types *)

open Clogic
open Ctypes

type 'a located = { node : 'a; loc : Loc.position }

type 'expr cinteger = Char | Short | Int | Long | LongLong | Bitfield of 'expr

type 'a tagged = Tag | Decl of 'a

type 'expr ctype_node =
  | CTvoid
  | CTint of sign * 'expr cinteger
  | CTfloat of cfloat
  | CTvar of string
  | CTarray of 'expr ctype * 'expr option
  | CTpointer of 'expr ctype
  | CTstruct of string * 'expr field list tagged
  | CTunion of string * 'expr field list tagged
  | CTenum of string * (string * 'expr option) list tagged
  | CTfun of 'expr parameter list * 'expr ctype

and 'expr ctype = { 
  ctype_node : 'expr ctype_node;
  ctype_storage : storage_class;
  ctype_const : bool;
  ctype_volatile : bool;
}

and 'expr parameter = 'expr ctype * string

and 'expr field = 'expr ctype * string * 'expr option

(*s C parsed abstract syntax trees *)

type assign_operator = 
  | Aequal | Amul | Adiv | Amod | Aadd | Asub | Aleft | Aright 
  | Aand | Axor | Aor

type incr_operator = 
  | Uprefix_inc | Uprefix_dec | Upostfix_inc | Upostfix_dec 

type unary_operator = 
  | Uplus | Uminus | Unot | Ustar | Uamp | Utilde
  (* these are introduced during typing *)
  | Ufloat_of_int | Uint_of_float | Ufloat_conversion | Uint_conversion

type binary_operator =
  | Badd | Bsub | Bmul | Bdiv | Bmod 
  | Blt | Bgt | Ble | Bge | Beq | Bneq 
  | Bbw_and | Bbw_xor | Bbw_or | Band | Bor
  | Bshift_left | Bshift_right
  (* these are introduced during typing *)
  | Badd_int of (sign * Ctypes.cinteger) 
  | Bsub_int of (sign * Ctypes.cinteger) 
  | Bmul_int of (sign * Ctypes.cinteger)
  | Bdiv_int of (sign * Ctypes.cinteger)
  | Bmod_int of (sign * Ctypes.cinteger)
  | Blt_int | Bgt_int | Ble_int | Bge_int | Beq_int | Bneq_int 
  | Badd_float of cfloat | Bsub_float of cfloat 
  | Bmul_float of cfloat | Bdiv_float of cfloat
  | Blt_float of cfloat | Bgt_float of cfloat | Ble_float of cfloat 
  | Bge_float of cfloat | Beq_float of cfloat | Bneq_float of cfloat
  | Badd_pointer_int (* pointer + int *) 
  | Bsub_pointer     (* pointer - pointer *)
  | Blt_pointer | Bgt_pointer | Ble_pointer | Bge_pointer 
  | Beq_pointer | Bneq_pointer

type cexpr = cexpr_node located

and cexpr_node =
  | CEnop
  | CEconstant of constant
  | CEstring_literal of string
  | CEvar of string
  | CEdot of cexpr * string
  | CEarrow of cexpr * string
  | CEarrget of cexpr * cexpr
  | CEseq of cexpr * cexpr
  | CEassign of cexpr * cexpr
  | CEassign_op of cexpr * binary_operator * cexpr
  | CEunary of unary_operator * cexpr
  | CEincr of incr_operator * cexpr
  | CEbinary of cexpr * binary_operator * cexpr
  | CEcall of cexpr * cexpr list
  | CEcond of cexpr * cexpr * cexpr
  | CEcast of cexpr ctype * cexpr
  | CEsizeof_expr of cexpr
  | CEsizeof of cexpr ctype

type 'expr c_initializer = 
  | Iexpr of 'expr
  | Ilist of 'expr c_initializer list

type pctype = cexpr ctype

(* parsed logic AST *)

type parsed_term = Clogic.lexpr
type parsed_predicate = Clogic.lexpr
type parsed_spec = (parsed_term, parsed_predicate) spec
type parsed_loop_annot = (parsed_term, parsed_predicate) loop_annot
type parsed_logic_type = logic_type

type parsed_decl = 
  | LDlogic of 
      Info.logic_info * parsed_logic_type * string list * 
	(parsed_logic_type * string) list * parsed_term location list
  | LDlogic_def of 
      Info.logic_info * parsed_logic_type * string list * 
	(parsed_logic_type * string) list * parsed_term
  | LDpredicate_reads of 
      Info.logic_info * (parsed_logic_type * string) list 
      * parsed_term location list
  | LDpredicate_def of 
      Info.logic_info * (parsed_logic_type * string) list * parsed_predicate
  | LDaxiom of string * parsed_predicate
  | LDinvariant of string * parsed_predicate
  | LDghost of cexpr ctype * string * lexpr c_initializer option
  | LDtype of string * Loc.position

type ghost_lvalue = lexpr

type parsed_code_annot = 
  | Assert of parsed_predicate 
  | Assume of parsed_predicate 
  | Label of string
  | GhostSet of ghost_lvalue * lexpr 

type parsed_annot = 
  | Adecl of parsed_decl list
  | Aspec of parsed_spec
  | Acode_annot of parsed_code_annot
  | Aloop_annot of parsed_loop_annot

type cstatement = cstatement_node located

and cstatement_node =
  | CSnop
  | CSexpr of cexpr
  | CSif of cexpr * cstatement * cstatement
  | CSwhile of parsed_loop_annot * cexpr * cstatement
  | CSdowhile of parsed_loop_annot * cstatement * cexpr
  | CSfor of parsed_loop_annot * cexpr * cexpr * cexpr * cstatement
  | CSblock of block
  | CSreturn of cexpr option
  | CSbreak
  | CScontinue
  | CSlabel of string * cstatement
  | CSswitch of cexpr * cstatement
  | CScase of cexpr * cstatement
  | CSdefault of cstatement
  | CSgoto of string
  | CSannot of parsed_code_annot
  | CSspec of parsed_spec * cstatement

and block = decl located list * cstatement list

and decl = 
  | Cspecdecl of parsed_decl
  | Ctypedef of cexpr ctype * string
  | Ctypedecl of cexpr ctype
  | Cdecl of cexpr ctype * string * cexpr c_initializer option
  | Cfunspec of parsed_spec * cexpr ctype * string * cexpr parameter list
  | Cfundef of parsed_spec option * 
      cexpr ctype * string * cexpr parameter list * cstatement

type file = decl located list

(*s C typed abstract syntax trees *)

open Clogic

type texpr = {
  texpr_node : texpr_node;
  texpr_type : tctype;
  texpr_loc  : Loc.position;
}

and texpr_node =
  | TEnop
  | TEconstant of constant
  | TEstring_literal of string
  | TEvar of Info.env_info
  | TEdot of lvalue * Info.var_info
  | TEarrow of lvalue * Info.var_info
  | TEarrget of lvalue * texpr
  | TEseq of texpr * texpr
  | TEassign of lvalue * texpr
  | TEassign_op of lvalue * binary_operator * texpr
  | TEunary of unary_operator * texpr
  | TEincr of incr_operator * texpr
  | TEbinary of texpr * binary_operator * texpr
  | TEcall of texpr * texpr list
  | TEcond of texpr * texpr * texpr
  | TEcast of tctype * texpr
  | TEsizeof of tctype * int64
  | TEmalloc of tctype * texpr

and lvalue = texpr (* TODO: cf CIL *)

and tctype = Ctypes.ctype

type tterm = tctype term

type predicate = tctype Clogic.predicate

type spec = (tterm, predicate) Clogic.spec

type variant = tterm * string option

type loop_annot = (tterm, predicate) Clogic.loop_annot

type goto_status = GotoBackward | GotoForwardOuter | GotoForwardInner

type tstatement = {
  st_node : tstatement_node;
  st_break : bool;    (* may breaks *)
  st_continue : bool; (* may continue *)
  st_return : bool;   (* may return *)
  st_term : bool;     (* may terminate normally *)
  st_loc : Loc.position
}

and tstatement_node =
  | TSnop
  | TSexpr of texpr
  | TSif of texpr * tstatement * tstatement
  | TSwhile of loop_annot * texpr * tstatement
  | TSdowhile of loop_annot * tstatement * texpr
  | TSfor of loop_annot * texpr * texpr * texpr * tstatement
  | TSblock of tblock
  | TSreturn of texpr option
  | TSbreak
  | TScontinue
  | TSlabel of Info.label_info * tstatement
  | TSswitch of texpr * tstatement
  | TScase of texpr * tstatement
  | TSdefault of tstatement
  | TSgoto of goto_status * Info.label_info
  | TSassert of predicate
  | TSassume of predicate
  | TSlogic_label of string
  | TSspec of spec * tstatement
  | TSset of (tterm * tterm)

and tblock = tdecl located list * tstatement list

and tdecl = 
  | Tlogic of Info.logic_info * (tterm,tctype) logic_symbol
  | Taxiom of string * predicate
  | Tinvariant of string * predicate
  | Tghost of Info.var_info *  tterm c_initializer option
  | Ttype of string
  | Ttypedef of tctype * string
  | Ttypedecl of tctype
  | Tdecl of tctype * Info.var_info * texpr c_initializer option
  | Tfunspec of spec * tctype * Info.fun_info 
  | Tfundef of spec * tctype * Info.fun_info * tstatement 

type tfile = tdecl located list


(*

normalized AST

*)

type nexpr = {
  nexpr_node : nexpr_node;
  nexpr_type : nctype;
  nexpr_loc  : Loc.position;
}

and ncall = {
  ncall_fun : nexpr;
  ncall_args : nexpr list;
  mutable ncall_zones_assoc : (Info.zone * Info.zone) list;
}

and nexpr_node =
  | NEnop
  | NEconstant of constant
  | NEstring_literal of string
  | NEvar of Info.env_info
  | NEarrow of nlvalue * Info.zone * Info.var_info
  | NEseq of nexpr * nexpr
  | NEassign of nlvalue * nexpr
  | NEassign_op of nlvalue * binary_operator * nexpr
  | NEunary of unary_operator * nexpr
  | NEincr of incr_operator * nexpr
  | NEbinary of nexpr * binary_operator * nexpr
  | NEcall of ncall
  | NEcond of nexpr * nexpr * nexpr
  | NEcast of nctype * nexpr
  | NEmalloc of nctype * nexpr

and nlvalue = nexpr (* TODO: cf CIL *)

and nctype = (*int64*) Ctypes.ctype

type nterm = nctype Clogic.nterm

type npredicate = nctype Clogic.npredicate

type nspec = (nterm, npredicate) Clogic.spec

type nvariant = nterm * string option

type nloop_annot = (nterm, npredicate) Clogic.loop_annot

type nstatement = {
  nst_node : nstatement_node;
  nst_break : bool;    (* may break *)
  nst_continue : bool; (* may continue *)
  nst_return : bool;   (* may return *)
  nst_term : bool;     (* may terminate normally *)
  nst_loc : Loc.position
}

and nstatement_node =
  | NSnop
  | NSexpr of nexpr
  | NSif of nexpr * nstatement * nstatement
  | NSwhile of nloop_annot * nexpr * nstatement
  | NSdowhile of nloop_annot * nstatement * nexpr
  | NSfor of nloop_annot * nexpr * nexpr * nexpr * nstatement
  | NSblock of nblock
  | NSreturn of nexpr option
  | NSbreak
  | NScontinue
  | NSgoto of goto_status * Info.label_info
  | NSlabel of Info.label_info * nstatement
  | NSswitch of nexpr * (nexpr Cconst.IntMap.t) * 
      ((nexpr Cconst.IntMap.t * nstatement list) list)
      (* NSswitch(e,used_cases,all_cases):
	 e is the expression to test 
         used_cases is a map with gives all the possible cases, mapping each value to the constant expression in comes from in the program 
	 all_cases is the list of all branches, where each case is given by 
           - the cases involved (a submap of used_cases)
           - the statements to execute
	 *)
  | NSassert of npredicate
      (* [NSassume] used to communicate results of abstract interpretation
	 to the module that inserts verification conditions *)
  | NSassume of npredicate
  | NSlogic_label of string
  | NSspec of nspec * nstatement
  | NSdecl of nctype * Info.var_info * nexpr c_initializer option * nstatement

and nblock = nstatement list

and ndecl = 
  | Nlogic of Info.logic_info * (nterm,nctype) nlogic_symbol
  | Naxiom of string * npredicate
  | Ninvariant of string * npredicate
  | Ninvariant_strong of string * npredicate
  | Ntypedef of nctype * string
  | Ntypedecl of nctype
  | Ndecl of nctype * Info.var_info * nexpr c_initializer option
  | Ntype of string

type nfile = ndecl located list

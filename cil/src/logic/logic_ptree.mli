(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA   (Commissariat à l'Énergie Atomique)                           *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(* Logic parse trees *)

open Cil_types

type location = Lexing.position * Lexing.position

type logic_type =
  | LTvoid
  | LTinteger
  | LTreal
  | LTint of ikind
  | LTfloat of fkind
  | LTarray of logic_type
  | LTpointer of logic_type
  | LTenum of string
  | LTstruct of string
  | LTunion of string
  | LTnamed of string * logic_type list
(*  | LTarrow of logic_type list * logic_type *)

type quantifiers = (logic_type * string) list

type constant = IntConstant of string | FloatConstant of string

type relation = Lt | Gt | Le | Ge | Eq | Neq

type binop = Badd | Bsub | Bmul | Bdiv | Bmod | Bbw_and | Bbw_or | Bbw_xor |
             Blshift | Brshift

type unop = Uminus | Ustar | Uamp | Ubw_not

type lexpr = {
  lexpr_node : lexpr_node;
  lexpr_loc : location
}

(* PL is for Parsed Logic *)
and lexpr_node =
    (* both terms and predicates *)
  | PLvar of string
  | PLapp of string * string list * lexpr list
      (* terms *)
  | PLlambda of (logic_type * string) list * lexpr
  | PLconstant of constant
  | PLunop of unop * lexpr
  | PLbinop of lexpr * binop * lexpr
  | PLdot of lexpr * string
  | PLarrow of lexpr * string
  | PLarrget of lexpr * lexpr
  | PLold of lexpr
  | PLat of lexpr * string
  | PLbase_addr of lexpr
  | PLblock_length of lexpr
  | PLresult
  | PLnull
  | PLcast of logic_type * lexpr
  | PLrange of lexpr option * lexpr option
  | PLsizeof of logic_type
  | PLsizeofE of lexpr
  | PLcoercion of lexpr * logic_type
  | PLcoercionE of lexpr * lexpr
  | PLupdate of lexpr * string * lexpr
      (* predicates *)
  | PLfalse
  | PLtrue
  | PLrel of lexpr * relation * lexpr
  | PLand of lexpr * lexpr
  | PLor of lexpr * lexpr
  | PLxor of lexpr * lexpr
  | PLimplies of lexpr * lexpr
  | PLiff of lexpr * lexpr
  | PLnot of lexpr
  | PLif of lexpr * lexpr * lexpr
  | PLforall of quantifiers * lexpr
  | PLexists of quantifiers * lexpr
  | PLvalid of lexpr
  | PLvalid_index of lexpr * lexpr
  | PLvalid_range of lexpr * lexpr * lexpr
  | PLfresh of lexpr
  | PLnamed of string * lexpr
  | PLinstance_of of lexpr * logic_type
  | PLinstance_ofE of lexpr * lexpr
      (* tsets *)
  | PLcomprehension of lexpr * quantifiers * lexpr option
  | PLunion of lexpr list
  | PLinter of lexpr list
  | PLempty

type type_annot = (lexpr, logic_type) Cil_types.type_annot

(** global declarations. *)
type decl =
  | LDlogic_reads of
      string * string list * string list *
        logic_type * (logic_type * string) list * lexpr list
        (** LDlogic_reads(name,labels,type_params,
                          return_type, parameters, reads_tsets) *)
  | LDlogic_def of
      string * string list * string list *
	logic_type * (logic_type * string) list * lexpr
        (** LDlogic_def(name,labels,type_params,
                        return_type, parameters, definition) *)
  | LDtype of string * string list(** new logic type + its parameters *)
  | LDpredicate_reads of
      string * string list * string list *
	(logic_type * string) list * lexpr list
        (** LDpredicate_reads(name,labels,type_params,
                              parameters, reads_tsets) *)
  | LDpredicate_def of string * string list * string list *
      (logic_type * string) list * lexpr
        (** LDpredicate_def(name,labels,type_params, parameters, def) *)
  | LDlemma of string * bool * string list * string list * lexpr
      (** LDlemma(name,is_axiom,labels,type_params,property)
          [is_axiom] is true for an axiom, false for a lemma
       *)
  | LDinvariant of string * lexpr
  | LDtype_annot of type_annot


type spec = (lexpr, lexpr, lexpr) Cil_types.spec
type code_annot = (lexpr, lexpr, lexpr, lexpr) Cil_types.code_annot

type assigns = lexpr Cil_types.assigns
type variant = lexpr Cil_types.variant

type annot =
  | Adecl of location * decl
  | Aspec  (* the real spec is parsed afterwards.
              See cparser.mly (grammar rules involving SPEC) for
              more details.
            *)
  | Acode_annot of location * code_annot
  | Aloop_annot of location * code_annot list
  | Aattribute_annot of location * string

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../../.."
End:
*)

(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
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
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(* Logic parse trees *)

open Cil_types

(** logic constants. *)
type constant =
    IntConstant of string (** integer constant *)
  | FloatConstant of string (** real constant *)
  | StringConstant of string (** string constant *)
  | WStringConstant of string (** wide string constant *)

(** logic types. *)
type logic_type =
  | LTvoid (** C void *)
  | LTinteger (** mathematical integers. *)
  | LTreal (** mathematical real. *)
  | LTint of ikind (** C integral type.*)
  | LTfloat of fkind (** C floating-point type *)
  | LTarray of logic_type * constant option (** C array *)
  | LTpointer of logic_type (** C pointer *)
  | LTenum of string (** C enum *)
  | LTstruct of string (** C struct *)
  | LTunion of string (** C union *)
  | LTnamed of string * logic_type list (** declared logic type. *)
  | LTarrow of logic_type list * logic_type

(** quantifier-bound variables *)
type quantifiers = (logic_type * string) list

(** comparison operators. *)
type relation = Lt | Gt | Le | Ge | Eq | Neq

(** arithmetic and logic binary operators. *)
type binop = Badd | Bsub | Bmul | Bdiv | Bmod | Bbw_and | Bbw_or | Bbw_xor |
             Blshift | Brshift

(** unary operators. *)
type unop = Uminus | Ustar | Uamp | Ubw_not

(** logical expression. The distinction between locations, terms and
    predicate is done during typing.
*)
type lexpr = {
  lexpr_node : lexpr_node; (** kind of expression. *)
  lexpr_loc : location (** position in the source code. *)
}

(* PL is for Parsed Logic *)
(** kind of expression. *)
and path_elt =
    (** construct inside a functional update. *)
  | PLpathField of string
  | PLpathIndex of lexpr

and update_term =
  | PLupdateTerm of lexpr
  | PLupdateCont of ((path_elt list) * update_term) list
and lexpr_node =
    (* both terms and predicates *)
  | PLvar of string (** a variable *)
  | PLapp of string * string list * lexpr list (** an application. *)
      (* terms *)
  | PLlambda of quantifiers * lexpr (** a lambda abstraction. *)
  | PLlet of string * lexpr * lexpr (** local binding. *)
  | PLconstant of constant (** a constant. *)
  | PLunop of unop * lexpr (** unary operator. *)
  | PLbinop of lexpr * binop * lexpr (** binary operator. *)
  | PLdot of lexpr * string (** field access ({t a.x}) *)
  | PLarrow of lexpr * string (** field access ({t a->x})*)
  | PLarrget of lexpr * lexpr (** array access. *)
  | PLold of lexpr (** expression refers to pre-state of a function. *)
  | PLat of lexpr * string (** expression refers to a given program point. *)
  | PLresult (** value returned by a function. *)
  | PLnull (** null pointer. *)
  | PLcast of logic_type * lexpr (** cast. *)
  | PLrange of lexpr option * lexpr option (** interval of integers. *)
  | PLsizeof of logic_type (** sizeof a type. *)
  | PLsizeofE of lexpr (** sizeof the type of an expression. *)
  | PLcoercion of lexpr * logic_type
      (** coercion of an expression in a given type. *)
  | PLcoercionE of lexpr * lexpr
      (** coercion of the first expression into the type of the second one. *)
  | PLupdate of lexpr * (path_elt list) * update_term
      (** functional update of the field of a structure. *)
  | PLinitIndex of (lexpr * lexpr) list (** array constructor. *)
  | PLinitField of (string * lexpr) list (** struct/union constructor. *)
  | PLtypeof of lexpr (** type tag for an expression. *)
  | PLtype of logic_type (** type tag for a C type. *)
      (* predicates *)
  | PLfalse (** false (either as a term or a predicate. *)
  | PLtrue (** true (either as a term or a predicate. *)
  | PLrel of lexpr * relation * lexpr (** comparison operator. *)
  | PLand of lexpr * lexpr (** conjunction. *)
  | PLor of lexpr * lexpr (** disjunction. *)
  | PLxor of lexpr * lexpr (** logical xor. *)
  | PLimplies of lexpr * lexpr (** implication. *)
  | PLiff of lexpr * lexpr (** equivalence. *)
  | PLnot of lexpr (** negation. *)
  | PLif of lexpr * lexpr * lexpr (** conditional operator. *)
  | PLforall of quantifiers * lexpr (** universal quantification. *)
  | PLexists of quantifiers * lexpr (** existential quantification. *)
  | PLbase_addr of string option * lexpr (** base address of a pointer. *)
  | PLoffset of string option * lexpr (** base address of a pointer. *)
  | PLblock_length of string option * lexpr (** length of the block pointed to by an
                                expression. *)
  | PLvalid of string option * lexpr (** pointer is valid. *)
  | PLvalid_read of string option * lexpr (** pointer is valid for reading. *)
  | PLallocable of string option * lexpr (** pointer is valid for malloc. *)
  | PLfreeable of string option * lexpr (** pointer is valid for free. *)
  | PLinitialized of string option * lexpr (** l-value is guaranteed to be initalized *)
  | PLfresh of (string * string) option * lexpr * lexpr (** expression points to a newly allocated block. *)
  | PLseparated of lexpr list
      (** separation predicate. *)
  | PLnamed of string * lexpr (** named expression. *)
  | PLsubtype of lexpr * lexpr
      (** first type tag is a subtype of second one. *)
      (* tsets *)
  | PLcomprehension of lexpr * quantifiers * lexpr option
      (** set of expression defined in comprehension
          ({t \{ e | integer i; P(i)\}})*)
  | PLsingleton of lexpr
      (** singleton sets. *)
  | PLunion of lexpr list
      (** union of sets. *)
  | PLinter of lexpr list
      (** intersection of sets. *)
  | PLempty
      (** empty set. *)

(** type invariant. *)
type type_annot =  {inv_name: string;
                    this_type : logic_type;
                    this_name: string; (** name of its argument. *)
                    inv: lexpr
                   }

(** model field. *)
type model_annot =  {model_for_type: logic_type;
                     model_type : logic_type;
                     model_name: string; (** name of the model field. *)
                   }

(** Concrete type definition. *)
type typedef =
  | TDsum of (string * logic_type list) list
      (** sum type, list of constructors *)
  | TDsyn of logic_type (** synonym of an existing type *)

(** global declarations. *)
type decl = {
  decl_node : decl_node; (** kind of declaration. *)
  decl_loc : location (** position in the source code. *)
}
and decl_node =
  | LDlogic_def of
      string * string list * string list *
	logic_type * (logic_type * string) list * lexpr
        (** [LDlogic_def(name,labels,type_params,
                         return_type, parameters, definition)]
            represents the definition of a logic function [name] whose
	    return type is [return_type] and arguments are [parameters].
	    Its label arguments are [labels]. Polymorphic functions have
	    their type parameters in [type_params]. [definition] is the
	    body of the defined function.*)
  | LDlogic_reads of
      string * string list * string list *
        logic_type * (logic_type * string) list * lexpr list option
        (** [LDlogic_reads(name,labels,type_params,
           return_type, parameters, reads_tsets)] represents the declaration
           of logic function.  It has the same
            arguments as [LDlogic_def], except that the definition is
           abstracted to a set of read accesses in [read_tsets].
         *)
  | LDtype of string * string list * typedef option
      (** new logic type and its parameters, optionally followed by
          its definition. *)
  | LDpredicate_reads of
      string * string list * string list *
	(logic_type * string) list * lexpr list option
        (** [LDpredicate_reads(name,labels,type_params,
                              parameters, reads_tsets)]
            represents the declaration of a new predicate. It is similar to
            [LDlogic_reads] except that it has no [return_type].
         *)
  | LDpredicate_def of string * string list * string list *
      (logic_type * string) list * lexpr
        (** [LDpredicate_def(name,labels,type_params, parameters, def)]
            represents the definition of a new predicate. It is similar to
            [LDlogic_def] except that it has no [return_type].

         *)
  | LDinductive_def of string * string list * string list *
      (logic_type * string) list * (string * string list * string list * lexpr) list
        (** [LDinductive_def(name,labels,type_params, parameters, indcases)]
            represents an inductive definition of a new predicate.
         *)
  | LDlemma of string * bool * string list * string list * lexpr
      (** LDlemma(name,is_axiom,labels,type_params,property) represents
          a lemma or an axiom [name].
          [is_axiom] is true for an axiom and false for a lemma. [labels]
          is the list of label arguments and
          [type_params] the list of type parameters. Last, [property] is the
          statement of the lemma.
       *)
  | LDaxiomatic of string * decl list
        (** [LDaxiomatic(id,decls)]
            represents a block of axiomatic definitions.*)
  | LDinvariant of string * lexpr (** global invariant. *)
  | LDtype_annot of type_annot    (** type invariant. *)
  | LDmodel_annot of model_annot    (** model field. *)
  | LDvolatile of lexpr list * (string option * string option)
      (** volatile clause read/write. *)

and deps = lexpr Cil_types.deps (** C locations. *)

(** specification of a C function. *)
type spec = (lexpr, lexpr, lexpr) Cil_types.spec
type code_annot = (lexpr, lexpr, lexpr, lexpr) Cil_types.code_annot

(** assignment performed by a C function. *)
type assigns = lexpr Cil_types.assigns

(** variant for loop or recursive function. *)
type variant = lexpr Cil_types.variant

(** custom trees *)

type custom_tree =
  | CustomType of logic_type
  | CustomLexpr of lexpr
  | CustomOther of string * (custom_tree list)

(** all kind of annotations*)
type annot =
  | Adecl of decl list (** global annotation. *)
  | Aspec  (* the real spec is parsed afterwards.
              See cparser.mly (grammar rules involving SPEC) for
              more details.
            *) (** function specification. *)
  | Acode_annot of location * code_annot (** code annotation. *)
  | Aloop_annot of location * code_annot list (** loop annotation. *)
  | Aattribute_annot of location * string (** attribute annotation. *)
  | Acustom of location * string * custom_tree

(** ACSL extension for external spec file **)
type ext_decl =
  | Ext_decl of decl            (* decl contains a location *)
  | Ext_macro of string * lexpr (* lexpr contains a location *)
  | Ext_include of bool * string * location

type ext_function = 
  | Ext_spec of spec * location (* function spec *)
  | Ext_loop_spec of string * annot * location (* loop annotation or
			   code annotation relative to the loop body. *)
  | Ext_stmt_spec of string * annot * location (* code annotation. *)
  | Ext_glob of ext_decl

type ext_module = string * ext_decl list * ((string * location) * ext_function list) list

type ext_spec = ext_module list
(*
Local Variables:
compile-command: "LC_ALL=C make -C ../../.."
End:
*)

(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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
  | LTint of Cil_types.ikind (** C integral type.*)
  | LTfloat of Cil_types.fkind (** C floating-point type *)
  | LTarray of logic_type * constant option (** C array *)
  | LTpointer of logic_type (** C pointer *)
  | LTenum of string (** C enum *)
  | LTstruct of string (** C struct *)
  | LTunion of string (** C union *)
  | LTnamed of string * logic_type list (** declared logic type. *)
  | LTarrow of logic_type list * logic_type
  | LTattribute of logic_type * Cil_types.attribute (* Only const and volatile can appear here *)

type location = Cil_types.location

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
  | PLvalid_function of lexpr (** function pointer is compatible with pointed type. *)
  | PLallocable of string option * lexpr (** pointer is valid for malloc. *)
  | PLfreeable of string option * lexpr (** pointer is valid for free. *)
  | PLinitialized of string option * lexpr (** pointer is guaranteed to be
                                               initialized *)
  | PLdangling of string option * lexpr (** pointer is guaranteed to be
                                            dangling  *)
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
  | PLset of lexpr list
      (** sets of elements. *)
  | PLunion of lexpr list
      (** union of sets. *)
  | PLinter of lexpr list
      (** intersection of sets. *)
  | PLempty
      (** empty set. *)
  | PLlist of lexpr list
      (** list of elements. *)
  | PLrepeat of lexpr * lexpr
      (** repeat a list of elements a number of times. *)

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

(** dependencies of an assigned location. *)
and deps =
  | From of lexpr list (** tsets. Empty list means \nothing. *)
  | FromAny (** Nothing specified. Any location can be involved. *)

and from = (lexpr * deps)

(** zone assigned with its dependencies. *)
and assigns =
  | WritesAny (** Nothing specified. Anything can be written. *)
  | Writes of from list
    (** list of locations that can be written. Empty list means \nothing. *)

(** allocates and frees. 
    @since Oxygen-20120901  *)
and allocation =
  | FreeAlloc of lexpr list * lexpr list (** tsets. Empty list means \nothing. *)
  | FreeAllocAny (** Nothing specified. Semantics depends on where it 
		     is written. *)

(** variant of a loop or a recursive function. *)
and variant = lexpr * string option


type extension = string * lexpr list

(** Behavior in a specification. This type shares the name of its constructors
    with {!Cil_types.behavior}. *)
type behavior = {
  mutable b_name : string; (** name of the behavior. *)
  mutable b_requires : lexpr list; (** require clauses. *)
  mutable b_assumes : lexpr list; (** assume clauses. *)
  mutable b_post_cond : (Cil_types.termination_kind * lexpr) list; (** post-condition. *)
  mutable b_assigns : assigns; (** assignments. *)
  mutable b_allocation : allocation; (** frees, allocates. *)
  mutable b_extended : extension list (** extensions *)
}

(** Function or statement contract. This type shares the name of its
    constructors with {!Cil_types.spec}. *)
type spec = {
  mutable spec_behavior : behavior list;
  (** behaviors *)

  mutable spec_variant : variant option;
  (** variant for recursive functions. *)

  mutable spec_terminates: lexpr option;
  (** termination condition. *)

  mutable spec_complete_behaviors: string list list;
  (** list of complete behaviors.
      It is possible to have more than one set of complete behaviors *)

  mutable spec_disjoint_behaviors: string list list;
  (** list of disjoint behaviors.
     It is possible to have more than one set of disjoint behaviors *)
}

(** Pragmas for the value analysis plugin of Frama-C. *)

type loop_pragma =
  | Unroll_specs of lexpr list
  | Widen_hints of lexpr list
  | Widen_variables of lexpr list

(** Pragmas for the slicing plugin of Frama-C. *)
and slice_pragma =
  | SPexpr of lexpr
  | SPctrl
  | SPstmt

(** Pragmas for the impact plugin of Frama-C. *)
and impact_pragma =
  | IPexpr of lexpr
  | IPstmt

(** The various kinds of pragmas. *)
and pragma =
  | Loop_pragma of loop_pragma
  | Slice_pragma of slice_pragma
  | Impact_pragma of impact_pragma


(** all annotations that can be found in the code. This type shares the name of
    its constructors with {!Cil_types.code_annotation_node}. *)
type code_annot =
  | AAssert of string list * lexpr
  (** assertion to be checked. The list of strings is the list of
      behaviors to which this assertion applies. *)

  | AStmtSpec of string list * spec
  (** statement contract
      (potentially restricted to some enclosing behaviors). *)

  | AInvariant of string list * bool * lexpr
  (** loop/code invariant. The list of strings is the list of behaviors to which
      this invariant applies.  The boolean flag is true for normal loop
      invariants and false for invariant-as-assertions. *)

  | AVariant of variant
  (** loop variant. Note that there can be at most one variant associated to a
      given statement *)

  | AAssigns of string list * assigns
  (** loop assigns.  (see [b_assigns] in the behaviors for other assigns).  At
      most one clause associated to a given (statement, behavior) couple.  *)

  | AAllocation of string list * allocation
  (** loop allocation clause.  (see [b_allocation] in the behaviors for other
      allocation clauses).
      At most one clause associated to a given (statement, behavior) couple.
      @since Oxygen-20120901 when [b_allocation] has been added.  *)

  | APragma of pragma (** pragma. *)
  | AExtended of string list * extension
    (** extension in a loop annotation.
        @since Silicon-20161101 *)

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
  | Ext_macro of bool * string * lexpr (* lexpr contains a location *)
  | Ext_include of bool * string * location

type ext_function = 
  | Ext_spec of spec * location (* function spec *)
  | Ext_stmt of string list * annot * location (* loop/code annotation. *)
  | Ext_glob of ext_decl

type ext_module = string option * ext_decl list * ((string * location) option * ext_function list) list

type ext_spec = ext_module list
(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

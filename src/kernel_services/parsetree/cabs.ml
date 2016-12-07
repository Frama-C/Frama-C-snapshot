(****************************************************************************)
(*                                                                          *)
(*  Copyright (C) 2001-2003                                                 *)
(*   George C. Necula    <necula@cs.berkeley.edu>                           *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                    *)
(*                                                                          *)
(*  Redistribution and use in source and binary forms, with or without      *)
(*  modification, are permitted provided that the following conditions      *)
(*  are met:                                                                *)
(*                                                                          *)
(*  1. Redistributions of source code must retain the above copyright       *)
(*  notice, this list of conditions and the following disclaimer.           *)
(*                                                                          *)
(*  2. Redistributions in binary form must reproduce the above copyright    *)
(*  notice, this list of conditions and the following disclaimer in the     *)
(*  documentation and/or other materials provided with the distribution.    *)
(*                                                                          *)
(*  3. The names of the contributors may not be used to endorse or          *)
(*  promote products derived from this software without specific prior      *)
(*  written permission.                                                     *)
(*                                                                          *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE          *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,     *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;        *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER        *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT      *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN       *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE         *)
(*  POSSIBILITY OF SUCH DAMAGE.                                             *)
(*                                                                          *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux          *)
(*                        énergies alternatives)                            *)
(*               and INRIA (Institut National de Recherche en Informatique  *)
(*                          et Automatique).                                *)
(****************************************************************************)

(** Untyped AST.
    @plugin development guide **)

(*
** Types
*)

type cabsloc = Lexing.position * Lexing.position

type typeSpecifier = (* Merge all specifiers into one type *)
    Tvoid                             (* Type specifier ISO 6.7.2 *)
  | Tchar
  | Tbool
  | Tshort
  | Tint
  | Tlong
  | Tint64
  | Tfloat
  | Tdouble
  | Tsigned
  | Tunsigned
  | Tnamed of string
  (* each of the following three kinds of specifiers contains a field
   * or item list iff it corresponds to a definition (as opposed to
   * a forward declaration or simple reference to the type); they
   * also have a list of __attribute__s that appeared between the
   * keyword and the type name (definitions only) *)
  | Tstruct of string * field_group list option * attribute list
  | Tunion of string * field_group list option * attribute list
  | Tenum of string * enum_item list option * attribute list
  | TtypeofE of expression                      (* GCC __typeof__ *)
  | TtypeofT of specifier * decl_type       (* GCC __typeof__ *)

and storage =
    NO_STORAGE | AUTO | STATIC | EXTERN | REGISTER

and funspec =
    INLINE | VIRTUAL | EXPLICIT

and cvspec =
  | CV_CONST | CV_VOLATILE | CV_RESTRICT
  | CV_ATTRIBUTE_ANNOT of string

(* Type specifier elements. These appear at the start of a declaration *)
(* Everywhere they appear in this file, they appear as a 'spec_elem list', *)
(* which is not interpreted by cabs -- rather, this "word soup" is passed *)
(* on to the compiler.  Thus, we can represent e.g. 'int long float x' even *)
(* though the compiler will of course choke. *)
and spec_elem =
    SpecTypedef
  | SpecCV of cvspec            (* const/volatile *)
  | SpecAttr of attribute       (* __attribute__ *)
  | SpecStorage of storage
  | SpecInline
  | SpecType of typeSpecifier
  | SpecPattern of string       (* specifier pattern variable *)

(* decided to go ahead and replace 'spec_elem list' with specifier *)
and specifier = spec_elem list


(* Declarator type. They modify the base type given in the specifier. Keep
 * them in the order as they are printed (this means that the top level
 * constructor for ARRAY and PTR is the inner-level in the meaning of the
 * declared type) *)
and decl_type =
 | JUSTBASE                               (* Prints the declared name *)
 | PARENTYPE of attribute list * decl_type * attribute list
                                          (* Prints "(attrs1 decl attrs2)".
                                           * attrs2 are attributes of the
                                           * declared identifier and it is as
                                           * if they appeared at the very end
                                           * of the declarator. attrs1 can
                                           * contain attributes for the
                                           * identifier or attributes for the
                                           * enclosing type.  *)
 | ARRAY of decl_type * attribute list * expression
                                          (* Prints "decl [ attrs exp ]".
                                           * decl is never a PTR. *)
 | PTR of attribute list * decl_type      (* Prints "* attrs decl" *)
 | PROTO of decl_type * single_name list * bool
                                          (* Prints "decl (args[, ...])".
                                           * decl is never a PTR.*)

(* The base type and the storage are common to all names. Each name might
 * contain type or storage modifiers *)
(* e.g.: int x, y; *)
and name_group = specifier * name list

(* The optional expression is the bitfield *)
and field_group =
  | FIELD of specifier * (name * expression option) list
  | TYPE_ANNOT of Logic_ptree.type_annot

(* like name_group, except the declared variables are allowed to have initializers *)
(* e.g.: int x=1, y=2; *)
and init_name_group = specifier * init_name list

(* The decl_type is in the order in which they are printed. Only the name of
 * the declared identifier is pulled out. The attributes are those that are
 * printed after the declarator *)
(* e.g: in "int *x", "*x" is the declarator; "x" will be pulled out as *)
(* the string, and decl_type will be PTR([], JUSTBASE) *)
and name = string * decl_type * attribute list * cabsloc

(* A variable declarator ("name") with an initializer *)
and init_name = name * init_expression

(* Single names are for declarations that cannot come in groups, like
 * function parameters and functions *)
and single_name = specifier * name


and enum_item = string * expression * cabsloc

(*
** Declaration definition (at toplevel)
*)
and definition =
   FUNDEF of 
       (Logic_ptree.spec*cabsloc) option * single_name * block *
         cabsloc * cabsloc
 | DECDEF of (Logic_ptree.spec*cabsloc) option * init_name_group * cabsloc
     (* global variable(s), or function prototype *)
 | TYPEDEF of name_group * cabsloc
 | ONLYTYPEDEF of specifier * cabsloc
 | GLOBASM of string * cabsloc
 | PRAGMA of expression * cabsloc
 | LINKAGE of string * cabsloc * definition list (* extern "C" { ... } *)
 | GLOBANNOT of Logic_ptree.decl list
     (** Logical declaration (axiom, logic, etc.)*)
 | CUSTOM of Logic_ptree.custom_tree * string * cabsloc

(** the string is a file name, and then the list of toplevel forms.
    @plugin development guide *)
and file = string * (bool * definition) list


(*
** statements
*)

(* A block contains a list of local label declarations ( GCC's ({ __label__
 * l1, l2; ... }) ) , a list of definitions and a list of statements  *)
and block =
    { blabels: string list;
      battrs: attribute list;
      bstmts: statement list
    }

(* GCC asm directives have lots of extra information to guide the optimizer *)
and asm_details =
    { aoutputs: (string option * string * expression) list; (* optional name, constraints and expressions for outputs *)
      ainputs: (string option * string * expression) list; (* optional name, constraints and expressions for inputs *)
      aclobbers: string list; (* clobbered registers *)
      alabels: string list (* the labels for "asm goto" statements in gcc >= 4.6 *) 
    }

and raw_statement =
   NOP of cabsloc
 | COMPUTATION of expression * cabsloc
 | BLOCK of block * cabsloc * cabsloc
 | SEQUENCE of statement * statement * cabsloc
 | IF of expression * statement * statement * cabsloc
 | WHILE of loop_invariant * expression * statement * cabsloc
 | DOWHILE of loop_invariant * expression * statement * cabsloc
 | FOR of loop_invariant *
          for_clause * expression * expression * statement * cabsloc
 | BREAK of cabsloc
 | CONTINUE of cabsloc
 | RETURN of expression * cabsloc
 | SWITCH of expression * statement * cabsloc
 | CASE of expression * statement * cabsloc
 | CASERANGE of expression * expression * statement * cabsloc
 | DEFAULT of statement * cabsloc
 | LABEL of string * statement * cabsloc
 | GOTO of string * cabsloc
 | COMPGOTO of expression * cabsloc (* GCC's "goto *exp" *)
 | DEFINITION of definition (*definition or declaration of a variable or type*)

 | ASM of attribute list * (* typically only volatile and const *)
          string list * (* template *)
          asm_details option * (* extra details to guide GCC's optimizer *)
          cabsloc

(* Exception mechanism *)
 | THROW of expression option * cabsloc
 (** throws the corresponding expression. [None] corresponds to
     re-throwing the exception currently being catched (thus is only
     meaningful in a catch clause). This node is not generated by the
     C parser, but can be used by external front-ends.
  *)
 | TRY_CATCH of statement * (single_name option * statement) list * cabsloc
   (** [TRY_CATCH(s,clauses,loc)] catches exceptions thrown by execution of 
       [s], according to [clauses]. An
       exception [e] is catched by the first clause
       [(spec,(name, decl, _, _)),body]
       such that the type of [e] is compatible with [(spec,decl)]. [name]
       is then associated to a copy of [e], and [body] is executed. If the
       [single_name] is [None], all exceptions are catched by the corresponding
       clause.

       The corresponding [TryCatch] node in {!Cil_types.stmtkind} has a refined
       notion of catching that allows a clause to match for more than
       one type using appropriate conversions
       (see also {!Cil_types.catch_binder}).

       This node is not generated by the C parser, but can be used by external
       front-ends.
    *)

   (** MS SEH *)
 | TRY_EXCEPT of block * expression * block * cabsloc
 | TRY_FINALLY of block * block * cabsloc
 (* annotations *)
 | CODE_ANNOT of (Logic_ptree.code_annot * cabsloc)
 | CODE_SPEC of (Logic_ptree.spec * cabsloc)

and statement = { mutable stmt_ghost: bool; stmt_node:raw_statement }

and loop_invariant = Logic_ptree.code_annot list

and for_clause =
   FC_EXP of expression
 | FC_DECL of definition

(*
** Expressions
*)
and binary_operator =
    ADD | SUB | MUL | DIV | MOD
  | AND | OR
  | BAND | BOR | XOR | SHL | SHR
  | EQ | NE | LT | GT | LE | GE
  | ASSIGN
  | ADD_ASSIGN | SUB_ASSIGN | MUL_ASSIGN | DIV_ASSIGN | MOD_ASSIGN
  | BAND_ASSIGN | BOR_ASSIGN | XOR_ASSIGN | SHL_ASSIGN | SHR_ASSIGN

and unary_operator =
    MINUS | PLUS | NOT | BNOT | MEMOF | ADDROF
  | PREINCR | PREDECR | POSINCR | POSDECR

and expression = { expr_loc : cabsloc; expr_node: cabsexp }

and cabsexp =
    NOTHING

  | UNARY of unary_operator * expression
  | LABELADDR of string  (* GCC's && Label *)
  | BINARY of binary_operator * expression * expression
  | QUESTION of expression * expression * expression

   (* A CAST can actually be a constructor expression *)
  | CAST of (specifier * decl_type) * init_expression

    (* There is a special form of CALL in which the function called is
       __builtin_va_arg and the second argument is sizeof(T). This
       should be printed as just T *)
  | CALL of expression * expression list
  | COMMA of expression list
  | CONSTANT of constant
  | PAREN of expression
  | VARIABLE of string
  | EXPR_SIZEOF of expression
  | TYPE_SIZEOF of specifier * decl_type
  | EXPR_ALIGNOF of expression
  | TYPE_ALIGNOF of specifier * decl_type
  | INDEX of expression * expression
  | MEMBEROF of expression * string
  | MEMBEROFPTR of expression * string
  | GNU_BODY of block
  | EXPR_PATTERN of string     (* pattern variable, and name *)

and constant =
  | CONST_INT of string   (* the textual representation *)
  | CONST_FLOAT of string (* the textual representaton *)
  | CONST_CHAR of int64 list
  | CONST_WCHAR of int64 list
  | CONST_STRING of string
  | CONST_WSTRING of int64 list
    (* ww: wstrings are stored as an int64 list at this point because
     * we might need to feed the wide characters piece-wise into an
     * array initializer (e.g., wchar_t foo[] = L"E\xabcd";). If that
     * doesn't happen we will convert it to an (escaped) string before
     * passing it to Cil. *)

and init_expression =
  | NO_INIT
  | SINGLE_INIT of expression
  | COMPOUND_INIT of (initwhat * init_expression) list

and initwhat =
    NEXT_INIT
  | INFIELD_INIT of string * initwhat
  | ATINDEX_INIT of expression * initwhat
  | ATINDEXRANGE_INIT of expression * expression


                                        (* Each attribute has a name and some
                                         * optional arguments *)
and attribute = string * expression list

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

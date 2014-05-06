(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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

(** Type of AST's extensible printers. 
    @since Fluorine-20130401
    @plugin development guide *)

open Cil_types

(* ********************************************************************* *)
(** {2 Class type for extensible printer} *)
(* ********************************************************************* *)

(** The class type that a printer must implement. *)
class type extensible_printer_type = object

  (* ******************************************************************* *)
  (** {3 Useful functions for building pretty-printers} *)
  (* ******************************************************************* *)

  val mutable logic_printer_enabled : bool
  (** Local logical annotation (function specifications and code annotations
      are printed only if [logic_printer_enabled] is set to [true]. *)

  val mutable force_brace: bool
  (** If set to [true] (default is [false], some additional braces are
      printed. *)

  val mutable verbose: bool
  (** more info is displayed when on verbose mode. This flag is synchronized
      with Frama-C's kernel being on debug mode. *)

  val mutable is_ghost: bool
  (**  are we printing ghost code? *)

  method reset: unit -> unit

  method private current_function: varinfo option
  (** @return the [varinfo] corresponding to the function being printed *)
    
  method private current_behavior: funbehavior option
  (** @return the [funbehavior] being pretty-printed. *)

  method private has_annot: bool
  (** [true] if [current_stmt] has some annotations attached to it. *)

  method private current_stmt: stmt option
  (** @return the [stmt] being printed *)

  method private may_be_skipped: stmt -> bool
  (** This is called to check that a given statement may be
      compacted with another one.
      For example this is called whenever a [while(1)] followed by a
      conditional [if (cond) break;] may be compacted into [while (cond)]. *)

  method private require_braces: ?has_annot:bool -> block -> bool
  (** @return [true] if the given block must be enclosed in a block.
      [has_annot] indicates if the stmt corresponding to the block may have
      annotations (default is [true]). 
      @modify Fluorine-20130401 optional arguments has been modified. *)

  method private inline_block: ?has_annot:bool -> block -> bool
  (** @return [true] if the given block may be inlined in a single line.
      [has_annot] indicates if the stmt corresponding to the block may have
      annotations (default is [true]).
      @modify Fluorine-20130401 optional arguments has been modified. *)

  method private get_instr_terminator: unit -> string
  (** What terminator to print after an instruction. sometimes we want to
      print sequences of instructions separated by comma *)

  method private set_instr_terminator: string -> unit
    
  method private opt_funspec: Format.formatter -> funspec -> unit

  (* ******************************************************************* *)
  (** {3 Pretty-printing of C code} *)
  (* ******************************************************************* *)

  method location: Format.formatter -> location -> unit

  method constant: Format.formatter -> constant -> unit

  method varname: Format.formatter -> string -> unit
  (** Invoked each time an identifier name is to be printed. Allows for
      various manipulation of the name, such as unmangling. *)

  method vdecl: Format.formatter -> varinfo -> unit
  (** Invoked for each variable declaration. Note that variable
      declarations are all the [GVar], [GVarDecl], [GFun], all the [varinfo]
      in formals of function types, and the formals and locals for function
      definitions. *)

  method varinfo: Format.formatter -> varinfo -> unit
  (** Invoked on each variable use. *)

  method lval: Format.formatter -> lval -> unit
  (** Invoked on each lvalue occurence *)

  method field: Format.formatter -> fieldinfo -> unit

  method offset: Format.formatter -> offset -> unit
  (** Invoked on each offset occurence. The second argument is the base. *)

  method global: Format.formatter -> global -> unit
  (** Global (vars, types, etc.). This can be slow. *)

  method fieldinfo: Format.formatter -> fieldinfo -> unit
  (** A field declaration *)

  method storage: Format.formatter -> storage -> unit
  method ikind: Format.formatter -> ikind -> unit
  method fkind: Format.formatter -> fkind -> unit

  method typ: 
    ?fundecl:varinfo ->
      (Format.formatter -> unit) option -> Format.formatter -> typ -> unit
  (** Use of some type in some declaration.  [fundecl] is the name of the
      function which is declared with the corresponding type.  The second
      argument is used to print the declared element, or is None if we are just
      printing a type with no name being declared.  If [fundecl] is not None,
      second argument must also have a value. *)

  method attrparam:  Format.formatter -> attrparam -> unit
  (** Attribute paramter *)

  method attribute: Format.formatter -> attribute -> bool
  (** Attribute. Also return an indication whether this attribute must be
      printed inside the __attribute__ list or not. *)
    
  method attributes:  Format.formatter -> attributes -> unit
  (** Attribute lists *)

  method label:  Format.formatter -> label -> unit
  (** Label *)

  method line_directive: 
    ?forcefile:bool ->  Format.formatter -> location -> unit
  (** Print a line-number. This is assumed to come always on an empty line. If
      the forcefile argument is present and is true then the file name will be
      printed always. Otherwise the file name is printed only if it is
      different from the last time time this function is called. The last file
      name is stored in a private field inside the cilPrinter object. *)

  method stmt_labels: Format.formatter -> stmt -> unit
  (** Print only the labels of the statement. Used by [annotated_stmt]. *)

  method annotated_stmt: stmt ->  Format.formatter -> stmt -> unit
  (** Print an annotated statement. The code to be printed is given in the
      last {!Cil_types.stmt} argument. The initial {!Cil_types.stmt} argument
      records the statement which follows the one being printed. *)

  method stmtkind: stmt ->  Format.formatter -> stmtkind -> unit
  (** Print a statement kind. The code to be printed is given in the
      {!Cil_types.stmtkind} argument.  The initial {!Cil_types.stmt} argument
      records the statement which follows the one being printed;
      {!defaultCilPrinterClass} uses this information to prettify statement
      printing in certain special cases. The boolean flag indicated whether
      the statement has labels (which have already been printed) *)

  method instr: Format.formatter -> instr -> unit
  (** Invoked on each instruction occurrence. *)

  method stmt: Format.formatter -> stmt -> unit
  (** Control-flow statement. [annot] is [true] iff the printer prints the
      annotations of the stmt. *)

  method next_stmt : stmt -> Format.formatter -> stmt -> unit

  method block: ?braces: bool -> Format.formatter -> block -> unit
  (** Prints a block.
      Enclose the block braces '\{' and '\}' according to the optional
      argument. If it is not set, braces are put only when required.
      @modify Fluorine-20130401 optional arguments has been modified. *)

  method exp:  Format.formatter -> exp -> unit
  (** Print expressions *)

  method unop: Format.formatter -> unop -> unit
  method binop: Format.formatter -> binop -> unit

  method init:  Format.formatter -> init -> unit
  (** Print initializers. This can be slow. *)
  method file: Format.formatter -> file -> unit

  (* ******************************************************************* *)
  (** {3 Pretty-printing of annotations} *)
  (* ******************************************************************* *)

  method logic_constant: Format.formatter -> logic_constant -> unit
  method logic_type:
    (Format.formatter -> unit) option -> Format.formatter -> logic_type 
      -> unit
  method logic_type_def: Format.formatter -> logic_type_def -> unit
  method model_info: Format.formatter -> model_info -> unit
  method term_binop: Format.formatter -> binop -> unit
  method relation: Format.formatter -> relation -> unit
  method identified_term: Format.formatter -> identified_term -> unit
  method term: Format.formatter -> term -> unit
  method term_node: Format.formatter -> term -> unit
  method term_lval: Format.formatter -> term_lval -> unit
  method model_field: Format.formatter -> model_info -> unit
  method term_offset: Format.formatter -> term_offset -> unit
  method logic_label:  Format.formatter -> logic_label -> unit
  method logic_info: Format.formatter -> logic_info -> unit
  method logic_var: Format.formatter -> logic_var -> unit
  method quantifiers: Format.formatter -> quantifiers -> unit
  method predicate: Format.formatter -> predicate -> unit
  method predicate_named: Format.formatter -> predicate named -> unit
  method identified_predicate: 
    Format.formatter -> identified_predicate -> unit
  method behavior: Format.formatter -> funbehavior -> unit
  method requires: Format.formatter -> identified_predicate -> unit
  method complete_behaviors: Format.formatter -> string list -> unit
  method disjoint_behaviors: Format.formatter -> string list -> unit
  method terminates: Format.formatter -> identified_predicate -> unit

  method post_cond: 
    Format.formatter -> (termination_kind * identified_predicate) -> unit
  (** pretty prints a post condition according to the exit kind it represents
      @modify Boron-20100401 replaces [pEnsures] *)

  method assumes: Format.formatter -> identified_predicate -> unit

  method funspec: Format.formatter -> funspec -> unit

  method assigns:
    string -> Format.formatter -> identified_term assigns -> unit
  (** first parameter is the introducing keyword (e.g. loop_assigns or
      assigns). *)

  method allocation:
    isloop:bool -> Format.formatter -> identified_term allocation -> unit
  (** first parameter is the introducing keyword
      (e.g. loop_allocates, loop_frees, allocates or free)
      @since Oxygen-20120901. *)

  method from: string -> Format.formatter -> identified_term from -> unit
  (** prints an assignment with its dependencies. *)

  method code_annotation: Format.formatter -> code_annotation -> unit
  method global_annotation: Format.formatter -> global_annotation -> unit
  method decreases: Format.formatter -> term variant -> unit
  method variant: Format.formatter -> term variant -> unit

  (* ******************************************************************* *)
  (** {3 Modifying pretty-printer behavior}                              *)
  (* ******************************************************************* *)

  method without_annot:
    'a.
    (Format.formatter -> 'a -> unit) ->
    Format.formatter -> 
    'a -> 
    unit
  (** [self#without_annot printer fmt x] pretty prints [x] by using [printer],
      without pretty-printing its function contracts and code annotations. *)

  method force_brace:
    'a.
    (Format.formatter -> 'a -> unit) ->
    Format.formatter -> 
    'a -> 
    unit
(** [self#force_brace printer fmt x] pretty prints [x] by using [printer],
    but add some extra braces '\{' and '\}' which are hidden by default. *)

end

(* ********************************************************************* *)
(** {2 Types for customizing pretty printers} *)
(* ********************************************************************* *)

(** Styles of printing line directives *)
type line_directive_style =
  | Line_comment (** Before every element, print the line number in
                     comments. This is ignored by processing tools
                     (thus errors are reproted in the CIL output),
                     but useful for visual inspection *)
  | Line_comment_sparse (** Like LineComment but only print a line directive for
                            a new source line *)
  | Line_preprocessor_input  (** Use #line directives *)
  | Line_preprocessor_output (** Use # nnn directives (in gcc mode) *)

type state =
    { (** How to print line directives *)
      mutable line_directive_style: line_directive_style option;
      (** Whether we print something that will only be used as input to Cil's
	  parser. In that case we are a bit more liberal in what we print. *)
      mutable print_cil_input: bool;
      (** Whether to print the CIL as they are, without trying to be smart and
	  print nicer code. Normally this is false, in which case the pretty
	  printer will turn the while(1) loops of CIL into nicer loops, will not
	  print empty "else" blocks, etc. These is one case howewer in which if
	  you turn this on you will get code that does not compile: if you use
	  varargs the __builtin_va_arg function will be printed in its internal
	  form. *)
      mutable print_cil_as_is: bool;
      (** The length used when wrapping output lines. Setting this variable to
	  a large integer will prevent wrapping and make #line directives more
	  accurate. *)
      mutable line_length: int;
      (** Emit warnings when truncating integer constants (default true) *)
      mutable warn_truncate: bool }

(* ********************************************************************* *)
(** {2 Functions for pretty printing} *)
(* ********************************************************************* *)

module type S = sig

  val pp_varname: Format.formatter -> string -> unit

  (* ********************************************************************* *)
  (** {3 Printer for C constructs} *)
  (* ********************************************************************* *)

  val pp_location: Format.formatter -> location -> unit
  val pp_constant: Format.formatter -> constant -> unit

  val pp_storage: Format.formatter -> storage -> unit
  val pp_ikind: Format.formatter -> ikind -> unit
  val pp_fkind: Format.formatter -> fkind -> unit

  val pp_typ: Format.formatter -> typ -> unit
  val pp_exp: Format.formatter -> exp -> unit
  val pp_varinfo: Format.formatter -> varinfo -> unit
  val pp_lval: Format.formatter -> lval -> unit
  val pp_field: Format.formatter -> fieldinfo -> unit
  val pp_offset: Format.formatter -> offset -> unit
  val pp_init: Format.formatter -> init -> unit
  val pp_binop: Format.formatter -> binop -> unit
  val pp_unop: Format.formatter -> unop -> unit
  val pp_attribute: Format.formatter -> attribute -> unit
  val pp_attrparam: Format.formatter -> attrparam -> unit
  val pp_attributes: Format.formatter -> attributes -> unit
  val pp_instr: Format.formatter -> instr -> unit
  val pp_label: Format.formatter -> label -> unit
  val pp_stmt: Format.formatter -> stmt -> unit
  val pp_block: Format.formatter -> block -> unit
  val pp_global: Format.formatter -> global -> unit
  val pp_file: Format.formatter -> file -> unit

  (* ********************************************************************* *)
  (** {3 Printer for ACSL constructs} *)
  (* ********************************************************************* *)

  val pp_relation: Format.formatter -> relation -> unit

  val pp_model_info: Format.formatter -> model_info -> unit
  (** @since Oxygen-20120901 *)

  val pp_term_lval: Format.formatter -> term_lval -> unit
  val pp_logic_var: Format.formatter -> logic_var -> unit
  val pp_logic_type: Format.formatter -> logic_type -> unit
  val pp_identified_term:  Format.formatter -> identified_term -> unit
  val pp_term:  Format.formatter -> term -> unit
  val pp_model_field: Format.formatter -> model_info -> unit
  val pp_term_offset: Format.formatter -> term_offset -> unit
  val pp_logic_label: Format.formatter -> logic_label -> unit

  val pp_predicate: Format.formatter -> predicate -> unit
  val pp_predicate_named: Format.formatter -> predicate named -> unit
  val pp_identified_predicate: Format.formatter -> identified_predicate -> unit
  val pp_code_annotation: Format.formatter -> code_annotation -> unit
  val pp_funspec: Format.formatter -> funspec -> unit
  val pp_behavior: Format.formatter -> funbehavior -> unit
  val pp_global_annotation: Format.formatter -> global_annotation -> unit
  val pp_decreases: Format.formatter -> term variant -> unit
  val pp_variant: Format.formatter -> term variant -> unit
  val pp_from: Format.formatter -> identified_term from -> unit
  val pp_assigns: Format.formatter -> identified_term assigns -> unit

  val pp_allocation: Format.formatter -> identified_term allocation -> unit
  (** @since Oxygen-20120901 *)

  val pp_loop_from: Format.formatter -> identified_term from -> unit
  val pp_loop_assigns: Format.formatter -> identified_term assigns -> unit

  val pp_loop_allocation: Format.formatter -> identified_term allocation -> unit
  (** @since Oxygen-20120901 *)

  val pp_post_cond: 
    Format.formatter -> (termination_kind * identified_predicate) -> unit

  (* ********************************************************************* *)
  (** {3 General form of printers} *)
  (* ********************************************************************* *)

  val pp_full_assigns:
    string -> Format.formatter -> identified_term assigns -> unit
  (** first parameter is the introducing keyword (e.g. loop_assigns or
      assigns). *)

  val without_annot:
    (Format.formatter -> 'a -> unit) ->
    Format.formatter -> 
    'a -> 
    unit
  (** [without_annot printer fmt x] pretty prints [x] by using [printer],
      without pretty-printing its function contracts and code annotations. *)

  val force_brace:
    (Format.formatter -> 'a -> unit) ->
    Format.formatter -> 
    'a -> 
    unit
  (** [self#force_brace printer fmt x] pretty prints [x] by using [printer],
      but add some extra braces '\{' and '\}' which are hidden by default. *)

  (* ********************************************************************* *)
  (** {3 Extensible printer} *)
  (* ********************************************************************* *)

  class extensible_printer: unit -> extensible_printer_type
  (** Extend this class if you want to modify the default behavior of the
      printer. *)

  val change_printer: (unit -> extensible_printer) -> unit
  (** [change_printer extension] let the pp_* function use 
      [extension] as printer *)

  val printer: unit -> extensible_printer
(** @return the current printer.
    @since Neon-20130301 *)

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

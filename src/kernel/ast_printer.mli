(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

(** Default printers of Frama-C. Initialized with Cil default printers, and
    updated by the {!Printer} module.
    @plugin development guide *)

open Cil_types

(** Pretty prints a binary operator *)
val d_binop: (Format.formatter -> binop -> unit) ref

(** Pretty prints a binary relation *)
val d_relation: (Format.formatter -> relation -> unit) ref

(** Pretty prints an identifier *)
val d_ident: (Format.formatter -> string -> unit) ref

(** Pretty-print an expression using {!Printer.default_printer}  *)
val d_exp: (Format.formatter -> exp -> unit) ref

val d_var: (Format.formatter -> varinfo -> unit) ref

val d_type: (Format.formatter -> typ -> unit) ref

(** Pretty-print an lvalue using {!Printer.default_printer}   *)
val d_lval: (Format.formatter -> lval -> unit) ref

(** Pretty-print an offset using {!Printer.default_printer}, given the pretty
 * printing for the base.   *)
val d_offset: (Format.formatter -> offset -> unit) ref

(** Pretty-print an initializer using {!Printer.default_printer}.  This can be
 * extremely slow (or even overflow the stack) for huge initializers. *)
val d_init: (Format.formatter -> init -> unit) ref

(** Pretty-print an attribute using {!Printer.default_printer}  *)
val d_attr: (Format.formatter -> attribute -> unit) ref

(** Pretty-print an argument of an attribute using {!Printer.default_printer}  *)
val d_attrparam: (Format.formatter -> attrparam -> unit) ref

(** Pretty-print a list of attributes using {!Printer.default_printer}  *)
val d_attrlist: (Format.formatter -> attributes -> unit) ref

(** Pretty-print an instruction using {!Printer.default_printer}   *)
val d_instr: (Format.formatter -> instr -> unit) ref

(** Pretty-print a label using {!Printer.default_printer} *)
val d_label: (Format.formatter -> label -> unit) ref

(** Pretty-print a statement using {!Printer.default_printer}. This can be
 * extremely slow (or even overflow the stack) for huge statements. *)
val d_stmt: (Format.formatter -> stmt -> unit) ref

(** Pretty-print a block using {!Printer.default_printer}. This can be
 * extremely slow (or even overflow the stack) for huge blocks. *)
val d_block: (Format.formatter -> block -> unit) ref

(** Pretty-print the internal representation of a global using
 * {!Printer.default_printer}. This can be extremely slow (or even overflow the
 * stack) for huge globals (such as arrays with lots of initializers). *)
val d_global: (Format.formatter -> global -> unit) ref

val d_term_lval: (Format.formatter -> term_lval -> unit) ref
val d_logic_var: (Format.formatter -> logic_var -> unit) ref
val d_logic_type: (Format.formatter -> logic_type -> unit) ref
val d_term: (Format.formatter -> term -> unit) ref
val d_term_offset: (Format.formatter -> term_offset -> unit) ref

val d_predicate_named: (Format.formatter -> predicate named -> unit) ref
val d_code_annotation: (Format.formatter -> code_annotation -> unit) ref
val d_rooted_code_annotation:
  (Format.formatter -> rooted_code_annotation -> unit) ref
val d_funspec: (Format.formatter -> funspec -> unit) ref
val d_annotation: (Format.formatter -> global_annotation -> unit) ref

val d_file: (Format.formatter -> file -> unit) ref

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

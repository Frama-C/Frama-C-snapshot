(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

val pp_list : 'a Pretty_utils.formatter -> 'a list Pretty_utils.formatter
val pp_option : 'a Pretty_utils.formatter -> 'a option Pretty_utils.formatter
val pp_ref :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a ref -> unit
val pp_pair :
  'a Pretty_utils.formatter ->
  'b Pretty_utils.formatter -> ('a * 'b) Pretty_utils.formatter
val pp_tuple3 :
  ?pre:('a, 'b, 'c, 'd, 'd, 'a) format6 ->
  ?sep:('e, 'f, 'g, 'h, 'h, 'e) format6 ->
  ?suf:('i, 'j, 'k, 'l, 'l, 'i) format6 ->
  (Format.formatter -> 'm -> unit) ->
  (Format.formatter -> 'n -> unit) ->
  (Format.formatter -> 'o -> unit) ->
  Format.formatter -> 'm * 'n * 'o -> unit
val pp_tuple4 :
  ?pre:('a, 'b, 'c, 'd, 'd, 'a) format6 ->
  ?sep:('e, 'f, 'g, 'h, 'h, 'e) format6 ->
  ?suf:('i, 'j, 'k, 'l, 'l, 'i) format6 ->
  (Format.formatter -> 'm -> unit) ->
  (Format.formatter -> 'n -> unit) ->
  (Format.formatter -> 'o -> unit) ->
  (Format.formatter -> 'p -> unit) ->
  Format.formatter -> 'm * 'n * 'o * 'p -> unit
val pp_tuple5 :
  ?pre:('a, 'b, 'c, 'd, 'd, 'a) format6 ->
  ?sep:('e, 'f, 'g, 'h, 'h, 'e) format6 ->
  ?suf:('i, 'j, 'k, 'l, 'l, 'i) format6 ->
  (Format.formatter -> 'm -> unit) ->
  (Format.formatter -> 'n -> unit) ->
  (Format.formatter -> 'o -> unit) ->
  (Format.formatter -> 'p -> unit) ->
  (Format.formatter -> 'q -> unit) ->
  Format.formatter -> 'm * 'n * 'o * 'p * 'q -> unit
val pp_integer : Format.formatter -> Integer.t -> unit
val pp_int64 : Format.formatter -> int64 -> unit
val pp_string : Format.formatter -> string -> unit
val pp_bool : Format.formatter -> bool -> unit
val pp_int : Format.formatter -> int -> unit
val pp_char : Format.formatter -> char -> unit
val pp_float : Format.formatter -> float -> unit
val pp_variant :
  'a Pretty_utils.formatter -> ('a * string option) Pretty_utils.formatter
val pp_allocation :
  Format.formatter -> Cil_types.allocation -> unit
val pp_deps :
  Format.formatter -> Cil_types.deps -> unit
val pp_from :
  (Cil_types.identified_term * Cil_types.deps) Pretty_utils.formatter
val pp_assigns :
  Cil_types.from Pretty_utils.formatter ->
  Format.formatter -> Cil_types.assigns -> unit
val pp_file : Format.formatter -> Cil_types.file -> unit
val pp_global : Format.formatter -> Cil_types.global -> unit
val pp_typ : Cil_types.typ Pretty_utils.formatter
val pp_ikind : Format.formatter -> Cil_types.ikind -> unit
val pp_fkind : Format.formatter -> Cil_types.fkind -> unit
val pp_bitsSizeofTyp : Format.formatter -> Cil_types.bitsSizeofTyp -> unit
val pp_bitsSizeofTypCache :
  Format.formatter -> Cil_types.bitsSizeofTypCache -> unit
val pp_attribute : Cil_types.attribute Pretty_utils.formatter
val pp_attributes : Format.formatter -> Cil_types.attributes -> unit
val pp_attrparam : Cil_types.attrparam Pretty_utils.formatter
val pp_compinfo : Format.formatter -> Cil_types.compinfo -> unit
val pp_fieldinfo : Format.formatter -> Cil_types.fieldinfo -> unit
val pp_enuminfo : Format.formatter -> Cil_types.enuminfo -> unit
val pp_enumitem : Format.formatter -> Cil_types.enumitem -> unit
val pp_typeinfo : Format.formatter -> Cil_types.typeinfo -> unit
val pp_varinfo : Cil_types.varinfo Pretty_utils.formatter
val pp_storage : Format.formatter -> Cil_types.storage -> unit
val pp_exp : Cil_types.exp Pretty_utils.formatter
val pp_exp_node : Format.formatter -> Cil_types.exp_node -> unit
val pp_exp_info : Format.formatter -> Cil_types.exp_info -> unit
val pp_constant : Format.formatter -> Cil_types.constant -> unit
val pp_unop : Format.formatter -> Cil_types.unop -> unit
val pp_binop : Format.formatter -> Cil_types.binop -> unit
val pp_lval : Cil_types.lval Pretty_utils.formatter
val pp_lhost : Cil_types.lhost Pretty_utils.formatter
val pp_offset : Cil_types.offset Pretty_utils.formatter
val pp_init : Cil_types.init Pretty_utils.formatter
val pp_initinfo : Format.formatter -> Cil_types.initinfo -> unit
val pp_fundec : Format.formatter -> Cil_types.fundec -> unit
val pp_block : Cil_types.block Pretty_utils.formatter
val pp_stmt : Cil_types.stmt Pretty_utils.formatter
val pp_label : Format.formatter -> Cil_types.label -> unit
val pp_stmtkind : Format.formatter -> Cil_types.stmtkind -> unit
val pp_catch_binder : Cil_types.catch_binder Pretty_utils.formatter
val pp_instr : Cil_types.instr Pretty_utils.formatter
val pp_extended_asm : Cil_types.extended_asm Pretty_utils.formatter
val pp_lexing_position : Format.formatter -> Lexing.position -> unit
val pp_location : Format.formatter -> Cil_types.location -> unit
val pp_logic_constant : Format.formatter -> Cil_types.logic_constant -> unit
val pp_logic_real : Format.formatter -> Cil_types.logic_real -> unit
val pp_logic_type : Cil_types.logic_type Pretty_utils.formatter
val pp_identified_term : Cil_types.identified_term Pretty_utils.formatter
val pp_logic_label : Cil_types.logic_label Pretty_utils.formatter
val pp_logic_builtin_label :
  Cil_types.logic_builtin_label Pretty_utils.formatter
val pp_term : Cil_types.term Pretty_utils.formatter
val pp_term_node : Format.formatter -> Cil_types.term_node -> unit
val pp_term_lval : Format.formatter -> Cil_types.term_lval -> unit
val pp_term_lhost : Cil_types.term_lhost Pretty_utils.formatter
val pp_model_info : Format.formatter -> Cil_types.model_info -> unit
val pp_term_offset : Cil_types.term_offset Pretty_utils.formatter
val pp_logic_info : Format.formatter -> Cil_types.logic_info -> unit
val pp_builtin_logic_info : Format.formatter -> 'a -> unit
val pp_logic_body : Format.formatter -> Cil_types.logic_body -> unit
val pp_logic_type_info :
  Format.formatter -> Cil_types.logic_type_info -> unit
val pp_logic_type_def : Format.formatter -> Cil_types.logic_type_def -> unit
val pp_logic_var_kind : Format.formatter -> Cil_types.logic_var_kind -> unit
val pp_logic_var : Cil_types.logic_var Pretty_utils.formatter
val pp_logic_ctor_info : Cil_types.logic_ctor_info Pretty_utils.formatter
val pp_quantifiers : Format.formatter -> Cil_types.quantifiers -> unit
val pp_relation : Format.formatter -> Cil_types.relation -> unit
val pp_predicate_node : Format.formatter -> Cil_types.predicate_node -> unit
val pp_identified_predicate : Format.formatter -> Cil_types.identified_predicate -> unit
val pp_predicate : Cil_types.predicate Pretty_utils.formatter
val pp_spec : Format.formatter -> Cil_types.spec -> unit
val pp_acsl_extension : Format.formatter -> Cil_types.acsl_extension -> unit
val pp_acsl_extension_kind :
  Cil_types.acsl_extension_kind Pretty_utils.formatter
val pp_behavior : Format.formatter -> 'a -> unit
val pp_termination_kind :
  Format.formatter -> Cil_types.termination_kind -> unit
val pp_loop_pragma :
  Cil_types.term Pretty_utils.formatter ->
  Format.formatter -> Cil_types.loop_pragma -> unit
val pp_slice_pragma :
  Cil_types.term Pretty_utils.formatter ->
  Format.formatter -> Cil_types.slice_pragma -> unit
val pp_impact_pragma :
  Cil_types.term Pretty_utils.formatter ->
  Format.formatter -> Cil_types.impact_pragma -> unit
val pp_pragma :
  Cil_types.term Pretty_utils.formatter ->
  Format.formatter -> Cil_types.pragma -> unit
val pp_code_annotation_node :
  Format.formatter -> Cil_types.code_annotation_node -> unit
val pp_funspec : Format.formatter -> Cil_types.funspec -> unit
val pp_code_annotation : Cil_types.code_annotation Pretty_utils.formatter
val pp_funbehavior : Format.formatter -> 'a -> unit
val pp_global_annotation : Cil_types.global_annotation Pretty_utils.formatter
val pp_custom_tree : Format.formatter -> Cil_types.custom_tree -> unit
val pp_kinstr : Format.formatter -> Cil_types.kinstr -> unit
val pp_cil_function : Format.formatter -> Cil_types.cil_function -> unit
val pp_kernel_function : Format.formatter -> 'a -> unit
val pp_localisation : Format.formatter -> Cil_types.localisation -> unit
val pp_mach : Format.formatter -> 'a -> unit

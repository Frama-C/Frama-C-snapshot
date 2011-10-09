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

let d_ident = ref Format.pp_print_string
let d_binop = ref Cil.d_binop
let d_relation = ref Cil.d_relation
let d_exp = ref Cil.d_exp
let d_var = ref Cil.d_var
let d_lval = ref Cil.d_lval
let d_offset = ref Cil.d_offset
let d_init = ref Cil.d_init
let d_type = ref Cil.d_type
let d_global = ref Cil.d_global
let d_attrlist = ref Cil.d_attrlist
let d_attr = ref Cil.d_attr
let d_attrparam = ref Cil.d_attrparam
let d_label = ref Cil.d_label
let d_stmt = ref Cil.d_stmt
let d_block = ref Cil.d_block
let d_instr = ref Cil.d_instr

let d_term_lval = ref Cil.d_term_lval
let d_logic_var = ref Cil.d_logic_var
let d_logic_type = ref Cil.d_logic_type
let d_term = ref Cil.d_term
let d_term_offset = ref Cil.d_term_offset

let d_predicate_named = ref Cil.d_predicate_named
let d_code_annotation = ref Cil.d_code_annotation

let d_rooted_code_annotation =
  ref
    (fun fmt p -> match p with
    | Cil_types.User p
    | Cil_types.AI (_,p) -> Cil.d_code_annotation fmt p)

let d_funspec = ref Cil.d_funspec
let d_annotation = ref Cil.d_annotation

let d_file = ref (Cil.d_file Cil.defaultCilPrinter)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

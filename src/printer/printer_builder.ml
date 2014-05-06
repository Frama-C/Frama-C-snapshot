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

module Make
  (P: sig class printer: unit -> Printer_api.extensible_printer_type end) = 
struct

  class extensible_printer = P.printer
  let mk_printer = ref (new extensible_printer)
  let printer_ref = ref None
  let printer () = match !printer_ref with
    | None ->
      let p = !mk_printer () in
      printer_ref := Some p;
      p#reset ();
      p
    | Some p ->
      p#reset ();
      p
    
  let change_printer new_printer = 
    mk_printer := new_printer;
    printer_ref := None

  let without_annot f fmt x = (printer ())#without_annot f fmt x
  let force_brace f fmt x = (printer ())#force_brace f fmt x

  let pp_varname fmt x = (printer())#varname fmt x

  (* eta-expansion required for applying side-effect of [printer ()] at the
     right time *)
  let pp_location fmt x = (printer ())#location fmt x
  let pp_constant fmt x = (printer ())#constant fmt x
  let pp_ikind fmt x = (printer ())#ikind fmt x
  let pp_fkind fmt x = (printer ())#fkind fmt x
  let pp_storage fmt x = (printer ())#storage fmt x
  let pp_typ fmt x = (printer ())#typ None fmt x
  let pp_exp fmt x = (printer ())#exp fmt x
  let pp_varinfo fmt x = (printer ())#varinfo fmt x
  let pp_lval fmt x = (printer ())#lval fmt x
  let pp_field fmt x = (printer())#field fmt x
  let pp_offset fmt x = (printer ())#offset fmt x
  let pp_init fmt x = (printer ())#init fmt x
  let pp_binop fmt x = (printer ())#binop fmt x
  let pp_unop fmt x = (printer ())#unop fmt x
  let pp_attribute fmt x = ignore ((printer ())#attribute fmt x)
  let pp_attrparam fmt x = (printer ())#attrparam fmt x
  let pp_attributes fmt x = (printer ())#attributes fmt x
  let pp_instr fmt x = (printer ())#instr fmt x
  let pp_label fmt x = (printer ())#label fmt x
  let pp_logic_label fmt x = (printer ())#logic_label fmt x
  let pp_stmt fmt x = (printer ())#stmt fmt x
  let pp_block fmt x = (printer ())#block fmt x
  let pp_global fmt x = (printer ())#global fmt x
  let pp_file fmt x = (printer ())#file fmt x
  let pp_relation fmt x = (printer ())#relation fmt x
  let pp_model_info fmt x = (printer ())#model_info fmt x
  let pp_term_lval fmt x = (printer ())#term_lval fmt x
  let pp_logic_var fmt x = (printer ())#logic_var fmt x
  let pp_logic_type fmt x = (printer ())#logic_type None fmt x
  let pp_identified_term fmt x = (printer ())#identified_term fmt x
  let pp_term fmt x = (printer ())#term fmt x
  let pp_model_field fmt x = (printer())#model_field fmt x
  let pp_term_offset fmt x = (printer ())#term_offset fmt x
  let pp_predicate fmt x = (printer ())#predicate fmt x
  let pp_predicate_named fmt x = (printer ())#predicate_named fmt x
  let pp_identified_predicate fmt x = (printer ())#identified_predicate fmt x
  let pp_code_annotation fmt x = (printer ())#code_annotation fmt x
  let pp_funspec fmt x = (printer ())#funspec fmt x
  let pp_behavior fmt x = (printer ())#behavior fmt x
  let pp_global_annotation fmt x = (printer ())#global_annotation fmt x
  let pp_decreases fmt x = (printer ())#decreases fmt x
  let pp_variant fmt x = (printer ())#variant fmt x
  let pp_from fmt x = (printer ())#from "assigns" fmt x
  let pp_full_assigns fmt x = (printer ())#assigns fmt x
  let pp_assigns = pp_full_assigns "assigns"
  let pp_allocation fmt x = (printer ())#allocation ~isloop:false fmt x
  let pp_loop_from fmt x = (printer ())#from "loop assigns" fmt x
  let pp_loop_assigns fmt x = (printer ())#assigns "loop assigns" fmt x
  let pp_loop_allocation fmt x = (printer ())#allocation ~isloop:true fmt x
  let pp_post_cond fmt x = (printer ())#post_cond fmt x

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

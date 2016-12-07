(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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

open Cil_types

let dkey = Kernel.register_category "widen-hints"

let error ?msg loc typing_context =
  typing_context.Logic_typing.error loc
    "invalid widen_hints annotation%a"
    (Pretty_utils.pp_opt ~pre:": " Format.pp_print_string) msg

type hint_lval = {
  vars : lval option;
  names : string list;
  loc : Cil_datatype.Location.t;
}

type t = hint_lval * term list

(* Textual representation of the hint corresponding to "widen all variables". *)
let all_vars_str = "\"all\""

let pp_olval fmt olv =
  match olv with
  | None -> Format.fprintf fmt "%s" all_vars_str
  | Some lv -> Format.fprintf fmt "%a" Printer.pp_lval lv

exception Parse_error of string option

let parse_error ?msg () = raise (Parse_error msg)

(* Raises [parse_error] if [var] is not in the typing context,
   or if [var] is a logic variable. *)
let check_var typing_context var =
  try
    let lv = typing_context.Logic_typing.find_var var in
    match lv.lv_origin with
    | None ->
      parse_error ~msg:("cannot add widen hint on logic variable: " ^ var) ()
    | Some _vi -> ()
  with Not_found ->
    parse_error ~msg:("variable '"^ var ^"' not found") ()

let term_of_var typing_context var lexpr =
  check_var typing_context var;
  typing_context.Logic_typing.type_term typing_context.Logic_typing.pre_state lexpr

(* Converts a [lexpr] list into a [term] list.
   Requires a non-empty list.
   Note that the hints are not actually parsed, i.e. they may be
   syntactically invalid. We cannot parse them here because global variables
   are not available yet, so we defer parsing to Value. *)
let terms_of_hints typing_context hints =
  if hints = [] then parse_error ~msg:"no hints" ()
  else
    List.map (fun hint ->
        (typing_context.Logic_typing.type_term
           typing_context.Logic_typing.pre_state hint))
      hints

let rec parse_lval typing_context loc arg =
  let open Logic_ptree in
  match arg.lexpr_node with
  | PLnamed (name, node) (* global:x *) ->
    if name <> "global" then parse_error ~msg:("invalid label " ^ name) ()
    else
      let term = parse_lval typing_context loc node in
      { term with term_name = [name] }
  | PLconstant (StringConstant str) when str = "all" (* "all" variables *) ->
    Logic_const.tstring ~loc all_vars_str
  | PLvar var (* x *)
  | PLdot ({lexpr_node = PLvar var}, _) (* x.f *) ->
    term_of_var typing_context var arg
  | _ -> parse_error ~msg:(Format.asprintf "unknown expression: %a" Logic_print.print_lexpr arg) ()

(* Converts the parsing tokens to a list of terms. May raise Kernel.error. *)
let terms_of_parsed_widen_hints typing_context loc args =
  try
    match args with
    | arg :: hints ->
      let var = parse_lval typing_context loc arg in
      var, terms_of_hints typing_context hints
    | [] -> parse_error ()
  with Parse_error msg ->
    error ?msg loc typing_context

exception Invalid_hint

(* given a list of terms [var_term :: hint_terms], returns
   Some (var_string, hint_terms) or None in case of an error. *)
let widen_hint_terms_of_terms terms =
  try
    match terms with
    | var_term :: hint_terms ->
      begin
        match var_term with
        | {term_name; term_node = TConst (LStr var_str)} when var_str = all_vars_str ->
          let named_lval = {names = term_name; loc = var_term.term_loc; vars = None} in
          Some (named_lval, hint_terms)
        | {term_node = TLval (TVar lv, toffs)} ->
          (* we know that lv_origin <> None because we tested it when
             constructing the hint term *)
          let lval =
            !Db.Properties.Interp.term_lval_to_lval ~result:None (TVar lv, toffs)
          in
          let hint_lval =
            { names = var_term.term_name; loc = var_term.term_loc;
              vars = Some lval }
          in
          Some (hint_lval, hint_terms)
        | {term_node = TLval ((TMem {term_node = TLval (TVar _lv, _)}, _toffs) as _tlv)} ->
          failwith "widen_hints with pointers not yet supported"
        (*let lval =
            !Db.Properties.Interp.term_lval_to_lval ~result:None tlv
          in
          let named_lval =
            { Cil_types.name = var_term.term_name; loc = var_term.term_loc;
              content = Some lval }
          in
          Some (named_lval, hint_terms)*)
        | _ ->
          Kernel.debug ~source:(fst var_term.term_loc) ~dkey
            "invalid var_term: %a@." Printer.pp_term var_term;
          raise Invalid_hint
      end
    | _ ->
      Kernel.debug ~dkey "invalid terms: %a@."
        (Pretty_utils.pp_list ~sep:", " Printer.pp_term) terms;
      raise Invalid_hint
  with
    Invalid_hint -> None

let () = Logic_typing.register_behavior_extension "widen_hints"
    (fun ~typing_context ~loc args ->
       let var_term, hint_terms =
         terms_of_parsed_widen_hints typing_context loc args
       in
       let terms = var_term :: hint_terms in
       Ext_terms terms
    )

let () = Cil_printer.register_behavior_extension "widen_hints"
    (fun _pp fmt ext ->
       match ext with
       | Ext_id _ -> assert false
       | Ext_preds _ -> assert false
       | Ext_terms terms ->
         match widen_hint_terms_of_terms terms with
         | Some (hint_lval, hint_terms) ->
           Format.fprintf fmt "%a%a, %a"
             (Pretty_utils.pp_list ~sep:" " ~suf:":" Format.pp_print_string)
             hint_lval.names pp_olval hint_lval.vars
             (Pretty_utils.pp_list ~sep:", " Printer.pp_term) hint_terms
         | None ->
           Format.fprintf fmt "<invalid widen_hints>"
    )

let get_stmt_widen_hint_terms s =
  Annotations.fold_code_annot
    (fun _emitter annot acc ->
       let wh =
         match annot with
         | {annot_content = AStmtSpec (_, { spec_behavior = [{b_extended}]})} ->
           (* non-loop widen_hints *)
           let all_widen_hints_terms =
             Extlib.filter_map
               (fun (name, _) -> name = "widen_hints")
               (fun (_, ext) ->
                  match ext with
                  | Ext_id _ -> assert false
                  | Ext_preds _ -> assert false
                  | Ext_terms terms -> terms)
               b_extended
           in
           let filter_opt l =
             List.fold_left (fun acc o -> match o with
                 | None -> acc
                 | Some e -> e :: acc
               ) [] l
           in
           filter_opt (List.map widen_hint_terms_of_terms all_widen_hints_terms)
         | {annot_content =
              AExtended (_, ("widen_hints", Ext_terms terms))} ->
           (* loop widen_hints *)
           begin
             match widen_hint_terms_of_terms terms with
             | None -> []
             | Some t -> [t]
           end
         | _ -> []
       in
       acc @ wh
    ) s []

let is_global (hlv, _wh) =
  List.mem "global" hlv.names

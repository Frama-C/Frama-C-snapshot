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

open Cil_types

let dkey = Kernel.register_category "widen-hints"

let error ?msg loc typing_context =
  typing_context.Logic_typing.error loc
    "invalid widen_hints annotation%a"
    (Pretty_utils.pp_opt ~pre:": " Format.pp_print_string) msg

type hint_vars =
  | HintAllVars (* "all" vars: static hint *)
  | HintVar of varinfo (* static hint *)
  | HintMem of exp * offset (* dynamic hint *)

type hint_lval = {
  vars : hint_vars;
  names : string list;
  loc : Cil_datatype.Location.t;
}

type t = hint_lval * term list

(* Textual representation of the hint corresponding to "widen all variables". *)
let all_vars_str = "\"all\""

let pp_hvars fmt = function
  | HintAllVars -> Format.fprintf fmt "%s" all_vars_str
  | HintVar vi -> Format.fprintf fmt "%a" Printer.pp_varinfo vi
  | HintMem (e, offset) -> Format.fprintf fmt "%a" Printer.pp_lval (Mem e, offset)

exception Parse_error of string option

let parse_error ?msg () = raise (Parse_error msg)

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
           typing_context typing_context.Logic_typing.pre_state hint))
      hints

(* Parses [arg] using [typing_context].
   This function filters special cases ("all" variables, global label)
   to parse them using specific rules.
   All other cases are redispatched to the standard logic typer. *)
let rec parse_lval typing_context loc arg =
  let open Logic_ptree in
  match arg.lexpr_node with
  | PLnamed (name, node) (* global:x *) ->
    if name <> "global" then
      parse_error ~msg:("invalid label " ^ name) ()
    else
      let term = parse_lval typing_context loc node in
      { term with term_name = [name] }
  | PLconstant (StringConstant str) when str = "all" (* "all" variables *) ->
    Logic_const.tstring ~loc all_vars_str
  | _ ->
    let open Logic_typing in
    typing_context.type_term typing_context typing_context.pre_state arg

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
    | lval_term :: hint_thresholds ->
      begin
        match lval_term with
        | {term_name; term_node = TConst (LStr s)} when s = all_vars_str ->
          let named_lval =
            {names = term_name; loc = lval_term.term_loc; vars = HintAllVars}
          in
          Some (named_lval, hint_thresholds)
        | {term_node = TLval tlv} ->
          let (lhost, offset) =
            !Db.Properties.Interp.term_lval_to_lval ~result:None tlv
          in
          let hint_vars = match lhost with
            | Mem e -> HintMem (e, offset)
            | Var vi -> HintVar vi
          in
          let hint_lval =
            { names = lval_term.term_name; loc = lval_term.term_loc;
              vars = hint_vars }
          in
          Some (hint_lval, hint_thresholds)
        | _ ->
          Kernel.debug ~source:(fst lval_term.term_loc) ~dkey
            "invalid var_term: %a@." Printer.pp_term lval_term;
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
             hint_lval.names pp_hvars hint_lval.vars
             (Pretty_utils.pp_list ~sep:", " Printer.pp_term) hint_terms
         | None ->
           Format.fprintf fmt "<invalid widen_hints>"
    )

let get_widen_hints_annots stmt =
  Annotations.fold_code_annot
    (fun _emitter annot acc ->
       match annot with
       | {annot_content = AStmtSpec (_, { spec_behavior = [{b_extended}]})} ->
         acc @ Extlib.filter_map
           (fun (name, _) -> name = "widen_hints")
           (fun (_, ext) ->
              match ext with
              | Ext_id _ -> assert false
              | Ext_preds _ -> assert false
              | Ext_terms terms -> terms)
           b_extended
       | {annot_content =
            AExtended (_, ("widen_hints", Ext_terms terms))} ->
         (* loop widen_hints *)
         acc @ [terms]
       | _ -> acc
    ) stmt []

let get_stmt_widen_hint_terms s =
  let terms = get_widen_hints_annots s in
  let filter_opt l =
    List.fold_left (fun acc o -> match o with
        | None -> acc
        | Some e -> e :: acc
      ) [] l
  in
  filter_opt (List.map widen_hint_terms_of_terms terms)

let is_global (hlv, _wh) =
  List.mem "global" hlv.names

let is_dynamic (hlv, _wh) =
  match hlv.vars with
  | HintMem _ -> true
  | _ -> false

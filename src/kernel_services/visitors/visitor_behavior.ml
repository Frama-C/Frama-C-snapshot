(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

open Cil_types

type t =
  { (* copy mutable structure which are not shared across the AST*)
    cfile: file -> file;
    cinitinfo: initinfo -> initinfo;
    cblock: block -> block;
    cfunspec: funspec -> funspec;
    cfunbehavior: funbehavior -> funbehavior;
    cidentified_term: identified_term -> identified_term;
    cidentified_predicate: identified_predicate -> identified_predicate;
    cexpr: exp -> exp;
    ccode_annotation: code_annotation -> code_annotation;
    (* get the copy of a shared value *)
    get_stmt: stmt -> stmt;
    get_compinfo: compinfo -> compinfo;
    get_fieldinfo: fieldinfo -> fieldinfo;
    get_model_info: model_info -> model_info;
    get_enuminfo: enuminfo -> enuminfo;
    get_enumitem: enumitem -> enumitem;
    get_typeinfo: typeinfo -> typeinfo;
    get_varinfo: varinfo -> varinfo;
    get_logic_info: logic_info -> logic_info;
    get_logic_type_info: logic_type_info -> logic_type_info;
    get_logic_var: logic_var -> logic_var;
    get_kernel_function: kernel_function -> kernel_function;
    get_fundec: fundec -> fundec;
    (* get the original value tied to a copy *)
    get_original_stmt: stmt -> stmt;
    get_original_compinfo: compinfo -> compinfo;
    get_original_fieldinfo: fieldinfo -> fieldinfo;
    get_original_model_info: model_info -> model_info;
    get_original_enuminfo: enuminfo -> enuminfo;
    get_original_enumitem: enumitem -> enumitem;
    get_original_typeinfo: typeinfo -> typeinfo;
    get_original_varinfo: varinfo -> varinfo;
    get_original_logic_info: logic_info -> logic_info;
    get_original_logic_type_info: logic_type_info -> logic_type_info;
    get_original_logic_var: logic_var -> logic_var;
    get_original_kernel_function: kernel_function -> kernel_function;
    get_original_fundec: fundec -> fundec;
    (* change a binding... use with care *)
    set_stmt: stmt -> stmt -> unit;
    set_compinfo: compinfo -> compinfo -> unit;
    set_fieldinfo: fieldinfo -> fieldinfo -> unit;
    set_model_info: model_info -> model_info -> unit;
    set_enuminfo: enuminfo -> enuminfo -> unit;
    set_enumitem: enumitem -> enumitem -> unit;
    set_typeinfo: typeinfo -> typeinfo -> unit;
    set_varinfo: varinfo -> varinfo -> unit;
    set_logic_info: logic_info -> logic_info -> unit;
    set_logic_type_info: logic_type_info -> logic_type_info -> unit;
    set_logic_var: logic_var -> logic_var -> unit;
    set_kernel_function: kernel_function -> kernel_function -> unit;
    set_fundec: fundec -> fundec -> unit;
    (* change a reference... use with care *)
    set_orig_stmt: stmt -> stmt -> unit;
    set_orig_compinfo: compinfo -> compinfo -> unit;
    set_orig_fieldinfo: fieldinfo -> fieldinfo -> unit;
    set_orig_model_info: model_info -> model_info -> unit;
    set_orig_enuminfo: enuminfo -> enuminfo -> unit;
    set_orig_enumitem: enumitem -> enumitem -> unit;
    set_orig_typeinfo: typeinfo -> typeinfo -> unit;
    set_orig_varinfo: varinfo -> varinfo -> unit;
    set_orig_logic_info: logic_info -> logic_info -> unit;
    set_orig_logic_type_info: logic_type_info -> logic_type_info -> unit;
    set_orig_logic_var: logic_var -> logic_var -> unit;
    set_orig_kernel_function: kernel_function -> kernel_function -> unit;
    set_orig_fundec: fundec -> fundec -> unit;

    unset_varinfo: varinfo -> unit;
    unset_compinfo: compinfo -> unit;
    unset_enuminfo: enuminfo -> unit;
    unset_enumitem: enumitem -> unit;
    unset_typeinfo: typeinfo -> unit;
    unset_stmt: stmt -> unit;
    unset_logic_info: logic_info -> unit;
    unset_logic_type_info: logic_type_info -> unit;
    unset_fieldinfo: fieldinfo -> unit;
    unset_model_info: model_info -> unit;
    unset_logic_var: logic_var -> unit;
    unset_kernel_function: kernel_function -> unit;
    unset_fundec: fundec -> unit;

    unset_orig_varinfo: varinfo -> unit;
    unset_orig_compinfo: compinfo -> unit;
    unset_orig_enuminfo: enuminfo -> unit;
    unset_orig_enumitem: enumitem -> unit;
    unset_orig_typeinfo: typeinfo -> unit;
    unset_orig_stmt: stmt -> unit;
    unset_orig_logic_info: logic_info -> unit;
    unset_orig_logic_type_info: logic_type_info -> unit;
    unset_orig_fieldinfo: fieldinfo -> unit;
    unset_orig_model_info: model_info -> unit;
    unset_orig_logic_var: logic_var -> unit;
    unset_orig_kernel_function: kernel_function -> unit;
    unset_orig_fundec: fundec -> unit;

    (* copy fields that can referenced in other places of the AST*)
    memo_stmt: stmt -> stmt;
    memo_varinfo: varinfo -> varinfo;
    memo_compinfo: compinfo -> compinfo;
    memo_model_info: model_info -> model_info;
    memo_enuminfo: enuminfo -> enuminfo;
    memo_enumitem: enumitem -> enumitem;
    memo_typeinfo: typeinfo -> typeinfo;
    memo_logic_info: logic_info -> logic_info;
    memo_logic_type_info: logic_type_info -> logic_type_info;
    memo_fieldinfo: fieldinfo -> fieldinfo;
    memo_logic_var: logic_var -> logic_var;
    memo_kernel_function: kernel_function -> kernel_function;
    memo_fundec: fundec -> fundec;
    (* is the behavior a copy behavior *)
    is_copy_behavior: bool;
    is_fresh_behavior: bool;
    project: Project.t option;
    (* reset memoizing tables *)
    reset_behavior_varinfo: unit -> unit;
    reset_behavior_compinfo: unit -> unit;
    reset_behavior_enuminfo: unit -> unit;
    reset_behavior_enumitem: unit -> unit;
    reset_behavior_typeinfo: unit -> unit;
    reset_behavior_logic_info: unit -> unit;
    reset_behavior_logic_type_info: unit -> unit;
    reset_behavior_fieldinfo: unit -> unit;
    reset_behavior_model_info: unit -> unit;
    reset_behavior_stmt: unit -> unit;
    reset_logic_var: unit -> unit;
    reset_behavior_kernel_function: unit -> unit;
    reset_behavior_fundec: unit -> unit;
    (* iterates over tables *)
    iter_visitor_varinfo: (varinfo -> varinfo -> unit) -> unit;
    iter_visitor_compinfo: (compinfo -> compinfo -> unit) -> unit;
    iter_visitor_enuminfo: (enuminfo -> enuminfo -> unit) -> unit;
    iter_visitor_enumitem: (enumitem -> enumitem -> unit) -> unit;
    iter_visitor_typeinfo: (typeinfo -> typeinfo -> unit) -> unit;
    iter_visitor_stmt: (stmt -> stmt -> unit) -> unit;
    iter_visitor_logic_info: (logic_info -> logic_info -> unit) -> unit;
    iter_visitor_logic_type_info:
      (logic_type_info -> logic_type_info -> unit) -> unit;
    iter_visitor_fieldinfo: (fieldinfo -> fieldinfo -> unit) -> unit;
    iter_visitor_model_info: (model_info -> model_info -> unit) -> unit;
    iter_visitor_logic_var: (logic_var -> logic_var -> unit) -> unit;
    iter_visitor_kernel_function:
      (kernel_function -> kernel_function -> unit) -> unit;
    iter_visitor_fundec: (fundec -> fundec -> unit) -> unit;
    (* folds over tables *)
    fold_visitor_varinfo: 'a.(varinfo -> varinfo -> 'a -> 'a) -> 'a -> 'a;
    fold_visitor_compinfo: 'a.(compinfo -> compinfo -> 'a -> 'a) -> 'a -> 'a;
    fold_visitor_enuminfo: 'a.(enuminfo -> enuminfo -> 'a -> 'a) -> 'a -> 'a;
    fold_visitor_enumitem: 'a.(enumitem -> enumitem -> 'a -> 'a) -> 'a -> 'a;
    fold_visitor_typeinfo: 'a.(typeinfo -> typeinfo -> 'a -> 'a) -> 'a -> 'a;
    fold_visitor_stmt: 'a.(stmt -> stmt -> 'a -> 'a) -> 'a -> 'a;
    fold_visitor_logic_info:
      'a. (logic_info -> logic_info -> 'a -> 'a) -> 'a -> 'a;
    fold_visitor_logic_type_info:
      'a.(logic_type_info -> logic_type_info -> 'a -> 'a) -> 'a -> 'a;
    fold_visitor_fieldinfo:
      'a.(fieldinfo -> fieldinfo -> 'a -> 'a) -> 'a -> 'a;
    fold_visitor_model_info:
      'a. (model_info -> model_info -> 'a -> 'a) -> 'a -> 'a;
    fold_visitor_logic_var:
      'a.(logic_var -> logic_var -> 'a -> 'a) -> 'a -> 'a;
    fold_visitor_kernel_function:
      'a.(kernel_function -> kernel_function -> 'a -> 'a) -> 'a -> 'a;
    fold_visitor_fundec:
      'a.(fundec -> fundec -> 'a -> 'a) -> 'a -> 'a;
  }

let id = Extlib.id
let alphabetaunit _ _ = ()
let alphabetabeta _ x = x
let unitunit: unit -> unit = id
let alphaunit _ = ()

let inplace () =
  { cfile = id;
    get_compinfo = id;
    get_fieldinfo = id;
    get_model_info = id;
    get_enuminfo = id;
    get_enumitem = id;
    get_typeinfo = id;
    get_varinfo = id;
    get_logic_var = id;
    get_stmt = id;
    get_logic_info = id;
    get_logic_type_info = id;
    get_kernel_function = id;
    get_fundec = id;
    get_original_compinfo = id;
    get_original_fieldinfo = id;
    get_original_model_info = id;
    get_original_enuminfo = id;
    get_original_enumitem = id;
    get_original_typeinfo = id;
    get_original_varinfo = id;
    get_original_logic_var = id;
    get_original_stmt = id;
    get_original_logic_info = id;
    get_original_logic_type_info = id;
    get_original_kernel_function = id;
    get_original_fundec = id;
    cinitinfo = id;
    cblock = id;
    cfunspec = id;
    cfunbehavior = id;
    cidentified_term = id;
    cidentified_predicate = id;
    ccode_annotation = id;
    cexpr = id;
    is_copy_behavior = false;
    is_fresh_behavior = false;
    project = None;
    memo_varinfo = id;
    memo_compinfo = id;
    memo_enuminfo = id;
    memo_enumitem = id;
    memo_typeinfo = id;
    memo_logic_info = id;
    memo_logic_type_info = id;
    memo_stmt = id;
    memo_fieldinfo = id;
    memo_model_info = id;
    memo_logic_var = id;
    memo_kernel_function = id;
    memo_fundec = id;
    set_varinfo = alphabetaunit;
    set_compinfo = alphabetaunit;
    set_enuminfo = alphabetaunit;
    set_enumitem = alphabetaunit;
    set_typeinfo = alphabetaunit;
    set_logic_info = alphabetaunit;
    set_logic_type_info = alphabetaunit;
    set_stmt = alphabetaunit;
    set_fieldinfo = alphabetaunit;
    set_model_info = alphabetaunit;
    set_logic_var = alphabetaunit;
    set_kernel_function = alphabetaunit;
    set_fundec = alphabetaunit;
    set_orig_varinfo = alphabetaunit;
    set_orig_compinfo = alphabetaunit;
    set_orig_enuminfo = alphabetaunit;
    set_orig_enumitem = alphabetaunit;
    set_orig_typeinfo = alphabetaunit;
    set_orig_logic_info = alphabetaunit;
    set_orig_logic_type_info = alphabetaunit;
    set_orig_stmt = alphabetaunit;
    set_orig_fieldinfo = alphabetaunit;
    set_orig_model_info = alphabetaunit;
    set_orig_logic_var = alphabetaunit;
    set_orig_kernel_function = alphabetaunit;
    set_orig_fundec = alphabetaunit;
    unset_varinfo = alphaunit;
    unset_compinfo = alphaunit;
    unset_enuminfo = alphaunit;
    unset_enumitem = alphaunit;
    unset_typeinfo = alphaunit;
    unset_logic_info = alphaunit;
    unset_logic_type_info = alphaunit;
    unset_stmt = alphaunit;
    unset_fieldinfo = alphaunit;
    unset_model_info = alphaunit;
    unset_logic_var = alphaunit;
    unset_kernel_function = alphaunit;
    unset_fundec = alphaunit;
    unset_orig_varinfo = alphaunit;
    unset_orig_compinfo = alphaunit;
    unset_orig_enuminfo = alphaunit;
    unset_orig_enumitem = alphaunit;
    unset_orig_typeinfo = alphaunit;
    unset_orig_logic_info = alphaunit;
    unset_orig_logic_type_info = alphaunit;
    unset_orig_stmt = alphaunit;
    unset_orig_fieldinfo = alphaunit;
    unset_orig_model_info = alphaunit;
    unset_orig_logic_var = alphaunit;
    unset_orig_kernel_function = alphaunit;
    unset_orig_fundec = alphaunit;
    reset_behavior_varinfo = unitunit;
    reset_behavior_compinfo = unitunit;
    reset_behavior_enuminfo = unitunit;
    reset_behavior_enumitem = unitunit;
    reset_behavior_typeinfo = unitunit;
    reset_behavior_logic_info = unitunit;
    reset_behavior_logic_type_info = unitunit;
    reset_behavior_fieldinfo = unitunit;
    reset_behavior_model_info = unitunit;
    reset_behavior_stmt = unitunit;
    reset_logic_var = unitunit;
    reset_behavior_kernel_function = unitunit;
    reset_behavior_fundec = unitunit;
    iter_visitor_varinfo = alphaunit;
    iter_visitor_compinfo = alphaunit;
    iter_visitor_enuminfo = alphaunit;
    iter_visitor_enumitem = alphaunit;
    iter_visitor_typeinfo = alphaunit;
    iter_visitor_stmt = alphaunit;
    iter_visitor_logic_info = alphaunit;
    iter_visitor_logic_type_info = alphaunit;
    iter_visitor_fieldinfo = alphaunit;
    iter_visitor_model_info = alphaunit;
    iter_visitor_logic_var = alphaunit;
    iter_visitor_kernel_function = alphaunit;
    iter_visitor_fundec = alphaunit;
    fold_visitor_varinfo = alphabetabeta;
    fold_visitor_compinfo = alphabetabeta;
    fold_visitor_enuminfo = alphabetabeta;
    fold_visitor_enumitem = alphabetabeta;
    fold_visitor_typeinfo = alphabetabeta;
    fold_visitor_stmt = alphabetabeta;
    fold_visitor_logic_info = alphabetabeta;
    fold_visitor_logic_type_info = alphabetabeta;
    fold_visitor_fieldinfo = alphabetabeta;
    fold_visitor_model_info = alphabetabeta;
    fold_visitor_logic_var = alphabetabeta;
    fold_visitor_kernel_function = alphabetabeta;
    fold_visitor_fundec = alphabetabeta;
  }

let copy_visit_gen fresh prj =
  let varinfos = Cil_datatype.Varinfo.Hashtbl.create 103 in
  let compinfos = Cil_datatype.Compinfo.Hashtbl.create 17 in
  let enuminfos = Cil_datatype.Enuminfo.Hashtbl.create 17 in
  let enumitems = Cil_datatype.Enumitem.Hashtbl.create 17 in
  let typeinfos = Cil_datatype.Typeinfo.Hashtbl.create 17 in
  let logic_infos = Cil_datatype.Logic_info.Hashtbl.create 17 in
  let logic_type_infos = Cil_datatype.Logic_type_info.Hashtbl.create 17 in
  let fieldinfos = Cil_datatype.Fieldinfo.Hashtbl.create 17 in
  let model_infos = Cil_datatype.Model_info.Hashtbl.create 17 in
  let stmts = Cil_datatype.Stmt.Hashtbl.create 103 in
  let logic_vars = Cil_datatype.Logic_var.Hashtbl.create 17 in
  let kernel_functions = Cil_datatype.Kf.Hashtbl.create 17 in
  let fundecs = Cil_datatype.Varinfo.Hashtbl.create 17 in
  let orig_varinfos = Cil_datatype.Varinfo.Hashtbl.create 103 in
  let orig_compinfos = Cil_datatype.Compinfo.Hashtbl.create 17 in
  let orig_enuminfos = Cil_datatype.Enuminfo.Hashtbl.create 17 in
  let orig_enumitems = Cil_datatype.Enumitem.Hashtbl.create 17 in
  let orig_typeinfos = Cil_datatype.Typeinfo.Hashtbl.create 17 in
  let orig_logic_infos = Cil_datatype.Logic_info.Hashtbl.create 17 in
  let orig_logic_type_infos = Cil_datatype.Logic_type_info.Hashtbl.create 17 in
  let orig_fieldinfos = Cil_datatype.Fieldinfo.Hashtbl.create 17 in
  let orig_model_infos = Cil_datatype.Model_info.Hashtbl.create 17 in
  let orig_stmts = Cil_datatype.Stmt.Hashtbl.create 103 in
  let orig_logic_vars = Cil_datatype.Logic_var.Hashtbl.create 17 in
  let orig_kernel_functions = Cil_datatype.Kf.Hashtbl.create 17 in
  let orig_fundecs = Cil_datatype.Varinfo.Hashtbl.create 17 in
  let temp_set_logic_var x new_x =
    Cil_datatype.Logic_var.Hashtbl.add logic_vars x new_x
  in
  let temp_set_orig_logic_var new_x x =
    Cil_datatype.Logic_var.Hashtbl.add orig_logic_vars new_x x
  in
  let temp_unset_logic_var x =
    Cil_datatype.Logic_var.Hashtbl.remove logic_vars x
  in
  let temp_unset_orig_logic_var new_x =
    Cil_datatype.Logic_var.Hashtbl.remove orig_logic_vars new_x
  in
  let temp_memo_logic_var x =
    (*    Format.printf "search for %s#%d@." x.lv_name x.lv_id;*)
    let res =
      try Cil_datatype.Logic_var.Hashtbl.find logic_vars x
      with Not_found ->
        (*      Format.printf "Not found@.";*)
        let id = if fresh then Cil_const.new_raw_id () else x.lv_id in
        let new_x = { x with lv_id = id } in
        temp_set_logic_var x new_x; temp_set_orig_logic_var new_x x; new_x
    in
    (*    Format.printf "res is %s#%d@." res.lv_name res.lv_id;*)
    res
  in
  let temp_set_varinfo x new_x =
    Cil_datatype.Varinfo.Hashtbl.add varinfos x new_x;
    match x.vlogic_var_assoc, new_x.vlogic_var_assoc with
    | None, _ | _, None -> ()
    | Some lx, Some new_lx ->
      Cil_datatype.Logic_var.Hashtbl.add logic_vars lx new_lx
  in
  let temp_set_orig_varinfo new_x x =
    Cil_datatype.Varinfo.Hashtbl.add orig_varinfos new_x x;
    match new_x.vlogic_var_assoc, x.vlogic_var_assoc with
    | None, _ | _, None -> ()
    | Some new_lx, Some lx ->
      Cil_datatype.Logic_var.Hashtbl.add orig_logic_vars new_lx lx
  in
  let temp_unset_varinfo x =
    Cil_datatype.Varinfo.Hashtbl.remove varinfos x;
    match x.vlogic_var_assoc with
    | None -> ()
    | Some lx -> Cil_datatype.Logic_var.Hashtbl.remove logic_vars lx
  in
  let temp_unset_orig_varinfo new_x =
    Cil_datatype.Varinfo.Hashtbl.remove orig_varinfos new_x;
    match new_x.vlogic_var_assoc with
    | None -> ()
    | Some new_lx ->
      Cil_datatype.Logic_var.Hashtbl.remove orig_logic_vars new_lx
  in
  let temp_memo_varinfo x =
    try Cil_datatype.Varinfo.Hashtbl.find varinfos x
    with Not_found ->
      let new_x =
        if fresh then Cil_const.copy_with_new_vid x else begin
          let new_x = { x with vid = x.vid } in
          (match x.vlogic_var_assoc with
           | None -> ()
           | Some lv ->
             let new_lv = { lv with lv_origin = Some new_x } in
             new_x.vlogic_var_assoc <- Some new_lv);
          new_x
        end
      in
      temp_set_varinfo x new_x; temp_set_orig_varinfo new_x x; new_x
  in
  let temp_set_fundec f new_f =
    Cil_datatype.Varinfo.Hashtbl.add fundecs f.svar new_f
  in
  let temp_set_orig_fundec new_f f =
    Cil_datatype.Varinfo.Hashtbl.add orig_fundecs new_f.svar f
  in
  let temp_unset_fundec f =
    Cil_datatype.Varinfo.Hashtbl.remove fundecs f.svar
  in
  let temp_unset_orig_fundec new_f =
    Cil_datatype.Varinfo.Hashtbl.remove orig_fundecs new_f.svar
  in
  let temp_memo_fundec f =
    try Cil_datatype.Varinfo.Hashtbl.find fundecs f.svar
    with Not_found ->
      let v = temp_memo_varinfo f.svar in
      let new_f = { f with svar = v } in
      temp_set_fundec f new_f; temp_set_orig_fundec new_f f; new_f
  in
  let temp_set_kernel_function kf new_kf =
    Cil_datatype.Kf.Hashtbl.replace kernel_functions kf new_kf;
    match kf.fundec, new_kf.fundec with
    | Declaration(_,vi,_,_), Declaration(_,new_vi,_,_)
    | Declaration(_,vi,_,_), Definition({ svar = new_vi }, _)
    | Definition({svar = vi},_), Declaration(_,new_vi,_,_) ->
      temp_set_varinfo vi new_vi
    | Definition (fundec,_), Definition(new_fundec,_) ->
      temp_set_fundec fundec new_fundec
  in
  let temp_set_orig_kernel_function new_kf kf =
    Cil_datatype.Kf.Hashtbl.replace orig_kernel_functions new_kf kf;
    match new_kf.fundec, kf.fundec with
    | Declaration(_,new_vi,_,_), Declaration(_,vi,_,_)
    | Declaration(_,new_vi,_,_), Definition({ svar = vi }, _)
    | Definition({svar = new_vi},_), Declaration(_,vi,_,_) ->
      temp_set_orig_varinfo new_vi vi
    | Definition (new_fundec,_), Definition(fundec,_) ->
      temp_set_orig_fundec new_fundec fundec
  in
  let temp_unset_kernel_function kf =
    Cil_datatype.Kf.Hashtbl.remove kernel_functions kf;
    match kf.fundec with
    | Declaration(_,vi,_,_) -> temp_unset_varinfo vi
    | Definition (fundec,_) -> temp_unset_fundec fundec
  in
  let temp_unset_orig_kernel_function new_kf =
    Cil_datatype.Kf.Hashtbl.remove orig_kernel_functions new_kf;
    match new_kf.fundec with
    | Declaration(_,new_vi,_,_) -> temp_unset_orig_varinfo new_vi
    | Definition (new_fundec,_) -> temp_unset_orig_fundec new_fundec
  in
  let temp_memo_kernel_function kf =
    try Cil_datatype.Kf.Hashtbl.find kernel_functions kf
    with Not_found ->
      let new_kf =
        match kf.fundec with
        | Declaration (spec,vi,prms,loc) ->
          let new_vi = temp_memo_varinfo vi in
          { kf with fundec = Declaration(spec,new_vi,prms,loc) }
        | Definition(f,loc) ->
          let new_f = temp_memo_fundec f in
          { kf with fundec = Definition(new_f,loc) }
      in
      temp_set_kernel_function kf new_kf;
      temp_set_orig_kernel_function new_kf kf;
      new_kf
  in
  let temp_set_compinfo c new_c =
    Cil_datatype.Compinfo.Hashtbl.add compinfos c new_c;
    List.iter2
      (fun f new_f -> Cil_datatype.Fieldinfo.Hashtbl.add fieldinfos f new_f)
      c.cfields new_c.cfields
  in
  let temp_set_orig_compinfo new_c c =
    Cil_datatype.Compinfo.Hashtbl.add orig_compinfos new_c c;
    List.iter2
      (fun new_f f ->
         Cil_datatype.Fieldinfo.Hashtbl.add orig_fieldinfos new_f f)
      new_c.cfields c.cfields
  in
  let temp_unset_compinfo c =
    Cil_datatype.Compinfo.Hashtbl.remove compinfos c;
    List.iter
      (fun f -> Cil_datatype.Fieldinfo.Hashtbl.remove fieldinfos f) c.cfields
  in
  let temp_unset_orig_compinfo new_c =
    Cil_datatype.Compinfo.Hashtbl.remove orig_compinfos new_c;
    List.iter
      (fun new_f ->
         Cil_datatype.Fieldinfo.Hashtbl.remove orig_fieldinfos new_f)
      new_c.cfields
  in
  let temp_memo_compinfo c =
    try Cil_datatype.Compinfo.Hashtbl.find compinfos c
    with Not_found ->
      let new_c =
        Cil_const.copyCompInfo ~fresh c c.cname
      in
      temp_set_compinfo c new_c; temp_set_orig_compinfo new_c c; new_c
  in
  { cfile = (fun x -> { x with fileName = x.fileName });
    get_compinfo =
      (fun x ->
         try Cil_datatype.Compinfo.Hashtbl.find compinfos x with Not_found -> x);
    get_fieldinfo =
      (fun x ->
         try Cil_datatype.Fieldinfo.Hashtbl.find fieldinfos x
         with Not_found -> x);
    get_model_info =
      (fun x ->
         try Cil_datatype.Model_info.Hashtbl.find model_infos x
         with Not_found -> x);
    get_enuminfo =
      (fun x ->
         try Cil_datatype.Enuminfo.Hashtbl.find enuminfos x with Not_found -> x);
    get_enumitem =
      (fun x ->
         try Cil_datatype.Enumitem.Hashtbl.find enumitems x with Not_found -> x);
    get_typeinfo =
      (fun x ->
         try Cil_datatype.Typeinfo.Hashtbl.find typeinfos x with Not_found -> x);
    get_varinfo =
      (fun x ->
         try Cil_datatype.Varinfo.Hashtbl.find varinfos x with Not_found -> x);
    get_stmt =
      (fun x -> try Cil_datatype.Stmt.Hashtbl.find stmts x with Not_found -> x);
    get_logic_info =
      (fun x ->
         try Cil_datatype.Logic_info.Hashtbl.find logic_infos x
         with Not_found -> x);
    get_logic_type_info =
      (fun x ->
         try Cil_datatype.Logic_type_info.Hashtbl.find logic_type_infos x
         with Not_found -> x);
    get_logic_var =
      (fun x ->
         try Cil_datatype.Logic_var.Hashtbl.find logic_vars x
         with Not_found -> x);
    get_kernel_function =
      (fun x ->
         try Cil_datatype.Kf.Hashtbl.find kernel_functions x
         with Not_found -> x);
    get_fundec =
      (fun x ->
         try Cil_datatype.Varinfo.Hashtbl.find fundecs x.svar
         with Not_found -> x);
    get_original_compinfo =
      (fun x ->
         try Cil_datatype.Compinfo.Hashtbl.find orig_compinfos x
         with Not_found -> x);
    get_original_fieldinfo =
      (fun x ->
         try Cil_datatype.Fieldinfo.Hashtbl.find orig_fieldinfos x
         with Not_found -> x);
    get_original_model_info =
      (fun x ->
         try Cil_datatype.Model_info.Hashtbl.find orig_model_infos x
         with Not_found -> x);
    get_original_enuminfo =
      (fun x ->
         try Cil_datatype.Enuminfo.Hashtbl.find orig_enuminfos x
         with Not_found -> x);
    get_original_enumitem =
      (fun x ->
         try Cil_datatype.Enumitem.Hashtbl.find orig_enumitems x
         with Not_found -> x);
    get_original_typeinfo =
      (fun x ->
         try Cil_datatype.Typeinfo.Hashtbl.find orig_typeinfos x
         with Not_found -> x);
    get_original_varinfo =
      (fun x ->
         try Cil_datatype.Varinfo.Hashtbl.find orig_varinfos x
         with Not_found -> x);
    get_original_stmt =
      (fun x ->
         try Cil_datatype.Stmt.Hashtbl.find orig_stmts x with Not_found -> x);
    get_original_logic_var =
      (fun x ->
         try Cil_datatype.Logic_var.Hashtbl.find orig_logic_vars x
         with Not_found -> x);
    get_original_logic_info =
      (fun x ->
         try Cil_datatype.Logic_info.Hashtbl.find orig_logic_infos x
         with Not_found -> x);
    get_original_logic_type_info =
      (fun x ->
         try Cil_datatype.Logic_type_info.Hashtbl.find orig_logic_type_infos x
         with Not_found -> x);
    get_original_kernel_function =
      (fun x ->
         try Cil_datatype.Kf.Hashtbl.find orig_kernel_functions x
         with Not_found -> x);
    get_original_fundec =
      (fun x ->
         try Cil_datatype.Varinfo.Hashtbl.find orig_fundecs x.svar
         with Not_found -> x);
    cinitinfo = (fun x -> { init = x.init });
    cblock = (fun x -> { x with battrs = x.battrs });
    cfunspec = (fun x -> { x with spec_behavior = x.spec_behavior});
    cfunbehavior = (fun x -> { x with b_name = x.b_name});
    ccode_annotation =
      if fresh then Logic_const.refresh_code_annotation
      else (fun x -> { x with annot_id = x.annot_id });
    cidentified_predicate =
      if fresh then Logic_const.refresh_predicate
      else (fun x -> { x with ip_id = x.ip_id });
    cidentified_term =
      if fresh then Logic_const.refresh_identified_term
      else (fun x -> { x with it_id = x.it_id});
    cexpr =
      (fun x ->
         let id = if fresh then Cil_const.Eid.next () else x.eid in { x with eid = id });
    is_copy_behavior = true;
    is_fresh_behavior = fresh;
    project = Some prj;
    reset_behavior_varinfo =
      (fun () ->
         Cil_datatype.Varinfo.Hashtbl.clear varinfos;
         Cil_datatype.Varinfo.Hashtbl.clear orig_varinfos);
    reset_behavior_compinfo =
      (fun () ->
         Cil_datatype.Compinfo.Hashtbl.clear compinfos;
         Cil_datatype.Compinfo.Hashtbl.clear orig_compinfos);
    reset_behavior_enuminfo =
      (fun () ->
         Cil_datatype.Enuminfo.Hashtbl.clear enuminfos;
         Cil_datatype.Enuminfo.Hashtbl.clear orig_enuminfos);
    reset_behavior_enumitem =
      (fun () ->
         Cil_datatype.Enumitem.Hashtbl.clear enumitems;
         Cil_datatype.Enumitem.Hashtbl.clear orig_enumitems);
    reset_behavior_typeinfo =
      (fun () ->
         Cil_datatype.Typeinfo.Hashtbl.clear typeinfos;
         Cil_datatype.Typeinfo.Hashtbl.clear orig_typeinfos);
    reset_behavior_logic_info =
      (fun () ->
         Cil_datatype.Logic_info.Hashtbl.clear logic_infos;
         Cil_datatype.Logic_info.Hashtbl.clear orig_logic_infos);
    reset_behavior_logic_type_info =
      (fun () ->
         Cil_datatype.Logic_type_info.Hashtbl.clear logic_type_infos;
         Cil_datatype.Logic_type_info.Hashtbl.clear orig_logic_type_infos);
    reset_behavior_fieldinfo =
      (fun () ->
         Cil_datatype.Fieldinfo.Hashtbl.clear fieldinfos;
         Cil_datatype.Fieldinfo.Hashtbl.clear orig_fieldinfos);
    reset_behavior_model_info =
      (fun () ->
         Cil_datatype.Model_info.Hashtbl.clear model_infos;
         Cil_datatype.Model_info.Hashtbl.clear orig_model_infos);
    reset_behavior_stmt =
      (fun () ->
         Cil_datatype.Stmt.Hashtbl.clear stmts;
         Cil_datatype.Stmt.Hashtbl.clear orig_stmts);
    reset_logic_var =
      (fun () ->
         Cil_datatype.Logic_var.Hashtbl.clear logic_vars;
         Cil_datatype.Logic_var.Hashtbl.clear orig_logic_vars);
    reset_behavior_kernel_function =
      (fun () ->
         Cil_datatype.Kf.Hashtbl.clear kernel_functions;
         Cil_datatype.Kf.Hashtbl.clear orig_kernel_functions);
    reset_behavior_fundec =
      (fun () ->
         Cil_datatype.Varinfo.Hashtbl.clear fundecs;
         Cil_datatype.Varinfo.Hashtbl.clear orig_fundecs);
    memo_varinfo = temp_memo_varinfo;
    memo_compinfo = temp_memo_compinfo;
    memo_enuminfo =
      (fun x ->
         try Cil_datatype.Enuminfo.Hashtbl.find enuminfos x
         with Not_found ->
           let new_x = { x with ename = x.ename } in
           Cil_datatype.Enuminfo.Hashtbl.add enuminfos x new_x;
           Cil_datatype.Enuminfo.Hashtbl.add orig_enuminfos new_x x;
           new_x);
    memo_enumitem =
      (fun x ->
         try Cil_datatype.Enumitem.Hashtbl.find enumitems x
         with Not_found ->
           let new_x = { x with einame = x.einame } in
           Cil_datatype.Enumitem.Hashtbl.add enumitems x new_x;
           Cil_datatype.Enumitem.Hashtbl.add orig_enumitems new_x x;
           new_x);
    memo_typeinfo =
      (fun x ->
         try Cil_datatype.Typeinfo.Hashtbl.find typeinfos x
         with Not_found ->
           let new_x = { x with tname = x.tname } in
           Cil_datatype.Typeinfo.Hashtbl.add typeinfos x new_x;
           Cil_datatype.Typeinfo.Hashtbl.add orig_typeinfos new_x x;
           new_x);
    memo_logic_info =
      (fun x ->
         try Cil_datatype.Logic_info.Hashtbl.find logic_infos x
         with Not_found ->
           let new_v = temp_memo_logic_var x.l_var_info in
           let new_x = { x with l_var_info = new_v } in
           Cil_datatype.Logic_info.Hashtbl.add logic_infos x new_x;
           Cil_datatype.Logic_info.Hashtbl.add orig_logic_infos new_x x;
           new_x);
    memo_logic_type_info =
      (fun x ->
         try Cil_datatype.Logic_type_info.Hashtbl.find logic_type_infos x
         with Not_found ->
           let new_x = { x with lt_name = x.lt_name } in
           Cil_datatype.Logic_type_info.Hashtbl.add logic_type_infos x new_x;
           Cil_datatype.Logic_type_info.Hashtbl.add
             orig_logic_type_infos new_x x;
           new_x);
    memo_stmt =
      (fun x ->
         try Cil_datatype.Stmt.Hashtbl.find stmts x
         with Not_found ->
           let sid = if fresh then Cil_const.Sid.next () else x.sid in
           let new_x = { x with sid = sid } in
           Cil_datatype.Stmt.Hashtbl.add stmts x new_x;
           Cil_datatype.Stmt.Hashtbl.add orig_stmts new_x x;
           new_x);
    memo_fieldinfo =
      (fun x ->
         try Cil_datatype.Fieldinfo.Hashtbl.find fieldinfos x
         with Not_found ->
           let _ = temp_memo_compinfo x.fcomp in
           (* memo_compinfo fills the field correspondence table as well *)
           let new_x = Cil_datatype.Fieldinfo.Hashtbl.find fieldinfos x in
           Cil_datatype.Fieldinfo.Hashtbl.add fieldinfos x new_x;
           Cil_datatype.Fieldinfo.Hashtbl.add orig_fieldinfos new_x x;
           new_x);
    memo_model_info =
      (fun x ->
         try Cil_datatype.Model_info.Hashtbl.find model_infos x
         with Not_found ->
           let new_x = { x with mi_name = x.mi_name } in
           Cil_datatype.Model_info.Hashtbl.add model_infos x new_x;
           Cil_datatype.Model_info.Hashtbl.add orig_model_infos new_x x;
           new_x
      );
    memo_logic_var = temp_memo_logic_var;
    memo_kernel_function = temp_memo_kernel_function;
    memo_fundec = temp_memo_fundec;
    set_varinfo = temp_set_varinfo;
    set_compinfo = temp_set_compinfo;
    set_enuminfo = Cil_datatype.Enuminfo.Hashtbl.replace enuminfos;
    set_enumitem = Cil_datatype.Enumitem.Hashtbl.replace enumitems;
    set_typeinfo = Cil_datatype.Typeinfo.Hashtbl.replace typeinfos;
    set_logic_info = Cil_datatype.Logic_info.Hashtbl.replace logic_infos;
    set_logic_type_info =
      Cil_datatype.Logic_type_info.Hashtbl.replace logic_type_infos;
    set_stmt = Cil_datatype.Stmt.Hashtbl.replace stmts;
    set_fieldinfo = Cil_datatype.Fieldinfo.Hashtbl.replace fieldinfos;
    set_model_info = Cil_datatype.Model_info.Hashtbl.replace model_infos;
    set_logic_var = temp_set_logic_var;
    set_kernel_function = temp_set_kernel_function;
    set_fundec = temp_set_fundec;
    set_orig_varinfo = temp_set_orig_varinfo;
    set_orig_compinfo = temp_set_orig_compinfo;
    set_orig_enuminfo = Cil_datatype.Enuminfo.Hashtbl.replace orig_enuminfos;
    set_orig_enumitem = Cil_datatype.Enumitem.Hashtbl.replace orig_enumitems;
    set_orig_typeinfo = Cil_datatype.Typeinfo.Hashtbl.replace orig_typeinfos;
    set_orig_logic_info =
      Cil_datatype.Logic_info.Hashtbl.replace orig_logic_infos;
    set_orig_logic_type_info =
      Cil_datatype.Logic_type_info.Hashtbl.replace orig_logic_type_infos;
    set_orig_stmt = Cil_datatype.Stmt.Hashtbl.replace orig_stmts;
    set_orig_fieldinfo =
      Cil_datatype.Fieldinfo.Hashtbl.replace orig_fieldinfos;
    set_orig_model_info =
      Cil_datatype.Model_info.Hashtbl.replace orig_model_infos;
    set_orig_logic_var = temp_set_orig_logic_var;
    set_orig_kernel_function = temp_set_orig_kernel_function;
    set_orig_fundec = temp_set_orig_fundec;

    unset_varinfo = temp_unset_varinfo;
    unset_compinfo = temp_unset_compinfo;
    unset_enuminfo = Cil_datatype.Enuminfo.Hashtbl.remove enuminfos;
    unset_enumitem = Cil_datatype.Enumitem.Hashtbl.remove enumitems;
    unset_typeinfo = Cil_datatype.Typeinfo.Hashtbl.remove typeinfos;
    unset_logic_info = Cil_datatype.Logic_info.Hashtbl.remove logic_infos;
    unset_logic_type_info =
      Cil_datatype.Logic_type_info.Hashtbl.remove logic_type_infos;
    unset_stmt = Cil_datatype.Stmt.Hashtbl.remove stmts;
    unset_fieldinfo = Cil_datatype.Fieldinfo.Hashtbl.remove fieldinfos;
    unset_model_info = Cil_datatype.Model_info.Hashtbl.remove model_infos;
    unset_logic_var = temp_unset_logic_var;
    unset_kernel_function = temp_unset_kernel_function;
    unset_fundec = temp_unset_fundec;

    unset_orig_varinfo = temp_unset_orig_varinfo;
    unset_orig_compinfo = temp_unset_orig_compinfo;
    unset_orig_enuminfo = Cil_datatype.Enuminfo.Hashtbl.remove orig_enuminfos;
    unset_orig_enumitem = Cil_datatype.Enumitem.Hashtbl.remove orig_enumitems;
    unset_orig_typeinfo = Cil_datatype.Typeinfo.Hashtbl.remove orig_typeinfos;
    unset_orig_logic_info =
      Cil_datatype.Logic_info.Hashtbl.remove orig_logic_infos;
    unset_orig_logic_type_info =
      Cil_datatype.Logic_type_info.Hashtbl.remove orig_logic_type_infos;
    unset_orig_stmt = Cil_datatype.Stmt.Hashtbl.remove orig_stmts;
    unset_orig_fieldinfo =
      Cil_datatype.Fieldinfo.Hashtbl.remove orig_fieldinfos;
    unset_orig_model_info =
      Cil_datatype.Model_info.Hashtbl.remove orig_model_infos;
    unset_orig_logic_var = temp_unset_orig_logic_var;
    unset_orig_kernel_function = temp_unset_orig_kernel_function;
    unset_orig_fundec = temp_unset_orig_fundec;

    iter_visitor_varinfo =
      (fun f -> Cil_datatype.Varinfo.Hashtbl.iter f varinfos);
    iter_visitor_compinfo =
      (fun f -> Cil_datatype.Compinfo.Hashtbl.iter f compinfos);
    iter_visitor_enuminfo =
      (fun f -> Cil_datatype.Enuminfo.Hashtbl.iter f enuminfos);
    iter_visitor_enumitem =
      (fun f -> Cil_datatype.Enumitem.Hashtbl.iter f enumitems);
    iter_visitor_typeinfo =
      (fun f -> Cil_datatype.Typeinfo.Hashtbl.iter f typeinfos);
    iter_visitor_stmt =
      (fun f -> Cil_datatype.Stmt.Hashtbl.iter f stmts);
    iter_visitor_logic_info =
      (fun f -> Cil_datatype.Logic_info.Hashtbl.iter f logic_infos);
    iter_visitor_logic_type_info =
      (fun f -> Cil_datatype.Logic_type_info.Hashtbl.iter f logic_type_infos);
    iter_visitor_fieldinfo =
      (fun f -> Cil_datatype.Fieldinfo.Hashtbl.iter f fieldinfos);
    iter_visitor_model_info =
      (fun f -> Cil_datatype.Model_info.Hashtbl.iter f model_infos);
    iter_visitor_logic_var =
      (fun f -> Cil_datatype.Logic_var.Hashtbl.iter f logic_vars);
    iter_visitor_kernel_function =
      (fun f -> Cil_datatype.Kf.Hashtbl.iter f kernel_functions);
    iter_visitor_fundec =
      (fun f ->
         let f _ new_fundec =
           let old_fundec =
             Cil_datatype.Varinfo.Hashtbl.find orig_fundecs new_fundec.svar
           in
           f old_fundec new_fundec
         in
         Cil_datatype.Varinfo.Hashtbl.iter f fundecs);
    fold_visitor_varinfo =
      (fun f i -> Cil_datatype.Varinfo.Hashtbl.fold f varinfos i);
    fold_visitor_compinfo =
      (fun f i -> Cil_datatype.Compinfo.Hashtbl.fold f compinfos i);
    fold_visitor_enuminfo =
      (fun f i -> Cil_datatype.Enuminfo.Hashtbl.fold f enuminfos i);
    fold_visitor_enumitem =
      (fun f i -> Cil_datatype.Enumitem.Hashtbl.fold f enumitems i);
    fold_visitor_typeinfo =
      (fun f i -> Cil_datatype.Typeinfo.Hashtbl.fold f typeinfos i);
    fold_visitor_stmt =
      (fun f i -> Cil_datatype.Stmt.Hashtbl.fold f stmts i);
    fold_visitor_logic_info =
      (fun f i -> Cil_datatype.Logic_info.Hashtbl.fold f logic_infos i);
    fold_visitor_logic_type_info =
      (fun f i ->
         Cil_datatype.Logic_type_info.Hashtbl.fold f logic_type_infos i);
    fold_visitor_fieldinfo =
      (fun f i -> Cil_datatype.Fieldinfo.Hashtbl.fold f fieldinfos i);
    fold_visitor_model_info =
      (fun f i -> Cil_datatype.Model_info.Hashtbl.fold f model_infos i);
    fold_visitor_logic_var =
      (fun f i -> Cil_datatype.Logic_var.Hashtbl.fold f logic_vars i);
    fold_visitor_kernel_function =
      (fun f i ->
         Cil_datatype.Kf.Hashtbl.fold f kernel_functions i);
    fold_visitor_fundec =
      (fun f i ->
         let f _ new_fundec acc =
           let old_fundec =
             Cil_datatype.Varinfo.Hashtbl.find orig_fundecs new_fundec.svar
           in
           f old_fundec new_fundec acc
         in
         Cil_datatype.Varinfo.Hashtbl.fold f fundecs i);
  }

let copy = copy_visit_gen false
let refresh = copy_visit_gen true

let is_copy b = b.is_copy_behavior
let is_fresh b = b.is_fresh_behavior

let get_project b = b.project

let ccode_annotation b = b.ccode_annotation
let cexpr b = b.cexpr
let cidentified_predicate b = b.cidentified_predicate
let cidentified_term b = b.cidentified_term
let cfunbehavior b = b.cfunbehavior
let cfunspec b = b.cfunspec
let cblock b = b.cblock
let cinitinfo b = b.cinitinfo
let cfile b = b.cfile

module Memo = struct
  let varinfo b = b.memo_varinfo
  let compinfo b = b.memo_compinfo
  let fieldinfo b = b.memo_fieldinfo
  let model_info b = b.memo_model_info
  let enuminfo b = b.memo_enuminfo
  let enumitem b = b.memo_enumitem
  let stmt b = b.memo_stmt
  let typeinfo b = b.memo_typeinfo
  let logic_info b = b.memo_logic_info
  let logic_type_info b = b.memo_logic_type_info
  let logic_var b = b.memo_logic_var
  let kernel_function b = b.memo_kernel_function
  let fundec b = b.memo_fundec
end

module Reset = struct
  let varinfo b = b.reset_behavior_varinfo ()
  let compinfo b = b.reset_behavior_compinfo ()
  let enuminfo b = b.reset_behavior_enuminfo ()
  let enumitem b = b.reset_behavior_enumitem ()
  let typeinfo b = b.reset_behavior_typeinfo ()
  let logic_info b = b.reset_behavior_logic_info ()
  let logic_type_info b = b.reset_behavior_logic_type_info ()
  let fieldinfo b = b.reset_behavior_fieldinfo ()
  let model_info b = b.reset_behavior_model_info ()
  let stmt b = b.reset_behavior_stmt ()
  let logic_var b = b.reset_logic_var ()
  let kernel_function b = b.reset_behavior_kernel_function ()
  let fundec b = b.reset_behavior_fundec ()
end

module type Get = sig
  val varinfo: t -> varinfo -> varinfo
  val compinfo: t -> compinfo -> compinfo
  val enuminfo: t -> enuminfo -> enuminfo
  val enumitem: t -> enumitem -> enumitem
  val typeinfo: t -> typeinfo -> typeinfo
  val stmt: t -> stmt -> stmt
  val logic_info: t -> logic_info -> logic_info
  val logic_type_info: t -> logic_type_info -> logic_type_info
  val fieldinfo: t -> fieldinfo -> fieldinfo
  val model_info: t -> model_info -> model_info
  val logic_var: t -> logic_var -> logic_var
  val kernel_function: t -> kernel_function -> kernel_function
  val fundec: t -> fundec -> fundec
end

module Get = struct
  let varinfo b = b.get_varinfo
  let compinfo b = b.get_compinfo
  let fieldinfo b = b.get_fieldinfo
  let model_info b = b.get_model_info
  let enuminfo b = b.get_enuminfo
  let enumitem b = b.get_enumitem
  let stmt b = b.get_stmt
  let typeinfo b = b.get_typeinfo
  let logic_info b = b.get_logic_info
  let logic_type_info b = b.get_logic_type_info
  let logic_var b = b.get_logic_var
  let kernel_function b = b.get_kernel_function
  let fundec b = b.get_fundec
end

module Get_orig = struct
  let varinfo b = b.get_original_varinfo
  let compinfo b = b.get_original_compinfo
  let fieldinfo b = b.get_original_fieldinfo
  let model_info b = b.get_original_model_info
  let enuminfo b = b.get_original_enuminfo
  let enumitem b = b.get_original_enumitem
  let stmt b = b.get_original_stmt
  let typeinfo b = b.get_original_typeinfo
  let logic_info b = b.get_original_logic_info
  let logic_type_info b = b.get_original_logic_type_info
  let logic_var b = b.get_original_logic_var
  let kernel_function b = b.get_original_kernel_function
  let fundec b = b.get_original_fundec
end

module type Set = sig
  val varinfo: t -> varinfo -> varinfo -> unit
  val compinfo: t -> compinfo -> compinfo -> unit
  val enuminfo: t -> enuminfo -> enuminfo -> unit
  val enumitem: t -> enumitem -> enumitem -> unit
  val typeinfo: t -> typeinfo -> typeinfo -> unit
  val stmt: t -> stmt -> stmt -> unit

  val logic_info: t -> logic_info -> logic_info -> unit
  val logic_type_info:
    t -> logic_type_info -> logic_type_info -> unit
  val fieldinfo: t -> fieldinfo -> fieldinfo -> unit
  val model_info: t -> model_info -> model_info -> unit
  val logic_var: t -> logic_var -> logic_var -> unit
  val kernel_function:
    t -> kernel_function -> kernel_function -> unit
  val fundec: t -> fundec -> fundec -> unit
end

module Set = struct
  let varinfo b = b.set_varinfo
  let compinfo b = b.set_compinfo
  let fieldinfo b = b.set_fieldinfo
  let model_info b = b.set_model_info
  let enuminfo b = b.set_enuminfo
  let enumitem b = b.set_enumitem
  let stmt b = b.set_stmt
  let typeinfo b = b.set_typeinfo
  let logic_info b = b.set_logic_info
  let logic_type_info b = b.set_logic_type_info
  let logic_var b = b.set_logic_var
  let kernel_function b = b.set_kernel_function
  let fundec b = b.set_fundec
end

module Set_orig = struct
  let varinfo b = b.set_orig_varinfo
  let compinfo b = b.set_orig_compinfo
  let fieldinfo b = b.set_orig_fieldinfo
  let model_info b = b.set_model_info
  let enuminfo b = b.set_orig_enuminfo
  let enumitem b = b.set_orig_enumitem
  let stmt b = b.set_orig_stmt
  let typeinfo b = b.set_orig_typeinfo
  let logic_info b = b.set_orig_logic_info
  let logic_type_info b = b.set_orig_logic_type_info
  let logic_var b = b.set_orig_logic_var
  let kernel_function b = b.set_orig_kernel_function
  let fundec b = b.set_orig_fundec
end

module type Unset = sig
  val varinfo: t -> varinfo -> unit
  val compinfo: t -> compinfo -> unit
  val enuminfo: t -> enuminfo -> unit
  val enumitem: t -> enumitem -> unit
  val typeinfo: t -> typeinfo -> unit
  val stmt: t -> stmt -> unit

  val logic_info: t -> logic_info -> unit
  val logic_type_info: t -> logic_type_info -> unit
  val fieldinfo: t -> fieldinfo -> unit
  val model_info: t -> model_info -> unit
  val logic_var: t -> logic_var -> unit
  val kernel_function: t -> kernel_function -> unit
  val fundec: t -> fundec -> unit
end

module Unset = struct
  let varinfo b = b.unset_varinfo
  let compinfo b = b.unset_compinfo
  let fieldinfo b = b.unset_fieldinfo
  let model_info b = b.unset_model_info
  let enuminfo b = b.unset_enuminfo
  let enumitem b = b.unset_enumitem
  let stmt b = b.unset_stmt
  let typeinfo b = b.unset_typeinfo
  let logic_info b = b.unset_logic_info
  let logic_type_info b = b.unset_logic_type_info
  let logic_var b = b.unset_logic_var
  let kernel_function b = b.unset_kernel_function
  let fundec b = b.unset_fundec
end

module Unset_orig = struct
  let varinfo b = b.unset_orig_varinfo
  let compinfo b = b.unset_orig_compinfo
  let fieldinfo b = b.unset_orig_fieldinfo
  let model_info b = b.unset_model_info
  let enuminfo b = b.unset_orig_enuminfo
  let enumitem b = b.unset_orig_enumitem
  let stmt b = b.unset_orig_stmt
  let typeinfo b = b.unset_orig_typeinfo
  let logic_info b = b.unset_orig_logic_info
  let logic_type_info b = b.unset_orig_logic_type_info
  let logic_var b = b.unset_orig_logic_var
  let kernel_function b = b.unset_orig_kernel_function
  let fundec b = b.unset_orig_fundec
end

module Iter = struct
  let varinfo b = b.iter_visitor_varinfo
  let compinfo b = b.iter_visitor_compinfo
  let enuminfo b = b.iter_visitor_enuminfo
  let enumitem b = b.iter_visitor_enumitem
  let typeinfo b = b.iter_visitor_typeinfo
  let stmt b = b.iter_visitor_stmt
  let logic_info b = b.iter_visitor_logic_info
  let logic_type_info b = b.iter_visitor_logic_type_info
  let fieldinfo b = b.iter_visitor_fieldinfo
  let model_info b = b.iter_visitor_model_info
  let logic_var b = b.iter_visitor_logic_var
  let kernel_function b = b.iter_visitor_kernel_function
  let fundec b = b.iter_visitor_fundec
end

module Fold = struct
  let varinfo b = b.fold_visitor_varinfo
  let compinfo b = b.fold_visitor_compinfo
  let enuminfo b = b.fold_visitor_enuminfo
  let enumitem b = b.fold_visitor_enumitem
  let typeinfo b = b.fold_visitor_typeinfo
  let stmt b = b.fold_visitor_stmt
  let logic_info b = b.fold_visitor_logic_info
  let logic_type_info b = b.fold_visitor_logic_type_info
  let fieldinfo b = b.fold_visitor_fieldinfo
  let model_info b = b.fold_visitor_model_info
  let logic_var b = b.fold_visitor_logic_var
  let kernel_function b = b.fold_visitor_kernel_function
  let fundec b = b.fold_visitor_fundec
end

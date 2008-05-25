(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU General Public                   *)
(*  License version 2, as published by the Free Software Foundation.      *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU General Public License version 2 for more details         *)
(*  (enclosed in the file GPL).                                           *)
(*                                                                        *)
(**************************************************************************)

(* $Id: jc_typing.ml,v 1.204 2008/04/22 17:41:24 nrousset Exp $ *)

open Jc_env
open Jc_envset
open Jc_fenv
open Jc_pervasives
open Jc_constructors
open Jc_ast
open Format
open Jc_region
open Jc_iterators

exception Typing_error of Loc.position * string

let typing_error l = 
  Format.kfprintf 
    (fun fmt -> raise (Typing_error(l, flush_str_formatter()))) 
    str_formatter

let logic_type_table = Hashtbl.create 97

let exceptions_table = Hashtbl.create 97

let enum_types_table = Hashtbl.create 97

let structs_table = Hashtbl.create 97
let variants_table = Hashtbl.create 97

let mutable_fields_table = Hashtbl.create 97 (* structure name (string) -> field info *)
let committed_fields_table = Hashtbl.create 97 (* structure name (string) -> field info *)

let logic_functions_table = Hashtbl.create 97
let logic_functions_env = Hashtbl.create 97
let logic_constants_table = Hashtbl.create 97
let logic_constants_env = Hashtbl.create 97
let functions_table = Hashtbl.create 97
let functions_env = Hashtbl.create 97
let variables_table = Hashtbl.create 97
let variables_env = Hashtbl.create 97

let field_tag_counter = ref 0

let create_mutable_field st =
  incr field_tag_counter;
  let name = "committed_"^st.jc_struct_info_name in
  let fi = {
    jc_field_info_tag = !field_tag_counter;
    jc_field_info_name = name;
    jc_field_info_final_name = Jc_envset.get_unique_name name;
    jc_field_info_type = boolean_type;
    jc_field_info_root = st.jc_struct_info_root;
    jc_field_info_struct = st;
    jc_field_info_rep = false;
  } in
  Hashtbl.add committed_fields_table st.jc_struct_info_name fi

let find_struct_info loc id =
  try
    let st,_ = Hashtbl.find structs_table id in st
  with Not_found ->
    typing_error loc "undeclared structure %s" id

let find_struct_variant st =
  match st.jc_struct_info_root.jc_struct_info_variant with
    | None -> raise Not_found
    | Some vi -> vi

let is_numeric t =
  match t with
    | JCTnative (Tinteger | Treal) -> true
    | JCTenum _ -> true
    | _ -> false

let is_integer t =
  match t with
    | JCTnative Tinteger -> true
    | JCTenum _ -> true
    | _ -> false

let is_root_struct st = 
  match st.jc_struct_info_parent with None -> true | Some _ -> false

let lub_numeric_types t1 t2 =
  match t1,t2 with
    | JCTnative Treal,_ | _,JCTnative Treal -> Treal
    | _ -> Tinteger

let rec substruct st = function
  | (JCtag st') as tov ->
      if st == st' then true else
        let vi = struct_variant st and vi' = struct_variant st' in
        (vi == vi' && vi.jc_variant_info_is_union)
        || 
        begin match st.jc_struct_info_parent with
          | None -> false
          | Some p -> substruct p tov
        end
  | JCvariant vi ->
      struct_variant st == vi
  | JCunion ui ->
      struct_variant st == ui

let subtype ?(allow_implicit_cast=true) t1 t2 =
  match t1,t2 with
    | JCTnative t1, JCTnative t2 ->
        t1=t2
       (* TODO: integer is subtype of real *)
    | JCTenum ri1, JCTenum ri2 -> 
        allow_implicit_cast ||
          (Num.ge_num ri1.jc_enum_info_min ri2.jc_enum_info_min &&
             Num.le_num ri1.jc_enum_info_max ri2.jc_enum_info_max)
    | JCTenum _, JCTnative Tinteger ->
        true
    | JCTnative Tinteger, JCTenum _ -> 
        allow_implicit_cast 
    | JCTlogic s1, JCTlogic s2 ->
        s1=s2
    | JCTpointer(JCtag s1, _, _), JCTpointer(tov, _, _) -> 
        substruct s1 tov
    | JCTpointer(JCvariant v1, _, _), JCTpointer(JCvariant v2, _, _) ->
        v1 == v2
    | JCTnull, (JCTnull | JCTpointer _) ->
        true
    | _ ->
        false

let subtype_strict = subtype ~allow_implicit_cast:false

let mintype loc t1 t2 =
  try match t1,t2 with
    | JCTnative Tinteger, JCTnative Treal
    | JCTnative Treal, JCTnative Tinteger ->
        JCTnative Treal
    | JCTnative n1, JCTnative n2 ->
        if n1=n2 then t1 else raise Not_found
          (* TODO: integer is subtype of real *)
    | (JCTenum _ | JCTnative Tinteger), (JCTenum _| JCTnative Tinteger) ->
        Jc_pervasives.integer_type
    | JCTlogic s1, JCTlogic s2 ->
        if s1=s2 then t1 else raise Not_found
    | JCTpointer(JCtag s1, _, _), JCTpointer(tov, _, _) when substruct s1 tov ->
        t2
    | JCTpointer(tov, _, _), JCTpointer(JCtag s1, _, _) when substruct s1 tov ->
        t1
    | JCTpointer(JCvariant v1, _, _), JCTpointer(JCvariant v2, _, _) ->
        if v1 == v2 then t1 else raise Not_found
    | JCTnull, JCTnull -> JCTnull
    | JCTnull, JCTpointer _ -> t2
    | JCTpointer _, JCTnull -> t1       
    | JCTany, t | t, JCTany -> t
    | _ -> raise Not_found
  with Not_found ->
    typing_error loc "incompatible types: %a and %a"
      print_type t1 print_type t2

let unit_expr e =
  if e#typ = unit_type then e else 
    new expr_with ~typ:unit_type ~original_type:e#typ e

let same_type_no_coercion t1 t2 = 
  match t1,t2 with
    | JCTnative t1, JCTnative t2 -> t1=t2
    | JCTenum ei1, JCTenum ei2 -> ei1.jc_enum_info_name = ei2.jc_enum_info_name
    | JCTlogic s1, JCTlogic s2 -> s1=s2
    | JCTpointer(tov1,_,_), JCTpointer(tov2,_,_) -> 
        tag_or_variant_variant tov1 == tag_or_variant_variant tov2
    | JCTnull, JCTnull -> true
    | JCTnull, JCTpointer _
    | JCTpointer _, JCTnull -> true
    | _ -> false

let comparable_types t1 t2 =
  match t1,t2 with
    | JCTnative t1, JCTnative t2 -> t1=t2
    | JCTenum _, JCTenum _ -> true
    | JCTenum _, JCTnative Tinteger -> true
    | JCTnative Tinteger, JCTenum _ -> true
    | JCTlogic s1, JCTlogic s2 -> s1=s2
    | JCTpointer(tov1,_,_), JCTpointer(tov2,_,_) -> 
        tag_or_variant_variant tov1 == tag_or_variant_variant tov2
    | JCTnull, JCTnull -> true
    | JCTnull, JCTpointer _
    | JCTpointer _, JCTnull -> true
    | _ -> false


let rec list_assoc_name f id l =
  match l with
    | [] -> raise Not_found
    | fi::r -> 
        if (f fi) = id then fi
        else list_assoc_name f id r


let rec find_field_struct loc st allow_mutable = function
  | ("mutable" | "committed") as x ->
      if allow_mutable && !Jc_common_options.inv_sem = InvOwnership then
        let table =
          if x = "mutable" then mutable_fields_table
          else committed_fields_table
        in
        Hashtbl.find table (root_name st)
      else typing_error loc "field %s cannot be used here" x
  | f ->
      try
        list_assoc_name
          (fun f -> f.jc_field_info_name) f st.jc_struct_info_fields
      with Not_found ->
        match st.jc_struct_info_parent with
          | None -> 
              typing_error loc "no field %s in structure %s" 
                f st.jc_struct_info_name
          | Some st -> find_field_struct loc st allow_mutable f

  
let find_field loc ty f allow_mutable =
  match ty with
    | JCTpointer(JCtag st, _, _) -> find_field_struct loc st allow_mutable f
    | JCTpointer(JCvariant _, _, _)
    | JCTpointer(JCunion _, _, _)
    | JCTnative _ 
    | JCTenum _
    | JCTlogic _
    | JCTnull
    | JCTany ->
        typing_error loc "not a structure type"

let find_fun_info id = Hashtbl.find functions_env id

let find_logic_info id = Hashtbl.find logic_functions_env id

(* types *)

let type_type t =
  match t#node with
    | JCPTnative n -> JCTnative n
    | JCPTpointer (id, a, b) -> 
        (* first we try the most precise type (the tag) *)
        begin try
          let st, _ = Hashtbl.find structs_table id in
          JCTpointer(JCtag st, a, b)
        with Not_found ->
          try
            let vi = Hashtbl.find variants_table id in
            JCTpointer(JCvariant vi, a, b)
          with Not_found ->
            typing_error t#loc "unknown type or tag: %s" id
        end
    | JCPTidentifier id -> 
        try
          let _ = Hashtbl.find logic_type_table id in
          JCTlogic id
        with Not_found ->
          try
            let (ri (* ,_,_,_ *)) = Hashtbl.find enum_types_table id in
            JCTenum ri
          with Not_found ->
            typing_error t#loc "unknown type %s" id

let unary_op (t: [< operator_type]) (op: [< unary_op]) = op, t

let bin_op (t: [< operator_type]) (op: [< bin_op]) = op, t

(******************************************************************************)
(*                                  Patterns                                  *)
(******************************************************************************)

let valid_pointer_type st =
  JCTpointer(st, Some (Num.num_of_int 0), Some (Num.num_of_int 0))

(* ety = expected type *)
(* env: the variables already bound *)
(* vars: the var_info to use if encountering a given variable *)
(* Return: (vars, pat) where:
     vars is the environment of the binders of the pattern
     pat is the typed pattern. *)
let rec pattern env vars pat ety =
  let get_var ety id =
    let id = id#name in
    if List.mem_assoc id env then
      typing_error pat#loc
        "the variable %s appears twice in the pattern" id;
    try
      StringMap.find id vars
    with Not_found ->
      let vi = var ety id in
      vi.jc_var_info_assigned <- true;
      vi
  in
  let tpn, ty, newenv = match pat#node with
    | JCPPstruct(id, lpl) ->
        let tov = match ety with
          | JCTpointer(tov, _, _) -> tov
          | JCTnative _ | JCTenum _ | JCTlogic _ | JCTnull | JCTany ->
              typing_error pat#loc
                "this pattern doesn't match a structure nor a variant"
        in
        (* tag *)
        let st = find_struct_info id#loc id#name in
        if not (substruct st tov) then
          typing_error id#loc
            "tag %s is not a subtag of %s"
            st.jc_struct_info_name (tag_or_variant_name tov);
        (* fields *)
        let env, tlpl = List.fold_left
          (fun (env, acc) (l, p) ->
             let fi = find_field_struct l#loc st false l#name in
             let env, tp = pattern env vars p fi.jc_field_info_type in
             env, (fi, tp)::acc)
          (env, []) lpl
        in
        JCPstruct(st, List.rev tlpl), valid_pointer_type (JCtag st), env
    | JCPPvar id ->
        let vi = get_var ety id in
        JCPvar vi, ety, (id#name, vi)::env
    | JCPPor(p1, p2) ->
        let _, tp1 = pattern env vars p1 ety in
        let vars = pattern_vars vars tp1 in
        let env, tp2 = pattern env vars p2 ety in
        JCPor(tp1, tp2), ety, env
    | JCPPas(p, id) ->
        let env, tp = pattern env vars p ety in
        let vi = get_var (tp#typ) id in
        JCPas(tp, vi), ety, (id#name, vi)::env
    | JCPPany ->
        JCPany, ety, env
    | JCPPconst c ->
        let ty, _, c = const c in
        if not (subtype_strict ty ety) then
          typing_error pat#loc
            "type %a is not a subtype of %a" print_type ty print_type ety;
        JCPconst c, ety, env
  in newenv, new pattern ~typ:ty ~loc:pat#loc tpn
let pattern = pattern [] StringMap.empty

(******************************************************************************)
(*                                   Terms                                    *)
(******************************************************************************)

let num_op (op: [< `Badd | `Bsub | `Bmul | `Bdiv | `Bmod]) = op, Tinteger

let num_un_op t (op: [< `Uminus | `Ubw_not]) e =
  match op with
    | `Uminus
    | `Ubw_not -> JCTunary((unary_op t op :> unary_op * 'a),e)

let make_logic_unary_op loc (op : Jc_ast.unary_op) e2 =
  let t2 = e2#typ in
  match op with
    | `Unot -> assert false
    | ((`Uminus | `Ubw_not) as x) -> 
        if is_numeric t2 then
          let t = lub_numeric_types t2 t2 in
          JCTnative t,dummy_region,num_un_op (operator_of_native t) x e2
        else
          typing_error loc "numeric type expected"
(*    | `Upostfix_dec | `Upostfix_inc | `Uprefix_dec | `Uprefix_inc ->
        typing_error loc "pre/post incr/decr not allowed as logical term"*)

let term_coerce t1 t2 e =
  let tn1,e_int =
    match t1 with
      | JCTenum ri ->
(*
          let (_,to_int,_,_) = 
            Hashtbl.find enum_types_table ri.jc_enum_info_name 
          in
          { jc_term_node = JCTapp(to_int,[e]) ;
            jc_term_type = integer_type;
            jc_term_loc = e.jc_term_loc }  
*)
          Tinteger, e
      | JCTnative t -> t, e
      | _ -> assert false
  in
  match tn1,t2 with
    | Tinteger, Treal -> 
        let app = {
          jc_app_fun = real_of_integer;
          jc_app_args = [e_int];
          jc_app_region_assoc = [];
          jc_app_label_assoc = [];
        } in
        new term
          ~typ:real_type
          ~region:e#region
          ~name_label:e#name_label
          ~loc:e#loc
          (JCTapp app)
    | _ -> e_int

let logic_bin_op (t : [< operator_type ]) op =
  bin_op t op
(*
  match t,op with
    | _, BPgt -> gt_int
    | _, BPlt -> lt_int
    | _, BPge -> ge_int
    | _, BPle -> le_int
    | _, BPeq -> eq
    | _, BPneq -> neq
    | Tinteger, BPadd -> add_int
    | Treal, BPadd -> add_real
    | _, BPsub -> sub_int
    | _, BPmul -> mul_int
    | _, BPdiv -> div_int
    | _, BPmod -> mod_int
    | Tboolean, BPland -> band 
    | Tboolean, BPlor -> bor
        (* not allowed as expression op *)
    | _,BPimplies -> assert false
    | Tunit,_ -> assert false
    | _ -> assert false
*)

let make_logic_bin_op loc (op: [< bin_op]) e1 e2 =
  let t1 = e1#typ and t2 = e2#typ in
  match op with
    | `Bgt | `Blt | `Bge | `Ble ->
        if is_numeric t1 && is_numeric t2 then
          let t = lub_numeric_types t1 t2 in
          boolean_type,dummy_region,
          JCTbinary(term_coerce t1 t e1, logic_bin_op (operator_of_native t) op, 
                     term_coerce t2 t e2)
        else
          typing_error loc "numeric types expected for >, <, >= and <="
    | `Beq | `Bneq ->
        if is_numeric t1 && is_numeric t2 then
          let t = lub_numeric_types t1 t2 in
          boolean_type,dummy_region,
          JCTbinary(term_coerce t1 t e1, logic_bin_op (operator_of_native t) op,
                     term_coerce t2 t e2)
        else
        if is_pointer_type t1 && is_pointer_type t2 && (comparable_types t1 t2) then
          boolean_type,dummy_region,
          JCTbinary(e1, logic_bin_op `Unit op, e2)
        else
          typing_error loc "numeric or pointer types expected for == and !="
    | `Badd ->
        if is_pointer_type t1 && is_integer t2 then
          t1, e1#region, JCTshift(e1, term_coerce t2 Tinteger e2)
        else if is_numeric t1 && is_numeric t2 then
          let t = lub_numeric_types t1 t2 in
          (JCTnative t,dummy_region,
           JCTbinary(term_coerce t1 t e1,
                     logic_bin_op (operator_of_native t) op,
                     term_coerce t2 t e2))
        else
          typing_error loc "unexpected types for +"
    | `Bsub ->
        if is_pointer_type t1 && is_integer t2 then
          let _, _, te = make_logic_unary_op loc `Uminus e2 in
          let e2 = new term_with ~node:te e2 in
          t1,e1#region,JCTshift(e1, term_coerce t2 Tinteger e2)
        else if 
          is_pointer_type t1 && is_pointer_type t2 
          && comparable_types t1 t2 
        then
          (integer_type,dummy_region, 
           JCTbinary(e1, bin_op `Pointer `Bsub, e2))
        else if is_numeric t1 && is_numeric t2 then
          let t = lub_numeric_types t1 t2 in
          (JCTnative t,
           dummy_region,
           JCTbinary(term_coerce t1 t e1,
                     logic_bin_op (operator_of_native t) op, 
                     term_coerce t2 t e2))
        else
          typing_error loc "unexpected types for -"
    | `Bmul | `Bdiv | `Bmod | `Bbw_and | `Bbw_or | `Bbw_xor 
    | `Blogical_shift_right | `Barith_shift_right | `Bshift_left ->
        if is_numeric t1 && is_numeric t2 then
          let t = lub_numeric_types t1 t2 in
          (JCTnative t,dummy_region,
           JCTbinary(term_coerce t1 t e1,
                     logic_bin_op (operator_of_native t) op,
                     term_coerce t2 t e2))
        else typing_error loc "numeric types expected for *, / and %%"
    | `Bland | `Blor -> 
        let t=
          match (t1,t2) with
            | JCTnative t1, JCTnative t2 ->
                begin
                  match (t1,t2) with
                    | Tboolean,Tboolean -> Tboolean
                    | _ -> assert false (* TODO *)
                end
            | _ ->
                typing_error loc "booleans expected"
        in
        JCTnative t,
        dummy_region,
        JCTbinary(e1, logic_bin_op (operator_of_native t) op, e2)

        (* not allowed as term op *)
    | `Bimplies | `Biff -> assert false

(** Check that used logic labels appear in the environment,
and add the current [logic_label] to the node in [jc_nexpr_label].
[env] is the list of valid labels.
However, [logic_label] might be changed by the "\at" construction. *)
let rec type_logic_labels env logic_label e =
  let check e x =
    if not (List.mem x env) then
      typing_error e#loc "label `%a' not found" Jc_output_misc.label x
  in
  let iter_subs ?(env=env) logic_label =
    List.iter
      (fun e -> ignore (type_logic_labels env logic_label e))
      (INExpr.subs e)
  in
  e#set_logic_label logic_label;
  match e#node with
    | JCNEconst _ | JCNEvar _ | JCNEderef _ | JCNEbinary _
    | JCNEunary _ | JCNEassign _ | JCNEinstanceof _ | JCNEcast _
    | JCNEif _ | JCNEoffset _ | JCNEalloc _ | JCNEfree _ | JCNElet _
    | JCNEassert _ | JCNEloop _ | JCNEreturn _ | JCNEtry _
    | JCNEthrow _ | JCNEpack _ | JCNEunpack _ | JCNEmatch _ | JCNEquantifier _
    | JCNEmutable _ | JCNEtagequality _ | JCNErange _ ->
        iter_subs logic_label;
        env
    | JCNEapp(_, l, _) ->
        List.iter (check e) l;
        iter_subs logic_label;
        env
    | JCNEold _ ->
        check e LabelOld;
        iter_subs (Some LabelOld);
        env
    | JCNEat(_, l) ->
        check e l;
        iter_subs (Some l);
        env
    | JCNEblock el ->
        List.fold_left
          (fun env e -> type_logic_labels env logic_label e)
          env el
    | JCNElabel(lab, _) ->
        let lab = {
          label_info_name = lab;
          label_info_final_name = lab;
          times_used = 0;
        } in
        let env = (LabelName lab)::env in
        iter_subs ~env logic_label;
        env
let type_logic_labels env logic_label e =
  ignore (type_logic_labels env logic_label e)

let get_label e =
  match e#logic_label with
    | None -> typing_error e#loc "a memory state is needed here (\\at missing?)"
    | Some l -> l

let label_assoc loc id cur_label fun_labels effective_labels =
  match cur_label, fun_labels, effective_labels with
    | Some l, [lf], [] -> [lf,l]
    | _ ->
	try
	  List.map2
	    (fun l1 l2 -> (l1,l2))
	    fun_labels effective_labels
	with Invalid_argument _ ->
	  typing_error loc 
	    "wrong number of labels for %s" id
	  
let rec term env e =
  let ft = term env in
  let lab = ref "" in
  let label () = get_label e in
  let t, tr, te = match e#node with
    | JCNEconst c ->
        let t, tr, c = const c in t, tr, JCTconst c
    | JCNElabel(l, e1) ->
        let te1 = ft e1 in
        lab := l;
        te1#typ, te1#region, te1#node
    | JCNEvar id ->
        let vi =
          try List.assoc id env with Not_found ->
            try Hashtbl.find variables_env id with Not_found ->
              try Hashtbl.find logic_constants_env id with Not_found ->
                typing_error e#loc "unbound term identifier %s" id
        in
        vi.jc_var_info_type, vi.jc_var_info_region, JCTvar vi
    | JCNEderef(e1, f) ->
        let te1 = ft e1 in
        let fi = find_field e#loc te1#typ f true in
        fi.jc_field_info_type,
        Region.make_field te1#region fi,
        JCTderef(te1, label (), fi)
    | JCNEbinary(e1, op, e2) ->
        make_logic_bin_op e#loc op (ft e1) (ft e2)
    | JCNEunary(op, e1) ->
        make_logic_unary_op e#loc op (ft e1)
    | JCNEapp(id, labs, args) ->
        begin try
(* Yannick: no need for different rule for const logic *)
(*           if List.length args = 0 then *)
(*             let vi = Hashtbl.find logic_constants_env id in *)
(*             vi.jc_var_info_type, vi.jc_var_info_region, JCTvar vi *)
(*           else *)
	    begin
            let pi = find_logic_info id in
            let tl =
              try
                List.map2
                  (fun vi e ->
                     let ty = vi.jc_var_info_type in
                     let te = ft e in
                     if subtype_strict te#typ ty then te
                     else
                       typing_error e#loc 
                         "type %a expected instead of %a" 
                         print_type ty print_type te#typ) 
                  pi.jc_logic_info_parameters args
              with  Invalid_argument _ ->
                typing_error e#loc 
                  "wrong number of arguments for %s" id
            in
            let ty = match pi.jc_logic_info_result_type with
              | None ->
                  typing_error e#loc
                    "the logic info %s is a predicate; it should be \
used as an assertion, not as a term" pi.jc_logic_info_name
              | Some ty -> ty
	    in
            let label_assoc = 
	      label_assoc e#loc id e#logic_label pi.jc_logic_info_labels labs 
	    in
            let app = {
              jc_app_fun = pi;
              jc_app_args = tl;
              jc_app_region_assoc = [];
              jc_app_label_assoc = label_assoc;
            } in
            ty, Region.make_var ty pi.jc_logic_info_name, JCTapp app
          end
        with Not_found ->
          typing_error e#loc "unbound logic function identifier %s" id
        end
    | JCNEinstanceof(e1, t) ->
        boolean_type,
        dummy_region,
        JCTinstanceof(ft e1, label (), find_struct_info e#loc t)
    | JCNEcast(e1, t) ->
        let te1 = ft e1 in
        begin try
          let ri = Hashtbl.find enum_types_table t in
          if is_numeric te1#typ then
            JCTenum ri, dummy_region, JCTrange_cast(te1, ri)
          else
            typing_error e#loc "numeric type expected"
        with Not_found ->
          let st = find_struct_info e#loc t in
          match te1#typ with
            | JCTpointer(st1, a, b) ->
                if substruct st st1 then
                  (JCTpointer(JCtag st, a, b),
                   te1#region,
                   JCTcast(te1, label (), st))
                else
                  typing_error e#loc "invalid cast"
            | JCTnative _ | JCTlogic _ | JCTenum _ | JCTnull | JCTany ->
                typing_error e#loc "only structures can be cast"
        end
    | JCNEif(e1, e2, e3) ->
        let te1 = ft e1 and te2 = ft e2 and te3 = ft e3 in
        begin match te1#typ with
          | JCTnative Tboolean ->
              let t =
                let t2 = te2#typ and t3 = te3#typ in
                if subtype_strict t2 t3 then t3 else
                  if subtype_strict t3 t2 then t2 else
                    typing_error e#loc "incompatible result types"
              in
              t, te1#region, JCTif(te1, te2, te3)
          | _ -> typing_error e#loc "boolean expression expected"
        end
    | JCNEoffset(k, e1) ->
        let te1 = ft e1 in
        begin match te1#typ with
          | JCTpointer(JCtag st, _, _) ->
              integer_type, dummy_region, JCToffset(k, te1, st)
          | JCTpointer((JCvariant _ | JCunion _), _, _) ->
              assert false (* TODO *)
          | JCTnative _ | JCTlogic _ | JCTenum _ | JCTnull | JCTany ->
              typing_error e#loc "pointer expected"
        end        
    | JCNElet(pty, id, Some e1, e2) ->
        let te1 = ft e1 in
        let ty = match pty with
          | None -> te1#typ
          | Some pty ->
              let ty = type_type pty in
              if not (subtype te1#typ ty) then
                typing_error pty#loc
                  "inferred type is not a subtype of declared type"
              else
                ty
        in
        let vi = var ty id in
        let te2 = term ((id, vi)::env) e2 in
        te2#typ, te2#region, te2#node
    | JCNElet(Some pty, id, None, e2) ->
        let vi = var (type_type pty) id in
        let te2 = term ((id, vi)::env) e2 in
        te2#typ, te2#region, te2#node
    | JCNElet(None, _, None, _) ->
        typing_error e#loc "let with no initial value must have a type"
    | JCNEmatch(arg, pel) ->
        let targ = ft arg in
        let rty, tpel = match pel with
          | [] -> assert false (* should not be allowed by the parser *)
          | (p1, e1)::rem ->
              (* type the first item *)
              let penv, tp1 = pattern p1 targ#typ in
              let te1 = term (penv @ env) e1 in
              (* type the remaining items *)
              List.fold_left
                (fun (accrty, acctpel) (p, e2) ->
                   let penv, tp = pattern p targ#typ in
                   let te2 = term (penv @ env) e2 in
                   mintype e#loc accrty te2#typ,
                   (tp, te2)::acctpel)
                (te1#typ, [tp1, te1])
                (List.rev rem)
        in
        rty, targ#region, JCTmatch(targ, List.rev tpel)
    | JCNEold e1 ->
        let te1 = ft e1 in
        te1#typ, te1#region, JCTold te1
    | JCNEat(e1, lab) ->
        let te1 = ft e1 in
        te1#typ, te1#region, JCTat(te1, lab)
    | JCNEmutable(e, t) -> assert false (* TODO *)
    | JCNEtagequality(t1, t2) -> assert false (* TODO *)
    | JCNErange(Some e1, Some e2) ->
        let e1 = ft e1 and e2 = ft e2 in
        let t1 = e1#typ and t2 = e2#typ in
        assert (is_numeric t1 && is_numeric t2);
        let t = lub_numeric_types t1 t2 in
        JCTnative t, dummy_region, 
        JCTrange(Some (term_coerce t1 t e1),Some (term_coerce t2 t e2))
    | JCNErange(Some e, None) ->
        let e = ft e in
        let t = e#typ in
        assert (is_numeric t);
        t, dummy_region,JCTrange(Some e,None)
    | JCNErange(None, Some e) ->
        let e = ft e in
        let t = e#typ in
        assert (is_numeric t);
        t,dummy_region, JCTrange(None,Some e)
    | JCNErange(None, None) ->
        integer_type, dummy_region,JCTrange(None,None)
    (* Not terms: *)
    | JCNEassign _ | JCNEalloc _ | JCNEfree _ | JCNEblock _ | JCNEassert _
    | JCNEloop _ | JCNEreturn _ | JCNEtry _ | JCNEthrow _ | JCNEpack _
    | JCNEunpack _ | JCNEquantifier _ ->
        typing_error e#loc "construction not allowed in logic terms"
  in
  new term
    ~typ: t
    ~region: tr
    ~name_label: !lab
    ~loc: e#loc
    te

(******************************************************************************)
(*                                 Assertions                                 *)
(******************************************************************************)

(*
let term label_env logic_label env e =
  type_logic_labels label_env logic_label e;
  term env e
*)

(*  
let rel_unary_op loc op t =
  match op with
    | `Unot | `Ubw_not -> assert false
    | `Uminus | `Uplus -> 
        typing_error loc "not a proposition"
    | `Upostfix_dec | `Upostfix_inc | `Uprefix_dec | `Uprefix_inc ->
        typing_error loc "pre/post incr/decr not allowed as logical term"
*)

let rel_bin_op t (op: [< comparison_op]) =
  (bin_op t op :> pred_rel_op)
(*
  match t,op with
    | Tinteger,BPgt -> gt_int
    | Tinteger,BPlt -> lt_int
    | Tinteger,BPge -> ge_int
    | Tinteger,BPle -> le_int
    | _,BPeq -> eq
    | _,BPneq -> neq
    | _,(BPadd | BPsub | BPmul | BPdiv | BPmod) -> assert false
    | _,(BPland | BPlor | BPimplies | BPiff) -> assert false
    | _ -> assert false  (* TODO *)
*)

let make_and a1 a2 =
  match (a1#node, a2#node) with
    | (JCAtrue,_) -> a2
    | (_,JCAtrue) -> a1
(*
    | (LFalse,_) -> LFalse
    | (_,LFalse) -> LFalse
*)
    | (JCAand l1 , JCAand l2) -> new assertion(JCAand(l1@l2))
    | (JCAand l1 , _ ) -> new assertion(JCAand(l1@[a2]))
    | (_ , JCAand l2) -> new assertion(JCAand(a1::l2))
    | _ -> new assertion(JCAand [a1;a2])

let make_or a1 a2 =
  match (a1#node, a2#node) with
    | (JCAfalse,_) -> a2
    | (_,JCAfalse) -> a1
(*
    | (LFalse,_) -> LFalse
    | (_,LFalse) -> LFalse
*)
    | (JCAor l1 , JCAor l2) -> new assertion(JCAor(l1@l2))
    | (JCAor l1 , _ ) -> new assertion(JCAor(l1@[a2]))
    | (_ , JCAor l2) -> new assertion(JCAor(a1::l2))
    | _ -> new assertion(JCAor [a1;a2])

let make_rel_bin_op loc (op: [< comparison_op]) e1 e2 =
  let t1 = e1#typ and t2 = e2#typ in
  match op with
    | `Bgt | `Blt | `Bge | `Ble ->
        if is_numeric t1 && is_numeric t2 then
          let t = lub_numeric_types t1 t2 in
          JCArelation(term_coerce t1 t e1,
                      rel_bin_op (operator_of_native t) op,
                      term_coerce t2 t e2)
        else
          typing_error loc "numeric types expected for >, <, >= and <="
    | `Beq | `Bneq ->
        if is_numeric t1 && is_numeric t2 then
          let t = lub_numeric_types t1 t2 in
          JCArelation(term_coerce t1 t e1,
                      rel_bin_op (operator_of_native t) op,
                      term_coerce t2 t e2)
        else
          let t = operator_of_type (mintype loc t1 t2) in
          if comparable_types t1 t2 then 
            JCArelation(e1, rel_bin_op t op, e2)
          else
            typing_error loc "terms should have the same type for == and !="
(*        (* non propositional operators *)
    | `Badd | `Bsub | `Bmul | `Bdiv | `Bmod | `Bbw_and | `Bbw_or | `Bbw_xor
    | `Blogical_shift_right | `Barith_shift_right | `Bshift_left 
        -> assert false
        (* already recognized as connectives *)
    | `Bland | `Blor -> assert false 
    | `Bimplies -> assert false
    | `Biff -> assert false*)

let tag env hierarchy t =
  let check_hierarchy loc st =
    if hierarchy <> "" &&
      root_name st != hierarchy then
        typing_error loc
          "this is in the hierarchy of %s, while it should be in the hierarchy \
of %s"
          (root_name st) hierarchy
  in
  let tt = match t#node with
    | JCPTtag id ->
        let st = find_struct_info id#loc id#name in
        check_hierarchy id#loc st;
        JCTtag st
    | JCPTbottom -> JCTbottom
    | JCPTtypeof tof ->
        let ttof = term env tof in
        match ttof#typ with
          | JCTpointer(JCtag st, _, _) ->
              check_hierarchy tof#loc st;
              JCTtypeof (ttof, st)
          | _ -> typing_error tof#loc "tag pointer expression expected"
  in
  new tag ~loc:t#loc tt 

let rec assertion env e =
  let fa = assertion env in
  let ft = term env in
  let lab = ref "" in
  let label () = get_label e in
  let ta = match e#node with
    | JCNElabel(l, e) ->
        let te = fa e in
        lab := l;
        te#node
    | JCNEbinary(e1, (`Bland | `Blor | `Bimplies | `Biff as op), e2) ->
        let a1 = fa e1 and a2 = fa e2 in
        begin match op with
          | `Bland -> (make_and a1 a2)#node
          | `Blor -> (make_or a1 a2)#node
          | `Bimplies -> JCAimplies(a1, a2)
          | `Biff -> JCAiff(a1, a2)
        end
    | JCNEbinary(e1, (#comparison_op as op), e2) ->
        make_rel_bin_op e#loc op (ft e1) (ft e2)
    | JCNEunary(`Unot, e1) ->
        JCAnot(fa e1)
    | JCNEapp (id, labs, args) ->
        begin try
          let pi = find_logic_info id in
          let tl = try
            List.map2
              (fun vi e ->
                 let ty = vi.jc_var_info_type in
                 let te = ft e in
                 if subtype_strict te#typ ty then te
                 else
                   typing_error e#loc 
                     "type %a expected instead of %a" 
                     print_type ty print_type te#typ) 
              pi.jc_logic_info_parameters args
          with Invalid_argument _ ->
            typing_error e#loc "wrong number of arguments for %s" id
          in
	  let label_assoc =
		label_assoc e#loc id e#logic_label pi.jc_logic_info_labels labs
	      in
          let app = {
            jc_app_fun = pi;
            jc_app_args = tl;
            jc_app_region_assoc = [];
            jc_app_label_assoc = label_assoc;
          } in
          JCAapp app
        with Not_found ->
          typing_error e#loc "unbound predicate identifier %s" id
        end
    | JCNEinstanceof(e1, t) -> 
        JCAinstanceof(ft e1, label (), find_struct_info e#loc t)
    | JCNEcast _ -> assert false (* TODO *)
    | JCNEif(e1,e2,e3) ->
        let te1 = ft e1 and te2 = fa e2 and te3 = fa e3 in
        begin
          match te1#typ with
            | JCTnative Tboolean ->
                JCAif(te1,te2,te3)
            | _ ->
                typing_error e1#loc 
                  "boolean expression expected"
        end
    | JCNElet _ -> assert false (* TODO *)
    | JCNEmatch(arg, pel) ->
        let targ = ft arg in
        let tpal = List.map
          (fun (pat, body) ->
             let vars, tpat = pattern pat targ#typ in
             let tbody = assertion (vars @ env) body in
             tpat, tbody)
          pel
        in
        JCAmatch(targ, tpal)
    | JCNEquantifier(q, ty, idl, e1) ->
        let ty = type_type ty in
        (make_quantifier q e#loc ty idl env e1)#node
    | JCNEold e1 ->
        JCAold(fa e1)
    | JCNEat(e1, lab) ->
        JCAat(fa e1, lab)
    | JCNEmutable(e, t) ->
        let te = ft e in
        let te_st = match te#typ with
          | JCTpointer(JCtag st, _, _) -> st
          | _ -> typing_error e#loc "tag pointer expression expected"
        in
        let tt = tag env (root_name te_st) t in
        JCAmutable(te, te_st, tt)
    | JCNEtagequality(tag1, tag2) ->
        let ttag1 = tag env "" tag1 in
        let ttag2 = tag env "" tag2 in
        let st = match ttag1#node, ttag2#node with
          | JCTbottom, JCTbottom -> None
          | JCTbottom, JCTtag st
          | JCTtag st, JCTbottom
          | JCTbottom, JCTtypeof(_, st)
          | JCTtypeof(_, st), JCTbottom -> Some (root_name st)
          | JCTtag st1, JCTtag st2
          | JCTtypeof(_, st1), JCTtag st2
          | JCTtag st1, JCTtypeof(_, st2)
          | JCTtypeof(_, st1), JCTtypeof(_, st2) ->
              if st1.jc_struct_info_root != st2.jc_struct_info_root then
                typing_error e#loc "the hierarchy %s and %s are \
different"
                  (root_name st1)
                  (root_name st2)
              else
                Some (root_name st1)
        in
        JCAtagequality(ttag1, ttag2, st)
    (* Boolean terms: *)
    | JCNEconst _ | JCNEvar _ | JCNEderef _ ->
        let t = ft e in
        begin match t#typ with
          | JCTnative Tboolean -> JCAbool_term t
          | _ -> typing_error e#loc "non boolean expression"
        end
    (* Not assertions: *)
    | JCNEoffset _ | JCNErange _ | JCNEassign _ | JCNEalloc _ | JCNEfree _
    | JCNEassert _ | JCNEblock _ | JCNEloop _ | JCNEreturn _ | JCNEtry _
    | JCNEthrow _ | JCNEpack _ | JCNEunpack _ | JCNEbinary _ | JCNEunary _ ->
        typing_error e#loc "construction not allowed in logic assertions"
  in
  new assertion
    ~name_label: !lab
    ~loc: e#loc
    ta

and make_quantifier q loc ty idl env e : assertion =
  match idl with
    | [] -> assertion env e
    | id :: r ->
        let vi = var ty id in
        let env = (id, vi) :: env in
        let f = 
          JCAquantifier (q, vi, make_quantifier q loc ty r env e) 
        in
        new assertion ~loc:loc f

(******************************************************************************)
(*                                Expressions                                 *)
(******************************************************************************)

let loop_annot =
  let cnt = ref 0 in fun ~invariant ~free_invariant ~variant ->
    incr cnt;
    {
      jc_loop_tag = !cnt;
      jc_loop_invariant = invariant;
      jc_free_loop_invariant = free_invariant;
      jc_loop_variant = variant;
    }

let make_unary_op loc (op : Jc_ast.unary_op) e2 =
  let t2 = e2#typ in
  match op with
(*    | `Uprefix_inc | `Upostfix_inc | `Uprefix_dec | `Upostfix_dec (*as op*) ->
        begin
          let t = if is_pointer_type t2 then t2 else
            JCTnative (lub_numeric_types t2 t2) in
          match e2#node with
            | JCEvar v ->
                v.jc_var_info_assigned <- true;
                t, v.jc_var_info_region,
                assert false (* JCEincr_local(incr_op op, v) *)
            | JCEderef(e,f) ->
                t, e2#region,
                assert false (* JCEincr_heap(incr_op op, e, f) *)
            | _ -> typing_error e2#loc "not an lvalue"
        end*)
    | `Unot as op -> 
        let t=
          match t2 with
            | JCTnative t2 ->
                begin
                  match t2 with
                    | Tboolean -> Tboolean
                    | _ -> assert false (* TODO *)
                end
            | _ ->
                typing_error loc "boolean expected"
        in (JCTnative t,dummy_region,
            JCEunary(unary_op (operator_of_native t) op, e2))
    | `Uminus | `Ubw_not as op -> 
        if is_numeric t2 then
          let t = lub_numeric_types t2 t2 in
          (JCTnative t,dummy_region,
           JCEunary(unary_op (operator_of_native t) op, e2))
        else
          typing_error loc "numeric type expected"

let coerce t1 t2 e =
  let tn1, e_int =
    match t1 with
      | JCTenum ri -> Tinteger, e
      | JCTnative t -> t, e
      | _ -> assert false
  in
  match tn1, t2 with
    | Tinteger,Treal ->
        new expr
          ~typ: real_type
          ~loc: e#loc
          (JCEapp{
             jc_call_fun = JCfun real_of_integer_;
             jc_call_args = [e_int];
             jc_call_region_assoc = [];
             jc_call_label_assoc = [];
           })
    | _ -> e_int

let make_bin_op loc (op: operational_op) e1 e2 =
  let t1 = e1#typ and t2 = e2#typ in
  match op with
    | `Bgt | `Blt | `Bge | `Ble as op ->
        if is_numeric t1 && is_numeric t2 then
          let t = lub_numeric_types t1 t2 in
          (boolean_type, dummy_region,
           JCEbinary(coerce t1 t e1,
                     bin_op (operator_of_native t) op,
                     coerce t2 t e2))
        else
          typing_error loc "numeric types expected for <, >, <= and >="
    | `Beq | `Bneq as op ->
        if is_numeric t1 && is_numeric t2 then
          let t = lub_numeric_types t1 t2 in
          (boolean_type, dummy_region,
           JCEbinary(coerce t1 t e1,
                     bin_op (operator_of_native t) op,
                     coerce t2 t e2))
        else
          if t1 = boolean_type && t2 = boolean_type then
            (boolean_type, dummy_region, JCEbinary (e1, bin_op `Boolean op, e2))
          else
            if is_pointer_type t1 && is_pointer_type t2 &&
              comparable_types t1 t2 then
                (boolean_type, dummy_region,
                 JCEbinary(e1, bin_op `Pointer op, e2))
            else
              typing_error loc
                "numeric, boolean or pointer types expected for == and !="
    | `Badd as op  ->
        if is_pointer_type t1 && is_integer t2 then
          t1, e1#region, JCEshift(e1, coerce t2 Tinteger e2)
        else if is_numeric t1 && is_numeric t2 then
          let t = lub_numeric_types t1 t2 in
          (JCTnative t,
           dummy_region,
           JCEbinary(coerce t1 t e1,
                     bin_op (operator_of_native t) op,
                     coerce t2 t e2))
        else
          typing_error loc "unexpected types for +"
    | `Bsub as op  ->
        if is_pointer_type t1 && is_integer t2 then
          let _,_,te = make_unary_op loc `Uminus e2 in
          let e2 = new expr_with ~node:te e2 in
          t1, e1#region, JCEshift(e1, coerce t2 Tinteger e2)
        else if 
          is_pointer_type t1 && is_pointer_type t2 
          && comparable_types t1 t2 
        then
          (integer_type, dummy_region,
           JCEbinary(e1, bin_op `Pointer `Bsub, e2))
        else if is_numeric t1 && is_numeric t2 then
          let t = lub_numeric_types t1 t2 in
          (JCTnative t, dummy_region,
           JCEbinary(coerce t1 t e1,
                     bin_op (operator_of_native t) op,
                     coerce t2 t e2))
        else
          typing_error loc "unexpected types for -"
    | `Bmul | `Bdiv | `Bmod | `Bbw_and | `Bbw_or | `Bbw_xor 
    | `Blogical_shift_right | `Barith_shift_right | `Bshift_left as op  ->
        if is_numeric t1 && is_numeric t2 then
          let t = lub_numeric_types t1 t2 in
          (JCTnative t,dummy_region,
           JCEbinary(coerce t1 t e1,
                     bin_op (operator_of_native t) op,
                     coerce t2 t e2))
        else typing_error loc "numeric types expected for bitwaise operators"
(*    | `Bland | `Blor as op -> 
        let t = match (t1, t2) with
          | JCTnative t1, JCTnative t2 ->
              begin match (t1, t2) with
                | Tboolean, Tboolean -> Tboolean
                | _ -> assert false (* TODO *)
              end
          | _ -> typing_error loc "booleans expected"
        in
        (JCTnative t,
         dummy_region,
         JCEbinary(e1, bin_op (operator_of_native t) op, e2))*)
    (* not allowed as expression op *)
(*    | `Bimplies | `Biff -> assert false*)

let rec expr env e =
  let fe = expr env in
  let fa = assertion env in
  let ft = term env in
  let lab = ref "" in
  let ty, region, typed_e = match e#node with
    (* old expressions *)
    | JCNEconst c ->
        let t, tr, tc = const c in t, tr, JCEconst tc
    | JCNElabel(l, e1) ->
        let te1 = fe e1 in
        lab := l;
        te1#typ, te1#region, te1#node
    | JCNEvar id ->
        let vi =
          try List.assoc id env with Not_found -> 
            try Hashtbl.find variables_env id with Not_found -> 
              try Hashtbl.find logic_constants_env id with Not_found -> 
                typing_error e#loc "unbound identifier %s" id
        in vi.jc_var_info_type, vi.jc_var_info_region, JCEvar vi
    | JCNEderef(e1, f) -> 
        let te1 = fe e1 in
        let fi = find_field e#loc te1#typ f false in
        fi.jc_field_info_type,
        Region.make_field te1#region fi,
        JCEderef(te1, fi)
    | JCNEbinary(e1, (#operational_op as op), e2) ->
        make_bin_op e#loc op (fe e1) (fe e2)
    | JCNEbinary(e1, (#logical_op as op), e2) ->
        let te1 = fe e1 and te2 = fe e2 in
	boolean_type, dummy_region, begin match op with
	  | `Bland ->
              JCEif(
                te1,
                te2,
                new expr ~typ:boolean_type (JCEconst(JCCboolean false))
              )
          | `Blor ->
              JCEif(
                te1,
                new expr ~typ:boolean_type (JCEconst(JCCboolean true)),
                te2
              )
	  | `Bimplies | `Biff ->
	      typing_error e#loc "unexpected operator in expression"
	end
    | JCNEunary(op, e1) ->
        make_unary_op e#loc op (fe e1)
    | JCNEapp(id, labs, args) -> 
        begin try
          let fi = find_fun_info id in
          assert (labs = []);
          let tl = try
            List.map2
              (fun vi e ->
                 let ty = vi.jc_var_info_type in
                 let te = fe e in
                 if subtype te#typ ty then te
                 else
                   typing_error e#loc "type %a expected instead of %a" 
                     print_type ty print_type te#typ) 
              fi.jc_fun_info_parameters args
          with Invalid_argument _ ->
            typing_error e#loc "wrong number of arguments for %s" id
          in
          let ty = fi.jc_fun_info_result.jc_var_info_type in
          ty,
          Region.make_var ty fi.jc_fun_info_name,
          JCEapp {
            jc_call_fun = JCfun fi;
            jc_call_args = tl;
            jc_call_region_assoc = [];
            jc_call_label_assoc = [];
          }
        with Not_found -> try
(* Yannick: no need for different rule for const logic *)
(*           if List.length args = 0 then *)
(*             let vi = Hashtbl.find logic_constants_env id in *)
(*             vi.jc_var_info_type, vi.jc_var_info_region, JCEvar vi *)
(*           else *)
	  begin
            let pi = find_logic_info id in
            let tl =
              try
                List.map2
                  (fun vi e ->
                     let ty = vi.jc_var_info_type in
                     let te = fe e in
                     if subtype_strict te#typ ty then te
                     else
                       typing_error e#loc 
                         "type %a expected instead of %a" 
                         print_type ty print_type te#typ) 
                  pi.jc_logic_info_parameters args
              with  Invalid_argument _ ->
                typing_error e#loc 
                  "wrong number of arguments for %s" id
            in
            let ty = match pi.jc_logic_info_result_type with
              | None ->
                  typing_error e#loc
                    "the logic info %s is a predicate; it should be \
used as an assertion, not as a term" pi.jc_logic_info_name
              | Some ty -> ty
            in
            let label_assoc = 
              match e#logic_label, pi.jc_logic_info_labels, labs with
                | Some l, [lf], [] -> [lf,l]
                | _ ->
                    try
                      List.map2
                        (fun l1 l2 -> (l1,l2))
                        pi.jc_logic_info_labels labs
                    with Invalid_argument _ ->
                      typing_error e#loc 
                        "wrong number of labels for %s" id
            in
            let app = {
              jc_call_fun = JClogic_fun pi;
              jc_call_args = tl;
              jc_call_region_assoc = [];
              jc_call_label_assoc = label_assoc;
            } in
            ty, Region.make_var ty pi.jc_logic_info_name, JCEapp app
          end
        with Not_found ->
          typing_error e#loc "unbound function or logic function identifier %s" id
        end
    | JCNEassign(e1, e2) -> 
        let te1 = fe e1 and te2 = fe e2 in
        let t1 = te1#typ and t2 = te2#typ in
        if subtype t2 t1 then 
          match te1#node with
            | JCEvar v ->
                v.jc_var_info_assigned <- true;
                t1, te2#region, JCEassign_var(v, te2)
            | JCEderef(e, f) ->
                t1, te2#region, JCEassign_heap(e, f, te2)
            | _ -> typing_error e1#loc "not an lvalue"
        else
          typing_error e2#loc 
            "type '%a' expected in assignment instead of '%a'"
            print_type t1 print_type t2
    | JCNEinstanceof(e1, t) -> 
        let te1 = fe e1 in
        let st = find_struct_info e#loc t in
        boolean_type, dummy_region, JCEinstanceof(te1, st)
    | JCNEcast(e1, t) -> 
        let te1 = fe e1 in
        begin try
          let ri = Hashtbl.find enum_types_table t in
          if is_numeric te1#typ then
            JCTenum ri, te1#region, JCErange_cast(te1, ri)
          else
            typing_error e#loc "numeric type expected"
        with Not_found ->
          let st = find_struct_info e#loc t in
          match te1#typ with
            | JCTpointer(st1, a, b) ->
                if substruct st st1 then
                  (JCTpointer(JCtag st, a, b),
                   te1#region,
                   JCEcast(te1, st))
                else
                  typing_error e#loc "invalid cast"
            | _ ->
                typing_error e#loc
                  "only structures or numeric types can be cast"
        end
    | JCNEif(e1,e2,e3) ->
        let te1 = fe e1 and te2 = fe e2 and te3 = fe e3 in
        begin match te1#typ with
          | JCTnative Tboolean ->
	      let te2,te3 =
		if same_type_no_coercion te2#typ te3#typ then
		  te2,te3
		else
		  unit_expr te2, unit_expr te3 
	      in
              let t = mintype e#loc
                te2#typ
                te3#typ
              in
              t, te1#region, JCEif(te1, te2, te3)
          | _ -> typing_error e1#loc "boolean expression expected"
        end
    | JCNEoffset(k, e1) ->
        let te1 = fe e1 in
        begin match te1#typ with 
          | JCTpointer(JCtag st, _, _) ->
              integer_type, dummy_region, JCEoffset(k, te1, st)
          | JCTpointer((JCvariant _ | JCunion _), _, _) ->
              assert false (* TODO *)
          | _ -> typing_error e#loc "pointer expected"
        end
    | JCNEalloc(e1, t) ->
        let st = find_struct_info e#loc t in
        let ty = JCTpointer(JCtag st, Some zero, None) in
        ty, Region.make_var ty "alloc", JCEalloc (fe e1, st)
    | JCNEfree e1 ->
        unit_type, dummy_region, JCEfree (fe e1)
    | JCNElet(tyo, id, e1o, e2) ->
        let ty, te1o = match tyo, e1o with
          | None, None ->
              typing_error e#loc "let with no initial value must have a type"
          | Some ty, None ->
              type_type ty, None
          | None, Some e1 ->
              let te1 = fe e1 in
              te1#typ, Some te1
          | Some ty, Some e1 ->
              let te1 = fe e1 in
              let tty = type_type ty in
              if subtype te1#typ tty then
                tty, Some te1
              else
                typing_error e#loc
                  "inferred type is not a subtype of declared type"
        in
        let vi = var ty id in
        let te2 = expr ((id, vi)::env) e2 in
        te2#typ,
        te2#region,
        JCElet(vi, te1o, te2)
    (* old statements *)
    | JCNEassert e1 ->
        unit_type, dummy_region, JCEassert(assertion env e1)
    | JCNEblock el ->
        (* No warning when a value is ignored. *)
        let tel = List.map fe el in
        begin match List.rev tel with
          | [] ->
              unit_type, dummy_region, JCEconst JCCvoid
          | last::but_last ->
	      let but_last = List.map unit_expr but_last in
              last#typ, last#region, JCEblock(List.rev(last::but_last))
        end
    | JCNEloop(i, vo, body) ->
        unit_type,
        dummy_region,
        JCEloop(
          loop_annot
            ~invariant:(fa i)
            ~free_invariant:true_assertion
            ~variant:(apply_option ft vo),
          fe body)
    | JCNEreturn None ->
        unit_type, dummy_region, JCEreturn_void
    | JCNEreturn(Some e1) ->
        let te1 = fe e1 in
        let vi = List.assoc "\\result" env in
        if subtype te1#typ vi.jc_var_info_type then
          (unit_type,
           te1#region,
           JCEreturn(vi.jc_var_info_type, te1))
        else
          typing_error e#loc "type `%a' expected in return instead of `%a'"
            print_type vi.jc_var_info_type print_type te1#typ
    | JCNEtry(body, catches, finally) ->
        let tbody = unit_expr (fe body) in
        let tfinally = unit_expr (fe finally) in
        let tcatches = List.map begin function (id, v, cbody) ->
          let ei = try
            Hashtbl.find exceptions_table id#name
          with Not_found ->
            typing_error id#loc "undeclared exception: %s" id#name
          in
            match ei.jc_exception_info_type with
              | Some tei -> 
		  let vi = var tei v in
		    ei, Some vi, unit_expr (expr ((v, vi) :: env) cbody)
             | None -> ei, None, unit_expr (fe cbody)
        end catches in
          tbody#typ,
        tbody#region,
        JCEtry(tbody, tcatches, tfinally)
    | JCNEthrow(id, e1o) ->
        let ei = try
          Hashtbl.find exceptions_table id#name
        with Not_found ->
          typing_error id#loc "undeclared exception %s" id#name
        in
        let region, te1o = match e1o with
          | None -> dummy_region, None
          | Some e1 ->
              let te1 = fe e1 in
              let tei = match ei.jc_exception_info_type with
                | Some tei -> tei
                | None -> typing_error e#loc "this exception has no argument"
              in
              if subtype te1#typ tei then
                te1#region, Some te1
              else
                typing_error e#loc "type `%a' expected instead of `%a'"
                  print_type tei print_type te1#typ
        in
        Jc_pervasives.any_type, region, JCEthrow(ei, te1o)
    | JCNEpack(e1, t) ->
        let te1 = fe e1 in
        begin match te1#typ with
          | JCTpointer(JCtag st, _, _) ->
              let as_t = match t with
                | Some t -> find_struct_info t#loc t#name
                | None -> st
              in
              unit_type, te1#region, JCEpack(st, te1, as_t)
          | _ -> typing_error e#loc "only structures can be packed"
        end
    | JCNEunpack(e1, t) ->
        let te1 = fe e1 in 
        begin match te1#typ with
          | JCTpointer(JCtag st, _, _) ->
              let from_t = match t with
                | Some t -> find_struct_info t#loc t#name
                | None ->
                    let rec res = {
                      jc_struct_info_name = "bottom";
                      jc_struct_info_parent = None;
                      jc_struct_info_root = res;
                      jc_struct_info_fields = [];
                      jc_struct_info_variant = None;
                    }
                    in res
              in
              unit_type, te1#region, JCEunpack(st, te1, from_t)
          | _ -> typing_error e#loc "only structures can be unpacked"
        end
    | JCNEmatch(arg, pel) ->
        let targ = fe arg in
        let rty, tpel = match pel with
          | [] -> assert false (* should not be allowed by the parser *)
          | (p1, e1)::rem ->
              (* type the first item *)
              let penv, tp1 = pattern p1 targ#typ in
              let te1 = expr (penv @ env) e1 in
              (* type the remaining items *)
              List.fold_left
                (fun (accrty, acctpel) (p, e2) ->
                   let penv, tp = pattern p targ#typ in
                   let te2 = expr (penv @ env) e2 in
                   mintype e#loc accrty te2#typ,
                   (tp, te2)::acctpel)
                (te1#typ, [tp1, te1])
                rem
        in
        rty, targ#region, JCEmatch(targ, List.rev tpel)
    (* logic only *)
    | JCNEquantifier _ | JCNEold _ | JCNEat _ | JCNEmutable _
    | JCNEtagequality _ | JCNErange _ ->
        typing_error e#loc "construction not allowed in expressions"
  in
  new expr
    ~loc: e#loc
    ~typ: ty
    ~region: region
    ~name_label: !lab
    typed_e

(*******************************************************************************)
(*                                  Declarations                               *)
(*******************************************************************************)

let rec location_set env e =
  match e#node with
    | JCNElabel(l,e) -> 
        assert false (* TODO *)
    | JCNEvar id ->
        let vi =
          try List.assoc id env with Not_found ->
            try Hashtbl.find variables_env id with Not_found ->
              typing_error e#loc "unbound identifier %s" id
        in
        begin match vi.jc_var_info_type with
          | JCTpointer _ ->
              vi.jc_var_info_type, vi.jc_var_info_region, JCLSvar vi
          | _ -> assert false
        end
    | JCNEbinary(e, `Badd, i) ->
        let ty,tr,te = location_set env e in
        let ti = term env i in
        begin
          match ty, ti#typ with 
            | JCTpointer(st,_,_), t2 when is_integer t2 ->
                begin match ti#node with
                  | JCTrange(t1,t2) -> ty,tr,JCLSrange(te,t1,t2)
                  | _ -> ty,tr,JCLSrange(te,Some ti,Some ti)
(* TODO ?
                  | _ -> ty,tr,JCLSshift(te,ti)
*)
                end
            | JCTpointer _, _ -> 
                typing_error i#loc "integer expected, %a found"
                  print_type ti#typ
            | _ -> 
                typing_error e#loc "pointer expected"
        end
    | JCNEbinary _ ->
        assert false
    | JCNEderef(ls, f) -> 
        let t,tr,tls = location_set env ls in
        let fi = find_field e#loc t f false in
        let fr = Region.make_field tr fi in
        fi.jc_field_info_type, fr, JCLSderef(tls, get_label e, fi, fr)   
    | JCNErange _ | JCNEtagequality _| JCNEmutable _ | JCNEat _ | JCNEold _
    | JCNEquantifier _ | JCNEmatch _ | JCNEunpack _ | JCNEpack _ | JCNEthrow _
    | JCNEtry _ |JCNEreturn _ | JCNEloop _ |JCNEblock _ | JCNEassert _
    | JCNElet _ |JCNEfree _ | JCNEalloc _ | JCNEoffset _| JCNEif _ | JCNEcast _
    | JCNEinstanceof _ | JCNEassign _ | JCNEapp _ | JCNEunary _
    | JCNEconst _ ->
        assert false

let rec location env e =
  match e#node with
    | JCNElabel(l, e) ->
        assert false (* TODO *)
    | JCNEvar id ->
        let vi =
          try List.assoc id env with Not_found ->
            try Hashtbl.find variables_env id with Not_found ->
              typing_error e#loc "unbound identifier %s" id
        in
        vi.jc_var_info_type, vi.jc_var_info_region, JCLvar vi
    | JCNEderef(ls, f) ->
        let t, tr, tls = location_set env ls in
        let fi = find_field e#loc t f false in
        let fr = Region.make_field tr fi in
        fi.jc_field_info_type, fr, JCLderef(tls, get_label e, fi, fr)
    | JCNEat(e, lab) ->
        let t, tr, tl = location env e in
        t, tr, JCLat(tl, lab)
    | JCNErange _ | JCNEtagequality _ | JCNEmutable _ | JCNEold _
    | JCNEquantifier _ | JCNEmatch _ | JCNEunpack _ | JCNEpack _ | JCNEthrow _
    | JCNEtry _ | JCNEreturn _ | JCNEloop _ | JCNEblock _ | JCNEassert _
    | JCNElet _ | JCNEfree _ | JCNEalloc _ | JCNEoffset _ | JCNEif _ | JCNEcast _
    | JCNEinstanceof _ | JCNEassign _ | JCNEapp _ | JCNEunary _ | JCNEbinary _
    | JCNEconst _ ->
        typing_error e#loc "invalid memory location"

let default_label l =
  match l with
    | [l] -> Some l
    | _ -> None

(** Apply [type_logic_labels] in all expressions of a normalized clause,
with the correct label environment. *)
let type_logic_labels_in_clause = function
  | JCCrequires e ->
      type_logic_labels [LabelHere] (Some LabelHere) e
  | JCCbehavior(_, _, _, assumes, requires, assigns, ensures) ->
      Option_misc.iter (type_logic_labels [LabelHere] (Some LabelHere)) assumes;
      Option_misc.iter (type_logic_labels [LabelHere] (Some LabelHere)) requires;
      Option_misc.iter
        (fun (_, x) ->
           List.iter
             (type_logic_labels [LabelOld; LabelHere] (Some LabelHere)) x)
        assigns;
      (type_logic_labels [LabelOld; LabelHere] (Some LabelHere)) ensures

(** Apply [type_logic_labels] in all expressions of a normalized declaration,
with the correct label environment. *)
let type_logic_labels_in_decl d = match d#node with
  | JCDvar(_, _, init) ->
      Option_misc.iter (type_logic_labels [] None) init
  | JCDfun(_, _, _, clauses, body) ->
      Option_misc.iter
        (type_logic_labels [LabelHere; LabelPre] (Some LabelHere))
        body;
      List.iter type_logic_labels_in_clause clauses
  | JCDtag(_, _, _, invs) ->
      List.iter
        (fun (_, _, e) -> type_logic_labels [LabelHere] (Some LabelHere) e) invs
  | JCDlemma(_, _, labels, body) ->
      type_logic_labels labels (default_label labels) body
  | JCDlogic(_, _, labels, _, JCreads el) ->
      List.iter (type_logic_labels labels (default_label labels)) el
  | JCDlogic(_, _, labels, _, JCexpr e) ->
      type_logic_labels labels (default_label labels) e
  | JCDglobal_inv(_, body) ->
      type_logic_labels [LabelHere] (Some LabelHere) body
  | JCDvariant_type _ | JCDunion_type _ | JCDenum_type _ | JCDlogic_type _
  | JCDexception _ | JCDinvariant_policy _ | JCDseparation_policy _
  | JCDannotation_policy _ | JCDabstract_domain _ | JCDint_model _ 
  | JCDlogic_var _ ->
      ()


(* <====== A partir d'ici, c'est pas encore fait *)





let clause env vi_result c acc =
  match c with
    | JCCrequires e ->
        { acc with 
          jc_fun_requires = 
            make_and (assertion env e) acc.jc_fun_requires; }
    | JCCbehavior(loc, id, throws, assumes, requires, assigns, ensures) ->
        let throws,env_result = 
          match throws with
            | None -> None, (vi_result.jc_var_info_name,vi_result)::env 
            | Some id -> 
                try 
                  let ei = 
                    Hashtbl.find exceptions_table id#name 
                  in
                  let tei = match ei.jc_exception_info_type with
                    | Some tei -> tei
                    | None -> typing_error id#loc
                        "exception without value"
                  in
                  let vi = var tei "\\result" in
                  vi.jc_var_info_final_name <- "result";
                  Some ei, (vi.jc_var_info_name,vi)::env 
                with Not_found ->
                  typing_error id#loc 
                    "undeclared exception %s" id#name
        in
        let assumes = Option_misc.map (assertion env) assumes in
(*
        let requires = Option_misc.map (assertion (Some "Here") env) requires in
*)
        let assigns = 
          Option_misc.map 
            (fun (loc, l) -> 
              (loc, List.map 
                 (fun a -> let _,_,tl = location env a in 
                    (match tl with
                      | JCLvar vi -> vi.jc_var_info_assigned <- true
                      | _ -> ());
                    tl) 
                 l)) 
            assigns 
        in
        let b = {
          jc_behavior_throws = throws;
          jc_behavior_assumes = assumes;
(*
          jc_behavior_requires = requires;
*)
          jc_behavior_assigns = assigns;
          jc_behavior_ensures = assertion env_result ensures }
        in
(*
        eprintf "lab,loc for ensures: \"%s\", %a@."
          b.jc_behavior_ensures.jc_assertion_label
          Loc.gen_report_position b.jc_behavior_ensures.jc_assertion_loc;
*)
        { acc with jc_fun_behavior = (loc,id,b)::acc.jc_fun_behavior }
          

  
let param (t,id) =
  let ty = type_type t in
  let vi = var ~formal:true ty id in 
  (id,vi)

let assertion_true = new assertion JCAtrue

let field st root (rep, t, id) =
  let ty = type_type t in
  incr field_tag_counter;
  let name = st.jc_struct_info_name ^ "_" ^ id in
  let fi = {
    jc_field_info_tag = !field_tag_counter;
    jc_field_info_name = id ;
    jc_field_info_final_name = Jc_envset.get_unique_name name;
    jc_field_info_type = ty;
    jc_field_info_root = root;
    jc_field_info_struct = st;
    jc_field_info_rep = rep or (not (is_pointer_type ty));
  } in
  fi

let axioms_table = Hashtbl.create 17
let global_invariants_table = Hashtbl.create 17

(*let add_typedecl d (id, parent) =
  let root,par = 
    match parent with
      | None -> 
          (None, None)
      | Some p ->
          let st = find_struct_info d.jc_pdecl_loc p in
          (Some st.jc_struct_info_root, Some st)
  in
  let struct_info, root =
    try
      let struct_info,_ = Hashtbl.find structs_table id in
      let root = match root with
        | Some x -> x
        | None -> struct_info
      in
      struct_info.jc_struct_info_root <- root;
      struct_info.jc_struct_info_parent <- par;
      struct_info, root
    with Not_found ->
      assert false (* cannot happen, thanks to the function decl_declare *)
(*      let rec struct_info =
        { jc_struct_info_name = id;
          jc_struct_info_fields = [];
          jc_struct_info_parent = par;
          jc_struct_info_root = struct_info;
          jc_struct_info_variant = None;
        }
      in
      (* adding structure name in global environment before typing 
         the fields, because of possible recursive definition *)
      Hashtbl.replace structs_table id (struct_info,[]);
      struct_info, struct_info*)
  in
  root, struct_info*)

let add_vardecl (ty,id) =
  let ty = type_type ty in
  let vi = var ~static:true ty id in
  Hashtbl.add variables_env id vi

let get_vardecl id =
  Hashtbl.find variables_env id

let add_fundecl (ty,loc,id,pl) =
  try
    let fi = Hashtbl.find functions_env id in
    Format.eprintf 
      "FIXME: Warning: ignoring second declaration of function %s@." id;
    let param_env =
      List.map (fun v -> v.jc_var_info_name, v) fi.jc_fun_info_parameters
    in
    param_env, fi
  with Not_found ->
    let param_env = List.map param pl in
    let ty = type_type ty in
    let fi = make_fun_info id ty in
      fi.jc_fun_info_parameters <- List.map snd param_env;
      Hashtbl.replace functions_env id fi;
      param_env, fi

let get_fundecl id =
  let fi = Hashtbl.find functions_env id in
  let param_env =
    List.map (fun v -> v.jc_var_info_name, v) fi.jc_fun_info_parameters
  in
  param_env, fi

let add_logic_fundecl (ty,id,labels,pl) =
  try
    let pi = Hashtbl.find logic_functions_env id in
    let ty = pi.jc_logic_info_result_type in
    let param_env =
      List.map (fun v -> v.jc_var_info_name, v) pi.jc_logic_info_parameters
    in
    param_env, ty, pi
  with Not_found ->
    let param_env = List.map param pl in
    let ty = Option_misc.map type_type ty in
    let pi = make_rel id in
    pi.jc_logic_info_parameters <- List.map snd param_env;
    pi.jc_logic_info_result_type <- ty;
    pi.jc_logic_info_labels <- labels;
    Hashtbl.replace logic_functions_env id pi;
    param_env, ty, pi

let add_logic_constdecl (ty, id) =
  try
    let vi = Hashtbl.find logic_constants_env id in
      vi.jc_var_info_type, vi 
  with Not_found ->
    let ty = type_type ty in
    let vi = var ~static:true ty id in
      Hashtbl.add logic_constants_env id vi;
      ty, vi
        
let type_range_of_term ty t =
  match ty with
    | JCTpointer (JCtag st, n1opt, n2opt) ->
(*      let instanceofcstr = new assertion (JCAinstanceof (t, st)) in *)
(*      let mincstr = match n1opt with
          | None -> true_assertion
          | Some n1 ->
              let mint = 
                term_no_loc (JCToffset (Offset_min, t, st)) integer_type in
              let n1t =
                term_no_loc (JCTconst (JCCinteger (Num.string_of_num n1))) 
                  integer_type
              in
              new assertion (JCArelation (mint, Beq_int, n1t))
        in *)
        let maxcstr = match n2opt with
          | None -> true_assertion
          | Some n2 ->
              let maxt = 
                new term
                  ~loc: t#loc
                  ~typ: integer_type
                  (JCToffset (Offset_max, t, st))
              in
              let n2t =
                new term
                  ~loc: t#loc
                  ~typ: integer_type
                  (JCTconst (JCCinteger (Num.string_of_num n2)))
              in
              new assertion (JCArelation (maxt, (`Beq, `Integer), n2t))
        in
          maxcstr
(*        if is_root_struct st then *)
(*        Jc_pervasives.make_and [mincstr; maxcstr] *)
(*        else
          Jc_pervasives.make_and [instanceofcstr; mincstr; maxcstr] *)
    | JCTpointer (JCvariant vi, _, _) ->
        assert false (* TODO, but need to change JCToffset before *)
    | _ -> true_assertion

(* First pass: declare everything with no typing
 * (use dummy values that will be replaced by "decl")
 * (declare identifiers so that previous definitions can (possibly recursively)
 * use them) *)
(*let rec decl_declare d =
  match d.jc_pdecl_node with
    | JCPDtag(id, parent, fields, inv) ->
        (* declare structure name *)
        let rec struct_info = {
          jc_struct_info_name = id;
          jc_struct_info_fields = [];
          jc_struct_info_parent = None;
          jc_struct_info_root = struct_info;
          jc_struct_info_variant = None;
        } in
        Hashtbl.add structs_table id (struct_info, []);
        (* declare mutable field (if needed) *)
        if parent = None && !Jc_common_options.inv_sem = InvOwnership then
          create_mutable_field struct_info;
        (* TODO: declare fields *)
        (* TODO: declare invariants *)
        Hashtbl.replace structs_table id (struct_info, [])
    | JCPDvarianttype(id, _) ->
        Hashtbl.replace variants_table id {
          jc_variant_info_name = id;
          jc_variant_info_roots = [];
        }
    | JCPDvar _
    | JCPDfun _
    | JCPDrecfuns _
    | JCPDenumtype _
    | JCPDlogictype _
    | JCPDaxiom _
    | JCPDexception _
    | JCPDlogic _
    | JCPDglobinv _ ->
        () (* TODO *)
*)

let rec decl d =
  match d#node with
    | JCDvar (ty, id, init) ->
        let e = Option_misc.map (expr []) init in
        let vi = get_vardecl id in
        Hashtbl.add variables_table vi.jc_var_info_tag (vi, e)
    | JCDfun (ty, id, pl, specs, body) -> 
        let loc = id#loc in
        let param_env, fi = get_fundecl id#name in
        let vi = fi.jc_fun_info_result in
        let s = List.fold_right 
                  (clause param_env vi) specs 
                  { jc_fun_requires = assertion_true;
                    jc_fun_free_requires = assertion_true;
                    jc_fun_behavior = [] }
        in
        let b = Option_misc.map 
	  (unit_expr $ expr (("\\result",vi)::param_env)) body 
	in
        Hashtbl.add functions_table fi.jc_fun_info_tag (fi,loc,s,b)
    | JCDenum_type(id,min,max) ->
        let ri =
          { jc_enum_info_name = id;
            jc_enum_info_min = min;
            jc_enum_info_max = max;
          }
        in
(*
        let to_int = make_logic_fun ("integer_of_"^id) integer_type in
        let to_int_ = make_fun_info ("integer_of_"^id) integer_type in
        let of_int = make_fun_info (id^"_of_integer") (JCTenum ri) in
*)
        Hashtbl.add enum_types_table id (ri (*,to_int,to_int_,of_int*));
(*
        Hashtbl.add enum_conversion_logic_functions_table to_int id;
        Hashtbl.add enum_conversion_functions_table to_int_ id;
        Hashtbl.add enum_conversion_functions_table of_int id
*)

    | JCDtag(id, parent, fields, inv) ->
        let struct_info, _ = Hashtbl.find structs_table id in
        let root = struct_info.jc_struct_info_root in
        (* fields *)
        let env = List.map (field struct_info root) fields in
        struct_info.jc_struct_info_fields <- env;
        (* declare invariants as logical functions *)
        let invariants =
          List.fold_left
            (fun acc (id, x, e) ->
               if !Jc_common_options.inv_sem = InvNone then
                 typing_error id#loc
                   "use of structure invariants requires declaration \
of an invariant policy";
               let vi =
                 var (JCTpointer (JCtag struct_info, Some zero, Some zero)) x in
               let p = assertion [(x, vi)] e in
               let pi = make_rel id#name in
               pi.jc_logic_info_parameters <- [vi];
               pi.jc_logic_info_labels <- [LabelHere];
               eprintf "generating logic fun %s with one default label@."
                 pi.jc_logic_info_name;
               Hashtbl.replace logic_functions_table 
                 pi.jc_logic_info_tag (pi, JCAssertion p);
               Hashtbl.replace logic_functions_env id#name pi;
               (pi, p) :: acc)
            []
            inv
        in 
        Hashtbl.replace structs_table id (struct_info, invariants)

    | JCDvariant_type(id, tags) -> ()
    | JCDunion_type(id, tags) -> ()

(*    | JCDrectypes(pdecls) ->
        (* first pass: adding structure names *)
        List.iter (fun d -> match d.jc_pdecl_node with
                     | JCDstructtype(id,_,_,_) ->
                         (* parent type may not be declared yet *)
                         ignore (add_typedecl d (id,None))
                     | _ -> assert false
                  ) pdecls;
        (* second pass: adding structure fields *)
        List.iter (fun d -> match d.jc_pdecl_node with
                     | JCDstructtype(id,parent,fields,_) ->
                         let root,struct_info = add_typedecl d (id,parent) in
                         let env = List.map (field struct_info root) fields in
                         struct_info.jc_struct_info_fields <- env;
                         Hashtbl.replace structs_table id (struct_info,[])
                     | _ -> assert false
                  ) pdecls;
        (* third pass: typing invariants *)
        List.iter decl pdecls*)

    | JCDlogic_type(id) ->
        begin 
          try
            let _ = Hashtbl.find logic_type_table id in
            assert false
          with Not_found ->
            Hashtbl.add logic_type_table id id
        end
    | JCDlemma(id,is_axiom,labels,e) ->
        let te = assertion [] e in
        Hashtbl.add axioms_table id (is_axiom,labels,te)
    | JCDglobal_inv(id, e) ->
        let a = assertion [] e in
        let li = make_rel id in
          if !Jc_common_options.inv_sem = InvArguments then 
            Hashtbl.replace logic_functions_table 
              li.jc_logic_info_tag (li, JCAssertion a);
          Hashtbl.add global_invariants_table li a
    | JCDexception(id,tyopt) ->
        let tt = Option_misc.map type_type tyopt in
        Hashtbl.add exceptions_table id (exception_info tt id)
    | JCDlogic_var (ty, id, body) ->
        let ty, vi = add_logic_constdecl (ty, id) in
        let t = Option_misc.map 
	  (function body ->
	     let t = term [] body in
             if not (subtype t#typ ty) then
               typing_error d#loc
                 "inferred type differs from declared type"
             else (t,mintype t#loc t#typ ty)
	  ) body
        in
        Hashtbl.add logic_constants_table vi.jc_var_info_tag (vi, t)
    | JCDlogic(None, id, labels, pl, body) ->
        let param_env,ty,pi = add_logic_fundecl (None,id,labels,pl) in
        let p = match body with
          | JCreads reads ->
              JCReads (
                (List.map 
                   (fun a -> 
                      let _,_,tl = 
                        location param_env a 
                      in tl)) reads)
          | JCexpr body ->
              JCAssertion(assertion param_env body)
        in
        Hashtbl.add logic_functions_table pi.jc_logic_info_tag (pi, p)
    | JCDlogic(Some ty, id, labels, pl, body) ->
        let param_env,ty,pi = add_logic_fundecl (Some ty,id,labels,pl) in
        let ty = match ty with Some ty -> ty | None -> assert false in
        let t = match body with
          | JCreads reads ->
              JCReads (
                (List.map 
                   (fun a -> 
                      let _,_,tl = location param_env a 
                      in tl)) reads)
          | JCexpr body ->
              let t = term param_env body in
              if not (subtype_strict t#typ ty) then 
                typing_error d#loc
                  "inferred type differs from declared type" 
              else JCTerm t
        in
        Hashtbl.add logic_functions_table pi.jc_logic_info_tag (pi, t)
    | JCDint_model _|JCDabstract_domain _|JCDannotation_policy _
    | JCDseparation_policy _|JCDinvariant_policy _ ->
        assert false

let declare_struct_info d = match d#node with
  | JCDtag(id, parent, _, _) ->
      let rec si = {
        jc_struct_info_name = id;
        jc_struct_info_fields = [];
        jc_struct_info_parent = None;
        jc_struct_info_root = si;
        jc_struct_info_variant = None;
      } in
      Hashtbl.add structs_table id (si, []);
      (* declare the "mutable" field (if needed) *)
      if parent = None && !Jc_common_options.inv_sem = InvOwnership then
        create_mutable_field si
  | _ -> ()

let declare_function d = match d#node with
  | JCDfun(ty,id,pl,_specs,_body) ->
      ignore 
        (add_fundecl (ty,id#loc,id#name,pl))
  | JCDlogic(Some ty,id,labels,[],_body) ->
      ignore (add_logic_constdecl (ty,id))
  | JCDlogic(ty,id,labels,pl,_body) ->
      ignore (add_logic_fundecl (ty,id,labels,pl))
  | _ -> ()

let declare_variable d = match d#node with
  | JCDvar(ty,id,_init) ->
      add_vardecl (ty,id)
  | _ -> ()

let compute_struct_info_parent d = match d#node with
  | JCDtag(id, Some parent, _, _) ->
      let si, _ = Hashtbl.find structs_table id in
      let psi = find_struct_info d#loc parent in
      si.jc_struct_info_parent <- Some psi
  | _ -> ()

let fixpoint_struct_info_roots () =
  let modified = ref false in
  Hashtbl.iter
    (fun _ (si, _) ->
       match si.jc_struct_info_parent with
         | Some psi ->
             if si.jc_struct_info_root != psi.jc_struct_info_root then begin
               si.jc_struct_info_root <- psi.jc_struct_info_root;
               modified := true
             end
         | None -> ())
    structs_table;
  !modified

let type_variant d = match d#node with
  | JCDvariant_type(id, tags) | JCDunion_type(id, tags) ->
      (* declare the variant *)
      let vi = {
        jc_variant_info_name = id;
        jc_variant_info_roots = [];
        jc_variant_info_is_union = 
          match d#node with JCDvariant_type _ -> false | _ -> true;
      } in
      Hashtbl.add variants_table id vi;
      (* tags *)
      let roots = List.map
        (fun tag ->
           (* find the structure *)
           let st, _ = try
             Hashtbl.find structs_table tag#name
           with Not_found ->
             typing_error tag#loc
               "undefined tag: %s" tag#name
           in
           (* the structure must be root *)
           if st.jc_struct_info_parent <> None then
             typing_error tag#loc
               "the tag %s is not root" tag#name;
           (* the structure must not be used by another variant *)
           match st.jc_struct_info_variant with
             | None ->
                 (* update the structure variant and return the root *)
                 st.jc_struct_info_variant <- Some vi;
                 st
             | Some prev -> typing_error tag#loc
                 "tag %s is already used by type %s" tag#name
                   prev.jc_variant_info_name)
        tags
      in
      (* update the variant *)
      vi.jc_variant_info_roots <- roots
  | _ -> ()

let check_struct d = match d#node with
  | JCDtag(id, _, _, _) ->
      let loc = d#loc in
      let st = find_struct_info loc id in
      if st.jc_struct_info_root.jc_struct_info_variant = None then
        typing_error loc "the tag %s is not used by any type"
          st.jc_struct_info_name
  | _ -> ()

(* type declarations in the right order *)
let type_file ast =
  (* 1. logic types *)
  let is_logic_type d = 
    match d#node with JCDlogic_type _ -> true | _ -> false
  in
  let logic_types,ast = List.partition is_logic_type ast in
  List.iter decl logic_types;
  (* 2. enumerations *)
  let is_enum d = 
    match d#node with JCDenum_type _ -> true | _ -> false
  in
  let enums,ast = List.partition is_enum ast in
  List.iter decl enums;
  (* 3. records and variants *)
  List.iter declare_struct_info ast;
  List.iter compute_struct_info_parent ast;
  while fixpoint_struct_info_roots () do () done;
  List.iter type_variant ast;
  List.iter check_struct ast;
  (* 4. declaring global variables *)
  List.iter declare_variable ast;
  (* 5. declaring coding and logic functions *)
  List.iter declare_function ast;
  (* 6. remaining declarations *)
  List.iter decl ast

let print_file fmt () =
  let functions =
    Hashtbl.fold
      (fun _ (finfo,_,fspec,slist) f ->
         Jc_output.JCfun_def
           (finfo.jc_fun_info_result.jc_var_info_type,finfo.jc_fun_info_name,
            finfo.jc_fun_info_parameters,fspec,slist)
         :: f
      ) functions_table []
  in
  let logic_functions =
    Hashtbl.fold
      (fun _ (linfo,tora) f ->
         Jc_output.JClogic_fun_def
           (linfo.jc_logic_info_result_type,linfo.jc_logic_info_name,
            linfo.jc_logic_info_labels,
            linfo.jc_logic_info_parameters, tora)
         :: f
      ) logic_functions_table []
  in
  let logic_constants =
    Hashtbl.fold
      (fun _ (vi,t) f ->
         Jc_output.JClogic_const_def
           (vi.jc_var_info_type, vi.jc_var_info_name, Option_misc.map fst t)
        :: f
      ) logic_constants_table []
  in
  let logic_types =
    Hashtbl.fold
      (fun _ (s) f ->
        Jc_output.JClogic_type_def s
        :: f
      ) logic_type_table []
  in
  let variables =
    Hashtbl.fold
      (fun _ (vinfo,vinit) f ->
         Jc_output.JCvar_def
           (vinfo.jc_var_info_type,vinfo.jc_var_info_name,vinit)
         :: f
      ) variables_table []
  in
  let structs =
    Hashtbl.fold
      (fun name (sinfo,_) f ->
         let super = match sinfo.jc_struct_info_parent with
           | None -> None
           | Some st -> Some st.jc_struct_info_name
         in
         Jc_output.JCstruct_def
           (name,super,sinfo.jc_struct_info_fields,[])
         :: f
      ) structs_table []
  in
  let variants =
    Hashtbl.fold
      (fun name vinfo f ->
        let tags =
          List.map (fun sinfo -> sinfo.jc_struct_info_name)
            vinfo.jc_variant_info_roots
        in
        Jc_output.JCvariant_type_def (name,tags)
        :: f
      ) variants_table []
  in
  let enums =
    Hashtbl.fold
      (fun name rinfo f ->
         Jc_output.JCenum_type_def
           (name,rinfo.jc_enum_info_min,rinfo.jc_enum_info_max)
         :: f
      ) enum_types_table []
  in
  let axioms =
    Hashtbl.fold
      (fun name (is_axiom,labels, a) f ->
         Jc_output.JClemma_def (name,is_axiom, labels,a)
         :: f
      ) axioms_table []
  in
  let global_invariants =
    Hashtbl.fold
      (fun li a f ->
         Jc_output.JCglobinv_def (li.jc_logic_info_name,a)
         :: f
      ) global_invariants_table []
  in
  let exceptions =
    Hashtbl.fold
      (fun name ei f ->
         Jc_output.JCexception_def (name,ei)
         :: f
      ) exceptions_table []
  in
  (* make all structured types mutually recursive.
     make all functions mutually recursive.
     make all logic functions and constants mutually recursive.
  *)
  let tfile =
    (List.rev enums)
    @ (List.rev structs)
    @ (List.rev variants)
    @ (List.rev exceptions)
    @ (List.rev variables)
    @ (List.rev logic_types)
    @ (Jc_output.JCrec_fun_defs
      (List.rev logic_constants @ (List.rev logic_functions)))
    :: (List.rev axioms)
    @ (List.rev global_invariants)
    @ [Jc_output.JCrec_fun_defs (List.rev functions)]
  in
  Jc_output.print_decls fmt tfile;

(*
Local Variables: 
compile-command: "LC_ALL=C make -j -C .. bin/jessie.byte"
End: 
*)

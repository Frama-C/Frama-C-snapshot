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

(* $Id: jc_pervasives.ml,v 1.110 2008/07/03 14:34:11 marche Exp $ *)

open Format
open Jc_env
open Jc_envset
open Jc_fenv
open Jc_constructors
open Jc_ast
open Jc_region

let ( $ ) = fun f g x -> f(g x)

exception Error of Loc.position * string

let error l = 
  Format.kfprintf 
    (fun fmt -> raise (Error(l, flush_str_formatter()))) 
    str_formatter

let operator_of_native = function
  | Tunit -> `Unit
  | Tboolean -> `Boolean
  | Tinteger -> `Integer
  | Treal -> `Real
  | Tstring -> assert false

let operator_of_type = function
  | JCTnative n -> operator_of_native n
  | JCTenum _ -> `Integer
  | JCTlogic _ -> `Logic
  | JCTany | JCTtype_var _ -> assert false (* TODO? *)
  | JCTnull | JCTpointer _ -> `Pointer

let label_var ?(label_in_name=true) ?label_assoc lab name =
  let label =
    match label_assoc with
      | None -> lab 
      | Some a ->
	  try List.assoc lab a
	  with Not_found -> lab
  in			
  if label_in_name then 
    match label with
      | LabelHere -> name
      | LabelPre -> name ^ "_at_Pre"
      | LabelOld -> assert false (* name ^ "_at_Old" *)
      | LabelPost -> name ^ "_at_Post"
      | LabelName l -> name ^ "_at_" ^ l.label_info_final_name
  else
    match label with (* hack ?? *)
      | LabelHere -> name
      | LabelPost -> name
      | LabelPre -> name ^ "@init"
      | LabelOld -> name ^ "@"
      | LabelName l -> name ^ "@" ^ l.label_info_final_name

let new_label_name =
  let label_name_counter = ref 0 in function () ->
    incr label_name_counter;
    "JC_" ^ string_of_int !label_name_counter

let root_name st =
  st.jc_struct_info_root.jc_struct_info_name

let field_root_name fi =
  fi.jc_field_info_root.jc_struct_info_name

let string_of_native t =
  match t with
    | Tunit -> "unit"
    | Tinteger -> "integer"
    | Treal -> "real"
    | Tboolean -> "boolean"
    | Tstring -> "string"

let rec print_type fmt t =
  match t with
    | JCTnative n -> fprintf fmt "%s" (string_of_native n)
    | JCTlogic s -> fprintf fmt "%s" s
    | JCTenum ri -> fprintf fmt "%s" ri.jc_enum_info_name
    | JCTpointer(tov, ao, bo) ->
        begin match tov with
          | JCtag({ jc_struct_info_name = name }, [])
          | JCvariant { jc_variant_info_name = name }
	  | JCunion { jc_variant_info_name = name } ->
              fprintf fmt "%s" name
          | JCtag({ jc_struct_info_name = name }, params) ->
              fprintf fmt "%s<%a>" name
                (Pp.print_list Pp.comma print_type) params
        end;
	begin match ao, bo with
	  | None, None ->
	      fprintf fmt "[..]"
	  | Some a, None ->
	      fprintf fmt "[%s..]" (Num.string_of_num a)
	  | None, Some b ->
	      fprintf fmt "[..%s]" (Num.string_of_num b)
	  | Some a, Some b ->
	      if Num.eq_num a b then
		fprintf fmt "[%s]" (Num.string_of_num a)
	      else
		fprintf fmt "[%s..%s]"
		  (Num.string_of_num a) (Num.string_of_num b)
	end
    | JCTnull -> fprintf fmt "(nulltype)"  
    | JCTany -> fprintf fmt "(anytype)"  
    | JCTtype_var v -> fprintf fmt "(var%d)" (Jc_type_var.uid v)

let num_of_constant loc c =
    match c with
      | JCCinteger n -> 
	  (try Num.num_of_string n
	   with _ -> assert false)
      | _ -> invalid_arg ""
	  
let zero = Num.num_of_int 0
let minus_one = Num.num_of_int (-1)

let rec location_set_region = function
  | JCLSvar vi -> vi.jc_var_info_region
  | JCLSderef(_,_,_,r) -> r
  | JCLSrange(ls,_,_) -> location_set_region ls

type tlocation =
  | JCLvar of var_info
  | JCLderef of tlocation_set * field_info * region

(* operators *)

let is_relation_binary_op = function
  | `Blt | `Bgt | `Ble | `Bge | `Beq | `Bneq -> true
  | _ -> false

let is_logical_binary_op = function
  | `Bland | `Blor | `Bimplies | `Biff -> true
  | _ -> false

let is_arithmetic_binary_op = function
  | `Badd | `Bsub | `Bmul | `Bdiv | `Bmod -> true
  | _ -> false

let is_bitwise_binary_op = function
  | `Bbw_and | `Bbw_or | `Bbw_xor 
  | `Bshift_left | `Blogical_shift_right | `Barith_shift_right -> true
  | _ -> false

let is_logical_unary_op = function
  | `Unot -> true
  | _ -> false

let is_arithmetic_unary_op = function
  | `Uminus -> true
  | _ -> false

let is_bitwise_unary_op = function
  | `Ubw_not -> true
  | _ -> false

(* native types *)

let unit_type = JCTnative Tunit
let boolean_type = JCTnative Tboolean
let integer_type = JCTnative Tinteger
let real_type = JCTnative Treal
let null_type = JCTnull
let string_type = JCTnative Tstring
let any_type = JCTany

(* temporary variables *)

let tempvar_count = ref 0
(* let reset_tmp_var () = tempvar_count := 0 *)
let tmp_var_name () = 
  incr tempvar_count; "jessie_" ^ string_of_int !tempvar_count

(* constants *)

let const c =
  match c with
    | JCCvoid -> unit_type,dummy_region,c
    | JCCinteger _ -> integer_type,dummy_region,c
    | JCCreal _ -> real_type,dummy_region,c
    | JCCboolean _ -> boolean_type, dummy_region, c
    | JCCnull -> null_type,Region.make_var JCTnull "null",c
    | JCCstring _ -> string_type,dummy_region,c

(* variables *)

let var_tag_counter = ref 0

let var ?(unique=true) ?(static=false) ?(formal=false) ty id =
  incr var_tag_counter;
  let vi = {
    jc_var_info_tag = !var_tag_counter;
    jc_var_info_name = id;
    jc_var_info_final_name = 
      if unique then Jc_envset.get_unique_name id else id;
    jc_var_info_region = 
      if static then Region.make_const ty id else Region.make_var ty id;
    jc_var_info_type = ty;
    jc_var_info_formal = formal;
    jc_var_info_assigned = false;
    jc_var_info_static = static;
  }
  in vi

let newvar ty = var ty (tmp_var_name())

let newrefvar ty = 
  let vi = newvar ty in
    vi.jc_var_info_assigned <- true;
    vi

let copyvar vi =
  incr var_tag_counter;
  { vi with 
    jc_var_info_tag = !var_tag_counter; 
    jc_var_info_name = 
      "__jc_" ^ (string_of_int !var_tag_counter) ^ vi.jc_var_info_name;
    jc_var_info_final_name = 
      "__jc_" ^ (string_of_int !var_tag_counter) ^ vi.jc_var_info_final_name;
  }

(* exceptions *)

let exception_tag_counter = ref 0

let exception_info ty id =
  incr exception_tag_counter;
  let ei = {
    jc_exception_info_tag = !exception_tag_counter;
    jc_exception_info_name = id;
    jc_exception_info_type = ty;
  }
  in ei


(* logic functions *)

let empty_effects = 
  { jc_effect_alloc_table = StringRegionSet.empty;
    jc_effect_tag_table = VariantMap.empty;
    jc_effect_memories = FieldOrVariantRegionMap.empty;
    jc_effect_globals = VarSet.empty;
    jc_effect_mutable = StringSet.empty;
    jc_effect_committed = StringSet.empty;
  }

let empty_logic_info =
  {
    jc_logic_info_tag = 0;
    jc_logic_info_name = "";
    jc_logic_info_final_name = "";
    jc_logic_info_result_type = None;
    jc_logic_info_result_region = dummy_region; (* TODO *)
    jc_logic_info_parameters = [];
    jc_logic_info_param_regions = [];
    jc_logic_info_effects = empty_effects;
    jc_logic_info_calls = []; 
    jc_logic_info_labels = [];
  }

let logic_fun_tag_counter = ref 0

let make_logic_fun name ty =
  incr logic_fun_tag_counter;
  { jc_logic_info_tag = !logic_fun_tag_counter;
    jc_logic_info_name = name;
    jc_logic_info_final_name = Jc_envset.get_unique_name name;
    jc_logic_info_result_type = Some ty;
    jc_logic_info_result_region = Region.make_var ty name;
    jc_logic_info_parameters = [];
    jc_logic_info_param_regions = [];
    jc_logic_info_effects = empty_effects;
    jc_logic_info_calls = [];
    jc_logic_info_labels = [];
  }

let real_of_integer = make_logic_fun "real_of_int" real_type
let any_string = make_logic_fun "any_string" string_type

let () = 
  let vi = var ~formal:true integer_type "n" in
  real_of_integer.jc_logic_info_parameters <- [vi]

let full_separated = make_logic_fun "full_separated" null_type

(* logic predicates *)

let make_rel name =
  incr logic_fun_tag_counter;
  { jc_logic_info_tag = !logic_fun_tag_counter;
    jc_logic_info_name = name;
    jc_logic_info_final_name = Jc_envset.get_unique_name name;
    jc_logic_info_result_type = None;
    jc_logic_info_result_region = dummy_region;
    jc_logic_info_parameters = [];
    jc_logic_info_param_regions = [];
    jc_logic_info_effects = empty_effects;
    jc_logic_info_calls = [];
    jc_logic_info_labels = [];
  }

    
(* programs *)

let empty_fun_effect =
  { jc_reads = empty_effects;
    jc_writes = empty_effects;
    jc_raises = ExceptionSet.empty ;
  }

let fun_tag_counter = ref 0

let make_fun_info name ty =
  incr fun_tag_counter;
  let vi = var ty "\\result" in
  vi.jc_var_info_final_name <- "result";
  { jc_fun_info_tag = !fun_tag_counter;
    jc_fun_info_name = name;
    jc_fun_info_final_name = Jc_envset.get_unique_name name;
    jc_fun_info_parameters = [];
    jc_fun_info_result = vi;
    jc_fun_info_return_region = Region.make_var ty name;
    jc_fun_info_param_regions = [];
    jc_fun_info_calls = [];
    jc_fun_info_is_recursive = false;
    jc_fun_info_logic_apps = [];
    jc_fun_info_effects = empty_fun_effect;
  }


let real_of_integer_ = make_fun_info "real_of_int" real_type

let () = 
  let vi = var ~formal:true integer_type "n" in
  real_of_integer_.jc_fun_info_final_name <- "real_of_int";
  real_of_integer_.jc_fun_info_parameters <- [vi]


let option_compare comp opt1 opt2 = match opt1,opt2 with
  | None,None -> 0
  | None,Some _ -> -1
  | Some _,None -> 1
  | Some x,Some y -> comp x y

let rec list_compare comp ls1 ls2 = match ls1,ls2 with
  | [],[] -> 0
  | [],_ -> -1
  | _,[] -> 1
  | x1::r1,x2::r2 -> 
      let compx = comp x1 x2 in 
      if compx = 0 then list_compare comp r1 r2 else compx



(* terms *)


let rec is_constant_term t =
  match t#node with
    | JCTrange (None, None) (* CORRECT ? *)
    | JCTconst _ -> true
    | JCTvar _ | JCTshift _ | JCTderef _
    | JCTapp _ | JCTold _ | JCTat _ | JCToffset _
    | JCTinstanceof _ | JCTcast _ | JCTrange_cast _
    | JCTreal_cast _ | JCTif _ | JCTmatch _ -> false
    | JCTbinary (t1, _, t2) | JCTrange (Some t1, Some t2) ->
	is_constant_term t1 && is_constant_term t2
    | JCTunary (_, t) | JCTrange (Some t, None) | JCTrange (None, Some t) ->
	is_constant_term t

let term_num t = match t#node with
  | JCTconst _ -> 1
  | JCTvar _ -> 3
  | JCTbinary _ -> 5
  | JCTshift _ -> 7
  | JCTunary _ -> 13
  | JCTderef _ -> 17
  | JCTold _ -> 19
  | JCToffset _ -> 23
  | JCTinstanceof _ -> 31
  | JCTcast _ -> 37
  | JCTrange _ -> 41
  | JCTapp _ -> 43
  | JCTif _ -> 47
  | JCTat _ -> 53
  | JCTmatch _ -> 59
  | JCTrange_cast _ -> 61
  | JCTreal_cast _ -> 67

(* Comparison based only on term structure, not types not locations. *)
let rec raw_term_compare t1 t2 =
  match t1#node, t2#node with
    | JCTconst c1,JCTconst c2 -> 
	Pervasives.compare c1 c2
    | JCTvar v1,JCTvar v2 -> 
	Pervasives.compare v1.jc_var_info_tag v2.jc_var_info_tag
  | JCTbinary(t11,op1,t12),JCTbinary(t21,op2,t22) -> 
      let compop = Pervasives.compare op1 op2 in
      if compop = 0 then 
	let comp1 = raw_term_compare t11 t21 in
	if comp1 = 0 then raw_term_compare t12 t22 else comp1
      else compop
  | JCTshift(t11,t12),JCTshift(t21,t22) ->
      let comp1 = raw_term_compare t11 t21 in
      if comp1 = 0 then raw_term_compare t12 t22 else comp1
  | JCTunary(op1,t11),JCTunary(op2,t21) ->
      let compop = Pervasives.compare op1 op2 in
      if compop = 0 then raw_term_compare t11 t21 else compop
  | JCTold t11,JCTold t21 ->
      raw_term_compare t11 t21
  | JCTderef(t11,_,fi1),JCTderef(t21,_,fi2) ->
      let compfi = 
	Pervasives.compare fi1.jc_field_info_tag fi2.jc_field_info_tag
      in
      if compfi = 0 then raw_term_compare t11 t21 else compfi
  | JCToffset(ok1,t11,st1),JCToffset(ok2,t21,st2) ->
      let compok = Pervasives.compare ok1 ok2 in
      if compok = 0 then
	let compst = 
	  Pervasives.compare st1.jc_struct_info_name st2.jc_struct_info_name
	in
	if compst = 0 then raw_term_compare t11 t21 else compst
      else compok
  | JCTinstanceof(t11,lab1,st1),JCTinstanceof(t21,lab2,st2) 
  | JCTcast(t11,lab1,st1),JCTcast(t21,lab2,st2) ->
      let compst = 
	Pervasives.compare st1.jc_struct_info_name st2.jc_struct_info_name
      in
      if compst <> 0 then compst else
	let compst = 
	  Pervasives.compare lab1 lab2
	in
	if compst <> 0 then compst else
	  raw_term_compare t11 t21 
  | JCTrange(t11opt,t12opt),JCTrange(t21opt,t22opt) ->
      let comp1 = option_compare raw_term_compare t11opt t21opt in
      if comp1 = 0 then 
	option_compare raw_term_compare t12opt t22opt
      else comp1
  | JCTapp app1,JCTapp app2 ->
      let li1 = app1.jc_app_fun and ts1 = app1.jc_app_args in
      let li2 = app2.jc_app_fun and ts2 = app2.jc_app_args in
      let compli = 
	Pervasives.compare li1.jc_logic_info_tag li2.jc_logic_info_tag
      in
      if compli = 0 then
	list_compare raw_term_compare ts1 ts2
      else compli
  | JCTif(t11,t12,t13),JCTif(t21,t22,t23) ->
      let comp1 = raw_term_compare t11 t21 in
      if comp1 = 0 then 
	let comp2 = raw_term_compare t12 t22 in
	if comp2 = 0 then raw_term_compare t13 t23 else comp2
      else comp1
  | _ -> term_num t2 - term_num t1

let raw_term_equal t1 t2 = raw_term_compare t1 t2 = 0

let tag_num tag = match tag#node with
  | JCTtag _ -> 1
  | JCTbottom -> 3
  | JCTtypeof _ -> 5

let raw_tag_compare tag1 tag2 =
  match tag1#node,tag2#node with
    | JCTtag st1,JCTtag st2 ->
        Pervasives.compare st1.jc_struct_info_name st2.jc_struct_info_name
    | JCTbottom,JCTbottom -> 0
    | JCTtypeof(t1,st1),JCTtypeof(t2,st2) ->
        let compst = 
	  Pervasives.compare st1.jc_struct_info_name st2.jc_struct_info_name
        in
        if compst = 0 then raw_term_compare t1 t2 else compst
  | _ -> tag_num tag2 - tag_num tag1

let assertion_num a = match a#node with
  | JCAtrue -> 1
  | JCAfalse -> 3
  | JCArelation _ -> 5
  | JCAand _ -> 7
  | JCAor _ -> 11
  | JCAimplies _ -> 13
  | JCAiff _ -> 17
  | JCAnot _ -> 19
  | JCAapp _ -> 23
  | JCAquantifier _ -> 31
  | JCAold _ -> 37
  | JCAinstanceof  _ -> 41
  | JCAbool_term _ -> 43
  | JCAif _ -> 47
  (* ??? are these supposed to be prime numbers ? *)
  | JCAmutable _ -> 49
  | JCAtagequality _ -> 51
  | JCAat _ -> 53
  | JCAmatch _ -> 59

(* Comparison based only on assertion structure, not locations. *)
let rec raw_assertion_compare a1 a2 =
  match a1#node, a2#node with
    | JCAtrue,JCAtrue | JCAfalse,JCAfalse -> 0
    | JCArelation(t11,op1,t12),JCArelation(t21,op2,t22) ->
        let compop = Pervasives.compare op1 op2 in
        if compop = 0 then 
	  let comp1 = raw_term_compare t11 t21 in
	  if comp1 = 0 then raw_term_compare t12 t22 else comp1
        else compop
    | JCAand als1,JCAand als2 | JCAor als1,JCAor als2 ->
	list_compare raw_assertion_compare als1 als2
    | JCAimplies(a11,a12),JCAimplies(a21,a22) 
    | JCAiff(a11,a12),JCAiff(a21,a22) ->
        let comp1 = raw_assertion_compare a11 a21 in
        if comp1 = 0 then raw_assertion_compare a12 a22 else comp1
    | JCAnot a1,JCAnot a2 | JCAold a1,JCAold a2 ->
        raw_assertion_compare a1 a2
    | JCAapp app1, JCAapp app2 ->
	let li1 = app1.jc_app_fun in
	let li2 = app2.jc_app_fun in
        let compli = 
	  Pervasives.compare li1.jc_logic_info_tag li2.jc_logic_info_tag
        in
        if compli = 0 then
	  let tls1 = app1.jc_app_args in
	  let tls2 = app2.jc_app_args in
  	  list_compare raw_term_compare tls1 tls2
        else compli
    | JCAquantifier(q1,vi1,a1),JCAquantifier(q2,vi2,a2) ->
        let compq = Pervasives.compare q1 q2 in
        if compq = 0 then 
	  let compvi = Pervasives.compare vi1 vi2 in
	  if compvi = 0 then raw_assertion_compare a1 a2 else compvi
        else compq
    | JCAinstanceof(t1,_,st1),JCAinstanceof(t2,_,st2) ->
        let compst = 
	  Pervasives.compare st1.jc_struct_info_name st2.jc_struct_info_name
        in
        if compst = 0 then raw_term_compare t1 t2 else compst
    | JCAbool_term t1,JCAbool_term t2 ->
        raw_term_compare t1 t2
    | JCAif(t1,a11,a12),JCAif(t2,a21,a22) ->
        let comp0 = raw_term_compare t1 t2 in
        if comp0 = 0 then 
	  let comp1 = raw_assertion_compare a11 a21 in
	  if comp1 = 0 then raw_assertion_compare a12 a22 else comp1
        else comp0
    | JCAmutable(t1,st1,tag1),JCAmutable(t2,st2,tag2) ->
        let compst = 
	  Pervasives.compare st1.jc_struct_info_name st2.jc_struct_info_name
        in
        if compst = 0 then
	  let comptag = raw_tag_compare tag1 tag2 in
	  if comptag = 0 then raw_term_compare t1 t2 else comptag
        else compst
    | JCAtagequality(tag11,tag12,so1),JCAtagequality(tag21,tag22,so2) ->
        let compso = option_compare Pervasives.compare so1 so2 in
        if compso = 0 then
	  let comptag = raw_tag_compare tag11 tag21 in
	  if comptag = 0 then raw_tag_compare tag12 tag22 else comptag
        else compso
    | _ -> assertion_num a1 - assertion_num a2

let raw_assertion_equal a1 a2 = raw_assertion_compare a1 a2 = 0

let rec is_numeric_term t =
  match t#node with
    | JCTconst _ -> true
    | JCTvar _ | JCTshift _ | JCTderef _
    | JCToffset _ | JCTinstanceof _ | JCTrange _ -> false
    | JCTbinary (t1, _, t2) -> is_numeric_term t1 && is_numeric_term t2
    | JCTunary (_, t) | JCTold t | JCTat(t,_) | JCTcast (t, _, _) 
    | JCTrange_cast (t, _) | JCTreal_cast (t, _) -> is_numeric_term t
    | JCTapp _ -> false (* TODO ? *)
    | JCTif _ | JCTmatch _ -> false (* TODO ? *)


(* assertions *)

let true_assertion = new assertion JCAtrue
let is_true a = (a#node = JCAtrue)

let make_and al = 
  (* optimization *)
  let al = List.filter (fun a -> not (is_true a)) al in
  match al with
    | [] -> true_assertion
    | [a] -> a
    | a::tl -> new assertion (JCAand al)

let rec is_constant_assertion a =
  match a#node with
    | JCAtrue | JCAfalse -> true
    | JCArelation (t1, _, t2) -> 
	is_constant_term t1 && is_constant_term t2
    | JCAand al | JCAor al ->
	List.for_all is_constant_assertion al
    | JCAimplies (a1, a2) | JCAiff (a1, a2) ->
	is_constant_assertion a1 && is_constant_assertion a2
    | JCAnot a | JCAquantifier (_, _, a) | JCAold a | JCAat(a,_)
	-> is_constant_assertion a
    | JCAapp _ | JCAinstanceof _ | JCAmutable _ | JCAtagequality _
	-> false
    | JCAbool_term t -> is_constant_term t
    | JCAif (t, a1, a2) ->
	is_constant_term t &&
	  is_constant_assertion a1 &&
	  is_constant_assertion a2
    | JCAmatch (t, pal) ->
	is_constant_term t &&
	  (List.fold_left (fun acc (_, a) -> acc && is_constant_assertion a)
	     true pal)

(* fun specs *)

let default_behavior = { 
  jc_behavior_throws = None;
  jc_behavior_assumes = None;
  jc_behavior_assigns = None;
  jc_behavior_ensures = new assertion JCAtrue
}

let contains_normal_behavior fs =
  List.exists 
    (fun (_, _, b) -> b.jc_behavior_throws = None) 
    fs.jc_fun_behavior

let contains_exceptional_behavior fs =
  List.exists
    (fun (_, _, b) -> b.jc_behavior_throws <> None)
    fs.jc_fun_behavior

let is_purely_exceptional_fun fs =
  not (contains_normal_behavior fs) && 
    contains_exceptional_behavior fs


let rec skip_shifts e = match e#node with
  | JCEshift(e,_) -> skip_shifts e
  | _ -> e

let rec skip_term_shifts t = match t#node with
  | JCTshift(t,_) -> skip_term_shifts t
  | _ -> t

let rec skip_tloc_range t = match t with
  | JCLSrange(t,_,_) -> skip_tloc_range t
  | _ -> t

(* option *)

let select_option opt default = match opt with Some v -> v | None -> default
let apply_option f opt = match opt with None -> None | Some x -> Some(f x)

(*
let direct_embedded_struct_fields st =
  List.fold_left 
    (fun acc fi -> 
      match fi.jc_field_info_type with
	| JCTpointer(JCtag st', Some _, Some _) -> 
	    assert (st.jc_struct_info_name <> st'.jc_struct_info_name);
	    fi :: acc
	| JCTpointer(JCvariant st', Some _, Some _) -> 
	    assert false (* TODO *)
	| _ -> acc
    ) [] st.jc_struct_info_fields
    
let embedded_struct_fields st =
  let rec collect forbidden_set st = 
    let forbidden_set = StringSet.add st.jc_struct_info_name forbidden_set in
    let fields = direct_embedded_struct_fields st in
    let fstructs = 
      List.fold_left 
	(fun acc fi -> match fi.jc_field_info_type with
	  | JCTpointer (JCtag st', Some _, Some _) -> 
	      assert 
		(not (StringSet.mem st'.jc_struct_info_name forbidden_set));
	      st' :: acc
	   | JCTpointer (JCvariant vi, Some _, Some _) ->
	       assert false (* TODO *)
	   | _ -> assert false (* bug ? pas acc plutot ? *)
	       (* en plus c'est un pattern-matching fragile *)
	) [] fields
    in
    fields @ List.flatten (List.map (collect forbidden_set) fstructs)
  in
  let fields = collect (StringSet.singleton st.jc_struct_info_name) st in
  let fields = 
    List.fold_left (fun acc fi -> FieldSet.add fi acc) FieldSet.empty fields
  in
  FieldSet.elements fields
*)
let field_sinfo fi = 
  match fi.jc_field_info_type with
    | JCTpointer(JCtag(st, _), _, _) -> st
    | _ -> assert false

let field_bounds fi = 
  match fi.jc_field_info_type with 
    | JCTpointer(_,Some a,Some b) -> a,b | _ -> assert false

let map_elements map =
  StringMap.fold (fun _ i acc -> i::acc) map []

(*
let embedded_struct_roots st =
  let fields = embedded_struct_fields st in
  let structs = 
    List.fold_left (fun acc fi -> StructSet.add (field_sinfo fi) acc) 
      StructSet.empty fields
  in
  let structs = StructSet.elements structs in
  let roots = 
    List.fold_left 
      (fun acc st -> StringSet.add (root_name st) acc) 
      StringSet.empty structs
  in
  StringSet.elements roots
*)
let struct_variant st =
  match st.jc_struct_info_root.jc_struct_info_variant with
    | Some vi -> vi
    | None ->
	raise (Invalid_argument
		 ("struct_variant in jc_pervasives.ml ("
		  ^st.jc_struct_info_name^", "
		  ^st.jc_struct_info_root.jc_struct_info_name^")"))
	(* don't use struct_variant before checking that every tag is used
         * in a type *)

let tag_or_variant_variant = function
  | JCtag(st, _) -> struct_variant st
  | JCvariant vi -> vi
  | JCunion vi -> vi
  
let tag_or_variant_name = function
  | JCtag(st, _) -> "tag "^st.jc_struct_info_name
  | JCvariant vi -> "variant "^vi.jc_variant_info_name
  | JCunion vi -> "union "^vi.jc_variant_info_name

let rec pattern_vars acc pat =
  match pat#node with
    | JCPstruct(_, fpl) ->
	List.fold_left
	  (fun acc (_, pat) -> pattern_vars acc pat)
	  acc fpl
    | JCPvar vi ->
	StringMap.add vi.jc_var_info_name vi acc
    | JCPor(pat, _) ->
	pattern_vars acc pat
    | JCPas(pat, vi) ->
	pattern_vars (StringMap.add vi.jc_var_info_name vi acc) pat
    | JCPany | JCPconst _ ->
	acc

let struct_of_union st =
  (struct_variant st).jc_variant_info_is_union 
  
let field_of_union fi =
  struct_of_union fi.jc_field_info_struct 

let union_of_field fi =
  let st = fi.jc_field_info_struct in
  let vi = struct_variant st in
  assert vi.jc_variant_info_is_union;
  vi

let integral_type = function
  | JCTnative Tinteger -> true
  | JCTenum _ -> true 
  | JCTnative _ | JCTlogic _ | JCTpointer _ | JCTnull | JCTany
  | JCTtype_var _ -> false

let integral_union vi =
  assert vi.jc_variant_info_is_union;
  List.fold_left (fun acc st -> acc &&
		    match st.jc_struct_info_fields with
		      | [fi] -> integral_type fi.jc_field_info_type
		      | _ -> false
		 ) true vi.jc_variant_info_roots

(* These are only used by error messages, so feel free to change the strings. *)
let string_of_op = function
  | `Blt -> ">"
  | `Bgt -> "<"
  | `Ble -> "<="
  | `Bge -> ">="
  | `Beq -> "=="
  | `Bneq -> "!="
  | `Badd -> "+"
  | `Bsub -> "-"
  | `Bmul -> "*"
  | `Bdiv -> "/"
  | `Bmod -> "%"
  | `Bland -> "&&"
  | `Blor -> "||"
  | `Bimplies -> "==>"
  | `Biff -> "<=>"
  | `Bbw_and -> "&"
  | `Bbw_or -> "|"
  | `Bbw_xor -> "xor"
  | `Bshift_left -> "shift_left"
  | `Blogical_shift_right -> "logical_shift_right"
  | `Barith_shift_right -> "arith_shift_right"
  | `Uprefix_inc -> "prefix ++"
  | `Uprefix_dec -> "prefix --"
  | `Upostfix_inc -> "postfix ++"
  | `Upostfix_dec -> "prefix --"
  | `Uplus -> "unary +"
  | `Uminus -> "unary -"
  | `Unot -> "not"
  | `Ubw_not -> "bw not"
  | `Bconcat -> "strcat"

let string_of_op_type = function
  | `Integer -> "integer"
  | `Unit -> "unit"
  | `Real -> "real"
  | `Boolean -> "boolean"
  | `Pointer -> "pointer"
  | `Logic -> "<some logic type>"

(*
Local Variables: 
compile-command: "LC_ALL=C make -j -C .. bin/jessie.byte"
End: 
*)


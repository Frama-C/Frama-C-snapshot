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
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* $Id: jc_pervasives.ml,v 1.141 2008/11/19 12:35:24 ayad Exp $ *)

open Jc_stdlib
open Jc_env
open Jc_envset
open Jc_region
open Jc_ast
open Jc_fenv

open Jc_constructors

open Format
open Num

let ( $ ) = fun f g x -> f(g x)

exception Error of Loc.position * string

let error l = 
  Format.kfprintf 
    (fun fmt -> raise (Error(l, flush_str_formatter()))) 
    str_formatter

let fold_unit f () = f

let zero = Num.Int 0
let one = Num.Int 1
let two = Num.Int 2
let eight = Num.Int 8

let rec is_power_of_two i =
  if i <=/ zero then
    false
  else
    i =/ one || mod_num i two =/ zero && is_power_of_two (i // two)

let rec log2 i =
  assert (i >/ zero);
  if i =/ one then zero else one +/ log2 (i // two)

let operator_of_native = function
  | Tunit -> `Unit
  | Tboolean -> `Boolean
  | Tinteger -> `Integer
  | Treal -> `Real
  | Tdouble -> `Double
  | Tfloat -> `Float
  | Tstring -> assert false

let operator_of_type = function
  | JCTnative n -> operator_of_native n
  | JCTenum _ -> `Integer
  | JCTlogic _ -> `Logic
  | JCTany | JCTtype_var _ -> assert false (* TODO? *)
  | JCTnull | JCTpointer _ -> `Pointer

let new_label_name =
  let label_name_counter = ref 0 in function () ->
    incr label_name_counter;
    "JC_" ^ string_of_int !label_name_counter

let root_name st =
  st.jc_struct_info_hroot.jc_struct_info_name

let field_root_name fi =
  fi.jc_field_info_hroot.jc_struct_info_name

let string_of_native t =
  match t with
    | Tunit -> "unit"
    | Tinteger -> "integer"
    | Treal -> "real"
    | Tdouble -> "double"
    | Tfloat -> "float"
    | Tboolean -> "boolean"
    | Tstring -> "string"

let rec print_type fmt t =
  match t with
    | JCTnative n -> fprintf fmt "%s" (string_of_native n)
    | JCTlogic s -> fprintf fmt "%s" s
    | JCTenum ri -> fprintf fmt "%s" ri.jc_enum_info_name
    | JCTpointer(pc, ao, bo) ->
        begin match pc with
          | JCtag({ jc_struct_info_name = name }, [])
          | JCroot { jc_root_info_name = name } ->
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

let rec location_set_region locs = 
  match locs#node with
    | JCLSvar vi -> vi.jc_var_info_region
    | JCLSderef(_,_,_,r) -> r
    | JCLSrange(ls,_,_) -> location_set_region ls
    | JCLSrange_term(t1,_,_) -> t1#region

type location =
  | JCLvar of var_info
  | JCLderef of location_set * field_info * region

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
let double_type = JCTnative Tdouble
let float_type = JCTnative Tfloat
let null_type = JCTnull
let string_type = JCTnative Tstring
let any_type = JCTany

(* temporary variables *)

let tempvar_count = ref 0
(* let reset_tmp_var () = tempvar_count := 0 *)
let tmp_var_name () = 
  incr tempvar_count; "jessie_" ^ string_of_int !tempvar_count

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
  { jc_effect_alloc_tables = AllocMap.empty;
    jc_effect_tag_tables = TagMap.empty;
    jc_effect_raw_memories = MemoryMap.empty;
    jc_effect_precise_memories = LocationMap.empty;
    jc_effect_memories = MemoryMap.empty;
    jc_effect_globals = VarMap.empty;
    jc_effect_locals = VarMap.empty;
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
(*
    jc_logic_info_is_recursive = false;
*)
    jc_logic_info_axiomatic = None;
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
(*
    jc_logic_info_is_recursive = false;
*)
    jc_logic_info_axiomatic = None;
    jc_logic_info_labels = [];
  }

let real_of_integer = make_logic_fun "real_of_int" real_type
let any_string = make_logic_fun "any_string" string_type

let () = 
  let vi = var ~formal:true integer_type "n" in
  real_of_integer.jc_logic_info_parameters <- [vi]

let full_separated = make_logic_fun "full_separated" null_type

(* logic predicates *)

let make_pred name =
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
(*
    jc_logic_info_is_recursive = false;
*)
    jc_logic_info_labels = [];
    jc_logic_info_axiomatic = None;
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
    jc_fun_info_builtin_treatment = None;
    jc_fun_info_parameters = [];
    jc_fun_info_result = vi;
    jc_fun_info_return_region = Region.make_var ty name;
    jc_fun_info_has_return_label = false; 
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
    | JCTapp _ | JCTold _ | JCTat _ | JCToffset _ | JCTaddress _ 
    | JCTbase_block _
    | JCTinstanceof _ | JCTcast _ | JCTbitwise_cast _ | JCTrange_cast _
    | JCTreal_cast _ | JCTif _ | JCTmatch _ -> false
    | JCTbinary (t1, _, t2) | JCTrange (Some t1, Some t2) ->
	is_constant_term t1 && is_constant_term t2
    | JCTunary (_, t) | JCTrange (Some t, None) | JCTrange (None, Some t) ->
	is_constant_term t

(* Comparison based only on term structure, not types not locations. *)
module TermOrd = struct 
  type t = term 

  let term_num t = match t#node with
    | JCTconst _ -> 1
    | JCTvar _ -> 3
    | JCTbinary _ -> 5
    | JCTshift _ -> 7
    | JCTunary _ -> 13
    | JCTderef _ -> 17
    | JCTold _ -> 19
    | JCToffset _ -> 23
    | JCTaddress _ -> 25
    | JCTinstanceof _ -> 31
    | JCTcast _ -> 37
    | JCTbitwise_cast _ -> 39
    | JCTrange _ -> 41
    | JCTapp _ -> 43
    | JCTif _ -> 47
    | JCTat _ -> 53
    | JCTmatch _ -> 59
    | JCTrange_cast _ -> 61
    | JCTreal_cast _ -> 67
    | JCTbase_block _ -> 71

  let rec compare t1 t2 =
    match t1#node, t2#node with
      | JCTconst c1,JCTconst c2 -> 
	  Pervasives.compare c1 c2
      | JCTvar v1,JCTvar v2 -> 
	  Pervasives.compare v1.jc_var_info_tag v2.jc_var_info_tag
      | JCTbinary(t11,op1,t12),JCTbinary(t21,op2,t22) -> 
	  let compop = Pervasives.compare op1 op2 in
	  if compop = 0 then 
	    let comp1 = compare t11 t21 in
	    if comp1 = 0 then compare t12 t22 else comp1
	  else compop
      | JCTshift(t11,t12),JCTshift(t21,t22) ->
	  let comp1 = compare t11 t21 in
	  if comp1 = 0 then compare t12 t22 else comp1
      | JCTunary(op1,t11),JCTunary(op2,t21) ->
	  let compop = Pervasives.compare op1 op2 in
	  if compop = 0 then compare t11 t21 else compop
      | JCTold t11,JCTold t21 ->
	  compare t11 t21
      | JCTaddress(absolute1,t11),JCTaddress(absolute2,t21) ->
	  let compabs = Pervasives.compare absolute1 absolute2 in
	  if compabs = 0 then
	    compare t11 t21
	  else compabs
      | JCTderef(t11,_,fi1),JCTderef(t21,_,fi2) ->
	  let compfi = 
	    Pervasives.compare fi1.jc_field_info_tag fi2.jc_field_info_tag
	  in
	  if compfi = 0 then compare t11 t21 else compfi
      | JCToffset(ok1,t11,st1),JCToffset(ok2,t21,st2) ->
	  let compok = Pervasives.compare ok1 ok2 in
	  if compok = 0 then
	    let compst = 
	      Pervasives.compare st1.jc_struct_info_name st2.jc_struct_info_name
	    in
	    if compst = 0 then compare t11 t21 else compst
	  else compok
      | JCTinstanceof(t11,lab1,st1),JCTinstanceof(t21,lab2,st2) 
      | JCTcast(t11,lab1,st1),JCTcast(t21,lab2,st2)
      | JCTbitwise_cast(t11,lab1,st1),JCTbitwise_cast(t21,lab2,st2) ->
	  let compst = 
	    Pervasives.compare st1.jc_struct_info_name st2.jc_struct_info_name
	  in
	  if compst <> 0 then compst else
	    let compst = 
	      Pervasives.compare lab1 lab2
	    in
	    if compst <> 0 then compst else
	      compare t11 t21 
      | JCTreal_cast(t11,conv1),JCTreal_cast(t21,conv2) ->
	  let comp = 
	    Pervasives.compare conv1 conv2
	  in
	  if comp <> 0 then comp else
	    compare t11 t21 
      | JCTrange_cast(t11,ri1),JCTrange_cast(t21,ri2) ->	
	  let comp = 
	    Pervasives.compare ri1.jc_enum_info_name ri2.jc_enum_info_name
	  in
	  if comp <> 0 then comp else
	    compare t11 t21 
      | JCTrange(t11opt,t12opt),JCTrange(t21opt,t22opt) ->
	  let comp1 = option_compare compare t11opt t21opt in
	  if comp1 = 0 then 
	    option_compare compare t12opt t22opt
	  else comp1
      | JCTapp app1,JCTapp app2 ->
	  let li1 = app1.jc_app_fun and ts1 = app1.jc_app_args in
	  let li2 = app2.jc_app_fun and ts2 = app2.jc_app_args in
	  let compli = 
	    Pervasives.compare li1.jc_logic_info_tag li2.jc_logic_info_tag
	  in
	  if compli = 0 then
	    list_compare compare ts1 ts2
	  else compli
      | JCTif(t11,t12,t13),JCTif(t21,t22,t23) ->
	  let comp1 = compare t11 t21 in
	  if comp1 = 0 then 
	    let comp2 = compare t12 t22 in
	    if comp2 = 0 then compare t13 t23 else comp2
	  else comp1
      |JCTat(t11,lab1),JCTat(t21,lab2) ->
	 let compst = 
	   Pervasives.compare lab1 lab2
	 in
	 if compst <> 0 then compst else
	   compare t11 t21 
      | JCTmatch _, JCTmatch _ -> assert false (* TODO *)
      | JCTbase_block t11, JCTbase_block t21 ->
	  compare t11 t21	  
      | _ ->
	  (* Terms should have different constructors *)
	  assert (term_num t1 <> term_num t2);
	  term_num t2 - term_num t1

  let equal t1 t2 = compare t1 t2 = 0

  let rec hash t = 
    let h = match t#node with
      | JCTconst c1 -> 
	  Hashtbl.hash c1
      | JCTvar v1 -> 
	  Hashtbl.hash v1.jc_var_info_tag
      | JCTbinary(t11,op1,t12) -> 
	  Hashtbl.hash op1 * hash t11 * hash t12
      | JCTshift(t11,t12) ->
	  hash t11 * hash t12
      | JCTunary(op1,t11) ->
	  Hashtbl.hash op1 * hash t11 
      | JCTold t11 ->
	  hash t11 
      | JCTaddress(absolute1,t11) ->
	  Hashtbl.hash absolute1 * hash t11
      | JCTderef(t11,_,fi1) ->
	  Hashtbl.hash fi1.jc_field_info_tag * hash t11
      | JCToffset(ok1,t11,st1) ->
	  Hashtbl.hash ok1 * hash t11 
	  * Hashtbl.hash st1.jc_struct_info_name
      | JCTinstanceof(t11,_,_)
      | JCTcast(t11,_,_)
      | JCTbase_block(t11)
      | JCTbitwise_cast(t11,_,_)
      | JCTreal_cast(t11,_)
      | JCTrange_cast(t11,_)
      | JCTat(t11,_) -> 
	  hash t11
      | JCTrange(t11opt,t12opt) ->
	  Option_misc.map_default hash 1 t11opt
	  * Option_misc.map_default hash 1 t12opt
      | JCTapp app1 ->
	  let li1 = app1.jc_app_fun and ts1 = app1.jc_app_args in
	  List.fold_left 
	    (fun h arg -> h * hash arg) 
	    (Hashtbl.hash li1.jc_logic_info_tag) ts1
      | JCTif(t11,t12,t13) ->
	  hash t11 * hash t12 * hash t13
      | JCTmatch (_, _) -> assert false (* TODO *)
    in
    Hashtbl.hash (term_num t) * h
	
end

module TermTable = Hashtbl.Make(TermOrd)
module TermSet = Set.Make(TermOrd)
module TermMap = Map.Make(TermOrd)
	

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
        if compst = 0 then TermOrd.compare t1 t2 else compst
  | _ -> tag_num tag2 - tag_num tag1


(* Comparison based only on assertion structure, not locations. *)
module AssertionOrd = struct 
  type t = assertion

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
    | JCAeqtype _ -> 51
    | JCAat _ -> 53
    | JCAmatch _ -> 59
    | JCAsubtype _ -> 61

  let rec compare a1 a2 =
    match a1#node, a2#node with
      | JCAtrue,JCAtrue | JCAfalse,JCAfalse -> 0
      | JCArelation(t11,op1,t12),JCArelation(t21,op2,t22) ->
          let compop = Pervasives.compare op1 op2 in
          if compop = 0 then 
	    let comp1 = TermOrd.compare t11 t21 in
	    if comp1 = 0 then TermOrd.compare t12 t22 else comp1
          else compop
      | JCAand als1,JCAand als2 | JCAor als1,JCAor als2 ->
	  list_compare compare als1 als2
      | JCAimplies(a11,a12),JCAimplies(a21,a22) 
      | JCAiff(a11,a12),JCAiff(a21,a22) ->
          let comp1 = compare a11 a21 in
          if comp1 = 0 then compare a12 a22 else comp1
      | JCAnot a1,JCAnot a2 | JCAold a1,JCAold a2 ->
          compare a1 a2
      | JCAapp app1, JCAapp app2 ->
	  let li1 = app1.jc_app_fun in
	  let li2 = app2.jc_app_fun in
          let compli = 
	    Pervasives.compare li1.jc_logic_info_tag li2.jc_logic_info_tag
          in
          if compli = 0 then
	    let tls1 = app1.jc_app_args in
	    let tls2 = app2.jc_app_args in
  	    list_compare TermOrd.compare tls1 tls2
          else compli
      | JCAquantifier(q1,vi1,a1),JCAquantifier(q2,vi2,a2) ->
          let compq = Pervasives.compare q1 q2 in
          if compq = 0 then 
	    let compvi = Pervasives.compare vi1 vi2 in
	    if compvi = 0 then compare a1 a2 else compvi
          else compq
      | JCAinstanceof(t1,_,st1),JCAinstanceof(t2,_,st2) ->
          let compst = 
	    Pervasives.compare st1.jc_struct_info_name st2.jc_struct_info_name
          in
          if compst = 0 then TermOrd.compare t1 t2 else compst
      | JCAbool_term t1,JCAbool_term t2 ->
          TermOrd.compare t1 t2
      | JCAif(t1,a11,a12),JCAif(t2,a21,a22) ->
          let comp0 = TermOrd.compare t1 t2 in
          if comp0 = 0 then 
	    let comp1 = compare a11 a21 in
	    if comp1 = 0 then compare a12 a22 else comp1
          else comp0
      | JCAmutable(t1,st1,tag1),JCAmutable(t2,st2,tag2) ->
          let compst = 
	    Pervasives.compare st1.jc_struct_info_name st2.jc_struct_info_name
          in
          if compst = 0 then
	    let comptag = raw_tag_compare tag1 tag2 in
	    if comptag = 0 then TermOrd.compare t1 t2 else comptag
          else compst
      | JCAeqtype(tag11,tag12,so1),JCAeqtype(tag21,tag22,so2) ->
          let compso = option_compare Pervasives.compare so1 so2 in
          if compso = 0 then
	    let comptag = raw_tag_compare tag11 tag21 in
	    if comptag = 0 then raw_tag_compare tag12 tag22 else comptag
          else compso
      | _ -> 
	  (* Assertions should have different constructors *)
	  assert (assertion_num a1 <> assertion_num a2);
	  assertion_num a1 - assertion_num a2

  let equal a1 a2 = compare a1 a2 = 0
end

let rec is_numeric_term t =
  match t#node with
    | JCTconst _ -> true
    | JCTvar _ | JCTshift _ | JCTderef _
    | JCToffset _ | JCTaddress _ | JCTinstanceof _ | JCTrange _ -> false
    | JCTbinary (t1, _, t2) -> is_numeric_term t1 && is_numeric_term t2
    | JCTunary (_, t) | JCTold t | JCTat(t,_) | JCTcast (t, _, _) 
    | JCTbitwise_cast (t, _, _) | JCTbase_block t
    | JCTrange_cast (t, _) | JCTreal_cast (t, _) -> is_numeric_term t
    | JCTapp _ -> false (* TODO ? *)
    | JCTif _ | JCTmatch _ -> false (* TODO ? *)


(* assertions *)

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
    | JCAapp _ | JCAinstanceof _ | JCAmutable _ | JCAeqtype _
    | JCAsubtype _
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
  jc_behavior_ensures = new assertion JCAtrue;
  jc_behavior_free_ensures = new assertion JCAtrue;
}

let contains_normal_behavior fs =
  List.exists 
    (fun (_, _, b) -> b.jc_behavior_throws = None) 
    (fs.jc_fun_default_behavior :: fs.jc_fun_behavior)

let contains_exceptional_behavior fs =
  List.exists
    (fun (_, _, b) -> b.jc_behavior_throws <> None)
    (fs.jc_fun_default_behavior :: fs.jc_fun_behavior)

let is_purely_exceptional_fun fs =
  not (contains_normal_behavior fs) && 
    contains_exceptional_behavior fs


let rec skip_shifts e = match e#node with
  | JCEshift(e,_) -> skip_shifts e
  | _ -> e

let rec skip_term_shifts t = match t#node with
  | JCTshift(t,_) -> skip_term_shifts t
  | _ -> t

let rec skip_tloc_range locs = match locs#node with
  | JCLSrange(t,_,_) -> skip_tloc_range t
  | _ -> locs

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

let pointer_struct = function
  | JCTpointer(JCtag(st, []), _, _) -> st
  | ty -> 
      Format.printf "%a@." print_type ty;
      assert false

let pointer_class = function
  | JCTpointer(pc, _, _) -> pc
  | ty -> 
      Format.printf "%a@." print_type ty;
      assert false

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
let struct_root st =
  match st.jc_struct_info_hroot.jc_struct_info_root with
    | Some vi -> vi
    | None ->
	raise (Invalid_argument
		 ("struct_root in jc_pervasives.ml ("
		  ^st.jc_struct_info_name^", "
		  ^st.jc_struct_info_hroot.jc_struct_info_name^")"))
	(* don't use struct_root before checking that every tag is used
         * in a type *)

let pointer_class_root = function
  | JCtag(st, _) -> struct_root st
  | JCroot vi -> vi
  
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

let root_is_plain_union rt = rt.jc_root_info_kind = RplainUnion
let root_is_discr_union rt = rt.jc_root_info_kind = RdiscrUnion
let root_is_union rt = root_is_plain_union rt || root_is_discr_union rt

let struct_of_plain_union st =
  let vi = struct_root st in vi.jc_root_info_kind = RplainUnion

let struct_of_discr_union st =
  let vi = struct_root st in vi.jc_root_info_kind = RdiscrUnion

let struct_of_union st =
  let vi = struct_root st in root_is_union vi
  
let union_of_field fi =
  let st = fi.jc_field_info_struct in
  let vi = struct_root st in
  assert (root_is_union vi);
  vi

let integral_union vi =
  assert (root_is_union vi);
  List.fold_left (fun acc st -> acc &&
		    match st.jc_struct_info_fields with
		      | [fi] -> is_integral_type fi.jc_field_info_type
		      | _ -> false
		 ) true vi.jc_root_info_hroots

let struct_has_bytesize st =
  List.fold_left 
    (fun acc fi -> acc && 
       match fi.jc_field_info_bitsize with None -> false | Some _ -> true)
    true st.jc_struct_info_fields
  
let struct_bitsize st = 
  List.fold_left 
    (fun acc fi -> 
       match fi.jc_field_info_bitsize with
	 | Some x -> acc + x
	 | None -> assert false)
    0 st.jc_struct_info_fields

let struct_bytesize st = 
  struct_bitsize st / 8

let possible_struct_bytesize st = 
  if struct_has_bytesize st then Some (struct_bytesize st) else None

(* These are only used by error messages, so feel free to change the strings. *)
let string_of_op = function
  | `Blt -> "<"
  | `Bgt -> ">"
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
  | `Double -> "gen_float"
  | `Float -> "gen_float"
  | `Boolean -> "boolean"
  | `Pointer -> "pointer"
  | `Logic -> "<some logic type>"


let builtin_logic_symbols =
  (* return type, jessie name, why name, parameter types *)
  [ Some real_type, "\\real_abs", "abs_real", [real_type] ;
    Some real_type, "\\real_sqrt", "sqrt_real", [real_type];  
    Some real_type, "\\real_max", "real_max", [real_type; real_type] ;
    Some real_type, "\\real_min", "real_min", [real_type; real_type] ;
    Some integer_type, "\\int_max", "int_max", [integer_type; integer_type] ;
    Some integer_type, "\\int_min", "int_min", [integer_type; integer_type] ;
  ]

let builtin_function_symbols =
  (* return type, jessie name, why name, parameter types, special treatment *)
  [
    double_type, "\\double_sqrt", "sqrt_gen_float", [double_type], TreatGenFloat `Double ;
    float_type, "\\float_sqrt", "sqrt_gen_float", [float_type], TreatGenFloat `Float ;
    double_type, "\\double_abs", "abs_gen_float", [double_type], TreatGenFloat `Double  ;
    float_type, "\\float_abs", "abs_gen_float", [float_type], TreatGenFloat `Float ;
    double_type, "\\neg_double", "neg_gen_float", [double_type], TreatGenFloat `Double ;
    float_type, "\\neg_float", "neg_gen_float", [float_type], TreatGenFloat `Float ;  
]




(*
Local Variables: 
compile-command: "LC_ALL=C make -j -C .. bin/jessie.byte"
End: 
*)


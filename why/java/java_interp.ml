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

(* $Id: java_interp.ml,v 1.133 2008/04/22 06:53:45 nrousset Exp $ *)

open Format
open Jc_output
open Jc_env
open Jc_fenv
open Jc_ast
open Jc_constructors
open Jc_constructors.PExpr
open Jc_constructors.PDecl
open Jc_pervasives
open Java_env
open Java_ast
open Java_tast
open Java_pervasives
open Java_typing

let exn_name e = new identifier e.jc_exception_info_name
let var_name v = v.jc_var_info_name
let fi_name f = f.jc_field_info_name

let reg_loc ?id ?kind ?name loc = Output.reg_loc "K" ?id ?kind ?name loc

(*s loop tags *)

let get_loop_counter = 
  let counter = ref 0 in
  function () -> let tag = !counter in incr counter; tag

(*s range types *)

(* byte = int8 *)
let byte_range =
  {
    jc_enum_info_name = "byte";
    jc_enum_info_min = min_byte;
    jc_enum_info_max = max_byte;
  }

(* short = int16 *)
let short_range =
  {
    jc_enum_info_name = "short";
    jc_enum_info_min = min_short;
    jc_enum_info_max = max_short;
  }

(* int = int32 *)
let int_range =
  {
    jc_enum_info_name = "int32";
    jc_enum_info_min = min_int;
    jc_enum_info_max = max_int;
  }

(* long = int64 *)
let long_range =
  {
    jc_enum_info_name = "long";
    jc_enum_info_min = min_long;
    jc_enum_info_max = max_long;
  }

(* char = uint16 *)
let char_range =
  {
    jc_enum_info_name = "char";
    jc_enum_info_min = min_char;
    jc_enum_info_max = max_char;
  }

let range_types acc =
  if !Java_options.ignore_overflow then acc else
  List.fold_left
    (fun acc ri ->
       (mkenum_type_def
          ~name: ri.jc_enum_info_name
          ~left: ri.jc_enum_info_min
          ~right: ri.jc_enum_info_max
          ())::acc) 
    acc [ byte_range ; short_range ; int_range ; long_range ; char_range ]


let byte_type = JCTenum byte_range
let short_type = JCTenum short_range 
let int_type = JCTenum int_range 
let long_type = JCTenum long_range 
let char_type = JCTenum char_range 

let get_enum_info t =
  match t with
    | Tshort -> short_range
    | Tint -> int_range
    | Tlong -> long_range
    | Tchar -> char_range
    | Tbyte -> byte_range
    | _ -> assert false

let tr_base_type t =
  match t with
    | Tunit -> Jc_pervasives.unit_type
    | Tboolean -> Jc_pervasives.boolean_type
    | Tinteger -> Jc_pervasives.integer_type
    | Tshort -> 
	if !Java_options.ignore_overflow then Jc_pervasives.integer_type else
	short_type
    | Tint -> 
	if !Java_options.ignore_overflow then Jc_pervasives.integer_type else
	int_type
    | Tlong -> 
	if !Java_options.ignore_overflow then Jc_pervasives.integer_type else
	long_type
    | Tchar -> 
	if !Java_options.ignore_overflow then Jc_pervasives.integer_type else
	char_type
    | Tbyte  -> 
	if !Java_options.ignore_overflow then Jc_pervasives.integer_type else
	byte_type
    | Treal -> Jc_pervasives.real_type
    | Tfloat -> Jc_pervasives.real_type (* TODO *)
    | Tdouble -> Jc_pervasives.real_type (* TODO *)

(*s class types *)

let rec object_variant = {
  jc_variant_info_name = "Object";
  jc_variant_info_roots = [ object_root ];
  jc_variant_info_is_union = false;
}

and object_root = {
  jc_struct_info_name = "Object";
  jc_struct_info_parent = None;
  jc_struct_info_root = object_root;
  jc_struct_info_fields = [];
  jc_struct_info_variant = Some object_variant;
}

let get_class name =
  {
    jc_struct_info_name = name;
    jc_struct_info_parent = None;
    jc_struct_info_root = object_root;
    jc_struct_info_fields = [];
    jc_struct_info_variant = Some object_variant;
  }

(*
let get_interface ii =
  {
    jc_struct_info_name = ii.interface_info_name;
    jc_struct_info_parent = None;
    jc_struct_info_root = ii.interface_info_name;
    jc_struct_info_fields = [];
  }
*)

let rec interface_root = {
  jc_struct_info_name = "interface";
  jc_struct_info_parent = None;
  jc_struct_info_root = interface_root;
  jc_struct_info_fields = [];
  jc_struct_info_variant = Some object_variant;
}

let st_interface = 
  {
    jc_struct_info_name = "interface";
    jc_struct_info_parent = None;
    jc_struct_info_root = interface_root;
    jc_struct_info_fields = [];
    jc_struct_info_variant = Some object_variant;
  }

(*s array types *)

let num_zero = Num.Int 0
let num_minus_one = Num.Int (-1)

let array_struct_table = Hashtbl.create 17
      
let rec get_array_struct loc t = 
  try
    (Hashtbl.find array_struct_table t: struct_info)
  with Not_found -> 
    eprintf "Array struct for type %a not found: %a@." 
      Java_typing.print_type t Loc.report_position loc;
    raise Not_found

and tr_type loc t =
  match t with
    | JTYbase t -> tr_base_type t	
    | JTYnull -> JCTnull
    | JTYclass (non_null, ci) -> 
	let st = get_class ci.class_info_name in
	  JCTpointer 
	    (JCtag st, Some num_zero, if non_null then Some num_zero else None)
    | JTYinterface ii ->
	JCTpointer(JCtag st_interface, Some num_zero,None)
(*
	let st = get_interface ii in
	JCTpointer(st,Some num_zero,
	           (* if non_null then Some num_zero else *) None)
*)
	
    | JTYarray (non_null, t) ->
	let st = get_array_struct loc t in
	  JCTpointer (JCtag st, Some num_zero, if non_null then Some num_minus_one else None)
    | JTYlogic i -> JCTlogic i

(*s structure fields *)

let fi_table = Hashtbl.create 97

let get_field fi =
  try
    Hashtbl.find fi_table fi.java_field_info_tag
  with
      Not_found -> 
	eprintf "Internal error: field '%s' not found@." 
	  fi.java_field_info_name;
	assert false

let create_field loc fi =
  Java_options.lprintf "Creating JC field '%s'@." fi.java_field_info_name;
  let ty = tr_type loc fi.java_field_info_type in
  let ci = 
    match fi.java_field_info_class_or_interface with
      | TypeClass ci -> get_class ci.class_info_name
      | TypeInterface ii -> get_class ii.interface_info_name
  in
  let nfi =
    { jc_field_info_name = fi.java_field_info_name;
      jc_field_info_final_name = fi.java_field_info_name;
      jc_field_info_tag  = fi.java_field_info_tag;
      jc_field_info_type = ty;
      jc_field_info_root = ci.jc_struct_info_root;
      jc_field_info_struct = ci;
      jc_field_info_rep = false;
      (*
	jc_field_info_final_name = vi.java_field_info_name;
	jc_var_info_assigned = vi.java_var_info_assigned;
	jc_var_info_type = tr_type vi.java_var_info_type;
	jc_var_info_tag = vi.java_var_info_tag;
      *)
    }
  in Hashtbl.add fi_table fi.java_field_info_tag nfi;
  nfi

let static_fields_table = Hashtbl.create 97

let get_static_var fi =
  try
    Hashtbl.find static_fields_table fi.java_field_info_tag
  with
      Not_found -> 
	eprintf "Java_interp.get_static_var->Not_found: %s@." fi.java_field_info_name;
	raise Not_found
 

(* local variables and parameters *)

let vi_table = Hashtbl.create 97

let get_var vi =
  try
    Hashtbl.find vi_table vi.java_var_info_tag
  with
      Not_found -> 
	eprintf "Java_interp.get_var->Not_found: '%s', %a@." 
	  vi.java_var_info_final_name
	  Loc.report_position vi.java_var_info_decl_loc
	;
	raise Not_found

let create_var ?(formal=false) loc vi =
  let ty = tr_type loc vi.java_var_info_type in
  let nvi = Jc_pervasives.var ~formal ty vi.java_var_info_final_name in
  nvi.jc_var_info_assigned <- vi.java_var_info_assigned;
  Hashtbl.add vi_table vi.java_var_info_tag nvi;
  nvi

(*s logic types *)

let tr_logic_type id acc =
  JClogic_type_def id :: acc

(*s logic funs *)

let logics_table = Hashtbl.create 97

let get_logic_fun fi =
  try
    Hashtbl.find logics_table fi.java_logic_info_tag
  with
      Not_found -> 
	eprintf "Anomaly: cannot find logic symbol `%s'@." fi.java_logic_info_name;
	assert false

let tr_logic_label = function
  | LabelPre -> Jc_env.LabelPre
  | LabelHere -> Jc_env.LabelHere
  | LabelOld -> Jc_env.LabelOld
  | LabelName s -> 
      Jc_env.LabelName { 
	label_info_name = s; 
	label_info_final_name = s;
	times_used = 0;
      }

let create_logic_fun loc fi =
  let nfi =
    match fi.java_logic_info_result_type with
      | None ->
	  Jc_pervasives.make_rel fi.java_logic_info_name 
      | Some t ->
	  Jc_pervasives.make_logic_fun fi.java_logic_info_name 
	    (tr_type loc t) 
  in
  nfi.jc_logic_info_parameters <-
    List.map (create_var loc) fi.java_logic_info_parameters;
  nfi.jc_logic_info_labels <- 
    List.map tr_logic_label fi.java_logic_info_labels;
  Hashtbl.add logics_table fi.java_logic_info_tag nfi;
  nfi

(*s program funs *)

let funs_table = Hashtbl.create 97

let get_fun loc tag =
  try
    Hashtbl.find funs_table tag
  with
      Not_found -> 
	eprintf "Java_interp.get_fun->Not_found: %a@." Loc.report_position loc;
	raise Not_found

let create_fun loc tag result name params =
  let nfi =
    match result with
      | None ->
	  Jc_pervasives.make_fun_info name 
	    Jc_pervasives.unit_type
      | Some vi ->
	  Jc_pervasives.make_fun_info name
	    (tr_type loc vi.java_var_info_type) 
  in
  nfi.jc_fun_info_parameters <-
    List.map (fun (vi, _) -> create_var loc vi) params;
  Hashtbl.add funs_table tag nfi;
  nfi

(*s exceptions *)

let exceptions_table = Hashtbl.create 17 

let get_exception ty =
  match ty with
    | JTYclass(_,ci) ->
	begin
	  try
	    Hashtbl.find exceptions_table ci.class_info_name
	  with
	      Not_found -> 
		eprintf "exception %s not found@." ci.class_info_name;
		assert false
	end
    | _ -> assert false

let exceptions_tag = ref 0

let create_exception ty n =
  incr exceptions_tag;
  let ei =
    { jc_exception_info_name = n;
      jc_exception_info_tag = !exceptions_tag;
      jc_exception_info_type = ty     
    }
  in
  Hashtbl.add exceptions_table n ei;
  ei

(*s terms *)

let lit l =
  match l with
  | Integer s | Char s -> JCCinteger s
  | Float s -> JCCreal s
  | Bool b -> JCCboolean b
  | String s -> JCCstring s
  | Null  -> JCCnull

let lun_op t op: [> Jc_ast.unary_op] =
  match op with
    | Unot -> `Unot
    | Uminus when (t = Tinteger || t = Tint) -> `Uminus
    | Uminus -> 
	begin match t with
	  | Tshort  -> assert false (* TODO *)
	  | Tboolean  -> assert false (* TODO *)
	  | Tbyte  -> assert false (* TODO *)
	  | Tchar  -> assert false (* TODO *)
	  | Tint  -> assert false (* should never happen *)
	  | Tfloat  -> assert false (* TODO *)
	  | Tlong  -> assert false (* TODO *)
	  | Tdouble  -> assert false (* TODO *)
	  | Treal  -> assert false (* TODO *)
	  | Tunit -> assert false (* TODO *)
	  | Tinteger -> assert false (* should never happen *)
	end
    | Uplus -> assert false
    | Ucompl -> `Ubw_not
	
let lbin_op t op: [> Jc_ast.operational_op] =
  match op with
    | Bgt -> `Bgt
    | Bge -> `Bge
    | Ble -> `Ble
    | Blt -> `Blt
    | Bne -> `Bneq
    | Beq -> `Beq
    | Basr -> `Barith_shift_right
    | Blsr -> `Blogical_shift_right
    | Blsl -> `Bshift_left
    | Bbwxor -> `Bbw_xor
    | Bbwor -> `Bbw_or
    | Bbwand -> `Bbw_and
    | Biff|Bimpl|Bor|Band ->
        assert false
        (* TODO (il suffit d'utiliser bin_op au lieu de operational_op) *)
    | Bmod -> `Bmod
    | Bdiv -> `Bdiv
    | Bmul -> `Bmul
    | Bsub -> `Bsub
    | Badd -> `Badd

let lobj_op op: [> comparison_op] =
  match op with
    | Bne -> `Bneq
    | Beq -> `Beq
    | _ -> assert false

(* non_null funs & preds *)
    
let non_null_funs = Hashtbl.create 17
let non_null_preds = Hashtbl.create 17
  
let non_null_fun si =
  try
    Hashtbl.find non_null_funs si.jc_struct_info_name
  with
      Not_found -> assert false

let non_null_pred name =
  try
    Hashtbl.find non_null_preds name
  with
      Not_found -> assert false
	
let create_non_null_fun si =
  let fi = 
    Jc_pervasives.make_fun_info 
      ("non_null_" ^ si.jc_struct_info_name)
      Jc_pervasives.boolean_type
  in
    Hashtbl.add non_null_funs si.jc_struct_info_name fi;
    fi

let create_non_null_pred si =
  let li = 
    Jc_pervasives.make_rel 
      ("Non_null_" ^ si.jc_struct_info_name)
  in
    Hashtbl.add non_null_preds si.jc_struct_info_name li;
    li
(*
let dummy_loc_term ty t =
  new term ~typ:ty t

let term_zero = 
  dummy_loc_term Jc_pervasives.integer_type 
    (JCTconst (JCCinteger "0"))

let term_maxint = 
  dummy_loc_term Jc_pervasives.integer_type 
    (JCTconst (JCCinteger "2147483647"))

let term_plus_one t =
  JCTbinary (t, `Badd_int, { t with jc_term_node = JCTconst (JCCinteger "1") })
*)

let zero = mkint ~value:0 ()
let maxint = mkint ~valuestr:"2147483647" ()
let plus_one e =
  let loc = e#loc in
  mkadd ~loc ~expr1:e ~expr2:(mkint ~loc ~value:1 ()) ()

let rec term t =
  let t' =
    match t.java_term_node with
      | JTlit l ->
          mkconst ~const:(lit l) ()
      | JTun (t,op,e1) ->
          mkunary
            ~op:(lun_op t op)
            ~expr:(term e1)
            ()
      | JTbin(e1,t,op,e2) ->
          mkbinary
            ~expr1:(term e1)
            ~op:(lbin_op t op)
            ~expr2:(term e2)
            ()
      | JTapp (fi, el) -> 
          mkapp
            ~fun_name: (get_logic_fun fi).jc_logic_info_name
            ~args: (List.map term el)
            ()
      | JTvar vi ->
          mkvar ~name:(var_name (get_var vi)) ()
      | JTfield_access(t,fi) ->
          mkderef
            ~expr: (term t)
            ~field: (fi_name (get_field fi))
            ()
      | JTstatic_field_access(ci,fi) ->	  
	  mkvar ~name:(var_name (get_static_var fi)) ()
      | JTarray_length(t) -> 
	  begin
	    match t.java_term_type with
	      | JTYarray (_, ty) ->
		  let _st = get_array_struct t.java_term_loc ty in
		  let t = term t in
		  plus_one (mkoffset_max ~loc:t#loc ~expr:t ())
	      | _ -> assert false
	  end
      | JTarray_access(t1,t2) -> 
	  begin
	    match t1.java_term_type with
	      | JTYarray (_, ty) ->
		  let st = get_array_struct t.java_term_loc ty in
		  let t1' = term t1 in
                  mkderef
                    ~expr:
                    (mkshift
                       ~loc: t.java_term_loc
                       ~expr: t1'
                       ~offset: (term t2)
                       ())
                    ~field: (fi_name (List.hd st.jc_struct_info_fields))
                    ()
	      | _ -> assert false
	  end
      | JTarray_range _ -> assert false
(*       | JTold t -> *)
(*           mkold ~expr: (term t) () *)
      | JTat(t,lab) ->
          mkat
            ~expr: (term t)
            ~label: (tr_logic_label lab)
            ()
      | JTcast(ty,(*lab,*)t) ->
	  begin
	    match ty with
	      | JTYbase _ -> term t
	      | JTYclass(_,ci) ->
		  let st = get_class ci.class_info_name in
                  mkcast
                    ~expr: (term t)
                    ~typ: st.jc_struct_info_name
                    ()
	      | _ -> assert false (* TODO *)
	  end
  in
  let _ = tr_type t.java_term_loc t.java_term_type in
  new pexpr ~loc: t.java_term_loc t'#node

let quantifier = function
  | Forall -> Jc_ast.Forall
  | Exists -> Jc_ast.Exists

let ptype_node_of_type = function
  | JCTnative n -> JCPTnative n
  | JCTlogic s -> JCPTidentifier s
  | JCTenum e -> JCPTidentifier e.jc_enum_info_name
  | JCTpointer(JCtag st, l, r) -> JCPTpointer(st.jc_struct_info_name, l, r)
  | JCTpointer((JCvariant v | JCunion v), l, r) ->
      JCPTpointer(v.jc_variant_info_name, l, r)
  | JCTnull
  | JCTany -> assert false
let ptype_of_type t = new ptype (ptype_node_of_type t)

let rec assertion ?(reg=0) a =
  let a' =
    match a.java_assertion_node with
      | JAtrue ->
          mkboolean ~value:true ()
      | JAfalse ->
          mkboolean ~value:false ()
      | JAat(a,lab) -> 
	  mkat 
	    ~expr:(assertion a) 
	    ~label:(tr_logic_label lab)
	    ()
      | JAnot a ->
          mknot ~expr:(assertion a) ()
      | JAbin (e1, t, op, e2) ->
          mkbinary
            ~expr1: (term e1)
            ~op: (lbin_op t op)
            ~expr2: (term e2)
            ()
      | JAbin_obj (e1, op, e2) -> (* case e1 != null *) 
	  if op = Bne && e2.java_term_node = JTlit Null then
	    let t1 = term e1 in
	      match e1.java_term_type with
		| JTYbase _ | JTYnull | JTYlogic _ -> assert false
		| JTYclass (_, ci) ->
                    mkapp
                      ~fun_name: (non_null_pred "Object").jc_logic_info_name
                      ~args: [t1]
                      ()
		| JTYinterface ii ->
                    mkeq
                      ~expr1: (mkoffset_max ~expr:t1 ())
                      ~expr2: zero
                      ()
		| JTYarray (_, t) ->
		    let si = get_array_struct Loc.dummy_position t in
                    let li = non_null_pred si.jc_struct_info_name in
                    mkapp
                      ~fun_name: li.jc_logic_info_name
                      ~args: [t1]
                      ()
	  else mkbinary
            ~expr1: (term e1)
            ~op: (lobj_op op)
            ~expr2: (term e2)
            ()
      | JAapp (fi, el)-> 
          mkapp
            ~fun_name: (get_logic_fun fi).jc_logic_info_name
            ~args: (List.map term el)
            ()
      | JAquantifier (q, vi, a)-> 
	  let vi = create_var a.java_assertion_loc vi in
          mkquantifier
            ~quantifier: (quantifier q)
            ~typ: (ptype_of_type vi.jc_var_info_type)
            ~vars: [var_name vi]
            ~body: (assertion a)
            ()
      | JAimpl (a1, a2)-> 
          mkimplies
            ~expr1: (assertion a1)
            ~expr2: (assertion a2)
            ()
      | JAiff (a1, a2)-> 
          mkiff
            ~expr1: (assertion a1)
            ~expr2: (assertion a2)
            ()
      | JAor (a1, a2)-> 
          mkor
            ~expr1: (assertion a1)
            ~expr2: (assertion a2)
            ()
      | JAand (a1, a2)-> 
          mkand
            ~expr1: (assertion a1)
            ~expr2: (assertion a2)
            ()
      | JAbool_expr t ->
          term t
      | JAinstanceof (t, lab, ty) ->
	  let ty = tr_type Loc.dummy_position ty in
	    match ty with
	      | JCTpointer (JCtag si, _, _) ->
                  mkinstanceof
                    ~expr: (term t)
                    ~typ: si.jc_struct_info_name
                    ()
	      | _ -> assert false
  in
  new pexpr ~loc:a.java_assertion_loc a'#node
    
(*let dummy_loc_assertion a =
  { jc_assertion_loc = Loc.dummy_position; 
    jc_assertion_label = "";
    jc_assertion_node = a }
*)

let create_static_var loc type_name fi =
  let ty = tr_type loc fi.java_field_info_type in
  let name = type_name ^ "_" ^ fi.java_field_info_name in
  let vi = Jc_pervasives.var ~static:true ty name in
  Hashtbl.add static_fields_table fi.java_field_info_tag vi;
  vi

(*s translation of structure types *)

let rec term_of_expr e = 
  let t =
    match e.java_expr_node with
      | JElit l -> JTlit l
      | JEvar vi -> JTvar vi
      | JEbin (e1, op, e2) -> 
	  JTbin (term_of_expr e1, Tinteger, op, term_of_expr e2)
      | JEun (op, e) -> JTun (Tinteger, op, term_of_expr e)
      | JEfield_access (e, fi) -> JTfield_access (term_of_expr e, fi)
      | JEstatic_field_access (ty, fi) -> JTstatic_field_access (ty, fi)
      | JEarray_access (e1, e2) ->
	  JTarray_access (term_of_expr e1, term_of_expr e2)
      | JEcast (t, e) -> JTcast (t, term_of_expr e)
      | _ -> assert false
  in
    { java_term_loc = e.java_expr_loc;
      java_term_type = e.java_expr_type;
      java_term_node = t }
      
(* exceptions *)

let tr_exception ei acc =
  (mkexception_def
     ~name:ei.jc_exception_info_name
     ?arg_type:(Option_misc.map ptype_of_type ei.jc_exception_info_type)
     ()) :: acc
  
(* array_length funs *)

let java_array_length_funs = Hashtbl.create 17

let java_array_length_fun st =
  try
    Hashtbl.find java_array_length_funs st.jc_struct_info_name 
  with
      Not_found -> assert false

let create_java_array_length_fun st =
  let fi = 
    Jc_pervasives.make_fun_info 
      ("java_array_length_" ^ st.jc_struct_info_name)
      Jc_pervasives.integer_type
  in
  Hashtbl.add java_array_length_funs st.jc_struct_info_name fi;
  fi
    
let array_types decls =
  Java_options.lprintf "(**********************)@.";
  Java_options.lprintf "(* array types        *)@.";
  Java_options.lprintf "(**********************)@.";
  Hashtbl.fold
    (fun t (s,f) (acc0, acc, decls) ->
       let st = {
	 jc_struct_info_name = s;
	 jc_struct_info_parent = None;
	 jc_struct_info_root = object_root;
	 jc_struct_info_fields = [];
	 jc_struct_info_variant = Some object_variant;
       }
       in
       let fi = { 
	 jc_field_info_name = f;
	 jc_field_info_final_name = f;
	 jc_field_info_tag = 0 (* TODO *);
	 jc_field_info_type = tr_type Loc.dummy_position t;
	 jc_field_info_root = object_root;
	 jc_field_info_struct = st;
	 jc_field_info_rep = false;
       }
       in
       st.jc_struct_info_fields <- [fi];
       Java_options.lprintf "%s@." st.jc_struct_info_name;
       Hashtbl.add array_struct_table t st;
       
       (* predicate non_null *)
       let non_null_pred = create_non_null_pred st in
       
       (* java_array_length fun *)
       let fi = create_java_array_length_fun st in
       let vi =
         Jc_pervasives.var (JCTpointer(JCtag st,Some num_zero,Some num_minus_one)) "x" in
       let vie = mkvar ~name:(var_name vi) () in
       let result_var = mkvar ~name:"\\result" () in
       let spec = [
         mkbehavior
           ~name: "non_null"
	   ~assigns:(Loc.dummy_position,[])
           ~ensures:
           (mkand ~list:[
              mkeq
                ~expr1: result_var
                ~expr2: (plus_one (mkoffset_max ~expr: vie ()))
                ();
              mkbinary ~op:`Bge
                ~expr1: result_var
                ~expr2: zero
                ();
              mkbinary ~op:`Ble
                ~expr1: result_var
                ~expr2: maxint
                ();
            ] ())
           ();
       ] in
       (* non_null fun & pred *)
       let non_null_fi = create_non_null_fun st in
       let non_null_spec = [
         mkbehavior
           ~name: "normal"
           ~ensures:
           (mkif
              ~condition: result_var
              ~expr_then:
              (mkbinary ~op:`Bge
                 ~expr1: (mkoffset_max ~expr:vie ())
                 ~expr2: (mkint ~value:(-1) ())
                 ())
              ~expr_else: (mkeq ~expr1:vie ~expr2:(mknull ()) ())
              ())
           ();
       ] in
       let args = [ptype_of_type vi.jc_var_info_type, var_name vi] in
       (mklogic_def
          ~name: non_null_pred.jc_logic_info_name
          ~labels: [Jc_env.LabelHere]
	  ~params: args
          ~body:
          (mkbinary
             ~expr1: (mkoffset_max ~expr:vie ())
             ~op:`Bge
             ~expr2: (mkint ~value:(-1) ())
             ())
          ()) :: acc0,
       (mktag_def
          ~name:st.jc_struct_info_name
          ~super:"Object"
	  ~fields:
          (List.map begin fun fi ->
             fi.jc_field_info_rep,
             ptype_of_type fi.jc_field_info_type,
             fi_name fi
           end st.jc_struct_info_fields)
          ()) :: acc,
       (mkfun_def
          ~result_type: (ptype_of_type fi.jc_fun_info_result.jc_var_info_type)
	  ~name: (new identifier fi.jc_fun_info_name)
          ~params: args
          ~clauses: spec
          ()) :: 
	 (mkfun_def
            ~result_type:
            (ptype_of_type non_null_fi.jc_fun_info_result.jc_var_info_type)
            ~name: (new identifier non_null_fi.jc_fun_info_name)
            ~params: args
            ~clauses: non_null_spec
            ()) :: decls)
    Java_analysis.array_struct_table
    ([], [
       mktag_def ~name:"interface" ();
       mkvariant_type_def
         ~name:"interface"
         ~tags:[ new identifier "interface" ]
         ();
       mkvariant_type_def
         ~name:"Object"
         ~tags:[ new identifier "Object" ]
         ();
     ], decls)
    

(*****************

 locations

***************)

let rec location_set logic_label t =
  match t.java_term_node with
      | JTlit l -> assert false (* TODO *)
      | JTun(t,op,e1) -> assert false (* TODO *)
      | JTbin(e1,t,op,e2) -> assert false (* TODO *)
      | JTapp (_, _) -> assert false (* TODO *)
      | JTvar vi ->
          mkvar ~name:(var_name (get_var vi)) ()
      | JTfield_access(t,fi) -> 
	  begin match logic_label with
	    | None -> assert false
	    | Some lab ->
                let _ = tr_logic_label lab in
                mkderef
                  ~expr: (location_set logic_label t)
                  ~field: (fi_name (get_field fi))
                  ()
	  end
      | JTstatic_field_access(ci,fi) ->
	  mkvar ~name:(var_name (get_static_var fi)) ()
      | JTarray_length(t) -> assert false (* TODO *)
      | JTarray_access(t1,t2) -> 
	  begin
	    match t1.java_term_type with
	      | JTYarray (_, ty) ->
		  let st = get_array_struct t1.java_term_loc ty in
		  let t1' = location_set logic_label t1 in
		  let t2' = term t2 in
		  let shift = mkrange ~locations:t1' ~left:t2' ~right:t2' () in
		  begin match logic_label with
		    | None -> assert false
		    | Some lab ->
                        let _ = tr_logic_label lab in
                        let fi = List.hd st.jc_struct_info_fields in
                        mkderef
                          ~expr: shift
                          ~field: (fi_name fi)
                          ()
		  end
	      | _ -> assert false
	  end
      | JTarray_range(t1,t2,t3) -> 
	  begin
	    match t1.java_term_type with
	      | JTYarray (_, ty) ->
		  let st = get_array_struct t1.java_term_loc ty in
		  let t1' = location_set logic_label t1 in
		  let t2' = Option_misc.map term t2 in
		  let t3' = Option_misc.map term t3 in
		  let shift = mkrange ~locations:t1' ?left:t2' ?right:t3' () in
		  begin match logic_label with
		    | None -> assert false
		    | Some lab ->
                        let fi = List.hd st.jc_struct_info_fields in
                        mkderef
                          ~expr: shift
                          ~field: (fi_name fi)
                          ()
		  end
	      | _ -> assert false
	  end
(*
      | JTold t -> assert false (* TODO *)
*)
      | JTat _ -> assert false (* TODO, maybe change logic_label ? *)
      | JTcast(ty,t) -> assert false (* TODO *)


let location logic_label t =
  match t.java_term_node with
      | JTlit l -> assert false (* TODO *)
      | JTun(t,op,e1) -> assert false (* TODO *)
      | JTbin(e1,t,op,e2) -> assert false (* TODO *)
      | JTapp (_, _) -> assert false (* TODO *)
      | JTvar vi ->
          mkvar ~name:(var_name (get_var vi)) ()
      | JTfield_access(t,fi) -> 
	  begin match logic_label with
	    | None -> assert false
	    | Some lab ->
                let _ = tr_logic_label lab in
                mkderef
                  ~expr: (location_set logic_label t)
                  ~field: (fi_name (get_field fi))
                  ()
	  end
      | JTstatic_field_access(ci,fi) ->
	  mkvar ~name:(var_name (get_static_var fi)) ()
      | JTarray_length(t) -> assert false (* TODO *)
      | JTarray_access(t1,t2) -> 
	  begin
	    match t1.java_term_type with
	      | JTYarray (_, ty) ->
		  let st = get_array_struct t1.java_term_loc ty in
		  let t1' = location_set logic_label t1 in
		  let t2' = term t2 in
		  let shift = mkrange ~locations:t1' ~left:t2' ~right:t2' () in
		  begin match logic_label with
		    | None -> assert false
		    | Some lab ->
                        let _ = tr_logic_label lab in
                        let fi = List.hd st.jc_struct_info_fields in
                        mkderef
                          ~expr: shift
                          ~field: (fi_name fi)
                          ()
		  end
	      | _ -> assert false
	  end
      | JTarray_range(t1,t2,t3) -> 
	  begin
	    match t1.java_term_type with
	      | JTYarray (_, ty) ->
		  let st = get_array_struct t1.java_term_loc ty in
		  let t1' = location_set logic_label t1 in
		  let t2' = Option_misc.map term t2 in
		  let t3' = Option_misc.map term t3 in
		  let shift = mkrange ~locations:t1' ?left:t2' ?right:t3' () in
		  begin match logic_label with
		    | None -> assert false
		    | Some lab ->
                        let _ = tr_logic_label lab in
                        let fi = List.hd st.jc_struct_info_fields in
                        mkderef
                          ~expr: shift
                          ~field: (fi_name fi)
                          ()
		  end
	      | _ -> assert false
	  end
(*
      | JTold t -> assert false (* TODO *)
*)
      | JTat _ -> assert false (* TODO, maybe change logic_label ? *)
      | JTcast(ty,t) -> assert false (* TODO *)
  

let un_op op: [> Jc_ast.unary_op] =
  match op with
    | Uminus -> `Uminus
    | Ucompl -> `Ubw_not
    | Unot -> `Unot
    | Uplus -> assert false (* TODO *)

let bin_op op: [> Jc_ast.bin_op] =
  match op with
    | Badd -> `Badd
    | Bmod -> `Bmod
    | Bdiv -> `Bdiv
    | Bmul -> `Bmul
    | Bsub -> `Bsub
    | Biff -> assert false
    | Bor -> `Blor
    | Band -> `Bland
    | Bimpl -> assert false 
    | Bgt -> `Bgt
    | Bne -> `Bneq
    | Beq -> `Beq
    | Bge -> `Bge
    | Ble -> `Ble
    | Blt -> `Blt
    | Basr -> `Barith_shift_right
    | Blsr -> `Blogical_shift_right
    | Blsl -> `Bshift_left
    | Bbwxor -> `Bbw_xor
    | Bbwor -> `Bbw_or
    | Bbwand -> `Bbw_and

let incr_op op: [> pm_unary_op] =
  match op with
    | Preincr -> `Uprefix_inc
    | Predecr -> `Uprefix_dec
    | Postincr -> `Upostfix_inc
    | Postdecr -> `Upostfix_dec

let int_cast loc t e =
  if !Java_options.ignore_overflow ||
    match t with
      | JTYbase Tint -> false
      | _ -> true
  then e else
    mkcast
      ~loc
      ~expr: e
      ~typ: int_range.jc_enum_info_name
      ()

let rec expr ?(reg=false) e =
  let reg = ref reg in
  let e' =
    match e.java_expr_node with
      | JElit l ->
          mkconst ~const:(lit l) ()
      | JEincr_local_var(op,v) -> 
	  reg := true;
          mkunary
            ~op: (incr_op op)
            ~expr: (mkvar ~name:(var_name (get_var v)) ())
            ()
      | JEincr_field(op,e1,fi) -> 
	  reg := true;
          mkincr_heap
            ~op: (incr_op op)
            ~expr: (expr e1)
            ~field: (fi_name (get_field fi))
            ()
      | JEincr_array (op, e1, e2) ->
	  begin
	    match e1.java_expr_type with
	      | JTYarray (_, ty) ->
		  let st = get_array_struct e1.java_expr_loc ty in
		  let e1' = expr e1 in
		  let shift = mkshift
                    ~loc:e.java_expr_loc
                    ~expr:e1'
                    ~offset:(expr e2)
                    ()
                  in
		  let fi = (List.hd st.jc_struct_info_fields) in
                  mkincr_heap
                    ~op: (incr_op op)
                    ~expr: shift
                    ~field: (fi_name fi)
                    ()
	      | _ -> assert false
	  end
      | JEun (op, e1) -> 
	  let e1 = expr e1 in
	  reg := true;	  
	  int_cast e.java_expr_loc e.java_expr_type
            (mkunary ~op:(un_op op) ~expr:e1 ())
      | JEbin (e1, op, e2) (* case e1 == null *)
	  when op = Beq && e2.java_expr_node = JElit Null ->
	  let e = expr e1 in
	  begin
            let st = match e1.java_expr_type with
	      | JTYclass _ | JTYinterface _ -> object_root
	      | JTYarray (_,t) -> get_array_struct e1.java_expr_loc t
	      | _ -> assert false
            in
            mknot
              ~expr:
              (mkapp
                 (* Romain: pourquoi non_null_fun et pas null_fun ? *)
                 ~fun_name: (non_null_fun st).jc_fun_info_name
                 ~args: [e]
                 ())
              ()
	  end
      | JEbin (e1, op, e2) (* case e1 != null *)
	  when op = Bne && e2.java_expr_node = JElit Null ->
	  let e = expr e1 in
	  begin
            let st = match e1.java_expr_type with
	      | JTYclass _ | JTYinterface _ -> object_root
	      | JTYarray (_,t) -> get_array_struct e1.java_expr_loc t
	      | _ -> assert false
            in
            mknot
              ~expr:
              (mkapp
                 ~fun_name: (non_null_fun st).jc_fun_info_name
                 ~args: [e]
                 ())
              ()
	  end
      | JEbin (e1, op, e2) ->
	  let e1 = expr e1 and e2 = expr e2 in
	  reg := true;
	  int_cast e.java_expr_loc e.java_expr_type
            (mkbinary ~expr1:e1 ~op:(bin_op op) ~expr2:e2 ())
      | JEif (e1,e2,e3) -> 
          mkif
            ~condition: (expr e1)
            ~expr_then: (expr e2)
            ~expr_else: (expr e3)
            ()
      | JEvar vi ->
          mkvar ~name:(var_name (get_var vi)) ()
      | JEstatic_field_access(ci,fi) ->
	  mkvar ~name:(var_name (get_static_var fi)) ()
      | JEfield_access(e1,fi) -> 
	  reg := true;
	  mkderef ~expr:(expr e1) ~field:(fi_name (get_field fi)) ()
      | JEarray_length e -> 
	  begin
	    match e.java_expr_type with
	      | JTYarray (_, ty) ->
		  let st = get_array_struct e.java_expr_loc ty in
		  reg := true;
		  mkapp
                    ~fun_name:(java_array_length_fun st).jc_fun_info_name
                    ~args:[expr e]
                    ()
	      | _ -> assert false
	  end
      | JEarray_access(e1,e2) -> 
	  begin
	    match e1.java_expr_type with
	      | JTYarray (_, ty) ->
		  let st = get_array_struct e1.java_expr_loc ty in
		  let e1' = expr e1 in
		  let shift =
                    mkshift
                      ~loc: e.java_expr_loc
                      ~expr: e1'
                      ~offset: (expr e2)
                      ()
		  in
		  reg := true;
                  mkderef
                    ~expr: shift
                    ~field: (fi_name (List.hd st.jc_struct_info_fields))
                    ()
	      | _ -> assert false
	  end
      | JEassign_local_var(vi,e) ->
          mkassign
            ~location: (mkvar ~name:(var_name (get_var vi)) ())
            ~value: (expr e)
            ()
      | JEassign_local_var_op(vi,op,e) ->
	  reg := true;
          mkassign
            ~location: (mkvar ~name:(var_name (get_var vi)) ())
            ~op: (bin_op op)
            ~value: (expr e)
            ()
      | JEassign_field(e1,fi,e2) ->
	  reg := true;
          mkassign
            ~location: (expr e1)
            ~field: (fi_name (get_field fi))
            ~value: (expr e2)
            ()
      | JEassign_field_op(e1,fi,op,e2) ->
	  reg := true;
          mkassign
            ~location: (expr e1)
            ~field: (fi_name (get_field fi))
            ~op: (bin_op op)
            ~value: (expr e2)
            ()
      | JEassign_static_field (fi, e) ->
          mkassign
            ~location: (mkvar ~name:(var_name (get_static_var fi)) ())
            ~value: (expr e)
            ()
      | JEassign_static_field_op (fi, op, e) ->
	  reg := true;
          mkassign
            ~location: (mkvar ~name:(var_name (get_static_var fi)) ())
            ~op: (bin_op op)
            ~value: (expr e)
            ()
      | JEassign_array(e1,e2,e3) ->
	  reg := true;
	  begin
	    match e1.java_expr_type with
	      | JTYarray (_, ty) ->
		  let st = get_array_struct e1.java_expr_loc ty in
		  let e1' = expr e1 in
		  let shift =
                    mkshift
                      ~loc: e.java_expr_loc
                      ~expr: e1'
                      ~offset: (expr e2)
                      ()
		  in
		  let fi = (List.hd st.jc_struct_info_fields) in
		  let e3' = expr e3 in
                  mkassign
                    ~location: shift
                    ~field: (fi_name fi)
                    ~value: e3'
                    ()
	      | _ -> assert false
	  end
      | JEassign_array_op(e1,e2,op,e3) ->
	  begin
	    match e1.java_expr_type with
	      | JTYarray (_, ty) ->
		  let st = get_array_struct e1.java_expr_loc ty in
		  let e1' = expr e1 in
		  let shift =
                    mkshift
                      ~loc: e.java_expr_loc
                      ~expr: e1'
                      ~offset: (expr e2)
                      ()
		  in
		  let fi = (List.hd st.jc_struct_info_fields) in
		  let e3' = expr e3 in
                  mkassign
                    ~location: shift
                    ~field: (fi_name fi)
                    ~op: (bin_op op)
                    ~value: e3'
                    ()
	      | _ -> assert false
	  end
      | JEcall(e1,mi,args) -> 
	  reg := true;
          mkapp
            ~fun_name:
            (get_fun e.java_expr_loc mi.method_info_tag).jc_fun_info_name
            ~args: (List.map expr (e1 :: args))
            ()
      | JEconstr_call (e1, ci, args) -> 
	  reg := true;
          mkapp
            ~fun_name:
            (get_fun e.java_expr_loc ci.constr_info_tag).jc_fun_info_name
            ~args: (List.map expr (e1 :: args))
            ()
      | JEstatic_call(mi,args) -> 
	  reg := true;
          mkapp
            ~fun_name:
            (get_fun e.java_expr_loc mi.method_info_tag).jc_fun_info_name
            ~args: (List.map expr args)
            ()
      | JEnew_array(ty,[e1]) ->
	  let si = get_array_struct e.java_expr_loc ty in
          mkalloc
            ~count: (expr e1)
            ~typ: si.jc_struct_info_name
            ()
      | JEnew_array(ty,_) ->
	  assert false (* TODO *)
      | JEnew_object(ci,args) ->
	  let si = get_class ci.constr_info_class.class_info_name in
	  let ty = JCTpointer(JCtag si, Some num_zero, Some num_zero) in
	  let this = Jc_pervasives.var ~formal:true ty "this" in
	  let tt = Jc_pervasives.var ~formal:true Jc_pervasives.unit_type "tt" in
	  let args =
            (mkvar ~name:(var_name this) ()) :: List.map expr args in
          mklet
            ~var: (var_name this)
            ~typ: (ptype_of_type this.jc_var_info_type)
            ~init: (mkalloc ~typ:si.jc_struct_info_name ())
            ~body:
            (mklet
               ~var: (var_name tt)
               ~typ: (ptype_of_type tt.jc_var_info_type)
               ~init:
               (mkapp
                  ~fun_name:
                  (get_fun e.java_expr_loc ci.constr_info_tag).jc_fun_info_name
                  ~args: args
                  ())
               ~body: (mkvar ~name:(var_name this) ())
               ())
            ()
      | JEcast(ty,e1) ->
	  begin
	    match ty with
	      | JTYbase t -> 
		  if !Java_options.ignore_overflow then expr e1 else begin
                    reg := true;
                    mkcast
                      ~expr: (expr e1)
                      ~typ: (get_enum_info t).jc_enum_info_name
                      ()
                  end
	      | JTYclass(_,ci) ->
		  let st = get_class ci.class_info_name in
		  reg := true;	    
                  mkcast ~expr:(expr e1) ~typ:st.jc_struct_info_name ()
	      | JTYinterface ii -> 
		  begin
		    match e1.java_expr_type with
		      | JTYinterface _ -> expr e1
		      | _ -> assert false (* TODO *)
(*
		  eprintf "Warning: cast to interface '%s' ignored.@."
		    ii.interface_info_name;
		    (expr e1).jc_texpr_node
*)
		  end
	      | JTYarray (_, ty) ->
		  let st = get_array_struct e.java_expr_loc ty in
		  reg := true;	    
                  mkcast ~expr:(expr e1) ~typ:st.jc_struct_info_name ()
	      | JTYnull | JTYlogic _ -> assert false 
	  end
      | JEinstanceof(e,ty) ->
	  begin
	    match ty with
	      | JTYclass(_,ci) ->
		  let st = get_class ci.class_info_name in
                  mkinstanceof ~expr:(expr e) ~typ:st.jc_struct_info_name ()
	      | _ -> assert false (* TODO *)
	  end

  in
  let _ = if !reg then reg_loc e.java_expr_loc else "" in
  new pexpr ~loc: e.java_expr_loc e'#node

let initialiser e =
  match e with
    | JIexpr e -> expr ~reg:true e
    | _ -> assert false (* TODO *)

(*
let dummy_loc_statement s =
  { jc_tstatement_loc = Loc.dummy_position; 
    jc_tstatement_node = s }

let make_block l =
  match l with
    | [s] -> s
    | _ -> dummy_loc_statement (JCTSblock l)
*)

let reg_assertion a = 
  let a' = assertion ~reg:1 a  in
  let _ = reg_loc a.java_assertion_loc in
  a'

let reg_assertion_option a =
  match a with
    | None -> mkboolean ~value:true ()
    | Some a -> reg_assertion a

let loop_annot inv dec =
  let invariant = reg_assertion inv in
  let variant = Option_misc.map begin fun t ->
    let t' = term t in
    let _ = reg_loc t.java_term_loc in
    t'
  end dec
  in
  invariant, variant

let rec statement s =
  let s' =
    match s.java_statement_node with
      | JSskip ->
          mkvoid ()
      | JSbreak label -> 
          mkbreak ?label ()
      | JSreturn_void ->
          mkreturn ()
      | JSreturn e -> 
          let _ = tr_type e.java_expr_loc e.java_expr_type in
	  mkreturn ~expr:(expr e) ()
      | JSthrow e ->
          mkthrow
            ~exn: (exn_name (get_exception e.java_expr_type))
            ~argument: (expr e)
            ()
      | JSblock l ->
          mkblock ~exprs:(List.map statement l)	()
      | JSvar_decl (vi, init, s) -> 
	  let vi = create_var s.java_statement_loc vi in
          mklet
            ~typ: (ptype_of_type vi.jc_var_info_type)
            ~var: (var_name vi)
            ?init: (Option_misc.map initialiser init)
            ~body: (statement s)
            ()
      | JSif (e, s1, s2) ->
          mkif
            ~condition: (expr e)
            ~expr_then: (statement s1)
            ~expr_else: (statement s2)
            ()
      | JSwhile(e,inv,dec,s) ->
	  let (invariant, variant) = loop_annot inv dec in
          mkwhile ~invariant ?variant ~condition:(expr e) ~body:(statement s) ()
      | JSfor (el1, e, inv, dec, el2, body) ->
	  let (invariant, variant) = loop_annot inv dec in
          mkfor
            ~inits: (List.map expr el1)
            ~condition: (expr e)
            ~updates: (List.map expr el2)
            ~body: (statement body)
            ~invariant
            ?variant
            ~loc: s.java_statement_loc
            ()
      | JSfor_decl(decls,e,inv,dec,sl,body) ->
	  let decls = List.map begin fun (vi, init) ->
	    create_var s.java_statement_loc vi,
            Option_misc.map initialiser init
          end decls in
	  let (invariant, variant) = loop_annot inv dec in
          (* TODO: Now that we produce parsed AST we could put inits in ~init *)
	  let res =
	    List.fold_right
	      (fun (vi,init) acc ->
                 mklet
                   ~typ: (ptype_of_type vi.jc_var_info_type)
                   ~var: (var_name vi)
                   ?init
                   ~body: acc
                   ())
	      decls
              (mkfor
                 ~loc: s.java_statement_loc
                 ~condition: (expr e)
                 ~updates: (List.map expr sl)
                 ~invariant
                 ?variant
                 ~body: (statement body)
                 ())
	  in mkblock ~exprs:[res] ()
      | JSexpr e -> expr e
      | JSassert(id,e) -> 
	  let _ = (* TODO: use it *)
	    match id with
	      | None -> reg_loc e.java_assertion_loc
	      | Some id -> reg_loc ~id e.java_assertion_loc
	  in
	  let e' = reg_assertion e in
          mkassert ~expr:e' ()
      | JSswitch(e,l) -> 
          mkswitch ~expr:(expr e) ~cases:(List.map switch_case l) ()
      | JStry(s1, catches, finally) ->
          mktry
            ~expr: (block s1)
            ~catches:
            (List.map 
	       (fun (vi,s2) ->
		  let e = get_exception vi.java_var_info_type in
		  let vti = create_var s.java_statement_loc vi in
                  mkcatch
                    ~exn: (exn_name e)
                    ~name: (var_name vti)
                    ~body: (block s2)
                    ())
	       catches)
            ~finally:
            (mkblock
               ~exprs:
               (Option_misc.fold (fun s acc -> List.map statement s) finally [])
               ())
            ()
  in new pexpr ~loc:s.java_statement_loc s'#node

and statements l = List.map statement l

and block l = mkblock ~exprs:(statements l) ()

and switch_case (labels, b) =
  (List.map switch_label labels, block b)

and switch_label = function
  | Java_ast.Default -> None
  | Java_ast.Case e -> Some (expr e)

let behavior (id,assumes,throws,assigns,ensures) =
  mkbehavior
    ~loc: (fst id)
    ~name: (snd id)
    ?throws:
    (Option_misc.map
       (fun ci -> exn_name (get_exception (JTYclass(false,ci))))
       throws)
    ?assigns:
    (Option_misc.map
       (fun (loc,a) ->
          mkassigns
            ~loc
            ~locations:(List.map (location (Some LabelPre)) a)
            ())
       assigns)
    ?assumes: (Option_misc.map assertion assumes)
    ~ensures: (reg_assertion ensures)
    ()

let true_assertion = mkboolean ~value:true ()

let tr_method mi req behs b acc =
  let java_params = mi.method_info_parameters in
  let params =
    List.map (fun (p, _) -> create_var Loc.dummy_position p) java_params in
  let params =
    match mi.method_info_has_this with
      | None -> params
      | Some vi -> (create_var Loc.dummy_position vi) :: params
  in
  let params = List.map
    (fun vi -> ptype_of_type vi.jc_var_info_type, (var_name vi))
    params
  in
  let return_type = 
    Option_misc.map 
      (fun vi -> 	
 	 let _nvi = create_var Loc.dummy_position vi in 
 	 vi.java_var_info_type) 
      mi.method_info_result 
  in
  let behaviors = List.map behavior behs in
  let nfi = 
    create_fun Loc.dummy_position 
      mi.method_info_tag mi.method_info_result 
      mi.method_info_trans_name mi.method_info_parameters
  in
  let body = Option_misc.map block b in
  let _ = 
    reg_loc ~id:nfi.jc_fun_info_name 
      ~name:("Method " ^ mi.method_info_name)
      mi.method_info_loc
  in
  let requires =
    mkrequires (reg_assertion_option req)
  in
  let result_type = (* need the option monad... *)
    Option_misc.map
    ptype_of_type
      (Option_misc.map
         (tr_type Loc.dummy_position)
         return_type)
  in
  let def = mkfun_def
    ?result_type
    ~name: (new identifier nfi.jc_fun_info_name)
    ~params
    ~clauses: (requires::behaviors)
    ?body
    ()
  in def::acc
      
let tr_constr ci req behs b acc =
  let params = List.map
    (fun (vi, _) -> create_var Loc.dummy_position vi)
    ci.constr_info_parameters 
  in
  let this =
    match ci.constr_info_this with
      | None -> assert false
      | Some vi -> (create_var Loc.dummy_position vi) 
  in
  let nfi = 
    create_fun Loc.dummy_position ci.constr_info_tag None
      ci.constr_info_trans_name ci.constr_info_parameters
  in
  let body = statements b
(*
@ 
    [dummy_loc_statement (JCTSreturn(this.jc_var_info_type,
				     dummy_loc_expr 
				       this.jc_var_info_type
				       (JCTEvar this)))] 
*)
  in
(* NO: TODO 
  let body = 
    dummy_loc_statement (JCTSdecl(this,None,make_block body))
  in
  *)
  let body = match body with
    | [] -> None
    | _ -> Some (mkblock ~exprs:body ())
  in
  let _ = 
    reg_loc ~id:nfi.jc_fun_info_name 
      ~name:("Constructor of class "^ci.constr_info_class.class_info_name)
      ci.constr_info_loc 
  in
  let params = List.map
    (fun vi -> ptype_of_type vi.jc_var_info_type, var_name vi)
    (this :: params)
  in
  let requires = mkrequires (reg_assertion_option req) in
  let behaviors = List.map behavior behs in
  let def = mkfun_def
    ~name: (new identifier nfi.jc_fun_info_name)
    ~params
    ?body
    ~clauses: (requires::behaviors)
    ()
  in def :: acc
	  
(*s axioms *)

let tr_axiom id is_axiom lab p acc =
  let def = mklemma_def
    ~name: id
    ~axiom: is_axiom
    ~labels: (List.map tr_logic_label lab)
    ~body: (assertion p)
    ()
  in def::acc

let default_label l =
  match l with
    | [l] -> Some l
    | _ -> None

let tr_non_null_logic_fun () =
  let si = get_class "Object" in
  let vi = Jc_pervasives.var
    (JCTpointer (JCtag si, Some num_zero, None)) "x" in
  let vit = mkvar ~name:(var_name vi) () in
  let offset_maxt = mkoffset_max ~expr:vit () in
  let offset_maxa = mkeq ~expr1:offset_maxt ~expr2:zero () in
  let non_null_pred = create_non_null_pred si in
  mklogic_def
    ~name: non_null_pred.jc_logic_info_name
    ~labels: [Jc_env.LabelHere]
    ~params: [ptype_of_type vi.jc_var_info_type, var_name vi]
    ~body: offset_maxa
    ()
      
let tr_logic_fun fi b acc =   
  let nfi = create_logic_fun Loc.dummy_position fi in
  let def_ =
    mklogic_def
      ?typ: (Option_misc.map ptype_of_type nfi.jc_logic_info_result_type)
      ~name: nfi.jc_logic_info_name
      ~labels: nfi.jc_logic_info_labels
      ~params:
      (List.map
         (fun vi -> ptype_of_type vi.jc_var_info_type, var_name vi)
         nfi.jc_logic_info_parameters)
  in
  let def = match b with
    | Java_typing.JAssertion a -> def_ ~body:(assertion a) ()
    | Java_typing.JTerm t -> def_ ~body:(term t) ()
    | Java_typing.JReads l ->
	let logic_label = default_label fi.java_logic_info_labels in
        def_ ~reads:(List.map (location logic_label) l) ()
  in (def::acc)

let tr_field type_name acc fi =
  let vi = create_static_var Loc.dummy_position type_name fi in
  let vi_ty = vi.jc_var_info_type in
  let fi_ty = fi.java_field_info_type in
  if fi.java_field_info_is_final then
    let logic_body, axiom_body = try
      let e = 
	Hashtbl.find Java_typing.field_initializer_table fi.java_field_info_tag
      in
      let values =
	Hashtbl.find Java_typing.final_field_values_table fi.java_field_info_tag
      in
      let get_value value = match fi_ty with
	| JTYarray (_,JTYbase t) | JTYbase t -> 
	    begin match t with
	      | Tshort | Tbyte | Tchar | Tint 
	      | Tlong | Tdouble | Tinteger -> 
		  JCCinteger (Num.string_of_num value)
	      | Tboolean -> 
		  let b = match Num.string_of_num value with
		    | "0" -> false
		    | "1" -> true
		    | _ -> assert false (* should never happen *)
		  in JCCboolean b
	      | Tfloat | Treal -> assert false (* TODO *) 
	      | Tunit -> assert false
	    end
	| JTYnull | JTYclass _ | JTYinterface _ | JTYarray _ | JTYlogic _ -> 
	    assert false
      in
      match e with
	| None ->
            None, None
	| Some (JIexpr e) ->
	    assert (List.length values = 1);
	    (* evaluated constant expressions are translated *)
            let t = term (term_of_expr e) in
	    Some (mkconst ~const:(get_value (List.hd values)) ~loc:t#loc ()),
	    None
	| Some (JIlist il) ->
	    let n = List.length il in
	    assert (List.length values = n);
	    let si = match vi_ty with
	      | JCTpointer (JCtag si, _, _) -> si
	      | _ -> assert false
	    in
	    let vit = mkvar ~name:(var_name vi) () in
	    let a =
              mkapp
                ~fun_name:
                (non_null_pred si.jc_struct_info_name).jc_logic_info_name
                ~args: [vit]
                ()
	    in
	    let a =
              mkand
                ~expr1: a
                ~expr2:
                (mkeq
                   ~expr1: (mkoffset_max ~expr:vit ())
                   ~expr2: (mkint ~value:(n-1) ())
                   ())
                ()
	    in
	    let fi' = List.hd si.jc_struct_info_fields in 
	    let a, _ = List.fold_left2 begin fun (acc, cpt) init n ->
	      match init with
		| JIexpr e ->
		    let _ = term (term_of_expr e) in (* Why not used? *)
                    mkand
                      ~expr1: acc
                      ~expr2:
                      (mkeq
                         ~expr1:
                         (mkderef
                            ~expr:
                            (mkshift
                               ~expr: vit
                               ~offset: (mkint ~value:cpt ())
                               ())
                            ~field: (fi_name fi')
                            ())
                         ~expr2: (mkconst ~const:(get_value n) ())
                         ())
                      (),
		    cpt + 1
		| JIlist _ -> assert false (* TODO / Not supported *)
            end (a, 0) il values in
	    None, Some a
    with Not_found ->
      Java_options.lprintf
        "Warning: final field '%s' of %a has no known value@."
	fi.java_field_info_name 
	Java_typing.print_type_name 
	fi.java_field_info_class_or_interface;
      None, None
    in
    let def1 =
      mklogic_var_def
        ~typ: (ptype_of_type vi_ty)
        ~name: (var_name vi) 
	?body:logic_body 
	()
    in
    let acc = def1 :: acc in
    let def2 = match axiom_body with
      | None -> []
      | Some a -> [
          mklemma_def
            ~name: (fi.java_field_info_name^"_values")
            ~axiom: true
            ~labels: [Jc_env.LabelHere]
            ~body: a
            ()
        ]
    in
    def2 @ acc
  else
    let e = 
      try match Hashtbl.find Java_typing.field_initializer_table 
	fi.java_field_info_tag with
	  | None -> None
	  | Some e -> Some (initialiser e)
      with Not_found -> None
    in
    let acc =
      (mkvar_def
         ~typ: (ptype_of_type vi_ty)
         ~name: (var_name vi)
         ?init: e
         ())::acc
    in
    acc

(* class *)

let tr_class ci acc0 acc =
  let (static_fields, fields) = 
    List.partition 
      (fun fi -> fi.java_field_info_is_static)
      ci.class_info_fields
  in
  let super =
    let superclass = Option_misc.map (fun ci -> ci.class_info_name)
      ci.class_info_extends in
      match superclass with 
	| None -> if ci.class_info_name = "Object" then None else Some "Object"
	| _ -> superclass
  in
  let acc = List.fold_left (tr_field ci.class_info_name) acc static_fields in
    (* create exceptions if subclass of Exception *)
    begin
      if ci.class_info_is_exception then
	ignore (create_exception 
		  (Some (tr_type Loc.dummy_position (JTYclass (false, ci))))
		  ci.class_info_name);
    end;
    let jc_fields = List.map (create_field Loc.dummy_position) fields in
      (* non_null fun & pred *)
    let si = get_class ci.class_info_name in
    let acc = 
      if ci.class_info_name = "Object" then
	let non_null_fi = create_non_null_fun si in
	let vi = Jc_pervasives.var
	  (JCTpointer (JCtag si, Some num_zero, None)) "x" in
	let result = Jc_pervasives.var Jc_pervasives.boolean_type "\\result" in
	let vit = mkvar ~name:(var_name vi) () in
	let offset_maxt = mkoffset_max ~expr:vit () in
	let offset_maxa = mkeq ~expr1:offset_maxt ~expr2:zero () in
        let non_null_spec = [
          mkbehavior
            ~name: "normal"
            ~ensures:
            (mkif
               ~condition: (mkvar ~name:(var_name result) ())
               ~expr_then: offset_maxa
               ~expr_else: (mkeq ~expr1:vit ~expr2:(mknull ()) ())
               ())
            ()
        ] in
        (mkfun_def
           ~result_type: (ptype_of_type Jc_pervasives.boolean_type)
           ~name: (new identifier non_null_fi.jc_fun_info_name)
           ~params: [ptype_of_type vi.jc_var_info_type, var_name vi]
           ~clauses: non_null_spec
           ()):: acc
      else acc
    in
    let fields = List.map begin function fi ->
      fi.jc_field_info_rep,
      ptype_of_type fi.jc_field_info_type,
      fi_name fi
    end jc_fields in
    (mktag_def
       ~name: ci.class_info_name
       ?super
       ~fields
       ())::acc0, acc
	
(* interfaces *)

let tr_interface ii acc = 
  let (static_fields, fields) = 
    List.partition 
      (fun fi -> fi.java_field_info_is_static)
      ii.interface_info_fields
  in
  let acc = 
    List.fold_left (tr_field ii.interface_info_name) acc static_fields 
  in
  let model_fields = List.map (create_field Loc.dummy_position) fields in
  acc

let tr_class_or_interface ti acc0 acc =
  match ti with
    | TypeClass ci -> 
	Java_options.lprintf "Creating JC structure for class '%s'@."
          ci.class_info_name;
	tr_class ci acc0 acc
    | TypeInterface ii -> 
	Java_options.lprintf "Handling interface '%s'@." ii.interface_info_name;
	(acc0, tr_interface ii acc)

let tr_invariants ci id invs decls =
  let invs =
    List.map
      (fun ((_, s), a) -> 
	 let vi = create_var Loc.dummy_position id in
	 new identifier s, var_name vi, assertion a)
      invs
  in
  List.map begin fun d -> match d#node with
    | JCDtag (s, so, fil, struct_invs) when s = ci.class_info_name ->
	mktag_def
          ~name:s
          ?super:so
          ~fields:fil
          ~invariants:(struct_invs @ invs)
          ()
    | _ -> d
  end decls

(* static invariants *)

let tr_static_invariant (s, a) = 
  mkglobal_inv_def ~name:s ~body:(assertion a) ()

(*
Local Variables: 
compile-command: "LC_ALL=C make -j -C .. bin/krakatoa.byte"
End: 
*)


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

(* $Id: jc_interp.ml,v 1.394 2008/12/09 09:14:18 marche Exp $ *)

open Jc_stdlib
open Jc_env
open Jc_envset
open Jc_region
open Jc_ast
open Jc_fenv

open Jc_name
open Jc_constructors
open Jc_pervasives
open Jc_separation
open Jc_struct_tools
open Jc_effect
open Jc_interp_misc
open Jc_invariants
open Jc_pattern

open Output
open Format
open Num
open Pp

(******************************************************************************)
(*                            source positioning                              *)
(******************************************************************************)

let pos_table = Hashtbl.create 97

let abs_fname f =
  if Filename.is_relative f then
    Filename.concat (Unix.getcwd ()) f 
  else f

type source_ref = 
    {
      in_mark: string; 
      pos: Loc.position 
    }

type gui_elem = 
    {
      out_mark: string; 
      kind: kind option; 
      name: string option; 
      beh: string option 
    }

let reg_pos sce gui =
  if gui.out_mark <> "" && StdHashtbl.mem Output.pos_table gui.out_mark then
    (* If GUI element already refered to in output table, do not 
     * reference it twice. This is the case in particular for generated 
     * annotations. *)
    gui.out_mark
  else
    (* Generate a new mark if not fixed in GUI element *)
    let mark = 
      if gui.out_mark = "" then 
	Jc_pervasives.new_label_name() 
      else gui.out_mark
    in
    let (n,f,l,b,e,k) = 
      if sce.in_mark <> "" && Hashtbl.mem Jc_options.pos_table sce.in_mark then
	(* If source location present in input table, copy information to
	 * output table. *)
	let (f,l,b,e,k,o) = Hashtbl.find Jc_options.pos_table sce.in_mark in
	let n =
	  try match List.assoc "name" o with
            | Rc.RCident s | Rc.RCstring s -> Some s
            | _ -> raise Not_found
	  with Not_found -> gui.name
	in
	(n,f,l,b,e,k)
      else
	(* By default, refer to the Jessie source file *)
	let b,e = sce.pos in
	let f = abs_fname b.Lexing.pos_fname in
	let l = b.Lexing.pos_lnum in
	let fc = b.Lexing.pos_cnum - b.Lexing.pos_bol in
	let lc = e.Lexing.pos_cnum - b.Lexing.pos_bol in
	(gui.name,f,l,fc,lc,None)
    in
    (* If present, always prefer new kind *)
    let k = match gui.kind with None -> k | Some k -> Some k in
    Hashtbl.replace pos_table mark (k,n,gui.beh,f,l,b,e);
    mark

let reg_check ?mark ?kind pos =
  let sce = 
    { in_mark = Option_misc.map_default (fun x -> x) "" mark; pos = pos; } 
  in
  let gui = { out_mark = ""; kind = kind; name = None; beh = None; } in
  reg_pos sce gui
      
let reg_decl ~in_mark ~out_mark ~name ~beh pos =
  let sce = { in_mark = in_mark; pos = pos; } in
  let gui =
    { out_mark = out_mark; kind = None; name = Some name; beh = Some beh; } 
  in
  ignore (reg_pos sce gui)

let make_check ?mark ?kind pos e' =
  let mark = reg_check ?mark ?kind pos in
  Label(mark,e')

let make_guarded_app ~mark kind pos f args =
  make_check ~mark ~kind pos (make_app f args)

let print_kind fmt k =
  fprintf fmt "%s"
    (match k with
       | Pack -> "Pack"
       | Unpack -> "Unpack"
       | DivByZero -> "DivByZero"
       | AllocSize -> "AllocSize"
       | UserCall -> "UserCall"
       | PointerDeref -> "PointerDeref"
       | IndexBounds -> "IndexBounds"
       | DownCast -> "DownCast"
       | ArithOverflow -> "ArithOverflow"
    )

let print_locs fmt =
  Hashtbl.iter 
    (fun id (kind,name,beh,f,l,b,e) ->
       fprintf fmt "[%s]@\n" id;
       Option_misc.iter
         (fun k -> fprintf fmt "kind = %a@\n" print_kind k) kind;
       Option_misc.iter
         (fun n -> fprintf fmt "name = \"%s\"@\n" n) name;
       Option_misc.iter
	 (fun b -> fprintf fmt "behavior = \"%s\"@\n" b) beh;
       fprintf fmt "file = \"%s\"@\n" (String.escaped f);
       fprintf fmt "line = %d@\n" l;
       fprintf fmt "begin = %d@\n" b;
       fprintf fmt "end = %d@\n@\n" e)
    pos_table


(******************************************************************************)
(*                                 Operators                                  *)
(******************************************************************************)

let native_operator_type op = match snd op with
  | `Unit -> Jc_pervasives.unit_type
  | `Boolean -> Jc_pervasives.boolean_type
  | `Integer -> Jc_pervasives.integer_type
  | `Real -> Jc_pervasives.real_type
  | `Double -> Jc_pervasives.double_type
  | `Float -> Jc_pervasives.float_type

let unary_op: expr_unary_op -> string = function
  | `Uminus, `Integer -> "neg_int"
  | `Uminus, `Real -> "neg_real"
(*
  | `Usqrt, `Real  -> "sqrt_real"
  | `Usqrt,`Double -> "sqrt_gen_float"
  | `Usqrt, `Float -> "sqrt_gen_float"
*)
  | `Unot, `Boolean -> "not"
  | `Ubw_not, `Integer -> "bw_compl"
  | _ -> assert false (* not proper type *)

let term_unary_op: expr_unary_op -> string = function
  | `Uminus, `Integer -> "neg_int"
  | `Uminus, `Real -> "neg_real"
  | `Unot, `Boolean -> "bool_not"
  | `Ubw_not, `Integer -> "bw_compl"
  | _ -> assert false (* not proper type *)

let float_operator f =
  match f with
    | `Badd -> "add_gen_float"
    | `Bsub -> "sub_gen_float"
    | `Bmul -> "mul_gen_float"
    | `Bdiv -> "div_gen_float"
    | `Bmod -> "mod_gen_float"

let float_format f =
  match f with
    | `Double -> "Double"
    | `Float -> "Single"

let rounding_mode = function
    | Round_nearest_even -> "nearest_even" 
    | Round_to_zero -> "to_zero"
    | Round_up -> "up"
    | Round_down -> "down"
    | Round_nearest_away -> "nearest_away"

  
let bin_op: expr_bin_op -> string = function
    (* integer *)
  | `Bgt, `Integer -> "gt_int_"
  | `Blt, `Integer -> "lt_int_"
  | `Bge, `Integer -> "ge_int_"
  | `Ble, `Integer -> "le_int_"
  | `Beq, `Integer -> "eq_int_"
  | `Bneq, `Integer -> "neq_int_"
  | `Badd, `Integer -> "add_int"
  | `Bsub, `Integer -> "sub_int"
  | `Bmul, `Integer -> "mul_int"
  | `Bdiv, `Integer -> 
      if safety_checking () then "div_int_" else "div_int" 
  | `Bmod, `Integer -> 
      if safety_checking () then "mod_int_" else "mod_int"
      (* pointer *)
  | `Beq, `Pointer -> 
      if safety_checking () then "eq_pointer" else "safe_eq_pointer"
  | `Bneq, `Pointer -> 
      if safety_checking () then "neq_pointer" else "safe_neq_pointer"
  | `Bsub, `Pointer -> 
      if safety_checking () then "sub_pointer_" else "safe_sub_pointer_"
      (* real *)
  | `Bgt, `Real -> "gt_real_"
  | `Blt, `Real -> "lt_real_"
  | `Bge, `Real -> "ge_real_"
  | `Ble, `Real -> "le_real_"
  | `Beq, `Real -> "eq_real_"
  | `Bneq, `Real -> "neq_real_"
  | `Bgt, (`Double | `Float) -> "gt_gen_float"
  | `Blt, (`Double | `Float) -> "lt_gen_float"
  | `Bge, (`Double | `Float) -> "ge_gen_float"
  | `Ble, (`Double | `Float) -> "le_gen_float"
  | `Beq, (`Double | `Float) -> "eq_gen_float"
  | `Bneq, (`Double | `Float) -> "neq_gen_float"
  | `Badd, `Real -> "add_real"
  | `Bsub, `Real -> "sub_real"
  | `Bmul, `Real -> "mul_real"
  | `Bdiv, `Real -> 
      if safety_checking () then "div_real_" else "div_real"
      (* bool *)
  | `Beq, `Boolean -> "eq_bool_"
  | `Bneq, `Boolean -> "neq_bool_"
      (* bitwise *)
  | `Bbw_and, `Integer -> "bw_and"
  | `Bbw_or, `Integer -> "bw_or"
  | `Bbw_xor, `Integer -> "bw_xor"
  | `Bbw_and, `Boolean -> "bool_and"
  | `Bbw_or, `Boolean -> "bool_or"
  | `Bbw_xor, `Boolean -> "bool_xor"
      (* shift *)
  | `Bshift_left, `Integer -> "lsl"
  | `Blogical_shift_right, `Integer -> "lsr"
  | `Barith_shift_right, `Integer -> "asr"
  | `Bland, _ -> assert false
  | `Blor, _ -> assert false
  | `Bconcat, _ -> "string_concat"
  | op, opty ->
      Jc_typing.typing_error Loc.dummy_position
        "Can't use operator %s with type %s in expressions"
        (string_of_op op) (string_of_op_type opty)

let term_bin_op: term_bin_op -> string = function
    (* integer *)
  | `Bgt, `Integer -> "gt_int_bool"
  | `Blt, `Integer -> "lt_int_bool"
  | `Bge, `Integer -> "ge_int_bool"
  | `Ble, `Integer -> "le_int_bool"
  | `Beq, `Integer -> "eq_int_bool"
  | `Bneq, `Integer -> "neq_int_bool"
  | `Badd, `Integer -> "add_int"
  | `Bsub, `Integer -> "sub_int"
  | `Bmul, `Integer -> "mul_int"
  | `Bdiv, `Integer -> "div_int"
  | `Bmod, `Integer -> "mod_int"
      (* pointer *)
  | `Beq, `Pointer -> "eq_pointer_bool"
  | `Bneq, `Pointer -> "neq_pointer_bool"
  | `Bsub, `Pointer -> "sub_pointer"
      (* logic *)
  | `Beq, `Logic -> "eq"
  | `Bneq, `Logic -> "neq"
      (* real *)
  | `Bgt, `Real -> "gt_real_bool"
  | `Blt, `Real -> "lt_real_bool"
  | `Bge, `Real -> "ge_real_bool"
  | `Ble, `Real -> "le_real_bool"
  | `Beq, `Real -> "eq_real_bool"
  | `Bneq, `Real -> "neq_real_bool"
  | `Badd, `Real -> "add_real"
  | `Bsub, `Real -> "sub_real"
  | `Bmul, `Real -> "mul_real"
  | `Bdiv, `Real -> "div_real"
      (* bool *)
(*  | `Beq_bool, `Boolean -> "eq_bool"
  | `Bneq_bool, `Boolean -> "neq_bool"*)
      (* bitwise *)
  | `Bbw_and, `Integer -> "bw_and"
  | `Bbw_or, `Integer -> "bw_or"
  | `Bbw_xor, `Integer -> "bw_xor"
  | `Bshift_left, `Integer -> "lsl"
  | `Blogical_shift_right, `Integer -> "lsr"
  | `Barith_shift_right, `Integer -> "asr"
      (* logical *)
  | `Blor, `Boolean -> "bool_or"
  | `Bland, `Boolean ->  "bool_and"
  | `Biff, _ | `Bimplies, _ -> 
      assert false (* TODO *)
  | op, opty ->
      Jc_typing.typing_error Loc.dummy_position
        "Can't use operator %s with type %s in terms"
        (string_of_op op) (string_of_op_type opty)

let pred_bin_op: pred_bin_op -> string = function
    (* integer *)
  | `Bgt, `Integer -> "gt_int"
  | `Blt, `Integer -> "lt_int"
  | `Bge, `Integer -> "ge_int"
  | `Ble, `Integer -> "le_int"
  | `Beq, `Integer -> "eq_int"
  | `Bneq, `Integer -> "neq_int"
      (* pointer *)
  | `Beq, (`Pointer | `Logic) -> "eq"
  | `Bneq, (`Pointer | `Logic) -> "neq"
      (* real *)
  | `Beq, `Real -> "eq_real"
  | `Bneq, `Real -> "neq_real"
  | `Bgt, `Real -> "gt_real"
  | `Blt, `Real -> "lt_real"
  | `Bge, `Real -> "ge_real"
  | `Ble, `Real -> "le_real"
      (* logical *)
  | `Blor, `Boolean -> "bor"
  | `Bland, `Boolean -> "band"
  | `Biff, `Boolean
  | `Bimplies, `Boolean -> assert false (* TODO *)
      (* boolean *)
  | `Beq, `Boolean -> "eq_bool"
  | `Bneq, `Boolean -> "eq_bool"
  | op, opty ->
      Jc_typing.typing_error Loc.dummy_position
        "Can't use operator %s with type %s in assertions"
        (string_of_op op) (string_of_op_type opty)


(******************************************************************************)
(*                                   types                                    *)
(******************************************************************************)

let has_equality_op = function
  | JCTnative Tunit -> false
  | JCTnative Tboolean -> true
  | JCTnative Tinteger -> true
  | JCTnative Treal -> true
  | JCTnative Tdouble -> true
  | JCTnative Tfloat -> true
  | JCTnative Tstring -> true
  | JCTlogic _s -> (* TODO *) false
  | JCTenum _ei -> true
  | JCTpointer _
  | JCTnull ->  true
  | JCTany -> false
  | JCTtype_var _ -> false (* TODO ? *)
  
let equality_op_for_type = function
  | JCTnative Tunit -> assert false
  | JCTnative Tboolean -> "eq_bool"
  | JCTnative Tinteger -> "eq_int"
  | JCTnative Treal -> "eq_real"
  | JCTnative Tdouble -> "eq_gen_float"
  | JCTnative Tfloat -> "eq_gen_float"
  | JCTnative Tstring -> "eq"
  | JCTlogic s -> (* TODO *) assert false
  | JCTenum ei -> eq_of_enum ei
  | JCTpointer _
  | JCTnull ->  "eq"
  | JCTany -> assert false
  | JCTtype_var _ -> assert false (* TODO ? *)


(******************************************************************************)
(*                                 Structures                                 *)
(******************************************************************************)

let tr_struct st acc =
  let tagid_type = tag_id_type (struct_root st) in
    (* Declarations of field memories. *)
  let acc =
    if !Jc_options.separation_sem = SepRegions then acc else
      if struct_of_plain_union st then acc else
        List.fold_left
          (fun acc fi ->
             let mem = memory_type (JCmem_field fi) in
             Param(
               false,
               field_memory_name fi,
               Ref_type(Base_type mem))::acc)
          acc st.jc_struct_info_fields
  in
  (* Declarations of translation functions for union *)
  let vi = struct_root st in
  let acc = 
    if not (root_is_union vi) then acc else
      if integral_union vi then acc else
        let uty = bitvector_type in
        List.fold_left
          (fun acc fi ->
	     if has_equality_op fi.jc_field_info_type then
               Logic(false,logic_field_of_union fi,
                     [("",uty)],tr_base_type fi.jc_field_info_type)
               :: Logic(false,logic_union_of_field fi,
			[("",tr_base_type fi.jc_field_info_type)],uty)
               :: Axiom((logic_field_of_union fi)^"_of_"^(logic_union_of_field fi),
			LForall("x",tr_base_type fi.jc_field_info_type,
				LPred(equality_op_for_type fi.jc_field_info_type,
                                      [LApp(logic_field_of_union fi,
                                            [LApp(logic_union_of_field fi, 
                                                  [LVar "x"])]);
                                       LVar "x"])))
               :: acc
	     else acc)
          acc st.jc_struct_info_fields
  in
  (* declaration of the tag_id *)
  let acc =
    Logic(false,tag_name st,[],tagid_type)::acc
  in

  let acc = 
    if struct_of_union st then acc 
    else 
      let pc = JCtag(st,[]) in
      let ac = alloc_class_of_pointer_class pc in
	(* Validity parameters *)
	make_valid_pred ~equal:true ac pc 
	:: make_valid_pred ~equal:false ac pc 
	:: make_valid_pred ~equal:false ~right:false ac pc 
	:: make_valid_pred ~equal:false ~left:false ac pc 
	:: make_valid_pred ~equal:true (* TODO ? *) JCalloc_bitvector pc 
	  (* Allocation parameters *)
	:: make_alloc_param ~check_size:true ac pc 
	:: make_alloc_param ~check_size:false ac pc 
	:: make_alloc_param ~check_size:true JCalloc_bitvector pc 
	:: make_alloc_param ~check_size:false JCalloc_bitvector pc 
	:: (if Region.exists_bitwise () then make_conversion_params pc else [])
	@ acc
  in
    
  match st.jc_struct_info_parent with
    | None ->
        (* axiom for parenttag *)
        let name = st.jc_struct_info_name ^ "_parenttag_bottom" in
        let p = LPred("parenttag", [ LVar (tag_name st); LVar "bottom_tag" ]) in
        Axiom(name, p)::acc
    | Some(p, _) ->
        (* axiom for parenttag *)
        let name =
          st.jc_struct_info_name ^ "_parenttag_" ^ p.jc_struct_info_name
        in
        let p =
          LPred("parenttag", [ LVar (tag_name st); LVar (tag_name p) ])
        in
        Axiom(name, p)::acc


(******************************************************************************)
(*                                 Coercions                                  *)
(******************************************************************************)

let term_coerce ~type_safe ~global_assertion lab ?(cast=false) pos ty_dst ty_src e e' =
  match ty_dst, ty_src with
      (* identity *)
    | JCTnative t, JCTnative u when t = u -> e'
    | JCTlogic t, JCTlogic u when t = u -> e'
    | JCTany, JCTany -> e'
      (* between integer/enum and real *)
    | JCTnative Treal, JCTnative Tinteger -> 
	begin match e' with
	  | LConst(Prim_int n) ->
	      LConst(Prim_real(n ^ ".0")) 
	  | _ -> 
	      LApp("real_of_int",[ e' ])
	end
    | JCTnative Treal, JCTenum ri -> 
	begin match e' with
	  | LConst(Prim_int n) ->
	      LConst(Prim_real(n ^ ".0")) 
	  | _ -> 
	      let e' = LApp(logic_int_of_enum ri,[ e' ]) in
	      LApp("real_of_int",[ e' ])
	end
    | JCTnative Treal, JCTnative Tdouble ->
	LApp("float_value",[ e' ])
    | JCTnative Treal, JCTnative Tfloat ->
	LApp("float_value",[ e' ])
    | JCTnative Tdouble, JCTnative Treal ->
	assert false (* TODO *)
	(* LApp("round_float ?",[ e' ]) *)
    | JCTnative Tinteger, JCTnative Treal -> 
	LApp("int_of_real",[ e' ])
      (* between enums and integer *)
    | JCTenum ri1, JCTenum ri2
	when ri1.jc_enum_info_name = ri2.jc_enum_info_name -> e'
    | JCTenum ri1, JCTenum ri2 ->
        assert cast; (* Typing should have inserted an explicit cast *)
        let e' = LApp(logic_int_of_enum ri2,[ e' ]) in
        LApp(logic_enum_of_int ri1,[ e' ])
    | JCTnative Tinteger, JCTenum ri ->
        LApp(logic_int_of_enum ri,[ e' ])
    | JCTenum ri, JCTnative Tinteger ->
        LApp(logic_enum_of_int ri,[ e' ]) 
      (* between pointers and null *)
    | JCTpointer _ , JCTnull -> e'
    | JCTpointer(pc1,_,_), JCTpointer(JCtag(st2,_),_,_) 
        when Jc_typing.substruct st2 pc1 -> e'
    | JCTpointer(JCtag(st1,_),_,_), JCTpointer _ -> 
	let tag = 
	  ttag_table_var ~label_in_name:global_assertion lab
	    (struct_root st1,e#region)
	in
        LApp("downcast", [ tag; e'; LVar (tag_name st1) ])
    |  _ -> 
         Jc_typing.typing_error pos 
           "can't coerce type %a to type %a" 
           print_type ty_src print_type ty_dst
           
let eval_integral_const e =
  let rec eval e =
    match e#node with
      | JCEconst(JCCinteger s) -> Numconst.integer s 
      | JCErange_cast(e,_ri2) -> eval e
      | JCEunary(op,e) ->
          let v = eval e in
          begin match op with
            | `Uminus, `Integer -> minus_num v
            | `Uminus, (`Real | `Double | `Float | `Boolean | `Unit)
            | `Unot, _
            | `Ubw_not, _ -> raise Exit
          end
      | JCEbinary(e1,op,e2) ->
          let v1 = eval e1 in
          let v2 = eval e2 in
          begin match op with
            | `Badd, `Integer -> v1 +/ v2
            | `Bsub, `Integer -> v1 -/ v2
            | `Bmul, `Integer -> v1 */ v2
            | `Bdiv, `Integer -> v1 // v2 (* TODO: or quo_num? *)
            | `Bmod, `Integer -> mod_num v1 v2
            | (`Badd | `Barith_shift_right | `Bbw_and | `Bbw_or | `Bbw_xor
              | `Bdiv | `Beq | `Bge | `Bgt | `Ble | `Blogical_shift_right
              | `Blt | `Bmod | `Bmul | `Bneq | `Bshift_left | `Bsub), _ ->
		raise Exit
	    | `Bconcat, _ -> raise Exit
	    | `Bland, _ -> raise Exit
	    | `Blor, _ -> raise Exit
          end
      | JCEif(e1,e2,e3) ->
          (* TODO: write [eval_boolean_const] *)
          raise Exit
      | JCEconst _ | JCEvar _ | JCEshift _ | JCEderef _ 
      | JCEinstanceof _ | JCEcast _ | JCEbitwise_cast _ | JCEreal_cast _ 
      | JCEoffset _ | JCEbase_block _ 
      | JCEaddress _ 
      | JCEalloc _ | JCEfree _ | JCEmatch _ |JCEunpack _ |JCEpack _
      | JCEthrow _ | JCEtry _ | JCEreturn _ | JCEloop _ | JCEblock _
      | JCEcontract _ | JCEassert _ 
      | JCElet _ | JCEassign_heap _ | JCEassign_var _ | JCEapp _
      | JCEreturn_void -> raise Exit

  in
  try Some(eval e) with Exit -> None

let rec fits_in_enum ri e = 
  match eval_integral_const e with
    | Some c -> ri.jc_enum_info_min <=/ c && c <=/ ri.jc_enum_info_max
    | None -> false

let coerce ~check_int_overflow mark pos ty_dst ty_src e e' =
  match ty_dst, ty_src with
      (* identity *)
    | JCTnative t, JCTnative u when t = u -> e'
    | JCTlogic t, JCTlogic u when t = u -> e'
    | JCTany, JCTany -> e'
      (* between integer/enum and real *)
    | JCTnative Treal, JCTnative Tinteger -> 
	begin match e' with
	  | Cte(Prim_int n) ->
	      Cte(Prim_real(n ^ ".0")) 
	  | _ -> 
	      make_app "real_of_int" [ e' ]
	end
    | JCTnative Treal, JCTenum ri -> 
	begin match e' with
	  | Cte(Prim_int n) ->
	      Cte(Prim_real(n ^ ".0")) 
	  | _ -> 
	      make_app "real_of_int" [ make_app (logic_int_of_enum ri) [ e' ] ]
	end
    | JCTnative Tinteger, JCTnative Treal -> 
	make_app "int_of_real" [ e' ]
    | JCTnative Tdouble, JCTnative Treal ->
	make_app "gen_float_of_real" [ Var "Double" ; Var "nearest_even" ; e' ]
    | JCTnative Treal, JCTnative Tdouble ->
	make_app "float_value" [ e' ]
    | JCTnative Tfloat, JCTnative Treal ->
	make_app "gen_float_of_real" [ Var "Single" ; Var "nearest_even" ; e' ]
    | JCTnative Treal, JCTnative Tfloat ->
	make_app "float_value" [ e' ]
      (* between enums and integer *)
    | JCTenum ri1, JCTenum ri2 
	when ri1.jc_enum_info_name = ri2.jc_enum_info_name -> e'
    | JCTenum ri1, JCTenum ri2 -> 
        let e' = make_app (logic_int_of_enum ri2) [ e' ] in
        if not check_int_overflow || fits_in_enum ri1 e then 
          make_app (safe_fun_enum_of_int ri1) [ e' ]
        else
          make_guarded_app ~mark ArithOverflow pos (fun_enum_of_int ri1) [ e' ]
    | JCTnative Tinteger, JCTenum ri ->
        make_app (logic_int_of_enum ri) [ e' ]
    | JCTenum ri, JCTnative Tinteger ->
        if not check_int_overflow || fits_in_enum ri e then 
          make_app (safe_fun_enum_of_int ri) [ e' ]
        else
          make_guarded_app ~mark ArithOverflow pos (fun_enum_of_int ri) [ e' ]
      (* between pointers and null *)
    | JCTpointer _ , JCTnull -> e'
    | JCTpointer(pc1,_,_), JCTpointer(JCtag(st2,_),_,_) 
        when Jc_typing.substruct st2 pc1 -> e'
    | JCTpointer(JCtag(st1,_),_,_), JCTpointer _  -> 
	let downcast_fun = 
	  if safety_checking () then "downcast_" else "safe_downcast_"
	in
	let tag = tag_table_var(struct_root st1,e#region) in
        make_guarded_app ~mark DownCast pos downcast_fun
          [ tag; e'; Var(tag_name st1) ] 
    | _ -> 
        Jc_typing.typing_error pos
          "can't coerce type %a to type %a" 
          print_type ty_src print_type ty_dst


(******************************************************************************)
(*                                   terms                                    *)
(******************************************************************************)

(* [pattern_lets] is a list of (id, value), which should be binded
 * at the assertion level. *)
let pattern_lets = ref []
let concat_pattern_lets lets = pattern_lets := lets @ !pattern_lets
let bind_pattern_lets body =
  let binds = 
    List.fold_left
      (fun body bind ->
	 match bind with
	   | JCforall(id, ty) -> LForall(id, ty, body)
	   | JClet(id, value) -> LLet(id, value, body))
      body (List.rev !pattern_lets)
  in
  pattern_lets := [];
  binds

let is_base_block t = match t#node with
  | JCTbase_block _ -> true
  | _ -> false

let rec term ~type_safe ~global_assertion ~relocate lab oldlab t =
  let ft = term ~type_safe ~global_assertion ~relocate lab oldlab in
  let term_coerce = term_coerce ~type_safe ~global_assertion lab in
  let t' = match t#node with
    | JCTconst JCCnull -> LVar "null"
    | JCTvar v -> tvar ~label_in_name:global_assertion lab v
    | JCTconst c -> LConst(const c)
    | JCTunary(op,t1) ->
        let t1'= ft t1 in
        LApp(term_unary_op op, 
	     [ term_coerce t#pos (native_operator_type op) t1#typ t1 t1' ])
(*     | JCTbinary(t1,(`Bsub,`Pointer),t2) -> *)
(*         let t1' = LApp("address",[ ft t1 ]) in *)
(*         let t2' = LApp("address",[ ft t2 ]) in *)
(* 	let st = pointer_struct t1#typ in *)
(* 	let s = string_of_int (struct_size_in_bytes st) in *)
(*         LApp( *)
(* 	  term_bin_op (`Bdiv,`Integer), *)
(* 	  [ LApp(term_bin_op (`Bsub,`Integer), [ t1'; t2' ]); *)
(* 	    LConst(Prim_int s) ]) *)
    | JCTbinary(t1,(_,(`Pointer | `Logic) as op),t2) ->
        let t1' = ft t1 in
        let t2' = ft t2 in
        LApp(term_bin_op op, [ t1'; t2' ])
    | JCTbinary(t1,(_, #native_operator_type as op),t2) ->
        let t1' = ft t1 in
        let t2' = ft t2 in
        let ty = native_operator_type op in
        LApp(term_bin_op op,
             [ term_coerce t1#pos ty t1#typ t1 t1'; 
               term_coerce t2#pos ty t2#typ t2 t2' ])
    | JCTshift(t1,t2) -> 
        let t1' = ft t1 in
        let t2' = ft t2 in
        LApp("shift",[ t1'; term_coerce t2#pos integer_type t2#typ t2 t2' ])
    | JCTif(t1,t2,t3) ->
        let t1' = ft t1 in
        let t2' = ft t2 in
        let t3' = ft t3 in
        TIf(t1', t2', t3')
    | JCToffset(k,t1,st) -> 
	let ac = tderef_alloc_class ~type_safe t1 in
        let alloc = 
	  talloc_table_var ~label_in_name:global_assertion lab (ac,t1#region) 
	in
	begin match ac with
	  | JCalloc_root _ ->
              let f = match k with
		| Offset_min -> "offset_min"
		| Offset_max -> "offset_max"
              in
              let t1' = ft t1 in
              LApp(f,[ alloc; t1' ])
	  | JCalloc_bitvector -> 
              let f = match k with
		| Offset_min -> "offset_min_bytes"
		| Offset_max -> "offset_max_bytes"
              in
              let t1' = ft t1 in
	      let s = string_of_int (struct_size_in_bytes st) in
              LApp(f,[ alloc; t1'; LConst(Prim_int s) ])
	end
    | JCTaddress(Addr_absolute,t1) -> 
        LApp("absolute_address",[ ft t1 ])
    | JCTaddress(Addr_pointer,t1) -> 
        LApp("address",[ ft t1 ])
    | JCTbase_block(t1) -> 
        LApp("base_block",[ ft t1 ])
    | JCTinstanceof(t1,lab',st) ->
      let lab = if relocate && lab' = LabelHere then lab else lab' in
        let t1' = ft t1 in
	let tag = 
	  ttag_table_var ~label_in_name:global_assertion lab
	    (struct_root st,t1#region) 
	in
        LApp("instanceof_bool",[ tag; t1'; LVar (tag_name st) ])
    | JCTcast(t1,lab',st) ->
      let lab = if relocate && lab' = LabelHere then lab else lab' in
        if struct_of_union st then 
	  ft t1 
	else
          let t1' = ft t1 in
	  let tag = 
	    ttag_table_var ~label_in_name:global_assertion lab
	      (struct_root st,t1#region) 
	  in
          LApp("downcast",[ tag; t1'; LVar (tag_name st) ])
    | JCTbitwise_cast(t1,_lab,_st) ->
	ft t1
    | JCTrange_cast(t1,ri) -> 
(*         eprintf "range_cast in term: from %a to %a@."  *)
(*           print_type t1#typ print_type (JCTenum ri); *)
        let t1' = ft t1 in
        term_coerce ~cast:true t1#pos (JCTenum ri) t1#typ t1 t1' 
    | JCTreal_cast(t1,rc) ->
        let t1' = ft t1 in
        begin match rc with
          | Integer_to_real ->
              term_coerce t1#pos real_type t1#typ t1 t1'
          | Double_to_real ->
              term_coerce t1#pos real_type t1#typ t1 t1'
	  | Float_to_real ->
	      term_coerce t1#pos real_type t1#typ t1 t1'
          | Real_to_integer ->
              term_coerce t1#pos integer_type t1#typ t1 t1'
	  | Round_double rm ->
              term_coerce t1#pos double_type t1#typ t1 t1'
	  | Round_float rm ->
              term_coerce t1#pos float_type t1#typ t1 t1'
	end
    | JCTderef(t1,lab',fi) -> 
	let lab = if relocate && lab' = LabelHere then lab else lab' in
	let mc,_ufi_opt = tderef_mem_class ~type_safe t1 fi in
	begin match mc with
	  | JCmem_field fi' -> 
	      assert (fi.jc_field_info_tag = fi'.jc_field_info_tag);
              let t1' = ft t1 in
              let mem = 
		tmemory_var ~label_in_name:global_assertion lab
		  (JCmem_field fi,t1#region) 
	      in
              LApp("select",[ mem; t1' ])
	  | JCmem_plain_union vi ->
	      let t1,off = tdestruct_union_access t1 (Some fi) in
	      (* Retrieve bitvector *)
              let t1' = ft t1 in
              let mem = 
		tmemory_var ~label_in_name:global_assertion lab
		  (JCmem_plain_union vi,t1#region) 
	      in
              let e' = LApp("select",[ mem; t1' ]) in
	      (* Retrieve subpart of bitvector for specific subfield *)
	      let off = match off with
		| Int_offset s -> int_of_string s
		| _ -> assert false (* TODO *)
	      in
	      let size = 
		match fi.jc_field_info_bitsize with
		  | Some x -> x / 8 
		  | None -> assert false
	      in
	      let off = string_of_int off and size = string_of_int size in
	      let e' = 
		LApp("extract_bytes",
		     [ e'; LConst(Prim_int off); LConst(Prim_int size) ])
	      in
	      (* Convert bitvector into appropriate type *)
	      begin match fi.jc_field_info_type with
		| JCTenum ri -> LApp(logic_enum_of_bitvector ri,[e'])
		| ty -> assert false (* TODO *)
	      end
	  | JCmem_bitvector ->
	      (* Retrieve bitvector *)
              let t1' = ft t1 in
	      let mem = 
		tmemory_var ~label_in_name:global_assertion lab
		  (JCmem_bitvector,t1#region) 
	      in
	      let off =
		match field_offset_in_bytes fi with
		  | Some x -> x
		  | None -> assert false
	      in
	      let size = 
		match fi.jc_field_info_bitsize with
		  | Some x -> x / 8 
		  | None -> assert false
	      in
	      let off = string_of_int off and size = string_of_int size in
	      let e' = 
		LApp("select_bytes",
		     [ mem; t1'; LConst(Prim_int off); LConst(Prim_int size) ])
	      in
	      (* Convert bitvector into appropriate type *)
	      begin match fi.jc_field_info_type with
		| JCTenum ri -> LApp(logic_enum_of_bitvector ri,[e'])
		| _ty -> assert false (* TODO *)
	      end
	end
    | JCTapp app ->
        let f = app.jc_app_fun in
        let args = 
	  List.fold_right (fun arg acc -> (ft arg) :: acc) app.jc_app_args []
        in
        let args = 
          try List.map2 (fun e e' -> (e,e')) app.jc_app_args args 
          with Invalid_argument _ -> assert false
        in
        let args =
          try
            List.map2 
              (fun v (t,t') -> term_coerce t#pos v.jc_var_info_type t#typ t t')
              f.jc_logic_info_parameters args
          with Invalid_argument _ -> 
            eprintf "fun = %s, len pars = %d, len args' = %d@." 
              f.jc_logic_info_name 
              (List.length f.jc_logic_info_parameters)
              (List.length args);
            assert false
        in
	let relab (lab1,lab2) =
	  (lab1, if lab2 = LabelHere then lab else lab2) 
	in
	let label_assoc =
	  if relocate then 
	    (LabelHere,lab) :: List.map relab app.jc_app_label_assoc
	  else app.jc_app_label_assoc
	in
        make_logic_fun_call ~label_in_name:global_assertion 
	  ~region_assoc:app.jc_app_region_assoc 
	  ~label_assoc:label_assoc
	  f args
    | JCTold t1 -> 
	let lab = if relocate && oldlab = LabelHere then lab else oldlab in
	term ~type_safe ~global_assertion ~relocate lab oldlab t1
    | JCTat(t1,lab') -> 
	let lab = if relocate && lab' = LabelHere then lab else lab' in
	term ~type_safe ~global_assertion ~relocate lab oldlab t1
    | JCTrange(t1,t2) -> assert false (* TODO ? *)
    | JCTmatch(t, ptl) ->
        let t' = ft t in
        (* TODO: use a temporary variable for t' *)
        (* if the pattern-matching is incomplete, default value is true *)
        let ptl',lets =
          pattern_list_term ft t' t#typ ptl (LConst(Prim_bool true)) in
	concat_pattern_lets lets;
        ptl'
  in
  (if t#mark <> "" then
     Tnamed(reg_check ~mark:t#mark t#pos,t')
   else
     t')

let () = ref_term := term

let named_term ~type_safe ~global_assertion ~relocate lab oldlab t =
  let t' = term ~type_safe ~global_assertion ~relocate lab oldlab t in
  match t' with
    | Tnamed _ -> t'
    | _ -> 
        let n = reg_check ~mark:t#mark t#pos in
        Tnamed(n,t')


(******************************************************************************)
(*                                assertions                                  *)
(******************************************************************************)

let tag ~type_safe ~global_assertion ~relocate lab oldlab tag= 
  match tag#node with
    | JCTtag st -> LVar (tag_name st)
    | JCTbottom -> LVar "bottom_tag"
    | JCTtypeof(t,st) ->
	let t' = term ~type_safe ~global_assertion ~relocate lab oldlab t in
	make_typeof st t#region t'

let rec assertion ~type_safe ~global_assertion ~relocate lab oldlab a =
  let fa = assertion ~type_safe ~global_assertion ~relocate lab oldlab in
  let ft = term ~type_safe ~global_assertion ~relocate lab oldlab in
  let ftag = tag ~type_safe ~global_assertion ~relocate lab oldlab in
  let term_coerce = term_coerce ~type_safe ~global_assertion lab in
  let a' = match a#node with
    | JCAtrue -> LTrue
    | JCAfalse -> LFalse
    | JCAif(t1,a2,a3) ->
        let t1' = ft t1 in
	let a2' = fa a2 in
	let a3' = fa a3 in
        LIf(t1',a2',a3')
    | JCAand al -> make_and_list (List.map fa al)
    | JCAor al -> make_or_list (List.map fa al)
    | JCAimplies(a1,a2) -> 
	let a1' = fa a1 in
	let a2' = fa a2 in
	make_impl a1' a2'
    | JCAiff(a1,a2) -> 
	let a1' = fa a1 in
	let a2' = fa a2 in
	make_equiv a1' a2'
    | JCAnot a1 -> 
	let a1' = fa a1 in
	LNot a1'
    | JCArelation(t1,((`Beq | `Bneq as op),_),t2) 
	when is_base_block t1 && is_base_block t2 ->
	let t1 = 
	  match t1#node with JCTbase_block t1' -> t1' | _ -> assert false 
	in
	let t2 = 
	  match t2#node with JCTbase_block t2' -> t2' | _ -> assert false 
	in
        let t1' = ft t1 in
        let t2' = ft t2 in
        let p = LPred("same_block", [ t1'; t2' ]) in
	begin match op with
	  | `Beq -> p
	  | `Bneq -> LNot p
	  | _ -> assert false
	end
    | JCArelation(t1,(_,(`Pointer | `Logic) as op),t2) ->
        let t1' = ft t1 in
        let t2' = ft t2 in
        LPred (pred_bin_op (op :> pred_bin_op),[ t1'; t2' ])
    | JCArelation(t1,(_, #native_operator_type as op),t2) ->
(* 	if Jc_options.debug then printf "%a@." Jc_output.assertion a; *)
        let t1' = ft t1 in
        let t2' = ft t2 in
        let ty = native_operator_type op in
        LPred(pred_bin_op (op :> pred_bin_op), 
              [ term_coerce t1#pos ty t1#typ t1 t1'; 
                term_coerce t2#pos ty t2#typ t2 t2' ])
    | JCAapp app -> 
        let f = app.jc_app_fun in
        let args = 
	  List.fold_right (fun arg acc -> (ft arg) :: acc) app.jc_app_args []
        in
        let args = 
          try List.map2 (fun e e' -> (e,e')) app.jc_app_args args 
          with Invalid_argument _ -> assert false
        in
        let args =
          try
            List.map2 
              (fun v (t,t') -> term_coerce t#pos v.jc_var_info_type t#typ t t')
              f.jc_logic_info_parameters args
          with Invalid_argument _ -> 
            eprintf "fun = %s, len pars = %d, len args' = %d@." 
              f.jc_logic_info_name 
              (List.length f.jc_logic_info_parameters)
              (List.length args);
            assert false
        in
	let label_assoc =
	  if relocate then 
	    let relab (lab1,lab2) =
	      (lab1, if lab2 = LabelHere then lab else lab2) 
	    in
	    (LabelHere,lab) :: List.map relab app.jc_app_label_assoc
	  else app.jc_app_label_assoc
	in
        make_logic_pred_call 
	  ~label_in_name:global_assertion 
	  ~region_assoc:app.jc_app_region_assoc 
	  ~label_assoc:label_assoc
	  f args
    | JCAquantifier(Forall,v,a1) -> 
        LForall(v.jc_var_info_final_name,
                tr_var_base_type v,
                fa a1)
    | JCAquantifier(Exists,v,a1) -> 
        LExists(v.jc_var_info_final_name,
                tr_var_base_type v,
                fa a1)
    | JCAold a1 -> 
	let lab = if relocate && oldlab = LabelHere then lab else oldlab in
	assertion ~type_safe ~global_assertion ~relocate lab oldlab a1
    | JCAat(a1,lab') -> 
	let lab = if relocate && lab' = LabelHere then lab else lab' in
	assertion ~type_safe ~global_assertion ~relocate lab oldlab a1
    | JCAbool_term t1 ->
        let t1' = ft t1 in
        LPred("eq",[ t1'; LConst(Prim_bool true) ])
    | JCAinstanceof(t1,lab',st) -> 
	let lab = if relocate && lab' = LabelHere then lab else lab' in
        let t1' = ft t1 in
	let tag = 
	  ttag_table_var ~label_in_name:global_assertion lab
	    (struct_root st,t1#region) 
	in
        LPred("instanceof",[ tag; t1'; LVar (tag_name st) ])
    | JCAmutable(te, st, ta) ->
        let te' = ft te in
        let tag = ftag ta in
        let mutable_field = LVar (mutable_name (JCtag(st, []))) in
        LPred("eq", [ LApp("select", [ mutable_field; te' ]); tag ])
    | JCAeqtype(tag1,tag2,_st_opt) ->
        let tag1' = ftag tag1 in
        let tag2' = ftag tag2 in
        LPred("eq", [ tag1'; tag2' ])
    | JCAsubtype(tag1,tag2,_st_opt) ->
        let tag1' = ftag tag1 in
        let tag2' = ftag tag2 in
        LPred("subtag", [ tag1'; tag2' ])
    | JCAmatch(arg, pal) ->
        let arg' = ft arg in
        (* TODO: use a temporary variable for arg' *)
        let pal', _ = pattern_list_assertion fa arg' arg#typ
          pal LTrue in
        pal'
  in
  let a' = bind_pattern_lets a' in
  if a#mark = "" then a' else LNamed(reg_check ~mark:a#mark a#pos, a')

let mark_assertion pos a' =
  match a' with
    | LNamed _ -> a'
    | _ -> LNamed(reg_check pos, a')
            
let named_assertion ~type_safe ~global_assertion ~relocate lab oldlab a =
  let a' = 
    assertion ~type_safe ~global_assertion ~relocate lab oldlab a 
  in
  mark_assertion a#pos a'


(******************************************************************************)
(*                                  Locations                                 *)
(******************************************************************************)

let rec pset ~type_safe ~global_assertion before loc = 
  let fpset = pset ~type_safe ~global_assertion before in
  let ft = term ~type_safe ~global_assertion ~relocate:false before before in
  let term_coerce = term_coerce ~type_safe ~global_assertion before in
  match loc#node with
    | JCLSderef(locs,lab,fi,_r) ->
        let m = tmemory_var ~label_in_name:global_assertion lab 
	  (JCmem_field fi,locs#region) in
        LApp("pset_deref", [m;fpset locs])
    | JCLSvar vi -> 
        let m = tvar ~label_in_name:global_assertion before vi in
        LApp("pset_singleton", [m])
    | JCLSrange(ls,None,None) ->
        let ls = fpset ls in
        LApp("pset_all", [ls])
    | JCLSrange(ls,None,Some b) ->
        let ls = fpset ls in
        let b' = ft b in
        LApp("pset_range_left", 
             [ls; 
              term_coerce b#pos integer_type b#typ b b'])
    | JCLSrange(ls,Some a,None) ->
        let ls = fpset ls in
        let a' = ft a in
        LApp("pset_range_right", 
             [ls; 
              term_coerce a#pos integer_type a#typ a a'])
    | JCLSrange(ls,Some a,Some b) ->
        let ls = fpset ls in
        let a' = ft a in
        let b' = ft b in
        LApp("pset_range", 
             [ls; 
              term_coerce a#pos integer_type a#typ a a'; 
              term_coerce b#pos integer_type b#typ b b'])
    | JCLSrange_term(ls,None,None) ->
        let ls = LApp("pset_singleton",[ ft ls ]) in
        LApp("pset_all", [ls])
    | JCLSrange_term(ls,None,Some b) ->
        let ls = LApp("pset_singleton",[ ft ls ]) in
        let b' = ft b in
        LApp("pset_range_left", 
             [ls; 
              term_coerce b#pos integer_type b#typ b b'])
    | JCLSrange_term(ls,Some a,None) ->
        let ls = LApp("pset_singleton",[ ft ls ]) in
        let a' = ft a in
        LApp("pset_range_right", 
             [ls; 
              term_coerce a#pos integer_type a#typ a a'])
    | JCLSrange_term(ls,Some a,Some b) ->
        let ls = LApp("pset_singleton",[ ft ls ]) in
        let a' = ft a in
        let b' = ft b in
        LApp("pset_range", 
             [ls; 
              term_coerce a#pos integer_type a#typ a a'; 
              term_coerce b#pos integer_type b#typ b b'])
        
let rec collect_locations ~type_safe ~global_assertion before (refs,mems) loc =
  let ft = term ~type_safe ~global_assertion ~relocate:false before before in
  match loc#node with
    | JCLderef(e,lab,fi,_fr) -> 
        let iloc = pset ~type_safe ~global_assertion lab e in
        let mc = 
(*           if field_of_union fi then FVvariant (union_of_field fi) else *)
	  JCmem_field fi
        in
        let l =
          try
            let l = MemoryMap.find (mc,location_set_region e) mems in
            iloc::l
          with Not_found -> [iloc]
        in
        (refs, MemoryMap.add (mc,location_set_region e) l mems)
    | JCLderef_term(t1,fi) -> 
        let iloc = LApp("pset_singleton",[ ft t1 ]) in
        let mc = JCmem_field fi in
        let l =
          try
            let l = MemoryMap.find (mc,t1#region) mems in
            iloc::l
          with Not_found -> [iloc]
        in
        (refs, MemoryMap.add (mc,t1#region) l mems)
    | JCLvar vi -> 
        let var = vi.jc_var_info_final_name in
        (StringMap.add var true refs,mems)
    | JCLat(loc,lab) ->
        collect_locations ~type_safe ~global_assertion before (refs,mems) loc

let rec collect_pset_locations ~type_safe ~global_assertion loc =
  let ft = term ~type_safe ~global_assertion ~relocate:false in
  match loc#node with
    | JCLderef(e,lab,fi,_fr) -> 
        pset ~type_safe ~global_assertion lab e
    | JCLderef_term(t1,fi) -> 
	let lab = match t1#label with Some l -> l | None -> assert false in
	LApp("pset_singleton",[ ft lab lab t1 ])	
    | JCLvar vi -> 
	LVar "pset_empty"
    | JCLat(loc,lab) ->
        collect_pset_locations ~type_safe ~global_assertion loc

let assigns ~type_safe ?region_list before ef locs loc =
  match locs with
    | None -> LTrue     
    | Some locs ->
  let refs = 
    (* HeapVarSet.fold
            (fun v m -> 
               if Ceffect.is_alloc v then m 
               else StringMap.add (heap_var_name v) (Reference false) m)
            assigns.Ceffect.assigns_var 
    *)
    VarMap.fold
      (fun v labs m -> StringMap.add v.jc_var_info_final_name false m)
      ef.jc_writes.jc_effect_globals StringMap.empty
  in
  let mems = 
    MemoryMap.fold
      (fun (fi,r) _labs acc -> 
	 if Option_misc.map_default (RegionList.mem r) true region_list then
           MemoryMap.add (fi,r) [] acc
	 else acc
      ) ef.jc_writes.jc_effect_memories MemoryMap.empty 
  in
  let refs,mems = 
    List.fold_left (collect_locations ~type_safe ~global_assertion:false before) (refs,mems) locs
  in
  let a =
    StringMap.fold
      (fun v p acc -> 
        if p then acc else
          make_and acc (LPred("eq", [LVar v; lvar ~constant:false (* <<- CHANGE THIS *) ~label_in_name:false before v])))
      refs LTrue
  in
  MemoryMap.fold
    (fun (mc,r) p acc -> 
       let v = memory_name(mc,r) in
       let ac = alloc_class_of_mem_class mc in
       make_and acc
	 (let a = LPred("not_assigns",
                [talloc_table_var ~label_in_name:false before (ac,r); 
                 lvar ~constant:false (* <<- CHANGE THIS *) ~label_in_name:false before v;
                 LVar v; location_list' p]) in
	  LNamed(reg_check loc,a))
    ) mems a

let reads ~type_safe ~global_assertion locs (mc,r) =
  let refs = StringMap.empty
  in
  let mems = MemoryMap.empty 
  in
  let refs,mems = 
    List.fold_left (collect_locations ~type_safe ~global_assertion LabelOld) (refs,mems) locs
  in
  let p = try MemoryMap.find (mc,r) mems with Not_found -> [] in
  location_list' p


(******************************************************************************)
(*                                Expressions                                 *)
(******************************************************************************)

let bounded lb rb s =
  let n = Num.num_of_string s in Num.le_num lb n && Num.le_num n rb

let lbounded lb s =
  let n = Num.num_of_string s in Num.le_num lb n

let rbounded rb s =
  let n = Num.num_of_string s in Num.le_num n rb

let rec const_int_term t =
  match t # node with
    | JCTconst (JCCinteger s) -> Some (Numconst.integer s)
    | JCTvar vi -> 
	begin 
	  try
	    let _, init =
	      Hashtbl.find
		Jc_typing.logic_constants_table
		vi.jc_var_info_tag
	    in
	      const_int_term init
	  with Not_found -> None
	end
    | JCTapp app -> 
	begin
	  try
	    let _, init =
	      Hashtbl.find
		Jc_typing.logic_constants_table
		app.jc_app_fun.jc_logic_info_tag
	    in
	      const_int_term init
	  with Not_found -> None
	end	      
    | JCTunary (uop, t) ->
	let no = const_int_term t in
	  Option_misc.fold
	    (fun n _ -> match uop with
	       | `Uminus, `Integer -> Some (Num.minus_num n)
	       | _ -> None)
	    no None
    | JCTbinary (t1, bop, t2) ->
	let no1 = const_int_term t1 in
	let no2 = const_int_term t2 in
	  begin match no1, no2 with
	    | Some n1, Some n2 ->
		begin match bop with
		  | `Badd, `Integer -> Some (n1 +/ n2)
		  | `Bsub, `Integer -> Some (n1 -/ n2) 
		  | `Bmul, `Integer -> Some (n1 */ n2)
		  | `Bdiv, `Integer -> 
		      let n = n1 // n2 in 
			if Num.is_integer_num n then
			  Some n 
			else 
			  None
		  | `Bmod, `Integer -> Some (Num.mod_num n1 n2)
		  | _ -> None
		end
	    | _ -> None
	  end
    | JCTrange_cast (t, _) -> const_int_term t
    | JCTconst _ | JCTshift _ | JCTderef _ 
    | JCTold _ | JCTat _ 
    | JCToffset _ | JCTaddress _ | JCTinstanceof _ | JCTbase_block _
    | JCTreal_cast _ | JCTif _ | JCTrange _ 
    | JCTcast _ | JCTmatch _ | JCTbitwise_cast _ ->
	assert false
	  
let rec const_int_expr e =
  match e # node with
    | JCEconst (JCCinteger s) -> Some (Numconst.integer s)
    | JCEvar vi ->
	begin
	  try
	    let _, init =
	      Hashtbl.find
		Jc_typing.logic_constants_table
		vi.jc_var_info_tag
	    in
	      const_int_term init
	  with Not_found -> None
	end
    | JCEapp call -> 
	begin match call.jc_call_fun with
	  | JClogic_fun li ->
	      begin
		try
		  let _, init =
		    Hashtbl.find
		      Jc_typing.logic_constants_table
		      li.jc_logic_info_tag
		  in
		    const_int_term init
		with Not_found -> None
	      end	      
	  | JCfun _ -> None
	end
    | JCErange_cast (e, _) -> const_int_expr e
    | JCEunary (uop, e) ->
	let no = const_int_expr e in
	  Option_misc.fold
	    (fun n _ -> match uop with
	       | `Uminus, `Integer -> Some (Num.minus_num n)
	       | _ -> None)
	    no None
    | JCEbinary (e1, bop, e2) ->
	let no1 = const_int_expr e1 in
	let no2 = const_int_expr e2 in
	  begin match no1, no2 with
	    | Some n1, Some n2 ->
		begin match bop with
		  | `Badd, `Integer -> Some (n1 +/ n2)
		  | `Bsub, `Integer -> Some (n1 -/ n2) 
		  | `Bmul, `Integer -> Some (n1 */ n2)
		  | `Bdiv, `Integer -> 
		      let n = n1 // n2 in 
			if Num.is_integer_num n then
			  Some n 
			else 
			  None
		  | `Bmod, `Integer -> Some (Num.mod_num n1 n2)
		  | _ -> None
		end
	    | _ -> None
	  end
    | JCEderef _ | JCEoffset _ | JCEbase_block _
    | JCEaddress _ | JCElet _ | JCEassign_var _
    | JCEassign_heap _ ->
	None
    | JCEif _ -> 
	None (* TODO *)
    | JCEconst _ | JCEinstanceof _ | JCEreal_cast _
    | JCEalloc _ | JCEfree _ | JCEassert _
    | JCEcontract _ | JCEblock _ | JCEloop _
    | JCEreturn_void | JCEreturn _ | JCEtry _
    | JCEthrow _ | JCEpack _ | JCEunpack _
    | JCEcast _ | JCEmatch _ | JCEshift _ 
    | JCEbitwise_cast _ -> 
	assert false
	
let destruct_pointer e = 
  let ptre, off = match e # node with
    | JCEshift (e1, e2) ->
	e1, 
	(let no = const_int_expr e2 in
	   Option_misc.fold 
	     (fun n _ -> 
		Int_offset (Num.string_of_num n)) no (Expr_offset e2))
    | _ -> e, Int_offset "0"
  in
    match ptre # typ with
      | JCTpointer (_, lb, rb) -> ptre, off, lb, rb
      | _ -> assert false

let rec make_lets l e =
  match l with
    | [] -> e
    | (tmp,a)::l -> Let(tmp,a,make_lets l e)
  
let old_to_pre = function
  | LabelOld -> LabelPre
  | lab -> lab

let old_to_pre_term =
  Jc_iterators.map_term
    (fun t -> match t#node with
       | JCTold t' 
       | JCTat(t',LabelOld) -> 
	   new term_with 
	     ~node:(JCTat(t',LabelPre)) t
       | JCTderef(t',LabelOld,fi) ->
	   new term_with 
	     ~node:(JCTderef(t',LabelPre,fi)) t	   
       | _ -> t)
  
let rec old_to_pre_lset lset =
  match lset#node with
    | JCLSvar _ -> lset
    | JCLSderef(lset,lab,fi,_region) ->
	new location_set_with
	  ~node:(JCLSderef(old_to_pre_lset lset, old_to_pre lab, fi, lset#region))
	  lset
    | JCLSrange(lset,t1,t2) ->
	new location_set_with
	  ~node:(JCLSrange(old_to_pre_lset lset, 
			   Option_misc.map old_to_pre_term t1,
			   Option_misc.map old_to_pre_term t2))
	  lset
    | JCLSrange_term(lset,t1,t2) ->
	new location_set_with
	  ~node:(JCLSrange_term(old_to_pre_term lset, 
				Option_misc.map old_to_pre_term t1,
				Option_misc.map old_to_pre_term t2))
	  lset

let rec old_to_pre_loc loc =
  match loc#node with
    | JCLvar _ -> loc
    | JCLat(l,lab) -> 
	new location_with
	  ~node:(JCLat(old_to_pre_loc l, old_to_pre lab))
	  loc
    | JCLderef(lset,lab,fi,_region) ->
	new location_with
	  ~node:(JCLderef(old_to_pre_lset lset,old_to_pre lab, fi, lset#region))
	  loc
    | JCLderef_term(t1,fi) ->
	new location_with
	  ~node:(JCLderef_term(old_to_pre_term t1,fi))
	  loc

let assumption al a' =
  let ef = List.fold_left Jc_effect.assertion empty_effects al in
  let read_effects = 
    local_read_effects ~callee_reads:ef ~callee_writes:empty_effects
  in
  BlackBox(Annot_type(LTrue, unit_type, read_effects, [], a', []))

(* Translate the heap update `e1.fi = tmp2' 

   essentially we want

   let tmp1 = [e1] in
   fi := upd(fi,tmp1,tmp2)

   special cases are considered to avoid statically known safety properties:
   if e1 has the form p + i then we build

   let tmpp = p in 
   let tmpi = i in
   let tmp1 = shift(tmpp, tmpi) in
    // depending on type of p and value of i
   ...
*)
let rec make_upd_simple mark pos e1 fi tmp2 =
  (* Temporary variables to avoid duplicating code *)
  let tmpp = tmp_var_name () in
  let tmpi = tmp_var_name () in
  let tmp1 = tmp_var_name () in  
  (* Define memory and allocation table *)
  let mc,_ufi_opt = deref_mem_class ~type_safe:false e1 fi in
  let mem = plain_memory_var (mc,e1#region) in
  let ac = alloc_class_of_mem_class mc in
  let alloc = alloc_table_var (ac,e1#region) in
  let lets, e' =
    if safety_checking () then
      let p,off,lb,rb = destruct_pointer e1 in
      let p' = expr p in
      let i' = offset off in
      let letspi = 
	[ (tmpp,p') ; (tmpi,i') ; 
	  (tmp1,make_app "shift" [Var tmpp; Var tmpi])] 
      in
      match off, lb, rb with
	| Int_offset s, Some lb, Some rb when bounded lb rb s ->
            let e1' = expr e1 in	    
	    [ (tmp1, e1') ],
            make_app "safe_upd_" [ mem; Var tmp1; Var tmp2 ]
	| Int_offset s, Some lb, _ when lbounded lb s ->
	    letspi, 
            make_guarded_app ~mark IndexBounds pos "lsafe_lbound_upd_" 
              [ alloc; mem; Var tmpp; Var tmpi; Var tmp2 ]
	| Int_offset s, _, Some rb when rbounded rb s ->
	    letspi, 
            make_guarded_app ~mark IndexBounds pos "rsafe_rbound_upd_" 
              [ alloc; mem; Var tmpp; Var tmpi; Var tmp2 ]
	| Int_offset s, None, None when int_of_string s = 0 ->
	    [ (tmp1, p') ], 
            make_guarded_app ~mark PointerDeref pos "upd_" 
              [ alloc; mem ; Var tmp1; Var tmp2 ]
	| _ ->
	    letspi, 
            make_guarded_app ~mark PointerDeref pos "offset_upd_" 
              [ alloc; mem ; Var tmpp; Var tmpi; Var tmp2 ]
    else
      let e1' = expr e1 in	    
      [ (tmp1,e1') ],
      make_app "safe_upd_" [ mem; Var tmp1; Var tmp2 ]
  in
  tmp1, lets, e'

and make_upd_union mark pos off e1 fi tmp2 =
  let e1' = expr e1 in
  (* Convert value assigned into bitvector *)
  let e2' = match fi.jc_field_info_type with
    | JCTenum ri -> make_app (logic_bitvector_of_enum ri) [Var tmp2]
    | _ty -> assert false (* TODO *)
  in
  (* Temporary variables to avoid duplicating code *)
  let tmp1 = tmp_var_name () in
  let tmp2 = tmp_var_name () in
  let v1 = Jc_pervasives.var e1#typ tmp1 in
  let e1 = new expr_with ~node:(JCEvar v1) e1 in
  let size = 
    match fi.jc_field_info_bitsize with
      | Some x -> x / 8 
      | None -> assert false
  in
  let union_size = 
    (union_type e1#typ).jc_root_info_union_size_in_bytes 
  in
  let e2' = 
    if size = union_size then
      (* TODO: deal with offset which should be null *)
      e2'
    else
      (* Retrieve bitvector for access to union *)
      let deref = make_deref_simple mark pos e1 fi in
      (* Replace subpart of bitvector for specific subfield *)
      let off = match off with
	| Int_offset s -> int_of_string s
	| _ -> assert false (* TODO *)
      in
      let off = string_of_int off and size = string_of_int size in
      make_app "replace_bytes"
	[ deref; Cte(Prim_int off); Cte(Prim_int size); e2' ]
  in
  let lets = [ (tmp1,e1'); (tmp2,e2') ] in
  let tmp1, lets', e' = make_upd_simple mark pos e1 fi tmp2 in
  tmp1, lets @ lets', e'

and make_upd_bytes mark pos e1 fi tmp2 =
  let e1' = expr e1 in
  (* Convert value assigned into bitvector *)
  let e2' = match fi.jc_field_info_type with
    | JCTenum ri -> make_app (logic_bitvector_of_enum ri) [Var tmp2]
    | _ty -> assert false (* TODO *)
  in
  (* Temporary variables to avoid duplicating code *)
  let tmp1 = tmp_var_name () in
  let v1 = Jc_pervasives.var e1#typ tmp1 in
  let e1 = new expr_with ~node:(JCEvar v1) e1 in
  (* Define memory and allocation table *)
  let mem = plain_memory_var (JCmem_bitvector,e1#region) in
  let alloc = alloc_table_var (JCalloc_bitvector,e1#region) in
  (* Store bitvector *)
  let off = 
    match field_offset_in_bytes fi with
      | Some x -> x
      | None -> assert false
  in
  let size = 
    match fi.jc_field_info_bitsize with
      | Some x ->  x / 8 
      | None -> assert false
  in
  let off = string_of_int off and size = string_of_int size in
  let e' = 
    if safety_checking () then
      make_guarded_app ~mark PointerDeref pos "upd_bytes_" 
        [ alloc; mem; Var tmp1; Cte(Prim_int off); Cte(Prim_int size); 
	  Var tmp2 ]
    else
      make_app "safe_upd_bytes_"
	[ mem; Var tmp1; Cte(Prim_int off); Cte(Prim_int size); Var tmp2 ]
  in
  let lets = [ (tmp1,e1'); (tmp2,e2') ] in
  tmp1, lets, e'

and make_upd mark pos e1 fi e2 =
  (* Value assigned stored in temporary at that point *)
  let v2 = match e2#node with JCEvar v2 -> v2 | _ -> assert false in
  let tmp2 = v2.jc_var_info_name in
  (* Dispatch depending on kind of memory *) 
  let mc,ufi_opt = deref_mem_class ~type_safe:false e1 fi in
  match mc,ufi_opt with
    | JCmem_field fi', None -> 
	assert (fi.jc_field_info_tag = fi'.jc_field_info_tag);
	make_upd_simple mark pos e1 fi tmp2
    | JCmem_field fi', Some ufi ->
	assert (fi.jc_field_info_tag = fi'.jc_field_info_tag);
        let tmp1, lets, e1' = make_upd_simple mark pos e1 fi tmp2 in
	let mems = overlapping_union_memories ufi in
	let ef = 
	  List.fold_left 
	    (fun ef mc -> add_memory_effect LabelHere ef (mc,e1#region)) 
	    empty_effects mems 
	in
	let write_effects = 
	  local_write_effects ~callee_reads:empty_effects ~callee_writes:ef
	in
	let e2' = 
	  BlackBox(Annot_type(LTrue, unit_type, [], write_effects, LTrue, [])) 
	in
	tmp1, lets, append e1' e2'
    | JCmem_plain_union _vi, _ ->
	let e1,off = destruct_union_access e1 (Some fi) in
	make_upd_union mark pos off e1 fi tmp2
    | JCmem_bitvector, _ -> 
	make_upd_bytes mark pos e1 fi tmp2

(* Translate the heap access `e.fi' 

   special cases are considered to avoid statically known safety properties:
   if e1 has the form p + i then we build an access that depends on the 
   type of p and the value of i
*)
and make_deref_simple mark pos e fi =
  (* Define memory and allocation table *)
  let mc,_ufi_opt = deref_mem_class ~type_safe:false e fi in
  let mem = memory_var (mc,e#region) in
  let ac = alloc_class_of_mem_class mc in
  let alloc = alloc_table_var (ac,e#region) in
  let e' = 
    if safety_checking() then
      match destruct_pointer e with
	| _, Int_offset s, Some lb, Some rb when bounded lb rb s ->
            make_app "safe_acc_" 
              [ mem ; expr e ]
	| p, (Int_offset s as off), Some lb, _ when lbounded lb s ->
            make_guarded_app ~mark IndexBounds pos "lsafe_lbound_acc_" 
              [ alloc; mem; expr p; offset off ]
	| p, (Int_offset s as off), _, Some rb when rbounded rb s ->
            make_guarded_app ~mark IndexBounds pos "rsafe_rbound_acc_" 
              [ alloc; mem; expr p; offset off ]
	| p, Int_offset s, None, None when int_of_string s = 0 ->
            make_guarded_app ~mark PointerDeref pos "acc_" 
              [ alloc; mem ; expr p ]
	| p, off, _, _ ->
            make_guarded_app ~mark PointerDeref pos "offset_acc_" 
              [ alloc; mem ; expr p; offset off ]
    else
      make_app "safe_acc_" [ mem; expr e ]
  in e'

and make_deref_union mark pos off e fi =
  (* Retrieve bitvector *)
  let e' = make_deref_simple mark pos e fi in
  (* Retrieve subpart of bitvector for specific subfield *)
  let off = match off with
    | Int_offset s -> int_of_string s
    | _ -> assert false (* TODO *)
  in
  let size = 
    match fi.jc_field_info_bitsize with
      | Some x -> x / 8 
      | None -> assert false
  in
  let off = string_of_int off and size = string_of_int size in
  let e' = 
    make_app "extract_bytes" [ e'; Cte(Prim_int off); Cte(Prim_int size) ]
  in
  (* Convert bitvector into appropriate type *)
  match fi.jc_field_info_type with
    | JCTenum ri -> make_app (logic_enum_of_bitvector ri) [e']
    | ty -> assert false (* TODO *)

and make_deref_bytes mark pos e fi =
  (* Define memory and allocation table *)
  let mem = memory_var (JCmem_bitvector,e#region) in
  let alloc = alloc_table_var (JCalloc_bitvector,e#region) in
  (* Retrieve bitvector *)
  let off = 
    match field_offset_in_bytes fi with
      | Some x -> x
      | None -> assert false
  in
  let size = 
    match fi.jc_field_info_bitsize with
      | Some x ->  x / 8
      | None -> assert false
  in
  let off = string_of_int off and size = string_of_int size in
  let e' = 
    if safety_checking () then
      make_guarded_app ~mark PointerDeref pos "acc_bytes_" 
        [ alloc; mem; expr e; Cte(Prim_int off); Cte(Prim_int size) ]
    else
      make_app "safe_acc_bytes_"
	[ mem; expr e; Cte(Prim_int off); Cte(Prim_int size) ]
  in
  (* Convert bitvector into appropriate type *)
  match fi.jc_field_info_type with
    | JCTenum ri -> make_app (logic_enum_of_bitvector ri) [e']
    | ty -> 
	Format.eprintf "%a@." print_type ty;
	assert false (* TODO *)

and make_deref mark pos e1 fi =
  (* Dispatch depending on kind of memory *)
  let mc,_uif_opt = deref_mem_class ~type_safe:false e1 fi in
  match mc with
    | JCmem_field _ ->
	make_deref_simple mark pos e1 fi
    | JCmem_plain_union _vi ->
	let e1,off = destruct_union_access e1 (Some fi) in
	make_deref_union mark pos off e1 fi
    | JCmem_bitvector -> 
	make_deref_bytes mark pos e1 fi

and offset = function
  | Int_offset s -> Cte (Prim_int s)
  | Expr_offset e -> 
      coerce ~check_int_overflow:(safety_checking())
        e#mark e#pos integer_type e#typ e
        (expr e)
  | Term_offset _ -> assert false

and list_type_assert ty e (lets, params) =
  let offset k e1 ty tmp =
    let ac = deref_alloc_class ~type_safe:false e1 in
    let alloc = 
      talloc_table_var ~label_in_name:false LabelHere (ac,e1#region) 
    in
    match ac with
      | JCalloc_root _ ->
          let f = match k with
	    | Offset_min -> "offset_min"
	    | Offset_max -> "offset_max"
          in
	  LApp(f,[alloc; LVar tmp])
      | JCalloc_bitvector -> 
	  let st = pointer_struct ty in
          let f = match k with
	    | Offset_min -> "offset_min_bytes"
	    | Offset_max -> "offset_max_bytes"
          in
	  let s = string_of_int (struct_size_in_bytes st) in
	  LApp(f,[alloc; LVar tmp; LConst(Prim_int s)])
  in
  let opt = match ty with
    | JCTpointer(pc,n1o,n2o) ->
	let tmp = tmp_var_name () in
	let offset_mina n = 
	  LPred ("le_int",
		 [offset Offset_min e ty tmp;
		  LConst (Prim_int (Num.string_of_num n))]) 
	in
	let offset_maxa n =
	  LPred ("ge_int",
		 [offset Offset_max e ty tmp;
		  LConst (Prim_int (Num.string_of_num n))])
	in
	begin match e#typ with
	  | JCTpointer (si', n1o', n2o') ->
	      begin match n1o, n2o with
		| None, None -> None
		| Some n, None ->
		    begin match n1o' with
		      | Some n' when Num.le_num n' n && not Jc_options.verify_all_offsets -> None
		      | _ -> Some (tmp, offset_mina n)
		    end
		| None, Some n -> 
		    begin match n2o' with
		      | Some n' when Num.ge_num n' n && not Jc_options.verify_all_offsets -> None
		      | _ -> Some (tmp, offset_maxa n)
		    end
		| Some n1, Some n2 ->
		    begin match n1o', n2o' with
		      | None, None -> Some (tmp, make_and (offset_mina n1) (offset_maxa n2))
		      | Some n1', None ->
			  if Num.le_num n1' n1 && not Jc_options.verify_all_offsets then 
			    Some (tmp, offset_maxa n2) 
			  else
			    Some (tmp, make_and (offset_mina n1) (offset_maxa n2))
		      | None, Some n2' ->
			  if Num.ge_num n2' n2 && not Jc_options.verify_all_offsets then 
			    Some (tmp, offset_mina n1) 
			  else
			    Some (tmp, make_and (offset_mina n1) (offset_maxa n2))
		      | Some n1', Some n2' ->
			  if Jc_options.verify_all_offsets then
			    Some (tmp, make_and (offset_mina n1) (offset_maxa n2))
			  else
			    if Num.le_num n1' n1 then 
			      if Num.ge_num n2' n2 then None else 
				Some (tmp, offset_maxa n2)
			    else
			      if Num.ge_num n2' n2 then 
				Some (tmp, offset_mina n1) else
				  Some (tmp, make_and (offset_mina n1) (offset_maxa n2))
		    end
	      end
	  | JCTnull ->
	      begin match n1o, n2o with
		| None, None -> None
		| Some n, None -> Some (tmp, offset_mina n)
		| None, Some n -> Some (tmp, offset_maxa n)
		| Some n1, Some n2 -> Some (tmp, make_and (offset_mina n1) (offset_maxa n2))
	      end
	  | _ -> None
	end
    | _ -> None
  in
  let e = expr_coerce ty e in
  match opt with
    | None -> None :: lets, e :: params
    | Some (tmp,a) -> Some (tmp, e, a) :: lets , (Var tmp) :: params

and type_assert ty e =
  List.as_singleton (fst (list_type_assert ty e ([],[])))


and value_assigned mark pos ty e =
  let assign_assert = type_assert ty e in
  let tmp_for_assert = match assign_assert with
    | None -> None
    | Some(tmp,e',a) -> 
	if not (safety_checking()) then None else Some(tmp,e',a)
  in
  match tmp_for_assert with
    | None -> 
	let e' = expr e in
	coerce ~check_int_overflow:(safety_checking()) 
	  mark e#pos ty e#typ e e'
    | Some(tmp,e',a) ->
	Let(tmp,e',make_check ~mark ~kind:IndexBounds pos (Assert(a,Var tmp)))

and expr e =
  let infunction = get_current_function () in
  let e' = match e#node with
    | JCEconst JCCnull -> Var "null"
    | JCEconst c -> Cte(const c)
    | JCEvar v -> var v
    | JCEunary(op,e1) ->
        let e1' = expr e1 in
        make_app (unary_op op) 
          [ coerce ~check_int_overflow:(safety_checking()) 
              e#mark e#pos (native_operator_type op) e1#typ e1 e1' ]
(*     | JCEbinary(e1,(`Bsub,`Pointer),e2) -> *)
(*         let e1' = make_app "address" [ expr e1 ] in *)
(*         let e2' = make_app "address" [ expr e2 ] in *)
(* 	let st = pointer_struct e1#typ in *)
(* 	let s = string_of_int (struct_size_in_bytes st) in *)
(*         make_app *)
(* 	  (bin_op (`Bdiv,`Integer)) *)
(* 	  [ make_app (bin_op (`Bsub,`Integer)) [ e1'; e2' ]; *)
(* 	    Cte(Prim_int s) ] *)
    | JCEbinary(e1,(_,(`Pointer | `Logic) as op),e2) ->
        let e1' = expr e1 in
        let e2' = expr e2 in
        make_app (bin_op op) [e1'; e2']
    | JCEbinary(e1,(`Bland,_),e2) ->
        let e1' = expr e1 in
        let e2' = expr e2 in
        (* lazy conjunction *)
        And(e1',e2')    
    | JCEbinary(e1,(`Blor,_),e2) ->
        let e1' = expr e1 in
        let e2' = expr e2 in
        (* lazy disjunction *)
        Or(e1',e2')
    | JCEbinary(e1,(#arithmetic_op as op,(`Double|`Float as format)),e2) ->
	let e1' = expr e1 in
        let e2' = expr e2 in
        make_guarded_app
	  ~mark:e#mark DivByZero e#pos
	  (float_operator op)
	  [Var (float_format format); Var "nearest_even" ; e1' ; e2' ]
    | JCEbinary(e1,(_,#native_operator_type as op),e2) ->
        let e1' = expr e1 in
        let e2' = expr e2 in
        let ty = native_operator_type op in
        let mk = match fst op with
          | `Bdiv | `Bmod -> make_guarded_app ~mark:e#mark DivByZero e#pos
          | _ -> make_app
	in
        mk (bin_op op) 
          [coerce ~check_int_overflow:(safety_checking()) 
             e1#mark e1#pos ty e1#typ e1 e1'; 
           coerce ~check_int_overflow:(safety_checking()) 
             e2#mark e2#pos ty e2#typ e2 e2'] 
    | JCEshift(e1,e2) -> 
	(* TODO: bitwise !!!!!!!!!!!!!!!!!!!!! *)
        let e1' = expr e1 in
        let e2' = expr e2 in
        make_app "shift" 
          [e1'; 
	   coerce ~check_int_overflow:(safety_checking()) 
             e2#mark e2#pos integer_type e2#typ e2 e2']
    | JCEif(e1,e2,e3) -> 
        let e1' = expr e1 in
        let e2' = expr e2 in
        let e3' = expr e3 in
        If(e1',e2',e3')
    | JCEoffset(k,e1,st) -> 
	let ac = deref_alloc_class ~type_safe:false e1 in
        let alloc = alloc_table_var (ac,e1#region) in
	begin match ac with
	  | JCalloc_root _ ->
              let f = match k with
		| Offset_min -> "offset_min"
		| Offset_max -> "offset_max"
              in
	      make_app f [alloc; expr e1] 
	  | JCalloc_bitvector -> 
              let f = match k with
		| Offset_min -> "offset_min_bytes"
		| Offset_max -> "offset_max_bytes"
              in
	      let s = string_of_int (struct_size_in_bytes st) in
	      make_app f [alloc; expr e1; Cte(Prim_int s)] 
	end
    | JCEaddress(Addr_absolute,e1) -> 
        make_app "absolute_address" [ expr e1 ] 
    | JCEaddress(Addr_pointer,e1) -> 
        make_app "address" [ expr e1 ] 
    | JCEbase_block(e1) -> 
        make_app "base_block" [ expr e1 ] 
    | JCEinstanceof(e1,st) ->
        let e1' = expr e1 in
        let tag = tag_table_var (struct_root st,e1#region) in
        (* always safe *)
        make_app "instanceof_" [ tag; e1'; Var(tag_name st) ]
    | JCEcast(e1,st) ->
        if struct_of_union st then 
	  expr e1 
	else
          let e1' = expr e1 in
          let tmp = tmp_var_name () in
          let tag = tag_table_var (struct_root st,e1#region) in
	  let downcast_fun = 
	    if safety_checking () then "downcast_" else "safe_downcast_"
	  in
          let call = 
            make_guarded_app e#mark DownCast e#pos downcast_fun
	      [ tag; Var tmp; Var(tag_name st) ]
          in
          Let(tmp,e1',call) (* Yannick: why a temporary here? *)
    | JCEbitwise_cast(e1,_st) ->
	expr e1
    | JCErange_cast(e1,ri) ->
        let e1' = expr e1 in
        coerce ~check_int_overflow:(safety_checking())
          e#mark e#pos (JCTenum ri) e1#typ e1 e1'
    | JCEreal_cast(e1,rc) ->
        let e1' = expr e1 in
        begin match rc with
          | Integer_to_real ->
              coerce ~check_int_overflow:(safety_checking())
                e#mark e#pos real_type e1#typ e1 e1'
          | Double_to_real ->
              coerce ~check_int_overflow:(safety_checking())
                e#mark e#pos real_type e1#typ e1 e1'
	  | Float_to_real ->
              coerce ~check_int_overflow:(safety_checking())
                e#mark e#pos real_type e1#typ e1 e1'
          | Real_to_integer ->
              coerce ~check_int_overflow:(safety_checking())
                e#mark e#pos integer_type e1#typ e1 e1'
          | Round_double rm ->
              coerce ~check_int_overflow:(safety_checking())
                e#mark e#pos double_type e1#typ e1 e1'
	  | Round_float rm ->
              coerce ~check_int_overflow:(safety_checking())
                e#mark e#pos float_type e1#typ e1 e1'
        end
    | JCEderef(e1,fi) ->
  	make_deref e#mark e#pos e1 fi
    | JCEalloc(e1,st) ->
	let e1' = expr e1 in
	let ac = deref_alloc_class ~type_safe:false e in
        let alloc = plain_alloc_table_var (ac,e#region) in
	let pc = JCtag(st,[]) in
	let args = alloc_arguments (ac,e#region) pc in
        if !Jc_options.inv_sem = InvOwnership then
          let tag = plain_tag_table_var (struct_root st,e#region) in
          let mut = mutable_name pc in
          let com = committed_name pc in
          make_app "alloc_parameter_ownership" 
            [alloc; Var mut; Var com; tag; Var (tag_name st); 
             coerce ~check_int_overflow:(safety_checking()) 
	       e1#mark e1#pos integer_type 
	       e1#typ e1 e1']
	else
          make_guarded_app e#mark
            AllocSize e#pos
            (alloc_param_name ~check_size:(safety_checking()) ac pc)
            (coerce ~check_int_overflow:(safety_checking()) 
	       e1#mark e1#pos integer_type 
	       e1#typ e1 e1'
             :: args)
    | JCEfree e1 ->
	let e1' = expr e1 in
	let ac = deref_alloc_class ~type_safe:false e1 in
        let alloc = plain_alloc_table_var (ac,e1#region) in
        if !Jc_options.inv_sem = InvOwnership then
	  let pc = pointer_class e1#typ in
	  let com = committed_name pc in
	  make_app "free_parameter_ownership" 
	    [alloc; Var com; e1']
        else
	  let free_fun = 
	    if safety_checking () then "free_parameter" 
	    else "safe_free_parameter"
	  in
	  make_app free_fun [alloc; e1']


    | JCEapp call ->
	begin match call.jc_call_fun with
          | JClogic_fun f -> 
              let arg_types_asserts, args =
		try match f.jc_logic_info_parameters with
		  | [] -> [], []
		  | params -> 
		      let param_types = 
			List.map (fun v -> v.jc_var_info_type) params 
		      in
		      List.fold_right2 list_type_assert
			param_types call.jc_call_args ([],[])
		with Invalid_argument _ -> assert false
              in
	      let param_assoc = 
		List.map2 (fun param arg -> param,arg) 
		  f.jc_logic_info_parameters call.jc_call_args
	      in
	      let with_body = 
		let _f,body =  
		  Hashtbl.find Jc_typing.logic_functions_table 
		    f.jc_logic_info_tag 
		in
		match body with
		  | JCTerm _ -> true
		  | JCReads _ -> false
		  | JCAssertion _ | JCInductive _ -> assert false
	      in
	      let pre, fname, locals, prolog, epilog, args = 
		make_arguments 
                  ~callee_reads: f.jc_logic_info_effects
                  ~callee_writes: empty_effects
                  ~region_assoc: call.jc_call_region_assoc
		  ~param_assoc ~with_globals:true ~with_body
		  f.jc_logic_info_final_name args
	      in
	      assert (pre = LTrue);
	      assert (fname = f.jc_logic_info_final_name);
              let call = make_logic_app fname args in
              let arg_types_assert =
		List.fold_right
		  (fun opt acc -> 
		     match opt with
                       | None -> acc
                       | Some(_tmp,_e,a) -> make_and a acc)
		  arg_types_asserts LTrue
              in
              let call = 
		if arg_types_assert = LTrue || not (safety_checking()) then 
		  call
		else
		  Assert(arg_types_assert,call) 
              in
              let call =
		List.fold_right
		  (fun opt c -> 
		     match opt with
                       | None -> c
                       | Some(tmp,e,_ass) -> Let(tmp,e,c))
		  arg_types_asserts call
              in
	      let call = append prolog call in
	      let call = 
		if epilog = Void then call else 
		  let tmp = tmp_var_name () in
		  Let(tmp,call,append epilog (Var tmp))
	      in
              define_locals ~writes:locals call

          | JCfun f ->
              let arg_types_asserts, args =
		try match f.jc_fun_info_parameters with
		  | [] -> [], []
		  | params -> 
		      let param_types = 
			List.map (fun v -> v.jc_var_info_type) params 
		      in
		      List.fold_right2 list_type_assert
			param_types call.jc_call_args ([],[])
		with Invalid_argument _ -> assert false
              in
	      let args = 
		List.fold_right2 
		  (fun e e' acc ->
		     let e' =
		       if is_pointer_type e#typ && Region.bitwise e#region then
			 let st = pointer_struct e#typ in
			 let vi = struct_root st in
			 make_app (of_pointer_address_name vi) [ e' ]
		       else e'
		     in
		     e' :: acc
		  ) call.jc_call_args args []
	      in
	      let param_assoc = 
		List.map2 (fun param arg -> param,arg) 
		  f.jc_fun_info_parameters call.jc_call_args
	      in
	      let fname = 
		if default_checking () then 
		  f.jc_fun_info_final_name ^ "_requires"
		else f.jc_fun_info_final_name
	      in
	      let with_body = 
		let _f,_loc,_s,body =  
		  Hashtbl.find Jc_typing.functions_table f.jc_fun_info_tag 
		in
		body <> None
	      in
	      let args =
		match f.jc_fun_info_builtin_treatment with
		  | None -> args
		  | Some (TreatGenFloat format) ->
		      (Var (float_format format))::(Var "nearest_even")::args
	      in
	      let pre, fname, locals, prolog, epilog, args = 
		make_arguments 
                  ~callee_reads: f.jc_fun_info_effects.jc_reads
                  ~callee_writes: f.jc_fun_info_effects.jc_writes
                  ~region_assoc: call.jc_call_region_assoc
		  ~param_assoc ~with_globals:false ~with_body
		  fname args
	      in
	      let call = make_guarded_app e#mark UserCall e#pos fname args in
	      let call = 
		if is_pointer_type e#typ && Region.bitwise e#region then
		  make_app "pointer_address" [ call ]
		else call
	      in
	      let call = 
		if pre = LTrue || not (default_checking()) then 
		  call
		else
		  make_check ~mark:e#mark (* ~kind:Separation *) e#pos 
		    (Assert(pre,call))
	      in
              let arg_types_assert =
		List.fold_right
		  (fun opt acc -> 
		     match opt with
                       | None -> acc
                       | Some(_tmp,_e,a) -> make_and a acc)
		  arg_types_asserts LTrue
              in
              let call = 
		if arg_types_assert = LTrue || not (safety_checking()) then 
		  call
		else
		  make_check ~mark:e#mark ~kind:IndexBounds e#pos 
		    (Assert(arg_types_assert,call))
              in
              let call =
		List.fold_right
		  (fun opt c -> 
		     match opt with
                       | None -> c
                       | Some(tmp,e,_ass) -> Let(tmp,e,c))
		  arg_types_asserts call
              in
	      let call = append prolog call in
	      let call = 
		if epilog = Void then call else 
		  let tmp = tmp_var_name () in
		  Let(tmp,call,append epilog (Var tmp))
	      in
              define_locals ~writes:locals call
	end
    | JCEassign_var(v,e1) -> 
	let e1' = value_assigned e#mark e#pos v.jc_var_info_type e1 in
	let e' = Assign(v.jc_var_info_final_name,e1') in
	if e#typ = Jc_pervasives.unit_type then e' else append e' (var v)
    | JCEassign_heap(e1,fi,e2) -> 
	let e2' = value_assigned e#mark e#pos fi.jc_field_info_type e2 in
	(* Define temporary variable for value assigned *)
	let tmp2 = tmp_var_name () in
	let v2 = Jc_pervasives.var fi.jc_field_info_type tmp2 in
	let e2 = 
	  new expr_with ~typ:fi.jc_field_info_type ~node:(JCEvar v2) e2 
	in
	(* Translate assignment *)
        let tmp1, lets, e' = make_upd e#mark e#pos e1 fi e2 in
	let lets = (tmp2,e2') :: lets in
        let e' = 
	  if (safety_checking()) && !Jc_options.inv_sem = InvOwnership then
            append (assert_mutable (LVar tmp1) fi) e' 
	  else e' 
	in
	let e' =
	  if e#typ = Jc_pervasives.unit_type then e' else append e' (Var tmp2)
	in
        let e' = make_lets lets e' in
        if !Jc_options.inv_sem = InvOwnership then
          append e' (assume_field_invariants fi)
        else e'
    | JCEblock el ->
        List.fold_right append (List.map expr el) Void
    | JCElet(v,e1,e2) -> 
        let e1' = match e1 with
          | None -> 
              any_value v.jc_var_info_type
          | Some e1 -> 
	      value_assigned e#mark e#pos v.jc_var_info_type e1
	in
	let e2' = expr e2 in
        if v.jc_var_info_assigned then 
	  Let_ref(v.jc_var_info_final_name,e1',e2')
        else 
	  Let(v.jc_var_info_final_name,e1',e2')
    | JCEreturn_void -> Raise(jessie_return_exception,None)     
    | JCEreturn(ty,e1) -> 
	let e1' = value_assigned e#mark e#pos ty e1 in
	let e' = Assign(jessie_return_variable,e1') in
	append e' (Raise(jessie_return_exception,None))
    | JCEunpack(st,e1,as_st) ->
        let e1' = expr e1 in 
        make_guarded_app e#mark Unpack e#pos
          (unpack_name st) [ e1'; Var (tag_name as_st) ]
    | JCEpack(st,e1,from_st) ->
        let e1' = expr e1 in 
        make_guarded_app e#mark Pack e#pos
          (pack_name st) [ e1'; Var (tag_name from_st) ]
    | JCEassert(b,asrt,a) -> 
	let a' = 
	  named_assertion 
	    ~type_safe:false ~global_assertion:false ~relocate:false
	    LabelHere LabelPre a
	in
	begin match asrt with
	  | Aassert | Ahint ->
	      if assert_in_current_behavior b then
		Assert(a',Void)
	      else if assume_in_current_behavior b then
		assumption [ a ] a'
	      else Void
	  | Aassume ->
	      if assume_in_current_behavior b then
		assumption [ a ] a'
	      else Void
	end
    | JCEloop(la,e1) ->
	let inv = 
	  List.map snd 
	    (List.filter 
	       (assert_in_current_behavior $ fst) la.jc_loop_invariant)
	in
	let assume_inv = 
	  List.map snd 
	    (List.filter
	       (assume_in_current_behavior $ fst) la.jc_loop_invariant)
	in
        let inv' = 
	  make_and_list 
	    (List.map 
	       (named_assertion 
		  ~type_safe:false ~global_assertion:false ~relocate:false
		  LabelHere LabelPre) inv) 
	in
        let assume_inv' = 
	  make_and_list 
	    (List.map 
	       (named_assertion 
		  ~type_safe:false ~global_assertion:false ~relocate:false
		  LabelHere LabelPre) assume_inv) 
	in
	(* free invariant: trusted or not *)
	let free_inv = la.jc_free_loop_invariant in
        let free_inv' = 
	  named_assertion 
	    ~type_safe:false ~global_assertion:false ~relocate:false
	    LabelHere LabelPre free_inv
        in
        let inv' = 
	  if Jc_options.trust_ai then inv' else make_and inv' free_inv' 
	in
	(* loop assigns  *)
	(* By default, the assigns clause for the function is taken *)
	(* TODO: add also a loop_assigns annotation *)
	(* Yannick: remove, as function loop assigns does not deal with
	   newly allocated memory, which may be assigned in loop *)
	let loop_assigns = None 
(* 	  List.fold_left *)
(* 	    (fun acc (pos,id,b) -> *)
(* 	       if safety_checking () || default_checking ()  *)
(* 		 || id = get_current_behavior () then *)
(* 		 match b.jc_behavior_assigns with *)
(* 		   | None -> acc *)
(* 		   | Some(_pos,loclist) ->  *)
(* 		       let loclist = List.map old_to_pre_loc loclist in *)
(* 		       match acc with *)
(* 			 | None -> Some loclist *)
(* 			 | Some loclist' -> Some (loclist @ loclist') *)
(* 	       else acc *)
(* 	    ) None (get_current_spec ()).jc_fun_behavior *)
	in
        let inv' = match loop_assigns with
	  | None -> inv'
	  | _ ->
	      let ass =
		assigns ~type_safe:false 
		  LabelPre infunction.jc_fun_info_effects loop_assigns e#pos
	      in
	      make_and inv' (mark_assertion e#pos ass)
	in
	(* loop body *) 
        let body = expr e1 in
	let add_assume s =
          let s = append (assumption assume_inv assume_inv') s in
          if Jc_options.trust_ai then
            append (assumption [ free_inv ] free_inv') s
          else s
        in
	let body = [ add_assume body ] in
	(* final generation, depending on presence of variant or not *)
        begin match la.jc_loop_variant with
          | Some t when default_checking () ->
              let variant = 
		named_term 
		  ~type_safe:false ~global_assertion:false ~relocate:false
		  LabelHere LabelPre t 
	      in
	      let variant = 
		term_coerce ~type_safe:false ~global_assertion:false LabelHere
		  t#pos integer_type t#typ t variant 
	      in
              While(Cte(Prim_bool true), inv', Some (variant,None), body)
          | _ ->
              While(Cte(Prim_bool true), inv', None, body)
        end
    | JCEcontract(req,dec,vi_result,behs,e) ->
	assert (req = None);
	assert (dec = None);
	begin match behs with
	  | [pos,id,b] ->
	      assert (b.jc_behavior_throws = None);
	      assert (b.jc_behavior_assumes = None);
	      assert (b.jc_behavior_assigns = None);
	      let a = 
		assertion 
		  ~type_safe:false ~global_assertion:false ~relocate:false 
		  LabelHere LabelOld b.jc_behavior_ensures
	      in
	      Triple(true,LTrue,expr e,a,[])
	  | _ -> assert false
	end
    | JCEthrow(exc,Some e1) -> 
        let e1' = expr e1 in
        Raise(exception_name exc,Some e1')
    | JCEthrow(exc, None) -> 
        Raise(exception_name exc,None)
    | JCEtry (s, catches, finally) -> 
(*      assert (finally.jc_statement_node = JCEblock []); (\* TODO *\) *)
        let catch (s,excs) (ei,v_opt,st) =
          if ExceptionSet.mem ei excs then
            (Try(s, 
                 exception_name ei, 
                 Option_misc.map (fun v -> v.jc_var_info_final_name) v_opt,
                 expr st),
             ExceptionSet.remove ei excs)
          else
            begin
(* YMo: too many questions about warning due to generated Jessie *)	      
(*               eprintf "Warning: exception %s cannot be thrown@." *)
(*                 ei.jc_exception_info_name; *)
              (s,excs)
            end
        in
        let ef = Jc_effect.expr empty_fun_effect s in
        let (s,_) =
          List.fold_left catch (expr s,ef.jc_raises) catches
        in s
    | JCEmatch (e, psl) ->
        let tmp = tmp_var_name () in
        let body = pattern_list_expr expr (LVar tmp) e#region
          e#typ psl in
        Let(tmp, expr e, body)
  in
  let e' = 
    if e#typ = Jc_pervasives.unit_type 
      && e#original_type <> Jc_pervasives.unit_type then
	(* Create dummy temporary *)
	let tmp = tmp_var_name () in
	Let(tmp,e',Void)
    else e'
  in 
  (* Ideally, only labels used in logical annotations should be kept *)
  if e#mark = "" then e' else Label(e#mark,e')

and expr_coerce ty e =
  coerce ~check_int_overflow:(safety_checking())
    e#mark e#pos ty
    e#typ e (expr e)

(*****************************)
(* axioms, lemmas, goals   *)
(**************************)

let tr_axiom loc id ~is_axiom labels a acc =

  if Jc_options.debug then
    Format.printf "[interp] axiom %s@." id;

  let lab = match labels with [lab] -> lab | _ -> LabelHere in
  let ef = Jc_effect.assertion empty_effects a in
  let a' = 
    assertion ~type_safe:false ~global_assertion:true ~relocate:false lab lab a
  in
  let params = tmodel_parameters ~label_in_name:true ef in
  reg_decl 
    ~out_mark:id
    ~in_mark:id
    ~name:id
    ~beh:(if is_axiom then "axiom" else "lemma")
    loc;
  let a' = List.fold_right (fun (n,_v,ty') a' -> LForall(n,ty',a')) params a' in
  if is_axiom then 
    Axiom(id,a') :: acc 
  else 
    Goal(id,a') :: Axiom(id ^ "_as_axiom",a') :: acc



(******************************************************************************)
(*                               Logic functions                              *)
(******************************************************************************)

(* let tr_logic_const vi init acc = *)
(*   let decl = *)
(*     Logic (false, vi.jc_var_info_final_name, [], tr_base_type vi.jc_var_info_type) :: acc *)
(*   in *)
(*     match init with *)
(*       | None -> decl *)
(*       | Some(t,ty) -> *)
(*           let t' = term ~type_safe ~global_assertion:true ~relocate:false LabelHere LabelHere t in *)
(*           let vi_ty = vi.jc_var_info_type in *)
(*           let t_ty = t#typ in *)
(*           (\* eprintf "logic const: max type = %a@." print_type ty; *\) *)
(*           let pred = *)
(*             LPred ( *)
(*               "eq", *)
(*               [term_coerce Loc.dummy_position ty vi_ty t (LVar vi.jc_var_info_name);  *)
(*                term_coerce t#pos ty t_ty t t']) *)
(*           in *)
(*         let ax = *)
(*           Axiom( *)
(*             vi.jc_var_info_final_name ^ "_value_axiom", *)
(*             bind_pattern_lets pred *)
(*           ) *)
(*         in *)
(*         ax::decl *)

let tr_logic_fun f ta acc =

  if Jc_options.debug then
    Format.printf "[interp] logic function %s@." f.jc_logic_info_final_name;

  let lab = 
    match f.jc_logic_info_labels with [lab] -> lab | _ -> LabelHere
  in
  let fa = 
    assertion ~type_safe:false ~global_assertion:true ~relocate:false lab lab 
  in
  let ft =
    term ~type_safe:false ~global_assertion:true ~relocate:false lab lab 
  in
  let term_coerce = term_coerce ~type_safe:false ~global_assertion:true lab in
  let params =
    List.map (tparam ~label_in_name:true lab) f.jc_logic_info_parameters
  in  
  let model_params = 
    tmodel_parameters ~label_in_name:true f.jc_logic_info_effects 
  in
  let params = params @ model_params in
  let params = List.map (fun (n,_v,ty') -> (n,ty')) params in

  (* Function definition *)
  let acc =  
    match f.jc_logic_info_result_type, ta with
      | None, JCAssertion a -> (* Predicate *)
          let body = fa a in
          Predicate(false, f.jc_logic_info_final_name, params, body) :: acc
      | Some ty, JCTerm t -> (* Function *)
          let ty' = tr_base_type ty in
          let t' = ft t in
	  let t' = term_coerce t#pos ty t#typ t t' in
          Function(false, f.jc_logic_info_final_name, params, ty', t') :: acc
      | ty_opt, JCReads r -> (* Logic *)
          let ty' = match ty_opt with
	    | None -> simple_logic_type "prop"
	    | Some ty -> tr_base_type ty
          in
          Logic(false, f.jc_logic_info_final_name, params, ty') :: acc
(*
      | ty_opt, JCAxiomatic l  ->
          let ty' = match ty_opt with
	    | None -> simple_logic_type "prop"
	    | Some ty -> tr_base_type ty
          in
	  let acc =
	    List.fold_right
	      (fun (id,a) acc -> 
		 tr_axiom id#pos id#name ~is_axiom:true f.jc_logic_info_labels a acc)
	      l acc 
	  in
	  Logic(false, f.jc_logic_info_final_name, params, ty') :: acc
*)
      | None, JCInductive l  ->
	  Inductive(false, f.jc_logic_info_final_name, params,  
		    List.map 
		      (fun (id,labels,a) ->
			 let ef = Jc_effect.assertion empty_effects a in
			 let a' = fa a in
			 let params = 
			   tmodel_parameters ~label_in_name:true ef 
			 in
			 let a' = 
			   List.fold_right 
			     (fun (n,_v,ty') a' -> LForall(n,ty',a')) params a' 
			 in 
			 (id#name, a')) l) :: acc
      | Some _, JCInductive _ -> assert false
      | None, JCTerm _ -> assert false 
      | Some _, JCAssertion _ -> assert false 
  in

  (* no_update axioms *)
(* TODO: use computed effects instead
  let acc = 
    match ta with 
      |	JCAssertion _ | JCTerm _ | JCInductive _ -> acc 
      | JCReads pset ->
    let memory_params_reads = 
      tmemory_detailed_params ~label_in_name:true f.jc_logic_info_effects
    in
    let params_names = List.map fst params in
    let normal_params = List.map (fun name -> LVar name) params_names in
    snd (List.fold_left
      (fun (count,acc) param ->
	 let paramty = snd param in
	 if not (is_memory_type paramty) then count,acc else
	   let (mc,r),_ = (* Recover which memory it is exactly *)
	     List.find (fun ((mc,r),(n,_v,_ty')) -> n = fst param) 
	       memory_params_reads
	   in
	   let zonety,basety = deconstruct_memory_type_args paramty in
	   let pset = 
	     reads ~type_safe:false ~global_assertion:true pset (mc,r) 
	   in
	   let sepa = LNot(LPred("in_pset",[LVar "tmp";pset])) in
	   let update_params = 
             List.map (fun name ->
			 if name = fst param then
			   LApp("store",[LVar name;LVar "tmp";LVar "tmpval"])
			 else LVar name
		      ) params_names
	   in
	   let a = 
             match f.jc_logic_info_result_type with
	       | None ->
		   LImpl(
                     sepa,
                     LIff(
		       LPred(f.jc_logic_info_final_name,normal_params),
		       LPred(f.jc_logic_info_final_name,update_params)))
	       | Some rety ->
		   LImpl(
                     sepa,
                     LPred("eq",[
			     LApp(f.jc_logic_info_final_name,normal_params);
			     LApp(f.jc_logic_info_final_name,update_params)]))
	   in
	   let a = 
             List.fold_left (fun a (name,ty) -> LForall(name,ty,a)) a params
	   in
	   let a = 
             LForall(
	       "tmp",raw_pointer_type zonety,
	       LForall(
		 "tmpval",basety,
		 a))
	   in
	   let name = 
	     "no_update_" ^ f.jc_logic_info_name ^ "_" ^ string_of_int count
	   in
	   count + 1, Axiom(name,a) :: acc
      ) (0,acc) params)
  in
*)

  (* no_assign axioms *)
(* TODO: use computed effects instead
  let acc = 
    match ta with 
      | JCAssertion _ | JCTerm _ | JCInductive _ -> acc 
      | JCReads pset ->
    let memory_params_reads = 
      tmemory_detailed_params ~label_in_name:true f.jc_logic_info_effects
    in
    let params_names = List.map fst params in
    let normal_params = List.map (fun name -> LVar name) params_names in
    snd (List.fold_left
      (fun (count,acc) param ->
	 let paramty = snd param in
	 if not (is_memory_type paramty) then count,acc else
	   let (mc,r),_ = (* Recover which memory it is exactly *)
	     List.find (fun ((mc,r),(n,_v,_ty')) -> n = fst param) 
	       memory_params_reads
	   in
	   let zonety,basety = deconstruct_memory_type_args paramty in
	   let pset = 
	     reads ~type_safe:false ~global_assertion:true pset (mc,r) 
	   in
	   let sepa = LPred("pset_disjoint",[LVar "tmp";pset]) in
	   let upda = 
	     LPred("not_assigns",[LVar "tmpalloc"; LVar (fst param);
				  LVar "tmpmem"; LVar "tmp"])
	   in
	   let update_params = 
             List.map (fun name ->
			 if name = fst param then LVar "tmpmem"
			 else LVar name
		      ) params_names
	   in
	   let a = 
             match f.jc_logic_info_result_type with
	       | None ->
		   LImpl(
                     make_and sepa upda,
                     LIff(
		       LPred(f.jc_logic_info_final_name,normal_params),
		       LPred(f.jc_logic_info_final_name,update_params)))
	       | Some rety ->
		   LImpl(
                     make_and sepa upda,
                     LPred("eq",[
			     LApp(f.jc_logic_info_final_name,normal_params);
			     LApp(f.jc_logic_info_final_name,update_params)]))
	   in
	   let a = 
             List.fold_left (fun a (name,ty) -> LForall(name,ty,a)) a params
	   in
	   let a = 
             LForall(
	       "tmp",raw_pset_type zonety,
               LForall(
		 "tmpmem",paramty,
		 LForall(
		   "tmpalloc",raw_alloc_table_type zonety,
		 a)))
	   in
	   let name = 
	     "no_assign_" ^ f.jc_logic_info_name ^ "_" ^ string_of_int count
	   in
	   count + 1, Axiom(name,a) :: acc
      ) (0,acc) params) (* memory_param_reads ? *)
  in
*)

  (* alloc_extend axioms *)
(* TODO: use computed effects instead
  let acc = 
    match ta with 
      | JCAssertion _ | JCTerm _ | JCInductive _ -> acc 
      | JCReads ps ->
    let alloc_params_reads = 
      talloc_table_params ~label_in_name:true f.jc_logic_info_effects
    in
    let params_names = List.map fst params in
    let normal_params = List.map (fun name -> LVar name) params_names in
    snd (List.fold_left
      (fun (count,acc) (n,_v,paramty) ->
	 assert (is_alloc_table_type paramty);
	 let exta = 
	   LPred("alloc_extends",[LVar n; LVar "tmpalloc"])
	 in
	 let ps = 
	   List.map 
	     (collect_pset_locations ~type_safe:false ~global_assertion:true) ps
	 in
	 let ps = location_list' ps in
	 let valida =
	   LPred("valid_pset",[LVar n; ps])
	 in
	 let update_params = 
           List.map (fun name ->
		       if name = n then LVar "tmpalloc"
		       else LVar name
		    ) params_names
	 in
	 let a = 
           match f.jc_logic_info_result_type with
	     | None ->
		 LImpl(
                   make_and exta valida,
                   LIff(
		     LPred(f.jc_logic_info_final_name,normal_params),
		     LPred(f.jc_logic_info_final_name,update_params)))
	     | Some rety ->
		 LImpl(
                   make_and exta valida,
                   LPred("eq",[
			   LApp(f.jc_logic_info_final_name,normal_params);
			   LApp(f.jc_logic_info_final_name,update_params)]))
	 in
	 let a = 
           List.fold_left (fun a (name,ty) -> LForall(name,ty,a)) a params
	 in
	 let a = 
	   LForall(
	     "tmpalloc",paramty,
	     a)
	 in
	 let name = 
	   "alloc_extend_" ^ f.jc_logic_info_name ^ "_" ^ string_of_int count
	 in
	 count + 1, Axiom(name,a) :: acc
      ) (0,acc) alloc_params_reads)
  in
*)

  acc

(*   (\* full_separated axioms. *\) *)
(*   let sep_preds =  *)
(*     List.fold_left (fun acc vi -> *)
(*       match vi.jc_var_info_type with *)
(*         | JCTpointer(st,_,_) ->  *)
(*             LPred("full_separated",[LVar "tmp"; LVar vi.jc_var_info_final_name]) *)
(*             :: acc *)
(*         | _ -> acc *)
(*     ) [] li.jc_logic_info_parameters *)
(*   in *)
(*   if List.length sep_preds = 0 then acc else *)
(*     let params_names = List.map fst params_reads in *)
(*     let normal_params = List.map (fun name -> LVar name) params_names in *)
(*     MemoryMap.fold (fun (mc,r) labels acc -> *)
(*       let update_params =  *)
(*         List.map (fun name -> *)
(*           if name = memory_name(mc,r) then *)
(*             LApp("store",[LVar name;LVar "tmp";LVar "tmpval"]) *)
(*           else LVar name *)
(*         ) params_names *)
(*       in *)
(*       let a =  *)
(*         match li.jc_logic_info_result_type with *)
(*           | None -> *)
(*               LImpl( *)
(*                 make_and_list sep_preds, *)
(*                 LIff( *)
(*                   LPred(li.jc_logic_info_final_name,normal_params), *)
(*                   LPred(li.jc_logic_info_final_name,update_params))) *)
(*           | Some rety -> *)
(*               LImpl( *)
(*                 make_and_list sep_preds, *)
(*                 LPred("eq",[ *)
(*                   LApp(li.jc_logic_info_final_name,normal_params); *)
(*                   LApp(li.jc_logic_info_final_name,update_params)])) *)
(*       in *)
(*       let a =  *)
(*         List.fold_left (fun a (name,ty) -> LForall(name,ty,a)) a params_reads *)
(*       in *)
(*       let structty = match mc with  *)
(*         | FVfield fi -> JCtag(fi.jc_field_info_struct, []) *)
(*         | FVvariant vi -> JCvariant vi *)
(*       in *)
(*       let basety = match mc with *)
(*         | FVfield fi -> tr_base_type fi.jc_field_info_type *)
(*         | FVvariant vi ->  *)
(*             if integral_union vi then why_integer_type else *)
(*               simple_logic_type (union_memory_type_name vi) *)
(*       in *)
(*       let a =  *)
(*         LForall( *)
(*           "tmp",pointer_type structty, *)
(*           LForall( *)
(*             "tmpval",basety, *)
(*             a)) *)
(*       in *)
(*       let mcname = match mc with *)
(*         | FVfield fi -> fi.jc_field_info_name *)
(*         | FVvariant vi -> vi.jc_root_info_name *)
(*       in *)
(*       Axiom( *)
(*         "full_separated_" ^ li.jc_logic_info_name ^ "_" ^ mcname, *)
(*         a) :: acc *)
(*     ) li.jc_logic_info_effects.jc_effect_memories acc *)


(***********************************)
(*             axiomatic decls     *)
(***********************************)


let tr_axiomatic_decl acc d =
  match d with
    | Jc_typing.ABaxiom(loc,id,labels,p) -> 
	tr_axiom loc ~is_axiom:true id labels p acc

(******************************************************************************)
(*                                 Functions                                  *)
(******************************************************************************)

let excep_posts_for_others exc_opt excep_behaviors =
  ExceptionMap.fold
    (fun exc bl acc ->
       match exc_opt with 
         | Some exc' -> 
	     if exc.jc_exception_info_tag = exc'.jc_exception_info_tag then
	       acc
	     else
	       (exception_name exc, LTrue) :: acc
         | None -> (exception_name exc, LTrue) :: acc
    ) excep_behaviors []
    
let fun_parameters ~type_safe params write_params read_params =
  let write_params = 
    List.map (fun (n,ty') -> (n,Ref_type(Base_type ty'))) write_params
  in
  let read_params = 
    List.map (fun (n,ty') -> (n,Base_type ty')) read_params
  in
  let params = 
    List.map (fun v ->
		let n,ty' = param ~type_safe v in
		(n, Base_type ty')
	     ) params
  in
  let params = params @ write_params @ read_params in
  match params with
    | [] -> [ ("tt", unit_type) ]
    | _ -> params

let annot_fun_parameters params write_params read_params annot_type =
  let params = fun_parameters ~type_safe:true params write_params read_params in
  List.fold_right (fun (n,ty') acc -> Prod_type(n, ty', acc))
    params annot_type
       
let function_body f spec behavior_name body =
  set_current_behavior behavior_name;
  set_current_spec spec;
  let e' = expr body in
  reset_current_behavior ();
  reset_current_spec ();
  e'

(* Only used for internal function, hence type-safe parameter set to false *)
let assume_in_precondition b pre =
  match b.jc_behavior_assumes with
    | None -> pre
    | Some a ->
	let a' = 
	  assertion ~type_safe:false ~global_assertion:false ~relocate:false
	    LabelHere LabelHere a
	in
	make_and a' pre

(* Only used for external prototype, hence type-safe parameter set to true *)
let assume_in_postcondition b post =
  match b.jc_behavior_assumes with
    | None -> post
    | Some a ->
	let a' = 
	  assertion ~type_safe:true ~global_assertion:false ~relocate:true 
	    LabelOld LabelOld a
	in
	make_impl a' post

let function_prototypes = Hashtbl.create 0 
  
let get_valid_pred_app vi =
  match vi.jc_var_info_type with
    | JCTpointer (pc, n1o, n2o) ->
	(* TODO: what about bitwise? *)
	let v' = 
          term ~type_safe:false ~global_assertion:false ~relocate:false
	    LabelHere LabelHere (new term_var vi) 
	in
	  begin match n1o, n2o with
            | None, None -> LTrue
	    | Some n, None ->
		let ac = alloc_class_of_pointer_class pc in
                let a' =
		  make_valid_pred_app ~equal:false
		    (ac, vi.jc_var_info_region) pc
                    v' (Some (const_of_num n)) None
                in
		  bind_pattern_lets a'
	    | None, Some n -> 
		let ac = alloc_class_of_pointer_class pc in
                let a' =
		  make_valid_pred_app ~equal:false 
		    (ac, vi.jc_var_info_region) pc
                    v' None (Some (const_of_num n))
                in
		  bind_pattern_lets a'
            | Some n1, Some n2 ->
		let ac = alloc_class_of_pointer_class pc in
                let a' =
		  make_valid_pred_app ~equal:false (ac, vi.jc_var_info_region) pc
                    v' (Some (const_of_num n1)) (Some (const_of_num n2))
                in
		  bind_pattern_lets a'
	  end
    | JCTnative _ | JCTlogic _ | JCTenum _ | JCTnull | JCTany
    | JCTtype_var _ -> LTrue
	
let tr_fun f funpos spec body acc =

  if Jc_options.debug then
    Format.printf "[interp] function %s@." f.jc_fun_info_name;

  (* global variables valid predicates *)
  let variables_valid_pred_apps = LTrue
(* Yannick: comment out because not taken into account in effects
    Hashtbl.fold 
      (fun _ (vi, _) acc ->
         let req = get_valid_pred_app vi in
	   make_and req acc) 
      Jc_typing.variables_table LTrue
*)
  in
    
  (* precondition for calling the function and extra one for analyzing it *)

  let external_requires = 
    named_assertion ~type_safe:true ~global_assertion:false ~relocate:false
      LabelHere LabelHere spec.jc_fun_requires
  in
  let external_requires = 
    if Jc_options.trust_ai then 
      external_requires
    else
      let free_requires = 
	named_assertion ~type_safe:true ~global_assertion:false ~relocate:false
	  LabelHere LabelHere spec.jc_fun_free_requires
      in 
	make_and external_requires free_requires
  in
    
  let internal_requires = 
    named_assertion ~type_safe:false ~global_assertion:false ~relocate:false
      LabelHere LabelHere spec.jc_fun_requires
  in
  let free_requires = 
    named_assertion ~type_safe:false ~global_assertion:false ~relocate:false 
      LabelHere LabelHere spec.jc_fun_free_requires
  in
  let free_requires = make_and variables_valid_pred_apps free_requires in
  let internal_requires = make_and internal_requires free_requires in
  let internal_requires =
    List.fold_left 
      (fun acc v ->
         let req = get_valid_pred_app v in
	   make_and req acc)
      internal_requires f.jc_fun_info_parameters
  in
    
  (* partition behaviors as follows:
     - (optional) 'safety' behavior (if Arguments Invariant Policy is selected)
     - (optional) 'inferred' behaviors (computed by analysis)
     - user defined behaviors *)

  let behaviors = spec.jc_fun_default_behavior :: spec.jc_fun_behavior in
  let (safety_behavior,
       normal_behaviors_inferred, normal_behaviors, 
       excep_behaviors_inferred, excep_behaviors) =
    List.fold_left
      (fun (safety,normal_inferred,normal,excep_inferred,excep) (pos,id,b) ->
	 let make_post ~type_safe ~internal =
	   let post =
	     if internal && Jc_options.trust_ai then
	       b.jc_behavior_ensures 
	     else
	       Assertion.mkand [ b.jc_behavior_ensures; 
				 b.jc_behavior_free_ensures ] ()
	   in
	   let post = 
	     named_assertion 
	       ~type_safe ~global_assertion:false ~relocate:false 
	       LabelPost LabelOld post
	   in
           let post = match b.jc_behavior_assigns with
             | None -> post
             | Some(assigns_pos,loclist) ->
		 let assigns_post = 
                   mark_assertion assigns_pos
                     (assigns ~type_safe 
			~region_list:f.jc_fun_info_param_regions
			LabelOld f.jc_fun_info_effects (Some loclist) funpos)
		 in
		 mark_assertion pos (make_and post assigns_post)
	   in post
	 in
	 let internal_post = make_post ~type_safe:false ~internal:true in
	 let external_post = make_post ~type_safe:true ~internal:false in
	 let behav = (id,b,internal_post,external_post) in
         match b.jc_behavior_throws with
           | None -> 
               begin match id with
                 | "safety" ->
		     assert (b.jc_behavior_assumes = None);
		     let internal_post = 
		       make_and variables_valid_pred_apps internal_post 
		     in
		     let external_post = 
		       make_and variables_valid_pred_apps external_post 
		     in
                     (id, b, internal_post, external_post) :: safety, 
                     normal_inferred, normal, excep_inferred, excep
                 | "inferred" -> 
		     assert (b.jc_behavior_assumes = None);
                     safety, behav :: normal_inferred, 
                     (if Jc_options.trust_ai then normal else behav :: normal),
                     excep_inferred, excep
                 | _ -> 
                     safety, normal_inferred, behav :: normal, 
                     excep_inferred, excep
               end
           | Some exc ->
               if id = "inferred" then 
		 begin
		   assert (b.jc_behavior_assumes = None);
                   safety, normal_inferred, normal, 
		   ExceptionMap.add_merge 
		     List.append exc [behav] excep_inferred, 
		   if Jc_options.trust_ai then excep else 
                     ExceptionMap.add_merge List.append exc [behav] excep
		 end
               else
                 safety, normal_inferred, normal, excep_inferred, 
                 ExceptionMap.add_merge List.append exc [behav] excep)
      ([], [], [], ExceptionMap.empty, ExceptionMap.empty)
      behaviors
  in
  let user_excep_behaviors = excep_behaviors in
  let excep_behaviors = 
    ExceptionSet.fold
      (fun exc acc -> 
         if ExceptionMap.mem exc acc then acc else
           let b = 
             { default_behavior with 
                 jc_behavior_throws = Some exc; 
		 jc_behavior_ensures = (new assertion JCAtrue); } 
           in
           ExceptionMap.add 
	     exc [exc.jc_exception_info_name ^ "_b", b, LTrue, LTrue] acc)
      f.jc_fun_info_effects.jc_raises
      excep_behaviors
  in

  (* Effects, parameters and locals *)

  let external_write_effects = 
    write_effects 
      ~callee_reads:f.jc_fun_info_effects.jc_reads
      ~callee_writes:f.jc_fun_info_effects.jc_writes
      ~region_list:f.jc_fun_info_param_regions
      ~params:f.jc_fun_info_parameters
  in
  let external_read_effects = 
    read_effects 
      ~callee_reads:f.jc_fun_info_effects.jc_reads
      ~callee_writes:f.jc_fun_info_effects.jc_writes
      ~region_list:f.jc_fun_info_param_regions
      ~params:f.jc_fun_info_parameters
  in
  let external_write_params =
    write_parameters 
      ~type_safe:true
      ~callee_reads:f.jc_fun_info_effects.jc_reads
      ~callee_writes:f.jc_fun_info_effects.jc_writes
      ~region_list:f.jc_fun_info_param_regions
      ~params:f.jc_fun_info_parameters
  in
  let internal_write_params =
    write_parameters 
      ~type_safe:false
      ~callee_reads:f.jc_fun_info_effects.jc_reads
      ~callee_writes:f.jc_fun_info_effects.jc_writes
      ~region_list:f.jc_fun_info_param_regions
      ~params:f.jc_fun_info_parameters
  in
  let external_read_params =
    read_parameters 
      ~type_safe:true
      ~callee_reads:f.jc_fun_info_effects.jc_reads
      ~callee_writes:f.jc_fun_info_effects.jc_writes
      ~region_list:f.jc_fun_info_param_regions
      ~params:f.jc_fun_info_parameters
      ~already_used:(List.map fst external_write_params)
  in
  let internal_read_params =
    read_parameters 
      ~type_safe:false
      ~callee_reads:f.jc_fun_info_effects.jc_reads
      ~callee_writes:f.jc_fun_info_effects.jc_writes
      ~region_list:f.jc_fun_info_param_regions
      ~params:f.jc_fun_info_parameters
      ~already_used:(List.map fst internal_write_params)
  in
  let internal_write_locals =
    write_locals 
      ~callee_reads:f.jc_fun_info_effects.jc_reads
      ~callee_writes:f.jc_fun_info_effects.jc_writes
      ~region_list:f.jc_fun_info_param_regions
      ~params:f.jc_fun_info_parameters
  in
  let internal_read_locals =
    read_locals 
      ~callee_reads:f.jc_fun_info_effects.jc_reads
      ~callee_writes:f.jc_fun_info_effects.jc_writes
      ~region_list:f.jc_fun_info_param_regions
      ~params:f.jc_fun_info_parameters
  in
  let define_locals e' =
    define_locals ~reads:internal_read_locals ~writes:internal_write_locals e'
  in

  (* Postcondition *)

  let add_modif_postcondition 
      ~internal f (_id,b,internal_post,external_post) acc = 
    let post = if internal then internal_post else external_post in
    make_and (f b post) acc 
  in
  let add_postcondition ~internal = 
    add_modif_postcondition ~internal (fun _b post -> post) 
  in
  let internal_safety_post =
    List.fold_right (add_postcondition ~internal:true) safety_behavior LTrue
  in
  let external_safety_post =
    List.fold_right (add_postcondition ~internal:false) safety_behavior LTrue
  in
  let normal_post =
    List.fold_right 
      (add_modif_postcondition ~internal:false assume_in_postcondition) 
      normal_behaviors LTrue
  in
  let normal_post_inferred =
    List.fold_right (add_postcondition ~internal:false)
      normal_behaviors_inferred LTrue
  in
  let excep_posts =
    ExceptionMap.fold
      (fun exc bl acc ->
         let a' = 
	   List.fold_right (add_postcondition ~internal:false) bl LTrue 
	 in
         (exception_name exc, a') :: acc
      ) excep_behaviors []
  in
  let excep_posts_inferred =
    ExceptionMap.fold
      (fun exc bl acc ->
         let a' = 
           List.fold_right 
	     (add_modif_postcondition ~internal:false assume_in_postcondition)
	     bl LTrue
         in
	 (exception_name exc, a') :: acc
      ) excep_behaviors_inferred []
  in

  (* Function type *)

  let ret_type = tr_var_type f.jc_fun_info_result in
  let param_normal_post = 
    if is_purely_exceptional_fun spec then LFalse else
      make_and_list [external_safety_post; normal_post; normal_post_inferred] 
  in
  let param_excep_posts = excep_posts @ excep_posts_inferred in
  let acc = 
    let annot_type = (* function declaration with precondition *)
      Annot_type(external_requires, ret_type,
                 external_read_effects, external_write_effects, 
		 param_normal_post, param_excep_posts)
    in
    let fun_type = 
      annot_fun_parameters f.jc_fun_info_parameters 
	external_write_params external_read_params annot_type 
    in
    let newid = f.jc_fun_info_final_name ^ "_requires" in
    Hashtbl.add function_prototypes newid fun_type;
    Param(false, newid, fun_type) :: acc
  in
  let acc = (* function declaration without precondition *)
    let annot_type =
      Annot_type(LTrue, ret_type,
                 external_read_effects, external_write_effects, 
		 param_normal_post, param_excep_posts)
    in
    let fun_type = 
      annot_fun_parameters f.jc_fun_info_parameters 
	external_write_params external_read_params annot_type 
    in
    let newid = f.jc_fun_info_final_name in
    Hashtbl.add function_prototypes newid fun_type;
    Param(false, newid, fun_type) :: acc
  in

  (* Function body *)

  match body with
    | None -> acc (* function was only declared *)
    | Some body ->
        if Jc_options.verify <> [] && 
          not (List.mem f.jc_fun_info_name Jc_options.verify) 
        then 
          acc (* function is not in the list of functions to verify *) 
        else
          let () = 
	    printf "Generating Why function %s@." f.jc_fun_info_final_name 
	  in

	  (* parameters *)
	  let params = 
	    fun_parameters ~type_safe:false f.jc_fun_info_parameters 
	      internal_write_params internal_read_params 
	  in

	  let wrap_body f spec bname body =
	    (* rename formals after parameters are computed and before body
	       is treated *)
	    let list_of_refs =
	      List.fold_right
		(fun id bl ->
		   if id.jc_var_info_assigned
		   then 
		     let n = id.jc_var_info_final_name in
		     let newn = "mutable_" ^ n in
		     id.jc_var_info_final_name <- newn;
		     (newn, n) :: bl
		   else bl) 
		f.jc_fun_info_parameters [] 
	    in
            let body = function_body f spec bname body in
	    let body =
	      if !Jc_options.inv_sem = InvOwnership then
		append (assume_all_invariants f.jc_fun_info_parameters) body
	      else body
	    in
	    let body = define_locals body in
	    let body = match f.jc_fun_info_result.jc_var_info_type with
	      | JCTnative Tunit -> 
		  Try(append body (Raise(jessie_return_exception,None)),
		      jessie_return_exception, None, Void)
	      | _ ->
		  let e' = any_value f.jc_fun_info_result.jc_var_info_type in
		  Let_ref(jessie_return_variable, e',
			  Try(append body Absurd,
			      jessie_return_exception, None,
			      Deref jessie_return_variable))
	    in
	    let body = make_label "init" body in
	    let body =
	      List.fold_right
		(fun (mut_id,id) e' -> Let_ref(mut_id, plain_var id, e')) 
		list_of_refs body 
	    in
	    (* FS#393: restore parameter real names *)
	    List.iter
	      (fun v ->
		 let n = v.jc_var_info_final_name in
		 if List.mem_assoc n list_of_refs then
		   v.jc_var_info_final_name <- List.assoc n list_of_refs
	      ) f.jc_fun_info_parameters;
	    body
	  in

          (* safety behavior *)
	  let acc = 
	    if Jc_options.verify_behavior "safety" then
              let safety_body = wrap_body f spec "safety" body in
              let newid = f.jc_fun_info_name ^ "_safety" in
              reg_decl 
		~out_mark:newid
		~in_mark:f.jc_fun_info_name
		~name:("function " ^ f.jc_fun_info_name)
		~beh:"Safety" 
		funpos;
              if is_purely_exceptional_fun spec then acc else
		if Jc_options.verify_invariants_only then acc else
                  Def(
                    newid,
                    Fun(
                      params,
                      internal_requires,
                      safety_body,
                      internal_safety_post,
                      excep_posts_for_others None excep_behaviors))
		  :: acc
	    else acc
	  in

          (* normal behaviors *)
          let acc =
            List.fold_right
              (fun (id,b,internal_post,_) acc ->
		 if Jc_options.verify_behavior id then
                   let normal_body = wrap_body f spec id body in
                   let newid = f.jc_fun_info_name ^ "_ensures_" ^ id in
                   let beh = 
                     if id="default" then "Default behavior" else
		       "Normal behavior `"^id^"'"
                   in
                   reg_decl 
                     ~out_mark:newid
                     ~in_mark:f.jc_fun_info_name
                     ~name:("function " ^ f.jc_fun_info_name)
                     ~beh  
                     funpos;
                   Def(
                     newid,
                     Fun(
		       params,
		       assume_in_precondition b internal_requires,
		       normal_body,
		       internal_post,
		       excep_posts_for_others None excep_behaviors))
		   :: acc
		 else acc
              ) normal_behaviors acc
          in 

          (* exceptional behaviors *)
          let acc =
            ExceptionMap.fold
              (fun exc bl acc ->
                 List.fold_right
                   (fun (id,b,internal_post,_) acc ->
		      if Jc_options.verify_behavior id then
			let except_body = wrap_body f spec id body in
                        let newid = f.jc_fun_info_name ^ "_exsures_" ^ id in
                        reg_decl 
                          ~out_mark:newid
                          ~in_mark:f.jc_fun_info_name
                          ~name:("function " ^ f.jc_fun_info_name)
                          ~beh:("Exceptional behavior `" ^ id ^ "'")  
                          funpos;
                        Def(newid,
                            Fun(
			      params,
			      assume_in_precondition b internal_requires,
			      except_body,
			      LTrue,
			      (exception_name exc, internal_post) :: 
                                excep_posts_for_others (Some exc) 
				excep_behaviors))
                        :: acc
		      else acc
		   ) bl acc
	      ) user_excep_behaviors acc
          in
          acc 

let tr_fun f funpos spec body acc =
  set_current_function f;
  let acc = tr_fun f funpos spec body acc in
  reset_current_function ();
  acc

let tr_specialized_fun n fname param_name_assoc acc =
  
  let rec modif_why_type = function
    | Prod_type(n,t1,t2) ->
	if StringMap.mem n param_name_assoc then
	  modif_why_type t2
	else Prod_type(n,t1,modif_why_type t2)
    | Base_type b -> Base_type b
    | Ref_type(t) -> Ref_type (modif_why_type t)
    | Annot_type (pre,t,reads,writes,post,signals) ->
	Annot_type (modif_assertion pre, modif_why_type t,
		    modif_namelist reads,
		    modif_namelist writes,
		    modif_assertion post, 
		    List.map (fun (x,a) -> (x,modif_assertion a)) signals)

  and modif_assertion a = 
    match a with
      | LTrue 
      | LFalse -> a
      | LAnd(a1,a2) -> LAnd(modif_assertion a1,modif_assertion a2)
      | LOr(a1,a2) -> LOr(modif_assertion a1,modif_assertion a2)
      | LIff(a1,a2) -> LIff(modif_assertion a1,modif_assertion a2)
      | LNot(a1) -> LNot(modif_assertion a1)
      | LImpl(a1,a2) -> LImpl(modif_assertion a1,modif_assertion a2)
      | LIf(t,a1,a2) -> LIf(modif_term t,modif_assertion a1,modif_assertion a2)
      | LLet(id,t,a) -> LLet(id,modif_term t,modif_assertion a)
      | LForall(id,t,a) -> LForall(id,t,modif_assertion a)
      | LExists(id,t,a) -> LExists(id,t,modif_assertion a)
      | LPred(id,l) -> LPred(id,List.map modif_term l)
      | LNamed (n, a) -> LNamed (n, modif_assertion a)
      
  and modif_term t =
    match t with
      | LConst(c) -> t
      | LApp(id,l) -> LApp(id,List.map modif_term l)
      | LVar(id) -> 
	  let id = StringMap.find_or_default id id param_name_assoc in
	  LVar id
      | LVarAtLabel(id,l) -> 
	  let id = StringMap.find_or_default id id param_name_assoc in
	  LVarAtLabel(id,l)
      | Tnamed(n,t) -> Tnamed(n,modif_term t)
      | TIf(t1,t2,t3) -> 
	  TIf(modif_term t1,modif_term t2,modif_term t3)

  and modif_namelist names =
    fst (List.fold_right 
	   (fun id (acc,set) -> 
	      let id = StringMap.find_or_default id id param_name_assoc in
	      id :: acc, StringSet.add id set
	   ) names ([],StringSet.empty))
  in

  let fun_type = Hashtbl.find function_prototypes fname in
  let new_fun_type = modif_why_type fun_type in
  Param(false, n, new_fun_type) :: acc


(******************************************************************************)
(*                               Logic entities                               *)
(******************************************************************************)

let tr_logic_type id acc = Type(id,[]) :: acc


let tr_exception ei acc =
  Jc_options.lprintf "producing exception '%s'@." ei.jc_exception_info_name;
  let typ = match ei.jc_exception_info_type with
    | Some tei -> Some (tr_base_type tei)
    | None -> None
  in
  Exception(exception_name ei, typ) :: acc

(* let tr_native_type nty acc = *)
(*   let lt = tr_base_type (JCTnative nty) in *)
(*   Logic(false,logic_bitvector_of_native nty,["",lt],bitvector_type) *)
(*   :: Logic(false,logic_native_of_bitvector nty,["",bitvector_type],lt) *)
(*   :: Axiom((logic_native_of_bitvector nty)^"_of_"^(logic_bitvector_of_native nty), *)
(* 	   LForall("x",lt, *)
(* 		   LPred(equality_op_for_type (JCTnative nty), *)
(*                          [LApp(logic_native_of_bitvector nty, *)
(*                                [LApp(logic_bitvector_of_native nty,  *)
(*                                      [LVar "x"])]); *)
(*                           LVar "x"]))) *)
(*   :: Axiom((logic_bitvector_of_native nty)^"_of_"^(logic_native_of_bitvector nty), *)
(* 	   LForall("x",bitvector_type, *)
(* 		   LPred("eq", (\* TODO: equality for bitvectors ? *\) *)
(*                          [LApp(logic_bitvector_of_native nty, *)
(*                                [LApp(logic_native_of_bitvector nty,  *)
(*                                      [LVar "x"])]); *)
(*                           LVar "x"]))) *)
(*   :: acc *)

let range_of_enum ri = 
  Num.add_num (Num.sub_num ri.jc_enum_info_max ri.jc_enum_info_min) (Num.Int 1)

let tr_enum_type ri (* to_int of_int *) acc =
  let name = ri.jc_enum_info_name in
  let min = Num.string_of_num ri.jc_enum_info_min in
  let max = Num.string_of_num ri.jc_enum_info_max in
  let width = Num.string_of_num (range_of_enum ri) in
  let lt = simple_logic_type name in
  let in_bounds x =
    LAnd(LPred("le_int",[LConst(Prim_int min); x]),
         LPred("le_int",[x; LConst(Prim_int max)]))
  in
  let safe_of_int_type =
    let post =
      LPred("eq_int",
            [LApp(logic_int_of_enum ri,[LVar "result"]);
             if !Jc_options.int_model = IMbounded then LVar "x"
             else LApp(mod_of_enum ri,[LVar "x"])])
    in
    Prod_type("x", Base_type(why_integer_type),
              Annot_type(LTrue,
                         Base_type lt,
                         [],[],post,[]))
  in
  let of_int_type =
    let pre = 
      if !Jc_options.int_model = IMbounded then in_bounds (LVar "x") else LTrue 
    in
    let post =
      LPred("eq_int",
            [LApp(logic_int_of_enum ri,[LVar "result"]);
             if !Jc_options.int_model = IMbounded then LVar "x"
             else LApp(mod_of_enum ri,[LVar "x"])])
    in
    Prod_type("x", Base_type(why_integer_type),
              Annot_type(pre,Base_type lt,[],[],post,[]))
  in
  let any_type =
    Prod_type("", Base_type(simple_logic_type "unit"),
              Annot_type(LTrue,Base_type lt,[],[],LTrue,[]))
  in
  Type(name,[])
  :: Logic(false,logic_int_of_enum ri,
           [("",lt)],why_integer_type)
  :: Logic(false,logic_enum_of_int ri,
           [("",why_integer_type)],lt)
  :: Predicate(false,eq_of_enum ri,[("x",lt);("y",lt)],
               LPred("eq_int",[LApp(logic_int_of_enum ri,[LVar "x"]);
                               LApp(logic_int_of_enum ri,[LVar "y"])]))
  :: (if !Jc_options.int_model = IMmodulo then
        let width = LConst (Prim_int width) in
        let fmod t = LApp (mod_of_enum ri, [t]) in
        [Logic (false, mod_of_enum ri, 
                ["x", simple_logic_type "int"], simple_logic_type "int");
         Axiom (name ^ "_mod_def",
                LForall ("x", simple_logic_type "int",
                         LPred ("eq_int", [LApp (mod_of_enum ri, [LVar "x"]);
                                           LApp (logic_int_of_enum ri, 
                                                 [LApp (logic_enum_of_int ri,
                                                        [LVar "x"])])])));
         Axiom (name ^ "_mod_lb",
                LForall ("x", simple_logic_type "int",
                         LPred ("ge_int", [LApp (mod_of_enum ri, [LVar "x"]);
                                           LConst (Prim_int min)])));
         Axiom (name ^ "_mod_gb",
                LForall ("x", simple_logic_type "int",
                         LPred ("le_int", [LApp (mod_of_enum ri, [LVar "x"]);
                                           LConst (Prim_int max)])));
         Axiom (name ^ "_mod_id",
                LForall ("x", simple_logic_type "int",
                         LImpl (in_bounds (LVar "x"), 
                                LPred ("eq_int", [LApp (mod_of_enum ri, 
                                                        [LVar "x"]);
                                                  LVar "x"]))));
         Axiom (name ^ "_mod_lt",
                LForall ("x", simple_logic_type "int",
                         LImpl (LPred ("lt_int", [LVar "x"; 
                                                  LConst (Prim_int min)]), 
                                LPred ("eq_int", [fmod (LVar "x");
                                                  fmod (LApp ("add_int", 
                                                              [LVar "x"; 
                                                               width]))]))));
         Axiom (name ^ "_mod_gt",
                LForall ("x", simple_logic_type "int",
                         LImpl (LPred ("gt_int", [LVar "x"; 
                                                  LConst (Prim_int max)]), 
                                LPred ("eq_int", [fmod (LVar "x");
                                                  fmod (LApp ("sub_int", 
                                                              [LVar "x"; 
                                                               width]))]))));
        ]
      else [])
  @ Param(false,fun_enum_of_int ri,of_int_type)
  :: Param(false,safe_fun_enum_of_int ri,safe_of_int_type)
  :: Param(false,fun_any_enum ri,any_type)
  :: Axiom(name^"_range",
           LForall("x",lt,in_bounds 
                     (LApp(logic_int_of_enum ri,[LVar "x"]))))
  :: Axiom(name^"_coerce",
           LForall("x",why_integer_type,
                   LImpl(in_bounds (LVar "x"),
                         LPred("eq_int",
                               [LApp(logic_int_of_enum ri,
                                     [LApp(logic_enum_of_int ri, 
                                           [LVar "x"])]) ; 
                                LVar "x"]))))
  :: Logic(false,logic_bitvector_of_enum ri,["",lt],bitvector_type)
  :: Logic(false,logic_enum_of_bitvector ri,["",bitvector_type],lt)
  :: Axiom((logic_enum_of_bitvector ri)^"_of_"^(logic_bitvector_of_enum ri),
	   LForall("x",lt,
		   LPred(equality_op_for_type (JCTenum ri),
                         [LApp(logic_enum_of_bitvector ri,
                               [LApp(logic_bitvector_of_enum ri, 
                                     [LVar "x"])]);
                          LVar "x"])))
  :: Axiom((logic_bitvector_of_enum ri)^"_of_"^(logic_enum_of_bitvector ri),
	   LForall("x",bitvector_type,
		   LPred("eq", (* TODO: equality for bitvectors ? *)
                         [LApp(logic_bitvector_of_enum ri,
                               [LApp(logic_enum_of_bitvector ri, 
                                     [LVar "x"])]);
                          LVar "x"])))
  :: acc

let tr_enum_type_pair ri1 ri2 acc =
  (* Information from first enum *)
  let name1 = ri1.jc_enum_info_name in
  let min1 = ri1.jc_enum_info_min in
  let max1 = ri1.jc_enum_info_max in
  (* Information from second enum *)
  let name2 = ri2.jc_enum_info_name in
  let min2 = ri2.jc_enum_info_min in
  let max2 = ri2.jc_enum_info_max in
  if not (!Jc_options.int_model = IMmodulo) then acc else
    if max1 </ min2 || max2 </ min1 then acc else
      (* Compute intersection of ranges *)
      let min = if min1 <=/ min2 && min2 <=/ max1 then min2 else min1 in
      let max = if min1 <=/ max2 && max2 <=/ max1 then max2 else max1 in
      let in_bounds x =
        LAnd(LPred("le_int",[LConst(Prim_int (Num.string_of_num min)); x]),
             LPred("le_int",[x; LConst(Prim_int (Num.string_of_num max))]))
      in
      (* Integer model is modulo and enum ranges intersect. Produce useful
       * axioms that relate both modulos when they coincide.
       *)
      let range1 = range_of_enum ri1 in
      let range2 = range_of_enum ri2 in
      let mod_coincide smallri bigri smallname bigname =
        (* When modulo the big range is in the intersection of the ranges, 
         * both modulos coincide.
         *)
        let modsmall = LApp(mod_of_enum smallri,[LVar "x"]) in
        let modbig = LApp(mod_of_enum bigri,[LVar "x"]) in
        Axiom(smallname ^ "_" ^ bigname ^ "_mod_coincide",
              LForall("x",why_integer_type,
                      LImpl(in_bounds modbig,
                            LPred("eq_int",[modsmall;modbig]))))
      in
      if range1 </ range2 then
        mod_coincide ri1 ri2 name1 name2 :: acc
      else if range2 </ range1 then
        mod_coincide ri2 ri1 name2 name1 :: acc
      else
        mod_coincide ri1 ri2 name1 name2
        :: mod_coincide ri2 ri1 name2 name1 :: acc

let tr_variable vi e acc =
  if vi.jc_var_info_assigned then
    let t = Ref_type(tr_var_type vi) in
      Param(false,vi.jc_var_info_final_name,t)::acc
  else
    let t = tr_base_type vi.jc_var_info_type in
      Logic(false,vi.jc_var_info_final_name,[],t)::acc

let tr_region r acc =
  Type(r.jc_reg_final_name,[]) :: acc

let tr_memory (mc,r) acc =
  Param(
    false,memory_name(mc,r),
    Ref_type(Base_type(memory_type mc))) :: acc

let tr_alloc_table (pc,r) acc =
  Param(
    false,alloc_table_name(pc,r),
    Ref_type(Base_type(alloc_table_type pc))) :: acc

let tr_tag_table (rt,r) acc =
  Param(
    false,tag_table_name(rt,r),
    Ref_type(Base_type(tag_table_type rt))) :: acc


(******************************************************************************)
(*                                  Roots                                     *)
(******************************************************************************)

let tr_root rt acc =
  let pc = JCroot rt in
  let ac = alloc_class_of_pointer_class pc in
  let acc =
    if root_is_union rt then
      (* Declarations of field memories. *)
      let acc = 
	if root_is_plain_union rt 
	  && !Jc_options.separation_sem = SepRegions then acc else
            let mem = bitvector_type in
              Param(false,union_memory_name rt,Ref_type(Base_type mem)) :: acc
      in
	(* Validity parameters *)
	make_valid_pred ~equal:true ac pc 
	:: make_valid_pred ~equal:false ac pc 
	:: make_valid_pred ~equal:false ~right:false ac pc 
	:: make_valid_pred ~equal:false ~left:false ac pc 
        :: make_valid_pred ~equal:false (* TODO ? *) JCalloc_bitvector pc 
	  (* Allocation parameter *)
	:: make_alloc_param ~check_size:true ac pc 
	:: make_alloc_param ~check_size:false ac pc 
	:: make_alloc_param ~check_size:true JCalloc_bitvector pc 
	:: make_alloc_param ~check_size:false JCalloc_bitvector pc 
	:: acc
    else
      make_valid_pred ~equal:true ac pc :: make_valid_pred ~equal:false ac pc :: acc 
  in
  let of_ptr_addr =
    Logic(false, of_pointer_address_name rt,
	  [ ("",raw_pointer_type why_unit_type) ], pointer_type ac pc)
  in
  let addr_axiom =
    let p = "p" in
    Axiom("pointer_addr_of_" ^ (of_pointer_address_name rt),
	  LForall(p, raw_pointer_type why_unit_type,
		  make_eq_pred (JCTpointer(pc,None,None))
		    (LVar p)
		    (LApp("pointer_address",
			  [ LApp(of_pointer_address_name rt,
				 [ LVar p ])]))))
  in
  let rev_addr_axiom =
    let p = "p" in
    Axiom((of_pointer_address_name rt) ^ "_of_pointer_addr",
	  LForall(p, pointer_type ac pc,
		  make_eq_pred (JCTpointer(pc,None,None))
		    (LVar p)
		    (LApp(of_pointer_address_name rt,
			  [ LApp("pointer_address",
				 [ LVar p ])]))))
  in
  let lt = tr_base_type (JCTpointer(pc,None,None)) in
  let conv = 
    [Logic(false,logic_bitvector_of_variant rt,["",lt],bitvector_type);
     Logic(false,logic_variant_of_bitvector rt,["",bitvector_type],lt);
     Axiom((logic_variant_of_bitvector rt)^"_of_"^(logic_bitvector_of_variant rt),
	   LForall("x",lt,
		   LPred(equality_op_for_type (JCTpointer (pc,None,None)),
                         [LApp(logic_variant_of_bitvector rt,
			       [LApp(logic_bitvector_of_variant rt, 
                                     [LVar "x"])]);
                          LVar "x"])));
     Axiom((logic_bitvector_of_variant rt)^"_of_"^(logic_variant_of_bitvector rt),
	   LForall("x",bitvector_type,
		   LPred("eq", (* TODO: equality for bitvectors ? *)
                         [LApp(logic_bitvector_of_variant rt,
			       [LApp(logic_variant_of_bitvector rt, 
                                       [LVar "x"])]);
                          LVar "x"])))
    ]
  in
  let tag_table =
    Param(
      false,
      variant_tag_table_name rt,
      Ref_type(
        Base_type {
          logic_type_name = tag_table_type_name;
          logic_type_args = [root_model_type rt];
        }))
  in
  let alloc_table =
    Param(
      false,
      variant_alloc_table_name rt,
      Ref_type(
        Base_type {
          logic_type_name = alloc_table_type_name;
          logic_type_args = [root_model_type rt];
        }))
  in
  let type_def = Type(root_type_name rt, []) in
  (* Axiom: the variant can only have the given tags *)
  let axiom_variant_has_tag =
    let v = "x" in
    let tag_table = generic_tag_table_name rt in
    Axiom(
      variant_axiom_on_tags_name rt,
      LForall(
        v,
        pointer_type ac pc,
        LForall(
          tag_table,
          tag_table_type rt,
          make_or_list
            (List.map
               (make_instanceof (LVar tag_table) (LVar v))
               rt.jc_root_info_hroots)
      )))
  in
  (* Axioms: int_of_tag(T1) = 1, ... *)
  let (acc, _) = List.fold_left
    (fun (acc, index) st ->
       let axiom =
         Axiom(
           axiom_int_of_tag_name st,
           make_eq
             (make_int_of_tag st)
             (LConst(Prim_int(string_of_int index)))
         )
       in axiom::acc, index+1)
    (acc, 1)
    rt.jc_root_info_hroots
  in
  let acc = 
    type_def::alloc_table::tag_table::axiom_variant_has_tag
    :: of_ptr_addr :: addr_axiom :: rev_addr_axiom
    :: conv @ acc 
  in
  acc

(*
  Local Variables: 
  compile-command: "unset LANG; make -j -C .. bin/jessie.byte"
  End: 
*)

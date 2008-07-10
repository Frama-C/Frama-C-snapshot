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

(* $Id: jc_interp.ml,v 1.301 2008/07/11 06:35:50 moy Exp $ *)

open Jc_env
open Jc_envset
open Jc_fenv
open Jc_pervasives
open Jc_constructors
open Jc_ast
open Jc_invariants
open Output
open Format
open Jc_name
open Jc_region
open Jc_separation
open Jc_interp_misc
open Jc_struct_tools
open Jc_pattern

open Num

(* locs table *)


let locs_table = Hashtbl.create 97

let abs_fname f =
  if Filename.is_relative f then
    Filename.concat (Unix.getcwd ()) f 
  else f

let reg_loc ?id ?oldid ?kind ?name ?beh (b,e) =  
  try 
    (* If label already refered to in [Output.locs_table], do not reference it
     * more. This is the case for generated annotations. 
     *)
    match id with None -> raise Not_found | Some id ->
      ignore (Hashtbl.find Output.locs_table id);
      id
  with Not_found ->

  let id,oldid = match id,oldid with
    | None,_ -> Jc_pervasives.new_label_name (), oldid
    | Some n, None -> n,Some n
    | Some n, Some o -> n, Some o
  in
(*
  Format.eprintf "Jc_interp: reg_loc id = '%s'@." id;
*)
  let (name,f,l,b,e,kind2) = 
    try
      match oldid with
        | None -> 
            raise Not_found
        | Some n -> 
            let (f,l,b,e,k,o) = Hashtbl.find Jc_options.locs_table n in
(*
            Format.eprintf "Jc_interp: reg_loc id '%s' found@." oldid;
*)
            Jc_options.lprintf "keeping old location for id '%s'@." n;
            let n =
              try 
                match List.assoc "name" o with
                  | Rc.RCident s | Rc.RCstring s -> Some s
                  | _ -> raise Not_found
              with Not_found -> name
            in
            (n,f,l,b,e,k)
    with Not_found ->
(*
      Format.eprintf "Jc_interp: reg_loc id '%s' not found@." id;
*)
      let f = abs_fname b.Lexing.pos_fname in
      let l = b.Lexing.pos_lnum in
      let fc = b.Lexing.pos_cnum - b.Lexing.pos_bol in
      let lc = e.Lexing.pos_cnum - b.Lexing.pos_bol in
      (name,f,l,fc,lc,None)
  in
  let kind = match kind with None -> kind2 | _ -> kind in
  Jc_options.lprintf "recording location for id '%s'@." id;
  Hashtbl.replace locs_table id (kind,name,beh,f,l,b,e);
  id
    
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
    locs_table


(*******************************************************************************)
(*                                  Operators                                  *)
(*******************************************************************************)

let native_operator_type op = match snd op with
  | `Unit -> Jc_pervasives.unit_type
  | `Boolean -> Jc_pervasives.boolean_type
  | `Integer -> Jc_pervasives.integer_type
  | `Real -> Jc_pervasives.real_type

let unary_op: expr_unary_op -> string = function
  | `Uminus, `Integer -> "neg_int"
  | `Uminus, `Real -> "neg_real"
  | `Unot, `Boolean -> "not"
  | `Ubw_not, `Integer -> "bw_compl"
  | _ -> assert false (* not proper type *)

let term_unary_op = unary_op

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
  | `Bdiv, `Integer -> "div_int_"
  | `Bmod, `Integer -> "mod_int_"
      (* pointer *)
  | `Beq, `Pointer -> "eq_pointer"
  | `Bneq, `Pointer -> "neq_pointer"
  | `Bsub, `Pointer -> "sub_pointer" (* TODO: require same block *)
      (* real *)
  | `Bgt, `Real -> "gt_real_"
  | `Blt, `Real -> "lt_real_"
  | `Bge, `Real -> "ge_real_"
  | `Ble, `Real -> "le_real_"
  | `Beq, `Real -> "eq_real_"
  | `Bneq, `Real -> "neq_real_"
  | `Badd, `Real -> "add_real"
  | `Bsub, `Real -> "sub_real"
  | `Bmul, `Real -> "mul_real"
  | `Bdiv, `Real -> "div_real_"
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
  | `Beq, `Pointer -> "eq_pointer"
  | `Bneq, `Pointer -> "neq_pointer"
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

(*******************************************************************************)
(*                                  Coercions                                  *)
(*******************************************************************************)

let logic_enum_of_int n = n.jc_enum_info_name ^ "_of_integer"
let safe_fun_enum_of_int n = "safe_" ^ n.jc_enum_info_name ^ "_of_integer_"
let fun_enum_of_int n = n.jc_enum_info_name ^ "_of_integer_"
let logic_int_of_enum n = "integer_of_" ^ n.jc_enum_info_name
let mod_of_enum n = "mod_" ^ n.jc_enum_info_name ^ "_of_integer"
let fun_any_enum n = "any_" ^ n.jc_enum_info_name
let eq_of_enum n = "eq_" ^ n.jc_enum_info_name

let logic_union_of_field fi = "union_of_" ^ fi.jc_field_info_name
let logic_field_of_union fi = fi.jc_field_info_name ^ "_of_union"
  
let equality_op_for_type = function
  | JCTnative Tunit -> assert false
  | JCTnative Tboolean -> "eq_bool"
  | JCTnative Tinteger -> "eq_int"
  | JCTnative Treal -> "eq_real"
  | JCTnative Tstring -> "eq"
  | JCTlogic s -> (* TODO *) assert false
  | JCTenum ei -> eq_of_enum ei
  | JCTpointer _
  | JCTnull ->  "eq_pointer"
  | JCTany -> assert false
  | JCTtype_var _ -> assert false (* TODO ? *)

let term_coerce ?(cast=false) loc tdest tsrc e =
  match tdest, tsrc with
    | JCTnative t, JCTnative u when t=u -> e
    | JCTnative Treal, JCTnative Tinteger -> 
	begin
	  match e with
	    | LConst (Prim_int n) ->
		LConst (Prim_real (n ^ ".0")) 
	    | _ -> 
		LApp("real_of_int",[e])
	end
    | JCTnative Tinteger, JCTnative Treal -> 
	LApp("int_of_real",[e])
    | JCTlogic t, JCTlogic u when t=u -> e
    | JCTenum ri1, JCTenum ri2 when ri1==ri2 -> e
    | JCTenum ri1, JCTenum ri2 ->
        assert cast; (* Typing should have inserted an explicit cast *)
        let e' = LApp(logic_int_of_enum ri2,[e]) in
        LApp(logic_enum_of_int ri1,[e'])
    | JCTnative Tinteger, JCTenum ri ->
        let e' = LApp(logic_int_of_enum ri,[e]) in
          begin
            match e with
              | Tnamed(n,_) -> Tnamed(n,e')
              | _ -> e'
          end
    | JCTenum ri, JCTnative Tinteger ->
(*
        Jc_typing.typing_error loc 
          "can't coerce type %a to type %a" 
          print_type tsrc print_type tdest
;
        assert false (* a explicit cast should be required by jc_typing *)
*)
        LApp(logic_enum_of_int ri,[e]) 
    | JCTpointer (JCvariant _, _, _), JCTpointer _ -> e
    | JCTpointer (st1, _, _), JCTpointer(JCtag(st2, _),_,_) 
        when Jc_typing.substruct st2 st1 -> e
    | JCTpointer (JCtag(st, _), a, b), (JCTpointer(_,_,_) (* YMo, why null? | JCTnull*))  -> 
        LApp("downcast", 
             [ LVar (tag_table_name (JCtag(st, []))) ; e ;
               LVar (tag_name st) ])    
    | JCTpointer _, JCTnull  -> 
	e
    |  _ -> 
         Jc_typing.typing_error loc 
           "can't coerce type %a to type %a" 
           print_type tsrc print_type tdest
           
let make_guarded_app ~name (k : kind) loc f l =
  let lab =
    match name with
      | "" -> reg_loc ~kind:k loc
      | n -> reg_loc ~id:n ~kind:k loc
  in
  let l = match l with
    | [Label(namelab,e)] when name = namelab ->
        (* YM: same label used twice. See with Claude for semantics of labels.
           Current patch: remove label in inner expression. *)
        [e]
    | [App(f,Label(namelab,e))] when name = namelab ->
        (* Occurs on test [band.c] in FramaC *)
        [App(f,e)]
    | _ -> l
  in
  Label (lab, make_app f l)

let eval_integral_const e =
  let rec eval e =
    match e#node with
      | JCEconst(JCCinteger s) -> Numconst.integer s 
      | JCErange_cast(e,_ri2) -> eval e
      | JCEunary(op,e) ->
          let v = eval e in
          begin match op with
            | `Uminus, `Integer -> minus_num v
            | `Uminus, (`Real | `Boolean | `Unit)
            | `Unot, _
            | `Ubw_not, _ ->
                failwith "Not integral const"
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
                failwith "Not integral const"
	    | `Bconcat, _ -> assert false (* TODO *)
          end
      | JCEif(e1,e2,e3) ->
          (* TODO: write [eval_boolean_const] *)
          failwith "Not integral const"
      | JCEconst _ | JCEvar _ | JCEshift _ | JCEderef _ 
      | JCEinstanceof _ | JCEcast _ | JCEreal_cast _ | JCEoffset _ 
      | JCEalloc _ | JCEfree _ | JCEmatch _ |JCEunpack _ |JCEpack _
      | JCEthrow _ | JCEtry _ | JCEreturn _ | JCEloop _ | JCEblock _
      | JCEassert _ | JCElet _ | JCEassign_heap _ | JCEassign_var _ | JCEapp _
      | JCEreturn_void ->
          failwith "Not integral const"
  in
  try Some(eval e) with Failure "Not integral const" -> None

let rec fits_in_enum ri e = 
  match eval_integral_const e with
    | Some v -> ri.jc_enum_info_min <=/ v && v <=/ ri.jc_enum_info_max
    | None -> false

let coerce ~no_int_overflow lab loc tdest tsrc orig e =
  match tdest, tsrc with
    | JCTnative t, JCTnative u when t=u -> e
    | JCTnative Treal, JCTnative Tinteger -> 
	begin
	  match e with
	    | Cte (Prim_int n) ->
		Cte (Prim_real (n ^ ".0")) 
	    | _ -> 
		make_app "real_of_int" [e]
	end
    | JCTnative Tinteger, JCTnative Treal -> 
	make_app "int_of_real" [e]
    | JCTlogic t, JCTlogic u when t=u -> e
    | JCTenum ri1, JCTenum ri2 when ri1==ri2 -> e
    | JCTenum ri1, JCTenum ri2 -> 
        let e' = make_app (logic_int_of_enum ri2) [e] in
        if no_int_overflow || fits_in_enum ri1 orig then 
          make_app (safe_fun_enum_of_int ri1) [e']
        else
          make_guarded_app ~name:lab ArithOverflow loc 
            (fun_enum_of_int ri1) [e']
    | JCTnative Tinteger, JCTenum ri ->
        make_app (logic_int_of_enum ri) [e]
    | JCTenum ri, JCTnative Tinteger ->
        if no_int_overflow || fits_in_enum ri orig then 
          make_app (safe_fun_enum_of_int ri) [e]
        else
          make_guarded_app ~name:lab ArithOverflow loc (fun_enum_of_int ri) [e]
    | _ , JCTnull -> e
    | JCTpointer (JCvariant _, _, _), JCTpointer _ -> e
    | JCTpointer (st1,_,_), JCTpointer (JCtag(st2, _),_,_) 
        when Jc_typing.substruct st2 st1 -> e
    | JCTpointer (JCtag(st, _),_,_), _  -> 
        make_guarded_app ~name:lab DownCast loc "downcast_" 
          [ Deref (tag_table_name (JCtag(st, []))) ; e ;
            Var (tag_name st) ] 
    | JCTany, JCTany -> e
    | _ -> 
        Jc_typing.typing_error loc 
          "can't coerce type %a to type %a" 
          print_type tsrc print_type tdest

(*******************************************************************************)
(*                                    Terms                                    *)
(*******************************************************************************)

let lvar ?(assigned=true) ?(label_in_name=false) label v =
  if label_in_name then
    LVar(label_var label v)
  else
    if assigned then
      match label with 
        | LabelHere -> LVar v
        | LabelOld -> LVarAtLabel(v,"")
        | LabelPre -> LVarAtLabel(v,"init")
        | LabelPost -> LVar v
        | LabelName l -> LVarAtLabel(v,l.label_info_final_name)
    else LVar v

let nvar v = Var v

let var v =
  if v.jc_var_info_assigned 
  then Deref v.jc_var_info_final_name
  else Var v.jc_var_info_final_name

let lvar_info label v = 
  lvar ~assigned:v.jc_var_info_assigned label v.jc_var_info_final_name

let memvar ?assigned ?label_in_name label (fi,r) =
  let mem = field_region_memory_name(fi,r) in
  let mut = match !current_function with
    | None -> true
    | Some infunction -> mutable_memory infunction (fi,r) 
  in
  if mut then
    lvar ?assigned ?label_in_name label mem
  else
    lvar ?assigned ?label_in_name LabelHere mem

let relocate_label ~relocate truelab curlab =
  if relocate && curlab = LabelHere then truelab else curlab

(* Return (t, lets) where:
 * t is the Why term
 * lets is a list of (id, value), which should be binded
 * at the assertion level. *)
let rec term ~global_assertion ~relocate label oldlabel t =
  let ft = term ~global_assertion ~relocate label oldlabel in
  let rlab = relocate_label ~relocate label in
  let t', lets =
  match t#node with
    | JCTconst JCCnull -> LVar "null", []
    | JCTvar v -> lvar_info label v, []
    | JCTconst c -> LConst(const c), []
    | JCTunary(op, t1) ->
        let t1', lets = ft t1 in
        let opty = native_operator_type op in
        LApp(unary_op op, [term_coerce t#loc opty t1#typ t1']),
        lets
    (* binary operators on pointers (no coercion) *)
    | JCTbinary(t1, (_, (`Pointer | `Logic) as op), t2) ->
        let t1', lets1 = ft t1 in
        let t2', lets2 = ft t2 in
        LApp (term_bin_op op, [ t1'; t2']), lets1@lets2
    (* binary operators on native types *)
    | JCTbinary(t1, (_, #native_operator_type as op), t2) ->
        let t1', lets1 = ft t1 in
        let t2', lets2 = ft t2 in
        let t = native_operator_type op in
        LApp (term_bin_op op,
              [ term_coerce t1#loc t t1#typ t1'; 
                term_coerce t2#loc t t2#typ t2']),
        lets1@lets2
    | JCTshift(t1,t2) -> 
        let t1', lets1 = ft t1 in
        let t2', lets2 = ft t2 in
        LApp("shift",[t1'; 
                      term_coerce t2#loc integer_type 
                        t2#typ t2']), lets1@lets2
    | JCTif(t1,t2,t3) ->
        let t1', lets1 = ft t1 in
        let t2', lets2 = ft t2 in
        let t3', lets3 = ft t3 in
        TIf(t1', t2', t3'), lets1@lets2@lets3
    | JCTderef(t,label,fi) -> 
	let label = rlab label in
        let t', lets = ft t in
        let mem = memvar ~label_in_name:global_assertion label (fi,t#region) in
        let deref = LApp("select",[mem; t']) in
        let deref =
          if field_of_union fi then
            (* Translate back access to union type to field type *)
            if integral_union (union_of_field fi) then
              (* Type of union is integer, and type of field an integral type *)
              match fi.jc_field_info_type with
                | JCTnative Tinteger -> deref
                | JCTenum ri -> LApp(logic_enum_of_int ri,[deref])
                | _ -> assert false
            else LApp(logic_field_of_union fi,[deref])
          else deref
        in
        deref, lets
    | JCTapp app ->
        let f = app.jc_app_fun and l = app.jc_app_args in
        let args, lets = List.fold_right
          (fun arg (args, lets) ->
             let arg', arglets = ft arg in
             arg'::args, arglets@lets)
          l ([], [])
        in
        let args' = 
          try
            List.map2 (fun x y -> x, y) l args 
          with Invalid_argument _ -> assert false
        in
        let args =
          try
            List.map2 
              (fun vi (t, t') -> 
                 term_coerce t#loc 
                   vi.jc_var_info_type t#typ t')
              f.jc_logic_info_parameters args'
          with Invalid_argument _ -> 
            eprintf "fun = %s, len pars = %d, len args' = %d@." 
              f.jc_logic_info_name 
              (List.length f.jc_logic_info_parameters)
              (List.length args');
            assert false
        in
        make_logic_fun_call ~label_in_name:global_assertion f args
          app.jc_app_region_assoc app.jc_app_label_assoc, lets
    | JCTold(t) -> term ~global_assertion ~relocate oldlabel oldlabel t
    | JCTat(t,lab) -> term ~global_assertion ~relocate lab oldlabel t
    | JCToffset(k,t,st) -> 
        let alloc = 
          alloc_region_table_name (JCtag(st, []), t#region)
        in
        let f = match k with
          | Offset_min -> "offset_min"
          | Offset_max -> "offset_max"
        in
        let t', lets = ft t in
        LApp(f,[LVar alloc; t']), lets
    | JCTinstanceof(t,label,ty) ->
	let label = rlab label in
        let t', lets = ft t in
        let tag = tag_table_name (JCtag(ty, [])) in
        LApp("instanceof_bool",
             [lvar label tag; t';LVar (tag_name ty)]), lets
    | JCTcast(t,label,ty) ->
	let label = rlab label in
        if struct_of_union ty then ft t else
          let t', lets = ft t in
          let tag = tag_table_name (JCtag(ty, [])) in
          LApp("downcast",
               [lvar ~label_in_name:global_assertion label tag; 
		t';LVar (tag_name ty)]), lets
    | JCTrange_cast(t,ri) -> 
        eprintf "range_cast in term: from %a to %a@." 
          print_type t#typ print_type (JCTenum ri);
        let t', lets = ft t in
        let t' = term_coerce ~cast:true t#loc (JCTenum ri) t#typ t' in
        t', lets
    | JCTreal_cast(t,rc) ->
        let t', lets = ft t in
        let t' = match rc with
          | Integer_to_real ->
              term_coerce t#loc real_type integer_type t'
          | Real_to_integer ->
              term_coerce t#loc integer_type real_type t'
        in
        t', lets
    | JCTrange(t1,t2) -> assert false (* TODO ? *)
    | JCTmatch(t, ptl) ->
        let t', lets1 = ft t in
        (* TODO: use a temporary variable for t' *)
        (* if the pattern-matching is incomplete, default value is true *)
        let ptl', lets2 =
          pattern_list_term ft t' t#typ ptl (LConst(Prim_bool true)) in
        ptl', lets1@lets2
  in
  (if t#name_label <> "" then
     Tnamed(reg_loc ~id:t#name_label t#loc,t')
   else
     t'), lets

let named_term ~global_assertion ~relocate label oldlabel t =
  let t', lets = term ~global_assertion ~relocate label oldlabel t in
  match t' with
    | Tnamed _ -> t', lets
    | _ -> 
        let n = reg_loc t#loc in
        Tnamed(n,t'), lets

(*******************************************************************************)
(*                                  Assertions                                 *)
(*******************************************************************************)

let tag ~global_assertion ~relocate label oldlabel = function
  | JCTtag st -> LVar (tag_name st), []
  | JCTbottom -> LVar "bottom_tag", []
  | JCTtypeof(t, st) ->
      let te, lets = term ~global_assertion ~relocate label oldlabel t in
      make_typeof st te, lets

let rec assertion ~global_assertion ~relocate label oldlabel a =
  let fa = assertion ~global_assertion ~relocate label oldlabel 
  and ft = term ~global_assertion ~relocate label oldlabel
  and ftag = tag ~global_assertion ~relocate label oldlabel
  in
  let a', lets =
    match a#node with
      | JCAtrue -> LTrue, []
      | JCAfalse -> LFalse, []
      | JCAif(t1,p2,p3) ->
          let t1', lets = ft t1 in
            LIf(t1', fa p2, fa p3), lets
      | JCAand l -> make_and_list (List.map fa l), []
      | JCAor l -> make_or_list (List.map fa l), []
      | JCAimplies(a1,a2) -> make_impl (fa a1) (fa a2), []
      | JCAiff(a1,a2) -> make_equiv (fa a1) (fa a2), []
      | JCAnot(a) -> LNot(fa a), []
      (* pointer relation (no coercion) *)
      | JCArelation(t1, (_, (`Pointer | `Logic) as op),t2) ->
          let t1', lets1 = ft t1 in
          let t2', lets2 = ft t2 in
          LPred (pred_bin_op (op :> pred_bin_op), [ t1'; t2']), lets1@lets2
      (* native type relation *)
      | JCArelation(t1, (_, #native_operator_type as op), t2) ->
          let t1', lets1 = ft t1 in
          let t2', lets2 = ft t2 in
          let t = native_operator_type op in
            LPred(pred_bin_op (op :> pred_bin_op), 
                  [ term_coerce t1#loc t
                      t1#typ t1'; 
                    term_coerce t2#loc t 
                      t2#typ t2']), lets1@lets2
      | JCAapp app -> 
          let f = app.jc_app_fun in
          let l = app.jc_app_args in
          let args, lets = List.fold_right
            (fun arg (args, lets) ->
               let arg', arglets = ft arg in
               arg'::args, arglets@lets)
            l ([], [])
          in
          let args' = 
            try
              List.map2 (fun x y -> x, y) l args 
            with Invalid_argument _ -> assert false
          in
          (* No type verification for full_separated for the moment. *)
          if f.jc_logic_info_name = "full_separated" then
            make_logic_pred_call ~label_in_name:false f args [] [], lets
          else begin try
            make_logic_pred_call ~label_in_name:global_assertion f  
              (try
                 List.map2 
                   (fun vi (t, t') -> 
                      term_coerce t#loc 
                        vi.jc_var_info_type t#typ t')
                   f.jc_logic_info_parameters args'
               with Invalid_argument _ -> assert false)
              app.jc_app_region_assoc
              app.jc_app_label_assoc, lets
          with Invalid_argument _ -> assert false
          end
      | JCAquantifier (Forall, v, p) -> 
          LForall (v.jc_var_info_final_name,
                   tr_base_type v.jc_var_info_type,
                   fa p), []
      | JCAquantifier(Exists,v,p) -> 
          LExists(v.jc_var_info_final_name,
                  tr_base_type v.jc_var_info_type,
                  fa p), []
      | JCAold a -> assertion ~global_assertion ~relocate oldlabel oldlabel a, []
      | JCAat(a,lab) -> assertion ~global_assertion ~relocate lab oldlabel a, []
      | JCAbool_term(t) ->
          let t', lets = ft t in
          LPred("eq",[t'; LConst(Prim_bool true)]), lets
      | JCAinstanceof(t,lab,ty) -> 
          let t', lets = ft t in
          let tag = tag_table_name (JCtag(ty, [])) in
	  let tag = label_var ~label_in_name:global_assertion lab tag in
          LPred("instanceof", [LVar tag; t'; LVar (tag_name ty)]), lets
      | JCAmutable(te, st, ta) ->
          let te', lets1 = ft te in
          let tag, lets2 = ftag ta#node in
          let mutable_field = LVar (mutable_name (JCtag(st, []))) in
          LPred("eq", [ LApp("select", [ mutable_field; te' ]); tag ]),
          lets1@lets2
      | JCAtagequality(t1, t2, h) ->
          let t1', lets1 = ftag t1#node in
          let t2', lets2 = ftag t2#node in
          LPred("eq", [ t1'; t2' ]), lets1@lets2
      | JCAmatch(arg, pal) ->
          let arg', lets = ft arg in
          (* TODO: use a temporary variable for arg' *)
          let pal', _ = pattern_list_assertion fa arg' arg#typ
            pal LTrue in
          pal', lets
  in
  let a' = make_pred_binds lets a' in
  if a#name_label <> "" then
    begin
(*
      eprintf "Assertion has label %s@." a#name_label;
*)
      LNamed(reg_loc ~id:a#name_label a#loc,a')
    end
  else
    begin
      (*
        eprintf "Assertion has no label@.";
      *)
      a'
    end
  

let named_jc_assertion loc a =
  match a with
      (* | LTrue *) | LNamed _ -> 
        (*
          eprintf "Assertion already named %s@." lab;
        *)
        a
    | _ -> 
        let n = reg_loc loc in 
          (*
            eprintf "Registering new name %s for assertion@." n;
          *)
          LNamed(n,a)
            
            
let named_assertion ~global_assertion ~relocate label oldlabel a =
  let a' = assertion ~global_assertion ~relocate label oldlabel a in
  named_jc_assertion a#loc a'

let struct_alloc_arg a =
  alloc_table_name a, alloc_table_type a

let field_memory_arg fi =
  field_memory_name fi, field_memory_type fi

(*******************************************************************************)
(*                                 Expressions                                 *)
(*******************************************************************************)

let rec is_substruct si1 si2 =
  if si1.jc_struct_info_name = si2.jc_struct_info_name then true else
    let vi = struct_variant si1 and vi' = struct_variant si2 in
    (vi == vi' && vi.jc_variant_info_is_union)
    ||
    match si1.jc_struct_info_parent with
      | None -> false
      | Some(si, _) -> is_substruct si si2

type interp_lvalue =
  | LocalRef of var_info
  | HeapRef of field_info * expr

let const_un = Cte(Prim_int "1")

type shift_offset = Int_offset of string | Expr_offset of Jc_ast.expr 

let bounded lb rb s =
  let n = Numconst.integer s in Num.le_num lb n && Num.le_num n rb

let lbounded lb s =
  let n = Numconst.integer s in Num.le_num lb n

let rbounded rb s =
  let n = Numconst.integer s in Num.le_num n rb

let destruct_pointer e = 
  let ptre,off = match e#node with
    | JCEshift(e1,e2) -> 
        begin match e2#node with
        | JCEconst (JCCinteger s) -> 
            e1,Int_offset s
        | JCEconst _ -> assert false
        | _ ->
            e1,Expr_offset e2
        end
    | _ -> e,Int_offset "0"
  in
  match ptre#typ with
  | JCTpointer(_,lb,rb) -> ptre,off,lb,rb
  | _ -> assert false

let rec make_lets l e =
  match l with
    | [] -> e
    | (tmp,a)::l -> Let(tmp,a,make_lets l e)

let return_void = ref false


let write_mems ~callee_writes ~regions =
  FieldOrVariantRegionMap.fold
    (fun (fi,distr) labels acc ->
       if Region.polymorphic distr then
         try 
           let locr = RegionList.assoc distr regions in
           (* Check that we do not pass a same memory twice as 
            * a pointer.
            *)
           begin
(*             assert(not(FieldOrVariantRegionList.mem (fi,locr) acc));*)
             (Var(field_or_variant_region_memory_name (fi,locr)))::acc 
           end
         with Not_found -> 
           (* Local memory. Not passed in argument by the caller. *)
           acc
       else acc)
    callee_writes.jc_effect_memories
    []

let read_mems ~caller_writes ~callee_reads ~callee_writes ~regions =
  FieldOrVariantRegionMap.fold
    (fun (fi,distr) labels acc ->
       if FieldOrVariantRegionMap.mem (fi,distr)
         callee_writes.jc_effect_memories then acc
       else
         if Region.polymorphic distr then
                  (* Distant region is polymorphic. It should be passed as
                   * argument to the function. 
                   *)
           try 
             let locr = RegionList.assoc distr regions in
             if Region.polymorphic locr then
               if FieldOrVariantRegionMap.mem (fi,locr) 
                 caller_writes.jc_effect_memories
               then
                 (Deref(field_or_variant_region_memory_name(fi,locr)))::acc 
               else (Var(field_or_variant_region_memory_name(fi,locr)))::acc 
             else begin
               (* Check that we do not pass in argument a constant 
                * memory that is also written directly by 
                * the function called.
                *)
(*                assert(not(FieldOrVariantRegionMap.mem (fi,locr) *)
(*                             callee_reads.jc_effect_memories *)
(*                          )); *)
               assert(not(FieldOrVariantRegionMap.mem (fi,locr)
                            callee_writes.jc_effect_memories
                         ));
               if FieldOrVariantRegionMap.mem (fi,locr) 
                 caller_writes.jc_effect_memories
               then (Var(field_or_variant_region_memory_name(fi,locr)))::acc 
               else (Deref(field_or_variant_region_memory_name(fi,locr)))::acc 
             end
           with Not_found -> 
             (* Local memory. Not passed in argument by the caller. *)
             acc
         else acc)
    callee_reads.jc_effect_memories
    []

let write_allocs ~callee_writes ~regions =
  StringRegionSet.fold
    (fun (a,distr) acc ->
       if Region.polymorphic distr then
         try 
           let locr = RegionList.assoc distr regions in
           (* Check that we do not pass a same allocation table 
            * twice as a pointer.
            *)
           begin
(*             assert(not(StringRegionList.mem (a,locr) acc));*)
             (Var(alloc_region_table_name2(a,locr)))::acc 
           end
         with Not_found -> 
                    (* Local allocation table. 
                     * Not passed in argument by the caller. 
                     *)
           acc
       else acc)
    callee_writes.jc_effect_alloc_table
    []

let read_allocs ~caller_writes ~callee_writes ~callee_reads ~regions =
  StringRegionSet.fold
    (fun (a,distr) acc ->
       if Region.polymorphic distr then
                  (* Distant region is polymorphic. It should be passed as
                   * argument to the function. 
                   *)
         try 
           let locr = RegionList.assoc distr regions in
           if Region.polymorphic locr then
             if StringRegionSet.mem (a,locr) 
               caller_writes.jc_effect_alloc_table
             then
               (Deref(alloc_region_table_name2(a,locr)))::acc 
             else (Var(alloc_region_table_name2(a,locr)))::acc 
           else
             begin
               (* Check that we do not pass in argument a constant 
                * allocation table that is also written directly
                * by the function called.
                *)
               let fallocs = (* StringRegionSet.union *)
(*                  callee_reads.jc_effect_alloc_table *)
                 callee_writes.jc_effect_alloc_table
               in
               assert(not(StringRegionSet.mem (a,locr) fallocs));
               if StringRegionSet.mem (a,locr) 
                 callee_writes.jc_effect_alloc_table
               then (Var(alloc_region_table_name2(a,locr)))::acc 
               else (Deref(alloc_region_table_name2(a,locr)))::acc 
             end
         with Not_found -> 
                    (* Local allocation table.
                     * Not passed in argument by the caller. *)
           acc
       else acc)
    (StringRegionSet.diff
       callee_reads.jc_effect_alloc_table
       callee_writes.jc_effect_alloc_table)
    []

(*******************************************************************************)
(*                                  Locations                                  *)
(*******************************************************************************)

let rec pset ~global_assertion before loc = 
  let fpset = pset ~global_assertion before in
  let ft = term ~global_assertion ~relocate:false before before in
  match loc with
    | JCLSderef(ls,lab,fi,r) ->
        let m = memvar ~label_in_name:global_assertion lab (fi,r) in
        LApp("pset_deref", [m;fpset ls])
    | JCLSvar vi -> 
        let m = lvar_info before vi in
        LApp("pset_singleton", [m])
    | JCLSrange(ls,None,None) ->
        let ls = fpset ls in
        LApp("pset_all", [ls])
    | JCLSrange(ls,None,Some b) ->
        let ls = fpset ls in
        let b', lets = ft b in
        assert (lets = []);
        LApp("pset_range_left", 
             [ls; 
              term_coerce b#loc integer_type b#typ b'])
    | JCLSrange(ls,Some a,None) ->
        let ls = fpset ls in
        let a', lets = ft a in
        assert (lets = []);
        LApp("pset_range_right", 
             [ls; 
              term_coerce a#loc integer_type a#typ a'])
    | JCLSrange(ls,Some a,Some b) ->
        let ls = fpset ls in
        let a', lets1 = ft a in
        let b', lets2 = ft b in
        assert (lets1 = [] && lets2 = []);
        LApp("pset_range", 
             [ls; 
              term_coerce a#loc integer_type a#typ a'; 
              term_coerce b#loc integer_type b#typ b'])
        
let rec collect_locations ~global_assertion before (refs,mems) loc =
  match loc with
    | JCLderef(e,lab,fi,fr) -> 
        let iloc = pset ~global_assertion lab e in
        let fvi = 
          if field_of_union fi then FVvariant (union_of_field fi) else FVfield fi
        in
        let l =
          try
            let l = FieldOrVariantRegionMap.find (fvi,location_set_region e) mems in
            iloc::l
          with Not_found -> [iloc]
        in
        (refs, FieldOrVariantRegionMap.add (fvi,location_set_region e) l mems)
    | JCLvar vi -> 
        let var = vi.jc_var_info_final_name in
        (StringMap.add var true refs,mems)
    | JCLat(loc,lab) ->
        collect_locations ~global_assertion before (refs,mems) loc

let rec make_union_loc = function
  | [] -> LVar "pset_empty"
  | [l] -> l
  | l::r -> LApp("pset_union",[l;make_union_loc r])

let assigns before ef locs loc =
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
    VarSet.fold
      (fun v m -> StringMap.add v.jc_var_info_final_name false m)
      ef.jc_writes.jc_effect_globals StringMap.empty
  in
  let mems = 
    FieldOrVariantRegionMap.fold
      (fun (fi,r) labels m -> 
         FieldOrVariantRegionMap.add (fi,r) [] m)
      ef.jc_writes.jc_effect_memories FieldOrVariantRegionMap.empty 
  in
  let refs,mems = 
    List.fold_left (collect_locations ~global_assertion:false before) (refs,mems) locs
  in
  let a =
    StringMap.fold
      (fun v p acc -> 
        if p then acc else
          make_and acc (LPred("eq", [LVar v; lvar before v])))
      refs LTrue
  in
  FieldOrVariantRegionMap.fold
    (fun (fvi,r) p acc -> 
       let v = field_or_variant_region_memory_name(fvi,r) in
       let root = match fvi with 
         | FVfield fi -> JCtag(fi.jc_field_info_root, [])
         | FVvariant vi -> JCvariant vi
       in
       let alloc = alloc_region_table_name(root,r) in
       make_and acc
	 (let a = LPred("not_assigns",
                [lvar (* ~assigned: ? *) before alloc; 
                 lvar before v;
                 LVar v; make_union_loc p]) in
	  LNamed(reg_loc loc,a))
    ) mems a

let reads locs (fvi,r) =
  let refs = StringMap.empty
  in
  let mems = FieldOrVariantRegionMap.empty 
  in
  let refs,mems = 
    List.fold_left (collect_locations ~global_assertion:false LabelOld) (refs,mems) locs
  in
  let p = try FieldOrVariantRegionMap.find (fvi,r) mems with Not_found -> [] in
  make_union_loc p
  
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
  match lset with
    | JCLSvar _ -> lset
    | JCLSderef(lset,lab,fi,region) ->
	JCLSderef(old_to_pre_lset lset, old_to_pre lab, fi, region)
    | JCLSrange(lset,t1,t2) ->
	JCLSrange(old_to_pre_lset lset, 
		  Option_misc.map old_to_pre_term t1,
		  Option_misc.map old_to_pre_term t2)

let rec old_to_pre_loc loc =
  match loc with
    | JCLvar _ -> loc
    | JCLat(l,lab) -> 
	JCLat(old_to_pre_loc l, old_to_pre lab)
    | JCLderef(lset,lab,fi,region) ->
	JCLderef(old_to_pre_lset lset,old_to_pre lab, fi, region)


(* translates the heap update `e1.f = e2' 

   essentially we want

   let tmp1 = [e1] in
   let tmp2 = [e2] in
   f := upd(f,tmp1,tmp2)

   special cases are considered to avoid statically known safety properties:
   if e1 as the form p + i then we build

   let tmpp = [p] in 
   let tmpi = [i] in
   let tmp1 = shift(tmpp, tmpi) in
    // depending on type of p and value of i
   ...
   
*)
let rec make_upd lab loc ~infunction ~threats fi e1 e2 =
  let expr = expr ~infunction ~threats and offset = offset ~infunction ~threats in
  let tmpp = tmp_var_name () in
  let tmpi = tmp_var_name () in
  let tmp1 = tmp_var_name () in  
  let tmp2 = tmp_var_name () in
  (* we first handle e2 *)
  let e2' = expr e2 in
  let v = Var tmp2 in
  let lets = 
    [ (tmp2, coerce ~no_int_overflow:(not threats) 
         e2#name_label e2#loc 
         fi.jc_field_info_type 
         e2#typ e2 e2') ]
  in
  (* we then go to e1 *)
  let mem = Var(field_region_memory_name(fi,e1#region)) in
  let alloc = 
    alloc_region_table_name (JCtag(fi.jc_field_info_root, []), e1#region)
  in
  let alloc = 
    if Region.polymorphic e1#region then
      if StringRegionSet.mem (field_root_name fi, e1#region)
        infunction.jc_fun_info_effects.jc_writes.jc_effect_alloc_table
      then Deref alloc
      else Var alloc
    else Deref alloc
  in
  let v = 
    if field_of_union fi then
      (* Translate back value of field type to union type *)
      if integral_union (union_of_field fi) then
        (* Type of union is integer, and type of field an integral type *)
        match fi.jc_field_info_type with
          | JCTnative Tinteger -> v
          | JCTenum ri -> make_app (logic_int_of_enum ri) [v]
          | _ -> assert false
      else make_app (logic_union_of_field fi) [v]
    else v
  in
  let lets, upd =
    if threats then
      let p,off,lb,rb = destruct_pointer e1 in
      let p' = expr p in
      let i' = offset off in
      let letspi = [ (tmpp, p') ; (tmpi, i') ; 
		     (tmp1, make_app "shift" [Var tmpp; Var tmpi])] 
      in
      match off,lb,rb with
	| Int_offset s, Some lb, Some rb when bounded lb rb s ->
            let e1' = expr e1 in	    
	    (tmp1, e1') :: lets, 
            make_app "safe_upd_" [ mem ; Var tmp1; v ]
	| Int_offset s,Some lb,Some rb when lbounded lb s ->
	    letspi @ lets, 
            make_guarded_app ~name:lab IndexBounds loc "lsafe_bound_upd_" 
              [ mem ; Var tmpp; Var tmpi; 
		Cte (Prim_int (Num.string_of_num rb)); v ]
	| Int_offset s,Some lb,Some rb when rbounded rb s ->
	    letspi @ lets, 
            make_guarded_app ~name:lab IndexBounds loc "rsafe_bound_upd_" 
              [ mem ; Var tmpp; Var tmpi; 
		Cte (Prim_int (Num.string_of_num lb)); v ]
	| off,Some lb,Some rb ->
	    letspi @ lets, 
            make_guarded_app ~name:lab IndexBounds loc "bound_upd_" 
              [ mem ; Var tmpp; Var tmpi;  
		Cte (Prim_int (Num.string_of_num lb)); 
		Cte (Prim_int (Num.string_of_num rb)); v ]
	| Int_offset s,Some lb,None when lbounded lb s ->
	    letspi @ lets, 
            make_guarded_app ~name:lab IndexBounds loc "lsafe_lbound_upd_" 
              [ alloc; mem; Var tmpp; Var tmpi; v ]
	| off,Some lb,None ->
	    letspi @ lets, 
            make_guarded_app ~name:lab IndexBounds loc "lbound_upd_" 
              [ alloc; mem; Var tmpp; Var tmpi;
		Cte (Prim_int (Num.string_of_num lb)); v ]
	| Int_offset s,None,Some rb when rbounded rb s ->
	    letspi @ lets, 
            make_guarded_app ~name:lab IndexBounds loc "rsafe_rbound_upd_" 
              [ alloc; mem; Var tmpp; Var tmpi; v ]
	| off,None,Some rb ->
	    letspi @ lets, 
            make_guarded_app ~name:lab IndexBounds loc "rbound_upd_" 
              [ alloc; mem; Var tmpp; Var tmpi;
		Cte (Prim_int (Num.string_of_num rb)); v ]
	| Int_offset s,None,None when int_of_string s = 0 ->
	    (tmp1, p') :: lets, 
            make_guarded_app ~name:lab PointerDeref loc "upd_" 
              [ alloc; mem ; Var tmp1; v ]
	| off,None,None ->
	    letspi @ lets, 
            make_guarded_app ~name:lab PointerDeref loc "offset_upd_" 
              [ alloc; mem ; Var tmpp; Var tmpi; v ]
    else
      let e1' = expr e1 in	    
      (tmp1, e1') :: lets, 
      make_app "safe_upd_"
	[ mem ; Var tmp1 ; v ]
  in
  tmp1, tmp2, lets, upd

and make_deref lab loc ~infunction ~threats fi e =
  let expr = expr ~infunction ~threats and offset = offset ~infunction ~threats in
  let mem = field_region_memory_name(fi,e#region) in
  let mem = 
    if Region.polymorphic e#region then
      if field_of_union fi && 
        FieldOrVariantRegionMap.mem 
        (FVvariant (union_of_field fi),e#region)
        infunction.jc_fun_info_effects.jc_writes.jc_effect_memories
        || 
        not (field_of_union fi) &&
        FieldOrVariantRegionMap.mem (FVfield fi,e#region)
        infunction.jc_fun_info_effects.jc_writes.jc_effect_memories
      then Deref mem
      else Var mem
    else Deref mem
  in
  let alloc = 
    alloc_region_table_name (JCtag(fi.jc_field_info_root, []),
                             e#region)
  in
  let alloc = 
    if Region.polymorphic e#region then
      if StringRegionSet.mem (field_root_name fi, e#region)
        infunction.jc_fun_info_effects.jc_writes.jc_effect_alloc_table
      then Deref alloc
      else Var alloc
    else Deref alloc
  in
  let deref = if threats then
    match destruct_pointer e with
      | _,Int_offset s,Some lb,Some rb when bounded lb rb s ->
          make_app "safe_acc_" 
            [ mem ; expr e ]
      | p,(Int_offset s as off),Some lb,Some rb when lbounded lb s ->
          make_guarded_app ~name:lab IndexBounds loc "lsafe_bound_acc_" 
            [ mem ; expr p; offset off;
              Cte (Prim_int (Num.string_of_num rb)) ]
      | p,(Int_offset s as off),Some lb,Some rb when rbounded rb s ->
          make_guarded_app ~name:lab IndexBounds loc "rsafe_bound_acc_" 
            [ mem ; expr p; offset off;
              Cte (Prim_int (Num.string_of_num lb)) ]
      | p,off,Some lb,Some rb ->
          make_guarded_app ~name:lab IndexBounds loc "bound_acc_" 
            [ mem ; expr p; offset off; 
              Cte (Prim_int (Num.string_of_num lb)); 
              Cte (Prim_int (Num.string_of_num rb)) ]
      | p,(Int_offset s as off),Some lb,None when lbounded lb s ->
          make_guarded_app ~name:lab IndexBounds loc "lsafe_lbound_acc_" 
            [ alloc; mem; expr p; offset off ]
      | p,off,Some lb,None ->
          make_guarded_app ~name:lab IndexBounds loc "lbound_acc_" 
            [ alloc; mem; expr p; offset off;
              Cte (Prim_int (Num.string_of_num lb)) ]
      | p,(Int_offset s as off),None,Some rb when rbounded rb s ->
          make_guarded_app ~name:lab IndexBounds loc "rsafe_rbound_acc_" 
            [ alloc; mem; expr p; offset off ]
      | p,off,None,Some rb ->
          make_guarded_app ~name:lab IndexBounds loc "rbound_acc_" 
            [ alloc; mem; expr p; offset off;
              Cte (Prim_int (Num.string_of_num rb)) ]
      | p,Int_offset s,None,None when int_of_string s = 0 ->
          make_guarded_app ~name:lab PointerDeref loc "acc_" 
            [ alloc; mem ; expr p ]
      | p,off,None,None ->
          make_guarded_app ~name:lab PointerDeref loc "offset_acc_" 
            [ alloc; mem ; expr p; offset off ]
  else
    make_app "safe_acc_"
      [ mem ; 
        (* coerce e#loc integer_type e#typ *) (expr e) ]
  in
  if field_of_union fi then
    (* Translate back access to union type to field type *)
    if integral_union (union_of_field fi) then
      (* Type of union is integer, and type of field an integral type *)
      match fi.jc_field_info_type with
        | JCTnative Tinteger -> deref
        | JCTenum ri ->
            if not threats then 
              make_app (safe_fun_enum_of_int ri) [deref]
            else
              make_guarded_app ~name:lab ArithOverflow loc 
                (fun_enum_of_int ri) [deref]
        | _ -> assert false
    else make_app (logic_field_of_union fi) [deref]
  else deref

and offset ~infunction ~threats = function
  | Int_offset s -> Cte (Prim_int s)
  | Expr_offset e -> 
      coerce ~no_int_overflow:(not threats)
        e#name_label e#loc integer_type e#typ e
        (expr ~infunction ~threats e)

and type_assert ~infunction ~threats ty e (lets, params) =
  let opt =
    match ty with
      | JCTpointer (si, n1o, n2o) ->
	  let tmp = tmp_var_name () in
	  let alloc = alloc_table_name si in
	  let offset_mina n = 
	    LPred ("le_int",
		   [LApp ("offset_min", 
			  [LVar alloc; LVar tmp]);
		    LConst (Prim_int (Num.string_of_num n))]) 
	  in
	  let offset_maxa n =
	    LPred ("ge_int",
		   [LApp ("offset_max", 
			  [LVar alloc; LVar tmp]);
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
  let e = expr_coerce ~infunction ~threats ty e in
  match opt with
    | None -> None :: lets, e :: params
    | Some (tmp,a) -> Some (tmp, e, a) :: lets , (Var tmp) :: params

and expr ~infunction ~threats e : expr =
  let expr = expr ~infunction ~threats in
  let loc = e#loc in
  let lab = (* Only use label once *)
    let used = ref false in fun () ->
      if !used then "" else (used := true; e#name_label)
  in
  let ie = match e#node with
    | JCEconst JCCnull -> Var "null"
    | JCEconst c -> Cte(const c)
    | JCEvar v ->
        if v.jc_var_info_assigned 
        then Deref v.jc_var_info_final_name
        else Var v.jc_var_info_final_name
    | JCEunary(op, e1) ->
        let e1' = expr e1 in
        make_app (unary_op op) 
          [coerce ~no_int_overflow:(not threats) 
             (lab()) loc (native_operator_type op) e1#typ e1 e1' ]
    | JCEbinary(e1, (_, (`Pointer | `Logic) as op), e2) ->
        let e1' = expr e1 in
        let e2' = expr e2 in
        make_app (bin_op op) [ e1'; e2']        
(*    | JCEbinary(e1, (`Bland, _), e2) ->
        (* lazy conjunction *)
        let e1' = expr e1 in
        let e2' = expr e2 in
        And(e1',e2')    
    | JCEbinary(e1,Blor,e2) ->
        (* lazy disjunction *)
        let e1' = expr e1 in
        let e2' = expr e2 in
        Or(e1',e2')     *)
    | JCEbinary(e1, (_, #native_operator_type as op), e2) ->
        let e1' = expr e1 in
        let e2' = expr e2 in
        let t = native_operator_type op in
        (match fst op with
           | `Bdiv | `Bmod ->
               make_guarded_app ~name:(lab()) DivByZero loc
           | _ -> make_app)
          (bin_op op) 
          [ coerce ~no_int_overflow:(not threats) 
              e1#name_label e1#loc t e1#typ e1 e1'; 
            coerce ~no_int_overflow:(not threats) 
              e2#name_label e2#loc t e2#typ e2 e2'] 
    | JCEif(e1,e2,e3) -> 
        let e1 = expr e1 in
        let e2 = expr e2 in
        let e3 = expr e3 in
        If(e1,e2,e3)
    | JCEshift(e1,e2) -> 
        let e1' = expr e1 in
        let e2' = expr e2 in
        make_app "shift" 
          [e1'; 
           coerce ~no_int_overflow:(not threats) 
             e2#name_label e2#loc integer_type e2#typ e2 e2']
(*    | JCEsub_pointer(e1,e2) -> 
        let e1' = expr e1 in
        let e2' = expr e2 in
        (* FIXME: need to check that are in the same block *)
        make_app "sub_pointer" [ e1'; e2']*)
    | JCEoffset(k,e,st) -> 
        let alloc = 
          alloc_region_table_name (JCtag(st, []), e#region) in
        let alloc = 
          if Region.polymorphic e#region then
            if StringRegionSet.mem (root_name st, e#region)
              infunction.jc_fun_info_effects.jc_writes.jc_effect_alloc_table
            then Deref alloc
            else Var alloc
          else Deref alloc
        in
        let f = match k with
          | Offset_min -> "offset_min"
          | Offset_max -> "offset_max"
        in
        make_app f [alloc; expr e] 
    | JCEinstanceof(e,t) ->
        let e = expr e in
        let tag = tag_table_name (JCtag(t, [])) in
        (* always safe *)
        make_app "instanceof_" [Deref tag; e; Var (tag_name t)]
    | JCEcast (e, si) ->
        if struct_of_union si then expr e else
        (* ??? TODO faire ca correctement: on peut tres bien caster des expressions qui ne sont pas des termes !!! *)
(*
        let et, _ = term ~global_assertion:false LabelHere LabelHere (term_of_expr e) in*)
          let tmp = tmp_var_name () in
          let tag = tag_table_name (JCtag(si, [])) in
          let call = 
            make_guarded_app ~name:(lab()) DownCast loc "downcast_" 
              [Deref tag; Var tmp; Var (tag_name si)]
          in
          Let(tmp, expr e, call)
    | JCErange_cast(e1,ri) ->
        let e1' = expr e1 in
        coerce ~no_int_overflow:(not threats)
          (lab()) e#loc (JCTenum ri) e1#typ e1 e1'
    | JCEreal_cast(e1,rc) ->
        let e1' = expr e1 in
        begin match rc with
          | Integer_to_real ->
              coerce ~no_int_overflow:(not threats)
                (lab()) e#loc real_type integer_type e1 e1'
          | Real_to_integer ->
              coerce ~no_int_overflow:(not threats)
                (lab()) e#loc integer_type real_type e1 e1'
        end
    | JCEderef(e,fi) ->
	make_deref (lab()) loc ~infunction ~threats fi e
    | JCEalloc (siz, st) ->
        let alloc = alloc_region_table_name (JCtag(st, []), e#region) in
        let tag = tag_table_name (JCtag(st, [])) in
(*      
        let fields = embedded_struct_fields st in
        let fields = List.map (fun fi -> (fi,e#region)) fields in
        let roots = embedded_struct_roots st in
        let roots = List.map find_tag_or_variant roots in
        let roots = List.map (fun a -> (a, e#region)) roots 
*)
        let fields = all_memories ~select:fully_allocated (JCtag(st, [])) in
        let fields = List.map (fun fi -> (fi, e#region)) fields in
        let roots = all_types ~select:fully_allocated (JCtag(st, [])) in
        let roots = List.map (fun a -> (JCvariant a, e#region)) roots in
        begin
          match !Jc_options.inv_sem with
            | InvOwnership ->
                let mut = mutable_name (JCtag(st, [])) in
                let com = committed_name (JCtag(st, [])) in
                make_app "alloc_parameter_ownership" 
                  [Var alloc; Var mut; Var com; Var tag; Var (tag_name st); 
                   coerce ~no_int_overflow:(not threats) 
                     siz#name_label siz#loc integer_type 
                     siz#typ siz (expr siz)]
            | InvArguments | InvNone ->
              (* Claude : pourquoi un cas particulier pour taille 1 ?? *)
                begin match siz#node with 
                  | JCEconst(JCCinteger "1") ->
                      make_app (alloc_one_param_name st) 
                        ([Void; Var alloc]
                         @ (List.map (nvar $ alloc_region_table_name) roots)
                         @ (List.map (nvar $ field_region_memory_name) fields))
                  | _ ->
                      make_guarded_app 
                        ~name:(lab()) AllocSize loc
                        (alloc_param_name st)
                        ([coerce ~no_int_overflow:(not threats) 
                            siz#name_label siz#loc integer_type 
                            siz#typ siz (expr siz); Var alloc]
                         @ (List.map (nvar $ alloc_region_table_name) roots)
                         @ (List.map (nvar $ field_region_memory_name) fields))
                end
        end
    | JCEfree e ->
        let st = match e#typ with
          | JCTpointer(JCtag(st, []), _, _) -> st
          | JCTpointer(JCvariant vi, _, _) -> assert false (* TODO *)
          | _ -> assert false
        in      
        let alloc = 
          alloc_region_table_name (JCtag(st, []), e#region) in
        if !Jc_options.inv_sem = InvOwnership then
          let com = committed_name (JCtag(st, [])) in
          make_app "free_parameter_ownership" [Var alloc; Var com; expr e]
        else
          make_app "free_parameter" [Var alloc; expr e]
    | JCEapp call ->
        let params = match call.jc_call_fun with
          | JClogic_fun f -> f.jc_logic_info_parameters
          | JCfun f -> f.jc_fun_info_parameters
        in
        let l = call.jc_call_args in
        let arg_types_asserts, el =
          try match params with
            | [] -> [], [Void]
            | p -> 
		let tyl = List.map (fun p -> p.jc_var_info_type) p in
		  List.fold_right2 
		    (type_assert ~infunction ~threats)
		    tyl l ([],[])
	  with Invalid_argument _ -> assert false
        in
        let write_mems, read_mems, write_allocs, read_allocs =
          match call.jc_call_fun with
            | JClogic_fun f ->
                [],
                read_mems
                  ~caller_writes: infunction.jc_fun_info_effects.jc_writes
                  ~callee_reads: f.jc_logic_info_effects
                  ~callee_writes: empty_effects
                  ~regions: call.jc_call_region_assoc,
                [],
                read_allocs
                  ~caller_writes: infunction.jc_fun_info_effects.jc_writes
                  ~callee_reads: f.jc_logic_info_effects
                  ~callee_writes: empty_effects
                  ~regions: call.jc_call_region_assoc
            | JCfun f ->
                write_mems
                  ~callee_writes: f.jc_fun_info_effects.jc_writes
                  ~regions: call.jc_call_region_assoc,
                read_mems
                  ~caller_writes: infunction.jc_fun_info_effects.jc_writes
                  ~callee_reads: f.jc_fun_info_effects.jc_reads
                  ~callee_writes: f.jc_fun_info_effects.jc_writes
                  ~regions: call.jc_call_region_assoc,
                write_allocs
                  ~regions: call.jc_call_region_assoc
                  ~callee_writes: f.jc_fun_info_effects.jc_writes,
                read_allocs
                  ~caller_writes: infunction.jc_fun_info_effects.jc_writes
                  ~callee_reads: f.jc_fun_info_effects.jc_reads
                  ~callee_writes: f.jc_fun_info_effects.jc_writes
                  ~regions: call.jc_call_region_assoc
        in
        let el = el @ write_allocs @ write_mems @ read_allocs @ read_mems in
        let name = match call.jc_call_fun with
          | JCfun f -> f.jc_fun_info_final_name
          | JClogic_fun f -> f.jc_logic_info_final_name
        in
        let call = make_guarded_app ~name:(lab()) UserCall loc name el in
        let arg_types_assert =
          List.fold_right
            (fun opt acc -> 
               match opt with
                 | None -> acc
                 | Some(tmp,e,a) -> make_and a acc)
            arg_types_asserts LTrue
        in
        let call = 
          if arg_types_assert = LTrue || not threats then call else
            Assert (arg_types_assert, call) 
        in
        let call =
          List.fold_right
            (fun opt c -> 
               match opt with
                 | None -> c
                 | Some(tmp,e,ass) -> Let(tmp,e,c))
            arg_types_asserts call
        in
        call
    | JCEassign_var (vi, e2) -> 
        let e2' = expr e2 in
        let n = vi.jc_var_info_final_name in
        let ie = Assign (n, coerce ~no_int_overflow:(not threats) 
			   e2#name_label e2#loc vi.jc_var_info_type 
			   e2#typ e2 e2')
	in
	let assign_var_assert = fst 
	  (type_assert ~infunction ~threats vi.jc_var_info_type e2 ([], []))
	in
	  assert (List.length assign_var_assert = 1);
	  let assign_var_assert = List.hd assign_var_assert in
	  let ie =
	    match assign_var_assert with
	      | None -> ie
	      | Some (tmp, e, a) ->
		  let ie = if not threats then ie else Assert (a, ie) in
		    Let (tmp, e, ie)
	  in
	    if e#typ = Jc_pervasives.unit_type then ie else append ie (var vi)
    | JCEassign_heap (e1, fi, e2) -> 
        let tmp1, tmp2, lets, upd = 
	  make_upd ~infunction ~threats (lab()) e#loc fi e1 e2 
        in
	let assign_heap_assert = fst 
	  (type_assert ~infunction ~threats fi.jc_field_info_type e2 ([], []))
	in
	assert (List.length assign_heap_assert = 1);
	let assign_heap_assert = List.hd assign_heap_assert in
	let upd =
	  match assign_heap_assert with
	    | None -> upd
	    | Some (tmp, e, a) ->
		let upd = if not threats then upd else Assert (a, upd) in
		Let (tmp, e, upd)
	in	  
	(* Yannick: ignore variables to be able to refine update function used. *)      
	(* Claude: do not ignore variable tmp2, since it may involve a coercion. 
	   Anyway, safety of the update do not depend on e2 *)
        let upd = 
	  if threats && !Jc_options.inv_sem = InvOwnership then
            append (assert_mutable (LVar tmp1) fi) upd else upd 
	in
	let upd =
	  if e#typ = Jc_pervasives.unit_type then upd else 
	    let tmp1var = 
	      new expr ~typ:e1#typ ~region:e1#region
		(JCEvar(Jc_pervasives.var e1#typ tmp1)) 
	    in
	    append upd
	      (make_deref "" loc ~infunction ~threats:false fi tmp1var)
	in
        let lets = make_lets lets upd in
        if !Jc_options.inv_sem = InvOwnership then
          append lets (assume_field_invariants fi)
        else
          lets
	    (* if !Jc_options.inv_sem = Jc_options.InvOwnership then   
	       (make_assume_field_assocs (fresh_program_point ()) fi)) *)
    | JCEblock l ->
        List.fold_right append (List.map expr l) Void
    | JCEloop (la, s) ->
	let inv = List.filter (compatible_with_current_behavior $ fst) 
	  la.jc_loop_invariant
	in
	let inv = List.map snd inv in
        let inv = List.map (named_assertion
			      ~global_assertion:false ~relocate:false
			      LabelHere
			      LabelPre
			   ) inv
        in 
	let inv = make_and_list inv in
	(* free invariant: trusted or not *)
        let free_inv = named_assertion 
          ~global_assertion:false ~relocate:false
          LabelHere 
          LabelPre 
          la.jc_free_loop_invariant 
        in
        let inv = if Jc_options.trust_ai then inv else make_and inv free_inv in
	(* loop assigns  *)
	(* the assigns clause for the function is taken *)
	(* TODO: add also a loop_assigns annotation *)

	let loop_assigns = 
	  let _cur_behavior = get_current_behavior () in
	  match get_current_spec () with
	    | None -> assert false
	    | Some s ->
		List.fold_left
		  (fun acc (loc,id,b) ->
		     if true (* TODO id = cur_behavior *) then
		       match b.jc_behavior_assigns with
			 | None -> acc
			 | Some(_loc,locs) -> 
			     let l = List.map old_to_pre_loc locs in
			     match acc with
			       | None -> Some l
			       | Some l' -> Some (l@l')
		     else acc)
		  None 
		  s.jc_fun_behavior
	in
        let inv = 
	  match loop_assigns with
	    | None -> inv
	    | _ ->
		let f = match !current_function with
		  | None -> assert false
		  | Some f -> f
		in
		make_and
		  inv
                  (named_jc_assertion
                     Loc.dummy_position
                     (assigns LabelPre f.jc_fun_info_effects loop_assigns Loc.dummy_position))
	in
	(* loop body *) 
        let body = [expr s] in
        let body = 
          if Jc_options.trust_ai then
            BlackBox (Annot_type (LTrue, unit_type, [], [], free_inv, [])) :: body 
          else body
        in
	(* final generation, depending on presence of variant or not *)
        begin 
          match la.jc_loop_variant with
            | Some t when threats ->
                let variant, lets = named_term
                  ~global_assertion:false ~relocate:false
                  LabelHere
                  LabelPre
                  t
                in
                assert (lets = []);
                While(Cte(Prim_bool true), inv,
                      Some (term_coerce t#loc integer_type 
                              t#typ variant,None), body)
            | _ ->
                While(Cte(Prim_bool true), inv,
                      None, body)
        end
    | JCEassert(behav,a) -> 
	if compatible_with_current_behavior behav then
          Assert(named_assertion
                   ~global_assertion:false ~relocate:false
                   LabelHere
                   LabelPre
                   a,
		 Void)
	else Void
    | JCElet (vi, e, s) -> 
        begin
          let e' = match e with
            | None -> 
                any_value vi.jc_var_info_type
            | Some e -> 
		(* eprintf "decl of vi=%s@." vi.jc_var_info_name; *)
                let e' = coerce ~no_int_overflow:(not threats) 
                  (lab()) e#loc vi.jc_var_info_type e#typ 
                  e (expr e)
		in
		let assign_var_assert = fst 
		  (type_assert ~infunction ~threats vi.jc_var_info_type e ([], []))
		in
		  assert (List.length assign_var_assert = 1);
		  let assign_var_assert = List.hd assign_var_assert in
		    match assign_var_assert with
		      | None -> e'
		      | Some (tmp, e, a) ->
			  let e' = if not threats then e' else Assert (a, e') in
			    Let (tmp, e, e')
          in
            if vi.jc_var_info_assigned then 
              Let_ref(vi.jc_var_info_final_name, e', expr s)
            else 
              Let(vi.jc_var_info_final_name, e', expr s)
        end
    | JCEreturn_void -> Raise(jessie_return_exception,None)     
    | JCEreturn (t,e) -> 
	let return_type_asserts, _ = 
	  type_assert ~infunction ~threats infunction.jc_fun_info_result.jc_var_info_type e ([], []) in
	let return_type_assert =
	  List.fold_right
	    (fun opt acc -> 
	       match opt with
		 | None -> acc
		 | Some (_, _, a) -> make_and a acc)
	    return_type_asserts LTrue
	in
	let assign = 
	    Assign 
	      (jessie_return_variable,
	       coerce ~no_int_overflow:(not threats) 
		  (lab()) e#loc t e#typ e (expr e))
	in
	let assign = 
	  if return_type_assert = LTrue || not threats then assign else
	    Assert (return_type_assert, assign)
	in
	let assign =
	  List.fold_right
	    (fun opt assign -> 
	       match opt with
		 | None -> assign
		 | Some (tmp, e, _) -> Let (tmp, e, assign))
	    return_type_asserts assign
	in
	append assign (Raise (jessie_return_exception, None))
    | JCEunpack(st, e1, as_t) ->
        let e1 = expr e1 in 
        make_guarded_app ~name:(lab()) Unpack e#loc
          (unpack_name st) [e1; Var (tag_name as_t)]
    | JCEpack(st, e1, from_t) ->
        let e1 = expr e1 in 
        make_guarded_app ~name:(lab()) Pack e#loc
          (pack_name st) [e1; Var (tag_name from_t)]
    | JCEthrow (ei, Some e) -> 
        let e = expr e in
        Raise(exception_name ei,Some e)
    | JCEthrow (ei, None) -> 
        Raise(exception_name ei,None)
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
              eprintf "Warning: exception %s cannot be thrown@."
                ei.jc_exception_info_name;
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
  let ie = 
    if e#typ = Jc_pervasives.unit_type 
      && e#original_type <> Jc_pervasives.unit_type then
	(* Create dummy temporary *)
	let tmp = tmp_var_name () in
	Let(tmp,ie,Void)
    else ie
  in
  match lab() with
    | "" -> ie
    | lab -> Label(lab, ie)

and expr_coerce ~infunction ~threats ty e =
  coerce ~no_int_overflow:(not threats)
    e#name_label e#loc ty
    e#typ e (expr ~infunction ~threats e)

(*******************************************************************************)
(*                                  Structures                                 *)
(*******************************************************************************)

(*
If T is a structure:
   valid_T(p, a, b, allocs ...) =
     if T is root:
       offset_min(alloc_i, p) == a &&
       offset_max(alloc_i, p) == b
     else if S is the direct superclass of T:
       valid_S(p, a, b, allocs ...)
     and for all field (T'[a'..b'] f) of p,
       valid_T'(p.f, a', b', allocs ...)
If T is a variant, then we only have the condition on offset_min and max.
*)
let make_valid_pred tov =
  let p = "p" in
  let a = "a" in
  let b = "b" in
  let params =
    let allocs = List.map
      (fun vi ->
         let tov = JCvariant vi in
           alloc_table_name tov,
         alloc_table_type tov)
      (Jc_struct_tools.all_types ~select:fully_allocated tov)
    in
    let memories = List.map
      (fun fi ->
         field_memory_name fi,
         field_memory_type fi)
      (Jc_struct_tools.all_memories ~select:fully_allocated tov)
    in
    let p = p, pointer_type tov in
    let a = a, why_integer_type in
    let b = b, why_integer_type in
      p::a::b::allocs@memories
  in
  let validity =
    let omin, omax, super_valid = match tov with
      | JCtag ({ jc_struct_info_parent = Some(st, pp) }, _) ->
          LTrue,
          LTrue,
          make_valid_pred_app (JCtag(st, pp)) (LVar p) (LVar a) (LVar b)
      | JCtag ({ jc_struct_info_parent = None }, _)
      | JCvariant _ 
      | JCunion _ ->
          make_eq (make_offset_min tov (LVar p)) (LVar a),
          make_eq (make_offset_max tov (LVar p)) (LVar b),
          LTrue
    in
    let fields_valid = match tov with
      | JCtag(st, _) ->
          List.map
            (function
               | { jc_field_info_type =
                     JCTpointer(ftov, Some fa, Some fb) } as fi ->
                   make_valid_pred_app ftov
                     (make_select_fi fi (LVar p))
                     (const_of_num fa)
                     (const_of_num fb)
               | _ ->
                   LTrue)
            st.jc_struct_info_fields
      | JCvariant _ | JCunion _ ->
          [LTrue]
    in
    make_and_list (omin::omax::super_valid::fields_valid)
  in
  Predicate(false, valid_pred_name tov, params, validity)

let tr_struct st acc =
  let alloc_ty = alloc_table_type (JCtag(st, [])) in
  let tagid_type = tag_id_type (JCtag(st, [])) in
  let ptr_type = pointer_type (JCtag(st, [])) in
(*  let all_fields = embedded_struct_fields st in
  let all_roots = embedded_struct_roots st in
  let all_roots = List.map find_struct all_roots in*)
  let all_fields = all_memories ~select:fully_allocated (JCtag(st, [])) in
  let all_roots = all_types ~select:fully_allocated (JCtag(st, [])) in
  let all_tovs = List.map (fun st -> JCvariant st) all_roots in
  let alloc = alloc_table_name (JCtag(st, [])) in
  let tagtab = tag_table_name (JCtag(st, [])) in
    (* Declarations of field memories. *)
  let acc =
    if !Jc_options.separation_sem = SepRegions then acc else
      if (struct_variant st).jc_variant_info_is_union then acc else
        List.fold_left
          (fun acc fi ->
             let mem = field_memory_type fi in
             Param(
               false,
               field_memory_name fi,
               Ref_type(Base_type mem))::acc)
          acc st.jc_struct_info_fields
  in
  (* Declarations of translation functions for union *)
  let vi = struct_variant st in
  let acc = 
    if not vi.jc_variant_info_is_union then acc else
      if integral_union vi then acc else
        let uty = simple_logic_type (union_memory_type_name (struct_variant st)) in
        List.fold_left
          (fun acc fi ->
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
             :: acc)
          acc st.jc_struct_info_fields
  in
  (* declaration of the tag_id *)
  let acc =
    Logic(false,tag_name st,[],tagid_type)::acc
  in

  let acc = (make_valid_pred (JCtag(st, []))) :: acc in

  (* Allocation of one element parameter. *)
  let alloc_type = 
    Annot_type(
      (* no pre *)
      LTrue,
      (* [st_root pointer] *)
      Base_type ptr_type,
      (* [reads all_fields writes alloc,tagtab] *)
      (List.map alloc_table_name all_tovs
        @ List.map (fun fi -> fi.jc_field_info_final_name) all_fields),[alloc;tagtab],
      (* normal post *)
      make_and_list [
        (* [valid_one_st(result,alloc...)] *)
        make_valid_one_pred_app (JCtag(st, [])) (LVar "result");
        (* [instanceof(tagtab,result,tag_st)] *)
        LPred("instanceof",[LVar tagtab;LVar "result";LVar(tag_name st)]);
        (* [alloc_extends(old(alloc),alloc)] *)
        LPred("alloc_extends",[LVarAtLabel(alloc,"");LVar alloc]);
        (* [alloc_extern(old(alloc),result)] *)
        LPred("alloc_extern",[LVarAtLabel(alloc,"");LVar "result"])
      ],
      (* no exceptional post *)
      [])
  in
  let alloc_type =
    List.fold_right (fun fi acc ->
      Prod_type(field_memory_name fi,
                Ref_type(Base_type(field_memory_type fi)),
                acc)
    ) all_fields alloc_type 
  in
  let alloc_type =
    List.fold_right (fun a acc ->
      Prod_type(alloc_table_name a,Ref_type(Base_type(alloc_table_type a)),acc)
    ) all_tovs alloc_type
  in
  let alloc_type = Prod_type(alloc,Ref_type(Base_type alloc_ty),alloc_type) in
  let alloc_type = Prod_type("tt",unit_type,alloc_type) in
  let acc = 
    Param(false,alloc_one_param_name st,alloc_type) :: acc
  in

  (* Allocation parameter. *)
  let alloc_type = 
    Annot_type(
      (* [n >= 0] *)
      LPred("ge_int",[LVar "n";LConst(Prim_int "0")]),
      (* [st_root pointer] *)
      Base_type ptr_type,
      (* [reads all_fields; writes alloc,tagtab] *)
      (List.map alloc_table_name all_tovs
        @ List.map (fun fi -> fi.jc_field_info_final_name) all_fields), [alloc; tagtab],
      (* normal post *)
      make_and_list [
        (* [valid_st(result,0,n-1,alloc...)] *)
        make_valid_pred_app (JCtag(st, []))
          (LVar "result")
          (LConst(Prim_int "0"))
          (LApp("sub_int",[LVar "n"; LConst(Prim_int "1")]));
        (* [instanceof(tagtab,result,tag_st)] *)
        LPred("instanceof",[LVar tagtab;LVar "result";LVar(tag_name st)]);
        (* [alloc_extends(old(alloc),alloc)] *)
        LPred("alloc_extends",[LVarAtLabel(alloc,"");LVar alloc]);
        (* [alloc_extern(old(alloc),result)] *)
        LPred("alloc_extern",[LVarAtLabel(alloc,"");LVar "result"])
      ],
      (* no exceptional post *)
      [])
  in
  let alloc_type =
    List.fold_right (fun fi acc ->
      Prod_type(field_memory_name fi,
                Ref_type(Base_type(field_memory_type fi)),
                acc)
    ) all_fields alloc_type
  in
  let alloc_type =
    List.fold_right (fun a acc ->
      Prod_type(alloc_table_name a,Ref_type(Base_type(alloc_table_type a)),acc)
    ) all_tovs alloc_type
  in
  let alloc_type = Prod_type(alloc,Ref_type(Base_type alloc_ty),alloc_type) in
  let alloc_type = Prod_type("n",Base_type why_integer_type,alloc_type) in
  let acc = 
    Param(false,alloc_param_name st,alloc_type) :: acc
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

(*******************************************************************************)
(*                                Logic functions                              *)
(*******************************************************************************)

let tr_logic_const vi init acc =
  let decl =
    Logic (false, vi.jc_var_info_final_name, [], tr_base_type vi.jc_var_info_type) :: acc
  in
    match init with
      | None -> decl
      | Some(t,ty) ->
          let t', lets = term ~global_assertion:true ~relocate:false LabelHere LabelHere t in
          let vi_ty = vi.jc_var_info_type in
          let t_ty = t#typ in
          (* eprintf "logic const: max type = %a@." print_type ty; *)
          let pred =
            LPred (
              "eq",
              [term_coerce Loc.dummy_position ty vi_ty (LVar vi.jc_var_info_name); 
               term_coerce t#loc ty t_ty t'])
          in
        let ax =
          Axiom(
            vi.jc_var_info_final_name ^ "_value_axiom",
            make_pred_binds lets pred
          )
        in
        ax::decl

let tr_logic_fun li ta acc =
  let params =
    List.map
      (fun vi ->
         (vi.jc_var_info_final_name,
           tr_base_type vi.jc_var_info_type))
      li.jc_logic_info_parameters
  in  
  let params_reads = 
    Jc_interp_misc.logic_params ~label_in_name:true li @ params 
  in
  let decl =
    match li.jc_logic_info_result_type, ta with
        (* Predicate *)
      | None, JCAssertion a -> 
          let a = assertion ~global_assertion:true ~relocate:false LabelHere LabelHere a in
            Predicate (false, li.jc_logic_info_final_name,params_reads, a) 
              (* Function *)
      | Some ty, JCTerm t -> 
          let ret = tr_base_type ty in
          let t, lets = term ~global_assertion:true ~relocate:false LabelHere LabelHere t in
          assert (lets = []);
          Function(false,li.jc_logic_info_final_name,params_reads, ret, t) 
      (* Logic *)
      | tyo, JCReads r ->
          let ret = match tyo with
            | None -> simple_logic_type "prop"
            | Some ty -> tr_base_type ty
          in
          Logic(false, li.jc_logic_info_final_name, params_reads, ret)
      (* Other *)
      | _ -> assert false
  in 
  let acc = decl :: acc in

  (* no_update axioms *)
  let acc = match ta with JCAssertion _ | JCTerm _ -> acc | JCReads pset ->
    let memory_params_reads = 
      Jc_interp_misc.memory_logic_params ~label_in_name:true li
    in
    let params_names = List.map fst params_reads in
    let normal_params = List.map (fun name -> LVar name) params_names in
    snd (List.fold_left
      (fun (count,acc) param ->
	 let paramty = snd param in
	 if not (is_memory_type paramty) then count,acc else
	   let (fvi,r),_ = (* Recover which memory it is exactly *)
	     List.find (fun ((fvi,r),(n,_)) -> n = fst param) 
	       memory_params_reads
	   in
	   let zonety,basety = deconstruct_memory_type_args paramty in
	   let pset = reads pset (fvi,r) in
	   let sepa = LNot(LPred("in_pset",[LVar "tmp";pset])) in
	   let update_params = 
             List.map (fun name ->
			 if name = fst param then
			   LApp("store",[LVar name;LVar "tmp";LVar "tmpval"])
			 else LVar name
		      ) params_names
	   in
	   let a = 
             match li.jc_logic_info_result_type with
	       | None ->
		   LImpl(
                     sepa,
                     LIff(
		       LPred(li.jc_logic_info_final_name,normal_params),
		       LPred(li.jc_logic_info_final_name,update_params)))
	       | Some rety ->
		   LImpl(
                     sepa,
                     LPred("eq",[
			     LApp(li.jc_logic_info_final_name,normal_params);
			     LApp(li.jc_logic_info_final_name,update_params)]))
	   in
	   let a = 
             List.fold_left (fun a (name,ty) -> LForall(name,ty,a)) a params_reads
	   in
	   let a = 
             LForall(
	       "tmp",raw_pointer_type zonety,
	       LForall(
		 "tmpval",basety,
		 a))
	   in
	   let name = 
	     "no_update_" ^ li.jc_logic_info_name ^ "_" ^ string_of_int count
	   in
	   count + 1, Axiom(name,a) :: acc
      ) (0,acc) params_reads)
  in

  (* no_assign axioms *)
  let acc = match ta with JCAssertion _ | JCTerm _ -> acc | JCReads pset ->
    let memory_params_reads = 
      Jc_interp_misc.memory_logic_params ~label_in_name:true li
    in
    let params_names = List.map fst params_reads in
    let normal_params = List.map (fun name -> LVar name) params_names in
    snd (List.fold_left
      (fun (count,acc) param ->
	 let paramty = snd param in
	 if not (is_memory_type paramty) then count,acc else
	   let (fvi,r),_ = (* Recover which memory it is exactly *)
	     List.find (fun ((fvi,r),(n,_)) -> n = fst param) 
	       memory_params_reads
	   in
	   let zonety,basety = deconstruct_memory_type_args paramty in
	   let pset = reads pset (fvi,r) in
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
             match li.jc_logic_info_result_type with
	       | None ->
		   LImpl(
                     make_and sepa upda,
                     LIff(
		       LPred(li.jc_logic_info_final_name,normal_params),
		       LPred(li.jc_logic_info_final_name,update_params)))
	       | Some rety ->
		   LImpl(
                     make_and sepa upda,
                     LPred("eq",[
			     LApp(li.jc_logic_info_final_name,normal_params);
			     LApp(li.jc_logic_info_final_name,update_params)]))
	   in
	   let a = 
             List.fold_left (fun a (name,ty) -> LForall(name,ty,a)) a params_reads
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
	     "no_assign_" ^ li.jc_logic_info_name ^ "_" ^ string_of_int count
	   in
	   count + 1, Axiom(name,a) :: acc
      ) (0,acc) params_reads)
  in

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
(*     FieldOrVariantRegionMap.fold (fun (fvi,r) labels acc -> *)
(*       let update_params =  *)
(*         List.map (fun name -> *)
(*           if name = field_or_variant_region_memory_name(fvi,r) then *)
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
(*       let structty = match fvi with  *)
(*         | FVfield fi -> JCtag(fi.jc_field_info_struct, []) *)
(*         | FVvariant vi -> JCvariant vi *)
(*       in *)
(*       let basety = match fvi with *)
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
(*       let fviname = match fvi with *)
(*         | FVfield fi -> fi.jc_field_info_name *)
(*         | FVvariant vi -> vi.jc_variant_info_name *)
(*       in *)
(*       Axiom( *)
(*         "full_separated_" ^ li.jc_logic_info_name ^ "_" ^ fviname, *)
(*         a) :: acc *)
(*     ) li.jc_logic_info_effects.jc_effect_memories acc *)

(*******************************************************************************)
(*                                  Functions                                  *)
(*******************************************************************************)

let parameter v =
  (v.jc_var_info_final_name,tr_type v.jc_var_info_type)
    
let excep_posts_for_others eopt excep_posts =
  ExceptionMap.fold
    (fun ei l acc ->
       match eopt with 
         | Some ei -> acc
         | None -> (exception_name ei, LTrue)::acc)
    excep_posts []
    
let interp_fun_params f write_mems read_mems annot_type =
  let annot_type = 
    if !Jc_options.separation_sem = SepNone then annot_type else
      List.fold_right (fun (name,ty) acc ->
        Prod_type(name,Base_type ty,acc)
      ) read_mems annot_type
  in
  let annot_type = 
    if !Jc_options.separation_sem = SepNone then annot_type else
      List.fold_right (fun (name,ty) acc ->
        Prod_type(name,Ref_type(Base_type ty),acc)
      ) write_mems annot_type
  in
  match f.jc_fun_info_parameters with
    | [] ->
        Prod_type("tt",unit_type, annot_type)
    | l ->
        List.fold_right
          (fun v acc ->
             Prod_type(v.jc_var_info_final_name,
                       tr_type v.jc_var_info_type,
                       acc))
          l annot_type
       
let function_body f spec behavior_name body =
  set_current_behavior behavior_name;
  set_current_spec spec;
  let e = expr ~infunction:f ~threats:(behavior_name = "safety") body in
  reset_current_behavior ();
  reset_current_spec ();
  e

let assume_in_precondition behav pre =
  match behav.jc_behavior_assumes with
    | None -> pre
    | Some assum ->
	make_and (assertion ~global_assertion:false ~relocate:false
		    LabelHere LabelHere assum) pre

let assume_in_postcondition behav post =
  match behav.jc_behavior_assumes with
    | None -> post
    | Some assum ->
	make_impl (assertion ~global_assertion:false ~relocate:true
		     LabelOld LabelOld assum) post
  
let tr_fun f funloc spec body acc =
  let requires_param = 
    (named_assertion 
       ~global_assertion:false ~relocate:false 
       LabelHere 
       LabelHere 
       spec.jc_fun_requires) 
  in
  let free_requires = 
    (named_assertion 
       ~global_assertion:false ~relocate:false
       LabelHere 
       LabelHere 
       spec.jc_fun_free_requires)
  in
  let requires = make_and requires_param free_requires in
  let requires_param = 
    if !Jc_options.annotation_sem = AnnotNone || Jc_options.trust_ai then 
      requires_param 
    else requires 
  in
  let requires =
    List.fold_left 
      (fun acc vi ->
         make_and
           (match vi.jc_var_info_type with
              | JCTpointer (tov, n1o, n2o) ->
                  let vit, lets = 
                    term ~global_assertion:false ~relocate:false
		      LabelHere LabelHere
                      (new term_var vi) 
                  in
                  begin match n1o, n2o with
                    | None, _ | _, None -> LTrue
                    | Some n1, Some n2 ->
                        let pred =
                          make_valid_pred_app tov
                            vit
                            (const_of_num n1)
                            (const_of_num n2)
                        in
                        make_pred_binds lets pred
                  end
              | JCTnative _ | JCTlogic _ | JCTenum _ | JCTnull | JCTany
              | JCTtype_var _ -> LTrue)
           acc)
      requires
      f.jc_fun_info_parameters
  in
  (* partition behaviors as follows:
     - (optional) 'safety' behavior (if Arguments Invariant Policy is selected)
     - (optional) 'inferred' behaviors (computed by analysis)
     - user defined behaviors *)
  let (safety_behavior,
       normal_behaviors_inferred, normal_behaviors, 
       excep_behaviors_inferred, excep_behaviors) =
    List.fold_left
      (fun (safety, normal_inferred, normal, excep_inferred, excep) (loc, id, b) ->
         let post =
           match b.jc_behavior_assigns with
             | None ->
                 (*
                   eprintf "lab,loc for ensures: \"%s\", %a@."
                   b.jc_behavior_ensures#name_label
                   Loc.gen_report_position b.jc_behavior_ensures#loc;
                 *)
                 (named_assertion ~global_assertion:false ~relocate:false LabelPost LabelOld 
                    b.jc_behavior_ensures)              
             | Some (locassigns, a) ->
                 named_jc_assertion loc
                   (make_and
                      (named_assertion ~global_assertion:false ~relocate:false LabelPost LabelOld
                         b.jc_behavior_ensures)         
                      (named_jc_assertion
                         locassigns
                         (assigns LabelOld f.jc_fun_info_effects (Some a) funloc)))
         in
(*          let a = *)
(*            match b.jc_behavior_assumes with *)
(*              | None -> post *)
(*              | Some e ->  *)
(*                  make_impl (assertion ~global_assertion:false LabelOld LabelOld e) post *)
(*          in *)
           match b.jc_behavior_throws with
             | None -> 
                 begin match id with
                   | "safety" ->
		       assert (b.jc_behavior_assumes = None);
                       ((id, b, post) :: safety, 
                        normal_inferred, normal, excep_inferred, excep)
                   | "inferred" -> 
		       assert (b.jc_behavior_assumes = None);
                       (safety,
                        (id, b, post) :: normal_inferred, 
                        (if Jc_options.trust_ai then normal else (id, b, post) :: normal), 
                        excep_inferred, excep)
                   | _ -> 
                       (safety, 
                        normal_inferred, (id, b, post) :: normal, 
                        excep_inferred, excep)
                 end
             | Some ei ->
                 let eb =
                   try
                     ExceptionMap.find ei excep
                   with Not_found -> []
                 in
                   if id = "inferred" then 
		     begin
		       assert (b.jc_behavior_assumes = None);
                       (safety, normal_inferred, normal, 
			ExceptionMap.add ei ((id, b, post) :: eb) excep_inferred, 
			if Jc_options.trust_ai then excep else 
                          ExceptionMap.add ei ((id, b, post) :: eb) excep)
		     end
                   else
                     (safety, normal_inferred, normal, excep_inferred, 
                      ExceptionMap.add ei ((id, b, post) :: eb) excep))
      ([], [], [], ExceptionMap.empty, ExceptionMap.empty)
      spec.jc_fun_behavior
  in
  let user_excep_behaviors = excep_behaviors in
  let excep_behaviors = 
    ExceptionSet.fold
      (fun ei acc -> 
         if ExceptionMap.mem ei acc then acc else
           let b = 
             { default_behavior with 
                 jc_behavior_throws = Some ei; jc_behavior_ensures = (new assertion JCAtrue); } 
           in
             ExceptionMap.add ei [ei.jc_exception_info_name ^ "_b", b, LTrue] acc)
      f.jc_fun_info_effects.jc_raises
      excep_behaviors
  in
  let reads =
    FieldOrVariantRegionMap.fold
      (fun (fi,r) labels acc -> 
        let mem = field_or_variant_region_memory_name(fi,r) in
        if Region.polymorphic r then
          if RegionList.mem r f.jc_fun_info_param_regions then
            if FieldOrVariantRegionMap.mem (fi,r) 
              f.jc_fun_info_effects.jc_writes.jc_effect_memories 
            then mem::acc 
            else acc
          else acc
        else mem::acc)
      f.jc_fun_info_effects.jc_reads.jc_effect_memories
      []
  in
  let reads =
    VarSet.fold
      (fun v acc -> v.jc_var_info_final_name::acc)
      f.jc_fun_info_effects.jc_reads.jc_effect_globals
      reads
  in
  let reads =
    StringRegionSet.fold
      (fun (a,r) acc -> 
        let alloc = alloc_region_table_name2(a,r) in
        if Region.polymorphic r then
          if RegionList.mem r f.jc_fun_info_param_regions then
            if StringRegionSet.mem (a,r) 
              f.jc_fun_info_effects.jc_writes.jc_effect_alloc_table 
            then alloc::acc 
            else acc
          else acc
        else alloc::acc)
      f.jc_fun_info_effects.jc_reads.jc_effect_alloc_table
      reads
  in
  let reads =
    VariantMap.fold
      (fun v labels acc -> (tag_table_name_vi v)::acc)
      f.jc_fun_info_effects.jc_reads.jc_effect_tag_table
      reads
  in
  let reads =
    StringSet.fold
      (fun v acc -> (mutable_name2 v)::acc)
      f.jc_fun_info_effects.jc_reads.jc_effect_mutable
      reads
  in
  let reads =
    StringSet.fold
      (fun v acc -> (committed_name2 v)::acc)
      f.jc_fun_info_effects.jc_reads.jc_effect_committed
      reads
  in
  let writes =
    FieldOrVariantRegionMap.fold
      (fun (fi,r) labels acc ->
        let mem = field_or_variant_region_memory_name(fi,r) in
        if Region.polymorphic r then
          if RegionList.mem r f.jc_fun_info_param_regions then mem::acc else acc
        else mem::acc)
      f.jc_fun_info_effects.jc_writes.jc_effect_memories
      []
  in
  let writes =
    VarSet.fold
      (fun v acc -> v.jc_var_info_final_name::acc)
      f.jc_fun_info_effects.jc_writes.jc_effect_globals
      writes
  in
  let writes =
    StringRegionSet.fold
      (fun (a,r) acc ->
        let alloc = alloc_region_table_name2(a,r) in
        if Region.polymorphic r then
          if RegionList.mem r f.jc_fun_info_param_regions then alloc::acc else acc
        else alloc::acc)
      f.jc_fun_info_effects.jc_writes.jc_effect_alloc_table
      writes
  in
  let writes =
    VariantMap.fold
      (fun v labels acc -> (tag_table_name_vi v)::acc)
      f.jc_fun_info_effects.jc_writes.jc_effect_tag_table
      writes
  in
  let writes =
    StringSet.fold
      (fun v acc -> (mutable_name2 v)::acc)
      f.jc_fun_info_effects.jc_writes.jc_effect_mutable
      writes
  in
  let writes =
    StringSet.fold
      (fun v acc -> (committed_name2 v)::acc)
      f.jc_fun_info_effects.jc_writes.jc_effect_committed
      writes
  in
  let param_write_mems,local_write_mems =
    FieldOrVariantRegionMap.fold
      (fun (fi,r) labels (param_acc,local_acc) ->
        if Region.polymorphic r then
          let mem = field_or_variant_region_memory_name(fi,r),
            field_or_variant_memory_type fi in
          if RegionList.mem r f.jc_fun_info_param_regions then
            mem::param_acc,local_acc
          else
            param_acc,mem::local_acc
        else param_acc,local_acc)
      f.jc_fun_info_effects.jc_writes.jc_effect_memories
      ([],[])
  in
  let param_read_mems,local_read_mems =
    FieldOrVariantRegionMap.fold
      (fun (fi,r) labels ((param_acc,local_acc) as acc) ->
         if FieldOrVariantRegionMap.mem (fi,r) 
           f.jc_fun_info_effects.jc_writes.jc_effect_memories
         then acc else
           if Region.polymorphic r then
          let mem = field_or_variant_region_memory_name(fi,r),
            field_or_variant_memory_type fi in
          if RegionList.mem r f.jc_fun_info_param_regions then
            mem::param_acc,local_acc
          else
            param_acc,mem::local_acc
        else param_acc,local_acc)
      f.jc_fun_info_effects.jc_reads.jc_effect_memories
      ([],[])
  in
  let param_write_allocs,local_write_allocs =
    StringRegionSet.fold
      (fun (a,r) (param_acc,local_acc) ->
        if Region.polymorphic r then
          let alloc = alloc_region_table_name2(a,r),alloc_table_type2 a in
          if RegionList.mem r f.jc_fun_info_param_regions then
            alloc::param_acc,local_acc
          else
            param_acc,alloc::local_acc
        else param_acc,local_acc)
      f.jc_fun_info_effects.jc_writes.jc_effect_alloc_table
      ([],[])
  in
  let param_read_allocs,local_read_allocs =
    StringRegionSet.fold
      (fun (a,r) (param_acc,local_acc) ->
	 
          let alloc = alloc_region_table_name2(a,r),alloc_table_type2 a in
        if Region.polymorphic r then
          if RegionList.mem r f.jc_fun_info_param_regions then
            alloc::param_acc,local_acc
          else
            param_acc,alloc::local_acc
        else
	  param_acc,local_acc)
      (StringRegionSet.diff 
        f.jc_fun_info_effects.jc_reads.jc_effect_alloc_table
        f.jc_fun_info_effects.jc_writes.jc_effect_alloc_table)
      ([],[])
  in
  let safety_post =
    List.fold_right
      (fun (_, _, e) acc -> make_and e acc)
      safety_behavior LTrue
  in
  let normal_post =
    List.fold_right
      (fun (_id,behav,post) acc -> 
	 let post = assume_in_postcondition behav post in
	 make_and post acc
      ) normal_behaviors LTrue
  in
  let normal_post_inferred =
    List.fold_right
      (fun (_,_,e) acc -> make_and e acc)
      normal_behaviors_inferred LTrue
  in
  let excep_posts =
    ExceptionMap.fold
      (fun ei l acc ->
         let p = 
           List.fold_right (fun (_,_,e) acc -> make_and e acc) l LTrue
         in (exception_name ei,p)::acc) 
      excep_behaviors []
  in
  let excep_posts_inferred =
    ExceptionMap.fold
      (fun ei l acc ->
         let p = 
           List.fold_right (fun (_id,behav,post) acc -> 
			      let post = assume_in_postcondition behav post in
			      make_and post acc
			   ) l LTrue
         in (exception_name ei,p)::acc) 
      excep_behaviors_inferred []
  in
    (* DEBUG *)
    (* Jc_options.lprintf "DEBUG: tr_fun 2@."; *)
    (* why parameter for calling the function *)
  let ret_type = tr_type f.jc_fun_info_result.jc_var_info_type in
  let param_normal_post = 
    if is_purely_exceptional_fun spec then LFalse else
      make_and_list [safety_post; normal_post; normal_post_inferred] 
  in
  let param_excep_posts = excep_posts @ excep_posts_inferred in
  let why_param = 
    let annot_type =
      Annot_type(requires_param, ret_type,
                 reads,writes, param_normal_post, param_excep_posts)
    in
    let fun_type = 
      interp_fun_params f 
        (param_write_allocs @ param_write_mems) 
        (param_read_allocs @ param_read_mems) 
        annot_type 
    in
      Param (false, f.jc_fun_info_final_name, fun_type)
  in
  match body with
    | None -> 
        (* function was only declared *)
        why_param :: acc
    | Some body ->
        if Jc_options.verify <> [] && 
          not (List.mem f.Jc_fenv.jc_fun_info_name Jc_options.verify) 
        then 
          why_param :: acc 
        else
          (* why functions for each behaviors *)
            let write_mems = 
              List.map (fun (name,ty) -> name,Ref_type(Base_type ty)) 
                (param_write_mems @ param_write_allocs)
            in
            let read_mems = 
              List.map (fun (name,ty) -> name,Base_type ty) 
                (param_read_mems @ param_read_allocs)
            in
            let params = match f.jc_fun_info_parameters with
              | [] -> ["tt", unit_type]
              | l -> List.map parameter l
            in
            let params = params @ write_mems @ read_mems in
              (* rename formals just before body is treated *)
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
              return_void := 
                (match f.jc_fun_info_result.jc_var_info_type with
                   | JCTnative Tunit -> true
                   | _ -> false);               
              printf "Generating Why function %s@."
                f.Jc_fenv.jc_fun_info_final_name;
              (* default behavior *)
              let body_safety = function_body f spec "safety" body in
              let tblock =
                if !Jc_options.inv_sem = InvOwnership then
                  append
                    (* (make_assume_all_assocs (fresh_program_point ()) 
                       f.jc_fun_info_parameters)*)
                    (assume_all_invariants f.jc_fun_info_parameters)
                    body_safety
                else
                  body_safety
              in
              let tblock =
                List.fold_left (fun acc (mem_name,_) ->
                  Let(mem_name,App(Var "any_memory",Void),acc)
                ) tblock local_read_mems
              in
              let tblock =
                List.fold_left (fun acc (mem_name,_) ->
                  Let_ref(mem_name,App(Var "any_memory",Void),acc)
                ) tblock local_write_mems
              in
              let tblock =
                List.fold_left (fun acc (alloc_name,_) ->
                  Let(alloc_name,App(Var "any_alloc_table",Void),acc)
                ) tblock local_read_allocs
              in
              let tblock =
                List.fold_left (fun acc (alloc_name,_) ->
                  Let_ref(alloc_name,App(Var "any_alloc_table",Void),acc)
                ) tblock local_write_allocs
              in
              let tblock = 
                if !return_void then
                  Try(append tblock (Raise(jessie_return_exception,None)),
                     jessie_return_exception,None,Void)
                else
                  let e = any_value f.jc_fun_info_result.jc_var_info_type in
                    Let_ref(jessie_return_variable,e,
                            Try(append tblock Absurd,
                                jessie_return_exception,None,
                                Deref(jessie_return_variable)))
              in
              let tblock = make_label "init" tblock in
              let tblock_safety =
                List.fold_right
                  (fun (mut_id,id) bl ->
                     Let_ref(mut_id,Var(id),bl)) list_of_refs tblock 
              in
              let newid = f.jc_fun_info_name ^ "_safety" in
              let _ = reg_loc 
                ~id:newid
                ~oldid:f.jc_fun_info_name
                ~name:("function " ^ f.jc_fun_info_name)
                ~beh:"Safety" funloc 
              in
              let acc = 
                if is_purely_exceptional_fun spec then acc else
                  if Jc_options.verify_invariants_only then acc else
                  Def(
                    newid,
                    Fun(
                      params,
                      requires,
                      tblock_safety,
                      safety_post,
                      excep_posts_for_others None excep_behaviors
                    ))::acc
              in
                (* user behaviors *)
              let acc = 
                if spec.jc_fun_behavior = [] then
                  acc
                else
                    (* normal behaviors *)
                  let acc =
                    List.fold_right
                      (fun (id,b,post) acc ->
 		          let body = function_body f spec id body in
                          let tblock =
                            if !Jc_options.inv_sem = InvOwnership then
                              append
                                (*      (make_assume_all_assocs (fresh_program_point ()) f.jc_fun_info_parameters)*)
                                (assume_all_invariants f.jc_fun_info_parameters)
                                body
                            else
                              body
                          in
                          let tblock =
                            List.fold_left (fun acc (mem_name,_) ->
                              Let(mem_name,App(Var "any_memory",Void),acc)
                            ) tblock local_read_mems
                          in
                          let tblock =
                            List.fold_left (fun acc (mem_name,_) ->
                              Let_ref(mem_name,App(Var "any_memory",Void),acc)
                            ) tblock local_write_mems
                          in
                          let tblock =
                            List.fold_left (fun acc (alloc_name,_) ->
                              Let(alloc_name,App(Var "any_alloc_table",Void),acc)
                            ) tblock local_read_allocs
                          in
                          let tblock =
                            List.fold_left (fun acc (alloc_name,_) ->
                              Let_ref(alloc_name,App(Var "any_alloc_table",Void),acc)
                            ) tblock local_write_allocs
                          in
                          let tblock = 
                            if !return_void then
                              Try(append tblock (Raise(jessie_return_exception,None)),
                                  jessie_return_exception,None,Void)
                            else
                              let e = any_value f.jc_fun_info_result.jc_var_info_type in
                                Let_ref(jessie_return_variable,e,
                                        Try(append tblock Absurd,
                                            jessie_return_exception,None,
                                            Deref(jessie_return_variable)))
                          in
                          let tblock = make_label "init" tblock in
                          let tblock =
                            List.fold_right
                              (fun (mut_id,id) bl ->
                                 Let_ref(mut_id,Var(id),bl)) list_of_refs tblock 
                          in

                         let newid = f.jc_fun_info_name ^ "_ensures_" ^ id in
                         let beh = 
                           if id="default" then "Behavior" else
                             "Normal behavior `"^id^"'"
                         in
                         let _ = reg_loc 
                           ~id:newid
                           ~oldid:f.jc_fun_info_name
                           ~name:("function "^f.jc_fun_info_name)
                           ~beh  
                           funloc 
                         in
			 let pre = assume_in_precondition b requires in
                         let d =
                           Def(
                             newid,
                             Fun(
                               params,
                               pre,
                               tblock,
                               post,
                               excep_posts_for_others None excep_behaviors
                             )
                           )
                         in d::acc)
                      normal_behaviors acc
                  in 
                    (* redefine [tblock] for use in exception functions *)
                    (* CLAUDE: pourquoi ??????
                       let tblock = make_label "init" tblock in
                       let tblock =
                       List.fold_right
                       (fun (mut_id,id) bl ->
                       Let_ref(mut_id,Var(id),bl)) list_of_refs tblock 
                       in
                    *)
                    (* exceptional behaviors *)
                  let acc =
                    ExceptionMap.fold
                      (fun ei l acc ->
                         List.fold_right
                           (fun (id,b,e) acc ->
                              let newid = 
                                f.jc_fun_info_name ^ "_exsures_" ^ id 
                              in
                              let _ = reg_loc 
                                ~id:newid
                                ~oldid:f.jc_fun_info_name
                                ~name:("function "^f.jc_fun_info_name)
                                ~beh:("Exceptional behavior `"^id^"'")  
                                funloc in
			      let pre = assume_in_precondition b requires in
                              let d =
                                Def(newid,
                                    Fun(params,
                                        pre,
                                        tblock,
                                        LTrue,
                                        (exception_name ei,e) :: 
                                          excep_posts_for_others (Some ei) excep_behaviors)) in
                                d::acc)
                           l acc)
                      user_excep_behaviors acc
                  in
                    acc 
        in why_param::acc

(* Store current function in global for appropriate translation of memories,
 * depending on effects in current function.
 *)
let tr_fun f funloc spec body acc =
  set_current_function f;
  let acc = tr_fun f funloc spec body acc in
  reset_current_function ();
  acc

let tr_logic_type id acc = Type(id,[])::acc

let tr_axiom id is_axiom p acc = 
  let ef = Jc_effect.assertion empty_effects p in
  let a = assertion ~global_assertion:true ~relocate:false LabelHere LabelHere p in
  let a =
    FieldOrVariantRegionMap.fold 
      (fun (fi,r) labels a -> 
         LogicLabelSet.fold
           (fun lab a ->
              LForall (label_var lab (field_or_variant_region_memory_name(fi,r)), 
                       field_or_variant_memory_type fi, a))
           labels a)
      ef.jc_effect_memories a 
  in
  let a =
    StringRegionSet.fold (fun (alloc,r) a -> 
      LForall (alloc_region_table_name2(alloc,r), alloc_table_type2 alloc, a)
    ) ef.jc_effect_alloc_table a 
  in
  let a =
    VariantMap.fold
      (fun vi labels a ->
         LogicLabelSet.fold
           (fun lab a ->
              LForall(label_var lab (tag_table_name_vi vi),
		      tag_table_type (JCvariant vi), a))
           labels a)
      ef.jc_effect_tag_table a
  in
  (* How to add quantification on other effects (alloc) without knowing 
   * their type ? *)
  if is_axiom then Axiom(id,a)::acc else Goal(id,a)::Axiom(id ^ "_as_axiom",a)::acc

let tr_exception ei acc =
  Jc_options.lprintf "producing exception '%s'@." ei.jc_exception_info_name;
  let typ = match ei.jc_exception_info_type with
    | Some tei -> Some (tr_base_type tei)
    | None -> None
  in
  Exception(exception_name ei, typ) :: acc

let range_of_enum ri = 
  Num.add_num (Num.sub_num ri.jc_enum_info_max ri.jc_enum_info_min) (Num.Int 1)

let tr_enum_type ri (* to_int of_int *) acc =
  let name = ri.jc_enum_info_name in
  let min = Num.string_of_num ri.jc_enum_info_min in
  let max = Num.string_of_num ri.jc_enum_info_max in
  let max_sub_min = Num.string_of_num (range_of_enum ri) in
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
        let width = LConst (Prim_int max_sub_min) in
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
    let t = Ref_type(tr_type vi.jc_var_info_type) in
      Param(false,vi.jc_var_info_final_name,t)::acc
  else
    let t = tr_base_type vi.jc_var_info_type in
      Logic(false,vi.jc_var_info_final_name,[],t)::acc

let tr_region r acc =
  Type(r.jc_reg_final_name,[]) :: acc

let tr_memory (fvi,r) acc =
  Param(
    false,field_or_variant_region_memory_name(fvi,r),
    Ref_type(Base_type(field_or_variant_memory_type fvi))) :: acc

let tr_alloc_table (tov,r) acc =
  Param(
    false,alloc_region_table_name(tov,r),
    Ref_type(Base_type(alloc_table_type tov))) :: acc

let tr_alloc_table2 (a,r) acc =
  Jc_options.lprintf "Looking for %s in Jc_interp.tr_alloc_table2@." a;
  tr_alloc_table (find_tag_or_variant a, r) acc

(*******************************************************************************)
(*                                   Variants                                  *)
(*******************************************************************************)

let tr_variant vi acc =
  let acc =
    if not vi.jc_variant_info_is_union then acc else
      (* Declaration of abstract type for union if not integer *)
      let acc = 
        if integral_union vi then acc else
          Type(union_memory_type_name vi,[]) :: acc 
      in
      (* Declarations of field memories. *)
      if !Jc_options.separation_sem = SepRegions then acc else
        let mem = union_memory_type vi in
        Param(false,
              union_memory_name vi,
              Ref_type(Base_type mem))::acc
  in
  let tag_table =
    Param(
      false,
      variant_tag_table_name vi,
      Ref_type(
        Base_type {
          logic_type_name = tag_table_type_name;
          logic_type_args = [variant_model_type vi];
        }))
  in
  let alloc_table =
    Param(
      false,
      variant_alloc_table_name vi,
      Ref_type(
        Base_type {
          logic_type_name = alloc_table_type_name;
          logic_type_args = [variant_model_type vi];
        }))
  in
  let type_def = Type(variant_type_name vi, []) in
  (* Axiom: the variant can only have the given tags *)
  let axiom_variant_has_tag =
    let v = "x" in
    let tag_table = tag_table_name_vi vi in
    Axiom(
      variant_axiom_on_tags_name vi,
      LForall(
        v,
        pointer_type (JCvariant vi),
        LForall(
          tag_table,
          tag_table_type (JCvariant vi),
          make_or_list
            (List.map
               (make_instanceof (LVar tag_table) (LVar v))
               vi.jc_variant_info_roots)
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
    vi.jc_variant_info_roots
  in
  let acc = type_def::alloc_table::tag_table::axiom_variant_has_tag::acc in
  (make_valid_pred (JCvariant vi)) :: acc

(*
  Local Variables: 
  compile-command: "unset LANG; make -j -C .. bin/jessie.byte"
  End: 
*)

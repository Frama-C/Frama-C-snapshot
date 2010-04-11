(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

(** Cil extensions for Frama-C *)

open Cil_types
open Cilutil
open Cil

let debug = false

let init_builtins () =
  let add t1 t2 =
    Logic_builtin.add
      { bl_name = "\\pointer_comparable";
        bl_profile = [("p1",  t1); ("p2", t2)];
        bl_type = None;
        bl_params = [];
        bl_labels = [];
        (* TODO: give an explicit definition *)
      }
  in
  let object_ptr = Ctype Cil.voidPtrType in
  let fun_ptr = Ctype (TPtr(TFun(Cil.voidType,None,false,[]),[])) in
  add object_ptr object_ptr;
  add fun_ptr fun_ptr;
  add object_ptr fun_ptr;
  add fun_ptr object_ptr

let () = Logic_env.Builtins.extend init_builtins

let warn fmt = Cil.warn fmt
let warn_once fmt =
  let b = Buffer.create 80 in
  let bfmt = Format.formatter_of_buffer b in
  Format.kfprintf
    (function fmt ->
       Format.pp_print_flush fmt ();
       let fmt = Buffer.contents b in
       Cil.warn ~once:true "%s" fmt
    ) bfmt fmt
    (* Cil.warn ~once:true fmt *)

let log_once fmt =
  let b = Buffer.create 80 in
  let bfmt = Format.formatter_of_buffer b in
  Format.kfprintf
    (function fmt ->
       Format.pp_print_flush fmt ();
       let fmt = Buffer.contents b in
       Cil.log ~once:true "%s" fmt
    ) bfmt fmt
    (* Cil.log ~once:true fmt *)

let labels_table l =
  let lbl_tbl = Hashtbl.create 17 in
  let goto_changer = object
  inherit nopCilVisitor
  method vstmt s =
    List.iter (function
                 | Label (l,_,_) -> Hashtbl.add lbl_tbl l s
                 |  _ -> ())
      s.labels;
    DoChildren
  end
  in
  List.iter (fun x -> ignore (visitCilStmt goto_changer x)) l;
  lbl_tbl

let update_gotos sid_tbl block =
  let goto_changer = object
  inherit nopCilVisitor
  method vstmt s = match s.skind with
  | Goto(sref,loc) ->
      (try
      let new_stmt = Cilutil.StmtMap.find !sref sid_tbl in
      ChangeTo (mkStmt (Goto (ref new_stmt,loc)))
      with Not_found -> DoChildren)
  | _ -> DoChildren
  end
  in
  visitCilBlock goto_changer block

let compact_body fundec =
  let labels_moved = ref false in
  let stmt_map = ref Cilutil.StmtMap.empty in
  let add_labels s l old_stmt =
    stmt_map := Cilutil.StmtMap.add old_stmt s !stmt_map ;
    s.labels <- l @ s.labels;
    labels_moved := true
  in
  let docompact(s1,s2) = match s1.skind, s2.skind with
      (* remove skip *)
    | Instr (Skip _), _ when s1.labels = [] ->
        if debug then prerr_endline "[compact_body] skip/_";
        Some [s2]
    | _, Instr (Skip _) when s2.labels = [] ->
        if debug then prerr_endline "[compact_body] _/skip";
        Some [s1]
      (* collapse useless blocks *)
    | Block b1, Block b2 when b1.battrs = b2.battrs && (s2.labels = [] || b2.bstmts <> []) ->
        if debug then prerr_endline "[compact_body] blck/blck";
        if s2.labels <> [] then begin
          let fst_b2 = List.hd b2.bstmts in
          add_labels fst_b2 s2.labels s2
        end;
        b1.bstmts <- b1.bstmts@b2.bstmts;
        Some [s1]
    | Block b, _ when b.battrs = [] ->
        if debug then prerr_endline "[compact_body] blck/instr";
        b.bstmts <- b.bstmts@[s2];
        Some [s1]
    | _, Block b when b.battrs = [] && (s2.labels = [] || b.bstmts <> []) ->
        if debug then prerr_endline "[compact_body] instr/blck";
        if s2.labels <> [] then begin
          let fst_b = List.hd b.bstmts in
          add_labels fst_b s2.labels s2
        end;
        b.bstmts <- s1::b.bstmts;
        Some [s2]
    | _ -> None
  in
  let block = fundec.sbody in
    (* Je pense que peepHole2 ne fonctionne pas correctement.
    * Peut-être serait-il mieux de faire notre propre fonction...
    * en plus, ça permettrait de combiner peepHole1 + peepHole2.
    * Exemple : peepHole2 ( { { x = 1; skip; } return x; } )
    *         -> { { x = 1; skip; return x; } }
    * Anne. 12/07/2007. *)
  block.bstmts <- peepHole2 ~agressive:true docompact block.bstmts;
  let block =
    if !labels_moved then update_gotos !stmt_map block
    else block
  in let rec get_stmts b = match b.bstmts with
    | ({skind=Block blk}) :: [] ->
        if debug then prerr_endline "[compact_body] block(block)";
        get_stmts blk
    | _ -> b.bstmts
  in fundec.sbody.bstmts <- get_stmts block


let current_stmt_tbl =
  let s = Stack.create () in
  Stack.push Kglobal s;
  s

let start_stmt ki =
  Stack.push ki current_stmt_tbl

let end_stmt () =
  try
    ignore (Stack.pop current_stmt_tbl)
  with Stack.Empty -> assert false

let current_stmt () =
  try
    Stack.top current_stmt_tbl
  with Stack.Empty -> assert false


type syntactic_context =
  | SyNone
  | SyBinOp of Cil_types.binop * Cil_types.exp * Cil_types.exp
  | SyUnOp of  Cil_types.exp
  | SyMem of  Cil_types.lval
  | SySep of Cil_types.exp * Cil_types.exp

let syntactic_context = ref SyNone
let set_syntactic_context e =
 (* (match e with
  | SyBinOp (e1,e2) ->
      ignore
        (Cil.warn
           "New binary context: %a %a\n"
           Cil.d_exp e1
           Cil.d_exp e2)
  | SyUnOp e ->
      ignore
        (Cil.warn
           "New unary context: %a\n"
           Cil.d_exp e)
  | SyMem e ->
      ignore
        (Cil.warn
           "New mem context: %a\n"
           Cil.d_lval e)
  | SyNone ->       ignore
        (Cil.warn
           "New null context\n"));*)
  syntactic_context := e

let get_syntactic_context () = current_stmt (),!syntactic_context

let value_analysis_alarm_status () =
  {status = Checked{emitter="value analysis"; valid = Maybe}}

type alarm_behavior = Aignore | Alog | Acall of (unit -> unit)
type warn_mode = {unspecified:alarm_behavior; others: alarm_behavior;
                  imprecision_tracing:alarm_behavior}

let warn_all_mode =
  {unspecified=Alog; others=Alog; imprecision_tracing=Alog}
let warn_none_mode =
  {unspecified=Aignore; others=Aignore; imprecision_tracing=Aignore}

let warn_div warn_mode =
  match warn_mode.others with
    Aignore -> ()
  | Acall f -> f()
  | Alog ->
      begin
	match get_syntactic_context () with
	| _,SyNone -> ()
	| _,(SyUnOp _ | SyMem _ | SySep _) -> assert false
	| ki,SyBinOp ((Div|Mod),_,exp_d) ->
	    let lexpr = Logic_utils.expr_to_term ~cast:true exp_d in
	    let annotation =
              Logic_const.new_code_annotation
		(AAssert ([],
			  Logic_const.unamed (Prel (Rneq,lexpr, lzero())),
                          value_analysis_alarm_status ()))
            in
	    if Alarms.register ki Alarms.Division_alarm annotation then
              warn "division by zero: %a" !Ast_printer.d_code_annotation annotation
	|_,SyBinOp (_,_,_) -> assert false
      end

let warn_signed_overflow warn_mode e mn mx =
  match warn_mode.others with
    Aignore -> ()
  | Acall f -> f()
  | Alog ->
      ( match get_syntactic_context () with
	ki, _ ->
	  begin
	    let exp_l = 
	      match e.enode with
	      | BinOp (op, l, r, _) ->
		  let l_l = Logic_utils.expr_to_term ~cast:true l in
		  let r_l = Logic_utils.expr_to_term ~cast:true r in
		  TBinOp (op, l_l, r_l)
	      | UnOp (op, ie, _) ->
		  let ie_l = Logic_utils.expr_to_term ~cast:true ie in
		  TUnOp (op, ie_l)
	      | _ -> assert false
	    in
	    let exp_l = 
	      Logic_const.term exp_l Linteger
	    in
            let lexpr = Logic_const.tinteger_s64 mn in
            let rexpr = Logic_const.tinteger_s64 mx in
            let p0 = Logic_const.prel(Rle, lexpr, exp_l) in
            let p1 = Logic_const.prel(Rle, exp_l, rexpr) in
            let p = Logic_const.pand (p0,p1) in
            let annotation =
              Logic_const.new_code_annotation 
		(AAssert ([],p,value_analysis_alarm_status ()))
            in
	    if Alarms.register ki Alarms.Signed_overflow_alarm annotation
	    then
              warn "signed overflow, should be between %Ld and %Ld: %a"
		mn mx
		!Ast_printer.d_code_annotation annotation
	  end)

let warn_shift warn_mode size =
  match warn_mode.others with
    Aignore -> ()
  | Acall f -> f()
  | Alog ->
      begin
    match get_syntactic_context () with
    | _,SyNone -> ()
    | _,(SyUnOp _ | SyMem _ | SySep _)-> assert false
    | ki,SyBinOp ((Shiftrt | Shiftlt),_,exp_d) ->
	let lexpr = Logic_utils.expr_to_term ~cast:true exp_d in
	let annotation =
          Logic_const.new_code_annotation
            (AAssert
		([],
		 Logic_const.pand
                    (Logic_const.unamed (Prel (Rge,lexpr, lzero())),
                    Logic_const.unamed
                      (Prel (Rlt,lexpr, lconstant (Int64.of_int size)))),
                 value_analysis_alarm_status ()))
	in
	if Alarms.register ki Alarms.Shift_alarm annotation then
          warn "invalid shift: %a" !Ast_printer.d_code_annotation annotation;
	()
    | _,SyBinOp(_,_,_) ->
	assert false
      end

let warn_mem warn_mode msg =
  match warn_mode.others with
    Aignore -> ()
  | Acall f -> f()
  | Alog ->
      begin

    match get_syntactic_context () with
    | _,SyNone -> ()
    | _,(SyBinOp _ | SyUnOp _ | SySep _) -> assert false
    | ki,SyMem lv_d ->
        let term = mkAddrOrStartOf lv_d in
        let lexpr = Logic_utils.expr_to_term ~cast:true term in
        let annotation =
          Logic_const.new_code_annotation
            (AAssert ([],
		      Logic_const.unamed (Pvalid lexpr),
                      value_analysis_alarm_status ()))
        in
        if Alarms.register ki Alarms.Memory_alarm annotation then
          warn "out of bounds %s. @[%a@]" msg !Ast_printer.d_code_annotation annotation;
        ( match lv_d with
          | Mem _,_ | _, (Index _ | Field _) -> ()
          | _ -> assert false)
      end

let warn_index warn_mode msg =
  match warn_mode.others with
    Aignore -> ()
  | Acall f -> f()
  | Alog ->
      begin
    match get_syntactic_context () with
    | _,SyNone -> ()
    | _,(SyMem _ | SyUnOp _ | SySep _) -> assert false
    | ki ,SyBinOp (IndexPI,e1,e2) ->
        let lexpr = Logic_utils.expr_to_term ~cast:true e1 in
        let rexpr = Logic_utils.expr_to_term ~cast:true e2 in
        let p0 = Logic_const.prel(Rle, lzero(), lexpr) in
        let p1 = Logic_const.prel(Rlt, lexpr, rexpr) in
        let p = Logic_const.pand (p0,p1) in
        let annotation =
          Logic_const.new_code_annotation (AAssert ([],p,value_analysis_alarm_status ()))
        in
        if Alarms.register ki Alarms.Index_alarm annotation then
          warn "%s out of bounds index. @[%a@]"
            msg !Ast_printer.d_code_annotation annotation
    | _,SyBinOp(_,_,_) ->
        assert false
      end

let warn_mem_read warn_mode = warn_mem warn_mode "read"
let warn_mem_write warn_mode = warn_mem warn_mode "write"

let comparable_pointers t1 t2 =
  let preds = Logic_env.find_all_logic_functions "\\pointer_comparable" in
  let cfct_ptr = TPtr (TFun(Cil.voidType,None,false,[]),[]) in
  let fct_ptr = Ctype cfct_ptr in
  let obj_ptr = Ctype Cil.voidPtrType in
  let discriminate t =
    match t.term_type with
      | Ctype ty ->
          (match Cil.unrollType ty with
               TPtr(TFun _,_) ->
                 Logic_const.term (TCastE(cfct_ptr,t)) fct_ptr, fct_ptr
             | TPtr _  -> t, obj_ptr
             | TInt _ when Cil.isLogicZero t -> t, obj_ptr
             | TVoid _ | TInt _ | TFloat _ | TFun _ | TNamed _
             | TComp _ | TEnum _ | TBuiltin_va_list _
             | TArray _ ->
                 Logic_const.term (TCastE(Cil.voidPtrType,t)) obj_ptr, obj_ptr)
      | _ -> Logic_const.term (TCastE(Cil.voidPtrType,t)) obj_ptr, obj_ptr

  in
  let t1, ty1 = discriminate t1 in
  let t2, ty2 = discriminate t2 in
  let pi =
    try
      List.find
        (function
             { l_profile = [v1; v2] } ->
               Logic_utils.is_same_type v1.lv_type ty1 &&
                 Logic_utils.is_same_type v2.lv_type ty2
           | _ -> false) preds
    with Not_found ->
      Cilmsg.fatal "built-in predicate \\pointer_comparable not found"
  in
  Papp (pi, [], [t1;t2])

let warn_pointer_comparison warn_mode =
  match warn_mode.others with
    Aignore -> ()
  | Acall f -> f()
  | Alog ->
      begin
  match get_syntactic_context () with
  | _,SyNone -> ()
  | _,(SyUnOp _ | SyMem _ | SySep _) -> assert false
  | ki,SyBinOp ((Eq|Ne|Ge|Le|Gt|Lt),exp_l,exp_r) ->
      let lexpr_l = Logic_utils.expr_to_term ~cast:true exp_l in
      let lexpr_r = Logic_utils.expr_to_term ~cast:true exp_r in
      let annotation =
        Logic_const.new_code_annotation
          (AAssert ([],
		    Logic_const.unamed (comparable_pointers lexpr_l lexpr_r),
                    value_analysis_alarm_status ())) in
      if Alarms.register ki Alarms.Pointer_compare_alarm annotation then
	warn "pointer comparison: %a" !Ast_printer.d_code_annotation annotation
  | _,SyBinOp(_,_,_) ->
      assert false
end

let result_nan_infinite op t1 t2 =
  (*TODO: this function does not return a correct predicate *)
  let pi =
    match Logic_env.find_all_logic_functions "\\is_finite" with
      | i :: _ -> i
      | [] -> assert false
  in
  let op = match op with
    Mult -> "*"
  | MinusA -> "-"
  | PlusA -> "+"
  | Div -> "/"
  | _ -> "OP"
  in
  let op = Logic_utils.expr_to_term ~cast:true (dummy_exp (Const (CStr op))) in
  Papp (pi, [], [op; t1; t2])

let warn_result_nan_infinite warn_mode =
  match warn_mode.others with
    Aignore -> ()
  | Acall f -> f()
  | Alog ->
      begin
  match get_syntactic_context () with
  | _,SyNone -> ()
  | _,(SyUnOp _ | SyMem _ | SySep _) -> assert false
  | ki,SyBinOp (op, exp_l, exp_r) ->
      let lexpr_l = Logic_utils.expr_to_term ~cast:true exp_l in
      let lexpr_r = Logic_utils.expr_to_term ~cast:true exp_r in
      let annotation =
        Logic_const.new_code_annotation
          (AAssert
	     ([],
             Logic_const.unamed (result_nan_infinite op lexpr_l lexpr_r),
              value_analysis_alarm_status ())) in
      if Alarms.register ki Alarms.Result_is_nan_or_infinite_alarm annotation
      then
        warn "float operation: %a" !Ast_printer. d_code_annotation annotation
end

let warn_uninitialized warn_mode =
  match warn_mode.unspecified with
    Aignore -> ()
  | Acall f -> f()
  | Alog ->
      begin
    match get_syntactic_context () with
    | _,SyNone -> ()
    | _,(SyBinOp _ | SyUnOp _ | SySep _) -> assert false
    | _,SyMem lv_d ->
        warn_once "accessing uninitialized left-value %a: assert(TODO)" d_lval lv_d
      end

let warn_escapingaddr warn_mode =
  match warn_mode.unspecified with
    Aignore -> ()
  | Acall f -> f()
  | Alog ->
      begin
    match get_syntactic_context () with
    | _,SyNone -> ()
    | _,(SyBinOp _ | SyUnOp _ | SySep _) -> assert false
    | _,SyMem lv_d ->
        warn_once "accessing left-value that contains escaping addresses %a: assert(TODO)" d_lval lv_d
      end

let warn_separated warn_mode =
  match warn_mode.unspecified with
      Aignore -> ()
    | Acall f -> f()
    | Alog ->
        begin
          match get_syntactic_context () with
            | _,SyNone -> ()
            | _,(SyBinOp _ | SyUnOp _ | SyMem _ ) -> assert false
            | ki,SySep(lv1,lv2) ->
                let llv1 = Logic_utils.expr_to_term ~cast:true lv1 in
                let llv2 = Logic_utils.expr_to_term ~cast:true lv2 in
                let alarm =
                  Logic_const.pseparated ~loc:(Instr.loc ki) [llv1; llv2]
                in let annotation =
                  Logic_const.new_code_annotation
                    (AAssert([],alarm,value_analysis_alarm_status ()))
                in
                if Alarms.register ki Alarms.Separation_alarm annotation then
                warn
                  "undefined multiple accesses in expression. \
                   assert \\separated(%a,%a);" d_exp lv1 d_exp lv2
        end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

(* ************************************************************************* *)
(* [JS 2011/03/11] All the below stuff manage warnings of the value analysis
   plug-in. Refactoring required. *)
(* ************************************************************************* *)

let current_stmt_tbl =
  let s = Stack.create () in
  Stack.push Kglobal s;
  s

let start_stmt ki = Stack.push ki current_stmt_tbl

let end_stmt () =
  try ignore (Stack.pop current_stmt_tbl)
  with Stack.Empty -> assert false

let current_stmt () =
  try Stack.top current_stmt_tbl
  with Stack.Empty -> assert false

type syntactic_context =
  | SyNone
  | SyBinOp of Cil_types.binop * Cil_types.exp * Cil_types.exp
  | SyUnOp of  Cil_types.exp
  | SyMem of  Cil_types.lval
  | SyMemLogic of Cil_types.term
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

let sc_kinstr_loc ki =
  match ki with
    | Kglobal -> (* can occur in case of obscure bugs (already happended)
                    with wacky initializers. Module Initial_state of
                    value analysis correctly positions the loc *)
        assert (Cil_datatype.Kinstr.equal Kglobal
                  (fst (get_syntactic_context ())));
        CurrentLoc.get ()
    | Kstmt s -> Cil_datatype.Stmt.loc s


type warn_origin = {
  warn_emitter: Emitter.t;
  warn_deps: State.t list;
}

let register_alarm ki alarm wo =
  Alarms.register ~deps:wo.warn_deps ki alarm wo.warn_emitter


type alarm_behavior = Aignore | Alog of warn_origin | Acall of (unit -> unit)
type warn_mode = {unspecified:alarm_behavior; others: alarm_behavior;
                  imprecision_tracing:alarm_behavior}

let warn_all_mode wo =
  {unspecified=Alog wo; others=Alog wo; imprecision_tracing=Alog wo}
let warn_none_mode =
  {unspecified=Aignore; others=Aignore; imprecision_tracing=Aignore}

let stop_if_stop_at_first_alarm_mode () =
  if (Kernel.StopAtFirstAlarm.get ())
  then exit 0 (* TODO: same mechanism as do_degenerate *)

let warn_div warn_mode =
  match warn_mode.others with
    Aignore -> ()
  | Acall f -> f()
  | Alog wo ->
      begin
        match get_syntactic_context () with
        | _,SyNone -> ()
        | _,(SyUnOp _ | SyMem _ | SyMemLogic _ | SySep _) -> assert false
        | ki,SyBinOp ((Div|Mod),_,exp_d) ->
            let loc = exp_d.eloc in
            let lexpr = Logic_utils.expr_to_term ~cast:true exp_d in
            let annotation =
              Logic_const.new_code_annotation
                (AAssert ([],
                          Logic_const.unamed ~loc (Prel (Rneq,lexpr, lzero()))))
            in
            if register_alarm ki (Division_alarm,annotation) wo then
              Kernel.warning ~current:true
                "@[division by zero:@ %a@]"
                !Ast_printer.d_code_annotation annotation;
	    stop_if_stop_at_first_alarm_mode ()
        |_,SyBinOp (_,_,_) -> assert false
      end

let warn_signed_overflow warn_mode mn mx =
  match warn_mode.others with
  | Aignore -> ()
  | Acall f -> f()
  | Alog wo ->
      let aux ki loc exp_l =
        (match mn with
           | Some mn ->
               let lexpr = Logic_const.tinteger_s64 mn in
               let p = Logic_const.prel ~loc (Rle, lexpr, exp_l) in
               let annotation =
                 Logic_const.new_code_annotation (AAssert ([],p))
               in
               if register_alarm ki (Signed_overflow_alarm,annotation)wo then
                 Kernel.warning ~current:true
                   "@[Signed overflow.@ %a@]"
                   !Ast_printer.d_code_annotation annotation;
	       stop_if_stop_at_first_alarm_mode ()
           | None -> ());
        ( match mx with
            | Some mx ->
                let rexpr = Logic_const.tinteger_s64 mx in
                let p = Logic_const.prel ~loc (Rle, exp_l, rexpr) in
                let annotation =
                  Logic_const.new_code_annotation (AAssert ([],p))
                in
                if register_alarm ki (Signed_overflow_alarm,annotation)wo then
                  Kernel.warning ~current:true
                    "@[Signed overflow.@ %a@]"
                    !Ast_printer.d_code_annotation annotation;
		stop_if_stop_at_first_alarm_mode ()
            | None -> ());
      in
      ( match get_syntactic_context () with
      | ki, SyUnOp e ->
          let te = Logic_utils.expr_to_term ~cast:false e in
          aux ki e.eloc te
      | ki, SyBinOp (op, l, r) ->
          let loc = l.eloc in
          let l_l = Logic_utils.expr_to_term ~cast:true l in
          let r_l = Logic_utils.expr_to_term ~cast:true r in
          let t = Logic_const.term ~loc (TBinOp (op, l_l, r_l)) Linteger in
          aux ki loc t
      | _ ->
          assert false
      )

let warn_shift warn_mode size =
  match warn_mode.others with
    Aignore -> ()
  | Acall f -> f()
  | Alog wo ->
      begin
    match get_syntactic_context () with
    | _,SyNone -> ()
    | _,(SyUnOp _ | SyMem _ | SyMemLogic _ | SySep _)-> assert false
    | ki,SyBinOp ((Shiftrt | Shiftlt),_,exp_d) ->
        let loc = exp_d.eloc in
        let lexpr = Logic_utils.expr_to_term ~cast:true exp_d in
        let annotation =
          Logic_const.new_code_annotation
            (AAssert
                ([],
                 Logic_const.pand ~loc
                    (Logic_const.unamed ~loc (Prel (Rge,lexpr, lzero())),
                    Logic_const.unamed ~loc
                      (Prel (Rlt,lexpr, lconstant (My_bigint.of_int size))))))
        in
        if register_alarm ki (Shift_alarm,annotation) wo then
          Kernel.warning ~current:true
            "@[invalid RHS operand for shift.@ %a@]" !Ast_printer.d_code_annotation annotation;
        stop_if_stop_at_first_alarm_mode ()
    | _,SyBinOp(_,_,_) ->
        assert false
      end

let warn_shift_left_positive warn_mode =
  match warn_mode.others with
    Aignore -> ()
  | Acall f -> f()
  | Alog wo ->
      begin
    match get_syntactic_context () with
    | _,SyNone -> ()
    | _,(SyUnOp _ | SyMem _ | SyMemLogic _ | SySep _)-> assert false
    | ki,SyBinOp ((Shiftrt | Shiftlt),exp_l,_) ->
        let loc = exp_l.eloc in
        let lexpr = Logic_utils.expr_to_term ~cast:true exp_l in
        let annotation =
          Logic_const.new_code_annotation
            (AAssert
                ([],
                Logic_const.unamed ~loc (Prel (Rge,lexpr, lzero()))))
        in
        if register_alarm ki (Shift_alarm,annotation) wo then
          Kernel.warning ~current:true
            "@[invalid LHS operand for left shift.@ %a@]" !Ast_printer.d_code_annotation annotation;
        stop_if_stop_at_first_alarm_mode ()
    | _,SyBinOp(_,_,_) ->
        assert false
      end

let warn_mem warn_mode msg =
  match warn_mode.others with
    Aignore -> ()
  | Acall f -> f()
  | Alog wo ->
      begin
        let warn_term ki loc term =
          let annotation =
            Logic_const.new_code_annotation
              (AAssert ([], Logic_const.unamed ~loc (Pvalid term)))
          in
          if register_alarm ki (Memory_alarm, annotation) wo then
            Kernel.warning ~current:true "@[out of bounds %s.@ %a@]" msg
              !Ast_printer.d_code_annotation annotation;
	  stop_if_stop_at_first_alarm_mode ()
        in
        match get_syntactic_context () with
          | _,SyNone -> ()
          | _,(SyBinOp _ | SyUnOp _ | SySep _) -> assert false
          | ki,SyMem lv_d ->
              let loc = sc_kinstr_loc ki in
              let exp = mkAddrOrStartOf ~loc lv_d in
              let term = Logic_utils.expr_to_term ~cast:true exp in
              warn_term ki loc term;
              (match lv_d with
                 | Mem _,_ | _, (Index _ | Field _) -> ()
                 | _ -> Format.printf "ERR 937: %a@." d_lval lv_d ; assert false)
          | ki,SyMemLogic term ->
              warn_term ki term.term_loc term
      end

let warn_index warn_mode msg index =
  match warn_mode.others with
    Aignore -> ()
  | Acall f -> f()
  | Alog wo ->
      begin
    match get_syntactic_context () with
    | _,SyNone -> ()
    | _,(SyMem _ | SyMemLogic _ | SyUnOp _ | SySep _) -> assert false
    | ki ,SyBinOp (IndexPI,e1,e2) ->
        let loc = e1.eloc in
        let lexpr = Logic_utils.expr_to_term ~cast:true e1 in
        let rexpr = Logic_utils.expr_to_term ~cast:true e2 in
        let p0 = Logic_const.prel ~loc:lexpr.term_loc (Rle, lzero(), lexpr) in
        let p1 = Logic_const.prel ~loc:rexpr.term_loc (Rlt, lexpr, rexpr) in
        let p = Logic_const.pand ~loc (p0,p1) in
        let annotation = Logic_const.new_code_annotation (AAssert ([],p)) in
        if register_alarm ki (Index_alarm,annotation) wo then
          Kernel.warning ~current:true "@[%s out of bounds index %s.@ %a@]"
            msg index !Ast_printer.d_code_annotation annotation;
	stop_if_stop_at_first_alarm_mode ()
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
    let loc = t.term_loc in
    match t.term_type with
      | Ctype ty ->
          (match Cil.unrollType ty with
             | TPtr(TFun _,_) ->
                 Logic_const.term ~loc (TCastE(cfct_ptr,t)) fct_ptr, fct_ptr
             | TPtr _  -> t, obj_ptr
             | TInt _ when Cil.isLogicZero t -> t, obj_ptr
             | TVoid _ | TInt _ | TFloat _ | TFun _ | TNamed _
             | TComp _ | TEnum _ | TBuiltin_va_list _
             | TArray _ ->
                 Logic_const.term ~loc (TCastE(voidPtrType,t)) obj_ptr, obj_ptr
          )
      | _ -> Logic_const.term ~loc (TCastE(voidPtrType,t)) obj_ptr, obj_ptr

  in
  let t1, ty1 = discriminate t1 in
  let t2, ty2 = discriminate t2 in
  let pi =
    try
      List.find
        (function
           | { l_profile = [v1; v2] } ->
               Logic_utils.is_same_type v1.lv_type ty1 &&
               Logic_utils.is_same_type v2.lv_type ty2
           | _ -> false) preds
    with Not_found ->
      Kernel.fatal "built-in predicate \\pointer_comparable not found"
  in
  Papp (pi, [], [t1;t2])

let warn_pointer_comparison warn_mode =
  match warn_mode.others with
    Aignore -> ()
  | Acall f -> f()
  | Alog wo ->
      begin
  match get_syntactic_context () with
  | _,SyNone -> ()
  | _,(SyUnOp _ | SyMem _ | SyMemLogic _ | SySep _) -> assert false
  | ki,SyBinOp ((Eq|Ne|Ge|Le|Gt|Lt),exp_l,exp_r) ->
      let loc = exp_l.eloc in
      let lexpr_l = Logic_utils.expr_to_term ~cast:true exp_l in
      let lexpr_r = Logic_utils.expr_to_term ~cast:true exp_r in
      let t = Logic_const.unamed ~loc (comparable_pointers lexpr_l lexpr_r) in
      let annotation = Logic_const.new_code_annotation (AAssert ([], t)) in
      if register_alarm ki (Pointer_compare_alarm,annotation) wo then
        Kernel.warning ~current:true
          "@[pointer comparison:@ %a@]"
          !Ast_printer.d_code_annotation annotation;
      stop_if_stop_at_first_alarm_mode ()
  | _,SyBinOp(_,_,_) ->
      assert false
end

let result_nan_infinite t =
  let pi =
    match Logic_env.find_all_logic_functions "\\is_finite" with
      | i :: _ -> i
      | [] -> assert false
  in
  let op = Logic_utils.expr_to_term ~cast:true t in
  Papp (pi, [], [op])

let warn_result_nan_infinite warn_mode =
  match warn_mode.others with
    Aignore -> ()
  | Acall f -> f()
  | Alog wo ->
      begin
  match get_syntactic_context () with
  | _,SyNone -> ()
  | _,(SyBinOp _ | SyMem _ | SyMemLogic _ | SySep _) -> assert false
  | ki,SyUnOp (exp_r) ->
      let loc = exp_r.eloc in
      let t = Logic_const.unamed ~loc (result_nan_infinite exp_r) in
      let annotation = Logic_const.new_code_annotation (AAssert ([], t)) in
      if register_alarm ki (Result_is_nan_or_infinite_alarm,annotation) wo then
        Kernel.warning ~current:true ~once:true
          "@[float operation:@ %a@]"
          !Ast_printer. d_code_annotation annotation;
      stop_if_stop_at_first_alarm_mode ()
end

let warn_uninitialized warn_mode =
  match warn_mode.unspecified with
  | Aignore -> ()
  | Acall f -> f()
  | Alog wo ->
    match get_syntactic_context () with
    | _,SyNone -> ()
    | _,(SyBinOp _ | SyUnOp _ | SySep _ | SyMemLogic _) -> assert false
    | ki,SyMem lv_d ->
      let loc = sc_kinstr_loc ki in
      let e = Cil.mkAddrOrStartOf ~loc lv_d in
      let term = Logic_utils.expr_to_term ~cast:false e in
      let annotation =
        Logic_const.new_code_annotation
          (AAssert ([], Logic_const.unamed ~loc (Pinitialized term)))
      in
      if register_alarm ki (Other_alarm, annotation) wo then
        Kernel.warning ~current:true
          "@[accessing uninitialized left-value:@ %a@]"
          !Ast_printer.d_code_annotation annotation;
      stop_if_stop_at_first_alarm_mode ()

let warn_escapingaddr warn_mode =
  match warn_mode.unspecified with
  | Aignore -> ()
  | Acall f -> f()
  | Alog _ ->
    match get_syntactic_context () with
    | _,SyNone -> ()
    | _,(SyBinOp _ | SyUnOp _ | SySep _ | SyMemLogic _) -> assert false
    | _,SyMem lv_d ->
	Kernel.warning ~once:true ~current:true
          "@[accessing left-value %a@ that contains escaping addresses;@ assert(Ook)@]"
          d_lval lv_d;
	stop_if_stop_at_first_alarm_mode ()

let warn_separated warn_mode =
  match warn_mode.unspecified with
      Aignore -> ()
    | Acall f -> f()
    | Alog wo ->
        begin
          match get_syntactic_context () with
            | _,SyNone -> ()
            | _,(SyBinOp _ | SyUnOp _ | SyMem _ | SyMemLogic _) -> assert false
            | ki,SySep(lv1,lv2) ->
                let loc = sc_kinstr_loc ki in
                let llv1 = Logic_utils.expr_to_term ~cast:true lv1 in
                let llv2 = Logic_utils.expr_to_term ~cast:true lv2 in
                let alarm = Logic_const.pseparated ~loc [ llv1; llv2 ] in
                let annotation =
                  Logic_const.new_code_annotation (AAssert([],alarm))
                in
                if register_alarm ki (Separation_alarm, annotation) wo then
                  Kernel.warning ~current:true
                    "@[undefined multiple accesses in expression.@ %a;@]"
                    !Ast_printer.d_code_annotation annotation;
		stop_if_stop_at_first_alarm_mode ()
        end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

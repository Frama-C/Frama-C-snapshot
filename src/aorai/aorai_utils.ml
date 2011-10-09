(**************************************************************************)
(*                                                                        *)
(*  This file is part of Aorai plug-in of Frama-C.                        *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
(*    INSA  (Institut National des Sciences Appliquees)                   *)
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

open Cil
open Logic_const
open Logic_utils
open Data_for_aorai
open Extlib
open Cil_types
open Cil_datatype
open Promelaast
open Bool3
open Spec_tools

let mkStmt stmt = Cil.mkStmt ~valid_sid:true stmt

let mkStmtOneInstr instr = Cil.mkStmtOneInstr ~valid_sid:true instr

(**exception to avoid pre computation with structure and array**)
exception LazyInit;;

let rename_pred v1 v2 p =
  let r =
  object
    inherit Visitor.frama_c_copy (Project.current())
    method vlogic_var_use v =
      if Cil_datatype.Logic_var.equal v v1 then Cil.ChangeTo v2 
      else Cil.JustCopy
  end
  in
  Visitor.visitFramacPredicateNamed r p
  
(** Given a transition a function name and a function status (call or
    return) it returns if the cross condition can be statisfied with
    only function status.
 *)
let isCrossable tr func st =
  let rec isCross p =
    match p with
      | TOr  (c1, c2) -> bool3or (isCross c1) (isCross c2)
      | TAnd (c1, c2) -> bool3and (isCross c1) (isCross c2)
      | TNot c1 -> bool3not (isCross c1)
      | TCall (kf,None) when Kernel_function.equal func kf && st=Call -> True
      | TCall (kf, Some _) when Kernel_function.equal func kf && st=Call -> 
        Undefined
      | TCall _ -> False
      | TReturn kf when Kernel_function.equal func kf && st=Return -> True
      | TReturn _ -> False
      | TTrue -> True
      | TFalse -> False
      | TRel _ -> Undefined
  in
  let cond,_ = tr.cross in
  let res = isCross cond <> False in
  Aorai_option.debug ~level:2 "Function %a %s-state, \
    transition %s -> %s is%s possible" Kernel_function.pretty func
    (if st=Call then "pre" else "post")
    tr.start.Promelaast.name
    tr.stop.Promelaast.name
    (if res then "" else " NOT");
  res

(* ************************************************************************* *)

let find_enum, set_enum =
  let module H =
    Cil_state_builder.Inthash
      (Cil_datatype.Enumitem)
      (struct
         let name = "ltl_states_enum"
         let size = 17
         let kind = `Internal
         let dependencies = (* TODO: projectify the automata
                               and depend on it.
                             *)
           [ Ast.self;
             Aorai_option.Ltl_File.self;
             Aorai_option.Buchi.self;
             Aorai_option.Ya.self
           ]
       end)
  in
  (fun n ->
    try H.find n
    with Not_found ->
      Aorai_option.fatal
        "Could not find the enum item corresponding to a state"),
  (List.iter (fun (n,item) -> H.add n item))

let debug_display_func_status name =
  let pre = Data_for_aorai.get_func_pre name in
  let post = Data_for_aorai.get_func_post name in
  let debug_pre = debug_display_stmt_all_pre pre in
  let debug_post = debug_display_stmt_all_pre post in
  Aorai_option.debug "%s %s %s" debug_pre name debug_post;
  Aorai_option.debug "\n"

(* ************************************************************************* *)
(** Given a function name, is status (call or return) and an array of
    boolean describing states status, it returns a couple of boolean
    array. The first one describes the set of reachable states and the
    second one is the set of crossable transitions. *)
let get_next func status states =
  let st,tr = mk_empty_pre_or_post () in
  let (_,trans_l) = Data_for_aorai.getAutomata() in
  List.iter
    (fun t ->
       if (states.(t.start.nums)) && (isCrossable t func status) then
         begin
           st.(t.stop.nums)<- true ;
           tr.(t.numt)<- true
         end
    )
    trans_l;
  (st,tr)



(** Given a function name, its status (call or return) and an array of
    boolean describing states status, it returns a couple of boolean
    array. The first one describes the set of possible initial states and
    the second one is the set of crossable transitions. *)
let get_prev func status (states,trans) =
  let st,tr = mk_empty_pre_or_post () in
  let (_,trans_l) = Data_for_aorai.getAutomata() in
  List.iter
    (fun t ->
       if (states.(t.stop.nums)) && (isCrossable t func status)
         && trans.(t.numt)
       then
         st.(t.start.nums)<- true
    )
    trans_l;

  List.iter
    (fun t ->
       if (st.(t.stop.nums))  then
         tr.(t.numt)<- true
    )
    trans_l;
  (st,tr)

(* ************************************************************************* *)

let mk_pre_or_post_bycase_from_pre_or_post (st,tr) =
  let st_bc,tr_bc = mk_empty_pre_or_post_bycase () in
  let (_,trans_l) = Data_for_aorai.getAutomata() in
  Array.iteri
    (fun index t -> if st.(index) then t.(index)<-true)
    st_bc;

  List.iter
    (fun t -> if tr.(t.numt) then tr_bc.(t.stop.nums).(t.numt)<-true)
    trans_l;

  (st_bc,tr_bc)

(* ************************************************************************* *)

let debug_display_func_status_bycase name =
  let pre = Data_for_aorai.get_func_pre name in
(*  let pre = mk_pre_or_post_bycase_from_pre_or_post pre in *)
  let post = Data_for_aorai.get_func_post_bycase name in
  let debug_pre = debug_display_stmt_all_pre pre in
  let debug_post =  debug_display_stmt_all_pre_bycase post in
  Aorai_option.debug "%s %s %s" debug_pre name debug_post;;

(* ************************************************************************* *)

(** Given a function name, is status (call or return) and an array of boolean describing states status, it returns a couple of boolean array. The first one describes the set of reachable states and the second one is the set of crossable transitions. *)
let get_next_bycase func status states_bycase =
  (* In a first pass we compute all cases of specification (For each starting state, we compute ending states set) *)
  let st_bc,tr_bc = mk_empty_pre_or_post_bycase () in
  let (_,trans_l) = Data_for_aorai.getAutomata() in
    List.iter
      (fun t ->
         if (isCrossable t func status) then
           begin
             st_bc.(t.start.nums).(t.stop.nums)<- true ;
             tr_bc.(t.start.nums).(t.numt)<- true
           end
      )
      trans_l;

  (* In second pass we replace each ending state from states_bycase by the new computed one *)
  let res_st_bc,res_tr_bc = mk_empty_pre_or_post_bycase () in
  Array.iteri
    (fun init_st init_st_assocs ->
       Array.iteri
         (fun end_st b ->
            if b then
              begin
                res_st_bc.(init_st) <- (bool_array_or (res_st_bc.(init_st)) (st_bc.(end_st)) );
                res_tr_bc.(init_st) <- (bool_array_or (res_tr_bc.(init_st)) (tr_bc.(end_st)) );
              end
         )
         init_st_assocs;
    )
    states_bycase;

  (res_st_bc,res_tr_bc)





(** Given a function name,
    its status (call or return) and an array of boolean describing states
    status, it returns a couple of boolean array.
    The first one describes the set of possible initial states and
    the second one is the set of crossable transitions.
 *)
let get_prev_bycase func status (states_bycase ,transitions_bycase) =
  let res_st_bc,res_tr_bc = mk_empty_pre_or_post_bycase () in
  (* For each starting case, we call the get_prev function *)
  Array.iteri
    (fun case_st case_st_assocs ->
       let prev_st,prev_tr =
         get_prev func status (case_st_assocs,transitions_bycase.(case_st))
       in
       res_st_bc.(case_st) <- prev_st;
       res_tr_bc.(case_st) <- prev_tr
    )
    states_bycase;

  (res_st_bc,res_tr_bc)







(* ************************************************************************* *)

(** Given a transition a function name and a function status (call or return)
    it returns if the cross condition can be statisfied with only
    function status. *)
let isCrossableAtInit tr func =
  (* When in doubt, return true anyway. More clever plug-ins will take care
     of analysing the instrumented code if needed. *)
  let eval_term_at_init t =
    if Kernel.LibEntry.get() then t
    else begin
      let bool_res test =
        if test then Cil.lconstant My_bigint.one else Cil.lzero ()
      in
      let bool3_res dft test =
        match test with
          | True -> bool_res true
          | False -> bool_res false
          | Undefined -> dft
      in
      let is_true t =
        match t with
          | TConst(CInt64(i,_,_)) ->
            Bool3.bool3_of_bool (not (My_bigint.is_zero i))
          | TConst(CChr c) -> Bool3.bool3_of_bool (not (Char.code c <> 0))
          | TConst(CReal (f,_,_)) -> Bool3.bool3_of_bool (not (f <> 0.))
          | TConst(CStr _ | CWStr _) -> Bool3.True
          | _ -> Bool3.Undefined
      in
      let rec aux t =
        match t.term_node with
          | TConst (CEnum ei) ->
            aux (Logic_utils.expr_to_term ~cast:false ei.eival)
          | TLval lv ->
            (match aux_lv lv with
              | Some t -> t
              | None -> t)
          | TUnOp(op,t1) ->
            let t1 = aux t1 in
            (match op,t1.term_node with
              | Neg, TConst(CInt64(i,ik,_)) ->
                { t with term_node = TConst(CInt64(My_bigint.neg i,ik,None)) }
              | Neg, TConst(CReal(f,fk,_)) ->
                { t with term_node = TConst(CReal(~-. f,fk,None)) }
              | LNot, t1 ->  bool3_res t (is_true t1)
              | _ -> t)
          | TBinOp(op,t1,t2) ->
            let t1 = aux t1 in
            let t2 = aux t2 in
            let rec comparison comp t1 t2 =
              match t1.term_node,t2.term_node with
                | TConst (CInt64(i1,_,_)), TConst (CInt64(i2,_,_)) ->
                  bool_res (comp (My_bigint.compare i1 i2))
                | TConst (CChr c1), TConst (CChr c2) ->
                  bool_res (comp (Char.compare c1 c2))
                | TConst(CReal (f1,_,_)), TConst (CReal(f2,_,_)) ->
                  bool_res (comp (compare f1 f2))
                | TCastE(ty1,t1), TCastE(ty2,t2)
                  when Cil_datatype.Typ.equal ty1 ty2 ->
                  comparison comp t1 t2
                | _ -> t
            in
            (match op, t1.term_node, t2.term_node with

              | PlusA, TConst(CInt64(i1,ik1,_)), TConst(CInt64(i2,_,_)) ->
                (* kind is not relevant in the logic. *)
                { t with term_node =
                    TConst(CInt64(My_bigint.add i1 i2,ik1,None))}
              | MinusA, TConst(CInt64(i1,ik1,_)), TConst(CInt64(i2,_,_)) ->
                { t with term_node =
                    TConst(CInt64(My_bigint.sub i1 i2,ik1,None)) }
              | Mult, TConst(CInt64(i1,ik1,_)), TConst(CInt64(i2,_,_)) ->
                { t with term_node =
                    TConst(CInt64(My_bigint.mul i1 i2,ik1,None)) }
              | Div, TConst(CInt64(i1,ik1,_)), TConst(CInt64(i2,_,_)) ->
                (try
                   { t with term_node =
                       TConst(CInt64(My_bigint.c_div i1 i2,ik1,None)) }
                 with Division_by_zero -> t)
              | Mod, TConst(CInt64(i1,ik1,_)), TConst(CInt64(i2,_,_)) ->
                (try
                   { t with term_node =
                       TConst(CInt64(My_bigint.c_rem i1 i2,ik1,None)) }
                 with Division_by_zero -> t)
              | Shiftlt, TConst(CInt64(i1,ik1,_)), TConst(CInt64(i2,_,_)) ->
                { t with term_node =
                    TConst(CInt64(My_bigint.shift_left i1 i2,ik1,None)) }
              | Shiftrt, TConst(CInt64(i1,ik1,_)), TConst(CInt64(i2,_,_)) ->
                { t with term_node =
                    TConst(CInt64(My_bigint.shift_right i1 i2,ik1,None)) }
              | Lt, _, _ -> comparison ((<) 0) t1 t2
              | Gt, _, _ -> comparison ((>) 0) t1 t2
              | Le, _, _ -> comparison ((<=) 0) t1 t2
              | Ge, _, _ -> comparison ((>=) 0) t1 t2
              | Eq, _, _ -> comparison ((=) 0) t1 t2
              | Ne, _, _ -> comparison ((<>) 0) t1 t2
              | LAnd, t1, t2 ->
                bool3_res t (Bool3.bool3and (is_true t1) (is_true t2))
              | LOr, t1, t2 ->
                bool3_res t (Bool3.bool3or (is_true t1) (is_true t2))
              | _ -> t)
          | TCastE(ty,t1) ->
            let t1 = aux t1 in
            (match t1.term_type with
                Ctype ty1 when Cil_datatype.Typ.equal ty ty1 -> t1
              | _ -> { t with term_node = TCastE(ty,t1) })
          | _ -> t
      and aux_lv (base,off) =
        match base with
          | TVar v ->
            (try
               Extlib.opt_bind
                 (fun v ->
                   let init = Globals.Vars.find v in
                   let init = match init.Cil_types.init with
                       None -> Cil.makeZeroInit ~loc:v.vdecl v.vtype
                     | Some i -> i
                   in
                   aux_init off init)
                 v.lv_origin
             with Not_found -> None)
          | TMem t ->
            (match (aux t).term_node with
              | TAddrOf lv -> aux_lv (Cil.addTermOffsetLval off lv)
              | _ -> None)
          | TResult _ -> None
      and aux_init off initinfo =
        match off, initinfo with
          | TNoOffset, SingleInit e ->
            Some (aux (Logic_utils.expr_to_term ~cast:false e))
          | TIndex(t,oth), CompoundInit (ct,initl) ->
            (match (aux t).term_node with
              | TConst(CInt64(i1,_,_)) ->
                Cil.foldLeftCompound ~implicit:true
                  ~doinit:
                  (fun o i _ t ->
                    match o with
                      | Index({ enode = Const(CInt64(i2,_,_))},_)
                          when My_bigint.equal i1 i2 -> aux_init oth i
                      | _ -> t)
                  ~ct ~initl ~acc:None
              | _ -> None)
          | TField(f1,oth), CompoundInit(ct,initl) ->
            Cil.foldLeftCompound ~implicit:true
              ~doinit:
              (fun o i _ t ->
                match o with
                  | Field(f2,_) when Cil_datatype.Fieldinfo.equal f1 f2 ->
                    aux_init oth i
                  | _ -> t)
              ~ct ~initl ~acc:None
          | _ -> None
      in
      aux t
    end
  in
  let eval_rel_at_init rel t1 t2 =
    let t1 = eval_term_at_init (Cil.constFoldTerm true t1) in
    let t2 = eval_term_at_init (Cil.constFoldTerm true t2) in
    let comp =
      match rel with
      | Req -> ((=) 0)
      | Rneq -> ((<>) 0)
      | Rge -> ((>=) 0)
      | Rgt -> ((>) 0)
      | Rle -> ((<=) 0)
      | Rlt -> ((<) 0)
    in
    let rec comparison t1 t2 =
      match t1.term_node,t2.term_node with
        | TConst (CInt64(i1,_,_)), TConst (CInt64(i2,_,_)) ->
          Bool3.bool3_of_bool (comp (My_bigint.compare i1 i2))
        | TConst (CChr c1), TConst (CChr c2) ->
          Bool3.bool3_of_bool (comp (Char.compare c1 c2))
        | TConst(CReal (f1,_,_)), TConst (CReal(f2,_,_)) ->
          Bool3.bool3_of_bool (comp (compare f1 f2))
        | TCastE(ty1,t1), TCastE(ty2,t2) when Cil_datatype.Typ.equal ty1 ty2 ->
          comparison t1 t2
        | _ -> Bool3.Undefined
    in
    comparison t1 t2
  in
  let rec isCross = function
    | TOr  (c1, c2) -> Bool3.bool3or (isCross c1) (isCross c2)
    | TAnd (c1, c2) -> Bool3.bool3and (isCross c1) (isCross c2)
    | TNot (c1) -> Bool3.bool3not (isCross c1)
    | TCall (s,None) -> Bool3.bool3_of_bool (Kernel_function.equal s func)
    | TCall (s, Some _) when Kernel_function.equal s func -> Undefined
    | TCall _ -> Bool3.False
    | TReturn _ -> Bool3.False
    | TTrue -> Bool3.True
    | TFalse -> Bool3.False
    | TRel(rel,t1,t2) -> eval_rel_at_init rel t1 t2

  in
  let (cond,_) = tr.cross in
  match isCross cond with
    | Bool3.True | Bool3.Undefined -> true
    | Bool3.False -> false

(* ************************************************************************* *)
(** {b Expressions management} *)

(** Returns an int constant expression which represents the given int value. *)
let mk_int_exp value =
  new_exp ~loc:Cil_datatype.Location.unknown
    (Const(CInt64(My_bigint.of_int value,IInt,Some(string_of_int value))))

(** Returns an lval expression which represents the access
    of the host_name variable (a string) with the offset off_exp
    (an expression).
 *)
let mk_offseted_array_lval host_name off_exp =
  let host_lval = (Cil.var (get_varinfo host_name)) in
    Cil.addOffsetLval
      (Index(off_exp,NoOffset))
      host_lval

(** Returns an lval expression which represents the access of the
    host_name variable (a string) with the offset off_value (an int). *)
let mk_int_offseted_array_lval host_name off_value =
  mk_offseted_array_lval host_name (mk_int_exp off_value)

let rec get_concrete_param_from_formal formal formall concretel f sid =
  match formall, concretel with
    | [],_
    | _, [] -> Aorai_option.fatal "The stmt %d is a call of the function %s, but it is not called with the formal parameter %s." sid f formal
    | f1::fl,c1::cl ->
        if (String.compare formal f1.vname)=0
        then c1.enode
        else get_concrete_param_from_formal formal fl cl f sid


(** Compute the set of concrete value of a call,
    associated to a given list of parameters.
    @param f name of the called function
    @param sid  stmt id of the call
    @param paramlist list of parameters name
    @return a list of exp_node, such that each formal parameter from
    paramlist is affected by the associated expression.
*)
let get_concrete_value_of_call (f:string) sid paramlist =
  let (stmt,_) = Kernel_function.find_from_sid sid in
  let kfunc = Globals.Functions.find_by_name f in
  let formall = Globals.Functions.get_params kfunc in
  match stmt.skind with
    | Instr(Cil_types.Call(_,_,concretel,_)) ->
        List.fold_left
          (fun fl p ->
             (* for an observed formal param p, we are looking for its associated concrete parameter *)
             (get_concrete_param_from_formal p formall concretel f sid)::fl
          )
          []
          paramlist
    | _ -> Aorai_option.fatal "The stmt %d have to be a call of the function %s, but it is not a call stmt." sid f


(** Compute the concrete value of a return
    @param f name of the called function
    @return an exp_node.
*)
let get_concrete_value_of_return (f:string) =
  let kf = Globals.Functions.find_by_name f in
  let rstmt =
    try Kernel_function.find_return kf
    with Kernel_function.No_Statement ->
      Aorai_option.fatal "Don't know what to do with a function declaration"
  in
  match rstmt.skind with
    | Cil_types.Return (Some (e),_) -> e.enode
    | Block (b) ->
        begin
          let s=(List.hd (List.rev b.bstmts)) in
          match s.skind with
            | Cil_types.Return (Some (e),_) -> e.enode
            | _ -> 
              Aorai_option.fatal 
                "The stmt %d have to be a return of the function %s, \
                  but it is not a well formed stmt." rstmt.sid f
        end
    | _ -> Aorai_option.fatal 
      "The stmt %d have to be a return of the function %s, \
       but it is not a well formed stmt." rstmt.sid f

(** This function rewrites a cross condition into an ACSL expression.
    Moreover, by giving current operation name and its status (call or
    return) the generation simplifies the generated expression.

    When called with an event (func, call_or_return), the conditions related
    to a particular event (PCall & co) will be directly evaluated to true or
    false. When event is None (in particular when generating the invariants),
    an appropriate predicate is generated.

    @param cross condition to convert from Promelaast.condition to
    {!Cil_types.predicate}
    @param op_logic_var operation variable
    @param status_logic_var status variable (call/return)
 *)
let crosscond_to_pred ?event cross op_logic_var status_logic_var =
  let inv = match event with None -> true | Some _ -> false in
  let check_current_event f status pred =
    let (curr_f, curr_status) = Extlib.the event in
    if Kernel_function.equal curr_f f && curr_status = status then pred
    else (Bool3.False, pfalse)
  in
  let rec convert =
    function
    (* Lazy evaluation of logic operators if the result can be statically
       computed *)
    | TOr  (c1, c2) -> (*BinOp(LOr,convert c1,convert c2,Cil.intType)*)
        begin
          let (c1_val,c1_pred) = convert c1 in
          match c1_val with
            | Bool3.True      -> (c1_val,c1_pred)
            | Bool3.False     -> convert c2
            | Undefined ->
                let (c2_val,c2_pred) = convert c2 in
                match c2_val with
                  | Bool3.True      -> (c2_val,c2_pred)
                  | Bool3.False     -> (c1_val,c1_pred)
                  | Undefined -> (Undefined,Logic_const.por(c1_pred, c2_pred))
        end

    | TAnd (c1, c2) -> (*BinOp(LAnd,convert c1,convert c2,Cil.intType)*)
        begin
          let (c1_val,c1_pred) = convert c1 in
          match c1_val with
            | Bool3.True      -> convert c2
            | Bool3.False     -> (c1_val,c1_pred)
            | Undefined ->
                let (c2_val,c2_pred) = convert c2 in
                match c2_val with
                  | Bool3.True      -> (c1_val,c1_pred)
                  | Bool3.False     -> (c2_val,c2_pred)
                  | Undefined -> (Undefined,Logic_const.pand(c1_pred, c2_pred))
        end

    | TNot (c1)     -> (*UnOp(LNot,convert c1,Cil.intType)*)
        begin
          let (c1_val,c1_pred) = convert c1 in
          match c1_val with
            | Bool3.True      -> (Bool3.False,pfalse)
            | Bool3.False     -> (Bool3.True,ptrue)
            | Undefined -> (c1_val,Logic_const.pnot(c1_pred))
        end

    (* Call and return are statically defined -- Case where inv = true *)
    | TCall (kf,b) when inv ->
      let s = Kernel_function.get_name kf in
      let res = 
        [ prel(Req,
              Logic_const.tvar op_logic_var,
              Logic_const.term (TConst(func_to_cenum  s))
                (Ctype (func_enum_type()))
         );
         prel(Req,
              Logic_const.tvar status_logic_var,
              Logic_const.term
                (TConst(op_status_to_cenum Promelaast.Call))
                (Ctype (status_enum_type()))
         )]
      in
      let res = 
        match b with
            None -> res
          | Some b ->
            List.rev_append
              (List.rev_map 
                 Logic_utils.named_of_identified_predicate b.b_assumes)
              res
      in
      (Undefined, Logic_const.pands res)
    | TReturn kf when inv ->
      let s = Kernel_function.get_name kf in
      (Undefined,
       Logic_const.pand(
         prel(Req,
              Logic_const.term
                (TLval(TVar(op_logic_var),TNoOffset))
                (Ctype Cil.intType),
              Logic_const.term
                (TConst(func_to_cenum s))
                (Ctype Cil.intType)
         ),
         prel(Req,
              Logic_const.term
                (TLval(TVar(status_logic_var),TNoOffset))
                (Ctype Cil.intType),
              Logic_const.term
                (TConst(op_status_to_cenum Promelaast.Return))
                (Ctype Cil.intType)
         )
       )
      )
    | TCall (f,b) ->
      let pred = match b with
          None -> Bool3.True, ptrue
        | Some b ->
          (Bool3.Undefined,
           Logic_const.pands
             (List.map Logic_utils.named_of_identified_predicate b.b_assumes))
      in
      check_current_event f Promelaast.Call pred
    | TReturn f ->
      check_current_event f Promelaast.Return (Bool3.True, ptrue)

    (* Other expressions are left unchanged *)
    | TTrue -> (Bool3.True, ptrue)
    | TFalse -> (Bool3.False, pfalse)
    | TRel(rel,t1,t2) ->
      (Bool3.Undefined, Logic_const.prel (rel,t1,t2))
  in
  snd (convert cross)

(* ************************************************************************* *)
(** {b Globals management} *)

(** Local copy of the file pointer *)
let file = ref Cil.dummyFile

(** Copy the file pointer locally in the class in order to ease globals
    management and initializes some tables. *)
let initFile f =
  file:=f ;
  Data_for_aorai.setCData ();
  (* Adding C variables into our hashtable *)
  Globals.Vars.iter (fun vi _ -> set_varinfo vi.vname vi);
  Globals.Functions.iter
    (fun kf ->
      let fname = Kernel_function.get_name kf in
      List.iter
        (fun vi -> set_paraminfo fname vi.vname vi)
        (Kernel_function.get_formals kf);
      if not (Data_for_aorai.isIgnoredFunction fname) then
        begin
	  try
            let ret  = Kernel_function.find_return kf in
            match ret.skind with
            | Cil_types.Return (Some e,_) ->
              (match e.enode with
              | Lval (Var vi,NoOffset) ->
                set_returninfo fname vi (* Add the vi of return stmt *)
              | _ -> () (* function without returned value *))
            | _ -> () (* function without returned value *)
	  with Kernel_function.No_Statement ->
	    Aorai_option.fatal
	      "Don't know what to do with a function declaration"
        end)

(** List of globals awaiting for adding into C file globals *)
let globals_queue = ref []

(** Flush all queued globals declarations into C file globals. *)
let flush_globals () =
  let (before,after)=List.fold_left
    (fun (b,a) elem ->
        match elem with
          | GFun(f,loc) as func ->
              (* [VP] if address of function is taken, it might be
                 used in a global initializer: keep a declaration at this point
                 to ensure ending up with a compilable C file in the end...
               *)
              let b =
                if f.svar.vaddrof then
                  GVarDecl(Cil.empty_funspec(),f.svar,loc)::b
                else b
              in
              (b,func::a)
          | other -> (other::b,a)
    )
    ([],[])
    !file.globals in

  !file.globals <- (List.rev before)@(List.rev !globals_queue)@(List.rev after);
  Kernel_function.clear_sid_info ();
  globals_queue:=[]

let mk_global glob = globals_queue:=glob::(!globals_queue)

(* Utilities for global variables *)
let mk_global_c_initialized_vars name ty ini=
  let vi = (Cil.makeGlobalVar name ty) in
    vi.vghost<-true;
    mk_global (GVar(vi,ini,vi.vdecl));
    Globals.Vars.add vi ini;
    set_varinfo name vi

let mk_global_var vi =
  vi.vghost<-true;
  let ini = 
    {Cil_types.init=Some(Cil.makeZeroInit ~loc:(CurrentLoc.get()) vi.vtype)}
  in
  mk_global (GVar(vi,ini,vi.vdecl));
  Globals.Vars.add vi ini;
  set_varinfo vi.vname vi

let mk_global_c_vars name ty =
  let vi = (Cil.makeGlobalVar name ty) in
  mk_global_var vi

let mk_global_c_var_init name init =
  let ty = Cil.typeOf init in
  let vi = Cil.makeGlobalVar name ty in
  vi.vghost <- true;
  let ini = { Cil_types.init = Some(SingleInit init) } in
  mk_global(GVar(vi,ini,vi.vdecl));
  Globals.Vars.add vi ini;
  set_varinfo name vi

let mk_int_const value =
  new_exp
    ~loc:(CurrentLoc.get())
    (Const(
       CInt64(
         My_bigint.of_int (value),
         IInt,
         Some(string_of_int(value))
       )))

let mk_global_c_initialized_array name size init =
  let ty =
    (TArray(
       TInt(IInt,[]),
       Some(mk_int_const size),
       empty_size_cache () ,
       []
     ))
  in
    mk_global_c_initialized_vars name ty init

let mk_global_c_array name size =
  let ty =
    (TArray(
       TInt(IInt,[]),
       Some(mk_int_const size),
       empty_size_cache (),
       []
     ))
  in
    mk_global_c_vars name ty


let mk_global_c_int name  =
  let ty = (TInt(IInt,[])) in
    mk_global_c_vars name ty

(* Utilities for global enumerations *)
let mk_global_c_enum_type_tagged name elements_l =
  let einfo =
    { eorig_name = name;
      ename = name;
      eitems = [];
      eattr = [];
      ereferenced = true;
      ekind = IInt; }
  in
  let l =
    List.map
      (fun (e,i) ->
        { eiorig_name = e;
          einame = e;
          eival = mk_int_const i;
          eiloc = Location.unknown;
          eihost = einfo})
      elements_l
  in
  einfo.eitems <- l;
  set_usedinfo name einfo;
  mk_global (GEnumTag(einfo, Location.unknown));
  einfo

let mk_global_c_enum_type name elements =
  let _,elements =
    List.fold_left (fun (i,l) x -> (i+1,(x,i)::l)) (0,[]) elements
  in
  (* no need to rev the list, as the elements got their value already *)
  ignore (mk_global_c_enum_type_tagged name elements)


let mk_global_c_enum name name_enuminfo =
  mk_global_c_vars name (TEnum(get_usedinfo name_enuminfo,[]))


let mk_global_c_initialized_enum name name_enuminfo ini =
  mk_global_c_initialized_vars name (TEnum(get_usedinfo name_enuminfo,[])) ini



(* ************************************************************************* *)
(** {b Terms management / computation} *)

(** Return an integer constant term from the given value. *)
let mk_int_term value = Cil.lconstant (My_bigint.of_int value)

(** Return an integer constant term with the 0 value.
    @deprecated use directly Cil.lzero
*)
let zero_term() = Cil.lzero ()

let one_term () = Cil.lconstant My_bigint.one

(** Returns a term representing the given logic variable (usually a fresh quantified variable). *)
let mk_term_from_logic_var lvar =
  Logic_const.term (TLval(TVar(lvar),TNoOffset)) (Ctype Cil.intType)

(** Returns a term representing the variable associated to the given varinfo *)
let mk_term_from_vi vi =
  Logic_const.term
    (TLval((Logic_utils.lval_to_term_lval ~cast:true (Cil.var vi))))
    (Ctype Cil.intType)

let mk_trans_cst i = mk_int_term i

(** Given an lval term 'host' and an integer value 'off', it returns a lval term host[off]. *)
let mk_offseted_array host off =
  Logic_const.term
    (TLval(Cil.addTermOffsetLval (TIndex(mk_int_term (off),TNoOffset)) host))
    (Ctype Cil.intType)

let int2enumstate nums =
  let enum = find_enum nums in
  Logic_const.term (TConst (CEnum enum)) (Ctype (TEnum (enum.eihost,[])))

(** Given an lval term 'host' and an integer value 'off', it returns a lval term host[off]. *)
let mk_offseted_array_states_as_enum host off =
  let enum = find_enum off in
  Logic_const.term
    (TLval
       (Cil.addTermOffsetLval
          (TIndex(Logic_const.term
                    (TConst(CEnum enum)) (Ctype (TEnum (enum.eihost,[]))),
                  TNoOffset))
          host))
    (Ctype Cil.intType)


(** Given an lval term 'host' and a term 'term_off', it returns a lval term host[off]. *)
let mk_offseted_array_lval_from_term host term_off =
  Logic_const.term
    (TLval(Cil.addTermOffsetLval (TIndex(term_off,TNoOffset)) host))
    (Ctype Cil.intType)


(** Given an lval term 'host' and a logic variable 'lvar_off', it returns a lval term host[off].
    Usually, logic variables stand for fresh quantified variables. *)
let mk_offseted_array_lval_from_lval host lvar_off =
  mk_offseted_array_lval_from_term host (mk_term_from_logic_var lvar_off)

(** Given the name of a logic and a list of logic variables it returns a call of the logic with variables as parameters. *)
(* [VP] Are we sure that the expression type is always int? *)
let mk_logic_call name logicvar_param_l =
  let li = get_logic name in
  match logicvar_param_l with
      | [] ->
          Logic_const.term
            (TLval (TVar li.l_var_info,TNoOffset)) (Ctype Cil.intType)
      | _ ->
          Logic_const.term
            (Tapp(li,[],
                  List.map (fun p -> mk_term_from_logic_var p)
                    logicvar_param_l))
            (Ctype Cil.intType)

(** Returns a lval term associated to the curState generated variable. *)
let host_state_term() =
  lval_to_term_lval ~cast:true (Cil.var (get_varinfo curState))
(*
(** Returns a lval term associated to the curStateOld generated variable. *)
let host_stateOld_term () =
  lval_to_term_lval ~cast:true (Cil.var (get_varinfo curStateOld))

(** Returns a lval term associated to the curTrans generated variable. *)
let host_trans_term () =
  lval_to_term_lval ~cast:true (Cil.var (get_varinfo curTrans))
*)
let state_term () =
  Logic_const.tvar (Cil.cvar_to_lvar (get_varinfo curState))
(*
let stateOld_term () =
  Logic_const.tvar (Cil.cvar_to_lvar (get_varinfo curStateOld))
let trans_term () =
  Logic_const.tvar (Cil.cvar_to_lvar (get_varinfo curTrans))
*)
let is_state_pred state =
  if Aorai_option.Deterministic.get () then
    Logic_const.prel (Req,state_term(),int2enumstate state.nums)
  else
    Logic_const.prel (Req,one_term(),
                      mk_offseted_array_states_as_enum
                        (host_state_term()) state.nums)

let is_out_of_state_pred state =
  if Aorai_option.Deterministic.get () then
    Logic_const.prel (Rneq,state_term(),int2enumstate state.nums)
  else
    Logic_const.prel (Req,zero_term(),
                      mk_offseted_array_states_as_enum
                        (host_state_term()) state.nums)

(** Given a logic variable and two bounds, it returns the predicate:
    min<=v<max
*)
let mk_logicvar_intervalle logvar min max =
  Logic_const.pand(prel(Rle,mk_int_term min,mk_term_from_logic_var logvar),
                   prel(Rlt,mk_term_from_logic_var logvar,mk_int_term max))

(** Given two names of generated arrays and their size, it returns the predicate: (forall i. 0<=i<size => host1[i]==host2[i]) *)
let mk_eq_tables host_name1 host_name2 size =
  let lval1 = lval_to_term_lval ~cast:true ( Cil.var (get_varinfo host_name1)) in
  let lval2 = lval_to_term_lval ~cast:true ( Cil.var (get_varinfo host_name2)) in
  let tmp_i = Cil_const.make_logic_var "_buch_i" Cil_types.Linteger in
  pforall([tmp_i],
          pimplies
            (mk_logicvar_intervalle tmp_i 0 size,
             prel(Req,
                  mk_offseted_array_lval_from_lval lval1 tmp_i ,
                  mk_offseted_array_lval_from_lval lval2 tmp_i)))

(** Given a name of generated array and its size, it returns the expression:
    (Valid_range(name,0,size-1) *)
let mk_valid_range name size =
  let var = get_varinfo name in
  let lval = lval_to_term_lval ~cast:true  (Cil.var var) in
  let min = mk_int_term 0 in
  let max = mk_int_term (size-1) in
  let range = trange (Some min, Some max) in
  let typ = Cil.typeOf_pointed var.vtype in
  let ltyp = Ctype typ in
  let lstyp = Logic_const.make_set_type ltyp in
    Pvalid(term (TBinOp(PlusPI,(term (TLval lval) ltyp),range)) lstyp)

(* Utilities for other globals *)

let mk_global_invariant pred name =
  let li = Cil_const.make_logic_info name in
  li.l_body <- LBpred pred;
  let annot = Cil_types.Dinvariant (li,Location.unknown) in
  Globals.Annotations.add_user annot;
  mk_global (GAnnot (annot, Location.unknown))

let mk_global_comment txt = mk_global (GText (txt))

(** Given
      + the name of the logic (string),
      + the list of its genericity parameter names (string),
      + the list of their type (logic_var),
      + the type of the function return
      + and a list of reads tsets,
    it returns a logic function declaration.
    A side effect of this function is the registration of this logic into the logics hashtbl from Data_for_aorai. *)
let mk_global_logic name (*generics_l*) types_l type_ret (*reads*) =
  let log_info = Cil_const.make_logic_info name in
  log_info.l_type <- type_ret;        (*        return type.        *)
  log_info.l_profile <- types_l;(*        type of the arguments.        *)
(*
  l_labels = [];         (*        label arguments of the function. *)
  l_body = LBreads([]); (*        body of the function.        *)
  l_tparams = []
*)
  Data_for_aorai.add_logic name log_info;
  Dfun_or_pred(log_info, Location.unknown)

let mk_global_axiom name pred =
  Dlemma (name, true, [], [], unamed pred, Location.unknown)

let mk_global_predicate name moment params_l pred =
  (*let log_var_params = List.map (fun p -> Cil.make_logic_var p Linteger) params_l in *)
  let pred_info= Cil_const.make_logic_info name in
  (*        name of the predicate.        *)
  pred_info.l_profile <- params_l; (*log_var_params;*)
  (*        arguments of the predicate.        *)
  pred_info.l_labels <- List.map (fun x -> LogicLabel(None, x)) moment;
  (*        label arguments.        *)
  pred_info.l_body <- LBpred pred; (*        definition.        *)
(*
  pred_info.l_type <- None;        (*        return type.        *)
  pred_info.l_tparams <- []
*)
  Data_for_aorai.add_predicate name pred_info;
  let annot = Dfun_or_pred(pred_info, Location.unknown) in
  Globals.Annotations.add_user annot;
  mk_global (GAnnot(annot, Location.unknown))

(*
(** Generates an axiomatisation of transitions from automata into globals.
    These annotations are used to express some pre and post condition properties *)
let mk_decl_axiomatized_automata () =
  let getNamedOffset s =
    Logic_const.term (TConst (CEnum (find_enum s.nums))) (Ctype Cil.intType)
  in
  let (_,trans_l) = getAutomata() in
  let param=(Cil_const.make_logic_var "tr" Linteger) in
  let logic=mk_global_logic transStart (*[]*) [param] (Some Linteger) (*[]*) (*[TSSingleton(TSLval(TSVar(param),TSNo_offset))]*) in
  let tr_start_log_info = Data_for_aorai.get_logic transStart in
  let annotlist=List.fold_left
    (fun res tr ->
       (mk_global_axiom
          (transStart^(string_of_int tr.numt))
          (Prel(Req,
                Logic_const.term
                  (Tapp(tr_start_log_info,[],[mk_int_term tr.numt]))
                  (Ctype Cil.intType),
                (getNamedOffset tr.start)
               ))
       )::res
    )
    [logic]
    trans_l in
  let annot = Daxiomatic(transStart,List.rev annotlist, Location.unknown) in
  Globals.Annotations.add_user annot;
  mk_global (GAnnot(annot, Location.unknown));


  let logic=mk_global_logic transStop  (*[]*) [param] (Some Linteger) (*[]*) (*[TSSingleton(TSLval(TSVar(param),TSNo_offset))]*) in
  let tr_stop_log_info = Data_for_aorai.get_logic transStop in
  let annotlist=List.fold_left
    (fun res tr ->
       (mk_global_axiom
         (transStop^(string_of_int tr.numt))
         (Prel(Req,
               Logic_const.term
                 (Tapp(tr_stop_log_info,[],[mk_int_term tr.numt]))
                 (Ctype Cil.intType),
               (getNamedOffset tr.stop)))
       )::res
    )
    [logic]
    trans_l in
  let annot = Daxiomatic(transStop, List.rev annotlist, Location.unknown) in
  Globals.Annotations.add_user annot;
  mk_global (GAnnot(annot, Location.unknown));

  let num= Cil_const.make_logic_var "_aorai_numTrans" Linteger in
  let op = Cil_const.make_logic_var "_aorai_op" Linteger in
  let st = Cil_const.make_logic_var "_aorai_status" Linteger in
  let pred =
    Logic_const.pands
      (List.map
         (fun tr ->
           pimplies(
             crosscond_to_pred tr.cross op st,
             prel(Req, mk_term_from_logic_var num, mk_int_term tr.numt)
             )
         )
         trans_l
      )
  in
  mk_global_predicate transCondP ["L"] [num;op;st] pred;

  let pred2 =
    papp(
      Data_for_aorai.get_predicate transCondP,
      [(LogicLabel(None,"L"),LogicLabel(None,"L"))],
      [mk_term_from_logic_var num;
       mk_term_from_logic_var (Cil.cvar_to_lvar (Data_for_aorai.get_varinfo curOp));
       mk_term_from_logic_var (Cil.cvar_to_lvar (Data_for_aorai.get_varinfo curOpStatus))
      ]
    )
  in
  mk_global_predicate transCond ["L"] [num] pred2
*)
(* ************************************************************************* *)
(** {b Initialization management / computation} *)

let get_states_trans_init root =
  let (states,trans) = Data_for_aorai.getAutomata () in
  let st_exps = (Array.make (List.length states) (mk_int_exp 0)) in

  List.iter (
    fun tr ->
      if (tr.start.Promelaast.init==Bool3.True) &&
        (isCrossableAtInit tr root)
      then
        begin
          Array.set st_exps tr.start.nums (mk_int_exp 1);
        end
  ) trans;

  let st_init =
    Array.mapi (
      fun i exp ->
        (*Chaque cas doit contenir : (offset * init)*)
        (Index(mk_int_exp i,NoOffset),SingleInit(exp))
    ) st_exps
  in
  {Cil_types.init=Some(CompoundInit(Cil.intType, Array.to_list st_init))}

let func_to_init name =
  {Cil_types.init=
      Some(SingleInit(
        new_exp ~loc:(CurrentLoc.get()) (Const(func_to_cenum (name)))))}

let funcStatus_to_init st =
  {Cil_types.init=Some(SingleInit(new_exp ~loc:(CurrentLoc.get())
                                    (Const(op_status_to_cenum (st)))))}

class visit_decl_loops_init () =
object (*(self) *)
  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vstmt_aux stmt =
    begin
      match stmt.skind with
        | Loop _ -> mk_global_c_vars (Data_for_aorai.loopInit^"_"^(string_of_int stmt.sid)) (TInt(IInt,[]))
        | _ -> ()
    end;
    Cil.DoChildren
end

let mk_decl_loops_init () =
  let visitor = new visit_decl_loops_init ()  in
  Cil.visitCilFile (visitor :> Cil.cilVisitor) !file

let change_vars subst subst_res kf label pred =
  let add_label t = ChangeDoChildrenPost(t,fun t -> tat(t,label)) in
  let visitor =
    object
      inherit Visitor.frama_c_copy (Project.current())

      method vterm t =
        match t.term_node with
            TLval (TVar { lv_origin = Some v},_) when v.vglob -> add_label t
          | TLval (TMem _,_) -> add_label t
          | _ -> DoChildren

      method vterm_lhost = function
        | TResult ty ->
          (match kf with
              None -> Aorai_option.fatal
                "found \\result without being at a Return event"
            | Some kf ->
              (try
                 ChangeTo (TVar (Kernel_function.Hashtbl.find subst_res kf))
               with Not_found ->
                 let new_lv =
                   Cil_const.make_logic_var
                     ("__retres_" ^ (Kernel_function.get_name kf)) (Ctype ty)
                 in
                 Kernel_function.Hashtbl.add subst_res kf new_lv;
                 ChangeTo (TVar new_lv)))
        | TMem _ | TVar _ -> DoChildren

      method vlogic_var_use lv =
        match lv.lv_origin with
          | Some v when not v.vglob ->
            (try
               ChangeTo (Cil_datatype.Logic_var.Hashtbl.find subst lv)
             with Not_found ->
               let new_lv = Cil_const.make_logic_var lv.lv_name lv.lv_type in
               Cil_datatype.Logic_var.Hashtbl.add subst lv new_lv;
               ChangeTo new_lv)
          | Some _ | None -> DoChildren
    end
  in Visitor.visitFramacPredicate visitor pred

let make_prev_pred func status st auto_state =
  let (_,tr_state) = auto_state in
  let auto = Data_for_aorai.getAutomata () in
  let trans = Path_analysis.get_transitions_to_state st auto in
  let event = (func, status) in
  let op = Data_for_aorai.get_logic_var Data_for_aorai.curOp in
  let func_status = Data_for_aorai.get_logic_var Data_for_aorai.curOp in
  List.fold_left
    (fun conds tr ->
      if tr_state.(tr.numt) then
        let cond, _ = tr.cross in
        let my_pred =
          Logic_const.pand
            (is_state_pred tr.start,
             crosscond_to_pred ~event cond op func_status)
        in
        Logic_const.por (my_pred,conds)
      else conds (* transition can't be taken at that point. *)
    )
    pfalse trans

let make_prev_pred_neg func status st auto_state =
  let (_,tr_state) = auto_state in
  let auto = Data_for_aorai.getAutomata () in
  let treat_one (start_states, cond1) st =
    let trans = Path_analysis.get_transitions_to_state st auto in
    let event = (func, status) in
    let op = Data_for_aorai.get_logic_var Data_for_aorai.curOp in
    let func_status = Data_for_aorai.get_logic_var Data_for_aorai.curOp in
    let start_states, cond2 =
      List.fold_left
        (fun (start_states,conds as acc) tr ->
          if tr_state.(tr.numt) then
            let cond, _ = tr.cross in
            let my_pred =
              Logic_const.pand
                (is_state_pred tr.start,
                 Logic_const.pnot 
                   (crosscond_to_pred ~event cond op func_status))
            in
            Data_for_aorai.Aorai_state.Set.add tr.start start_states,
            Logic_const.por (my_pred,conds)
          else acc)
        (start_states,pfalse) trans
    in start_states, Logic_const.pand (cond1, cond2)
  in
  let (start_states, cond) =
    List.fold_left treat_one (Data_for_aorai.Aorai_state.Set.empty,ptrue) st
  in
  let not_start =
    Data_for_aorai.Aorai_state.Set.fold
      (fun st acc -> Logic_const.pand (is_out_of_state_pred st,acc))
      start_states ptrue
  in
  Logic_const.por (cond, not_start)

let pred_of_condition subst subst_res label cond =
  let mk_func_event f =
    let op = tat (mk_term_from_vi (get_varinfo curOp),label) in
      (* [VP] TODO: change int to appropriate enum type. Also true
         elsewhere.
       *)
    let f = term (TConst (func_to_cenum f)) (Ctype (func_enum_type ())) in
    prel (Req,op,f)
  in
  let mk_func_status f status =
    let curr = tat (mk_term_from_vi (get_varinfo curOpStatus),label) in
    let call =
      term (TConst (op_status_to_cenum status)) (Ctype (status_enum_type()))
    in Logic_const.pand (mk_func_event f, prel(Req,curr,call))
  in
  let mk_func_start f = mk_func_status f Promelaast.Call in
  let mk_func_return f = mk_func_status f Promelaast.Return in
  let rec aux kf pos = function
    | TOr(c1,c2) ->
      kf, Logic_const.por (snd (aux kf pos c1), snd (aux kf pos c2))
    | TAnd(c1,c2) ->
      let kf, c1 = aux kf pos c1 in
      let kf, c2 = aux kf pos c2 in
      kf, Logic_const.pand (c1, c2)
    | TNot c -> let kf, c = aux kf (not pos) c in kf, Logic_const.pnot c
    | TCall (s,b) ->
      let pred = mk_func_start (Kernel_function.get_name s) in
      let pred = 
        match b with
          | None -> pred
          | Some b ->
            Logic_const.pands
              (pred :: 
                 (List.map 
                    Logic_utils.named_of_identified_predicate b.b_assumes))
      in
      kf, pred
    | TReturn s ->
      let kf = if pos then Some s else kf in
      kf, mk_func_return (Kernel_function.get_name s)
    | TTrue -> kf, ptrue
    | TFalse -> kf, pfalse
    | TRel(rel,t1,t2) ->
      kf,
      unamed (change_vars subst subst_res kf label (prel (rel,t1,t2)).content)
  in snd (aux None true cond)

let mk_deterministic_lemma () =
  let automaton = Data_for_aorai.getAutomata () in
  let make_one_lemma state =
    let label = Cil_types.LogicLabel(None, "L") in
    let disjoint_guards acc trans1 trans2 =
      if trans1.numt <= trans2.numt then acc
      (* don't need to repeat the same condition twice*)
      else
        let subst = Cil_datatype.Logic_var.Hashtbl.create 5 in
        let subst_res = Kernel_function.Hashtbl.create 5 in
        let guard1 = 
          pred_of_condition subst subst_res label (fst trans1.cross)
        in
        let guard2 = 
          pred_of_condition subst subst_res label (fst trans2.cross)
        in
        let pred = Logic_const.pnot (Logic_const.pand (guard1, guard2)) in
        let quants =
          Cil_datatype.Logic_var.Hashtbl.fold
            (fun _ lv acc -> lv :: acc) subst []
        in
        let quants = Kernel_function.Hashtbl.fold
          (fun _ lv acc -> lv :: acc) subst_res quants
        in
        (* [VP] far from perfect, but should give oracles for
           regression tests that stay relatively stable across vid
           changes.  *)
        let quants =
          List.sort (fun v1 v2 -> String.compare v1.lv_name v2.lv_name) quants
        in
        Logic_const.pand (acc, (pforall (quants, pred)))
    in
    let trans = Path_analysis.get_transitions_of_state state automaton in
    let prop = Extlib.product_fold disjoint_guards ptrue trans trans in
    let name = state.Promelaast.name ^ "_deterministic_trans" in
    let lemma =
      Dlemma (name, false, [label],[],prop,Cil_datatype.Location.unknown)
    in
    Globals.Annotations.add_user lemma;
    mk_global (GAnnot(lemma,Cil_datatype.Location.unknown))
  in
  List.iter make_one_lemma (fst automaton)

let unique_initial_trans root =
  let (_,tr) = Data_for_aorai.getAutomata () in
  let res =
    List.fold_left
      (fun acc tr ->
        if tr.start.Promelaast.init = Bool3.True then begin
          match acc, isCrossableAtInit tr root with
            | _, false -> acc
            | Some(false,_),_ -> acc
              (* once we decide that there could be two transitions active,
                 stay that way. *)
            | Some(true,idx), true -> Some(false,idx)
              (* we already have one transition that might be active. If we find
                 another, we don't have unicity. *)
            | None, true -> Some(true,tr.numt)
              (* All transitions seen so far were not active, we have found one
                 that is (or might be). just take it.
               *)
        end else acc
      )
      None
      tr
  in
  match res with
      None -> None
    | Some(false,_) -> None
    | Some(true,idx) -> Some idx

let make_enum_states () =
  let state_list =fst (Data_for_aorai.getAutomata()) in
  let state_list =
    List.map (fun x -> (x.Promelaast.name, x.Promelaast.nums)) state_list
  in
  let state_list =
      if not (Aorai_option.Deterministic.get ()) then state_list
      else
        (*[VP] Strictly speaking this is not needed, but Jessie tends
          to consider that a value of enum type can only be one of the
          tags, so that we must add this dummy state that is always a
          possible value, even when a contract concludes that curState
          is none of the others. Note that ISO C does not impose this
          limitation to values of enum types.
         *)
        (get_fresh "aorai_reject_state", -2)::state_list
  in
  let enum = mk_global_c_enum_type_tagged states state_list in
  let mapping =
    List.map
      (fun (name,id) ->
        let item =
          List.find (fun y -> y.einame = name) enum.eitems
        in
        (id, item))
      state_list
  in
  set_enum mapping

let getInitialState () =
  let loc = Cil_datatype.Location.unknown in
  let states = fst (Data_for_aorai.getAutomata()) in
  let s = List.find (fun x -> x.Promelaast.init = Bool3.True) states in
  Cil.new_exp ~loc (Const (CEnum (find_enum s.nums)))

(** This function computes all newly introduced globals (variables, enumeration structure, invariants, etc. *)
let initGlobals root complete =
  mk_global_comment "//****************";
  mk_global_comment "//* BEGIN Primitives generated for LTL verification";
  mk_global_comment "//* ";
  mk_global_comment "//* ";
  mk_global_comment "//* Some constants";
  make_enum_states ();
  mk_global_c_enum_type
    listOp (List.map (fun e -> func_to_op_func e) (getFunctions_from_c()));
  mk_global_c_initialized_enum curOp listOp
    (func_to_init (Kernel_function.get_name root));
  mk_global_c_enum_type  listStatus (callStatus::[termStatus]);
  mk_global_c_initialized_enum
    curOpStatus listStatus (funcStatus_to_init Promelaast.Call);

  mk_global_comment "//* ";
  mk_global_comment "//* States and Trans Variables";
  let st_init = get_states_trans_init root in
  if Aorai_option.Deterministic.get () then begin
    mk_global_c_var_init curState (getInitialState());
  end else begin
    mk_global_c_initialized_array curState (getNumberOfStates()) st_init;
  end;

  if complete then
    begin
      mk_global_comment "//* ";
      mk_global_comment "//* Loops management";
      mk_decl_loops_init ();
    end;

  if Aorai_option.Deterministic.get () then begin
    mk_global_comment "//* ";
    mk_global_comment "//**************** ";
    mk_global_comment "//* Proof that the automaton is deterministic";
    mk_global_comment "//* ";
    mk_deterministic_lemma ();
  end;

  mk_global_comment "//* ";
  mk_global_comment "//****************** ";
  mk_global_comment "//* Auxiliary variables used in transition conditions";
  mk_global_comment "//*";
  List.iter mk_global_var (Data_for_aorai.aux_variables());
  (match Data_for_aorai.abstract_logic_info () with
    | [] -> ()
    | l ->
      let annot =
        Daxiomatic
          ("Aorai_pebble_axiomatic",
           List.map
             (fun li -> Dfun_or_pred(li,Cil_datatype.Location.unknown)) l,
           Cil_datatype.Location.unknown)
      in
      Globals.Annotations.add_user annot;
      mk_global (GAnnot(annot, Cil_datatype.Location.unknown)));
  mk_global_comment "//* ";
  mk_global_comment "//* END Primitives generated for LTL verification";
  mk_global_comment "//****************";

  flush_globals()

(* ************************************************************************* *)
(** {b Pre/post management} *)

(** Function called by mk_abstract_pre and mk_abstract_post. *)
let mk_abstract_pre_post (states_l,trans_l) func status =
  (* Initially, no state is a source for crossable 
     transition and no transition is crossable
   *)
  let st_status = Array.make (List.length states_l) false in
  let tr_status = Array.make (List.length trans_l) false in

  (* Conjunction of forbidden transitions and disjunction of crossable
     transitions are computed together.  Moreover, authorized states
     are annotated in the same pass.  *)

  List.iter
    (fun tr ->
       if isCrossable tr func status then
         begin
           Array.set st_status tr.stop.nums true;
           Array.set tr_status tr.numt true
         end
    )
    trans_l;

  (st_status,tr_status)

(**{b Pre and post condition of C functions} In our point of view, the pre or
   the post condition of a C function are defined by the set of states
   authorized just before/after the call, as such as the set of crossable
   transitions. The following functions generates abstract 
   pre and post-conditions by using only informations deduced 
   from the buchi automata.
 *)
(** Given the buchi automata and the name of a function, it returns two arrays
    corresponding to the abstract pre-condition. *)
let mk_abstract_pre auto func =
  mk_abstract_pre_post auto func Promelaast.Call

(** Given the buchi automata and the name of a function, it returns two arrays
    corresponding to the abstract post-condition. *)
let mk_abstract_post auto func =
  mk_abstract_pre_post auto func Promelaast.Return

(** Generates a term representing the given pre or post condition.
    Transitions and states are rewritten into predicates in the same
    manner. The computation is then generalized Conjunction of
    forbidden and disjunction of authorized are computed together. *)
let pre_post_to_term  (st_status, _tr_status) =
  if Aorai_option.Deterministic.get () then begin
    let pp_to_term status var f =
      let (authorized,_) =
        Array.fold_left
          (fun (auth,i) b ->
            if b then
              Logic_const.por(auth,prel(Req,var (), f i)),i+1
            else
              auth,i+1)
          (pfalse,0)
          status
      in authorized
    in
(*    let tr =
      pp_to_term tr_status trans_term mk_trans_cst in
*)
    let st = pp_to_term st_status state_term int2enumstate in
    st
  end else begin
    let pp_to_term an_array array_term func =
      let (authorized,_) =
        Array.fold_left
          (fun (au_pred,i) b ->
            if b then
              begin
                (Logic_const.por(au_pred,prel(Req, one_term(),
                                              (func array_term i))),
                 i+1
                )
              end
            else
              (au_pred,i+1)
          )
          (pfalse,0)
          an_array
      in
      authorized
    in
(*    let tr = pp_to_term tr_status (host_trans_term ())  mk_offseted_array in
 *)
    let st =
      pp_to_term st_status (host_state_term ())
        mk_offseted_array_states_as_enum
    in
    st (* @ tr *)
  end

let pre_post_to_term_neg  (st_status, _) =
  if Aorai_option.Deterministic.get () then begin
    let pp_to_term status var f =
      let (forbidden,_) =
        Array.fold_left
          (fun (auth,i) b ->
            if not b then
              Logic_const.por(auth,prel(Req,var (), f i)),i+1
            else
              auth,i+1)
          (pfalse,0)
          status
      in forbidden
    in
    let st = pp_to_term st_status state_term int2enumstate in
    st
  end else begin
    let pp_to_term an_array array_term func =
      let (forbidden,_) =
        Array.fold_left
          (fun (au_pred,i) b ->
            if b then
              begin
                (Logic_const.pand(au_pred,prel(Req, zero_term(),
                                              (func array_term i))),
                 i+1
                )
              end
            else
              (au_pred,i+1)
          )
          (ptrue,0)
          an_array
      in
      forbidden
    in
    let st =
      pp_to_term st_status (host_state_term ())
        mk_offseted_array_states_as_enum
    in
    st
  end

(* assigns curState, curOp and curOpStatus *)
let aorai_assigns loc =
  let mk_from base =
    let zone =
      if Aorai_option.Deterministic.get () then
        Logic_const.term ~loc (TLval base) (Cil.typeOfTermLval base)
      else
        let intv = TIndex (trange ~loc (None,None), TNoOffset) in
        let tlv = Cil.addTermOffsetLval intv base in
        Logic_const.term ~loc (TLval tlv) (Cil.typeOfTermLval tlv)
    in (Logic_const.new_identified_term zone, FromAny)
  in
  Writes
    [ mk_from (host_state_term ());
      (* mk_from (host_stateOld_term ());
         mk_from (host_trans_term ()); *)
      (Logic_const.new_identified_term
         (Logic_const.tvar ~loc
            (Data_for_aorai.get_logic_var Data_for_aorai.curOpStatus)),
       FromAny);
      (Logic_const.new_identified_term
         (Logic_const.tvar ~loc
            (Data_for_aorai.get_logic_var Data_for_aorai.curOp)),
       FromAny)
    ]

let action_assigns trans =
  let add_if_needed v lv (known_vars, assigns as acc) =
    if Cil_datatype.Varinfo.Set.mem v known_vars then acc
    else
      Cil_datatype.Varinfo.Set.add v known_vars,
      (Logic_const.new_identified_term lv, FromAny)::assigns
  in
  let treat_one_action acc =
    function
      | Counter_init (host,off) | Counter_incr (host,off) 
      | Copy_value ((host,off),_) ->
        let my_var = 
          match host with
            | TVar ({ lv_origin = Some v}) -> v
            | _ -> Aorai_option.fatal "Auxiliary variable is not a C global"
        in 
        let my_off = 
          match off with
            | TNoOffset -> TNoOffset
            | TIndex _ -> TIndex(Logic_const.trange (None,None), TNoOffset)
            | TField _ -> 
              Aorai_option.fatal "Unexpected offset in auxiliary variable"
        in
        add_if_needed my_var
          (Logic_const.term (TLval(host,my_off)) 
             (Cil.typeOfTermLval (host,my_off)))
          acc
      | Pebble_init(_,v,c) ->
        let cc = Extlib.the c.lv_origin in
        let cv = Extlib.the v.lv_origin in
        add_if_needed cv (Logic_const.tvar v)
          (add_if_needed cc (Logic_const.tvar c) acc)
      | Pebble_move(_,v1,_,v2) ->
        let cv1 = Extlib.the v1.lv_origin in
        let cv2 = Extlib.the v2.lv_origin in
        add_if_needed cv1 (Logic_const.tvar v1) 
          (add_if_needed cv2 (Logic_const.tvar v2) acc)
  in
  let treat_one acc tr =
    let empty_pebble = 
      match tr.start.multi_state, tr.stop.multi_state with
        | Some(_,aux), None -> 
          let caux = Extlib.the aux.lv_origin in
          add_if_needed caux (Logic_const.tvar aux) acc
        | _ -> acc
    in
    List.fold_left treat_one_action empty_pebble (snd tr.cross)
  in
  Writes 
    (snd (List.fold_left treat_one (Cil_datatype.Varinfo.Set.empty,[]) trans))

(* force that we have a crossable transition for each state in which the
   automaton might be at current event. *)
let force_transition loc f st (_,tr_status) =
  let states,_ as auto = Data_for_aorai.getAutomata() in
  let aux (impossible_states,possible_states,has_crossable_trans) state =
    let trans = Path_analysis.get_transitions_of_state state auto in
    let add_one_trans (_,has_crossable_trans as acc) trans =
      if tr_status.(trans.numt) then begin
        let guard =
          crosscond_to_pred ~event:(f,st) (fst trans.cross)
            (Data_for_aorai.get_logic_var Data_for_aorai.curOp)
            (Data_for_aorai.get_logic_var Data_for_aorai.curOpStatus)
        in
        true,Logic_const.por ~loc (has_crossable_trans,guard)
      end else acc
    in
    let is_possible_start, cond =
      List.fold_left add_one_trans (false,pfalse) trans
    in
    if is_possible_start then begin
      let start = is_state_pred state in
      let has_crossable_trans =
        if Logic_utils.is_trivially_true cond then has_crossable_trans
        else Logic_const.new_predicate
          (pimplies ~loc (start,cond)) :: has_crossable_trans
      in
      impossible_states,
      Logic_const.por ~loc (possible_states,start), has_crossable_trans
    end else begin
      let not_start = is_out_of_state_pred state in
      Logic_const.pand ~loc (impossible_states,not_start),
      possible_states, has_crossable_trans
    end
  in
  let impossible_states, possible_states, crossable_trans =
    List.fold_left aux (ptrue, pfalse,[]) states
  in
  let states =
    if Aorai_option.Deterministic.get() then
      possible_states (* We're always in exactly one state, among the possible
                         ones, no need to list the impossible ones.
                       *)
    else (* requires that the cells for impossible states be '0' *)
      Logic_const.pand ~loc (possible_states, impossible_states)
  in
  Logic_const.new_predicate states :: (List.rev crossable_trans)

let mk_action loc l =
  let mk_one_action (v,e) =
    Logic_const.prel ~loc
      (Req, Logic_const.term ~loc (TLval v) (Cil.typeOfTermLval v),
       Logic_const.told ~loc e)
  in
  List.map mk_one_action l

let find_pebble_origin lab actions =
  let rec aux = function
    | [] -> Aorai_option.fatal "Transition to multi-state has no pebble action"
    | Pebble_init (_,_,count) :: _ -> 
      Logic_const.term 
        (TLval (TVar count, TNoOffset)) 
        (Logic_const.make_set_type count.lv_type)
    | Pebble_move (_,_,set,_) :: _-> Data_for_aorai.pebble_set_at set lab
    | _ :: tl -> aux tl
  in aux actions

let mk_sub ~loc pebble_set v =
  let sub = List.hd (Logic_env.find_all_logic_functions "\\subset") in
  Logic_const.papp ~loc
    (sub,[],
     [Logic_const.term ~loc (TLval (TVar v,TNoOffset)) pebble_set.term_type;
      pebble_set])

let pebble_guard ~loc pebble_set aux_var guard =
  let v = Cil_const.make_logic_var aux_var.lv_name aux_var.lv_type in
  let g = rename_pred aux_var v guard in
  let g = Logic_const.pand ~loc (mk_sub ~loc pebble_set v, g) in
  Logic_const.pexists ~loc ([v], g)

let pebble_guard_neg ~loc pebble_set aux_var guard =
  let v = Cil_const.make_logic_var aux_var.lv_name aux_var.lv_type in
  let g = rename_pred aux_var v guard in
  let g = 
    Logic_const.pimplies ~loc 
      (mk_sub ~loc pebble_set v, Logic_const.pnot ~loc g)
  in
  Logic_const.pforall ~loc ([v], g)

let pebble_post ~loc pebble_set aux_var guard =
  let v = Cil_const.make_logic_var aux_var.lv_name aux_var.lv_type in
  let g = rename_pred aux_var v guard in
  let g = Logic_const.pimplies ~loc (mk_sub ~loc pebble_set v, g) in
  Logic_const.pforall ~loc ([v], g)

(* behavior is the list of all behaviors related to the given state, trans
   the list of potentially active transitions ending in this state.
   If the state is a multi-state, we have one behavior
   whose assumes is the disjunction of these assumes
*)
let add_behavior_pebble_actions ~loc event behaviors state trans =
  match state.multi_state with
    | None -> behaviors
    | Some (set,aux) ->
      let name = Printf.sprintf "pebble_%s" state.name in
      let assumes =
        List.fold_left
          (fun acc b ->
            let assumes = List.map pred_of_id_pred b.b_assumes in
            Logic_const.por ~loc (acc, Logic_const.pands assumes))
          pfalse behaviors
      in
      let assumes = [ Logic_const.new_predicate assumes ] in
      let set = Data_for_aorai.pebble_set_at set Logic_const.here_label in
      let treat_action guard res action =
        match action with
          | Copy_value _ | Counter_incr _ | Counter_init _ -> res
          | Pebble_init (_,_,v) ->
            let a = Cil_const.make_logic_var aux.lv_name aux.lv_type in
            let guard = rename_pred aux a guard in
            let guard = 
              Logic_const.pand ~loc
                (Logic_const.prel
                   ~loc (Req,Logic_const.tvar a,Logic_const.tvar v),
                 guard)
            in
            Logic_const.term ~loc
              (Tcomprehension (Logic_const.tvar a,[a], Some guard))
              set.term_type
            :: res
          | Pebble_move(_,_,s1,_) ->
            let a = Cil_const.make_logic_var aux.lv_name aux.lv_type in
            let guard = rename_pred aux a guard in
            let in_s = 
              mk_sub ~loc 
                (Data_for_aorai.pebble_set_at s1 Logic_const.pre_label) a
            in
            let guard = Logic_const.pand ~loc (in_s,guard) in
            Logic_const.term ~loc
              (Tcomprehension (Logic_const.tvar a,[a], Some guard))
              set.term_type
            :: res
      in
      let treat_one_trans acc tr =
        let guard = crosscond_to_pred ~event (fst tr.cross)
          (Data_for_aorai.get_logic_var Data_for_aorai.curOp)
          (Data_for_aorai.get_logic_var Data_for_aorai.curOpStatus)
        in
        let guard = Logic_const.pold guard in
        List.fold_left (treat_action guard) acc (snd tr.cross)
      in
      let res = List.fold_left treat_one_trans [] trans in
      let res = Logic_const.term (Tunion res) set.term_type in
      let post_cond =
        [ Normal, Logic_const.new_predicate (Logic_const.prel (Req,set,res))]
      in
      Cil.mk_behavior ~name ~assumes ~post_cond () :: behaviors

let mk_action ~loc a =
  let term_lval lv =
    Logic_const.term ~loc (TLval lv) (Cil.typeOfTermLval lv)
  in
  match a with
    | Counter_init lv ->
      [Logic_const.prel ~loc 
          (Req, term_lval lv, Logic_const.tinteger ~loc ~ikind:IInt 1)]
    | Counter_incr lv ->
      [Logic_const.prel ~loc
          (Req, term_lval lv, 
           Logic_const.term ~loc
             (TBinOp (PlusA,
                      Logic_const.told ~loc (term_lval lv),
                      Logic_const.tinteger ~loc ~ikind:IInt 1))
             (Cil.typeOfTermLval lv))]
    | Pebble_init _ | Pebble_move _ -> [] (* Treated elsewhere *)
    | Copy_value (lv,t) ->
      [Logic_const.prel ~loc
          (Req, term_lval lv, Logic_const.told t)]

let mk_behavior ~loc auto event (st_status,tr_status) state =
  Aorai_option.debug "analysis of state %s (%d out of %d)"
    state.Promelaast.name state.nums (Array.length st_status);
  if st_status.(state.nums) then begin
    Aorai_option.debug "state %s is reachable" state.Promelaast.name;
    let my_trans = Path_analysis.get_transitions_to_state state auto in
    let rec treat_trans ((in_assumes, out_assumes, action_bhvs) as acc) l =
      match l with
        | [] -> acc
        | trans :: tl ->
            let consider, others =
              List.partition (fun x -> x.start.nums = trans.start.nums) tl
            in
            let start = is_state_pred trans.start in
            let not_start = is_out_of_state_pred trans.start in
            let in_guard, out_guard, my_action_bhvs =
              List.fold_left
                (fun (in_guard, out_guard, action_bhvs) trans ->
                  Aorai_option.debug "examining transition %d (out of %d)"
                    trans.numt (Array.length tr_status);
                  let (cond,actions) = trans.cross in
                  Aorai_option.debug "transition %d is active" trans.numt;
                  let guard =
                    crosscond_to_pred ~event cond
                      (Data_for_aorai.get_logic_var Data_for_aorai.curOp)
                      (Data_for_aorai.get_logic_var Data_for_aorai.curOpStatus)
                  in
                  let my_in_guard,my_out_guard =
                    match state.multi_state with
                      | None -> guard, Logic_const.pnot ~loc guard
                      | Some (_,aux) ->
                        let set = 
                          find_pebble_origin Logic_const.here_label actions
                        in
                        pebble_guard ~loc set aux guard, 
                        pebble_guard_neg ~loc set aux guard
                  in
                  let out_guard =
                    Logic_const.pand ~loc (out_guard, my_out_guard)
                  in
                  let in_guard, action_bhvs =
                    match actions with
                      | [] -> 
                        (Logic_const.por ~loc (in_guard,my_in_guard),
                         action_bhvs)
                      | _ ->
                        let name = 
                          Printf.sprintf "buch_state_%s_in_%d" 
                            state.name (List.length action_bhvs)
                        in
                        Aorai_option.debug "Name is %s" name;
                        let assumes = [
                          Logic_const.new_predicate 
                            (Logic_const.pand ~loc (start,my_in_guard))
                        ] 
                        in
                        let post_cond = 
                          Normal, 
                          Logic_const.new_predicate (is_state_pred state)
                        in
                        let treat_one_action acc a =
                          let posts = mk_action ~loc a in
                          match state.multi_state  with
                            | None ->
                              acc @ 
                                List.map 
                                (fun x -> 
                                  (Normal, Logic_const.new_predicate x))
                                posts
                            | Some (_,aux) ->
                              let set =
                                find_pebble_origin 
                                  Logic_const.pre_label actions
                              in
                              acc @
                                List.map 
                                (fun x -> 
                                  (Normal, 
                                   Logic_const.new_predicate 
                                     (pebble_post ~loc set aux x)))
                                posts
                        in
                        let post_cond = 
                          List.fold_left treat_one_action [post_cond] actions
                        in
                        let bhv = 
                          Cil.mk_behavior ~name ~assumes ~post_cond ()
                        in
                        in_guard, bhv :: action_bhvs
                  in
                  in_guard, out_guard, action_bhvs)
                (pfalse,ptrue,action_bhvs) (trans::consider)
            in
            treat_trans
              (Logic_const.por ~loc
                 (in_assumes, (Logic_const.pand ~loc (start, in_guard))),
               Logic_const.pand ~loc
                 (out_assumes,
                  (Logic_const.por ~loc (not_start, out_guard))),
               my_action_bhvs
              )
              others
    in
    let my_trans = List.filter (fun x -> tr_status.(x.numt)) my_trans in
    let in_assumes, out_assumes, action_behaviors =
      treat_trans (pfalse, ptrue, []) my_trans
    in
    let behaviors =
      if Logic_utils.is_trivially_false in_assumes then action_behaviors
      else begin
        let behavior_in =
          Cil.mk_behavior
            ~name:(Printf.sprintf "buch_state_%s_in" state.Promelaast.name)
            ~assumes:[Logic_const.new_predicate in_assumes]
            ~post_cond:
            [Normal, Logic_const.new_predicate (is_state_pred state)]
            ()
        in behavior_in :: action_behaviors
      end
    in
    let behaviors =
      add_behavior_pebble_actions ~loc event behaviors state my_trans
    in
    let behaviors =
      if Logic_utils.is_trivially_false out_assumes then behaviors
      else begin
        let post_cond =
          match state.multi_state with
            | None -> []
            | Some (set,_) ->
              let set = 
                Data_for_aorai.pebble_set_at set Logic_const.here_label
              in [Normal,
                  Logic_const.new_predicate
                    (Logic_const.prel ~loc
                       (Req,set, 
                        Logic_const.term ~loc Tempty_set set.term_type))]
        in
        let post_cond =
          (Normal, (Logic_const.new_predicate (is_out_of_state_pred state)))
          :: post_cond
        in
        let behavior_out =
          Cil.mk_behavior
            ~name:(Printf.sprintf "buch_state_%s_out" state.Promelaast.name)
            ~assumes:[Logic_const.new_predicate out_assumes]
            ~post_cond ()
        in behavior_out :: behaviors
      end
    in
    List.rev behaviors
  end else begin
    Aorai_option.debug "state %s is not reachable" state.Promelaast.name;
    (* We know that we'll never end up in this state. *)
    let name = Printf.sprintf "buch_state_%s_out" state.Promelaast.name in
    let post_cond =
      match state.multi_state with
        | None -> []
        | Some (set,_) ->
          let set = 
            Data_for_aorai.pebble_set_at set Logic_const.here_label
          in [Normal,
              Logic_const.new_predicate
                (Logic_const.prel ~loc
                   (Req,set, 
                    Logic_const.term ~loc Tempty_set set.term_type))]
    in
    let post_cond =
      (Normal, Logic_const.new_predicate (is_out_of_state_pred state))
      ::post_cond
    in
    [mk_behavior ~name ~post_cond ()]
  end

let auto_func_behaviors loc f st (_st_status, tr_status as state) =
  let event = f,st in
  let call_or_ret = 
    match st with
      | Promelaast.Call -> "call" 
      | Promelaast.Return -> "return"
  in
  Aorai_option.debug
    "func behavior for %a (%s)" Kernel_function.pretty f call_or_ret;
  let (states, trans) as auto = Data_for_aorai.getAutomata() in
    (* requires is not needed for pre_func, as it is enforced by the
       requires of the original C function itself (and the call to pre_func
       by definition the first instruction of the function).
     *)
  let post_cond =
    let called_pre =
      Logic_const.new_predicate
        (Logic_const.prel ~loc
           (Req,
            Logic_const.tvar ~loc
              (Data_for_aorai.get_logic_var Data_for_aorai.curOpStatus),
            (Logic_utils.mk_dummy_term
               (TConst (Data_for_aorai.op_status_to_cenum st))
               Cil.intType)))
    in
    let called_pre_2 =
      Logic_const.new_predicate
        (Logic_const.prel ~loc
           (Req,
            Logic_const.tvar ~loc
              (Data_for_aorai.get_logic_var Data_for_aorai.curOp),
            (Logic_utils.mk_dummy_term
               (TConst(Data_for_aorai.func_to_cenum
                         (Kernel_function.get_name f))) Cil.intType)))
    in
      (* let old_pred = Aorai_utils.mk_old_state_pred loc in *)
    [(Normal, called_pre); (Normal, called_pre_2)]
  in  
  let requires =
    if st = Promelaast.Call then [] else force_transition loc f st state
  in
  let glob_assigns = aorai_assigns loc in
  let trans_assigns = 
    action_assigns (List.filter (fun x -> tr_status.(x.numt)) trans)
  in
  let assigns = Logic_utils.concat_assigns glob_assigns trans_assigns in
  let global_behavior =
    Cil.mk_behavior ~requires ~post_cond ~assigns ()
  in
  let mk_behavior acc st =
    mk_behavior ~loc auto event state st @ acc
  in
  global_behavior :: (List.fold_left mk_behavior [] states)

let get_preds_wrt_params transl f status =
  (* These two constants are never used, but are syntactically
     needed to call the conversion function *)
  let op = Cil_const.make_logic_var "_aorai_op" Linteger in
  let st = Cil_const.make_logic_var "_aorai_status" Linteger in
  let event = (f,status) in
  let preds = ref [] in
  Array.iteri
    (fun trn b ->
       if b then
         begin
           Aorai_option.debug "considering transition %d" trn;
           let trans = Data_for_aorai.getTransition trn in
           (* Gets the cross condition of the transition *)
           let dnf =
             snd (Logic_simplification.simplifyCond (fst trans.cross))
           in
           let cond = Logic_simplification.simplifyDNFwrtCtx dnf f status in
           let pred = crosscond_to_pred ~event cond op st  in
           let retrieve_state (st,_) = st.nums = trans.stop.nums in
           let oth_preds =
             try
               snd (List.find retrieve_state !preds)
             with Not_found -> pfalse
           in
           let pred = Logic_const.por (oth_preds,pred) in
           preds :=
             (trans.stop,pred) ::
             (List.filter (not $ retrieve_state) !preds)
         end
    )
    transl;
    let preds =
      List.map (fun (st, pred) -> pimplies (is_state_pred st, pred)) !preds
    in
    let pred = Logic_const.pands preds in
    if Logic_utils.is_trivially_true pred then None else Some pred

let get_preds_pre_wrt_params f =
  let (_,pre_tr) = Data_for_aorai.get_func_pre (Kernel_function.get_name f) in
  get_preds_wrt_params (pre_tr) f Promelaast.Call

let get_preds_post_bc_wrt_params f =
  let post = Data_for_aorai.get_func_post_bycase (Kernel_function.get_name f) in
  let (_,post_tr) = pre_flattening post in
  get_preds_wrt_params (post_tr) f Promelaast.Return

let force_condition_to_predicate global_inv restricted_inv =
  let pred_l = ref [] in
  let mk_pred_det term index = prel(Rneq, term, mk_int_term index) in
  let mk_pred_nondet base index =
    prel(Req, mk_offseted_array base index, zero_term())
  in
  let treat global restric mk_pred=
    Array.iteri
      (fun index value ->
         if (not value) && global.(index) then
           begin
             let n_pred = mk_pred index in
             pred_l:= n_pred::!pred_l
           end
      )
      restric
  in
  treat (fst global_inv) (fst restricted_inv)
    (if Aorai_option.Deterministic.get() then mk_pred_det (state_term())
     else mk_pred_nondet (host_state_term ()));
(*  treat (snd global_inv) (snd restricted_inv)
    (if Aorai_option.Deterministic.get() then mk_pred_det (trans_term())
     else mk_pred_nondet (host_trans_term ()));*)
  if !pred_l<>[] then
    pands (List.rev !pred_l)
  else
    ptrue

let treat_val loc pred (base, range) =
  let add term =
    if Cil.isLogicZero base then term
    else Logic_const.term 
      (TBinOp (PlusA, Logic_const.tat (base,Logic_const.pre_label), term)) 
      Linteger
  in
  let add_cst i = add (Logic_const.tinteger ~ikind:IInt i) in
  let res =
    match range with
      | Fixed i -> Logic_const.prel (Req,loc, add_cst i)
      | Interval(min,max) ->
        let min = Logic_const.prel (Rle, add_cst min, loc) in
        let max = Logic_const.prel (Rle, loc, add_cst max) in
        Logic_const.pand (min,max)
      | Bounded (min,max) ->
        let min = Logic_const.prel (Rle, add_cst min, loc) in
        let max = Logic_const.prel (Rle, loc, add max) in
        Logic_const.pand (min,max)
      | Unbounded min -> Logic_const.prel (Rle, add_cst min, loc)
  in
  Aorai_option.debug ~dkey:"action" "Action predicate: %a"
    !Ast_printer.d_predicate_named res;
  Logic_const.por(pred,res)

let update_to_pred post_state (location,vals) =
  let loc = Cil_datatype.Location.unknown in
  let intv = List.fold_left (treat_val location) Logic_const.pfalse vals in
  match post_state.multi_state with
      | None -> intv
      | Some(set,aux) ->
        (* [VP 2011-09-05] In fact, not all the pebble come from the considered
           pre-state. Will this lead to too strong post-conditions?
         *)
        let set = Data_for_aorai.pebble_set_at set Logic_const.here_label in
        pebble_post ~loc set aux intv

let action_to_pred ~pre_state ~post_state kf =
  let my_stmt = Kernel_function.find_return kf in
  let my_kinstr = Kstmt my_stmt in
  let updates =
    Data_for_aorai.get_action_path kf my_kinstr pre_state post_state
  in
  List.map (update_to_pred post_state) updates

let get_global_loop_inv stmt =
  double_bool_array_or
    (double_bool_array_or
       (Spec_tools.pre_flattening 
          (Data_for_aorai.get_loop_int_pre_bycase stmt))
       (Spec_tools.pre_flattening 
          (Data_for_aorai.get_loop_ext_pre_bycase stmt)))
    (Spec_tools.pre_flattening (Data_for_aorai.get_loop_int_post_bycase stmt))

let get_restricted_int_pre_bc stmt =
  let global_loop_inv = get_global_loop_inv stmt in
  force_condition_to_predicate
    global_loop_inv
    (Spec_tools.pre_flattening (Data_for_aorai.get_loop_int_pre_bycase stmt))

let get_restricted_ext_pre_bc stmt =
  let global_loop_inv = get_global_loop_inv stmt in
  force_condition_to_predicate
    global_loop_inv
    (Spec_tools.pre_flattening (Data_for_aorai.get_loop_ext_pre_bycase stmt))

let get_restricted_int_post_bc stmt =
  let global_loop_inv = get_global_loop_inv stmt in
  force_condition_to_predicate
    global_loop_inv
    (Spec_tools.pre_flattening (Data_for_aorai.get_loop_int_post_bycase stmt))

let rec display s =
  try
    let i=String.index s '\n' in
    if i=0 then
      begin
        Aorai_option.result " ";
        display (String.sub s 1 ((String.length s)-1))
      end
    else
      begin
        Aorai_option.result "%s" (String.sub s 0 i);
        if i+1=(String.length s) then
          Aorai_option.result " "
        else
          display (String.sub s (i+1) ((String.length s)-i-1))
      end
  with
      Not_found -> Aorai_option.result "%s" s

(** Intermediate function that factorizes some functionalities.
    This function is designed to be internally called. *)
let display_operations_spec__ (sorted:bool) (bycase:bool) =
  (* Aorai_option.result  *)
  display "\n########\n# Operations specification:\n#";
  let listOfNames = (Data_for_aorai.getFunctions_from_c ()) in
  let listOfNames = if sorted then List.sort (String.compare) listOfNames else listOfNames in
  List.iter
    (fun name ->
         let pre = Spec_tools.debug_display_stmt_all_pre (Data_for_aorai.get_func_pre ~securised:true name) in
         let post = if bycase then
           Spec_tools.debug_display_stmt_all_pre_bycase (Data_for_aorai.get_func_post_bycase ~securised:true name)
         else
           Spec_tools.debug_display_stmt_all_pre (Data_for_aorai.get_func_post ~securised:true name)
         in
         Aorai_option.result "#   %s  %s  %s" pre name post;
         Aorai_option.result "\n"
    )
    listOfNames;
  let ignFuncs=List.fold_left
    (fun ls s -> (ls^" "^s))
    ""
    (List.sort (String.compare) (Data_for_aorai.getIgnoredFunctions()))
  in
  display ("#\n#   Ignored functions: { "^ignFuncs^" }");
  display "#\n# End of operations specification\n########"



(** Some displaying functions *)
let display_operations_spec () =
 display_operations_spec__ false false

let display_operations_spec_bycase () =
  display_operations_spec__ false true

let display_operations_spec_sorted () =
  display_operations_spec__ true false

let display_operations_spec_sorted_bycase () =
  display_operations_spec__ true true



let debug_display_all_specs () =
  (* Step 1 : Displaying operations specification *)
  display_operations_spec_sorted_bycase ();


  (* Step 2 : Displaying loops specification *)
  (* Aorai_option.result *)
  display "\n########\n# Loops specification:\n#";
  let sortedLoopsIndex =
    List.sort (fun r1 r2 ->
                        if r1.sid > r2.sid then 1
                 else if r1.sid < r2.sid then -1
                 else 0
              ) (Data_for_aorai.get_loops_index ())
  in
  List.iter
    (fun stmt ->
       Aorai_option.result "#   stmt.sid=%d" stmt.sid;
       Aorai_option.result "#      loop pres  : %s"
         (Spec_tools.debug_display_stmt_all_pre 
            (Data_for_aorai.get_loop_ext_pre stmt));
       Aorai_option.result "#                   %s"
         (Spec_tools.debug_display_stmt_all_pre_bycase
            (Data_for_aorai.get_loop_ext_pre_bycase stmt));

       Aorai_option.result "#      block pres : %s"
         (Spec_tools.debug_display_stmt_all_pre 
            (Data_for_aorai.get_loop_int_pre stmt));
       Aorai_option.result "#                   %s"
         (Spec_tools.debug_display_stmt_all_pre_bycase 
            (Data_for_aorai.get_loop_int_pre_bycase stmt));

       Aorai_option.result "#      block posts: %s"
         (Spec_tools.debug_display_stmt_all_pre 
            (Data_for_aorai.get_loop_int_post stmt));
       Aorai_option.result "#                   %s"
         (Spec_tools.debug_display_stmt_all_pre_bycase 
            (Data_for_aorai.get_loop_int_post_bycase stmt));

       Aorai_option.result "#      loop posts : %s"
         (Spec_tools.debug_display_stmt_all_pre 
            (Data_for_aorai.get_loop_ext_post stmt));
       Aorai_option.result "#                   %s"
         (Spec_tools.debug_display_stmt_all_pre_bycase 
            (Data_for_aorai.get_loop_ext_post_bycase stmt));
    )
    sortedLoopsIndex;
  (* Aorai_option.result  *)
  display "# End of loops specification\n########\n"

let pasEtatOp pos op =
  Aorai_option.warning 
    "No state can be enabled %s operation '%s'. \
     It can be an error of the analyzed C program." pos op
let pasEtatAvantOp op = pasEtatOp "before" op
let pasEtatApresOp op = pasEtatOp "after" op

let pasEtatLoop pos quid id = Aorai_option.warning "No state can be enabled %s the %s of the loop '%d'. It can be an error of the analyzed C program." pos quid id
let pasEtatAvantLoop id = pasEtatLoop "before" "whole loop" id
let pasEtatApresLoop id = pasEtatLoop "after" "whole loop" id
let pasEtatAvantLoopBlock id = pasEtatLoop "before" "internal block" id
let pasEtatApresLoopBlock id = pasEtatLoop "after" "internal block" id




let display_all_warnings_about_operations_specs() =
  (* Aorai_option.result  *)
  let listOfNames = (Data_for_aorai.getFunctions_from_c ()) in
  let listOfNames = List.sort (String.compare) listOfNames in
  List.iter
    (fun name ->
         let pre =
           Data_for_aorai.get_func_pre ~securised:true name
         in
         let post =
           Data_for_aorai.get_func_post_bycase ~securised:true name
         in
         if is_empty_pre_post pre then pasEtatAvantOp name;
         if is_empty_post_bc post then pasEtatApresOp name
    )
    listOfNames;

  if(List.length (Data_for_aorai.getIgnoredFunctions())) >0 then
    let ignFuncs=List.fold_left
      (fun ls s -> (ls^" "^s))
      ""
      (List.sort (String.compare) (Data_for_aorai.getIgnoredFunctions()))
    in
    display ("Ignored functions: { "^ignFuncs^" }")

let display_all_warnings_about_loops_specs() =
  let sortedLoopsIndex =
    List.sort
      (fun r1 r2 ->
          if r1.sid > r2.sid then 1
        else if r1.sid < r2.sid then -1
        else 0)
      (Data_for_aorai.get_loops_index ())
  in
  List.iter
    (fun stmt ->
      if is_empty_pre_post (Data_for_aorai.get_loop_ext_pre stmt) &&
          is_empty_post_bc (Data_for_aorai.get_loop_ext_pre_bycase stmt)
      then pasEtatAvantLoop stmt.sid;
      if is_empty_pre_post (Data_for_aorai.get_loop_ext_post stmt) &&
          is_empty_post_bc  (Data_for_aorai.get_loop_ext_post_bycase stmt)
      then pasEtatApresLoop stmt.sid;
      if is_empty_pre_post (Data_for_aorai.get_loop_int_pre stmt) &&
          is_empty_post_bc  (Data_for_aorai.get_loop_int_pre_bycase stmt)
      then pasEtatAvantLoopBlock stmt.sid;
      if is_empty_pre_post (Data_for_aorai.get_loop_int_post stmt) &&
          is_empty_post_bc  (Data_for_aorai.get_loop_int_post_bycase stmt)
      then pasEtatApresLoopBlock stmt.sid)
    sortedLoopsIndex

let display_all_warnings_about_specs () =
  display_all_warnings_about_operations_specs ();
  display_all_warnings_about_loops_specs ()

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

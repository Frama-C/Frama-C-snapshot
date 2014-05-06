(**************************************************************************)
(*                                                                        *)
(*  This file is part of Aorai plug-in of Frama-C.                        *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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
open Cil_types
open Cil_datatype
open Promelaast
open Bool3


let rename_pred v1 v2 p =
  let r =
  object
    inherit Visitor.frama_c_copy (Project.current())
    method! vlogic_var_use v =
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
    State_builder.Int_hashtbl
      (Cil_datatype.Enumitem)
      (struct
         let name = "ltl_states_enum"
         let size = 17
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
        if test then Cil.lconstant Integer.one else Cil.lzero ()
      in
      let bool3_res dft test =
        match test with
          | True -> bool_res true
          | False -> bool_res false
          | Undefined -> dft
      in
      let is_true t =
        match t with
          | TConst(Integer(i,_)) ->
            Bool3.bool3_of_bool (not (Integer.is_zero i))
          | TConst(LChr c) -> Bool3.bool3_of_bool (not (Char.code c <> 0))
          | TConst(LReal r) -> Bool3.bool3_of_bool (not (r.r_nearest <> 0.))
          | TConst(LStr _ | LWStr _) -> Bool3.True
          | _ -> Bool3.Undefined
      in
      let rec aux t =
        match t.term_node with
          | TConst (LEnum ei) ->
            aux (Logic_utils.expr_to_term ~cast:false ei.eival)
          | TLval lv ->
            (match aux_lv lv with
              | Some t -> t
              | None -> t)
          | TUnOp(op,t1) ->
            let t1 = aux t1 in
            (match op,t1.term_node with
               | Neg, TConst(Integer(i,_)) ->
                   { t with term_node = TConst(Integer(Integer.neg i,None)) }
               | Neg, TConst(LReal r) ->
		   let f = ~-. (r.r_nearest) in
		   let r = { 
		     r_literal = string_of_float f ;
		     r_nearest = f ;
		     r_upper = ~-. (r.r_lower) ;
		     r_lower = ~-. (r.r_upper) ;
		   } in
		   { t with term_node = TConst(LReal r) }
               | LNot, t1 ->  bool3_res t (is_true t1)
              | _ -> t)
          | TBinOp(op,t1,t2) ->
            let t1 = aux t1 in
            let t2 = aux t2 in
            let rec comparison comp t1 t2 =
              match t1.term_node,t2.term_node with
                | TConst (Integer(i1,_)), TConst (Integer(i2,_)) ->
                  bool_res (comp (Integer.compare i1 i2))
                | TConst (LChr c1), TConst (LChr c2) ->
                  bool_res (comp (Char.compare c1 c2))
                | TConst(LReal r1), TConst (LReal r2) ->
                  bool_res (comp (compare r1.r_nearest r2.r_nearest))
                | TCastE(ty1,t1), TCastE(ty2,t2)
                  when Cil_datatype.Typ.equal ty1 ty2 ->
                  comparison comp t1 t2
                | _ -> t
            in
            (match op, t1.term_node, t2.term_node with

              | PlusA, TConst(Integer(i1,_)), TConst(Integer(i2,_)) ->
                { t with term_node =
                    TConst(Integer(Integer.add i1 i2,None))}
              | MinusA, TConst(Integer(i1,_)), TConst(Integer(i2,_)) ->
                { t with term_node =
                    TConst(Integer(Integer.sub i1 i2,None)) }
              | Mult, TConst(Integer(i1,_)), TConst(Integer(i2,_)) ->
                { t with term_node =
                    TConst(Integer(Integer.mul i1 i2,None)) }
              | Div, TConst(Integer(i1,_)), TConst(Integer(i2,_)) ->
                (try
                   { t with term_node =
                       TConst(Integer(Integer.c_div i1 i2,None)) }
                 with Division_by_zero -> t)
              | Mod, TConst(Integer(i1,_)), TConst(Integer(i2,_)) ->
                (try
                   { t with term_node =
                       TConst(Integer(Integer.c_rem i1 i2,None)) }
                 with Division_by_zero -> t)
              | Shiftlt, TConst(Integer(i1,_)), TConst(Integer(i2,_)) ->
                { t with term_node =
                    TConst(Integer(Integer.shift_left i1 i2,None)) }
              | Shiftrt, TConst(Integer(i1,_)), TConst(Integer(i2,_)) ->
                { t with term_node =
                    TConst(Integer(Integer.shift_right i1 i2,None)) }
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
              | TAddrOf lv -> aux_lv (Logic_const.addTermOffsetLval off lv)
              | _ -> None)
          | TResult _ -> None
      and aux_init off initinfo =
        match off, initinfo with
          | TNoOffset, SingleInit e ->
            Some (aux (Logic_utils.expr_to_term ~cast:false e))
          | TIndex(t,oth), CompoundInit (ct,initl) ->
            (match (aux t).term_node with
              | TConst(Integer(i1,_)) ->
                Cil.foldLeftCompound ~implicit:true
                  ~doinit:
                  (fun o i _ t ->
                    match o with
                      | Index({ enode = Const(CInt64(i2,_,_))},_)
                          when Integer.equal i1 i2 -> aux_init oth i
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
        | TConst (Integer(i1,_)), TConst (Integer(i2,_)) ->
          Bool3.bool3_of_bool (comp (Integer.compare i1 i2))
        | TConst (LChr c1), TConst (LChr c2) ->
          Bool3.bool3_of_bool (comp (Char.compare c1 c2))
        | TConst(LReal r1), TConst (LReal r2) ->
          Bool3.bool3_of_bool (comp (compare r1.r_nearest r2.r_nearest))
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
    (Const(CInt64(Integer.of_int value,IInt,Some(string_of_int value))))


(** This function rewrites a cross condition into an ACSL expression.
    Moreover, by giving current operation name and its status (call or
    return) the generation simplifies the generated expression.
 *)
let crosscond_to_pred cross curr_f curr_status =
  let check_current_event f status pred =
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
  file := f;
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
  let before, after =
    List.fold_left
      (fun (b,a) elem ->
        match elem with
        | GFun(f,loc) as func ->
          (* [VP] if address of function is taken, it might be
             used in a global initializer: keep a declaration at this point
             to ensure ending up with a compilable C file in the end... *)
          let b =
            if f.svar.vaddrof then GVarDecl(Cil.empty_funspec(),f.svar,loc) :: b
            else b
          in
          b, func :: a
        | other -> other :: b, a)
      ([], [])
      !file.globals
  in
  !file.globals <- List.rev before @ List.rev !globals_queue @ List.rev after;
  Kernel_function.clear_sid_info ();
  globals_queue := []

let mk_global glob = globals_queue := glob :: !globals_queue

(* Utilities for global variables *)
let mk_global_c_initialized_vars name ty ini=
  let vi = (Cil.makeGlobalVar name ty) in
    vi.vghost<-true;
    mk_global (GVar(vi,ini,vi.vdecl));
    Globals.Vars.add vi ini;
    set_varinfo name vi

let mk_global_var_init vi ini =
  vi.vghost<-true;
  mk_global (GVar(vi,ini,vi.vdecl));
  Globals.Vars.add vi ini;
  set_varinfo vi.vname vi

let mk_global_var vi =
  let ini =
    {Cil_types.init=Some(Cil.makeZeroInit ~loc:(CurrentLoc.get()) vi.vtype)}
  in
  mk_global_var_init vi ini

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
         Integer.of_int (value),
         IInt,
         Some(string_of_int(value))
       )))


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


let mk_global_c_initialized_enum name name_enuminfo ini =
  mk_global_c_initialized_vars name (TEnum(get_usedinfo name_enuminfo,[])) ini



(* ************************************************************************* *)
(** {b Terms management / computation} *)

(** Return an integer constant term from the given value. *)
let mk_int_term value = Cil.lconstant (Integer.of_int value)

(** Return an integer constant term with the 0 value.
    @deprecated use directly Cil.lzero
*)
let zero_term() = Cil.lzero ()

let one_term () = Cil.lconstant Integer.one

(** Returns a term representing the variable associated to the given varinfo *)
let mk_term_from_vi vi =
  Logic_const.term
    (TLval((Logic_utils.lval_to_term_lval ~cast:true (Cil.var vi))))
    (Ctype Cil.intType)

(** Given an lval term 'host' and an integer value 'off', it returns a lval term host[off]. *)
let mk_offseted_array host off =
  Logic_const.term
    (TLval(Logic_const.addTermOffsetLval (TIndex(mk_int_term (off),TNoOffset)) host))
    (Ctype Cil.intType)

let int2enumstate nums =
  let enum = find_enum nums in
  Logic_const.term (TConst (LEnum enum)) (Ctype (TEnum (enum.eihost,[])))

(** Given an lval term 'host' and an integer value 'off', it returns a lval term host[off]. *)
let mk_offseted_array_states_as_enum host off =
  let enum = find_enum off in
  Logic_const.term
    (TLval
       (Logic_const.addTermOffsetLval
          (TIndex(Logic_const.term
                    (TConst(LEnum enum)) (Ctype (TEnum (enum.eihost,[]))),
                  TNoOffset))
          host))
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
    Logic_const.prel 
      (Req,one_term(), 
       Logic_const.tvar (Data_for_aorai.get_state_logic_var state))

let is_out_of_state_pred state =
  if Aorai_option.Deterministic.get () then
    Logic_const.prel (Rneq,state_term(),int2enumstate state.nums)
  else
    Logic_const.prel 
      (Req,zero_term(), 
       Logic_const.tvar (Data_for_aorai.get_state_logic_var state))


(* Utilities for other globals *)

let mk_global_comment txt = mk_global (GText (txt))


(* ************************************************************************* *)
(** {b Initialization management / computation} *)

let mk_global_states_init root =
  let (states,_ as auto) = Data_for_aorai.getAutomata () in
  let states = List.sort Data_for_aorai.Aorai_state.compare states in
  let is_possible_init state =
    state.Promelaast.init = Bool3.True &&
    (let trans = Path_analysis.get_transitions_of_state state auto in
     List.exists (fun tr -> isCrossableAtInit tr root) trans)
  in
  List.iter
    (fun state ->
        let init =
          if is_possible_init state then mk_int_exp 1 else mk_int_exp 0
        in
        let init = SingleInit init in
        let var = Data_for_aorai.get_state_var state in
        mk_global_var_init var { Cil_types.init = Some init})
    states

let func_to_init name =
  {Cil_types.init=
      Some(SingleInit(
        new_exp ~loc:(CurrentLoc.get()) (Const(func_to_cenum (name)))))}

let funcStatus_to_init st =
  {Cil_types.init=Some(SingleInit(new_exp ~loc:(CurrentLoc.get())
                                    (Const(op_status_to_cenum (st)))))}

class visit_decl_loops_init () =
object(self)
  inherit Visitor.frama_c_inplace

  method! vstmt_aux stmt =
    begin
      match stmt.skind with
        | Loop _ ->
          let scope = Kernel_function.find_enclosing_block stmt in
          let f = Extlib.the self#current_func in
          let name = Data_for_aorai.loopInit ^ "_" ^ (string_of_int stmt.sid) in
          let var =
            Cil.makeLocalVar f ~scope ~generated:true name Cil.intType
          in
          Data_for_aorai.set_varinfo name var
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

      method! vterm t =
        match t.term_node with
            TLval (TVar { lv_origin = Some v},_) when v.vglob -> add_label t
          | TLval (TMem _,_) -> add_label t
          | _ -> DoChildren

      method! vterm_lhost = function
        | TResult ty ->
          (match kf with
              None -> Aorai_option.fatal
                "found \\result without being at a Return event"
            | Some kf ->
              (try
                 ChangeTo (TVar (Kernel_function.Hashtbl.find subst_res kf))
               with Not_found ->
                 let new_lv =
                   Cil_const.make_logic_var_quant
                     ("__retres_" ^ (Kernel_function.get_name kf)) (Ctype ty)
                 in
                 Kernel_function.Hashtbl.add subst_res kf new_lv;
                 ChangeTo (TVar new_lv)))
        | TMem _ | TVar _ -> DoChildren

      method! vlogic_var_use lv =
        match lv.lv_origin with
          | Some v when not v.vglob ->
            (try
               ChangeTo (Cil_datatype.Logic_var.Hashtbl.find subst lv)
             with Not_found ->
               let new_lv =
                 Cil_const.make_logic_var_quant lv.lv_name lv.lv_type
               in
               Cil_datatype.Logic_var.Hashtbl.add subst lv new_lv;
               ChangeTo new_lv)
          | Some _ | None -> DoChildren
    end
  in Visitor.visitFramacPredicate visitor pred

let pred_of_condition subst subst_res label cond =
  let mk_func_event f =
    let op = tat (mk_term_from_vi (get_varinfo curOp),label) in
      (* [VP] TODO: change int to appropriate enum type. Also true
         elsewhere.
       *)
    let f = 
      term 
	(TConst (constant_to_lconstant (func_to_cenum f)))
	(Ctype (func_enum_type ())) 
    in
    prel (Req,op,f)
  in
  let mk_func_status f status =
    let curr = tat (mk_term_from_vi (get_varinfo curOpStatus),label) in
    let call =
      term 
	(TConst (constant_to_lconstant (op_status_to_cenum status)))
	(Ctype (status_enum_type()))
    in
    Logic_const.pand (mk_func_event f, prel(Req,curr,call))
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
    Annotations.add_global Aorai_option.emitter lemma
  in
  List.iter make_one_lemma (fst automaton)

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
  if Aorai_option.Deterministic.get () then make_enum_states ();
  (* non deterministic mode uses one variable for each possible state *)
  mk_global_c_enum_type
    listOp (List.map (fun e -> func_to_op_func e) (getFunctions_from_c()));
  mk_global_c_initialized_enum curOp listOp
    (func_to_init (Kernel_function.get_name root));
  mk_global_c_enum_type  listStatus (callStatus::[termStatus]);
  mk_global_c_initialized_enum
    curOpStatus listStatus (funcStatus_to_init Promelaast.Call);

  mk_global_comment "//* ";
  mk_global_comment "//* States and Trans Variables";
  if Aorai_option.Deterministic.get () then
    mk_global_c_var_init curState (getInitialState())
  else
    mk_global_states_init root;

  if complete then begin
    mk_global_comment "//* ";
    mk_global_comment "//* Loops management";
    mk_decl_loops_init ();
  end;

  mk_global_comment "//* ";
  mk_global_comment "//****************** ";
  mk_global_comment "//* Auxiliary variables used in transition conditions";
  mk_global_comment "//*";
  List.iter mk_global_var (Data_for_aorai.aux_variables());

  if Aorai_option.Deterministic.get () then begin
    (* must flush now previous globals which are used in the lemmas in order to
       be able to put these last ones in the right places in the AST. *)
    flush_globals ();
    mk_deterministic_lemma ();
  end;

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
      Annotations.add_global Aorai_option.emitter annot);
  mk_global_comment "//* ";
  mk_global_comment "//* END Primitives generated for LTL verification";
  mk_global_comment "//****************";

  flush_globals ()

(* ************************************************************************* *)
(** {b Pre/post management} *)

let automaton_locations loc =
  let auto_state =
    if Aorai_option.Deterministic.get () then
      [ Logic_const.new_identified_term (state_term()), FromAny ]
    else
      List.map
        (fun state ->
          Logic_const.new_identified_term 
            (Logic_const.tvar 
               (Data_for_aorai.get_state_logic_var state)), FromAny)
        (fst (Data_for_aorai.getAutomata()))
  in
  (Logic_const.new_identified_term
     (Logic_const.tvar ~loc
        (Data_for_aorai.get_logic_var Data_for_aorai.curOpStatus)),
   FromAny) ::
    (Logic_const.new_identified_term
       (Logic_const.tvar ~loc
          (Data_for_aorai.get_logic_var Data_for_aorai.curOp)),
     FromAny) ::
    auto_state

let automaton_assigns loc = Writes (automaton_locations loc)

let aorai_assigns state loc =
  let merged_states =
    Aorai_state.Map.fold
      (fun _ state acc -> Data_for_aorai.merge_end_state state acc)
      state Aorai_state.Map.empty
  in
  let bindings =
    Aorai_state.Map.fold
      (fun _ (_,_,b) acc -> Data_for_aorai.merge_bindings b acc)
      merged_states Cil_datatype.Term.Map.empty
  in
  let elements = 
    Cil_datatype.Term.Map.fold
      (fun t _ acc -> (Logic_const.new_identified_term t,FromAny)::acc)
      bindings []
  in
  Writes (automaton_locations loc @ elements)

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
            | TField _ | TModel _ ->
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
  let empty = (Cil_datatype.Varinfo.Set.empty,[]) in
  let empty_pebble =
    match trans.start.multi_state, trans.stop.multi_state with
      | Some(_,aux), None ->
        let caux = Extlib.the aux.lv_origin in
        add_if_needed caux (Logic_const.tvar aux) empty
      | _ -> empty
  in
  let _,res = List.fold_left treat_one_action empty_pebble (snd trans.cross) in
  Writes res

let get_reachable_trans state st auto current_state =
  match st with
    | Promelaast.Call ->
      (try
         let reach = Data_for_aorai.Aorai_state.Map.find state current_state in
         let treat_one_state end_state _ l =
           Path_analysis.get_edges state end_state auto @ l
         in
         Data_for_aorai.Aorai_state.Map.fold treat_one_state reach []
       with Not_found -> [])
    | Promelaast.Return ->
      let treat_one_state end_state (_,last,_) l =
        if Data_for_aorai.Aorai_state.Set.mem state last then
            Path_analysis.get_edges state end_state auto @ l
        else l
      in
      let treat_one_start _ map l =
       Data_for_aorai.Aorai_state.Map.fold treat_one_state map l
      in
      Data_for_aorai.Aorai_state.Map.fold treat_one_start current_state []

let get_reachable_trans_to state st auto current_state =
  match st with
    | Promelaast.Call ->
      let treat_one_start start map acc =
        if Data_for_aorai.Aorai_state.Map.mem state map then
          Path_analysis.get_edges start state auto @ acc
        else acc
      in
      Data_for_aorai.Aorai_state.Map.fold treat_one_start current_state []
    | Promelaast.Return ->
      let treat_one_state _ map acc =
        try
          let (_,last,_) = Data_for_aorai.Aorai_state.Map.find state map in
          Data_for_aorai.Aorai_state.Set.fold
            (fun start acc -> Path_analysis.get_edges start state auto @ acc)
            last acc
        with Not_found -> acc
      in Data_for_aorai.Aorai_state.Map.fold treat_one_state current_state []

(* force that we have a crossable transition for each state in which the
   automaton might be at current event. *)
let force_transition loc f st current_state =
  let (states, _ as auto) = Data_for_aorai.getAutomata () in
  let aux (impossible_states,possible_states,has_crossable_trans) state =
    let reachable_trans = get_reachable_trans state st auto current_state in
    let add_one_trans (has_crossable_trans, crossable_non_reject) trans =
      let has_crossable_trans = 
        Logic_simplification.tor has_crossable_trans (fst trans.cross)
      in
      let crossable_non_reject = 
        crossable_non_reject ||
          (isCrossable trans f st 
           && not (Data_for_aorai.is_reject_state trans.stop))
      in has_crossable_trans, crossable_non_reject
    in
    let cond, crossable_non_reject =
      List.fold_left add_one_trans (Promelaast.TFalse, false) reachable_trans
    in
    let cond = fst (Logic_simplification.simplifyCond cond) in
    let cond = crosscond_to_pred cond f st in
    let start = is_state_pred state in
    if Logic_utils.is_trivially_false cond then begin
      let not_start = is_out_of_state_pred state in
      Logic_const.pand ~loc (impossible_states,not_start),
      possible_states, has_crossable_trans
    end else begin
      let has_crossable_trans =
        if Logic_utils.is_trivially_true cond then has_crossable_trans
        else
          Logic_const.new_predicate
            (pimplies ~loc (start,cond)) :: has_crossable_trans
      in
      let possible_states =
        (* reject_state must not be the only possible state *)
        match st with
          | Promelaast.Return ->
            if Data_for_aorai.is_reject_state state then possible_states
            else  Logic_const.por ~loc (possible_states,start)
          | Promelaast.Call ->
            if crossable_non_reject then
              Logic_const.por ~loc (possible_states, start)
            else possible_states
      in
      impossible_states, possible_states, has_crossable_trans
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

let partition_action trans =
  let add_state t st map =
    let old = 
      try Cil_datatype.Term_lval.Map.find t map
      with Not_found -> Data_for_aorai.Aorai_state.Set.empty
    in
    let new_set = Data_for_aorai.Aorai_state.Set.add st old in
    Cil_datatype.Term_lval.Map.add t new_set map
  in
  let treat_one_action st acc =
    function
      | Counter_init t | Counter_incr t | Copy_value (t,_) -> add_state t st acc
      | Pebble_init _ | Pebble_move _ -> acc (* moving pebbles can occur at
                                                the same time (but not for
                                                same pebbles)
                                              *)
  in
  let treat_one_trans acc tr =
    List.fold_left (treat_one_action tr.start) acc (snd tr.cross)
  in
  List.fold_left treat_one_trans Cil_datatype.Term_lval.Map.empty trans

(* TODO: this must be refined to take pebbles into account: in that
   case, disjointness condition is on pebble set for each state. *)
let disjoint_states loc _ states precond =
  let states = Data_for_aorai.Aorai_state.Set.elements states in
  let rec product acc l =
    match l with
      | [] -> acc
      | hd::tl ->
        let pairs = List.map (fun x -> (hd,x)) tl in
        product (pairs @ acc) tl
  in
  let disjoint = product [] states in
  List.fold_left
    (fun acc (st1, st2) ->
      Logic_const.new_predicate
        (Logic_const.por ~loc 
           (is_out_of_state_pred st1,is_out_of_state_pred st2)) :: acc)
    precond
    disjoint

(* 
forces that parent states of a state with action are mutually exclusive,
at least at pebble level.
*)
let incompatible_states loc st current_state =
  let (states,_ as auto) = Data_for_aorai.getAutomata () in
  let aux precond state =
    let trans = get_reachable_trans_to state st auto current_state in
    let actions = partition_action trans in
    Cil_datatype.Term_lval.Map.fold (disjoint_states loc) actions precond
  in
  List.fold_left aux [] states

let auto_func_preconditions loc f st current_state =
  force_transition loc f st current_state @ 
    incompatible_states loc st current_state


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
  let v = Cil_const.make_logic_var_quant aux_var.lv_name aux_var.lv_type in
  let g = rename_pred aux_var v guard in
  let g = Logic_const.pand ~loc (mk_sub ~loc pebble_set v, g) in
  Logic_const.pexists ~loc ([v], g)

let pebble_guard_neg ~loc pebble_set aux_var guard =
  let v = Cil_const.make_logic_var_quant aux_var.lv_name aux_var.lv_type in
  let g = rename_pred aux_var v guard in
  let g =
    Logic_const.pimplies ~loc
      (mk_sub ~loc pebble_set v, Logic_const.pnot ~loc g)
  in
  Logic_const.pforall ~loc ([v], g)

let pebble_post ~loc pebble_set aux_var guard =
  let v = Cil_const.make_logic_var_quant aux_var.lv_name aux_var.lv_type in
  let g = rename_pred aux_var v guard in
  let g = Logic_const.pimplies ~loc (mk_sub ~loc pebble_set v, g) in
  Logic_const.pforall ~loc ([v], g)

(* behavior is the list of all behaviors related to the given state, trans
   the list of potentially active transitions ending in this state.
   If the state is a multi-state, we have one behavior
   whose assumes is the disjunction of these assumes
*)
let add_behavior_pebble_actions ~loc f st behaviors state trans =
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
            let a = Cil_const.make_logic_var_quant aux.lv_name aux.lv_type in
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
            let a = Cil_const.make_logic_var_quant aux.lv_name aux.lv_type in
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
        let guard = crosscond_to_pred (fst tr.cross) f st in
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
          (Req, term_lval lv, Logic_const.tinteger ~loc 1)]
    | Counter_incr lv ->
      [Logic_const.prel ~loc
          (Req, term_lval lv,
           Logic_const.term ~loc
             (TBinOp (PlusA,
                      Logic_const.told ~loc (term_lval lv),
                      Logic_const.tinteger ~loc 1))
             (Cil.typeOfTermLval lv))]
    | Pebble_init _ | Pebble_move _ -> [] (* Treated elsewhere *)
    | Copy_value (lv,t) ->
      [Logic_const.prel ~loc
          (Req, term_lval lv, Logic_const.told t)]

let is_reachable state status =
  let treat_one_state _ map = Data_for_aorai.Aorai_state.Map.mem state map in
  Data_for_aorai.Aorai_state.Map.exists treat_one_state status

let concat_assigns a1 a2 =
  match a1,a2 with
    | WritesAny, _ -> a2
    | _, WritesAny -> a1
    | Writes l1, Writes l2 ->
      Writes
        (List.fold_left
           (fun acc (loc,_ as elt) ->
             if List.exists
               (fun (x,_) ->
                 Cil_datatype.Term.equal x.it_content loc.it_content)
               l2
             then
               acc
             else
               elt :: acc)
           l2 l1)

let get_accessible_transitions auto state status =
  let treat_one_state curr_state (_,last,_) acc =
    if Data_for_aorai.Aorai_state.equal curr_state state then
      Data_for_aorai.Aorai_state.Set.union last acc
    else acc
  in
  let treat_start_state _ map acc =
    Data_for_aorai.Aorai_state.Map.fold treat_one_state map acc
  in
  let previous_set =
    Data_for_aorai.Aorai_state.Map.fold 
      treat_start_state status Data_for_aorai.Aorai_state.Set.empty
  in
  Data_for_aorai.Aorai_state.Set.fold
    (fun s acc -> Path_analysis.get_edges s state auto @ acc) previous_set []

(* Assumes that we don't have a multi-state here.
   pebbles are handled elsewhere
 *)
let mk_unchanged_aux_vars trans =
  let my_aux_vars = Cil_datatype.Term_lval.Set.empty in
  let add_one_action acc = function
    | Counter_init lv | Counter_incr lv | Copy_value (lv,_) ->
      Cil_datatype.Term_lval.Set.add lv acc
    | Pebble_init _ | Pebble_move _ -> acc
  in
  let add_one_trans acc tr = 
    let (_,actions) = tr.cross in
    List.fold_left add_one_action acc actions
  in
  let my_aux_vars = List.fold_left add_one_trans my_aux_vars trans in
  let treat_lval lv acc =
    let t = Data_for_aorai.tlval lv in
    let ot = Logic_const.told t in
    let p = Logic_const.prel (Req,t,ot) in
    (Normal, Logic_const.new_predicate p) :: acc
  in
  Cil_datatype.Term_lval.Set.fold treat_lval my_aux_vars []

let mk_behavior ~loc auto kf e status state =
  Aorai_option.debug "analysis of state %s (%d)"
    state.Promelaast.name state.nums;
  if is_reachable state status then begin
    Aorai_option.debug "state %s is reachable" state.Promelaast.name;
    let my_trans = get_accessible_transitions auto state status in
    let rec treat_trans 
        ((in_assumes, out_assumes, assigns, action_bhvs) as acc) l =
      match l with
        | [] -> acc
        | trans :: tl ->
            let consider, others =
              List.partition (fun x -> x.start.nums = trans.start.nums) tl
            in
            let start = is_state_pred trans.start in
            let not_start = is_out_of_state_pred trans.start in
            let in_guard, out_guard, assigns, my_action_bhvs =
              List.fold_left
                (fun (in_guard, out_guard, all_assigns, action_bhvs) trans ->
                  Aorai_option.debug "examining transition %d" trans.numt;
                  let (cond,actions) = trans.cross in
                  Aorai_option.debug "transition %d is active" trans.numt;
                  let guard = crosscond_to_pred cond kf e in
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
                  let in_guard, all_assigns, action_bhvs =
                    match actions with
                      | [] ->
                        (Logic_const.por ~loc (in_guard,my_in_guard),
                         all_assigns,
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
                        let assigns = action_assigns trans in
                        let all_assigns = concat_assigns assigns all_assigns in
                        let bhv =
                          Cil.mk_behavior ~name ~assumes ~post_cond ()
                        in
                        in_guard, all_assigns, bhv :: action_bhvs
                  in
                  in_guard, out_guard, all_assigns, action_bhvs)
                (pfalse,ptrue,assigns, action_bhvs) (trans::consider)
            in
            treat_trans
              (Logic_const.por ~loc
                 (in_assumes, (Logic_const.pand ~loc (start, in_guard))),
               Logic_const.pand ~loc
                 (out_assumes,
                  (Logic_const.por ~loc (not_start, out_guard))),
               assigns,
               my_action_bhvs
              )
              others
    in
    let my_trans = List.filter (fun x -> isCrossable x kf e) my_trans in
    let in_assumes, out_assumes, assigns, action_behaviors =
      treat_trans (pfalse, ptrue, WritesAny, []) my_trans
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
      add_behavior_pebble_actions ~loc kf e behaviors state my_trans
    in
    let behaviors =
      if Logic_utils.is_trivially_false out_assumes then behaviors
      else begin
        let post_cond =
          match state.multi_state with
            | None -> mk_unchanged_aux_vars my_trans
            | Some (set,_) ->
              let set =
                Data_for_aorai.pebble_set_at set Logic_const.here_label
              in
              [Normal,
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
    assigns, behaviors
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
    WritesAny,[mk_behavior ~name ~post_cond ()]
  end

let auto_func_behaviors loc f st state =
  let call_or_ret =
    match st with
      | Promelaast.Call -> "call"
      | Promelaast.Return -> "return"
  in
  Aorai_option.debug
    "func behavior for %a (%s)" Kernel_function.pretty f call_or_ret;
  let (states, _) as auto = Data_for_aorai.getAutomata() in
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
            (Logic_const.term
               (TConst (constant_to_lconstant
			  (Data_for_aorai.op_status_to_cenum st)))
               (Ctype Cil.intType))))
    in
    let called_pre_2 =
      Logic_const.new_predicate
        (Logic_const.prel ~loc
           (Req,
            Logic_const.tvar ~loc
              (Data_for_aorai.get_logic_var Data_for_aorai.curOp),
            (Logic_const.term
               (TConst((constant_to_lconstant
			  (Data_for_aorai.func_to_cenum 
			     (Kernel_function.get_name f)))))
		  (Ctype Cil.intType))))
    in
      (* let old_pred = Aorai_utils.mk_old_state_pred loc in *)
    [(Normal, called_pre); (Normal, called_pre_2)]
  in
  let requires =
    if st = Promelaast.Call then [] else auto_func_preconditions loc f st state
  in
  let mk_behavior (assigns, behaviors) status =
    let new_assigns, new_behaviors =
      mk_behavior ~loc auto f st state status
    in
    concat_assigns new_assigns assigns, new_behaviors @ behaviors
  in
  let assigns = automaton_assigns loc in
  let assigns, behaviors = (List.fold_left mk_behavior (assigns,[]) states) in
  let global_behavior =
    Cil.mk_behavior ~requires ~post_cond ~assigns ()
  in
  (* Keep behaviors ordered according to the states they describe *)
  global_behavior :: (List.rev behaviors)

let get_preds_wrt_params_reachable_states state f status =
  let auto = Data_for_aorai.getAutomata () in
  let treat_one_trans acc tr = Logic_simplification.tor acc (fst tr.cross) in
  let find_trans state prev tr = 
    Path_analysis.get_edges prev state auto @ tr 
  in
  let treat_one_state state (_,last,_) acc =
    let my_trans =
      Data_for_aorai.Aorai_state.Set.fold (find_trans state) last []
    in
    let cond = List.fold_left treat_one_trans TFalse my_trans in
    let (_,dnf) = Logic_simplification.simplifyCond cond in
    let cond = Logic_simplification.simplifyDNFwrtCtx dnf f status in
    let pred = crosscond_to_pred cond f status in
    Logic_const.pand (acc, pimplies (is_state_pred state, pred))
  in
  Data_for_aorai.Aorai_state.Map.fold treat_one_state state ptrue

let get_preds_wrt_params_reachable_states state f status =
  let merge_reachable_state _  = Data_for_aorai.merge_end_state in
  let reachable_states =
    Data_for_aorai.Aorai_state.Map.fold 
      merge_reachable_state state Data_for_aorai.Aorai_state.Map.empty
  in
  get_preds_wrt_params_reachable_states reachable_states f status

let get_preds_pre_wrt_params f =
  let pre = Data_for_aorai.get_kf_init_state f in
  get_preds_wrt_params_reachable_states pre f Promelaast.Call

let get_preds_post_bc_wrt_params f =
  let post = Data_for_aorai.get_kf_return_state f in
  get_preds_wrt_params_reachable_states post f Promelaast.Return

let dkey = Aorai_option.register_category "action"

let treat_val loc base range pred =
  let add term =
    if Cil.isLogicZero base then term
    else Logic_const.term
      (TBinOp (PlusA, Logic_const.tat (base,Logic_const.pre_label), term))
      Linteger
  in
  let add_cst i = add (Logic_const.tinteger i) in
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
  Aorai_option.debug ~dkey "Action predicate: %a"
    Printer.pp_predicate_named res;
  Logic_const.por(pred,res)

let possible_states_preds state =
  let treat_one_state start map acc =
    let make_possible_state state _ acc =
      Logic_const.por (acc,is_state_pred state)
    in
    let possible_states =
      Data_for_aorai.Aorai_state.Map.fold make_possible_state map pfalse
    in
    Logic_const.pimplies
      (Logic_const.pat (is_state_pred start,Logic_const.pre_label),
       possible_states)
    :: acc
  in
  Data_for_aorai.Aorai_state.Map.fold treat_one_state state []

let update_to_pred ~start ~pre_state ~post_state location bindings =
  let loc = Cil_datatype.Location.unknown in
  let intv =
    Cil_datatype.Term.Map.fold
      (treat_val location) bindings Logic_const.pfalse
  in
  let pred =
    match post_state.multi_state with
      | None -> intv
      | Some(set,aux) ->
        (* [VP 2011-09-05] In fact, not all the pebble come from the considered
           pre-state. Will this lead to too strong post-conditions?
         *)
        let set = Data_for_aorai.pebble_set_at set Logic_const.here_label in
        pebble_post ~loc set aux intv
  in
  let guard =
    Logic_const.pand ~loc
      (Logic_const.pat ~loc (is_state_pred pre_state, start),
       is_state_pred post_state)
  in
  Logic_const.pimplies ~loc (guard, pred)

let action_to_pred ~start ~pre_state ~post_state bindings =
  let treat_one_loc loc vals acc =
    update_to_pred ~start ~pre_state ~post_state loc vals :: acc
  in
  Cil_datatype.Term.Map.fold treat_one_loc bindings []

let all_actions_preds start state =
  let treat_current_state pre_state post_state (_,_,bindings) acc =
    let my_bindings =
      action_to_pred ~start ~pre_state ~post_state bindings
    in
    my_bindings @ acc
  in
  let treat_start_state pre_state map acc =
    Data_for_aorai.Aorai_state.Map.fold
      (treat_current_state pre_state) map acc
  in
  Data_for_aorai.Aorai_state.Map.fold treat_start_state state []

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

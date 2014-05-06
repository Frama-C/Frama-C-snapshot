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

open Logic_ptree
open Cil
open Cil_types
open Promelaast
open Logic_simplification

module Aorai_state =
  Datatype.Make_with_collections(
    struct
      type t = Promelaast.state
      let structural_descr = Structural_descr.t_abstract
      let reprs = [ { nums = -1; name = ""; multi_state = None;
                      acceptation = Bool3.False; init = Bool3.False
                    } ]
      let name = "Aorai_state"
      let equal x y = Datatype.Int.equal x.nums y.nums
      let hash x = x.nums
      let rehash = Datatype.identity
      let compare x y = Datatype.Int.compare x.nums y.nums
      let copy = Datatype.identity
      let internal_pretty_code = Datatype.undefined
      let pretty fmt x = Format.fprintf fmt "state_%d" x.nums
      let varname _ =
        assert false (* unused while internal_pretty_code is undefined *)
      let mem_project = Datatype.never_any_project
    end
  )

module Aorai_typed_trans =
  Datatype.Make_with_collections(
    struct
      let name = "Aorai_typed_trans"
      type t =
          (Promelaast.typed_condition * Promelaast.action) Promelaast.trans
      let structural_descr = Structural_descr.t_abstract
      let reprs = [ { numt = -1; start = List.hd (Aorai_state.reprs);
                      stop = List.hd (Aorai_state.reprs);
                      cross = (TTrue,[]); } ]
      let equal x y = Datatype.Int.equal x.numt y.numt
      let hash x = x.numt
      let rehash = Datatype.identity
      let compare x y = Datatype.Int.compare x.numt y.numt
      let copy = Datatype.identity
      let internal_pretty_code = Datatype.undefined
      let pretty = Promelaoutput.print_transition
      let varname _ = assert false
      let mem_project = Datatype.never_any_project
    end)

module State_var =
  State_builder.Hashtbl
    (Aorai_state.Hashtbl)
    (Cil_datatype.Varinfo)
    (struct
        let name = "Data_for_aorai.State_var"
        let dependencies = [ Ast.self; Aorai_option.Ya.self ]
        let size = 7
     end)

let get_state_var =
  let add_var state = Cil.makeVarinfo true false state.name Cil.intType in
  State_var.memo add_var

let get_state_logic_var state = Cil.cvar_to_lvar (get_state_var state)

module Max_value_counter =
  State_builder.Hashtbl
    (Cil_datatype.Term.Hashtbl)
    (Cil_datatype.Term)
    (struct
        let name = "Data_for_aorai.Max_value_counter"
        let dependencies = [ Ast.self; Aorai_option.Ya.self ]
        let size = 7
     end)

let find_max_value t =
  try Some (Max_value_counter.find t) with Not_found -> None

let raise_error msg =
	Aorai_option.fatal "Aorai plugin internal error. \nStatus : %s.\n" msg;;
(*  Format.printf "Aorai plugin internal error. \nStatus : %s.\n" msg; *)
(*  assert false                                                             *)

let por t1 t2 =
  match t1,t2 with
      PTrue,_ | _,PTrue -> PTrue
    | PFalse,t | t,PFalse -> t
    | _,_ -> POr(t1,t2)

let pand t1 t2 =
  match t1,t2 with
      PTrue,t | t,PTrue -> t
    | PFalse,_ | _,PFalse -> PFalse
    | _,_ -> PAnd(t1,t2)

let pnot t =
  match t with
      PTrue -> PFalse
    | PFalse -> PTrue
    | PNot t -> t
    | _ -> PNot t

let rec is_same_expression e1 e2 =
  match e1,e2 with
    | PVar x, PVar y -> x = y
    | PVar _,_ | _,PVar _ -> false
    | PCst cst1, PCst cst2 -> Logic_utils.is_same_pconstant cst1 cst2
    | PCst _,_ | _,PCst _ -> false
    | PPrm (f1,x1), PPrm(f2,x2) -> f1 = x1 && f2 = x2
    | PPrm _,_ | _,PPrm _ -> false
    | PBinop(b1,l1,r1), PBinop(b2,l2,r2) ->
      b1 = b2 && is_same_expression l1 l2 && is_same_expression r1 r2
    | PBinop _, _ | _, PBinop _ -> false
    | PUnop(u1,e1), PUnop(u2,e2) -> u1 = u2 && is_same_expression e1 e2
    | PUnop _,_ | _,PUnop _ -> false
    | PArrget(a1,i1), PArrget(a2,i2) ->
      is_same_expression a1 a2 && is_same_expression i1 i2
    | PArrget _,_ | _,PArrget _ -> false
    | PField(e1,f1), PField(e2,f2) -> f1 = f2 && is_same_expression e1 e2
    | PField _,_ | _,PField _ -> false
    | PArrow(e1,f1), PArrow(e2,f2) -> f1 = f2 && is_same_expression e1 e2

let declared_logics = Hashtbl.create 97

let add_logic name log_info = Hashtbl.replace declared_logics name log_info

let get_logic name =
  try Hashtbl.find declared_logics name
  with Not_found ->
    raise_error ("Logic function '"^name^"' not declared in hashtbl")

let declared_predicates = Hashtbl.create 97

let add_predicate name pred_info = 
  Hashtbl.replace declared_predicates name pred_info

let get_predicate name =
  try Hashtbl.find declared_predicates name
  with Not_found -> raise_error ("Predicate '"^name^"' not declared in hashtbl")

(* ************************************************************************* *)
(* Some constant names used for generation *)
(* Logic variables *)
let transStart  = "aorai_Trans_Start"                    (* OK *)
let transStop   = "aorai_Trans_Stop"                     (* OK *)
let transCond   = "aorai_Trans_Cond"                     (* OK *)
let transCondP  = "aorai_Trans_Cond_param"               (* OK *)
let loopInit    = "aorai_Loop_Init"                      (* OK *)

(* C variables *)
let curState    = "aorai_CurStates"                      (* OK *)
let curStateOld = "aorai_CurStates_old"                  (* OK *)
let curTrans    = "aorai_CurTrans"                       (* OK *)
(*let curTransTmp = "aorai_CurTrans_tmp"                   (* OK *)*)
let curOp       = "aorai_CurOperation"                   (* OK *)
let curOpStatus = "aorai_CurOpStatus"                    (* OK *)
let acceptSt    = "aorai_AcceptStates"                   (* TODO *)

(* C constants #define *)
let nbOp        = "aorai_NbOper"                         (* Deprecated ? *)
let nbStates    = "aorai_NbStates"                       (* Deprecated ? *)
let nbAcceptSt  = "aorai_NbAcceptStates"                 (* Deprecated ? *)
let nbTrans     = "aorai_NbTrans"                        (* Deprecated ? *)

(* C Macros *)
let macro_ligth = "aorai_Macro_Prop_St_Tr_Without_Conds" (* Deprecated ? *)
let macro_full  = "aorai_Macro_Prop_St_Tr"               (* Deprecated ? *)
let macro_pure  = "aorai_Macro_Op_without_sub_call"      (* Deprecated ? *)

(* C enumeration *)
let listOp      = "aorai_ListOper"                       (* OK *)
let listStatus  = "aorai_OpStatusList"                   (* OK *)
let callStatus  = "aorai_Called"                         (* OK *)
let termStatus  = "aorai_Terminated"                     (* OK *)
let states      = "aorai_States"                         (* OK *)

(* C function *)
let buch_sync   = "Aorai_Sync"                           (* Deprecated ? *)

(* ************************************************************************* *)
(* Buchi automata as stored after parsing *)
let automata = ref ([],[])

(* Each transition with a parametrized cross condition (call param access or return value access) has its parametrized part stored in this array. *)
let cond_of_parametrizedTransitions = ref (Array.make (1) [[]])

(* List of variables name observed in the C file *)
let variables_from_c = ref []
(* List of functions name observed in the C file *)
let functions_from_c = ref []
(* List of functions call observed in the C file without declaration *)
let ignored_functions = ref []

(** Return the buchi automata as stored after parsing *)
let getAutomata () = !automata

(** Return the number of transitions of the automata *)
let getNumberOfTransitions () = List.length (snd !automata)

(** Return the number of states of the automata *)
let getNumberOfStates () = List.length (fst !automata)

let is_c_global name =
  try ignore (Globals.Vars.find_from_astinfo name VGlobal); true
  with Not_found ->
    try ignore (Globals.Functions.find_by_name name); true
    with Not_found -> false

let get_fresh =
  let used_names = Hashtbl.create 5 in
  fun name ->
    if Clexer.is_c_keyword name
      || Logic_lexer.is_acsl_keyword name || is_c_global name
      || Hashtbl.mem used_names name
    then begin
      let i = ref (try Hashtbl.find used_names name with Not_found -> 0) in
      let proposed_name () = name ^ "_" ^ string_of_int !i in
      while is_c_global (proposed_name()) do incr i done;
      Hashtbl.replace used_names name (!i+1);
      proposed_name ()
    end
    else begin
      Hashtbl.add used_names name 0;
      name
    end

module AuxVariables =
  State_builder.List_ref
    (Cil_datatype.Varinfo)
    (struct
      let name = "Data_for_aorai.AuxVariables"
      let dependencies =
        [ Aorai_option.Ltl_File.self; Aorai_option.Buchi.self;
          Aorai_option.Ya.self; Ast.self ]
     end)

module AbstractLogicInfo =
  State_builder.List_ref
    (Cil_datatype.Logic_info)
    (struct
        let name = "Data_for_aorai.AbstractLogicInfo"
        let dependencies =
        [ Aorai_option.Ltl_File.self; Aorai_option.Buchi.self;
          Aorai_option.Ya.self; Ast.self ]
     end)

class change_var vi1 vi2 =
  object
    inherit Visitor.frama_c_copy (Project.current ())
    method! vlogic_var_use vi =
      if Cil_datatype.Logic_var.equal vi1 vi then ChangeTo vi2 else SkipChildren
  end

let change_var_term vi1 vi2 t =
  Visitor.visitFramacTerm (new change_var vi1 vi2) t

let update_condition vi1 vi2 cond =
  let rec aux e =
    match e with
      | TOr (e1,e2) -> TOr(aux e1, aux e2)
      | TAnd (e1,e2) -> TAnd(aux e1, aux e2)
      | TNot e -> TNot (aux e)
      | TCall _ | TReturn _ | TTrue | TFalse -> e
      | TRel(rel,t1,t2) ->
        TRel(rel,change_var_term vi1 vi2 t1,change_var_term vi1 vi2 t2)
  in aux cond

let pebble_set_at li lab =
  assert (li.l_profile = []);
  let labels = List.map (fun x -> (x,lab)) li.l_labels in
  Logic_const.term (Tapp (li,labels,[])) (Extlib.the li.l_type)

let memo_multi_state st =
  match st.multi_state with
    | None ->
      let aux = Cil.makeGlobalVar (get_fresh "aorai_aux") Cil.intType in
      let laux = Cil.cvar_to_lvar aux in
      let set = Cil_const.make_logic_info (get_fresh (st.name ^ "_pebble")) in
      let typ = Logic_const.make_set_type (Ctype Cil.intType) in
      set.l_var_info.lv_type <- typ;
      set.l_labels <- [ LogicLabel(None,"L")];
      set.l_type <- Some typ;
      set.l_body <-
        LBreads
        [ Logic_const.new_identified_term (Logic_const.tvar laux) ];
      let multi_state = set,laux in
      st.multi_state <- Some multi_state;
      multi_state
    | Some multi_state -> multi_state

let change_bound_var st1 st2 cond =
  if Extlib.has_some st1.multi_state then begin
    let (_,idx1) = Extlib.the st1.multi_state in
    let (_,idx2) = memo_multi_state st2 in
    update_condition idx1 idx2 cond
  end else cond

let add_aux_variable vi = AuxVariables.add vi

let aux_variables = AuxVariables.get

let abstract_logic_info = AbstractLogicInfo.get

module StateIndex =
  State_builder.Counter(struct let name = "Data_for_aorai.StateIndex" end)

module TransIndex =
  State_builder.Counter(struct let name = "Data_for_aorai.TransIndex" end)

let new_state name =
  { name = get_fresh name; acceptation = Bool3.False;
    init = Bool3.False; nums = StateIndex.next();
    multi_state = None
  }

let new_intermediate_state () = new_state "aorai_intermediate_state"

let new_trans start stop cond =
  { start = start; stop = stop; cross = cond; numt = TransIndex.next () }

let check_states s =
  let states,trans = getAutomata() in
  let max = getNumberOfStates () in
  List.iter
    (fun x -> if x.nums >= max then
        Aorai_option.fatal "%s: State %d found while max id is supposed to be %d"
          s x.nums max)
    states;
  List.iter
    (fun x ->
      try 
        let y = List.find (fun y -> x.nums = y.nums && not (x==y)) states in
        Aorai_option.fatal "%s: State %s and %s share same id %d"
          s x.name y.name x.nums
      with Not_found -> ()
    )
    states;
  List.iter
    (fun x ->
      if not (List.memq x.start states) then
        Aorai_option.fatal
          "%s: Start state %d of transition %d is not among known states"
          s x.start.nums x.numt;
      if not (List.memq x.stop states) then
        Aorai_option.fatal
          "%s: End state %d of transition %d is not among known states"
          s x.start.nums x.numt;)
    trans

let cst_one = PCst (Logic_ptree.IntConstant "1")

let cst_zero = PCst (Logic_ptree.IntConstant "0")

let is_cst_zero e =
  match e with
    | PCst(IntConstant "0") -> true
    | _ -> false

let is_cst_one e =
  match e with
      PCst (IntConstant "1") -> true
    | _ -> false

let is_single elt =
  match elt.min_rep, elt.max_rep with
    | Some min, Some max -> is_cst_one min && is_cst_one max
    | _ -> false

(* Epsilon transitions will account for the possibility of
   not entering a repeated sequence at all. They will be normalized after
   the entire automaton is processed by adding direct transitions from the
   starting state to the children of the end state.
*)
type eps_trans =
    Normal of typed_condition * action
  | Epsilon of typed_condition * action

let print_epsilon_trans fmt = function
  | Normal (c,a) ->
    Format.fprintf fmt "%a%a"
      Promelaoutput.print_condition c
      Promelaoutput.print_action a
  | Epsilon (c,a) ->
    Format.fprintf fmt "epsilon-trans:@\n%a%a"
      Promelaoutput.print_condition c
      Promelaoutput.print_action a

type current_event =
  | ECall of
      kernel_function
    * Cil_types.logic_var Cil_datatype.Varinfo.Hashtbl.t
    * eps_trans Promelaast.trans
  | EReturn of kernel_function
  | ECOR of kernel_function
  | ENone (* None found yet *)
  | EMulti (* multiple event possible.
              repr of the stack does not take into account
              this particular event. *)

let add_current_event event env cond =
  let is_empty tbl = Cil_datatype.Varinfo.Hashtbl.length tbl = 0 in
  match env with
      [] -> assert false
    | old_event :: tl ->
      match event, old_event with
        | ENone, _ -> env, cond
        | _, ENone -> event::tl, cond
        | ECall (kf1,_,_), ECall (kf2,_,_)
          when Kernel_function.equal kf1 kf2 -> env, cond
        | ECall (kf1,tbl1,_), ECall (kf2,tbl2,_)->
          (* ltl2buchi generates such inconsistent guards, but luckily does
             not speak about formals. In this case, we just return False with
             an empty event. If this situation occurs in an handwritten
             automaton that uses formals we simply reject it.
           *)
          if is_empty tbl1 && is_empty tbl2 then ENone::tl, TFalse
          else
            Aorai_option.abort
              "specification is inconsistent: two call events for distinct \
               functions %a and %a at the same time."
              Kernel_function.pretty kf1 Kernel_function.pretty kf2
        | ECall (_,_,_), EMulti -> event::tl, cond
        | ECall (kf1,tbl1,_), EReturn kf2 ->
          if is_empty tbl1 then ENone::tl, TFalse
          else
            Aorai_option.abort
              "specification is inconsistent: trying to call %a and \
               return from %a at the same time."
              Kernel_function.pretty kf1 Kernel_function.pretty kf2
        | ECall(kf1,_,_), ECOR kf2
          when Kernel_function.equal kf1 kf2 ->
          event::tl, cond
        | ECall (kf1,tbl1,_), ECOR kf2 ->
          if is_empty tbl1 then ENone::tl, TFalse
          else
            Aorai_option.abort
              "specification is inconsistent: trying to call %a and \
               call or return from %a at the same time."
              Kernel_function.pretty kf1 Kernel_function.pretty kf2
        | EReturn kf1, ECall(kf2,tbl2,_) ->
          if is_empty tbl2 then ENone::tl, TFalse
          else
            Aorai_option.abort
              "specification is inconsistent: trying to call %a and \
               return from %a at the same time."
            Kernel_function.pretty kf2 Kernel_function.pretty kf1
        | EReturn kf1, (ECOR kf2 | EReturn kf2)
          when Kernel_function.equal kf1 kf2 -> event::tl, cond
        | EReturn _, EReturn _ -> ENone::tl, TFalse
        | EReturn _, ECOR _ -> ENone::tl, TFalse
        | EReturn _, EMulti -> ENone::tl, TFalse
        | (EMulti | ECOR _), _ -> assert false
          (* These are compound event. They cannot be found as individual ones*)

let merge_current_event env1 env2 cond1 cond2 =
  assert (List.tl env1 == List.tl env2);
  let old_env = List.tl env2 in
  match (List.hd env1, List.hd env2) with
      | ENone, _ -> env2, tor cond1 cond2
      | _, ENone -> env1, tor cond1 cond2
      | ECall(kf1,_,_), ECall(kf2,_,_)
        when Kernel_function.equal kf1 kf2 -> env2,  tor cond1 cond2
      | ECall _, ECall _ -> EMulti::old_env, tor cond1 cond2
      | ECall _, EMulti -> env2, tor cond1 cond2
      | ECall (kf1,_,_), ECOR kf2 when Kernel_function.equal kf1 kf2 ->
        env2, tor cond1 cond2
      | ECall (kf1,_,_), EReturn kf2 when Kernel_function.equal kf1 kf2 ->
        ECOR kf1 :: old_env, tor cond1 cond2
      | ECall _, (ECOR _ | EReturn _) -> EMulti :: old_env, tor cond1 cond2
      | EReturn kf1, ECall (kf2,_,_) when Kernel_function.equal kf1 kf2 ->
        ECOR kf1 :: old_env, tor cond1 cond2
      | EReturn _, ECall _  -> EMulti :: old_env, tor cond1 cond2
      | EReturn kf1, EReturn kf2 when Kernel_function.equal kf1 kf2 ->
        env2, tor cond1 cond2
      | EReturn _, EReturn _ -> EMulti :: old_env, tor cond1 cond2
      | EReturn _, EMulti -> env2, tor cond1 cond2
      | EReturn kf1, ECOR kf2 when Kernel_function.equal kf1 kf2 ->
        env2, tor cond1 cond2
      | EReturn _, ECOR _ ->
        EMulti :: old_env, tor cond1 cond2
      | ECOR kf1, (ECall(kf2,_,_) | EReturn kf2 | ECOR kf2)
        when Kernel_function.equal kf1 kf2 -> env1, tor cond1 cond2
      | ECOR _, (ECall _ | EReturn _ | ECOR _) ->
        EMulti :: old_env, tor cond1 cond2
      | ECOR _, EMulti -> env2, tor cond1 cond2
      | EMulti, (ECall _ | EReturn _ | ECOR _) -> env1, tor cond1 cond2
      | EMulti, EMulti -> EMulti::old_env, tor cond1 cond2

let get_bindings st my_var =
  let my_lval = TVar my_var, TNoOffset in
  match st with
      None -> my_lval
    | Some st ->
      let (_,idx) = memo_multi_state st in
      Logic_const.addTermOffsetLval (TIndex (Logic_const.tvar idx,TNoOffset)) my_lval

let get_bindings_term st my_var typ =
  Logic_const.term (TLval (get_bindings st my_var)) typ

let memo_aux_variable tr counter used_prms vi =
  try
    let my_var = Cil_datatype.Varinfo.Hashtbl.find used_prms vi in
    get_bindings_term counter my_var (Ctype vi.vtype)
  with Not_found ->
    let my_type =
      match counter with
        | None -> vi.vtype
        | Some _ -> TArray(vi.vtype,None,{scache=Not_Computed},[])
    in
    let my_var =
      Cil.makeGlobalVar (get_fresh ("aorai_" ^ vi.vname)) my_type
    in
    add_aux_variable my_var;
    let my_lvar = Cil.cvar_to_lvar my_var in
    Cil_datatype.Varinfo.Hashtbl.add used_prms vi my_lvar;
    (match tr.cross with
      | Normal (cond,action) ->
        let st = Extlib.opt_map (fun _ -> tr.stop) counter in
        let loc = get_bindings st my_lvar in
        let copy = Copy_value (loc,Logic_const.tvar (Cil.cvar_to_lvar vi)) in
        tr.cross <- Normal(cond,copy::action)
      | Epsilon _ ->
        Aorai_option.fatal "Epsilon transition used as Call event"
    );
    get_bindings_term counter my_lvar (Ctype vi.vtype)

let check_one top info counter s =
  match info with
    | ECall (kf,used_prms,tr) ->
      (try
         let vi = Globals.Vars.find_from_astinfo s (VFormal kf) in
         if top then Some (Logic_const.tvar (Cil.cvar_to_lvar vi))
         else Some (memo_aux_variable tr counter used_prms vi)
       with Not_found -> None)
    | EReturn kf when top && ( Datatype.String.equal s "return"
                               || Datatype.String.equal s "\\result") ->
      let rt = Kernel_function.get_return_type kf in
      if Cil.isVoidType rt then
        Aorai_option.abort
          "%a returns void. \\result is meaningless in this context"
          Kernel_function.pretty kf;
      Some (Logic_const.term (TLval (TResult rt,TNoOffset)) (Ctype rt))
    | ECOR _ | EReturn _ | EMulti | ENone -> None

let find_in_env env counter s =
  let current, stack =
    match env with
      | current::stack -> current, stack
      | [] -> Aorai_option.fatal "Empty type-checking environment"
  in
  match check_one true current counter s with
      Some lv -> lv
    | None ->
      let module M = struct exception Found of term end in
      (try
         List.iter
           (fun x ->
             match check_one false x counter s with
                 None -> ()
               | Some lv -> raise (M.Found lv))
           stack;
         let vi = Globals.Vars.find_from_astinfo s VGlobal in
         Logic_const.tvar (Cil.cvar_to_lvar vi)
       with
           M.Found lv -> lv
         | Not_found -> Aorai_option.abort "Unknown variable %s" s)

let find_prm_in_env env ?tr counter f x =
  let kf =
    try Globals.Functions.find_by_name f
    with Not_found -> Aorai_option.abort "Unknown function %s" f
  in
  if Datatype.String.equal x "return" ||
    Datatype.String.equal x "\\result" then begin
    (* Return event *)
    let rt = Kernel_function.get_return_type kf in
    if Cil.isVoidType rt then
      Aorai_option.abort
        "%a returns void. %s().%s is meaningless in this context"
        Kernel_function.pretty kf f x;
    let env,cond = add_current_event (EReturn kf) env (TReturn kf) in
    env,
    Logic_const.term (TLval (TResult rt,TNoOffset)) (Ctype rt),
    cond
  end else begin (* Complete Call followed by Return event *)
    let rec treat_env top =
      function
        | ECall(kf',_,_) as event :: _
            when Kernel_function.equal kf kf'->
          (match check_one top event counter x with
              Some lv ->
                env, lv, TTrue
            | None ->
              Aorai_option.abort "Function %s has no parameter %s" f x)
        | (ENone | EReturn _ | EMulti | ECOR _ | ECall _ )
          :: tl ->
          treat_env false tl
        | [] ->
          let env, cond =
            match tr with
                None ->
                  Aorai_option.abort
                    "Function %s is not in the call stack. \
                     Cannot use its parameter %s here" f x
              | Some tr ->
                add_current_event
                  (ECall (kf, Cil_datatype.Varinfo.Hashtbl.create 3, tr))
                  env
                  (TCall (kf,None))
          in
          let vi =
            try Globals.Vars.find_from_astinfo x (VFormal kf)
            with Not_found ->
              Aorai_option.abort "Function %s has no parameter %s" f x
          in
          (* By definition, we are at the call event: no need to store
             it in an aux variable or array here.
           *)
          env, Logic_const.tvar (Cil.cvar_to_lvar vi), cond
    in treat_env true env
  end

module C_logic_env =
struct
  let anonCompFieldName = Cabs2cil.anonCompFieldName
  let conditionalConversion = Cabs2cil.logicConditionalConversion
  let is_loop () = false
  let find_macro _ = raise Not_found
  let find_var _ = raise Not_found
  let find_enum_tag _ = raise Not_found
  let find_comp_type ~kind:_ _ = raise Not_found
  let find_comp_field info s =
    let field = Cil.getCompField info s in
    Field(field,NoOffset)
  let find_type _ = raise Not_found
  let find_label _ = raise Not_found

  include Logic_env
  let add_logic_function =
    add_logic_function_gen Logic_utils.is_same_logic_profile

  let integral_cast ty t =
    Aorai_option.abort
      "term %a has type %a, but %a is expected."
      Printer.pp_term t Printer.pp_logic_type Linteger Printer.pp_typ ty
end

module LTyping = Logic_typing.Make(C_logic_env)

let type_expr env ?tr ?current e =
  let loc = Cil_datatype.Location.unknown in
  let rec aux env cond e =
    match e with
        PVar s ->
          let var = find_in_env env current s in
          env, var, cond
      | PPrm(f,x) -> find_prm_in_env env ?tr current f x
      | PCst (Logic_ptree.IntConstant s) ->
        let e = Cil.parseIntLogic ~loc s in
        env, e, cond
      | PCst (Logic_ptree.FloatConstant str) ->
          let c = Logic_utils.string_to_float_lconstant str in
          env, Logic_const.term (TConst c) Lreal, cond
      | PCst (Logic_ptree.StringConstant s) ->
        let t =
          Logic_const.term
            (TConst(LStr (Logic_typing.unescape s))) (Ctype Cil.charPtrType)
        in
        env,t,cond
      | PCst (Logic_ptree.WStringConstant s) ->
        let t =
          Logic_const.term
            (TConst (LWStr (Logic_typing.wcharlist_of_string s)))
            (Ctype (TPtr(Cil.theMachine.wcharType,[])))
        in env,t,cond
      | PBinop(bop,e1,e2) ->
        let op = Logic_typing.type_binop bop in
        let env,e1,cond = aux env cond e1 in
        let env,e2,cond = aux env cond e2 in
        let t1 = e1.term_type in
        let t2 = e2.term_type in
        let t =
          if Logic_typing.is_arithmetic_type t1
            && Logic_typing.is_arithmetic_type t2
          then
            let t = Logic_typing.arithmetic_conversion t1 t2 in
            Logic_const.term
              (TBinOp (op,LTyping.mk_cast e1 t,LTyping.mk_cast e2 t))
              t
          else
            (match bop with
              | Logic_ptree.Badd
                  when
                    Logic_typing.is_integral_type t2
                    && Logic_utils.isLogicPointerType t1 ->
                Logic_const.term (TBinOp (PlusPI,e1,e2)) t1
              | Logic_ptree.Bsub
                  when
                    Logic_typing.is_integral_type t2
                    && Logic_utils.isLogicPointerType t1 ->
                Logic_const.term (TBinOp (MinusPI,e1,e2)) t1
              | Logic_ptree.Badd
                  when
                    Logic_typing.is_integral_type t1
                  && Logic_utils.isLogicPointerType t2 ->
                Logic_const.term (TBinOp (PlusPI,e2,e1)) t2
              | Logic_ptree.Bsub
                  when
                    Logic_typing.is_integral_type t1
                    && Logic_utils.isLogicPointerType t2 ->
                Logic_const.term (TBinOp (MinusPI,e2,e1)) t2
              | Logic_ptree.Bsub
                when
                  Logic_utils.isLogicPointerType t1
                  && Logic_utils.isLogicPointerType t2 ->
                Logic_const.term
                  (TBinOp (MinusPP,e1,LTyping.mk_cast e2 t1))
                  Linteger
              | _ ->
                Aorai_option.abort
                  "Invalid operands for binary operator %a: \
                   unexpected %a and %a"
                Printer.pp_binop op
                Printer.pp_term e1
                Printer.pp_term e2)
        in
        env, t, cond
      | PUnop(Logic_ptree.Uminus,e) ->
        let env,t,cond = aux env cond e in
        if Logic_typing.is_arithmetic_type t.term_type then
          env,Logic_const.term (TUnOp (Neg,t)) Linteger,cond
        else Aorai_option.abort
          "Invalid operand for unary -: unexpected %a" Printer.pp_term t
      | PUnop(Logic_ptree.Ubw_not,e) ->
        let env,t,cond = aux env cond e in
        if Logic_typing.is_arithmetic_type t.term_type then
          env,Logic_const.term (TUnOp (BNot,t)) Linteger,cond
        else Aorai_option.abort
          "Invalid operand for bitwise not: unexpected %a" Printer.pp_term t
      | PUnop(Logic_ptree.Uamp,e) ->
        let env, t, cond = aux env cond e in
        let ptr =
          try Ctype (TPtr (Logic_utils.logicCType t.term_type,[]))
          with Failure _ ->
            Aorai_option.abort "Cannot take address: not a C type(%a): %a"
              Printer.pp_logic_type t.term_type Printer.pp_term t
        in
        (match t.term_node with
          | TLval v | TStartOf v -> env, Logic_const.taddrof v ptr, cond
          | _ ->
            Aorai_option.abort "Cannot take address: not an lvalue %a"
              Printer.pp_term t
        )
      | PUnop (Logic_ptree.Ustar,e) ->
        let env, t, cond = aux env cond e in
        if Logic_utils.isLogicPointerType t.term_type then
          env,
          Logic_const.term
            (TLval (TMem t, TNoOffset))
            (Logic_typing.type_of_pointed t.term_type),
          cond
        else
          Aorai_option.abort "Cannot dereference term %a" Printer.pp_term t
      | PArrget(e1,e2) ->
        let env, t1, cond = aux env cond e1 in
        let env, t2, cond = aux env cond e2 in
        let t =
          if Logic_utils.isLogicPointerType t1.term_type
            && Logic_typing.is_integral_type t2.term_type
          then
            Logic_const.term
              (TBinOp (IndexPI,t1,t2))
              (Logic_typing.type_of_pointed t1.term_type)
          else if Logic_utils.isLogicPointerType t2.term_type
              && Logic_typing.is_integral_type t1.term_type
          then
            Logic_const.term
              (TBinOp (IndexPI,t2,t1))
              (Logic_typing.type_of_pointed t2.term_type)
          else if Logic_utils.isLogicArrayType t1.term_type
              && Logic_typing.is_integral_type t2.term_type
          then
            (match t1.term_node with
              | TStartOf lv | TLval lv ->
                Logic_const.term
                  (TLval
                     (Logic_const.addTermOffsetLval
                        (TIndex (t2, TNoOffset)) lv))
                  (Logic_typing.type_of_array_elem t1.term_type)
              | _ ->
                Aorai_option.fatal
                  "Unsupported operation: %a[%a]"
                  Printer.pp_term t1 Printer.pp_term t2)
          else if Logic_utils.isLogicArrayType t2.term_type
              && Logic_typing.is_integral_type t1.term_type
          then
            (match t2.term_node with
              | TStartOf lv | TLval lv ->
                Logic_const.term
                  (TLval
                     (Logic_const.addTermOffsetLval (TIndex (t1, TNoOffset)) lv))
                  (Logic_typing.type_of_array_elem t2.term_type)
              | _ ->
                Aorai_option.fatal
                  "Unsupported operation: %a[%a]"
                  Printer.pp_term t1 Printer.pp_term t2)
          else
            Aorai_option.abort
              "Subscripted value is neither array nor pointer: %a[%a]"
              Printer.pp_term t1 Printer.pp_term t2
        in
        env, t, cond
      | PField(e,s) ->
        let env, t, cond = aux env cond e in
        (match t.term_node with
          | TLval lv ->
            let off, ty = LTyping.type_of_field loc s t.term_type in
            let lv = Logic_const.addTermOffsetLval off lv in
            env, Logic_const.term (TLval lv) ty, cond
          | _ ->
            Aorai_option.fatal
              "Unsupported operation: %a.%s" Printer.pp_term t s)
      | PArrow(e,s) ->
        let env, t, cond = aux env cond e in
        if Logic_utils.isLogicPointerType t.term_type then begin
          let off, ty =
            LTyping.type_of_field loc s
              (Logic_typing.type_of_pointed t.term_type)
          in
          let lv = Logic_const.addTermOffsetLval off (TMem t,TNoOffset) in
          env, Logic_const.term (TLval lv) ty, cond
        end else
          Aorai_option.abort "base term is not a pointer in %a -> %s"
          Printer.pp_term t s
  in
  aux env TTrue e

let type_cond needs_pebble env tr cond =
  let current = if needs_pebble then Some tr.stop else None in
  let rec aux pos env =
    function
      | PRel(rel,e1,e2) ->
        let env, e1, c1 = type_expr env ~tr ?current e1 in
        let env, e2, c2 = type_expr env ~tr ?current e2 in
        let call_cond = if pos then tand c1 c2 else tor (tnot c1) (tnot c2) in
        let rel = TRel(Logic_typing.type_rel rel,e1,e2) in
        let cond = if pos then tand rel call_cond else tor rel call_cond in
        env, cond
      | PTrue -> env, TTrue
      | PFalse -> env, TFalse
      | POr(c1,c2) ->
        let env1, c1 = aux pos env c1 in
        let env2, c2 = aux pos env c2 in
        merge_current_event env1 env2 c1 c2
      | PAnd(c1,c2) ->
        let env, c1 = aux pos env c1 in
        let env, c2 = aux pos env c2 in
        env, TAnd(c1,c2)
      | PNot c ->
        let env, c = aux (not pos) env c in env, TNot c
      | PCall (s,b) ->
        let kf =
          try Globals.Functions.find_by_name s
          with Not_found -> Aorai_option.abort "No such function: %s" s
        in
        let b =
          Extlib.opt_map
            (fun b ->
              let bhvs = Annotations.behaviors ~populate:false kf in
              try List.find (fun x -> x.b_name = b) bhvs
              with Not_found ->
                Aorai_option.abort "Function %a has no behavior named %s"
                  Kernel_function.pretty kf b)
            b
        in
        if pos then
          add_current_event
            (ECall (kf, Cil_datatype.Varinfo.Hashtbl.create 3, tr)) env
            (TCall (kf,b))
          else env, TCall (kf,b)
      | PReturn s ->
        let kf =
          try
            Globals.Functions.find_by_name s
          with Not_found -> Aorai_option.abort "No such function %s" s
        in
        if pos then add_current_event (EReturn kf) env (TReturn kf)
        else env, TReturn kf
  in
  aux true (ENone::env) cond

module Reject_state =
  State_builder.Option_ref(Aorai_state)
    (struct
        let name = "Data_for_aorai.Reject_state"
        let dependencies =
          [ Ast.self; Aorai_option.Ltl_File.self; Aorai_option.Buchi.self;
            Aorai_option.Ya.self]
     end)

let get_reject_state () =
  let create () = new_state "aorai_reject" in
  Reject_state.memo create

let add_if_needed states st =
  if List.for_all (fun x -> not (Aorai_state.equal x st)) states
  then st::states
  else states

let rec type_seq default_state tr env needs_pebble curr_start curr_end seq =
  let loc = Cil_datatype.Location.unknown in
  match seq with
    | [] -> (* We identify start and end. *)
      (env, [], [], curr_end, curr_end)
    | elt :: seq ->
      let is_single_trans =
        match elt.min_rep, elt.max_rep with
          | Some min, Some max -> is_cst_one min && is_cst_one max
          | None, _ | _, None -> false
      in
      let is_opt =
        match elt.min_rep with
          | Some min -> is_cst_zero min
          | None-> true
      in
      let might_be_zero =
        is_opt ||
          (match Extlib.the elt.min_rep with PCst _ -> false | _ -> true)
      in
      let at_most_one =
        is_opt &&
          match elt.max_rep with
            | None -> false
            | Some max -> is_cst_one max
      in
      let has_loop = not at_most_one && not is_single_trans in
      let needs_counter =
        match elt.min_rep, elt.max_rep with
            | None, None -> false
            | Some min, None -> not (is_cst_zero min || is_cst_one min)
            | None, Some max -> not (is_cst_one max)
            | Some min, Some max ->
              not (is_cst_zero min || is_cst_one min) || not (is_cst_one max)
      in
      let fixed_number_of_loop =
        match elt.min_rep, elt.max_rep with
            | _, None -> false
            | None, Some max -> not (is_cst_zero max)
            | Some min, Some max -> is_same_expression min max
      in
      let my_end =
        match seq with
            [] when not (curr_end.nums = tr.stop.nums)
               || is_single_trans || at_most_one -> curr_end
          | _ -> new_intermediate_state ()
      in
      Aorai_option.debug "Examining single elt:@\n%s -> %s:@[%a@]"
        curr_start.name my_end.name Promelaoutput.print_seq_elt elt;
      let guard_exit_loop env current counter =
        if is_opt then TTrue
        else
          let e = Extlib.the elt.min_rep in
          let _,e,_ = type_expr env ?current e in
          (* If we have done at least the lower bound of cycles, we can exit
             the loop. *)
          TRel(Cil_types.Rle,e,counter)
      in
      let guard_loop env current counter =
        match elt.max_rep with
          | None ->
            (* We're using an int: adds an (somewhat artificial) requirements
               that the counter itself does not overflow...
             *)
            let i = Cil.max_signed_number (Cil.bitsSizeOf Cil.intType) in
            let e = Logic_const.tint ~loc i in
            TRel(Cil_types.Rlt, counter, e)
          | Some e ->
            let _,e,_ = type_expr env ?current e in
            Max_value_counter.replace counter e;
            (* The counter is incremented after the test: it
               must be strictly less than the upper bound to enter
               a new cycle.
             *)
            TRel(Cil_types.Rlt, counter, e)
      in
      let env,inner_states, inner_trans, inner_start, inner_end =
        match elt.condition with
          | None ->
            assert (elt.nested <> []);
            (* we don't have a completely empty condition. *)
            type_seq
              default_state tr env needs_pebble curr_start my_end elt.nested
          | Some cond ->
            let seq_start =
              match elt.nested with
                  [] -> my_end
                | _ -> new_intermediate_state ()
            in
            let trans_start = new_trans curr_start seq_start (Normal (TTrue,[]))
            in
            let inner_env, cond = type_cond needs_pebble env trans_start cond in
            let (env,states, seq_transitions, seq_end) =
              match elt.nested with
                | [] -> inner_env, [], [], my_end
                | _ ->
                  let intermediate = new_intermediate_state () in
                  let (env, states, transitions, _, seq_end) =
                    type_seq
                      default_state tr
                      inner_env needs_pebble seq_start intermediate elt.nested
                  in env, states, transitions, seq_end
            in
            let states = add_if_needed states curr_start in
            let transitions = trans_start :: seq_transitions in
            (match trans_start.cross with
                | Normal (conds,action) ->
                  trans_start.cross <- Normal(tand cond conds,action)
                | Epsilon _ ->
                  Aorai_option.fatal
                    "Transition guard translated as epsilon transition");
            let states = add_if_needed states seq_start in
            (match env with
              | [] | (ENone | ECall _) :: _ ->
                (env, states, transitions, curr_start, seq_end)
              | EReturn kf1 :: ECall (kf2,_,_) :: tl
                  when Kernel_function.equal kf1 kf2 ->
                (tl, states, transitions, curr_start, seq_end)
              | (EReturn _ | ECOR _ ) :: _ ->
                    (* If there is as mismatch (e.g. Call f; Return g), it will
                       be caught later. There are legitimate situations for
                       this pattern however (if the sequence itself occurs
                       in a non-empty context in particular)
                     *)
                (env, states, transitions, curr_start, seq_end)
              | EMulti :: env ->
                (env, states, transitions, curr_start, seq_end))
      in
      let loop_end = if has_loop then new_intermediate_state () else inner_end
      in
      let (_,oth_states,oth_trans,oth_start,_) =
        type_seq default_state tr env needs_pebble loop_end curr_end seq
      in
      let trans = inner_trans @ oth_trans in
      let states = List.fold_left add_if_needed oth_states inner_states in
      let auto = (inner_states,inner_trans) in
      if at_most_one then begin
        (* Just adds an epsilon transition from start to end *)
        let opt = new_trans curr_start oth_start (Epsilon (TTrue,[])) in
        env, states, opt::trans, curr_start, curr_end
      end
      else if has_loop then begin
        (* TODO: makes it an integer *)
        let counter =
          let ty = if needs_pebble then
              Cil_types.TArray (Cil.intType,None,{scache=Not_Computed},[])
            else Cil.intType
          in (* We won't always need a counter *)
          lazy (
            let vi = Cil.makeGlobalVar (get_fresh "aorai_counter") ty in
            add_aux_variable vi;
            vi
          )
        in
        let make_counter st =
          let vi = Lazy.force counter in
          let base = TVar (Cil.cvar_to_lvar vi), TNoOffset in
          if needs_pebble then
            let (_,idx) = memo_multi_state st in
            Logic_const.addTermOffsetLval
              (TIndex (Logic_const.tvar idx,TNoOffset)) base
          else base
        in
        let make_counter_term st =
          Logic_const.term (TLval (make_counter st)) (Ctype Cil.intType)
        in
        Aorai_option.debug "Inner start is %s; Inner end is %s"
          inner_start.name inner_end.name;
        let treat_state (states, oth_trans) st =
          let trans = Path_analysis.get_transitions_of_state st auto in
          if st.nums = inner_start.nums then begin
            let loop_trans =
              if needs_counter then begin
                List.fold_left
                  (fun acc tr ->
                    let init_action = Counter_init (make_counter tr.stop) in
                    let init_cross =
                      match tr.cross with
                        | Normal (cond, actions) ->
                          Normal(cond, init_action :: actions)
                        | Epsilon(cond, actions) ->
                          Epsilon(cond, init_action :: actions)
                    in
                    Aorai_option.debug "New init trans %s -> %s: %a"
                      st.name tr.stop.name
                      print_epsilon_trans init_cross;
                    let init_trans =
                      new_trans st tr.stop init_cross
                    in
                    if at_most_one then init_trans :: acc
                    else begin
                      let st =
                        if needs_pebble then Some curr_start else None
                      in
                      let loop_cond =
                        if needs_counter then
                          guard_loop env st
                            (make_counter_term curr_start)
                        else TTrue
                      in
                      let loop_action =
                        if needs_counter then begin
                          let counter = make_counter curr_start in
                          [ Counter_incr counter ]
                        end else []
                      in
                      let loop_cross =
                        match tr.cross with
                          | Normal(cond, actions) ->
                            Normal(tand loop_cond cond, loop_action @ actions)
                          | Epsilon(cond, actions) ->
                            Epsilon(tand loop_cond cond, loop_action @ actions)
                      in
                      Aorai_option.debug "New loop trans %s -> %s: %a"
                        inner_end.name tr.stop.name
                      print_epsilon_trans loop_cross;
                      let loop_trans =
                        new_trans inner_end tr.stop loop_cross in
                      init_trans :: loop_trans :: acc
                    end)
                  oth_trans trans
              end else oth_trans
            in
            let trans =
              if might_be_zero then begin
              (* We can bypass the inner transition altogether *)
                let zero_cond =
                  if is_opt then TTrue
                  else
                    let current =
                      if needs_pebble then Some curr_start else None
                    in
                    let _,t,_ =
                      type_expr env ?current (Extlib.the elt.min_rep)
                    in
                    TRel (Cil_types.Req, t, Logic_const.tinteger ~loc 0)
                in
                let no_seq = new_trans st oth_start (Epsilon (zero_cond,[])) in
                no_seq :: loop_trans
              end else loop_trans
            in
            states, trans
          end
          else if st.nums = inner_end.nums then begin
            (* adds conditions on counter if needed *)
            let st =
              if needs_pebble then Some curr_end else None
            in
            let min_cond =
              if needs_counter then
                guard_exit_loop env st (make_counter_term curr_end)
              else TTrue
            in
            let min_cond = Epsilon (min_cond,[]) in
            Aorai_option.debug "New exit trans %s -> %s: %a"
              inner_end.name oth_start.name
              print_epsilon_trans min_cond;
            let exit_trans = new_trans inner_end oth_start min_cond in
            let trans = exit_trans :: trans @ oth_trans in
            states, trans
          end else begin
            (* inner state: add a rejection state for consistency purposes
               iff we don't have a constant number of repetition (i.e. cut
               out branches where automaton wrongly start a new step) and
               don't have an otherwise branch in the original automaton.
             *)
            if fixed_number_of_loop || default_state then
              states, trans @ oth_trans
            else begin
              let cond =
                List.fold_left
                  (fun acc tr ->
                    match tr.cross with
                      | Normal (cond,_) | Epsilon (cond,_) ->
                        let cond = change_bound_var tr.stop st cond in
                        tor acc cond)
                  TFalse trans
              in
              let (cond,_) = Logic_simplification.simplifyCond cond in
              let cond = tnot cond in
              (match cond with
                  TFalse -> states, trans @ oth_trans
                | _ ->
                  let reject = get_reject_state () in
                  let states = add_if_needed states reject in
                  let trans = new_trans st reject (Normal(cond,[])) :: trans
                  in states, trans @ oth_trans
              )
            end
          end
        in
        let states, trans =
          List.fold_left treat_state
            (* inner transition gets added in treat_state *)
            (states, oth_trans)
            inner_states
        in
        env, states, trans, curr_start, curr_end
      end else
        env, states, trans, curr_start, curr_end

let single_path (states,transitions as auto) tr =
  Aorai_option.Deterministic.get () ||
    (let init = Path_analysis.get_init_states auto in
     match init with
       | [ st ] ->
         let auto = (states,
                     List.filter (fun x -> x.numt <> tr.numt) transitions)
         in
         Path_analysis.at_most_one_path auto st tr.start
       | _ -> false)

let find_otherwise_trans auto st =
  let trans = Path_analysis.get_transitions_of_state st auto in
  try let tr = List.find (fun x -> x.cross = Otherwise) trans in Some tr.stop
  with Not_found -> None

let type_trans auto env tr =
  let needs_pebble = not (single_path auto tr) in
  let has_siblings =
    match Path_analysis.get_transitions_of_state tr.start auto with
      | [] -> Aorai_option.fatal "Ill-formed automaton"
              (* at least tr should be there *)
      | [ _ ] -> false (* We only have one sequence to exit from there anyway *)
      | _::_::_ -> true
  in
  Aorai_option.debug
    "Analyzing transition %s -> %s: %a (needs pebble: %B)"
    tr.start.name tr.stop.name Promelaoutput.print_parsed tr.cross needs_pebble;
  match tr.cross with
    | Seq seq ->
      let default_state = find_otherwise_trans auto tr.start in
      let has_default_state = Extlib.has_some default_state in
      let _,states, transitions,_,_ =
        type_seq has_default_state tr env needs_pebble tr.start tr.stop seq
      in
      let (states, transitions) =
        if List.exists (fun st -> st.multi_state <> None) states then begin
        (* We have introduced some multi-state somewhere, we have to introduce
           pebbles and propagate them from state to state. *)
          let start = tr.start in
          let count = (* TODO: make it an integer. *)
            Cil.makeGlobalVar
              (get_fresh ("aorai_cnt_" ^ start.name)) Cil.intType
          in
          add_aux_variable count;
          let transitions =
            List.map
              (fun trans ->
                match trans.cross with
                  | Epsilon _ -> trans
                  | Normal(cond,actions) ->
                    let (dest,d_aux) = memo_multi_state tr.stop in
                    let actions =
                      if tr.start.nums <> start.nums then begin
                        let src,s_aux = memo_multi_state tr.start in
                        Pebble_move(dest,d_aux,src,s_aux) :: actions
                      end else begin
                        let v = Cil.cvar_to_lvar count in
                        let incr = Counter_incr (TVar v, TNoOffset) in
                        let init = Pebble_init (dest, d_aux, v) in
                        init::incr::actions
                      end
                    in
                    { trans with
                      cross = Normal(cond, actions) })
              transitions
          in
          states, transitions
        end else
          states, transitions
      in
        (* For each intermediate state, add a transition
           to either the default state or a rejection state (in which we will
           stay until the end of the execution, while another branch might
           succeed in an acceptance state.
           )*)
      let needs_default =
        has_siblings &&
          match transitions with
            | [] | [ _ ] -> false
            | _::_::_ -> true
      in
      Aorai_option.debug "Resulting transitions:@\n%a"
          (Pretty_utils.pp_list ~sep:"@\n"
             (fun fmt tr -> Format.fprintf fmt "%s -> %s:@[%a@]"
               tr.start.name tr.stop.name print_epsilon_trans tr.cross))
          transitions;
        states, transitions, needs_default
    | Otherwise -> [],[], false (* treated directly by type_seq *)

let add_reject_trans auto intermediate_states =
  let treat_one_state (states, trans) st =
    let my_trans = Path_analysis.get_transitions_of_state st auto in
    let reject_state = get_reject_state () in
    let states = add_if_needed states reject_state in
    let cond =
      List.fold_left
        (fun acc tr ->
          let cond,_ = tr.cross in
          let cond = change_bound_var tr.stop st cond in
          tor cond acc)
        TFalse my_trans
    in
    let cond = fst (Logic_simplification.simplifyCond (tnot cond)) in
    match cond with
        TFalse -> states,trans
      | _ ->
        Aorai_option.debug
          "Adding default transition %s -> %s: %a"
          st.name reject_state.name Promelaoutput.print_condition cond;
        states, new_trans st reject_state (cond,[]) :: trans
  in
  List.fold_left treat_one_state auto intermediate_states

let propagate_epsilon_transitions (states, _ as auto) =
  let rec transitive_closure start (conds,actions) known_states curr =
    let known_states = curr :: known_states in
    let trans = Path_analysis.get_transitions_of_state curr auto in
    List.fold_left
      (fun acc tr ->
        match tr.cross with
          | Epsilon (cond,my_actions) ->
            Aorai_option.debug "Treating epsilon trans %s -> %s"
              curr.name tr.stop.name;
            if List.exists (fun st -> st.nums = tr.stop.nums) known_states
            then acc
            else
              transitive_closure
                start (tand cond conds, my_actions @ actions)
                known_states tr.stop @ acc
          | Normal (cond, action) ->
            Aorai_option.debug "Adding transition %s -> %s from epsilon trans"
              start.name tr.stop.name;
            new_trans start tr.stop (tand cond conds,action @ actions) ::acc)
      [] trans
  in
  let treat_one_state acc st =
    acc @ transitive_closure st (TTrue,[]) [] st
  in
  let trans = List.fold_left treat_one_state [] states in
  (states, trans)

let add_default_trans (states, transitions as auto) otherwise =
  let add_one_trans acc tr =
    let st = tr.start in
    let my_trans = Path_analysis.get_transitions_of_state st auto in
    Aorai_option.debug "Considering new otherwise transition: %s -> %s"
      st.name tr.stop.name;
    let cond =
      List.fold_left
        (fun acc c ->
          let (cond,_) = c.cross in
          Aorai_option.debug "considering trans %s -> %s: %a"
            c.start.name c.stop.name Promelaoutput.print_condition cond;
          let neg = tnot cond in
          Aorai_option.debug "negation: %a"
            Promelaoutput.print_condition neg;
          Aorai_option.debug "acc: %a"
            Promelaoutput.print_condition acc;
         let res = tand acc (tnot cond) in
          Aorai_option.debug "partial result: %a"
            Promelaoutput.print_condition res;
          res
        )
        TTrue
        my_trans
    in
    Aorai_option.debug "resulting transition: %a"
      Promelaoutput.print_condition cond;
    let cond,_ = Logic_simplification.simplifyCond cond in
    let new_trans = new_trans st tr.stop (cond,[]) in
    new_trans::acc
  in
  let transitions = List.fold_left add_one_trans transitions otherwise in
  states, transitions

let type_cond_auto (st,tr as auto) =
  let otherwise = List.filter (fun t -> t.cross = Otherwise) tr in
  let add_if_needed acc st =
    if List.memq st acc then acc else st::acc
  in
  let type_trans (states,transitions,add_reject) tr =
    let (intermediate_states, trans, needs_reject) = type_trans auto [] tr in
    Aorai_option.debug
      "Considering parsed transition %s -> %s" tr.start.name tr.stop.name;
    Aorai_option.debug
      "Resulting transitions:@\n%a@\nEnd of transitions"
      (Pretty_utils.pp_list ~sep:"@\n"
         (fun fmt tr ->
           Format.fprintf fmt "%s -> %s: %a"
             tr.start.name tr.stop.name print_epsilon_trans tr.cross))
      trans;
    let add_reject =
      if needs_reject then
        (List.filter 
           (fun x -> not (Aorai_state.equal tr.start x || 
                            Aorai_state.equal tr.stop x))
           intermediate_states) @ add_reject
      else add_reject
    in
    (List.fold_left add_if_needed states intermediate_states,
     transitions @ trans,
     add_reject)
  in
  let (states, trans, add_reject) =
    List.fold_left type_trans (st,[],[]) tr
  in
  let auto = propagate_epsilon_transitions (states, trans) in
  let auto = add_reject_trans auto add_reject in
  let (states, transitions as auto) = add_default_trans auto otherwise in
  (* nums (and in the past numt) are used as indices in arrays. Therefore, we
     must ensure that we use consecutive numbers starting from 0, or we'll
     have needlessly long arrays.
   *)
  let (states, transitions as auto) =
    match Reject_state.get_option () with
      | Some state -> 
          (states, (new_trans state state (TTrue,[])):: transitions)
      | None -> auto
  in
  if Aorai_option.debug_atleast 1 then
    Promelaoutput.output_dot_automata auto "aorai_debug_typed.dot";
  let (_,trans) =
    List.fold_left
      (fun (i,l as acc) t ->
        let cond, action = t.cross in
        let cond = fst (Logic_simplification.simplifyCond cond)
        in match cond with
            TFalse -> acc
          | _ -> (i+1,{ t with cross = (cond,action); numt = i } :: l))
      (0,[]) transitions
  in
  let _, states =
    List.fold_left
      (fun (i,l as acc) s ->
        if
          List.exists
            (fun t -> t.start.nums = s.nums || t.stop.nums = s.nums)
            trans
        then begin
          s.nums <- i;
          (i+1, s :: l)
        end else acc)
      (0,[]) states
  in
   (List.rev states, List.rev trans)

(** Stores the buchi automaton and its variables and
    functions as it is returned by the parsing *)
let setAutomata auto =
  let auto = type_cond_auto auto in
  automata:=auto;
  check_states "typed automata";
  if Aorai_option.debug_atleast 1 then
    Promelaoutput.output_dot_automata auto "aorai_debug_reduced.dot";
  if (Array.length !cond_of_parametrizedTransitions) <
    (getNumberOfTransitions  ())
  then
    (* all transitions have a true parameterized guard, i.e. [[]] *)
    cond_of_parametrizedTransitions :=
      Array.make (getNumberOfTransitions  ()) [[]]

let getState num = List.find (fun st -> st.nums = num) (fst !automata)

let getStateName num = (getState num).name

let getTransition num =
  List.find (fun trans -> trans.numt = num) (snd !automata)

(** Initializes some tables according to data from Cil AST. *)
let setCData () =
  let (f_decl,f_def) =
    Globals.Functions.fold
      (fun f (lf_decl,lf_def) ->
	 let name = (Kernel_function.get_name f) in
	 match f.fundec with
	   | Definition _ -> (lf_decl,name::lf_def)
	   | Declaration _ -> (name::lf_decl,lf_def))
      ([],[])
  in
  functions_from_c:=f_def;
  ignored_functions:=f_decl;
  variables_from_c:=
    Globals.Vars.fold
    (fun v _ lv ->
      Pretty_utils.sfprintf "%a" Cil_datatype.Varinfo.pretty_vname v :: lv)
    []

(** Return the list of all function name observed in the C file, except ignored functions. *)
let getFunctions_from_c () =
  (!functions_from_c)

(** Return the list of all variables name observed in the C file. *)
let getVariables_from_c () =
  (!variables_from_c)

(** Return the list of names of all ignored functions. A function is ignored if it is used in C file and if its declaration is unavailable. *)
let getIgnoredFunctions () =
  (!ignored_functions)

(** Return the list of names of all ignored functions. A function is ignored if it is used in C file and if its declaration is unavailable. *)
let addIgnoredFunction fname =
  ignored_functions:=fname::(!ignored_functions)

(** Return true if and only if the given string fname denotes an ignored function. *)
let isIgnoredFunction fname =
  List.exists
    (fun s -> (String.compare fname s)=0)
    (!ignored_functions)

let is_reject_state state =
  match Reject_state.get_option () with
      None -> false
    | Some state' -> Aorai_state.equal state state'

(* ************************************************************************* *)
(* Table giving the varinfo structure associated to a given variable name *)
(* In practice it contains all variables (from promela and globals from C file) and only variables *)
let varinfos = Hashtbl.create 97
let paraminfos = Hashtbl.create 97

(* Add a new variable into the association table name -> varinfo *)
let set_varinfo name vi =
  Hashtbl.add varinfos name vi

(* Given a variable name, it returns its associated varinfo.
    If the variable is not found then an error message is print and an assert false is raised. *)
let get_varinfo name =
  try
    Hashtbl.find varinfos name
  with Not_found -> raise_error ("Variable not declared ("^name^")")

let get_logic_var name =
  let vi = get_varinfo name in Cil.cvar_to_lvar vi

(* Same as get_varinfo, but the result is an option.
   Hence, if the variable is not found then None is return. *)
let get_varinfo_option name =
  try
    Some(Hashtbl.find varinfos name)
  with
    | Not_found -> None

(* Add a new param into the association table (funcname,paramname) -> varinfo *)
let set_paraminfo funcname paramname vi =
  (* Aorai_option.log "Adding %s(...,%s,...) " funcname paramname; *)
  Hashtbl.add paraminfos (funcname,paramname)  vi

(* Given a function name and a param name, it returns the varinfo associated to the given param.
    If the variable is not found then an error message is print and an assert false is raised. *)
let get_paraminfo funcname paramname =
  try
    Hashtbl.find paraminfos (funcname,paramname)
  with Not_found ->
    raise_error
      ("Parameter '"^paramname^"' not declared for function '"^funcname^"'.")

(* Add a new param into the association table funcname -> varinfo *)
let set_returninfo funcname vi =
  (* Aorai_option.log "Adding return %s(...) " funcname ; *)
  Hashtbl.add paraminfos (funcname,"\\return")  vi

(* Given a function name, it returns the varinfo associated to the given param.
    If the variable is not found then an error message is print and an assert false is raised. *)
let get_returninfo funcname =
  try
    Hashtbl.find paraminfos (funcname,"\\return")
  with Not_found ->
    raise_error ("Return varinfo not declared for function '"^funcname^"'.")

type range =
  | Fixed of int (** constant value *)
  | Interval of int * int (** range of values *)
  | Bounded of int * term (** range bounded by a logic term (depending on
                              program parameter).
                           *)
  | Unbounded of int (** only the lower bound is known,
                         there is no upper bound *)

module Range = Datatype.Make_with_collections
  (struct
      type t = range
      let name = "Data_for_aorai.Range"
      let rehash = Datatype.identity
      let structural_descr = Structural_descr.t_abstract
      let reprs =
        Fixed 0 :: Interval (0,1) ::  Unbounded 0 ::
          List.map (fun x -> Bounded (0,x)) Cil_datatype.Term.reprs
      let equal = Datatype.from_compare
      let compare x y =
        match x,y with
            | Fixed c1, Fixed c2 -> Datatype.Int.compare c1 c2
            | Fixed _, _ -> 1
            | _, Fixed _ -> -1
            | Interval (min1,max1), Interval(min2, max2) ->
              let c1 = Datatype.Int.compare min1 min2 in
              if c1 = 0 then Datatype.Int.compare max1 max2 else c1
            | Interval _, _ -> 1
            | _,Interval _ -> -1
            | Bounded (min1,max1), Bounded(min2,max2) ->
              let c1 = Datatype.Int.compare min1 min2 in
              if c1 = 0 then Cil_datatype.Term.compare max1 max2 else c1
            | Bounded _, _ -> 1
            | _, Bounded _ -> -1
            | Unbounded c1, Unbounded c2 -> Datatype.Int.compare c1 c2
      let hash = function
        | Fixed c1 -> 2 * c1
        | Interval(c1,c2) -> 3 * (c1 + c2)
        | Bounded (c1,c2) -> 5 * (c1 + Cil_datatype.Term.hash c2)
        | Unbounded c1 -> 7 * c1
      let copy = function
        | Fixed c1 ->
          Fixed (Datatype.Int.copy c1)
        | Interval(c1,c2) ->
          Interval(Datatype.Int.copy c1, Datatype.Int.copy c2)
        | Bounded(c1,c2) ->
          Bounded(Datatype.Int.copy c1, Cil_datatype.Term.copy c2)
        | Unbounded c1 -> Unbounded (Datatype.Int.copy c1)
      let internal_pretty_code _ = Datatype.from_pretty_code
      let pretty fmt = function
        | Fixed c1 -> Format.fprintf fmt "%d" c1
        | Interval (c1,c2) ->
          Format.fprintf fmt "@[<2>[%d..@;%d]@]" c1 c2
        | Bounded(c1,c2) ->
          Format.fprintf fmt "@[<2>[%d..@;%a]@]" c1
            Cil_datatype.Term.pretty c2
        | Unbounded c1 -> Format.fprintf fmt "[%d..]" c1
      let varname _ = "r"
      let mem_project = Datatype.never_any_project
   end)

module Intervals = Cil_datatype.Term.Map.Make(Range)

module Vals = Cil_datatype.Term.Map.Make(Intervals)

(* If we have a bound for the number of iteration, the counter cannot grow
   more than bound (we go to a rejection state otherwise).
*)
let absolute_range loc min =
  let max = find_max_value loc in
  match max with
    | Some { term_node = TConst(Integer (t,_)) } ->
      Interval(min,Integer.to_int t)
    | Some x ->
      Bounded (min, Logic_const.term x.term_node x.term_type)
    | None -> Unbounded min

let merge_range loc base r1 r2 =
  match r1,r2 with
    | Fixed c1, Fixed c2 when Datatype.Int.compare c1 c2 = 0 -> r1
    | Fixed c1, Fixed c2 ->
      let min, max =
        if Datatype.Int.compare c1 c2 <= 0 then c1,c2 else c2,c1 in
      Interval (min,max)
    | Fixed c1, Interval(min,max) ->
      let min = if Datatype.Int.compare c1 min <= 0 then c1 else min in
      let max = if Datatype.Int.compare max c1 <= 0 then c1 else max in
      Interval (min,max)
    | Fixed c1, Bounded(min,_) ->
      let min = if Datatype.Int.compare c1 min <= 0 then c1 else min in
      Unbounded min
    | Fixed c1, Unbounded min ->
      let min = if Datatype.Int.compare c1 min <= 0 then c1 else min in
      Unbounded min
    | Interval(min,max), Fixed c ->
      if Datatype.Int.compare c min < 0 || Datatype.Int.compare c max > 0 then
        begin
          let min = if Datatype.Int.compare c min < 0 then c else min in
          if Cil.isLogicZero base then
            absolute_range loc min
          else Unbounded min
        end else r1
    | Interval(min1,max1), Interval(min2,max2) ->
      if Datatype.Int.compare min2 min1 < 0
        || Datatype.Int.compare max2 max1 > 0 then
        begin
          let min =
            if Datatype.Int.compare min2 min1 < 0 then min2 else min1
          in
          if Cil.isLogicZero base then
            absolute_range loc min
          else Unbounded min
        end else r1
    | Interval(min1,_), (Bounded(min2,_) | Unbounded min2)->
      let min = if Datatype.Int.compare min1 min2 <= 0 then min1 else min2 in
      Unbounded min
    | Bounded(min1,max1), Bounded(min2,max2)
      when Cil_datatype.Term.equal max1 max2 ->
      let min =
        if Datatype.Int.compare min2 min1 < 0 then min2 else min1
      in
      Bounded(min,max1)
    | Bounded(min1,_),
      (Fixed min2 | Interval(min2,_) | Bounded (min2,_) | Unbounded min2) ->
      let min =
        if Datatype.Int.compare min2 min1 < 0 then min2 else min1
      in Unbounded min
    | Unbounded min1,
        (Fixed min2 | Interval(min2,_) | Bounded (min2,_) | Unbounded min2) ->
      let min =
        if Datatype.Int.compare min2 min1 < 0 then min2 else min1
      in Unbounded min

let tlval lv = Logic_const.term (TLval lv) (Cil.typeOfTermLval lv)

let included_range range1 range2 =
  match range1, range2 with
    | Fixed c1, Fixed c2 -> Datatype.Int.equal c1 c2
    | Fixed c, Interval(l,h) ->
      Datatype.Int.compare l c <= 0 && Datatype.Int.compare c h <= 0
    | Fixed _, Bounded _ -> false
    | Fixed c1, Unbounded c2 -> Datatype.Int.compare c1 c2 >= 0
    | Interval (l1,h1), Interval(l2,h2) ->
      Datatype.Int.compare l1 l2 >= 0 && Datatype.Int.compare h1 h2 <= 0
    | Interval (l1,_), Unbounded l2 ->
      Datatype.Int.compare l1 l2 >= 0
    | Interval _, (Fixed _ | Bounded _ ) -> false
    | Bounded _, (Fixed _ | Interval _) -> false
    | Bounded(l1,h1), Bounded(l2,h2) ->
      Datatype.Int.compare l1 l2 >= 0 && Cil_datatype.Term.equal h1 h2
    | Bounded(l1,_), Unbounded l2 -> Datatype.Int.compare l1 l2 <= 0
    | Unbounded l1, Unbounded l2 -> Datatype.Int.compare l1 l2 <= 0
    | Unbounded _, (Fixed _ | Interval _ | Bounded _) -> false

let unchanged loc =
  Cil_datatype.Term.Map.add loc (Fixed 0) Cil_datatype.Term.Map.empty

let merge_bindings tbl1 tbl2 =
  let merge_range loc = Extlib.merge_opt (merge_range loc) in
  let merge_vals loc tbl1 tbl2 =
    match tbl1, tbl2 with
        | None, None -> None
        | Some tbl, None | None, Some tbl ->
            Some
              (Cil_datatype.Term.Map.merge 
                 (merge_range loc) tbl (unchanged loc))
        | Some tbl1, Some tbl2 ->
          Some (Cil_datatype.Term.Map.merge (merge_range loc) tbl1 tbl2)
  in
  Cil_datatype.Term.Map.merge merge_vals tbl1 tbl2

module End_state = 
  Aorai_state.Map.Make(Datatype.Triple(Aorai_state.Set)(Aorai_state.Set)(Vals))

type end_state = End_state.t

(** The data associated to each statement: We have a mapping from each
    possible state at the entrance to the function (before actual transition)
    to the current state possibles, associated to any action that has occured
    on that path.
 *)
module Case_state = Aorai_state.Map.Make(End_state)

type state = Case_state.t

let pretty_end_state start fmt tbl =
  Aorai_state.Map.iter
    (fun stop (fst,last, actions) ->
      Format.fprintf fmt
        "Possible path from %s to %s@\n  Initial trans:@\n"
        start.Promelaast.name stop.Promelaast.name;
      Aorai_state.Set.iter
        (fun state ->
          Format.fprintf fmt "    %s -> %s@\n" 
            start.Promelaast.name
            state.Promelaast.name)
        fst;
      Format.fprintf fmt "  Final trans:@\n";
      Aorai_state.Set.iter
        (fun state ->
          Format.fprintf fmt "    %s -> %s@\n"
            state.Promelaast.name stop.Promelaast.name)
        last;
      Format.fprintf fmt "  Related actions:@\n";
      Cil_datatype.Term.Map.iter
        (fun loc tbl ->
          Cil_datatype.Term.Map.iter
            (fun base itv ->
              Format.fprintf fmt "  %a <- %a + %a@\n"
                Cil_datatype.Term.pretty loc
                Cil_datatype.Term.pretty base
                Range.pretty itv)
            tbl)
        actions)
    tbl

let pretty_state fmt cases =
  Aorai_state.Map.iter (fun start tbl -> pretty_end_state start fmt tbl) cases

let included_state tbl1 tbl2 =
  try
    Aorai_state.Map.iter
      (fun s1 tbl1 ->
        let tbl2 = Aorai_state.Map.find s1 tbl2 in
        Aorai_state.Map.iter
          (fun s2 (fst1, last1, tbl1) ->
            let (fst2, last2, tbl2) = Aorai_state.Map.find s2 tbl2 in
            if not (Aorai_state.Set.subset fst1 fst2)
              || not (Aorai_state.Set.subset last1 last2)
            then raise Not_found;
            Cil_datatype.Term.Map.iter
              (fun base bindings1 ->
                let bindings2 =
                  Cil_datatype.Term.Map.find base tbl2
                in
                Cil_datatype.Term.Map.iter
                  (fun loc range1 ->
                    let range2 = Cil_datatype.Term.Map.find loc bindings2 in
                    if not 
                      (included_range range1 range2) then raise Not_found)
                  bindings1)
              tbl1)
          tbl1)
      tbl1;
    true
  with Not_found -> false

let merge_end_state tbl1 tbl2 =
  let merge_stop_state _ (fst1, last1, tbl1) (fst2, last2, tbl2) = 
    let fst = Aorai_state.Set.union fst1 fst2 in
    let last = Aorai_state.Set.union last1 last2 in
    let tbl = merge_bindings tbl1 tbl2 in
    (fst, last, tbl)
  in
  Aorai_state.Map.merge (Extlib.merge_opt merge_stop_state) tbl1 tbl2

let merge_state tbl1 tbl2 =
  let merge_state _ = merge_end_state in
  Aorai_state.Map.merge (Extlib.merge_opt merge_state) tbl1 tbl2

module Pre_state = 
  Kernel_function.Make_Table
    (Case_state)
    (struct
        let name = "Data_for_aorai.Pre_state"
        let dependencies = 
          [ Ast.self; Aorai_option.Ya.self; Aorai_option.Ltl_File.self;
            Aorai_option.To_Buchi.self; Aorai_option.Deterministic.self ]
        let size = 17
     end)

let set_kf_init_state kf state =
  let change old_state = merge_state old_state state in
  let set _ = state in
  ignore (Pre_state.memo ~change set kf)

let dkey = Aorai_option.register_category "dataflow"

let replace_kf_init_state kf state = 
  Aorai_option.debug ~dkey
    "Replacing pre-state of %a:@\n  @[%a@]"
    Kernel_function.pretty kf pretty_state state;
  Pre_state.replace kf state

let get_kf_init_state kf =
  try
    Pre_state.find kf
  with Not_found -> Aorai_state.Map.empty

module Post_state = 
  Kernel_function.Make_Table
    (Case_state)
    (struct
        let name = "Data_for_aorai.Post_state"
        let dependencies = 
          [ Ast.self; Aorai_option.Ya.self; Aorai_option.Ltl_File.self;
            Aorai_option.To_Buchi.self; Aorai_option.Deterministic.self ]
        let size = 17
     end)

let set_kf_return_state kf state =
  let change old_state = merge_state old_state state in
  let set _ = state in
  ignore (Post_state.memo ~change set kf)

let replace_kf_return_state = Post_state.replace

let get_kf_return_state kf =
  try
    Post_state.find kf
  with Not_found -> Aorai_state.Map.empty

module Loop_init_state =
  State_builder.Hashtbl
    (Cil_datatype.Stmt.Hashtbl)
    (Case_state)
    (struct
        let name = "Data_for_aorai.Loop_init_state"
        let dependencies =
          [ Ast.self; Aorai_option.Ya.self; Aorai_option.Ltl_File.self;
            Aorai_option.To_Buchi.self; Aorai_option.Deterministic.self ]
        let size = 17
     end)

let set_loop_init_state stmt state =
  let change old_state = merge_state old_state state in
  let set _ = state in
  ignore (Loop_init_state.memo ~change set stmt)

let replace_loop_init_state = Loop_init_state.replace

let get_loop_init_state stmt =
  try
    Loop_init_state.find stmt
  with Not_found -> Aorai_state.Map.empty

module Loop_invariant_state =
  State_builder.Hashtbl
    (Cil_datatype.Stmt.Hashtbl)
    (Case_state)
    (struct
        let name = "Data_for_aorai.Loop_invariant_state"
        let dependencies =
          [ Ast.self; Aorai_option.Ya.self; Aorai_option.Ltl_File.self;
            Aorai_option.To_Buchi.self; Aorai_option.Deterministic.self ]
        let size = 17
     end)

let set_loop_invariant_state stmt state =
  let change old_state = merge_state old_state state in
  let set _ = state in
  ignore (Loop_invariant_state.memo ~change set stmt)

let replace_loop_invariant_state = Loop_invariant_state.replace

let get_loop_invariant_state stmt =
  try Loop_invariant_state.find stmt
  with Not_found -> Aorai_state.Map.empty

let pretty_pre_state fmt =
  Pre_state.iter
    (fun kf state -> 
      Format.fprintf fmt "Function %a:@\n  @[%a@]@\n"
        Kernel_function.pretty kf pretty_state state)

let pretty_post_state fmt =
  Post_state.iter
    (fun kf state -> 
      Format.fprintf fmt "Function %a:@\n  @[%a@]@\n"
        Kernel_function.pretty kf pretty_state state)

let pretty_loop_init fmt =
  Loop_init_state.iter
    (fun stmt state -> 
      let kf = Kernel_function.find_englobing_kf stmt in
      Format.fprintf fmt "Function %a, sid %d:@\n  @[%a@]@\n"
        Kernel_function.pretty kf stmt.sid pretty_state state)

let pretty_loop_invariant fmt =
  Loop_invariant_state.iter
    (fun stmt state -> 
      let kf = Kernel_function.find_englobing_kf stmt in
      Format.fprintf fmt "Function %a, sid %d:@\n  @[%a@]@\n"
        Kernel_function.pretty kf stmt.sid pretty_state state)

let debug_computed_state ?(dkey=dkey) () =
  Aorai_option.debug ~dkey
    "Computed state:@\nPre-states:@\n  @[%t@]@\nPost-states:@\n  @[%t@]@\n\
     Loop init:@\n  @[%t@]@\nLoop invariants:@\n  @[%t@]"
    pretty_pre_state pretty_post_state pretty_loop_init pretty_loop_invariant

(* ************************************************************************* *)

let removeUnusedTransitionsAndStates () =
  (* Step 1 : computation of reached states and crossed transitions *)
  let treat_one_state state map set =
    Aorai_state.Map.fold
      (fun state (fst, last, _) set -> 
          Aorai_state.Set.add state 
            (Aorai_state.Set.union last
               (Aorai_state.Set.union fst set)))
      map
      (Aorai_state.Set.add state set)
  in
  let reached _ state set = Aorai_state.Map.fold treat_one_state state set in
  let reached_states = Pre_state.fold reached Aorai_state.Set.empty in
  let reached_states = Post_state.fold reached reached_states in
  let reached_states = Loop_init_state.fold reached reached_states in
  let reached_states = Loop_invariant_state.fold reached reached_states in
  (* Step 2 : computation of translation tables *)
  let state_list =
    List.sort 
      (fun x y -> Datatype.String.compare x.Promelaast.name y.Promelaast.name)
      (Aorai_state.Set.elements reached_states)
  in
  let (_, translate_table) =
    List.fold_left
      (fun (i,map) x ->
        let map = Aorai_state.Map.add x { x with nums = i } map in (i+1,map))
      (0,Aorai_state.Map.empty) state_list
  in
  let new_state s = Aorai_state.Map.find s translate_table in
  let (_, trans_list) =
    List.fold_left
      (fun (i,list as acc) trans ->
        try
          let new_start = new_state trans.start in
          let new_stop = new_state trans.stop in
          (i+1,
           { trans with start = new_start; stop = new_stop; numt = i } :: list)
        with Not_found -> acc)
      (0,[]) (snd (getAutomata()))
  in
  let state_list = List.map new_state state_list in
  Reject_state.may
    (fun reject_state ->
      try
        let new_reject = Aorai_state.Map.find reject_state translate_table in
        Reject_state.set new_reject
      with Not_found -> Reject_state.clear ());
  (* Step 3 : rewriting stored information *)
  automata:= (state_list,trans_list);
  check_states "reduced automata";

  let rewrite_state state =
    let rewrite_set set =
      Aorai_state.Set.fold 
        (fun s set -> Aorai_state.Set.add (new_state s) set) 
        set Aorai_state.Set.empty
    in
    let rewrite_bindings (fst_states, last_states, bindings) =
      (rewrite_set fst_states, rewrite_set last_states, bindings)
    in
    let rewrite_curr_state s bindings acc =
      let new_s = new_state s in
      let bindings = rewrite_bindings bindings in
      Aorai_state.Map.add new_s bindings acc
    in
    let rewrite_one_state s map acc =
      let new_s = new_state s in
      let new_map =
        Aorai_state.Map.fold rewrite_curr_state map Aorai_state.Map.empty
      in
      Aorai_state.Map.add new_s new_map acc
    in
    Aorai_state.Map.fold rewrite_one_state state Aorai_state.Map.empty
  in
  Pre_state.iter (fun kf state -> Pre_state.replace kf (rewrite_state state));
  Post_state.iter (fun kf state -> Post_state.replace kf (rewrite_state state));
  Loop_init_state.iter 
    (fun s state -> Loop_init_state.replace s (rewrite_state state));
  Loop_invariant_state.iter
    (fun s state -> Loop_invariant_state.replace s (rewrite_state state))

(* ************************************************************************* *)
(* Given the name of a function, it return the name of the associated element in the operation list. *)
let func_to_op_func f = "op_"^f

let used_enuminfo = Hashtbl.create 2

let set_usedinfo name einfo =
  Hashtbl.add used_enuminfo name einfo

let get_usedinfo name =
  try Hashtbl.find used_enuminfo name
  with Not_found -> raise_error ("Incomplete enum information.")

let get_cenum_option name =
  let opnamed = func_to_op_func name in
    Hashtbl.fold
      (fun _ ei value ->
	 match value with
	   | Some(_) as r -> r (* Already found *)
	   | None ->
	       let rec search = function
		 | {einame = n} as ei ::_ when n=name -> Some(CEnum ei)
		 | {einame = n} as ei ::_ when n=opnamed -> Some(CEnum ei)
		 | _::l -> search l
		 | [] -> None
	       in
	       search ei.eitems
      )
      used_enuminfo
      None

let func_enum_type () = TEnum(Hashtbl.find used_enuminfo listOp,[])
let status_enum_type () = TEnum(Hashtbl.find used_enuminfo listStatus,[])

let func_to_cenum func =
  try
    let ei = Hashtbl.find used_enuminfo listOp in
    let name = func_to_op_func func in
    let rec search = function
      | {einame = n} as ei ::_ when n=name -> CEnum ei
      | _::l -> search l
      | [] -> raise_error
        ("Operation '"^name^"' not found in operations enumeration")
    in
      search ei.eitems
      (*    CEnum(ex,s,ei)*)
  with Not_found -> raise_error ("Operation not found")

let op_status_to_cenum status =
  try
    let ei = Hashtbl.find used_enuminfo listStatus in
    let name = if status = Promelaast.Call then callStatus else termStatus in
    let rec search = function
      | {einame=n} as ei ::_ when n=name -> CEnum ei
      | _::l -> search l
      | [] -> raise_error ("Status not found")
    in
      search ei.eitems
  with Not_found -> raise_error ("Status not found")


(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)

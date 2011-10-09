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

open Promelaast
open Extlib
open Logic_const
open Cil_types
open Cil
open Cil_datatype
open Ast_info
open Spec_tools

(**************************************************************************)

(**
   This visitor does not modify the AST.
   It just generates a first abstract specification for each function.
   This specification is stored into Data_for_aorai and can be accessed 
   by using get_func_pre or get_func_post.
*)
class visit_computing_abstract_pre_post_from_buch 
  (auto:Promelaast.typed_automaton) (root:string) (considerAcceptance:bool)  =

object (self)
  inherit Visitor.frama_c_inplace as super

  method vfunc f =
    let kf = Extlib.the self#current_kf in
    if not (Data_for_aorai.isIgnoredFunction f.svar.vname) then begin
      (* Extraction of a first abstraction of pre/post condition of 
         the current function. 
       *)
      let pre_st,pre_tr  = 
        (Aorai_utils.mk_abstract_pre  auto kf) in
      let post_st,post_tr = 
        (Aorai_utils.mk_abstract_post auto kf) in
      if f.svar.vname = root then
        begin
          (* Pre simplification for Root (only initial states) *)
          List.iter (
            fun tr ->
              if 
                (pre_tr.(tr.Promelaast.numt)) &&
                  ((tr.Promelaast.start.Promelaast.init==Bool3.False) 
                   || not (Aorai_utils.isCrossableAtInit tr kf))
              then
                begin
                  pre_tr.(tr.Promelaast.numt)<- false;
                  pre_st.(tr.Promelaast.stop.Promelaast.nums)<- false
                end
          ) (snd auto);

          List.iter (
            fun tr ->
              if (pre_tr.(tr.Promelaast.numt)) then
                pre_st.(tr.Promelaast.stop.Promelaast.nums) <- true
          ) (snd auto);

          if considerAcceptance then begin
            (* Post simplification for Root (Only acceptance states) *)
            List.iter (
              fun tr ->
                if (post_tr.(tr.Promelaast.numt)) &&
                  (tr.Promelaast.stop.Promelaast.acceptation==Bool3.False)
                then
                  begin
                    post_tr.(tr.Promelaast.numt)<- false;
                    post_st.(tr.Promelaast.stop.Promelaast.nums)<- false
                  end
            ) (snd auto);
            List.iter (
              fun tr ->
                if (post_tr.(tr.Promelaast.numt)) then
                  post_st.(tr.Promelaast.stop.Promelaast.nums) <- true
            ) (snd auto)
          end
        end;
      Data_for_aorai.set_func_pre f.svar.vname (pre_st, pre_tr) ;
      Data_for_aorai.set_func_post f.svar.vname (post_st, post_tr)
    end;
    DoChildren
end

let get_call_name exp = match exp.enode with
  | Const(CStr(s)) -> s
  | Lval(Var(vi),NoOffset) -> vi.vname
  | _ ->
    Aorai_option.not_yet_implemented
      "At this time, only explicit calls are allowed by the Aorai plugin."

(****************************************************************************)

(* The instrumentation is done in two passes:
   1) creating auxiliary functions for each non-ignored C function, that update
   automaton's state when entering and exiting the function
   2) generating specifications for all the functions.

   We maintain tables from aux to orig so that the second visitor knows which
   is which. Note that this tables are cleared after each visit, and thus need 
   not be projectified.
*)

(* the various kinds of auxiliary functions. *)
type func_auto_mode =
    Not_auto_func (* original C function. *)
  | Pre_func of kernel_function (* Pre_func f denotes a function updating 
                                   the automaton before call to f. *)
  | Post_func of kernel_function (* Post_func f denotes a function updating
                                    the automaton when returning from f. *)

(* table from auxiliary functions to the corresponding original one. *)
let func_orig_table = Cil_datatype.Varinfo.Hashtbl.create 17

let kind_of_func vi =
  try Cil_datatype.Varinfo.Hashtbl.find func_orig_table vi
  with Not_found -> Not_auto_func

(**
   This visitor adds an auxiliary function for each C function which takes
   care of setting the automaton in a correct state before calling the 
   original one, and replaces each occurrence of the original function by
   the auxiliary one. It also takes care of changing the automaton at function's
   return.
*)
class visit_adding_code_for_synchronisation (auto:Promelaast.typed_automaton) =
object (self)
  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  val aux_post_table = Kernel_function.Hashtbl.create 17

  method vglob_aux g =
    match g with
    | GFun (fundec,loc) ->
        (* TODO: generate the aux_func_post *)
      let kf = Extlib.the self#current_kf in
      let vi = Kernel_function.get_vi kf in
      let vi_pre = Cil_const.copy_with_new_vid vi in
      vi_pre.vname <- Data_for_aorai.get_fresh (vi_pre.vname ^ "_pre_func");
      Cil_datatype.Varinfo.Hashtbl.add func_orig_table vi_pre (Pre_func kf);
        (* TODO: 
           - what about protos that have no specified args 
           (NB: cannot be identified here because of implem of Kernel_function).
           - what about varargs?
         *)
      let (rettype,args,varargs,_) = Cil.splitFunctionTypeVI vi_pre in
      vi_pre.vtype <- TFun(Cil.voidType, args, varargs,[]);
      vi_pre.vattr <- []; 
        (* in particular get rid of __no_return if set in vi*)
      let arg = 
        if Cil.isVoidType rettype 
        then []
        else ["res",rettype,[]]
      in
      let vi_post = 
        Cil.makeGlobalVar 
          (Data_for_aorai.get_fresh (vi.vname ^ "_post_func"))
          (TFun(voidType,Some arg,false,[]))
      in
      Kernel_function.Hashtbl.add aux_post_table kf vi_post;
      Cil_datatype.Varinfo.Hashtbl.add func_orig_table vi_post (Post_func kf);
      let globs = 
        [ GVarDecl(Cil.empty_funspec (), vi_pre, loc);
          GVarDecl(Cil.empty_funspec (), vi_post,loc) ]
      in
      fundec.sbody.bstmts <-
        Cil.mkStmtOneInstr
        (Call(None,Cil.evar ~loc vi_pre,
              List.map (fun x -> Cil.evar ~loc x)
                (Kernel_function.get_formals kf),
              loc))
      :: fundec.sbody.bstmts;
      ChangeDoChildrenPost([g], fun x -> globs @ x)
    | _ -> DoChildren

  method vstmt_aux stmt =
    match stmt.skind with
      | Return (res,loc)  ->
        let kf = Extlib.the self#current_kf in
        let vi = Kernel_function.get_vi kf in
        let current_function = vi.vname in
        if not (Data_for_aorai.isIgnoredFunction current_function) then begin
          let args = match res with
            | None -> []
            | Some exp -> [Cil.copy_exp exp]
          in
          let aux_vi = Kernel_function.Hashtbl.find aux_post_table kf in
          let call = 
            mkStmtOneInstr (Call (None,Cil.evar ~loc aux_vi,args,loc))
          in
          let new_return = mkStmt ~valid_sid:true stmt.skind in
          let new_stmts = [call; new_return] in
          stmt.skind<-Block(Cil.mkBlock(new_stmts))
        end;
        SkipChildren
      | _ -> DoChildren

end

(*********************************************************************)

(* update from formals of original C function to one of the auxiliary  
   function (f_aux or f_pre)
 *)
class change_formals old_kf new_kf =
  let old_formals = Kernel_function.get_formals old_kf in
  let new_formals = Kernel_function.get_formals new_kf in
  let formals = List.combine old_formals new_formals in
object
  inherit Visitor.frama_c_inplace
  method vlogic_var_use lv =
    match lv.lv_origin with
      | None -> SkipChildren
      | Some vi ->
        try 
          let vi'= List.assq vi formals in
          ChangeTo (Cil.cvar_to_lvar vi')
        with Not_found -> SkipChildren
end

(* update \result to param of f_post when it exists. Must not be called if
   f_post has no parameter (original f returns void). *)
class change_result new_kf =
  let v = List.hd (Kernel_function.get_formals new_kf) in
object
  inherit Visitor.frama_c_inplace
  method vterm_lhost lh =
    match lh with
        TResult _ -> ChangeTo (TVar (Cil.cvar_to_lvar v))
      | _ -> DoChildren
end

let post_treatment_loops = Hashtbl.create 97

let get_action_invariant kf ki (status,_) =
  let (state,_ as auto) = Data_for_aorai.getAutomata () in
  let treat_one_state pre_state post_state =
    let trans = Path_analysis.get_transitions_of_state pre_state auto in
    if List.exists (fun x -> status.(x.stop.nums).(post_state.nums)) trans then
        begin
          let bindings =
            Data_for_aorai.get_action_path kf ki pre_state post_state
          in
          List.map (Aorai_utils.update_to_pred post_state) bindings
        end 
      else []
  in
  List.flatten (Extlib.product treat_one_state state state)

let get_action_post_cond ?(pre_states=[]) ~post_states kf =
  let (_, transitions) = Data_for_aorai.getAutomata () in
  let pre_st, pre_tr = 
    Data_for_aorai.get_func_pre (Kernel_function.get_name kf)
  in
  let pre_states =
    match pre_states with
      | [] ->
        let (states,_) = Data_for_aorai.getAutomata () in
        List.filter (fun x -> pre_st.(x.nums)) states
      | _ -> pre_states
  in
  let pre_states =
    List.fold_left
      (fun acc tr -> 
        if pre_tr.(tr.numt) &&
          List.exists
          (fun x -> Data_for_aorai.Aorai_state.equal x tr.stop) pre_states
        then Data_for_aorai.Aorai_state.Set.add tr.start acc else acc)
      Data_for_aorai.Aorai_state.Set.empty transitions
  in
  let pre_states = Data_for_aorai.Aorai_state.Set.elements pre_states in
  let treat_one_path pre_state post_state =
    let post_conds = Aorai_utils.action_to_pred ~pre_state ~post_state kf in
    Aorai_option.debug ~dkey:"action"
      "Getting action post-conditions for %a, from state %s to state %s@\n%a"
      Kernel_function.pretty kf
      pre_state.Promelaast.name post_state.Promelaast.name
      (Pretty_utils.pp_list ~sep:Pretty_utils.nl_sep 
         !Ast_printer.d_predicate_named)
      post_conds;
    let pre = Aorai_utils.is_state_pred pre_state in
    let pre = Logic_const.pold pre in
    let post = Aorai_utils.is_state_pred post_state in
    List.map
      (fun p -> (Logic_const.pimplies (Logic_const.pand (pre,post), p)))
      post_conds
  in
  let post_cond = 
    List.flatten (Extlib.product treat_one_path pre_states post_states)
  in
  List.map 
    (fun post_cond -> (Normal, Logic_const.new_predicate post_cond))
    post_cond

(**
   This visitor add a specification to each fonction and to each loop,
   according to specifications stored into Data_for_aorai.
*)
class visit_adding_pre_post_from_buch auto treatloops =

  let predicate_to_invariant kf stmt pred =
    (* 4) Add new annotation *)
    Annotations.add
      kf
      stmt
      [ (*Ast.self; *)
        (*Aorai_option.Ltl_File.self;
        Aorai_option.Buchi.self;
        Aorai_option.Ya.self ;*)
        (*Aorai_option.AbstractInterpretationOff.self ;*)
        Aorai_option.AbstractInterpretation.self ]
      (User
         (Logic_const.new_code_annotation (AInvariant([],true,pred))));
  in

  (** Given a couple of bool array (States , Transitions),
      this function computes a predicate and add it as an invariant. *)
  let condition_to_invariant kf (st, tr as cond) stmt =
    let pred_authorized = Aorai_utils.pre_post_to_term cond in
    let pred_forbidden =
      Aorai_utils.pre_post_to_term_neg (Array.map not st,tr)
    in
    let pred = Logic_const.pand (pred_authorized, pred_forbidden) in
    predicate_to_invariant kf stmt pred
  in

  let partition_pre_state (post_bc_st,_) =
    let check_one_state (idx, equivs) case =
      if Spec_tools.is_empty_behavior case then (idx+1,equivs)
      else
        let is_equiv i1 = Spec_tools.bool_array_eq post_bc_st.(i1) case in
        let rec aux l =
          match l with
            | [] -> [[idx]]
            | eq::l ->
              if is_equiv (List.hd eq)
              then (idx::eq)::l
              else eq :: aux l
        in
        (idx+1,aux equivs)
    in
    let (_,equivs) = Array.fold_left check_one_state (0,[]) post_bc_st in
    equivs
  in
  let update_assigns loc spec =
    let update_assigns bhv =
      bhv.b_assigns <-
        Logic_utils.concat_assigns bhv.b_assigns (Aorai_utils.aorai_assigns loc)
    in
    List.iter update_assigns spec.spec_behavior
  in
  let mk_auto_fct_spec kf status auto_state =
    let loc = Kernel_function.get_location kf in
    Aorai_utils.auto_func_behaviors loc kf status auto_state
  in
  let mk_pre_fct_spec kf =
    mk_auto_fct_spec kf Promelaast.Call
      (Data_for_aorai.get_func_pre (Kernel_function.get_name kf))
  in
  let mk_post_fct_spec kf =
    mk_auto_fct_spec kf Promelaast.Return
      (Spec_tools.pre_flattening
         (Data_for_aorai.get_func_post_bycase (Kernel_function.get_name kf)))
  in
  let mk_post kf =
    let fct_name = Kernel_function.get_name kf in
    let auto_state_pre = Data_for_aorai.get_func_pre fct_name in
    (* Rewriting arrays characterizing status into predicates *)
    let preds_post_bc = Data_for_aorai.get_func_post_bycase fct_name in
    (*   + Post-condition registration *)
    (* If several states are associated to the same post-condition,
       then their specification is factorised. *)
    let equivs = partition_pre_state preds_post_bc in
    let bhvs =
      match equivs with
        | [ s ] -> (* we just have one possible case, no need to generate
                      assumes and a negative behavior
                    *)
          let name = "Buchi_property_behavior" in
          let i = List.hd s in
          let p =
            Aorai_utils.pre_post_to_term
              ((fst preds_post_bc).(i),(snd preds_post_bc).(i))
          in
          let post_cond = Normal, Logic_const.new_predicate p in
          let post_cond =
            if Aorai_option.Deterministic.get () then [post_cond]
            else begin
              let p =
                Aorai_utils.pre_post_to_term_neg
                  ((Array.map not (fst preds_post_bc).(i)),
                   (snd preds_post_bc).(i))
              in
              [Normal, Logic_const.new_predicate p; post_cond]
            end
          in
          let post_cond =
            match Aorai_utils.get_preds_post_bc_wrt_params kf with
              | None -> post_cond
              | Some p -> (Normal, Logic_const.new_predicate p) :: post_cond
          in
          let post_states =
            List.filter (fun x -> (fst preds_post_bc).(i).(x.nums))
              (fst (Data_for_aorai.getAutomata ()))
          in
          let post_cond =
            post_cond @ get_action_post_cond ~post_states kf
          in
          [Cil.mk_behavior ~name ~post_cond ()]
        | _ ->
          let bhvs =
            List.fold_left 
              (fun acc equiv ->
                let case = List.hd equiv in
                let pre_states = List.map Data_for_aorai.getState equiv in
                let post_states =
                  List.filter (fun x -> (fst preds_post_bc).(case).(x.nums))
                    (fst (Data_for_aorai.getAutomata ()))
                in
                let assumes_l =
                  List.map 
                    (fun i ->
                      Aorai_utils.make_prev_pred
                        kf Promelaast.Call 
                        (Data_for_aorai.getState i) auto_state_pre)
                    equiv
                in
                let name = "Buchi_property_behavior_in_"^(string_of_int case) in
                let assumes =
                  [Logic_const.new_predicate (Logic_const.pors assumes_l)]
                in
                let p =
                  Aorai_utils.pre_post_to_term
                    ((fst preds_post_bc).(case),(snd preds_post_bc).(case))
                in
                let post_cond = Normal, Logic_const.new_predicate p in
                let post_cond =
                  match Aorai_utils.get_preds_post_bc_wrt_params kf with
                    | None -> [post_cond]
                    | Some p -> [Normal, Logic_const.new_predicate p; post_cond]
                in
                let post_cond =
                  post_cond @ 
                    (get_action_post_cond ~pre_states ~post_states kf)
                in
                Cil.mk_behavior ~name ~assumes ~post_cond () :: acc)
              []
              equivs
          in
          if Aorai_option.Deterministic.get () then bhvs
          else begin 
            (* post-conditions for state in which we are not at the 
               end of the functions. They have to be grouped differently
               than positive information because of non-determinism (if two
               non-equivalent states are active when entering the function
               and activate the same state at exit)
             *)
            let rec aux bhvs i =
              if i < 0 then bhvs
              else begin
                let name = 
                  "Buchi_property_behavior_out_" ^ (string_of_int i) in
                let my_preds =
                  List.fold_left 
                    (fun acc equiv ->
                      if (fst preds_post_bc).(List.hd equiv).(i) then
                        acc @ equiv
                      else acc)
                    [] equivs
                in
                let my_preds = List.map Data_for_aorai.getState my_preds in
                let assumes =
                  Aorai_utils.make_prev_pred_neg
                    kf Promelaast.Call my_preds auto_state_pre
                in
                let assumes = [Logic_const.new_predicate assumes] in
                let state = Data_for_aorai.getState i in
                let p = Aorai_utils.is_out_of_state_pred state in
                let post_cond = [Normal, Logic_const.new_predicate p] in
                let bhvs = Cil.mk_behavior ~name ~assumes ~post_cond () :: bhvs
                in aux bhvs (i-1)
              end
            in
            aux bhvs (Data_for_aorai.getNumberOfStates () - 1)
          end
    in
    (* if bycase is set*)
    (* adding require called and behavior ensures return *)
          
    if Aorai_option.AddingOperationNameAndStatusInSpecification.get() 
    then begin
      let called_post = 
        Logic_const.new_predicate 
          (Logic_const.prel 
             (Req ,
              Logic_const.tvar 
                (Data_for_aorai.get_logic_var Data_for_aorai.curOpStatus),
              Logic_const.term 
                (TConst
                   (Data_for_aorai.op_status_to_cenum Promelaast.Return)) 
                (Ctype Cil.intType)))
      in
      let called_post_2 = 
        Logic_const.new_predicate 
          (Logic_const.prel 
             (Req,
              Logic_const.tvar
                (Data_for_aorai.get_logic_var Data_for_aorai.curOp),
              Logic_const.term 
                (TConst(Data_for_aorai.func_to_cenum fct_name)) 
                (Ctype Cil.intType)))
      in
      let name = "Buchi_property_behavior_function_states" in
      let post_cond = [Normal, called_post; Normal, called_post_2] in
      Cil.mk_behavior ~name ~post_cond () :: bhvs
    end else bhvs
  in
object(self)

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  (* We have to update assigns whenever a call occurs in the scope of
     a statement contract (function always update the automaton's state, 
     so assigns there have to be changed anyway.)
  *)
  val has_call = Stack.create ()

  method private enter_block () = Stack.push (ref false) has_call

  method private call () = Stack.iter (fun x -> x := true) has_call

  method private leave_block () = !(Stack.pop has_call)

  method vfunc f =
    let my_kf = Extlib.the self#current_kf in
    let vi = Kernel_function.get_vi my_kf in
    let spec = Kernel_function.get_spec my_kf in
    let loc = Kernel_function.get_location my_kf in
    let fct_name = Kernel_function.get_name my_kf in
    begin
      match kind_of_func vi with
        | Pre_func _ | Post_func _ ->
          Aorai_option.fatal 
            "functions managing automaton's state are \
             not supposed to have a body"
        | Not_auto_func -> (* Normal C function *)
          let bhvs = mk_post my_kf in
          let auto_state_pre = Data_for_aorai.get_func_pre fct_name in
          let requires = 
            Aorai_utils.force_transition 
              loc my_kf Promelaast.Call auto_state_pre
          in
          let bhvs =
            match Cil.find_default_behavior spec with
                Some b ->
                  b.b_requires <- requires @ b.b_requires; bhvs
              | None ->
                let bhv = Cil.mk_behavior ~requires () in
                bhv::bhvs
          in
          spec.spec_behavior <- bhvs @ spec.spec_behavior
    end;
    let after f = update_assigns f.svar.vdecl spec; f in
    ChangeDoChildrenPost(f,after)

  method vglob_aux g =
    match g with
      | GVarDecl(_,v,_) when 
          Cil.isFunctionType v.vtype
          && not (Kernel_function.is_definition (Extlib.the self#current_kf))
        ->
        let my_kf = Extlib.the self#current_kf in
        (* don't use get_spec, as we'd generate default assigns,
           while we'll fill the spec just below. *)
        let spec = my_kf.spec in
        let vi = Kernel_function.get_vi my_kf in
        begin
          match kind_of_func vi with
            | Pre_func kf ->
              (* must advance the automaton according to current call. *)
              let bhvs = mk_pre_fct_spec kf in
              let bhvs =
                Visitor.visitFramacBehaviors (new change_formals kf my_kf) bhvs
              in
              spec.spec_behavior <- bhvs;
              ChangeDoChildrenPost([g],fun x -> x)
            | Post_func kf ->
              (* must advance the automaton according to return event. *)
              let (rt, _, _, _) = 
                Cil.splitFunctionTypeVI (Kernel_function.get_vi kf)
              in
              let bhvs = mk_post_fct_spec kf in
              let bhvs =
                (* if return type is not void, convert \result in the formal
                   arg of current kf. Otherwise, there's no conversion to do.
                 *)
                if Cil.isVoidType rt then bhvs
                else
                  Visitor.visitFramacBehaviors (new change_result my_kf) bhvs
              in
              spec.spec_behavior <- bhvs;
              ChangeDoChildrenPost([g], fun x -> x)
            | Not_auto_func -> DoChildren (* they are not considered here. *)
        end
      | _ -> DoChildren

  method vstmt_aux stmt =
    let treat_loop body_ref stmt =

      (* varinfo of the init_var associated to this loop *)
      let vi_init = 
        Data_for_aorai.get_varinfo 
          (Data_for_aorai.loopInit^"_"^(string_of_int stmt.sid)) 
      in

(*    1) The associated init variable is set to 0 in first position
         (or in second position if the first stmt is a if)*)

      let loc = Cil_datatype.Stmt.loc stmt in
      let stmt_varset =
        Cil.mkStmtOneInstr
          (Set((Var vi_init,NoOffset), Cil.zero ~loc, loc))
      in
      stmt_varset.sid<-(Cil.Sid.next ());
      stmt_varset.ghost<-true;
      begin
        (* Function adapted from the cil printer *)
        try
          let rec skipEmpty = function
              [] -> []
            | {skind=Instr (Skip _);labels=[]} :: rest -> skipEmpty rest
            | x -> x
          in
          match skipEmpty !body_ref.bstmts with
            | {skind=If(_,tb,fb,_)} as head:: _ ->
                begin
                  match skipEmpty tb.bstmts, skipEmpty fb.bstmts with
                    | _, {skind=Break _}:: _
                    | _, {skind=Goto _} :: _
                    | {skind=Goto _} :: _, _
                    | {skind=Break _} :: _, _ ->
                      !body_ref.bstmts <- 
			head :: stmt_varset :: List.tl !body_ref.bstmts
                    | _ ->
                        raise Not_found
                end
            | _ -> raise Not_found
        with
          | Not_found ->
              !body_ref.bstmts<-stmt_varset::!body_ref.bstmts
      end;

(*    2) The associated init variable is set to 1 before the loop *)
      let new_loop = mkStmt stmt.skind in
      new_loop.sid<-(Cil.Sid.next ());
      let stmt_varset =
        Cil.mkStmtOneInstr 
          (Set((Var(vi_init),NoOffset), Cil.one ~loc, loc))
      in
      stmt_varset.sid <- Cil.Sid.next ();
      stmt_varset.ghost <- true;
      let block = mkBlock [stmt_varset;new_loop] in
      stmt.skind<-Block(block);

(*    3) Generation of the loop invariant *)
      let mk_imply operator predicate =
        pimplies
          (prel(operator,
                Aorai_utils.mk_term_from_vi vi_init,
                Aorai_utils.zero_term()),
               predicate)
      in
(* The loop invariant is :
     (Global invariant)  // all never reached state are set to zero
   & (Pre2)              // internal pre-condition
   & (Init => Pre1)      // external pre-condition
   & (not Init => Post2) // internal post-condition
   & counter_invariant   // values of counters. 
   (init : fresh variable which indicates if the iteration is the first one).
*)
      let kf = Extlib.the self#current_kf in
      let global_loop_inv = Aorai_utils.get_global_loop_inv stmt in
      condition_to_invariant kf global_loop_inv new_loop;

      let pre2 = Aorai_utils.get_restricted_int_pre_bc stmt in
      if pre2.content <> Ptrue then
        predicate_to_invariant kf new_loop pre2;

      let pre1 = Aorai_utils.get_restricted_ext_pre_bc stmt in
      if pre1.content <> Ptrue then
        predicate_to_invariant kf new_loop (mk_imply Rneq pre1);

      let post2 = Aorai_utils.get_restricted_int_post_bc stmt in
      if post2.content <> Ptrue then
        predicate_to_invariant kf new_loop (mk_imply Req post2);

      let action_state =
        Spec_tools.double_bool_array_or_bycase
          (Data_for_aorai.get_loop_int_post_bycase stmt)
          (Data_for_aorai.get_loop_ext_pre_bycase stmt)
      in
      let action_inv = get_action_invariant kf (Kstmt stmt) action_state in
      List.iter (predicate_to_invariant kf new_loop) action_inv;

(*    4) Keeping in mind to preserve old annotations after visitor end *)
      Hashtbl.add post_treatment_loops (ref stmt) (ref new_loop);

(*    5) Updated stmt is returned *)
          stmt
    in
    self#enter_block ();
    let after s =
      if self#leave_block () then
        begin
          let annots = Annotations.get_all_annotations stmt in
          let annots = 
            List.map Annotations.get_code_annotation annots in
          let specs =
            snd (List.split (Logic_utils.extract_contract annots))
          in
          List.iter (update_assigns (Cil_datatype.Stmt.loc stmt)) specs;
          s
        end
      else s
    in
    if treatloops then
      match stmt.skind with
        | Loop (_,block,_,_,_) ->
            ChangeDoChildrenPost(stmt, after $ (treat_loop (ref block)))

        | _ -> ChangeDoChildrenPost(stmt, after)
    else
      ChangeDoChildrenPost(stmt,after)

  method vinst = function
    | Call _ -> self#call (); DoChildren
    | _ -> DoChildren

end





(****************************************************************************)
(**
  This visitor computes the list of ignored functions.
  A function is ignored if its call is present in the C program, 
  while its definition is not available.
*)
class visit_computing_ignored_functions () =
  let declaredFunctions = Data_for_aorai.getFunctions_from_c () in
  let isDeclaredInC fname =
    List.exists
      (fun s -> (String.compare fname s)=0)
      declaredFunctions
  in
object (*(self) *)

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vfunc _f = DoChildren

  method vstmt_aux stmt =
    match stmt.skind with
      | Instr(Call (_,funcexp,_,_)) ->
          let name = get_call_name funcexp in
          (* If the called function is neither ignored, nor declared, 
             then it has to be added to ignored functions. *)
          if (not (Data_for_aorai.isIgnoredFunction name))
            && (not (isDeclaredInC name)) then
                (Data_for_aorai.addIgnoredFunction name);
          DoChildren
      | _ -> DoChildren

end

(*************************************************************************)
(* Call of the visitors *)

let compute_abstract file root (considerAcceptance:bool) =
  let visitor = new visit_computing_abstract_pre_post_from_buch
    (Data_for_aorai.getAutomata()) (root) considerAcceptance
  in
  Cil.visitCilFile (visitor :> Cil.cilVisitor) file

(* This visitor requires the AI to compute loop invariants. *)
let add_pre_post_from_buch file treatloops  =
  let visitor =
    new visit_adding_pre_post_from_buch
      (Data_for_aorai.getAutomata())
      treatloops
  in
  Cil.visitCilFile (visitor :> Cil.cilVisitor) file;
  (* Transfer previous annotation on the new loop statement.
     Variant clause has to be preserved at the end of the annotation.*)
  Hashtbl.iter
    (fun old_stmt new_stmt ->
      let new_s = !new_stmt in
      let old_s = !old_stmt in
      let kf = Kernel_function.find_englobing_kf old_s in
      let old_annots = Annotations.get_all_annotations old_s in
      (* Erasing annotations from the old statement before attaching them with
	 the new one *)
      Annotations.reset_stmt ?reset:true kf old_s;
      List.iter (Annotations.add kf new_s []) old_annots;
    )
    post_treatment_loops

let add_sync_with_buch file  =
  let visitor =
    new visit_adding_code_for_synchronisation (Data_for_aorai.getAutomata())
  in
  Cil.visitCilFile (visitor :> Cil.cilVisitor) file

(* Call of the visitor *)
let compute_ignored_functions file =
  let visitor = new visit_computing_ignored_functions () in
  Cil.visitCilFile (visitor :> Cil.cilVisitor) file

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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


open Cil_types

let preconditions_emitter =
  Emitter.create
    "Call Preconditions"
    [ Emitter.Property_status ]
    ~correctness:[]
    ~tuning:[]

(* Map from a requires to the its specializations at all call sites. *)
module PreCondProxyGenerated =
  State_builder.Hashtbl(Property.Hashtbl)(Datatype.List(Property))
    (struct
      let name = "Call Preconditions Generated"
      let dependencies = [Ast.self]
      let size = 97
    end)


module PropStmt =
  Datatype.Pair_with_collections(Property)(Cil_datatype.Stmt)
    (struct let module_name = "Statuses_by_call.PropStmt" end)

module FunctionPointers =
  Cil_state_builder.Stmt_hashtbl(Kernel_function.Hptset)
    (struct
      let name = "Statuses_by_call.FunctionPointers"
      let dependencies = [Ast.self]
      let size = 37
    end)

let add_called_function stmt kf =
  let prev =
    try FunctionPointers.find stmt
    with Not_found -> Kernel_function.Hptset.empty
  in
  let s = Kernel_function.Hptset.add kf prev in
  FunctionPointers.replace stmt s


let all_functions_with_preconditions stmt =
  match stmt with
  | { skind=Instr (Call(_,{enode = Lval (Var vkf, NoOffset)},_,_)
                  |Local_init(_,ConsInit(vkf,_,_),_)) } ->
    let kf = Globals.Functions.get vkf in
    Kernel_function.Hptset.singleton kf
  |  _ ->
    try FunctionPointers.find stmt
    with Not_found -> Kernel_function.Hptset.empty


exception Non_Transposable

let rec replace_formal_by_concrete vinfo = function
  | [] -> raise Non_Transposable
  | (formal, term) :: tl ->
    if vinfo.vid = formal.vid
    then term
    else replace_formal_by_concrete vinfo tl

(* Visitor to replace formal parameters by concrete arguments, given by the
   association list [arguments]. Also replaces logic label Pre by Here (valid at
   the call site).
   Raises Non_Transposable if the address of a formal is used, or if a formal is
   used under a label different from Pre and Here (the formal would be out of
   scope, but possibly not the concrete argument). *)
let replacement_visitor ~arguments = object (self)
  inherit Visitor.frama_c_copy (Project.current ())

  val mutable under_label = false

  method private is_under_label = function
    | BuiltinLabel (Pre | Here) -> false
    | _ -> true

  method private replace_tlval tlval =
    let t_lhost, t_offset = tlval in
    match t_lhost with
    | TMem _ ->
      let normalise_lval = function
        | TLval ((TMem {term_node=TAddrOf lv}), ofs) ->
          TLval (Logic_const.addTermOffsetLval ofs lv)
        | TLval ((TMem {term_node=TStartOf lv}), ofs) ->
          TLval (Logic_const.addTermOffsetLval (TIndex (Cil.lzero (), ofs)) lv)
        | x -> x
      in
      Cil.DoChildrenPost normalise_lval
    | TVar { lv_origin = Some vinfo } when vinfo.vformal ->
      if under_label then raise Non_Transposable;
      begin
        let new_term = replace_formal_by_concrete vinfo arguments in
        let add_offset lv = TLval (Logic_const.addTermOffsetLval t_offset lv) in
        match new_term.term_node with
        | TLval lv -> Cil.ChangeDoChildrenPost (add_offset lv, fun x -> x)
        | _ ->
          if t_offset = TNoOffset
          then Cil.ChangeTo new_term.term_node
          else
            let ltyp = new_term.term_type in
            let tmp_lvar = Cil.make_temp_logic_var ltyp in
            let tmp_linfo =
              { l_var_info = tmp_lvar; l_body = LBterm new_term;
                l_type = None; l_tparams = []; l_labels = []; l_profile = [];  }
            in
            let lval_node = TLval (TVar tmp_lvar, t_offset) in
            let lval_term = Tlet (tmp_linfo, Logic_const.term lval_node ltyp) in
            Cil.ChangeDoChildrenPost (lval_term, fun x -> x)
      end
    | _ -> Cil.DoChildren

  method! vterm_node = function
    | TConst _ | TSizeOf _ | TSizeOfStr _
    | TAlignOf _ | Tnull | Ttype _ | Tempty_set -> Cil.SkipChildren
    | TLval tlval -> self#replace_tlval tlval
    | TAddrOf tlval ->
      begin
        match fst tlval with
        | TVar { lv_origin = Some vinfo } when vinfo.vformal ->
          raise Non_Transposable
        | _ -> Cil.DoChildren
      end
    | Tat (_, label) ->
      let previous_label = under_label in
      under_label <- self#is_under_label label;
      Cil.DoChildrenPost (fun t -> under_label <- previous_label; t)
    | _ -> Cil.DoChildren

  method! vlogic_label = function
    | BuiltinLabel Pre ->
      Cil.ChangeDoChildrenPost (Logic_const.here_label, fun x -> x)
    | _ -> Cil.DoChildren
end

(* Associates each formal to a term corresponding to the concrete argument. *)
let rec associate acc ~formals ~concretes =
  match formals, concretes with
  | [], _ -> acc
  | _, [] -> raise Non_Transposable
  | formal :: formals, concrete :: concretes ->
    let term = Logic_utils.expr_to_term ~cast:true concrete in
    associate ((formal, term) :: acc) ~formals ~concretes

let transpose_pred_at_callsite ~formals ~concretes pred =
  let pred = Logic_const.pred_of_id_pred pred in
  try
    let arguments = associate [] ~formals ~concretes in
    let visitor :> Cil.cilVisitor = replacement_visitor arguments in
    let new_pred = Cil.visitCilPredicateNode visitor pred.pred_content in
    let p_unnamed = Logic_const.unamed ~loc:pred.pred_loc new_pred in
    let p_named = { p_unnamed with pred_name = pred.pred_name } in
    Some (Logic_const.new_predicate p_named)
  with Non_Transposable -> None


(* Map from [requires * stmt] to the specialization of the requires
   at the statement. Only present if the kernel function that contains
   the requires can be called at the statement. *)
module PreCondAt =
  State_builder.Hashtbl(PropStmt.Hashtbl)(Property)
    (struct
      let size = 37
      let dependencies = [ Ast.self ]
      let name = "Statuses_by_call.PreCondAt"
    end)

(* Transposes the precondition property [pid] of the called function [kf]
   at call site [stmt], with arguments [args], result assigned in [result],
   and function expression [funcexp]. *)
let rec transpose_precondition stmt pid kf funcexp args =
  let formals = Kernel_function.get_formals kf in
  let ip = match pid with
    | Property.IPPredicate {Property.ip_pred} -> ip_pred
    | _ -> assert false
  in
  let ip = transpose_pred_at_callsite ~formals ~concretes:args ip in
  let kf_call = Kernel_function.find_englobing_kf stmt in
  let p = Property.ip_property_instance kf_call stmt ip pid in
  PreCondAt.add (pid, stmt) p;
  (match funcexp.enode with
   | Lval (Var vkf, NoOffset) ->
     assert (Cil_datatype.Varinfo.equal vkf (Kernel_function.get_vi kf))
   | _ ->
     let loc = Cil_datatype.Stmt.loc stmt in
     Kernel.debug ~source:(fst loc)
       "Adding precondition for call to %a through pointer"
       Kernel_function.pretty kf;
     add_called_function stmt kf;
     add_call_precondition pid p
  );
  p

and precondition_at_call kf pid stmt =
  try PreCondAt.find (pid, stmt)
  with Not_found ->
    let do_call = transpose_precondition stmt pid kf in
    match stmt.skind with
    | Instr (Call (_, funcexp, args, _)) -> do_call funcexp args
    | Instr (Local_init (v, ConsInit (f, args, kind), loc)) ->
      let do_call _result funcexp args _loc = do_call funcexp args in
      Cil.treat_constructor_as_func do_call v f args kind loc
    | _ -> assert false

and setup_precondition_proxy called_kf precondition =
  if not (PreCondProxyGenerated.mem precondition) then begin
    Kernel.debug "Setting up syntactic call-preconditions for precondition \
                  of %a" Kernel_function.pretty called_kf;
    let call_preconditions =
      List.map
        (fun (_,stmt) -> precondition_at_call called_kf precondition stmt)
        (Kernel_function.find_syntactic_callsites called_kf)
    in
    Property_status.logical_consequence
      preconditions_emitter precondition call_preconditions;
    PreCondProxyGenerated.add precondition call_preconditions
  end

and add_call_precondition precondition call_precondition =
  let prev = try PreCondProxyGenerated.find precondition with Not_found -> [] in
  let all = call_precondition :: prev in
  PreCondProxyGenerated.replace precondition all;
  Property_status.logical_consequence preconditions_emitter precondition all

let fold_requires f kf acc =
  let bhvs = Annotations.behaviors ~populate:false kf in
  List.fold_right
    (fun bhv acc -> List.fold_right (f bhv) bhv.b_requires acc) bhvs acc


(* Properties for kf-preconditions at call-site stmt, if created.
   Returns both the initial property and its copy at call site. *)
let all_call_preconditions_at ~warn_missing kf stmt =
  let aux bhv precond properties =
    let pid_spec = Property.ip_of_requires kf Kglobal bhv precond in
    if PreCondAt.mem (pid_spec, stmt) then
      let pid_call = precondition_at_call kf pid_spec stmt in
      (pid_spec, pid_call) :: properties
    else (
      if warn_missing then
        Kernel.fatal ~source:(fst (Cil_datatype.Stmt.loc stmt))
          "Preconditions %a for %a not yet registered at this statement"
          Printer.pp_identified_predicate precond Kernel_function.pretty kf;
      properties)
  in
  fold_requires aux kf []

let setup_all_preconditions_proxies kf =
  let aux bhv req () =
    let ip = Property.ip_of_requires kf Kglobal bhv req in
    setup_precondition_proxy kf ip
  in
  fold_requires aux kf ()

let replace_call_precondition ip stmt ip_at_call =
  (try
     (* Remove previous binding *)
     let cur = PreCondAt.find (ip, stmt) in
     PreCondAt.remove (ip, stmt);
     let all = PreCondProxyGenerated.find ip in
     let all' = Extlib.filter_out (Property.equal cur) all in
     PreCondProxyGenerated.replace ip all';
   with Not_found -> ());
  PreCondAt.replace (ip, stmt) ip_at_call;
  add_call_precondition ip ip_at_call


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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
  | { skind=Instr (Call(_,{enode = Lval (Var vkf, NoOffset)},_,_)) } ->
    let kf = Globals.Functions.get vkf in
    Kernel_function.Hptset.singleton kf
  |  _ ->
    try FunctionPointers.find stmt
    with Not_found -> Kernel_function.Hptset.empty


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

let rec precondition_at_call kf pid stmt =
  try PreCondAt.find (pid, stmt)
  with Not_found ->
    let loc = (Cil_datatype.Stmt.loc stmt) in
    let kf_call = Kernel_function.find_englobing_kf stmt in
    let name = Pretty_utils.sfprintf "%s: %a"
      (Property.Names.get_prop_name_id pid)
      (Description.pp_localized ~kf:`Never ~ki:false ~kloc:true) pid
    in
    let p = Property.ip_other name (Some kf_call) (Kstmt stmt) in
    PreCondAt.add (pid, stmt) p;
    (match stmt.skind with
    | Instr(Call(_, e, _, _)) ->
      (match e.enode with
      | Lval (Var vkf, NoOffset) ->
        assert
          (Cil_datatype.Varinfo.equal vkf (Kernel_function.get_vi kf))
      | _ ->
        Kernel.debug ~source:(fst loc)
          "Adding precondition for call to %a through pointer"
          Kernel_function.pretty kf;
        add_called_function stmt kf;
        add_call_precondition pid p
      )
    | _ -> assert false (* meaningless on a non-call statement *)
    );
    p

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
  List.fold_left
    (fun acc bhv -> List.fold_left (f bhv) acc bhv.b_requires) acc bhvs


(* Properties for kf-preconditions at call-site stmt, if created.
   Returns both the initial property and its copy at call site. *)
let all_call_preconditions_at ~warn_missing kf stmt =
  let aux bhv properties precond =
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
  let aux bhv () req =
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
compile-command: "make -C ../.."
End:
*)

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

module R = Datascope.R
let debug1 fmt = R.debug ~level:1 fmt
let debug2 fmt = R.debug ~level:2 fmt

open Cil_datatype
open Cil_types

module Data = struct
  type t = Locations.Zone.t
  let bottom = Locations.Zone.bottom
  let equal = Locations.Zone.equal
  let intersects = Locations.Zone.valid_intersects
  let merge = Locations.Zone.join (* over-approx *)
  let diff = Locations.Zone.diff (* over-approx *)
  let pretty fmt z = Format.fprintf fmt "@[<h 1>%a@]" Locations.Zone.pretty z

  let exp_zone stmt exp = !Db.From.find_deps_no_transitivity stmt exp
end

module Ctx = struct
  type t = Data.t Stmt.Hashtbl.t
  let create = Stmt.Hashtbl.create
  let find = Stmt.Hashtbl.find
  let add ctx k d =
    let d =
      try let old_d = find ctx k in Data.merge old_d d with Not_found -> d
    in Stmt.Hashtbl.replace ctx k d
  (* let mem = Stmt.Hashtbl.mem : useless because Ctx has to be initialized to bot *)
  let _pretty fmt infos =
    Stmt.Hashtbl.iter
      (fun k d -> Format.fprintf fmt "Stmt:%d -> %a@\n" k.sid Data.pretty d)
      infos
end

let compute_new_data old_zone l_zone l_dpds exact r_dpds =
  if (Data.intersects old_zone l_zone) then
    let zone = if exact then Data.diff old_zone l_zone else old_zone in
    let zone = Data.merge zone l_dpds in
    let zone = Data.merge zone r_dpds in
      (true, zone)
  else (false, old_zone)

(* the call result can be processed like a normal assignment *)
let process_call_res data stmt lvaloption froms =
  let data = match lvaloption with
    | None -> false, data
    | Some lval ->
        let ret_dpds = froms.Function_Froms.deps_return in
        let r_dpds = Function_Froms.Memory.LOffset.collapse ret_dpds in
        let r_dpds = Function_Froms.Deps.to_zone r_dpds in
        let l_dpds, exact, l_zone =
          Datascope.get_lval_zones ~for_writing:true  stmt lval in
          compute_new_data data l_zone l_dpds exact r_dpds
  in data

(* we need [data_after] zone after the call, so we need to add the dpds
* of each output that intersects this zone.
* Moreover, we need to add the part of [data_after] that has not been
* modified for sure. *)
let process_froms data_after froms =
  let from_table = froms.Function_Froms.deps_table in
  let process_out_call out (default, deps) (to_prop, used, new_data) =
    let out_dpds = Function_Froms.Deps.to_zone deps in
    let exact = not default in
    (* be careful to compare out with data_after and not new_data *)
    if (Data.intersects data_after out) then
      let to_prop = if exact then Data.diff to_prop out else to_prop in
      let new_data = Data.merge new_data out_dpds in
         (to_prop, true, new_data)
    else (to_prop, used, new_data)
  in
  let to_prop =
    (* part of data_after that we need to compute before call :
    * = data_after minus all exact outputs.
    * Don't use [data_after - (merge out)] to avoid approximation in merge *)
    data_after in
  let new_data = Data.bottom in (* add out_dpds when out intersects data_after*)
  let used = false in (* is the call needed ? *)
  let to_prop, used, new_data =
    try Function_Froms.Memory.fold process_out_call from_table
          (to_prop, used, new_data)
    with Function_Froms.Memory.Cannot_fold ->
      process_out_call Locations.Zone.top (false, Function_Froms.Deps.top)
          (to_prop, used, new_data)
  in let data = Data.merge to_prop new_data in
    (used, data)

let process_call_args data called_kf stmt args =
  let param_list = Kernel_function.get_formals called_kf in
  let asgn_arg_to_param data param arg =
    let param_zone = Locations.zone_of_varinfo param in
    let arg_dpds = Data.exp_zone stmt arg in
    let exact = true in (* param is always a variable so asgn is exact *)
    let _used, data =
      compute_new_data data param_zone Data.bottom exact arg_dpds in
      (* can ignore 'used' because if we need param, we already know that the
      * call is needed *)
      data
  in
  let rec do_param_arg data param_list args =
    match param_list, args with
      | [], [] -> data
      | p :: param_list, a :: args ->
          let data = asgn_arg_to_param data p a in
            do_param_arg data param_list args
      | [], _ -> (* call to a variadic function *)
          (* warning already sent during 'from' computation. *)
          (* TODO : merge the remaining args in data ?... *)
          data
      | _, [] -> R.abort "call to a function with to few arguments"
  in do_param_arg data param_list args

let process_one_call data stmt lvaloption froms =
  let res_used, data = process_call_res data stmt lvaloption froms in
  let out_used, data = process_froms data froms in
  let used = res_used || out_used in
    used, data

let process_call data_after stmt lvaloption funcexp args =
  let funcexp_dpds, called_functions =
    !Db.Value.expr_to_kernel_function ~with_alarms:CilE.warn_none_mode
      (Kstmt stmt) ~deps:(Some Data.bottom) funcexp
  in
  let used, data =
    try
      let froms = !Db.From.Callwise.find (Kstmt stmt) in
        process_one_call data_after stmt lvaloption froms
    with Not_found -> (* don't have callwise (-calldeps option) *)
      let do_call kf acc =
        (* notice that we use the same old data for each possible call *)
        (process_one_call data_after stmt lvaloption (!Db.From.get kf))::acc
      in
      let l = Kernel_function.Hptset.fold do_call called_functions [] in
        (* in l, we have one result for each possible function called *)
      List.fold_left
        (fun (acc_u,acc_d) (u,d) -> (acc_u || u), Data.merge acc_d d)
        (false, Data.bottom)
        l
  in
  if used then
    let data =
      (* no problem of order because parameters are disjoint for sure *)
      Kernel_function.Hptset.fold
        (fun kf data -> process_call_args data kf stmt args)
        called_functions
        data
    in
    let data =  Data.merge funcexp_dpds data in
      used, data
  else begin
    assert (R.verify (Data.equal data data_after)
              "if statement not used, data doesn't change !");
    used, data
  end

module Computer (Param:sig val states : Ctx.t end) = struct

  let name = "Zones"
  let debug = false

  let used_stmts = ref []
  let add_used_stmt stmt = used_stmts := stmt :: !used_stmts
  let get_and_reset_used_stmts () =
    let stmts = !used_stmts in used_stmts := [] ; stmts

  type t = Data.t
  let pretty = Data.pretty

  module StmtStartData = struct
    type data = t
    let clear () = Stmt.Hashtbl.clear Param.states
    let mem = Stmt.Hashtbl.mem Param.states
    let find = Stmt.Hashtbl.find Param.states
    let replace = Stmt.Hashtbl.replace Param.states
    let add = Stmt.Hashtbl.add Param.states
    let iter f = Stmt.Hashtbl.iter f Param.states
    let length () = Stmt.Hashtbl.length Param.states
  end

  let combineStmtStartData _stmt ~old new_ =
    if Data.equal new_ old then None else Some new_

  let combineSuccessors = Data.merge

  let doStmt _stmt = Dataflow2.Default

  let doInstr stmt instr data =
    match instr with
      | Set (lval, exp, _) ->
          let l_dpds, exact, l_zone =
            Datascope.get_lval_zones ~for_writing:true stmt lval in
          let r_dpds = Data.exp_zone stmt exp in
          let used, data = compute_new_data data l_zone l_dpds exact r_dpds in
          let _ = if used then add_used_stmt stmt in
              Dataflow2.Done data
      |  Call (lvaloption,funcexp,args,_) ->
          let used, data = process_call data stmt lvaloption funcexp args in
          let _ = if used then add_used_stmt stmt in
            Dataflow2.Done data
      | _ -> Dataflow2.Default

  let filterStmt _stmt _next = true

  let funcExitData = Data.bottom

end

let compute_ctrl_info pdg ctrl_part used_stmts =
  let module CtrlComputer = Computer (struct let states = ctrl_part end) in
  let module CtrlCompute = Dataflow2.Backwards(CtrlComputer) in
  let seen = Stmt.Hashtbl.create 50 in
  let rec add_node_ctrl_nodes new_stmts node =
    let ctrl_nodes = !Db.Pdg.direct_ctrl_dpds pdg node in
      List.fold_left add_ctrl_node new_stmts ctrl_nodes
  and add_ctrl_node new_stmts ctrl_node =
    debug2 "[zones] add ctrl node %a@." PdgTypes.Node.pretty ctrl_node;
    match PdgTypes.Node.stmt ctrl_node with
      | None -> (* node without stmt : add its ctrl_dpds *)
          add_node_ctrl_nodes new_stmts ctrl_node
      | Some stmt ->
          debug2 "[zones] node %a is stmt %d@."
            PdgTypes.Node.pretty ctrl_node stmt.sid;
          if Stmt.Hashtbl.mem seen stmt then new_stmts
          else
            let ctrl_zone = match stmt.skind with
              | Switch (exp,_,_,_) |  If (exp,_,_,_) -> Data.exp_zone stmt exp
              | _ -> Data.bottom
            in Ctx.add ctrl_part stmt ctrl_zone;
               Stmt.Hashtbl.add seen stmt ();
               debug2 "[zones] add ctrl zone %a at stmt %d@."
                 Data.pretty ctrl_zone stmt.sid;
               stmt::new_stmts
  and add_stmt_ctrl new_stmts stmt =
    debug1 "[zones] add ctrl of stmt %d@." stmt.sid;
    if Stmt.Hashtbl.mem seen stmt then new_stmts
    else begin
      Stmt.Hashtbl.add seen stmt ();
      match !Db.Pdg.find_simple_stmt_nodes pdg stmt with
        | [] -> []
        | n::_ -> add_node_ctrl_nodes new_stmts n
    end
  in
  let rec add_stmts_ctrl stmts all_used_stmts =
    let all_used_stmts = stmts @ all_used_stmts in
    let new_stmts = List.fold_left add_stmt_ctrl [] stmts in
    let preds =  List.fold_left (fun acc s -> s.preds @ acc) [] new_stmts in
      if preds <> [] then CtrlCompute.compute preds;
    let used_stmts = CtrlComputer.get_and_reset_used_stmts () in
      if used_stmts = [] then all_used_stmts
      else add_stmts_ctrl used_stmts all_used_stmts
  in
    add_stmts_ctrl used_stmts []

let compute kf stmt lval =
  let f = Kernel_function.get_definition kf in
  let dpds, _exact, zone =
    Datascope.get_lval_zones ~for_writing:false stmt lval in
  let zone = Data.merge dpds zone in
    debug1 "[zones] build for %a before %d in %a@\n"
      Data.pretty zone stmt.sid Kernel_function.pretty kf;
  let data_part = Ctx.create 50 in
  List.iter (fun s -> Ctx.add data_part s Data.bottom) f.sallstmts;
  let _ = Ctx.add data_part stmt zone in
  let module DataComputer = Computer (struct let states = data_part end) in
  let module DataCompute = Dataflow2.Backwards(DataComputer) in
  let _ = DataCompute.compute stmt.preds in
  let ctrl_part = data_part (* Ctx.create 50 *) in
    (* it is confusing to have 2 part in the provided information,
    * because in fact, it means nothing to separate them.
    * So let's put everything in the same object *)
  let used_stmts = DataComputer.get_and_reset_used_stmts () in
  let all_used_stmts =
    if used_stmts = [] then []
    else compute_ctrl_info (!Db.Pdg.get kf) ctrl_part used_stmts
  in
  let all_used_stmts =
    List.fold_left
      (fun e acc -> Stmt.Hptset.add acc e) Stmt.Hptset.empty all_used_stmts
  in
  all_used_stmts, data_part

let get stmt_zones stmt =
  try Ctx.find stmt_zones stmt with Not_found -> Data.bottom

let pretty fmt stmt_zones =
  let pp s d = Format.fprintf fmt "Stmt:%d -> %a@." s.sid Data.pretty d in
  (* Sort output so that it does not depend on the OCaml hash function.
     Can be removed when OCaml 4.01 is mandatory *)
  let sorted = Stmt.Hashtbl.fold Stmt.Map.add stmt_zones Stmt.Map.empty in
  Stmt.Map.iter pp sorted

       (*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

let () =
  Db.register (* kernel_function -> stmt -> lval -> StmtHptset.t * t_zones *)
    Db.Journalization_not_required (* TODO *)
    (*
    (Db.Journalize("Scope.build_zones",
                   Datatype.func Kernel_type.kernel_function
                     (Datatype.func Kernel_type.stmt
                        (Datatype.func Kernel_type.lval
                           (Datatype.couple Kernel_type.stmt_set zones_ty)))))
                           *)
    Db.Scope.build_zones compute;

  Db.register (* t_zones ->  Cil_types.stmt -> Locations.Zone.t *)
    Db.Journalization_not_required (* TODO *)
    (*(Db.Journalize("Scope.get_zones",
                   Datatype.func zones_ty (Datatype.func Kernel_type.stmt data_ty)))*)
  Db.Scope.get_zones get;

  Db.register (* (Format.formatter -> t_zones -> unit) *)
    Db.Journalization_not_required (* TODO *)
    (*(Db.Journalize("Scope.pretty_zones",
                   Datatype.func Datatype.formatter (Datatype.func zones_ty Datatype.unit)))*)
  Db.Scope.pretty_zones pretty;

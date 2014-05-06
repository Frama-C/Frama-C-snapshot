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

(** Find the statements that defines a given data at a program point,
* ie. in each backward path starting from this point, find the statement
* the the data has been assigned for the last time. *)

open Cil_datatype
open Cil_types

let debug1 fmt = Datascope.R.debug ~level:1 fmt

module Interproc =
 Datascope.R.True(struct
          let option_name = "-scope-defs-interproc"
          let help = "interprocedural defs computation"
        end)

module NSet = PdgTypes.Node.Set

let add_list_to_set l s = List.fold_left (fun r n -> NSet.add n r) s l


let _pp_list_node_underout prefix fmt =
  Pretty_utils.pp_list ~pre:(prefix ^^ " @[") ~suf:"@]@." ~sep:"@ "
    (fun fmt (n, undef) ->
      match undef with
        | None -> PdgTypes.Node.pretty fmt n
        | Some undef ->
            Format.fprintf fmt "%a {underout %a}"
              PdgTypes.Node.pretty n Locations.Zone.pretty undef)
    fmt

let _pp_set prefix fmt =
  Pretty_utils.pp_iter ~pre:(prefix ^^ " @[") ~suf:"@]@." ~sep:"@ "
    NSet.iter PdgTypes.Node.pretty fmt


(** The nodes [nodes] define the searched location [z]. If those nodes are calls
    to functions, go inside those calls, and find which nodes are relevant. *)
let rec add_callee_nodes z acc nodes =
  let new_nodes, acc = NSet.fold
    (fun node acc2 ->
      match !Db.Pdg.node_key node with
        | PdgIndex.Key.SigCallKey (cid, PdgIndex.Signature.Out out_key) ->
            let callees =
              Db.Value.call_to_kernel_function (PdgIndex.Key.call_from_id cid)
            in
            Kernel_function.Hptset.fold (fun kf (new_nodes, acc) ->
              let callee_pdg = !Db.Pdg.get kf in
              let outputs = match out_key with
                | PdgIndex.Signature.OutLoc out ->
                    (* [out] might be an over-approximation of the location
                       we are searching for. We refine the search if needed. *)
                    let z = Locations.Zone.narrow out z in
                    fst (!Db.Pdg.find_location_nodes_at_end callee_pdg z)
                | PdgIndex.Signature.OutRet -> (* probably never occurs *)
                    fst (!Db.Pdg.find_output_nodes callee_pdg out_key)
              in
              let outputs = List.map fst outputs in
              add_list_to_set outputs new_nodes, add_list_to_set outputs acc)
              callees
              acc2
        | _ -> acc2)
    nodes
    (NSet.empty, acc)
  in if NSet.is_empty new_nodes then acc else add_callee_nodes z acc new_nodes

(** [kf] doesn't define all the data that we are looking for: the [undef]
    zone must have been defined in its caller, let's find it. [z] is the
    initial zone that we are looking for, so that we do not look for more
    than it. *)
(* BYTODO: maybe [undef] could be used instead of [z] altogether *)
let rec add_caller_nodes z kf acc (undef, nodes) = 
  let join_undef u u' = match u, u' with
    | _, None -> u
    | None, Some _ -> u'
    | Some z, Some z' -> Some (Locations.Zone.join z z')
  in
  let add_one_call_nodes pdg (acc_undef, acc) stmt =
    let acc_undef, acc = match undef with
      | None -> acc_undef, acc
      | Some undef ->
          let nodes_for_undef, undef' =
            !Db.Pdg.find_location_nodes_at_stmt pdg stmt ~before:true undef
          in
          let acc_undef = join_undef acc_undef undef' in
          let acc = add_list_to_set (List.map fst nodes_for_undef) acc in
            acc_undef, acc
    in
    let add_call_input_nodes node (acc_undef, acc) =
      match !Db.Pdg.node_key node with
        | PdgIndex.Key.SigKey (PdgIndex.Signature.In in_key) ->
            begin match in_key with
              | PdgIndex.Signature.InCtrl ->
                  (* We only look for the values *)
                  acc_undef, acc
              | PdgIndex.Signature.InNum n_param ->
                  let n = !Db.Pdg.find_call_input_node pdg stmt n_param in
                    acc_undef, NSet.add n acc
              | PdgIndex.Signature.InImpl z' ->
                  let z = Locations.Zone.narrow z z' in
                  let nodes, undef'= !Db.Pdg.find_location_nodes_at_stmt 
                                       pdg stmt ~before:true z
                  in
                  let acc_undef = join_undef acc_undef undef' in
                    acc_undef, add_list_to_set (List.map fst nodes) acc
            end
        | _ -> acc_undef, acc
    in
      NSet.fold add_call_input_nodes nodes (acc_undef, acc)
  in
  let add_one_caller_nodes acc (kf, stmts) =
    let pdg = !Db.Pdg.get kf in
    let acc_undef, caller_nodes = 
      List.fold_left (add_one_call_nodes pdg) (None, NSet.empty) stmts
    in add_caller_nodes z kf (NSet.union caller_nodes acc) (acc_undef, caller_nodes)
  in List.fold_left add_one_caller_nodes acc (!Db.Value.callers kf)

let compute_aux kf stmt lval =
  debug1 "[Defs.compute] for %a at sid:%d in '%a'@."
    Printer.pp_lval lval stmt.sid Kernel_function.pretty kf;
  try
    let pdg = !Db.Pdg.get kf in
    let zone = !Db.Value.lval_to_zone (Kstmt stmt)
                 ~with_alarms:CilE.warn_none_mode lval
    in
    let nodes, undef =
      !Db.Pdg.find_location_nodes_at_stmt pdg stmt ~before:true zone
    in
    let nodes = add_list_to_set (List.map fst nodes) NSet.empty  in
    let nodes = 
      if Interproc.get () then
        begin
          let caller_nodes = add_caller_nodes zone kf nodes (undef, nodes) in
            add_callee_nodes zone caller_nodes caller_nodes
        end
      else nodes
    in
    Some (nodes, undef)
  with Db.Pdg.Bottom | Db.Pdg.Top | Not_found ->
    None

let compute kf stmt lval =
  let extract (nodes, undef) =
    let add_node node defs =
      match PdgIndex.Key.stmt (!Db.Pdg.node_key node) with
        | None -> defs
        | Some s -> Stmt.Hptset.add s defs
    in
    (* select corresponding stmts *)
    let defs = NSet.fold add_node nodes Stmt.Hptset.empty in
    (defs, undef)
  in
  Extlib.opt_map extract (compute_aux kf stmt lval)

(* Variation of the function above. For each PDG node that has been found,
   we find whether it directly modifies [lval] through an affectation
   (statements [Set] or [Call (lv, _)], or if the change is indirect
   through the body of a call. *)
let compute_with_def_type kf stmt lval =
  let extract (nodes, undef) =
    let add_node node acc =
      let change stmt (direct, indirect) =
        let (prev_d, pred_i) =
          try Stmt.Map.find stmt acc
          with Not_found -> (false, false)
        in
        let after = (direct || prev_d, indirect || pred_i) in
        Stmt.Map.add stmt after acc
      in
      match !Db.Pdg.node_key node with
        | PdgIndex.Key.Stmt s -> change s (true, false)
        | PdgIndex.Key.CallStmt _ -> assert false
        | PdgIndex.Key.SigCallKey (s, sign) ->
          (match sign with
            | PdgIndex.Signature.Out (PdgIndex.Signature.OutRet) ->
                change s (true, false) (* defined by affectation in 'v = ()' *)
            | PdgIndex.Signature.In _ ->
                change s (true, false) (* defined by formal v in 'f(v)' *)
            | PdgIndex.Signature.Out (PdgIndex.Signature.OutLoc _) -> begin
                match s.skind with
                  | Instr (Call (_, { enode = Lval (Var vi, NoOffset)}, _, _))
                      when let kf = Globals.Functions.get vi in
                           !Db.Value.use_spec_instead_of_definition kf
                      ->
                      (* defined through a call, but function has no body *)
                      change s (true, false)
                  | _ ->
                      (* defined within call to a function with a body*)
                      change s (false, true)  
              end
          )
        | PdgIndex.Key.SigKey _ -> acc
        | s -> Format.printf "## %a@." PdgIndex.Key.pretty s; acc
    in
    let stmts = NSet.fold add_node nodes Stmt.Map.empty in
    (stmts, undef)
  in
  Extlib.opt_map extract (compute_aux kf stmt lval)


(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

module D = Datatype.Option
             (Datatype.Pair(Stmt.Hptset)(Datatype.Option(Locations.Zone)))

module DT = Datatype.Option
             (Datatype.Pair
                (Stmt.Map.Make(Datatype.Pair(Datatype.Bool)(Datatype.Bool)))
                (Datatype.Option(Locations.Zone)))

let () =
  Db.register (* kernel_function -> stmt -> lval ->
                   (Cil_datatype.Stmt.Hptset.t * Locations.Zone.t option) option *)
    (Db.Journalize
       ("Scope.get_defs",
        Datatype.func3 Kernel_function.ty Stmt.ty Lval.ty (D.ty)))
    Db.Scope.get_defs compute;
  Db.register (* kernel_function -> stmt -> lval ->
                    ((bool, bool) Cil_datatype.Stmt.Map.t *
                     Locations.Zone.t option) option *)
    (Db.Journalize
       ("Scope.get_defs_with_type",
        Datatype.func3 Kernel_function.ty Stmt.ty Lval.ty (DT.ty)))
    Db.Scope.get_defs_with_type compute_with_def_type;



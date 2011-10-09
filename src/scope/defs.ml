(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(** Find the statements that defines a given data at a program point,
* ie. in each backward path starting from this point, find the statement
* the the data has been assigned for the last time. *)

open Cil_datatype
open Cil_types

let debug1 fmt = Datascope.R.debug ~level:1 fmt
let debug2 fmt = Datascope.R.debug ~level:2 fmt

module Interproc =
 Datascope.R.True(struct
          let option_name = "-scope-defs-interproc"
          let help = "interprocedural defs computation"
          let kind = `Tuning
        end)


let rec add_callee_nodes acc nodes =
  let new_nodes, acc = List.fold_left (fun acc2 (node,_) ->
    match !Db.Pdg.node_key node with
    | PdgIndex.Key.SigCallKey (cid,(PdgIndex.Signature.Out out_key)) ->
      let callees =
        Db.Value.call_to_kernel_function (PdgIndex.Key.call_from_id cid)
      in
      Kernel_function.Hptset.fold (fun kf (new_nodes, acc) ->
        let callee_pdg = !Db.Pdg.get kf in
        let outputs = fst (!Db.Pdg.find_output_nodes callee_pdg out_key) in
        outputs @ new_nodes, outputs @ acc)
        callees
        acc2
    | _ -> acc2)
    ([], acc)
    nodes
  in match new_nodes with [] -> acc
    | _ -> add_callee_nodes acc new_nodes

let rec add_caller_nodes kf acc (undef,nodes) =
  let callers = !Db.Value.callers kf in
    List.fold_left
      (fun acc (kf,stmts) ->
        let pdg = !Db.Pdg.get kf in
        let acc_undef,caller_nodes =
          List.fold_left (fun (acc_undef,acc) stmt ->
            let nodes_for_undef, undef' =
              !Db.Pdg.find_location_nodes_at_stmt pdg stmt ~before:true
                undef
            in
            let acc_undef = match undef' with
              | None -> acc_undef
              | Some z -> Locations.Zone.join acc_undef z
            in
            List.fold_left (fun (acc_undef,acc) (node,_) ->
              match !Db.Pdg.node_key node with
              | PdgIndex.Key.SigKey (PdgIndex.Signature.In in_key) ->
                begin match in_key with
                | PdgIndex.Signature.InCtrl ->(* We only look for the values *)
                  acc_undef,acc
                | PdgIndex.Signature.InNum n_param ->
                  acc_undef,
                  (!Db.Pdg.find_call_input_node pdg stmt n_param,None)::acc
                | PdgIndex.Signature.InImpl z ->
                  let nodes,undef'=
                    !Db.Pdg.find_location_nodes_at_stmt pdg stmt ~before:true z
                  in
                  let acc_undef = match undef' with
                    | None -> acc_undef
                    | Some z -> Locations.Zone.join acc_undef z
                  in

                  acc_undef, nodes@acc
                end
              | _ -> acc_undef,acc)
              (acc_undef,nodes_for_undef@acc)
            nodes)
          (Locations.Zone.bottom,[])
          stmts
        in
        add_caller_nodes kf (caller_nodes@acc) (acc_undef,caller_nodes))
      acc
      callers

let compute kf stmt lval =
  debug1 "[Defs.compute] for %a at sid:%d in '%a'@."
    !Ast_printer.d_lval lval stmt.sid Kernel_function.pretty kf;
  try
    let pdg = !Db.Pdg.get kf in
    let zone = !Db.Value.lval_to_zone (Kstmt stmt)
                 ~with_alarms:CilE.warn_none_mode lval
    in
    let nodes, undef =
      !Db.Pdg.find_location_nodes_at_stmt pdg stmt ~before:true zone
    in
    let nodes = 
      if Interproc.get () then
        begin
          let caller_nodes =
            add_caller_nodes kf nodes
              ((match undef with None -> Locations.Zone.bottom | Some z -> z), 
               nodes)
          in add_callee_nodes caller_nodes caller_nodes 
        end
      else nodes
    in
    let add_node defs (node,_z) =
      match PdgIndex.Key.stmt (!Db.Pdg.node_key node) with
        | None -> defs
        | Some s -> Stmt.Set.add s defs
    in
    (* select corresponding stmts *)
    let defs = List.fold_left add_node Stmt.Set.empty nodes in
    Some (defs, undef)
  with Db.Pdg.Bottom | Db.Pdg.Top | Not_found ->
    None

       (*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

let () =
  Db.register (* kernel_function -> stmt -> lval ->
                    (Cilutil.StmtSet.t * Locations.Zone.t option) option *)
    (Db.Journalize
       ("Scope.get_defs",
        Datatype.func3
          Kernel_function.ty
          Stmt.ty
          Lval.ty
          (Datatype.option
             (Datatype.pair Stmt.Set.ty (Datatype.option Locations.Zone.ty)))))
    Db.Scope.get_defs compute


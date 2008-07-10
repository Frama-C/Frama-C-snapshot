(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA   (Commissariat à l'Énergie Atomique)                           *)
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

(* $Id: annot.ml,v 1.13 2008/07/09 11:26:38 uid530 Exp $ *)

open Cil_types

module M = Macros
module G = PdgTypes.G
module Dpd = PdgTypes.Dpd
module FI = PdgIndex.FctIndex
module Key = PdgIndex.Key
module PI = PdgTypes.InternalPdg

let add_info_nodes pdg (nodes_acc, undef_acc) info =
  let stmt = info.Db.Properties.Interp.To_zone.ki in
  let before = info.Db.Properties.Interp.To_zone.before in
  let zone = info.Db.Properties.Interp.To_zone.zone in
    if M.debug2 () then
      Format.printf "[pdg:annotation] need %a %s stmt %d@\n"
        Locations.Zone.pretty zone
        (if before then "before" else "after") stmt.sid;
    let nodes, undef_loc =
      Sets.find_location_nodes_at_stmt pdg stmt before zone
    in
    let undef_acc = match undef_acc, undef_loc with 
      | None, _ -> undef_loc
      | _, None -> undef_acc
      | Some z1, Some z2 -> Some (Locations.Zone.join z1 z2)
    in
      (nodes @ nodes_acc, undef_acc)

let find_code_annot_nodes pdg ~before stmt annot =
  if M.debug2 () then
    Format.printf "[pdg:annotation] CodeAnnot-%d %s stmt %d : %a @\n"
      annot.annot_id
      (if before then "before" else "after") stmt.sid
      !Ast_printer.d_code_annotation annot;
  if Db.Value.is_accessible (Cil_types.Kstmt stmt) then
    begin
      let kf = M.get_pdg_kf pdg in
      let (list_info, list_info_decl), pragmas =
        !Db.Properties.Interp.To_zone.from_stmt_annot annot ~before (stmt, kf)
      in
      let data_dpds = ([], None) in
      let data_dpds = 
        List.fold_left (add_info_nodes pdg) data_dpds list_info in
      let add_decl_nodes decl_var (nodes_acc, undef_acc) =
        let node = !Db.Pdg.find_decl_var_node pdg decl_var in
          ((node,None)::nodes_acc, undef_acc)
      in
      let data_dpds =
        Cil.VarinfoSet.fold add_decl_nodes list_info_decl data_dpds 
      in
      let stmt_nodes = !Db.Pdg.find_simple_stmt_nodes pdg stmt in
      let ctrl_dpds = match stmt_nodes with
        | stmt_node::_ ->
            (* only have to consider the first node for ctrl dpds... *)
            !Db.Pdg.direct_ctrl_dpds pdg stmt_node
        | _ -> assert false (* accessible stmt with no node !!!??? *)
      in
      let add_stmt_nodes s acc = 
        (!Db.Pdg.find_stmt_and_blocks_nodes pdg s) @ acc in
      (* can safely ignore pragmas.ctrl
      * because we already have the ctrl dpds from the stmt node. *)
      let stmt_pragmas = pragmas.Db.Properties.Interp.To_zone.stmt in
      let ctrl_dpds = 
        Cilutil.StmtSet.fold add_stmt_nodes stmt_pragmas ctrl_dpds in
        if M.debug2 () then
          begin
            let p fmt (n,z) = match z with
              | None -> PdgTypes.Node.pretty fmt n
              | Some z -> Format.fprintf fmt "%a(%a)"
                            PdgTypes.Node.pretty n Locations.Zone.pretty z
            in
            let pl fmt l = 
              List.iter (fun n -> Format.fprintf fmt " %a" p n) l
            in
            let data_nodes, data_undef = data_dpds in
          Format.printf "[pdg:annotation] ctrl nodes = %a@\n" 
            PdgTypes.Node.pretty_list ctrl_dpds;
          Format.printf "[pdg:annotation] data nodes = %a@\n" 
            pl data_nodes;
          match data_undef with None -> ()
            | Some data_undef ->
          Format.printf "[pdg:annotation] data undef = %a@\n" 
            Locations.Zone.pretty data_undef;
          end;
      ctrl_dpds, data_dpds
    end
  else 
    begin
      if M.debug2 () then
        Format.printf "[pdg:annotation] CodeAnnot-%d : unreachable stmt ! @\n"
          annot.annot_id;
      raise PdgIndex.NotFound (* unreachable statement *)
    end


       (*
class annotations_visitor prj pdg = object (self)
  inherit Visitor.generic_frama_c_visitor  (Cil.inplace_visit ()) prj

  method vcode_annot annot =
    let _ =
      try
        let stmt = Cilutil.valOf self#current_stmt in
        let before = self#is_annot_before in
          ignore (find_code_annot_nodes pdg before stmt annot)
      with PdgIndex.NotFound -> (* unreachable *) ()
    in Cil.SkipChildren
end

let add_annotations kf pdg =
  if M.debug2 () then
    Format.printf "[pdg] synchronize annotations for function %s@."
      (Kernel_function.get_name kf);
  match kf.Db_types.fundec with
  | Db_types.Declaration _ -> ()
  | Db_types.Definition(f, _) ->
      let visit = new annotations_visitor (Project.current ()) pdg in
        ignore (Cil.visitCilFunction (visit :> Cil.cilVisitor) f)
        *)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)

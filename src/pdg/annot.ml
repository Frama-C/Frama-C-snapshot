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
open Cil_datatype
open PdgIndex

type data_info = ((PdgTypes.Node.t * Locations.Zone.t option) list
                   * Locations.Zone.t option) option

type ctrl_info = PdgTypes.Node.t list

type decl_info = PdgTypes.Node.t list

let zone_info_nodes pdg data_info =
  let add_info_nodes pdg (nodes_acc, undef_acc) info =
    let stmt = info.Db.Properties.Interp.To_zone.ki in
    let before = info.Db.Properties.Interp.To_zone.before in
    let zone = info.Db.Properties.Interp.To_zone.zone in
      Pdg_parameters.debug ~level:2 "[pdg:annotation] need %a %s stmt %d@."
        Locations.Zone.pretty zone
        (if before then "before" else "after") stmt.sid;
      let nodes, undef_loc =
        Sets.find_location_nodes_at_stmt pdg stmt ~before zone
      in
      let undef_acc = match undef_acc, undef_loc with
        | None, _ -> undef_loc
        | _, None -> undef_acc
        | Some z1, Some z2 -> Some (Locations.Zone.join z1 z2)
      in
        (nodes @ nodes_acc, undef_acc)
  in match data_info with
    | None -> None (* To_zone.xxx didn't manage to compute the zone *)
    | Some data_info ->
        let data_dpds = ([], None) in
        let data_dpds =
          List.fold_left (add_info_nodes pdg) data_dpds data_info
        in Some data_dpds

let get_decl_nodes pdg decl_info =
  let add_decl_nodes decl_var nodes_acc =
    let node = Sets.find_decl_var_node pdg decl_var in
      node::nodes_acc
  in
  Varinfo.Set.fold add_decl_nodes decl_info []

let find_nodes_for_function_contract pdg f_interpret =
  let kf =  PdgTypes.Pdg.get_kf pdg in
  let (data_info, decl_label_info) = f_interpret kf in
  let data_dpds = zone_info_nodes pdg data_info in
  let decl_nodes = (* No way to get stmt from labels of at construct into function contracts *)
    get_decl_nodes pdg decl_label_info.Db.Properties.Interp.To_zone.var
  in
    decl_nodes, data_dpds

let find_fun_precond_nodes (pdg:PdgTypes.Pdg.t) p =
  let named_p = { name = []; loc = Location.unknown; content = p } in
  let f_interpret kf =
    let f_ctx = !Db.Properties.Interp.To_zone.mk_ctx_func_contrat
                  ~state_opt:(Some true) kf in
      !Db.Properties.Interp.To_zone.from_pred named_p f_ctx
  in find_nodes_for_function_contract pdg f_interpret

let find_fun_postcond_nodes pdg p =
  let named_p = { name = []; loc = Location.unknown; content = p } in
  let f_interpret kf =
    let f_ctx = !Db.Properties.Interp.To_zone.mk_ctx_func_contrat
                  ~state_opt:(Some false) kf in
      !Db.Properties.Interp.To_zone.from_pred named_p f_ctx
  in let nodes,deps = find_nodes_for_function_contract pdg f_interpret
  in let nodes =
      (* find is \result is used in p, and if it is the case,
       * add the node [Sets.find_output_node pdg]
       * to the returned list of nodes.
       *)
      if !Db.Properties.Interp.to_result_from_pred named_p then
        (Sets.find_output_node pdg)::nodes
      else nodes
  in nodes,deps

let find_fun_variant_nodes pdg t =
  let f_interpret kf =
    let f_ctx = !Db.Properties.Interp.To_zone.mk_ctx_func_contrat
                  ~state_opt:(Some true) kf in
      !Db.Properties.Interp.To_zone.from_term t f_ctx
  in find_nodes_for_function_contract pdg f_interpret

let find_code_annot_nodes pdg stmt annot =
  Pdg_parameters.debug "[pdg:annotation] CodeAnnot-%d stmt %d : %a @."
    annot.annot_id stmt.sid
    Printer.pp_code_annotation annot;
  if Db.Value.is_reachable_stmt stmt then
      begin
        let kf =  PdgTypes.Pdg.get_kf pdg in
        let (data_info, decl_label_info), pragmas =
          !Db.Properties.Interp.To_zone.from_stmt_annot annot (stmt, kf)
        in
        let data_dpds = zone_info_nodes pdg data_info in
        let decl_nodes = get_decl_nodes pdg decl_label_info.Db.Properties.Interp.To_zone.var in
	let labels = decl_label_info.Db.Properties.Interp.To_zone.lbl in
        let stmt_key = Key.stmt_key stmt in
        let stmt_node = match stmt_key with
          | Key.Stmt _ -> !Db.Pdg.find_stmt_node pdg stmt
          | Key.CallStmt _ -> !Db.Pdg.find_call_ctrl_node pdg stmt
          | _ -> assert false
        in
        let ctrl_dpds = !Db.Pdg.direct_ctrl_dpds pdg stmt_node in
        let add_stmt_nodes s acc =
	  try !Db.Pdg.find_stmt_and_blocks_nodes pdg s @ acc
	  with Not_found -> acc
	in
        (* can safely ignore pragmas.ctrl
         * because we already have the ctrl dpds from the stmt node. *)
        let stmt_pragmas = pragmas.Db.Properties.Interp.To_zone.stmt in
        let ctrl_dpds = Stmt.Set.fold add_stmt_nodes stmt_pragmas ctrl_dpds in
        let add_label_nodes l acc = match l with
            | StmtLabel stmt -> 
                (* TODO: we could be more precise here if we knew which label 
                * is really useful... *)
                let add acc l =
                  try (Sets.find_label_node pdg !stmt l)::acc
                  with Not_found -> acc
                in List.fold_left add acc (!stmt).labels
            | LogicLabel (Some stmt, str) -> 
                let add acc l = match l with 
                  | Label (sl, _, _) when sl = str ->
                      (try (Sets.find_label_node pdg stmt l)::acc
                       with Not_found -> acc)
                  | _ -> acc
                in List.fold_left add acc stmt.labels
            | LogicLabel (None, _) -> acc
        in
        let ctrl_dpds = Logic_label.Set.fold add_label_nodes labels ctrl_dpds in
        if Pdg_parameters.debug_atleast 2 then begin
          let p fmt (n,z) = match z with
            | None -> PdgTypes.Node.pretty fmt n
            | Some z -> Format.fprintf fmt "%a(%a)"
                PdgTypes.Node.pretty n Locations.Zone.pretty z
          in
          let pl fmt l = List.iter (fun n -> Format.fprintf fmt " %a" p n) l in
          Pdg_parameters.debug " ctrl nodes = %a"
            PdgTypes.Node.pretty_list ctrl_dpds;
          Pdg_parameters.debug " decl nodes = %a"
            PdgTypes.Node.pretty_list decl_nodes;
          match data_dpds with
            | None ->
                Pdg_parameters.debug " data nodes = None (failed to compute)"
            | Some (data_nodes, data_undef) ->
                begin
                Pdg_parameters.debug " data nodes = %a" pl data_nodes;
                match data_undef with
                  | None -> ()
                  | Some data_undef ->
                      Pdg_parameters.debug " data undef = %a"
                        Locations.Zone.pretty data_undef;
                end
        end;
        ctrl_dpds, decl_nodes, data_dpds
      end
  else begin
    Pdg_parameters.debug ~level:2
      "[pdg:annotation] CodeAnnot-%d : unreachable stmt ! @."
      annot.annot_id;
    raise Not_found (* unreachable statement *)
  end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

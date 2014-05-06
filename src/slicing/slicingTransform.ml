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

(** Export the slicing project *)

(**/**)
open Cil_types
open Cil

(**/**)

(* Look at (only once) the callers of [kf] ([kf] included). *)
let exists_fun_callers fpred kf =
  let table = ref Cil_datatype.Varinfo.Set.empty in
  let rec exists_fun_callers kf =
    if fpred kf
    then true
    else let vf = Kernel_function.get_vi kf in
      if Cil_datatype.Varinfo.Set.mem vf !table
      then false (* no way to call the initial [kf]. *)
      else begin
        table := Cil_datatype.Varinfo.Set.add vf !table ;
        List.exists
          (fun (kf,_) -> exists_fun_callers kf)
          (!Db.Value.callers kf)
      end
  in
  exists_fun_callers kf

let is_src_fun_visible prj = exists_fun_callers (SlicingMacros.is_src_fun_visible prj)

let is_src_fun_called prj kf =
  let kf_entry, _library = Globals.entry_point () in
  let fpred f =
    if (kf_entry == f)
    then SlicingMacros.is_src_fun_visible prj f (* for the entry point *)
    else SlicingMacros.is_src_fun_called  prj f (* for the others *)
  in exists_fun_callers fpred kf

module Visibility (SliceName : sig
                     val get : kernel_function -> bool -> int -> string
                   end) = struct

  exception EraseAssigns
  exception EraseAllocation

  type proj = SlicingInternals.project
  type transform = {
    slice: SlicingInternals.fct_slice;
    src_visible: bool (* whether the src function of the slice is visible and
                         can be used to give names *);
    keep_body: bool (* if false and the function has a body, the body will be
                       removed. Ignored otherwise *);
  }
  type fct =
    | Iff of transform
    | Isrc of bool (* same meaning as keep_body *)
    | Iproto

  let keep_body kf = 
    Kernel_function.is_definition kf &&
      not (!Db.Value.use_spec_instead_of_definition kf)

  let fct_info project kf =
    let fi = SlicingMacros.get_kf_fi project kf in
    let slices = SlicingMacros.fi_slices fi in
    let src_visible = is_src_fun_visible project kf in
      SlicingParameters.debug ~level:1 "[SlicingTransform.Visibility.fct_info] processing %a (%d slices/src %svisible)"
          Kernel_function.pretty kf (List.length slices)
          (if src_visible then "" else "not ");
    let need_addr = (Kernel_function.get_vi kf).vaddrof in
    let src_name_used = src_visible || need_addr in
    let keep_body = keep_body kf in
    let info_list =
      List.map
        (fun ff -> Iff {slice = ff; src_visible = src_name_used; keep_body})
        slices
    in
      if src_visible    then Isrc keep_body :: info_list
       else if need_addr then  Iproto :: info_list (* TODO for #344 *)
      else info_list

  let fct_name svar ff =
    let name = match ff with
    | Isrc _ | Iproto -> 
      let kf_entry,_ = Globals.entry_point () in
      let vi_entry = Kernel_function.get_vi kf_entry in
      if Cil_datatype.Varinfo.equal svar vi_entry then
        svar.vname ^ "_orig"
      else svar.vname
    | Iff {slice = ff; src_visible} ->
        let kf = SlicingMacros.get_ff_kf ff in
        let ff_num = ff.SlicingInternals.ff_id in
        SliceName.get kf src_visible ff_num
    in
      SlicingParameters.debug ~level:2 "[SlicingTransform.Visibility.fct_name] get fct_name = %s" name;
      name

  let visible_mark m = not (!Db.Slicing.Mark.is_bottom m)

  let param_visible ff_opt n = match ff_opt with
    | Isrc _ | Iproto -> true
    | Iff {slice = ff} -> visible_mark (Fct_slice.get_param_mark ff n)

  let body_visible ff_opt = match ff_opt with
    | Iproto -> false
    | Isrc keep -> keep
    | Iff {keep_body} -> keep_body

  let inst_visible ff_opt inst = match ff_opt with
    | Isrc _ -> true
    | Iproto -> false
    | Iff {slice = ff} ->
        let m = !Db.Slicing.Slice.get_mark_from_stmt ff inst in
          visible_mark m

  let label_visible ff_opt inst label =  match ff_opt with
    | Isrc _ -> true
    | Iproto -> false
    | Iff {slice = ff} ->
        let m = !Db.Slicing.Slice.get_mark_from_label ff inst label in
        let v = visible_mark m in
          SlicingParameters.debug ~level:2
            "[SlicingTransform.Visibility.label_visible] label %a is %svisible"
            Printer.pp_label label (if v then "" else "in");
          v

  let data_in_visible ff data_in = match data_in with
    | None -> true
    | Some data_in ->
        (* it is too difficult to know if the callers of this slice
        * compute [data_in] or not, but let's see if, by chance,
        * some data have been selected manually... *)
        let m = Fct_slice.get_input_loc_under_mark ff data_in in
        let v = visible_mark m in
          SlicingParameters.debug ~level:2
            "[SlicingTransform.Visibility.data_in_visible] data %a is %svisible"
            Locations.Zone.pretty data_in (if v then "" else "in");
          v

 let all_nodes_visible ff nodes =
   let is_visible visi n =
     let m = Fct_slice.get_node_mark ff n in
       if !Db.Slicing.Mark.is_bottom m then
         begin
           SlicingParameters.debug ~level:3
             "[SlicingTransform.Visibility.all_nodes_visible] node %a invisible"
             (!Db.Pdg.pretty_node true) n;
           false
         end
       else visi
   in  List.fold_left is_visible true nodes

  exception NoDataInfo

  let data_nodes_visible ff (decl_nodes, data_info) =
    let keep_annots = SlicingParameters.Mode.KeepAnnotations.get () in
    SlicingParameters.debug ~level:2
    "[SlicingTransform.Visibility.data_nodes_visible (with keep_annots = %s)] ?"
      (if keep_annots then "true" else "false");
    let decls_visible = all_nodes_visible ff decl_nodes in
      if keep_annots then decls_visible
      else
        match data_info with
          | None -> raise NoDataInfo
          | Some (data_nodes, data_in) ->
        let is_data_visible visi (n,z) =
          let key = PdgTypes.Node.elem_key n in
          let key = match z, key with
            | Some z, PdgIndex.Key.SigCallKey
                        (call, PdgIndex.Signature.Out
                                 (PdgIndex.Signature.OutLoc out_z)) ->
                let z = Locations.Zone.narrow z out_z in
                  PdgIndex.Key.call_output_key (PdgIndex.Key.call_from_id call) z
            | _, _ -> key
          in
          let m = Fct_slice.get_node_key_mark ff key in
            if !Db.Slicing.Mark.is_bottom m then
              begin
                SlicingParameters.debug ~level:2
                  "[SlicingTransform.Visibility.data_nodes_visible]@\n\
                   node %a invisible"
                  (!Db.Pdg.pretty_node true) n;
                false
              end
            else visi
        in
        let visible = decls_visible && data_in_visible ff data_in in
        let data_visible = List.fold_left is_data_visible visible data_nodes in
          data_visible

(* work-around to avoid outputting annotations with type errors:
   in case we end up with NotImplemented somewhere, we keep the annotation
   iff all C variables occuring in there are visible.
 *)
  let all_logic_var_visible, all_logic_var_visible_identified_term, all_logic_var_visible_term,
      all_logic_var_visible_assigns, all_logic_var_visible_deps =
    let module Exn = struct exception Invisible end in
    let vis ff = object
      inherit Visitor.frama_c_inplace
      method! vlogic_var_use v =
        match v.lv_origin with
            None -> DoChildren
          | Some v when
              v.vformal &&
                not
                (visible_mark
                   (Fct_slice.get_param_mark ff
                      (Kernel_function.get_formal_position v
                         (SlicingMacros.get_ff_kf ff)+1)))
                         (* For some reason, pdg counts parameters starting
                            from 1 *)
              -> raise Exn.Invisible
          | Some v when
              not v.vglob &&
                not (visible_mark (Fct_slice.get_local_var_mark ff v)) ->
              raise Exn.Invisible
          | Some _ -> DoChildren
    end
    in (fun ff pred ->
          try
            ignore (Visitor.visitFramacPredicate (vis ff) pred); true
          with Exn.Invisible -> false),
    (fun ff term ->
          try
            ignore (Visitor.visitFramacIdTerm (vis ff) term); true
          with Exn.Invisible -> false),
    (fun ff term ->
          try
            ignore (Visitor.visitFramacTerm (vis ff) term); true
          with Exn.Invisible -> false),
    (fun ff (b,_) ->
       try
         ignore (Visitor.visitFramacTerm (vis ff) b.it_content); true
       with Exn.Invisible -> false),
    (fun ff d ->
       try
         ignore (Visitor.visitFramacTerm (vis ff) d.it_content); true
       with Exn.Invisible -> false)

  let annotation_visible ff_opt stmt annot =
    SlicingParameters.debug ~current:true ~level:2
      "[SlicingTransform.Visibility.annotation_visible] ?";
    Db.Value.is_reachable_stmt stmt &&
    Alarms.find annot = None && (* Always drop alarms: the alarms table
                                   in the new project is not synchronized *)
    match ff_opt with
    | Isrc _ -> true
    | Iproto -> false
    | Iff {slice = ff} ->
        let kf = SlicingMacros.get_ff_kf ff  in
        let pdg = !Db.Pdg.get kf in
        try
        let ctrl_nodes, decl_nodes, data_info =
          !Db.Pdg.find_code_annot_nodes pdg stmt annot
        in
        let data_visible = data_nodes_visible ff (decl_nodes, data_info) in
        let visible = ((all_nodes_visible ff ctrl_nodes) && data_visible) in
          SlicingParameters.debug ~level:2
            "[SlicingTransform.Visibility.annotation_visible] -> %s"
              (if visible then "yes" else "no");
          visible
        with
          | NoDataInfo ->
            SlicingParameters.debug ~level:2
             "[SlicingTransform.Visibility.annotation_visible] \
                not implemented -> invisible"; false
          | Logic_interp.To_zone.NYI msg ->
            SlicingParameters.warning ~current:true ~once:true
              "Dropping unsupported ACSL annotation";
            SlicingParameters.debug ~level:2
             "[SlicingTransform.Visibility.annotation_visible] \
                %s -> invisible" msg;
            false


  let fun_precond_visible ff_opt p =
    SlicingParameters.debug ~level:2
      "[SlicingTransform.Visibility.fun_precond_visible] %a ?"
      Printer.pp_predicate_named
      { name = []; loc = Cil_datatype.Location.unknown; content = p };
    let visible = match ff_opt with
      | Isrc _ -> true
      | Iproto -> true
      | Iff {slice = ff} ->
          let kf = SlicingMacros.get_ff_kf ff  in
          let pdg = !Db.Pdg.get kf in
            try
              let nodes = !Db.Pdg.find_fun_precond_nodes pdg p in
                data_nodes_visible ff nodes
            with NoDataInfo ->
              all_logic_var_visible ff p

    in SlicingParameters.debug ~level:2 "[SlicingTransform.Visibility.precond_visible] -> %s"
         (if visible then "yes" else "no");
       visible

  let fun_postcond_visible ff_opt p =
    SlicingParameters.debug ~level:2
      "[SlicingTransform.Visibility.fun_postcond_visible] %a ?"
      Printer.pp_predicate_named
      { name = []; loc = Cil_datatype.Location.unknown; content = p };
    let visible = match ff_opt with
      | Isrc _ -> true
      | Iproto -> true
      | Iff {slice = ff} ->
          let kf = SlicingMacros.get_ff_kf ff  in
          let pdg = !Db.Pdg.get kf in
            try
              let nodes = !Db.Pdg.find_fun_postcond_nodes pdg p in
                data_nodes_visible ff nodes
            with NoDataInfo -> all_logic_var_visible ff p

    in SlicingParameters.debug ~level:2
         "[SlicingTransform.Visibility.fun_postcond_visible] -> %s"
              (if visible then "yes" else "no");
       visible

  let fun_variant_visible ff_opt v =
    SlicingParameters.debug ~level:2
      "[SlicingTransform.Visibility.fun_variant_visible] %a ?"
      Printer.pp_term v ;
    let visible = match ff_opt with
      | Isrc _ -> true
      | Iproto -> true
      | Iff {slice = ff} ->
          let kf = SlicingMacros.get_ff_kf ff  in
          let pdg = !Db.Pdg.get kf in
            try
              let nodes = !Db.Pdg.find_fun_variant_nodes pdg v in
                data_nodes_visible ff nodes
            with NoDataInfo -> all_logic_var_visible_term ff v
    in SlicingParameters.debug ~level:2 "[SlicingTransform.Visibility.fun_variant_visible] -> %s"
              (if visible then "yes" else "no");
       visible

  let fun_frees_visible ff_opt v =
    let keep_annots = SlicingParameters.Mode.KeepAnnotations.get () in
    SlicingParameters.debug ~level:2
      "[SlicingTransform.Visibility.fun_frees_visible \
       (with keep_annots = %B)] ?"
      keep_annots;
    if not keep_annots then raise EraseAllocation;
    let visible =
      match ff_opt with
        | Isrc _ -> true
        | Iproto -> true
        | Iff {slice = ff} -> all_logic_var_visible_identified_term ff v
    in SlicingParameters.debug ~level:2 "[SlicingTransform.Visibility.fun_frees_visible] -> %s"
              (if visible then "yes" else "no");
       visible

  let fun_allocates_visible ff_opt v =
    let keep_annots = SlicingParameters.Mode.KeepAnnotations.get () in
    SlicingParameters.debug ~level:2
      "[SlicingTransform.Visibility.fun_allocates_visible \
       (with keep_annots = %B)] ?"
      keep_annots;
    if not keep_annots then raise EraseAllocation;
    let visible =
      match ff_opt with
        | Isrc _ -> true
        | Iproto -> true
        | Iff {slice = ff} -> all_logic_var_visible_identified_term ff v
    in SlicingParameters.debug ~level:2 "[SlicingTransform.Visibility.fun_allocates_visible] -> %s"
              (if visible then "yes" else "no");
       visible

  let fun_assign_visible ff_opt v =
    let keep_annots = SlicingParameters.Mode.KeepAnnotations.get () in
    SlicingParameters.debug ~level:2
      "[SlicingTransform.Visibility.fun_assign_visible \
       (with keep_annots = %B)] ?"
      keep_annots;
    if not keep_annots then raise EraseAssigns;
    let visible =
      match ff_opt with
        | Isrc _ -> true
        | Iproto -> true
        | Iff {slice = ff} -> all_logic_var_visible_assigns ff v
    in SlicingParameters.debug ~level:2 "[SlicingTransform.Visibility.fun_assign_visible] -> %s"
              (if visible then "yes" else "no");
       visible

  let fun_deps_visible ff_opt v =
    let keep_annots = SlicingParameters.Mode.KeepAnnotations.get () in
    SlicingParameters.debug ~level:2
      "[SlicingTransform.Visibility.fun_deps_visible \
       (with keep_annots = %B)] ?"
      keep_annots;
    let visible =
      match ff_opt with
        | Isrc _ -> true
        | Iproto -> true
        | Iff {slice = ff} -> all_logic_var_visible_deps ff v
    in
    SlicingParameters.debug ~level:2
      "[SlicingTransform.Visibility.fun_deps_visible] -> %s"
      (if visible then "yes" else "no");
    visible

  let loc_var_visible ff_opt var = match ff_opt with
    | Isrc _ -> true
    | Iproto -> false
    | Iff {slice = ff} ->
        let m = !Db.Slicing.Slice.get_mark_from_local_var ff var in
          visible_mark m

  let res_call_visible ff call_stmt = match ff with
    | Isrc _ -> true
    | Iproto -> false
    | Iff {slice = ff} ->
        let key = PdgIndex.Key.call_outret_key call_stmt in
        let _, ff_marks = ff.SlicingInternals.ff_marks in
          try
            let m = PdgIndex.FctIndex.find_info ff_marks key in
            visible_mark m
          with Not_found -> false

  let result_visible _kf ff = match ff with
    | Isrc _ | Iproto -> true
    | Iff {slice = ff} ->
        let key = PdgIndex.Key.output_key in
        let _, ff_marks = ff.SlicingInternals.ff_marks in
          try
            let m = PdgIndex.FctIndex.find_info ff_marks key in
            visible_mark m
          with Not_found -> false

  let called_info (project, ff) call_stmt =
    let info = match ff with
      | Isrc _ | Iproto -> None
      | Iff {slice = ff} ->
          try
            let _, ff_marks = ff.SlicingInternals.ff_marks in
            let called, _ =
              PdgIndex.FctIndex.find_call ff_marks call_stmt in
          match called with
            | None | Some (None) ->
                SlicingParameters.error "Undefined called function call-%d\n"
                  call_stmt.sid;
                assert false
              | Some (Some (SlicingInternals.CallSrc _)) -> None
              | Some (Some (SlicingInternals.CallSlice ff)) ->
                  let kf_ff = SlicingMacros.get_ff_kf ff in
                  (* BY: no idea why this is not the same code as in fct_info *)
                  let src_visible = is_src_fun_visible project kf_ff in
                  let keep_body = keep_body kf_ff in
                  Some (kf_ff, Iff { slice = ff; src_visible; keep_body})
          with Not_found ->
            (* the functor should call [called_info] only for visible calls *)
            assert false
    in
     SlicingParameters.debug ~level:2 "[SlicingTransform.Visibility.called_info] called_info stmt %d -> %s@."
        call_stmt.sid (if info = None then "src" else "some slice");
    info

  let cond_edge_visible _ff_opt s =
    Db.Value.condition_truth_value s

end

let default_slice_names kf _src_visible ff_num =
  let fname = Kernel_function.get_name kf in
  let kf_entry,_ = Globals.entry_point () in
  if Kernel_function.equal kf kf_entry then fname
  else Printf.sprintf "%s_slice_%d" fname ff_num

let extract ~f_slice_names new_proj_name slicing_project =
  SlicingParameters.feedback ~level:1
    "exporting project to '%s'..." new_proj_name;
  !Db.Slicing.Request.apply_all_internal slicing_project;
  let module S = struct let get = f_slice_names end in
  let module Visi = Visibility (S) in
  let module Transform = Filter.F (Visi) in
  let tmp_prj =
    Transform.build_cil_file (new_proj_name ^ " tmp") slicing_project
  in
  let new_prj =
    !Db.Sparecode.rm_unused_globals ~new_proj_name ~project:tmp_prj ()
  in
  Project.remove ~project:tmp_prj ();
  let ctx = Parameter_state.get_selection_context () in
  Project.copy ~selection:ctx new_prj;
  SlicingParameters.feedback
    ~level:2 "done (exporting project to '%s')." new_proj_name;
  new_prj

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

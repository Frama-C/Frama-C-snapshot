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

(** Export the slicing project *)

(**/**)
open Cilutil
open Cil_types
open Cil

module T = SlicingTypes.Internals
module M = SlicingMacros

(**/**)

let is_fi_top fi =
  (*let m = Fct_slice.get_top_input_mark fi in*)
  fi.T.fi_top (* || not (!Db.Slicing.Mark.is_bottom m) *)

(* Look at (only once) the callers of [kf] ([kf] included). *)
let exists_fun_callers fpred kf =
  let table = ref VarinfoSet.empty in
  let rec exists_fun_callers kf =
    if fpred kf
    then true
    else let vf = Kernel_function.get_vi kf in
      if VarinfoSet.mem vf !table
      then false (* no way to call the initial [kf]. *)
      else
        (table := VarinfoSet.add vf !table ;
         List.exists (fun (kf,_) -> exists_fun_callers kf) (!Db.Value.callers kf))
  in exists_fun_callers kf

let is_src_fun_visible prj = exists_fun_callers (M.is_src_fun_visible prj)

let is_src_fun_called prj kf =
  let kf_entry, _library = Globals.entry_point () in
  let fpred f =
    if (kf_entry == f)
    then M.is_src_fun_visible prj f (* for the entry point *)
    else M.is_src_fun_called  prj f (* for the others *)
  in exists_fun_callers fpred kf

module Visibility (SliceName : sig
                     val get : Db_types.kernel_function -> bool -> int -> string
                   end) = struct
  type t_proj = T.t_project
  type t_fct =
      Iff of (T.t_fct_slice * bool)
                 (* the boolean says if the src function of the slice is visible
                 * and can be used to give names *)
    | Isrc
    | Iproto

  let fct_info project kf =
    let fi = M.get_kf_fi project kf in
    let slices = M.fi_slices fi in
    let src_visible = is_src_fun_visible project kf in
      M.debug 1 "[slicing:export] processing %a (%d slices/src %svisible)@."
          Kernel_function.pretty_name kf (List.length slices)
          (if src_visible then "" else "not ");
    let need_addr = (Kernel_function.get_vi kf).vaddrof in
    let src_name_used = src_visible || need_addr in
    let info_list = List.map (fun ff -> Iff (ff, src_name_used)) slices in
      if src_visible    then Isrc :: info_list
       else if need_addr then  Iproto :: info_list (* TODO for #344 *)
      else info_list

  let fct_name svar ff =
    let name = match ff with
    | Isrc -> svar.vname
    | Iproto ->
        svar.vname
    | Iff (ff, src_visible) ->
        let kf = M.get_ff_kf ff in
        let ff_num = ff.T.ff_id in
          SliceName.get kf src_visible ff_num
    in
      M.debug 2 "[slicing:export] get fct_name = %s@." name;
      name

  let visible_mark m = not (!Db.Slicing.Mark.is_bottom m)

  let param_visible ff_opt n = match ff_opt with
    | Isrc | Iproto -> true
    | Iff (ff,_) -> visible_mark (Fct_slice.get_param_mark ff n)

  let body_visible ff_opt = match ff_opt with
    | Iproto -> false | Isrc | Iff _ -> true

  let inst_visible ff_opt inst = match ff_opt with
    | Isrc -> true
    | Iproto -> false
    | Iff (ff,_) ->
        let m = !Db.Slicing.Slice.get_mark_from_stmt ff inst in
          visible_mark m

  let label_visible ff_opt inst label =  match ff_opt with
    | Isrc -> true
    | Iproto -> false
    | Iff (ff,_) ->
        let m = !Db.Slicing.Slice.get_mark_from_label ff inst label in
          visible_mark m

  let data_in_visible ff data_in = match data_in with 
    | None -> true
    | Some data_in ->
        (* it is too difficult to know if the callers of this slice
        * compute [data_in] or not, but let's see if, by chance,
        * some data have been selected manually... *)
        let m = Fct_slice.get_input_loc_under_mark ff data_in in
          if !Db.Slicing.Mark.is_bottom m then
            begin
              M.debug 2 "[Slicing:annotation_visible] data %a invisible@."
                Locations.Zone.pretty data_in;
              false
            end
          else true

  let data_nodes_visible ff (data_nodes, data_in) =
    let visible = data_in_visible ff data_in in
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
            M.debug 2 "[slicing:annotation_visible] node %a invisible@."
              (!Db.Pdg.pretty_node true) n;
            false
          end
        else visi
    in
    let data_visible = List.fold_left is_data_visible visible data_nodes in
      data_visible

  let annotation_visible ff_opt stmt ~before annot =
    M.debug 2 "[slicing:annotation_visible] ?@.";
    match ff_opt with
    | Isrc -> true
    | Iproto -> false
    | Iff (ff,_) ->
        let kf = M.get_ff_kf ff  in
        let pdg = !Db.Pdg.get kf in
        try
        let ctrl_nodes, data_info =
          !Db.Pdg.find_code_annot_nodes pdg before stmt annot in
        let is_visible visi n =
          let m = Fct_slice.get_node_mark ff n in
         if !Db.Slicing.Mark.is_bottom m then
           begin
             M.debug 3 "[slicing:annotation_visible] node %a invisible@."
                 (!Db.Pdg.pretty_node true) n;
             false
           end
         else visi
        in
        let ctrl_visible = List.fold_left is_visible true ctrl_nodes in
        let data_visible = data_nodes_visible ff data_info in
        let visible = (ctrl_visible && data_visible) in
          M.debug 2 "[Slicing:annotation_visible] -> %s@."
              (if visible then "yes" else "no");
          visible
        with Extlib.NotYetImplemented _ -> (* TODO remove this when ok *)
            (M.debug 2 
              "[slicing:annotation_visible] not implemented -> invisible";
            false)

  let fun_precond_visible ff_opt p =
    M.debug 2 "[slicing:fun_precond_visible] %a ?@."
      !Ast_printer.d_predicate_named 
      { name = []; loc = locUnknown; content = p };
    let visible = match ff_opt with
      | Isrc -> true
      | Iproto -> true
      | Iff (ff,_) ->
          let kf = M.get_ff_kf ff  in
          let pdg = !Db.Pdg.get kf in
            try
              let data_info = !Db.Pdg.find_fun_precond_nodes pdg p in
                data_nodes_visible ff data_info
            with Extlib.NotYetImplemented _ -> (* TODO remove this when ok *)
              true (* keep visible at the moment : needed by security analysis *)
    in M.debug 2 "[Slicing:precond_visible] -> %s@."
         (if visible then "yes" else "no");
       visible

  let fun_postcond_visible ff_opt p =
    M.debug 2 "[slicing:fun_postcond_visible] %a ?@."
      !Ast_printer.d_predicate_named 
      { name = []; loc = locUnknown; content = p };
    let visible = match ff_opt with
      | Isrc -> true
      | Iproto -> true
      | Iff (ff,_) ->
          let kf = M.get_ff_kf ff  in
          let pdg = !Db.Pdg.get kf in
            try
              let data_info = !Db.Pdg.find_fun_postcond_nodes pdg p in
                data_nodes_visible ff data_info 
            with Extlib.NotYetImplemented _ -> (* TODO remove this when ok *)
              true (* keep visible at the moment : needed by security analysis *)
    in M.debug 2 "[slicing:fun_postcond_visible] -> %s@."
              (if visible then "yes" else "no");
       visible

  let fun_variant_visible ff_opt v =
    M.debug 2 "[slicing:fun_variant_visible] %a ?@."
      !Ast_printer.d_term v ;
    let visible = match ff_opt with
      | Isrc -> true
      | Iproto -> true
      | Iff (ff,_) ->
          let kf = M.get_ff_kf ff  in
          let pdg = !Db.Pdg.get kf in
            try
              let data_info = !Db.Pdg.find_fun_variant_nodes pdg v in
                data_nodes_visible ff data_info 
            with Extlib.NotYetImplemented _ -> (* TODO remove this when ok *)
              false
    in M.debug 2 "[slicing:fun_variant_visible] -> %s@."
              (if visible then "yes" else "no");
       visible

  let fun_assign_visible _ff_opt _v =
    M.debug 2 "[slicing:fun_assign_visible] ?@.";
    let visible = (* TODO *) false
    in M.debug 2 "[slicing:fun_assign_visible] -> %s@."
              (if visible then "yes" else "no");
       visible

  let loc_var_visible ff_opt var = match ff_opt with
    | Isrc -> true
    | Iproto -> false
    | Iff (ff,_) ->
        let m = !Db.Slicing.Slice.get_mark_from_local_var ff var in
          visible_mark m

  let res_call_visible ff call_stmt = match ff with
    | Isrc -> true
    | Iproto -> false
    | Iff (slice, _) ->
        let key = PdgIndex.Key.call_outret_key call_stmt in
        let _, ff_marks = slice.T.ff_marks in
          try
            let m = PdgIndex.FctIndex.find_info ff_marks key in
            visible_mark m
          with PdgIndex.NotFound -> false

  let result_visible _kf ff = match ff with
    | Isrc | Iproto -> true
    | Iff (slice, _) ->
        let key = PdgIndex.Key.output_key in
        let _, ff_marks = slice.T.ff_marks in
          try
            let m = PdgIndex.FctIndex.find_info ff_marks key in
            visible_mark m
          with PdgIndex.NotFound -> false

  let called_info (project, ff) call_stmt =
    let info = match ff with
      | Isrc | Iproto -> None
      | Iff (slice, _) ->
          try
            let _, ff_marks = slice.T.ff_marks in
            let called, _ =
              PdgIndex.FctIndex.find_call ff_marks call_stmt in
          match called with
            | None | Some (None) ->
                Format.printf "Undefined called function call-%d\n"
                  call_stmt.sid;
                  M.bug "unknown call"
              | Some (Some (T.CallSrc _)) -> None
              | Some (Some (T.CallSlice ff)) ->
                  let kf_ff = M.get_ff_kf ff in
                  let src_visible = is_src_fun_visible project kf_ff in
                    (Some (kf_ff, Iff (ff, src_visible)))
          with PdgIndex.NotFound ->
            (* the functor should call [called_info] only for visible calls *)
            assert false
    in
    M.debug 2 "[slicing:export] called_info stmt %d -> %s@."
        call_stmt.sid (if info = None then "src" else "some slice");
    info
end

let default_slice_names kf _src_visible ff_num =
  let fname = Kernel_function.get_name kf in
  let kf_entry,_ = Globals.entry_point () in
  if Kernel_function.equal kf kf_entry then fname
  else (fname ^ "_slice_" ^ (string_of_int (ff_num)))

let extract new_proj_name ?(f_slice_names=default_slice_names) slicing_project =
  M.debug 0 "[slicing] export to '%s'@." new_proj_name;
  !Db.Slicing.Request.apply_all_internal slicing_project;
  let fresh_project = Project.create new_proj_name in
  let module S = struct let get = f_slice_names end in
  let module Visi = Visibility (S) in
  let module Transform = Filter.F (Visi) in
    Transform.build_cil_file fresh_project slicing_project;
  let ctx = Cmdline.get_selection_context () in
    Project.copy ~only:ctx fresh_project;
    !Db.Sparecode.rm_unused_globals ~project:fresh_project ();
    fresh_project

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

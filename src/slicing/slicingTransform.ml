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

open Cil_types
open Cil

module T = SlicingTypes.Internals
module M = SlicingMacros

(**/**)

let is_fi_top fi =
  (*let m = Fct_slice.get_top_input_mark fi in*)
  fi.T.fi_top (* || not (!Db.Slicing.Mark.is_bottom m) *)



let is_src_fun_called prj kf =
  let table = ref VarinfoSet.empty in
  let rec is_src_fun_called kf =
    if !Db.Slicing.Project.is_directly_called_internal prj kf
      || is_fi_top (M.get_kf_fi prj kf)
    then true
    else let vf = Kernel_function.get_vi kf in
      if VarinfoSet.mem vf !table
      then false (* no way to call the initial [kf]. *)
      else
        (table := VarinfoSet.add vf !table ;
         List.exists (fun (kf,_) -> is_src_fun_called kf) (!Db.Value.callers kf))
  in is_src_fun_called kf

module Visibility (SliceName : sig
                     val get : Db_types.kernel_function -> bool -> int -> string
                   end) = struct
  type t_proj = T.t_project
  type t_fct = (T.t_fct_slice * bool) option

  let fct_info project kf =
    let fi = M.get_kf_fi project kf in
    let slices = M.fi_slices fi in
    let src_called = is_src_fun_called project kf in
    let info_list = List.map (fun ff -> Some (ff, src_called)) slices in
      if src_called
      then None :: info_list
      else info_list

  let fct_name svar ff = match ff with
    | None -> svar.vname
    | Some (ff, src_called) ->
        let kf = M.get_ff_kf ff in
        let ff_num = ff.T.ff_id in
          SliceName.get kf src_called ff_num

  let visible_mark m = not (!Db.Slicing.Mark.is_bottom m)

  let param_visible ff_opt n = match ff_opt with
    | None -> true
    | Some (ff,_) -> visible_mark (Fct_slice.get_param_mark ff n)

  let inst_visible ff_opt inst = match ff_opt with
    | None -> true
    | Some (ff,_) ->
        let m = !Db.Slicing.Slice.get_mark_from_stmt ff inst in
          visible_mark m

  let label_visible ff_opt inst label =  match ff_opt with
    | None -> true
    | Some (ff,_) ->
        let m = !Db.Slicing.Slice.get_mark_from_label ff inst label in
          visible_mark m


            (*
  let annot_visible f_select ff ~before stmt annot =
    let kf = M.get_ff_kf ff  in
    let selections = !Db.Slicing.Select.empty_selects () in
    let spare_mark =
      !Db.Slicing.Mark.make ~addr:false ~ctrl:false ~data:false in
    let selections = f_select selections spare_mark annot ~before stmt kf in
    let is_selected visible sel =
      assert (kf == !Db.Slicing.Select.get_function sel) ;
      visible && (!Db.Slicing.Request.is_already_selected_internal ff sel)
    in
    let visible = Db.Slicing.Select.fold_selects is_selected true selections
    in visible
    *)

  let annotation_visible ff_opt stmt ~before annot = 
    if M.debug1 () then Format.printf "[Slicing:annotation_visible] ?@\n";
    match ff_opt with
    | None -> true
    | Some (ff,_) ->
        let kf = M.get_ff_kf ff  in
        let pdg = !Db.Pdg.get kf in
        let ctrl_nodes, (data_nodes, data_in) =
          !Db.Pdg.find_code_annot_nodes pdg before stmt annot in
        let nodes = ctrl_nodes @ data_nodes in
          (*
        let selections = !Db.Slicing.Select.empty_selects () in
        let spare_mark =
          !Db.Slicing.Mark.make ~addr:false ~ctrl:false ~data:false in
        let selections = !Db.Slicing.Select.select_pdg_nodes
                           selections spare_mark nodes kf in
        let selections = !Db.Slicing.Select.select_stmt_zone
                           selections spare_mark data_in before stmt kf in
    let is_selected visible sel =
      assert (kf == !Db.Slicing.Select.get_function sel) ;
      visible && (!Db.Slicing.Request.is_already_selected_internal ff sel)
    in
    let visible = Db.Slicing.Select.fold_selects is_selected true selections
    *)
        let visible = 
          if (Locations.Zone.equal Locations.Zone.bottom data_in) then true
          else
            (* it is too difficult to know if the callers of this slice
            * compute [data_in] or not, but let's see if, by chance,
            * some data have been selected manually... *)
            let m = Fct_slice.get_input_loc_under_mark ff data_in in
              if !Db.Slicing.Mark.is_bottom m then 
                begin
                  if M.debug2 () then
                    Format.printf 
                      "[Slicing:annotation_visible] data %a invisible@\n"
                      Locations.Zone.pretty data_in;
                  false
                end
              else true
        in
        let is_visible visi n = 
          let m = Fct_slice.get_node_mark ff n in
         if !Db.Slicing.Mark.is_bottom m then 
           begin
             if M.debug2 () then
               Format.printf "[Slicing:annotation_visible] node %a invisible@\n"
                 (!Db.Pdg.pretty_node true) n;
             false
           end
         else visi
        in
        let visible = List.fold_left is_visible visible nodes in
          if M.debug1 () then 
            Format.printf "[Slicing:annotation_visible] -> %s@\n"
              (if visible then "yes" else "no");
          visible


          (*
       let f_select s m =
         !Db.Slicing.Select.select_stmt_annot s m ~spare:true in
         annot_visible f_select ff ~before stmt annot
         *)
  (*
  let loop_annot_visible f_select ff loop annot =
    let loop_entry = match loop.skind with
      | (Loop  (_,{bstmts=loop_entry::_},_,_,_)) -> loop_entry
      | _ -> assert false
    in annot_visible f_select ff ~before:true loop_entry annot

  let loop_invariant_visible ff_opt loop annot = match ff_opt with
    | None -> true
    | Some (ff,_) ->
        loop_annot_visible !Db.Slicing.Select.select_stmt_pred ff loop
          (Logic_const.pred_of_id_pred annot)

  let loop_variant_visible ff_opt loop (annot,_opt) = match ff_opt with
    | None -> true
    | Some (ff,_) ->
        loop_annot_visible !Db.Slicing.Select.select_stmt_term ff loop annot

  let loop_assign_visible _ff_opt _loop _annot = false (*TODO *)

  let loop_pragma_visible _ff_opt _loop _annot = false (* TODO *)
  *)

  let loc_var_visible ff_opt var = match ff_opt with
    | None -> true
    | Some (ff,_) ->
        let m = !Db.Slicing.Slice.get_mark_from_local_var ff var in
          visible_mark m

  let res_call_visible ff call_stmt = match ff with
    | None -> true
    | Some (slice, _) ->
        let key = PdgIndex.Key.call_outret_key call_stmt in
        let _, ff_marks = slice.T.ff_marks in
          try
            let m = PdgIndex.FctIndex.find_info ff_marks key in
            visible_mark m
          with PdgIndex.NotFound -> false

  let result_visible _kf ff = match ff with
    | None -> true
    | Some (slice, _) ->
        let key = PdgIndex.Key.output_key in
        let _, ff_marks = slice.T.ff_marks in
          try
            let m = PdgIndex.FctIndex.find_info ff_marks key in
            visible_mark m
          with PdgIndex.NotFound -> false

  let called_info (project, ff) call_stmt =
    match ff with
      | None -> None
      | Some (slice, _) ->
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
                  let src_called = is_src_fun_called project kf_ff in
                    (Some (kf_ff, Some (ff, src_called)))
          with PdgIndex.NotFound ->
            (* the functor should call [called_info] only for visible calls *)
            assert false
end

let default_slice_names kf _src_called ff_num =
  let kf_entry,_ = Globals.entry_point () in
  let fname = Kernel_function.get_name kf in
  if Kernel_function.equal kf kf_entry then fname
    else (fname ^ "_slice_" ^ (string_of_int (ff_num)))

let extract new_proj_name ?(f_slice_names=default_slice_names) slicing_project =
  if M.info () then Format.printf "[slicing] export to '%s'@\n" new_proj_name;
  !Db.Slicing.Request.apply_all_internal slicing_project;
  let fresh_project = Project.create new_proj_name in
  let module S = struct let get = f_slice_names end in
  let module Visi = Visibility (S) in
  let module Transform = Filter.F (Visi) in
  Transform.build_cil_file fresh_project slicing_project;
  let options = 
    let a o = Project.Selection.add o Kind.Do_Not_Select_Dependencies in
    let add_opt = Project.Selection.empty in
    let add_opt = a Cmdline.MinValidAbsoluteAddress.self add_opt in
    let add_opt = a Cmdline.MaxValidAbsoluteAddress.self add_opt in
    let add_opt = a Cmdline.AutomaticContextMaxDepth.self add_opt in
    let add_opt = a Cmdline.AllocatedContextValid.self add_opt in
    let add_opt = a Cmdline.IgnoreOverflow.self add_opt in
    let add_opt = a Cmdline.UnsafeArrays.self add_opt in
    let add_opt = a Cmdline.LibEntry.self add_opt in
      a Cmdline.MainFunction.self add_opt
      
  in
  Project.copy ~only:options fresh_project;
  fresh_project

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

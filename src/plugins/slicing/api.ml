(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(* ---------------------------------------------------------------------- *)
(** Global data management *)

let split_slice s =
  SlicingParameters.debug ~level:1 "[Api.split_slice]";
  SlicingProject.split_slice s

let merge_slices ff_1 ff_2 ~replace =
  SlicingParameters.debug ~level:1 "[Api.merge_slices]";
  SlicingProject.merge_slices ff_1 ff_2 replace

let copy_slice ff =
  SlicingParameters.debug ~level:1 "[Api.copy_slice]";
  Fct_slice.copy_slice ff

(* ---------------------------------------------------------------------- *)
(** {1 Global setting } *)

let self = SlicingState.self

(* ---------------------------------------------------------------------- *)

  (** {2 Functions with journalized side effects } *)

let set_modes calls callers sliceUndef keepAnnotations () =
  SlicingParameters.Mode.Calls.set calls ;
  SlicingParameters.Mode.Callers.set callers ;
  SlicingParameters.Mode.SliceUndef.set sliceUndef;
  SlicingParameters.Mode.KeepAnnotations.set keepAnnotations
let set_modes =
  Journal.register "Slicing.Api.set_modes"
    (Datatype.func4
       ~label1:("calls", None) Datatype.int
       ~label2:("callers", None) Datatype.bool
       ~label3:("sliceUndef", None) Datatype.bool
       ~label4:("keepAnnotations", None) Datatype.bool
       (Datatype.func
          Datatype.unit
          Datatype.unit))
    set_modes
let set_modes ?(calls=SlicingParameters.Mode.Calls.get ())
    ?(callers=SlicingParameters.Mode.Callers.get ())
    ?(sliceUndef=SlicingParameters.Mode.SliceUndef.get ())
    ?(keepAnnotations=SlicingParameters.Mode.KeepAnnotations.get ())
    () =
  set_modes calls callers sliceUndef keepAnnotations ()

(* ---------------------------------------------------------------------- *)

(** {1 Slicing project } *)
module Project = struct

  (** {2 Values } *)

 let default_slice_names = SlicingTransform.default_slice_names
  let () =
    Journal.Binding.add
      (Datatype.func3
         Kernel_function.ty Datatype.bool Datatype.int Datatype.string)
      default_slice_names
      "Slicing.Api.Project.default_slice_names"

  (** {2 Functions with journalized side effects } *)

  let reset_slicing = Journal.register "Slicing.Api.Project.reset_slicing"
      (Datatype.func Datatype.unit Datatype.unit)
      SlicingState.reset_slicing

  let extract f_slice_names = SlicingTransform.extract ~f_slice_names
  let extract = Journal.register "Slicing.Api.Project.extract"
      (Datatype.func2
         ~label1:("f_slice_names",
                  Some (fun () -> default_slice_names))
         (Datatype.func3
            Kernel_function.ty Datatype.bool Datatype.int Datatype.string)
         Datatype.string
         Project.ty)
      extract
  let extract ?(f_slice_names=default_slice_names) new_proj_name =
    extract f_slice_names new_proj_name

  let print_dot = PrintSlice.build_dot_project
  let print_dot = Journal.register "Slicing.Api.Project.print_dot"
      (Datatype.func2
         ~label1:("filename", None) Datatype.string
         ~label2:("title", None) Datatype.string
         Datatype.unit)
      print_dot
  let print_dot ~filename ~title =
    print_dot filename title

  let change_slicing_level =
    Journal.register "Slicing.Api.Project.change_slicing_level"
      (Datatype.func2
         Kernel_function.ty
         Datatype.int
         Datatype.unit)
      SlicingMacros.change_slicing_level

  (** {2 No needs of Journalization} *)

  let is_directly_called_internal = SlicingMacros.is_src_fun_called
  let is_called = Fct_slice.is_src_fun_called
  let has_persistent_selection = SlicingMacros.has_persistent_selection

  (** {2 Debug} *)

  let pretty = SlicingProject.print_project_and_worklist

end

(* ---------------------------------------------------------------------- *)

(** {1 Mark} *)
module Mark = struct

  type t = SlicingTypes.sl_mark
  let dyn_t = SlicingTypes.dyn_sl_mark

  (** {2 No needs of Journalization} *)

  let compare = SlicingMarks.compare_marks
  let pretty = SlicingMarks.pretty_mark
  let make = SlicingMarks.mk_user_mark
  let is_bottom = SlicingMarks.is_bottom_mark
  let is_spare = SlicingMarks.is_spare_mark
  let is_ctrl = SlicingMarks.is_ctrl_mark
  let is_data = SlicingMarks.is_addr_mark
  let is_addr = SlicingMarks.is_data_mark
  let get_from_src_func = Fct_slice.get_mark_from_src_fun
end

(* ---------------------------------------------------------------------- *)

(** {1 Selection} *)
module Select = struct

  type t = SlicingTypes.sl_select
  let dyn_t = SlicingTypes.Sl_select.ty
  type set = SlicingCmds.set
  module S = Cil_datatype.Varinfo.Map.Make(SlicingTypes.Fct_user_crit)
  let dyn_set = S.ty

  (** {2 Journalized selectors } *)

  let empty_selects = Journal.register
      "Slicing.Api.Select.empty_selects"
      dyn_set
      Cil_datatype.Varinfo.Map.empty

  let select_stmt set spare = SlicingCmds.select_stmt set ~spare
  let select_stmt = Journal.register "Slicing.Api.Select.select_stmt"
      (Datatype.func4
       dyn_set
       ~label2:("spare", None) Datatype.bool
       Stmt.ty
       Kernel_function.ty
       dyn_set)
      select_stmt
  let select_stmt set ~spare =
    select_stmt set spare

  let select_stmt_ctrl set spare = SlicingCmds.select_stmt_ctrl set ~spare
  let select_stmt_ctrl = Journal.register "Slicing.Api.Select.select_stmt_ctrl"
      (Datatype.func4
         dyn_set
         ~label2:("spare", None) Datatype.bool
         Stmt.ty
         Kernel_function.ty
         dyn_set)
      select_stmt_ctrl
  let select_stmt_ctrl set ~spare =
    select_stmt_ctrl set spare

  let select_stmt_lval_rw set mark rd wr stmt eval =
    SlicingCmds.select_stmt_lval_rw set mark ~rd ~wr stmt ~eval
  let select_stmt_lval_rw = Journal.register
      "Slicing.ApiSelect.select_stmt_lval_rw"
      (Datatype.func4
         dyn_set
         SlicingTypes.dyn_sl_mark
         ~label3:("rd", None) Datatype.String.Set.ty
         ~label4:("wr", None) Datatype.String.Set.ty
         (Datatype.func3
            Stmt.ty
            ~label2:("eval", None) Stmt.ty
            Kernel_function.ty
            dyn_set))
      select_stmt_lval_rw
  let select_stmt_lval_rw set mark ~rd ~wr stmt ~eval =
    select_stmt_lval_rw set mark rd wr stmt eval

  let select_stmt_lval set mark lval before stmt eval =
    SlicingCmds.select_stmt_lval set mark lval ~before stmt ~eval
  let select_stmt_lval = Journal.register "Slicing.Api.Select.select_stmt_lval"
      (Datatype.func4
       dyn_set
       Mark.dyn_t
       Datatype.String.Set.ty
       ~label4:("before", None) Datatype.bool
       (Datatype.func3
          Stmt.ty
          ~label2:("eval", None) Stmt.ty
          Kernel_function.ty
          dyn_set))
    select_stmt_lval
  let select_stmt_lval set mark lval ~before stmt ~eval =
    select_stmt_lval set mark lval before stmt eval

  let select_stmt_annots set mark spare threat user_assert slicing_pragma loop_inv loop_var =
    SlicingCmds.select_stmt_annots set mark ~spare ~threat ~user_assert ~slicing_pragma ~loop_inv ~loop_var
  let select_stmt_annots = Journal.register
    "Slicing.Api.Select.select_stmt_annots"
    (Datatype.func4
       dyn_set
       Mark.dyn_t
       ~label3:("spare", None) Datatype.bool
       ~label4:("threat", None) Datatype.bool
       (Datatype.func4
          ~label1:("user_assert", None) Datatype.bool
          ~label2:("slicing_pragma", None) Datatype.bool
          ~label3:("loop_inv", None) Datatype.bool
          ~label4:("loop_var", None) Datatype.bool
          (Datatype.func2
             Stmt.ty
             Kernel_function.ty
             dyn_set)))
    select_stmt_annots
  let select_stmt_annots set mark ~spare ~threat ~user_assert ~slicing_pragma ~loop_inv ~loop_var =
    select_stmt_annots set mark spare threat user_assert slicing_pragma loop_inv loop_var

  let select_func_lval = Journal.register "Slicing.Api.Select.select_func_lval"
      (Datatype.func4
         dyn_set
         Mark.dyn_t
         Datatype.String.Set.ty
         Kernel_function.ty
         dyn_set)
      SlicingCmds.select_func_lval

  let select_func_lval_rw set mark rd wr eval =
    SlicingCmds.select_func_lval_rw set mark ~rd ~wr ~eval
  let select_func_lval_rw = Journal.register
    "Slicing.Api.Select.select_func_lval_rw"
    (Datatype.func4
       dyn_set
       Mark.dyn_t
       ~label3:("rd", None) Datatype.String.Set.ty
       ~label4:("wr", None) Datatype.String.Set.ty
       (Datatype.func2
          ~label1:("eval", None) Stmt.ty
          Kernel_function.ty
          dyn_set))
    select_func_lval_rw
  let select_func_lval_rw set mark ~rd ~wr ~eval =
  select_func_lval_rw set mark rd wr eval

  let select_func_return set spare =
    SlicingCmds.select_func_return set ~spare
  let select_func_return = Journal.register
    "Slicing.Api.Select.select_func_return"
    (Datatype.func3
       dyn_set
       ~label2:("spare", None) Datatype.bool
       Kernel_function.ty
       dyn_set)
    select_func_return
  let select_func_return set ~spare = select_func_return set spare

  let select_func_calls_to set spare =
    SlicingCmds.select_func_calls_to set ~spare
  let select_func_calls_to = Journal.register
      "Slicing.Api.Select.select_func_calls_to"
      (Datatype.func3
         dyn_set
         ~label2:("spare", None) Datatype.bool
         Kernel_function.ty
         dyn_set)
      select_func_calls_to
  let select_func_calls_to set ~spare =
    select_func_calls_to set spare

  let select_func_calls_into set spare =
    SlicingCmds.select_func_calls_into set ~spare
  let select_func_calls_into = Journal.register
      "Slicing.Api.Select.select_func_calls_into"
      (Datatype.func3
         dyn_set
         ~label2:("spare", None) Datatype.bool
         Kernel_function.ty
         dyn_set)
      select_func_calls_into
  let select_func_calls_into set ~spare =
    select_func_calls_into set spare

  let select_func_annots set mark spare threat user_assert slicing_pragma loop_inv loop_var =
    SlicingCmds.select_func_annots set mark ~spare ~threat ~user_assert ~slicing_pragma ~loop_inv ~loop_var
  let select_func_annots = Journal.register
      "Slicing.Api.Select.select_func_annots"
      (Datatype.func4
         dyn_set
         Mark.dyn_t
         ~label3:("spare", None) Datatype.bool
         ~label4:("threat", None) Datatype.bool
         (Datatype.func4
            ~label1:("user_assert", None) Datatype.bool
            ~label2:("slicing_pragma", None) Datatype.bool
            ~label3:("loop_inv", None) Datatype.bool
            ~label4:("loop_var", None) Datatype.bool
            (Datatype.func Kernel_function.ty dyn_set)))
      select_func_annots
  let select_func_annots set mark ~spare ~threat ~user_assert ~slicing_pragma ~loop_inv ~loop_var =
    select_func_annots set mark spare threat user_assert slicing_pragma loop_inv loop_var

  (** {2 No Journalization} *)

  let select_func_zone = SlicingCmds.select_func_zone
  let select_stmt_term = SlicingCmds.select_stmt_term
  let select_stmt_pred = SlicingCmds.select_stmt_pred
  let select_stmt_annot = SlicingCmds.select_stmt_annot
  let select_stmt_zone = SlicingCmds.select_stmt_zone

  let select_pdg_nodes = SlicingCmds.select_pdg_nodes

  (** {2 No Journalization} *)

  let get_function = SlicingCmds.get_select_kf
  let merge_internal = SlicingSelect.merge_db_select

  let add_to_selects_internal = SlicingSelect.Selections.add_to_selects
  let iter_selects_internal = SlicingSelect.Selections.iter_selects_internal
  let fold_selects_internal = SlicingSelect.Selections.fold_selects_internal
  let select_stmt_internal = SlicingSelect.select_stmt_computation
  let select_label_internal = SlicingSelect.select_label
  let select_min_call_internal = SlicingSelect.select_minimal_call
  let select_stmt_zone_internal = SlicingSelect.select_stmt_zone
  let select_zone_at_entry_point_internal = SlicingSelect.select_zone_at_entry
  let select_zone_at_end_internal = SlicingSelect.select_zone_at_end
  let select_modified_output_zone_internal = SlicingSelect.select_modified_output_zone
  let select_stmt_ctrl_internal = SlicingSelect.select_stmt_ctrl
  let select_entry_point_internal = SlicingSelect.select_entry_point
  let select_return_internal = SlicingSelect.select_return
  let select_decl_var_internal = SlicingSelect.select_decl_var
  let select_pdg_nodes_internal = SlicingSelect.select_pdg_nodes

  (** {2 Debug} *)

  let pretty = SlicingSelect.print_select

end

(* ---------------------------------------------------------------------- *)

(** {1 Slice} *)
module Slice = struct

  type t = SlicingTypes.sl_fct_slice
  let dyn_t = SlicingTypes.dyn_sl_fct_slice

  (** {2 Functions with journalized side effects } *)

  let create =
    Journal.register "Slicing.Api.Slice.create"
      (Datatype.func Kernel_function.ty dyn_t)
      SlicingProject.create_slice

  let remove =
    Journal.register "Slicing.Api.Slice.remove"
      (Datatype.func dyn_t Datatype.unit)
      SlicingProject.remove_ff

  let remove_uncalled =
    Journal.register "Slicing.Api.Slice.remove_uncalled"
      (Datatype.func Datatype.unit Datatype.unit)
      SlicingProject.remove_uncalled_slices

  (** {2 No needs of Journalization} *)

  let get_all = SlicingProject.get_slices
  let get_function = SlicingMacros.get_ff_kf
  let get_callers = SlicingProject.get_slice_callers

  let get_called_slice ff stmt =
    match stmt.skind with
    | Instr (Call _ | Local_init (_, ConsInit _, _)) ->
      fst (Fct_slice.get_called_slice ff stmt)
    | _ -> None

  let get_called_funcs ff stmt =
    match stmt.skind with
    | Instr (Call (_,expr_f,_,_)) ->
      if snd (Fct_slice.get_called_slice ff stmt) then
        Kernel_function.Hptset.elements
          (snd (!Db.Value.expr_to_kernel_function (Kstmt stmt) ~deps:None expr_f))
      else
        []
    | Instr (Local_init (_, ConsInit (f, _, _), _)) -> [ Globals.Functions.get f ]
    | _ -> []

  let get_mark_from_stmt = Fct_slice.get_stmt_mark
  let get_mark_from_label = Fct_slice.get_label_mark
  let get_mark_from_local_var = Fct_slice.get_local_var_mark

  let get_mark_from_formal ff var =
    let kf = SlicingMacros.get_ff_kf ff in
    let param_list = Kernel_function.get_formals kf in
    let rec find n var_list = match var_list with
      | [] -> raise Not_found
      | v :: var_list -> if Cil_datatype.Varinfo.equal v var then n
        else find (n+1) var_list
    in let n = find 1 param_list in
    Fct_slice.get_param_mark ff n

  let get_user_mark_from_inputs = Fct_slice.merge_inputs_m1_mark

  let get_num_id = SlicingMacros.get_ff_id

  let from_num_id kf num =
    List.find
      (fun f -> num = SlicingMacros.get_ff_id f)
      (SlicingProject.get_slices kf)

  (** {2 Debug} *)

  let pretty = SlicingProject.pretty_slice

end

(* ---------------------------------------------------------------------- *)

(** {1 Slicing request} *)
module Request = struct

  (** {2 Functions with journalized side effects } *)

  let apply_all propagate_to_callers =
    SlicingCmds.apply_all ~propagate_to_callers
  let apply_all = Journal.register "Slicing.Api.Request.apply_all"
      (Datatype.func
         ~label:("propagate_to_callers", None) Datatype.bool
         Datatype.unit)
      apply_all
  let apply_all ~propagate_to_callers =
    apply_all propagate_to_callers

  let apply_all_internal =
    Journal.register "Slicing.Api.Request.apply_all_internal"
      (Datatype.func Datatype.unit Datatype.unit)
      SlicingCmds.apply_all_actions

  let apply_next_internal =
    Journal.register "Slicing.Api.Request.apply_next_internal"
      (Datatype.func Datatype.unit Datatype.unit)
      SlicingCmds.apply_next_action

  let propagate_user_marks =
    Journal.register "Slicing.Api.Request.propagate_user_marks"
      (Datatype.func Datatype.unit Datatype.unit)
    SlicingCmds.topologic_propagation

  let copy_slice = Journal.register "Slicing.Api.Request.copy_slice"
      (Datatype.func
         Slice.dyn_t
         Slice.dyn_t)
      copy_slice

  let split_slice = Journal.register "Slicing.Api.Request.split_slice"
      (Datatype.func
         Slice.dyn_t
         (Datatype.list Slice.dyn_t))
      split_slice

  let merge_slices ff_1 ff_2 replace =
    merge_slices ff_1 ff_2 ~replace
  let merge_slices = Journal.register "Slicing.Api.Request.merge_slices"
      (Datatype.func3
         Slice.dyn_t
         Slice.dyn_t
         ~label3:("replace", None) Datatype.bool
         Slice.dyn_t)
      merge_slices
  let merge_slices ff_1 ff_2 ~replace =
    merge_slices ff_1 ff_2 replace

  let add_call_slice caller to_call =
    SlicingSelect.call_ff_in_caller ~caller ~to_call
  let add_call_slice =
    Journal.register "Slicing.Api.Request.add_call_slice"
      (Datatype.func2
         ~label1:("caller", None) Slice.dyn_t
         ~label2:("to_call", None) Slice.dyn_t
         Datatype.unit)
      add_call_slice
  let add_call_slice ~caller ~to_call =
    add_call_slice caller to_call

  let add_call_fun caller to_call =
    SlicingSelect.call_fsrc_in_caller ~caller ~to_call
  let add_call_fun =
    Journal.register "Slicing.Api.Request.add_call_fun"
      (Datatype.func2
         ~label1:("caller", None) Slice.dyn_t
         ~label2:("to_call", None) Kernel_function.ty
         Datatype.unit)
      add_call_fun
  let add_call_fun ~caller ~to_call =
    add_call_fun caller to_call

  let add_call_min_fun caller to_call =
    SlicingSelect.call_min_f_in_caller ~caller ~to_call
  let add_call_min_fun =
    Journal.register "Slicing.Api.Request.add_call_min_fun"
      (Datatype.func2
         ~label1:("caller", None) Slice.dyn_t
         ~label2:("to_call", None) Kernel_function.ty
         Datatype.unit)
      add_call_min_fun
  let add_call_min_fun ~caller ~to_call =
    add_call_min_fun caller to_call

  let add_selection = Journal.register "Slicing.Request.add_selection"
      (Datatype.func
         Select.dyn_set Datatype.unit)
    SlicingCmds.add_selection

  let add_persistent_selection =
    Journal.register "Slicing.Request.add_persistent_selection"
      (Datatype.func
         Select.dyn_set Datatype.unit)
      SlicingCmds.add_persistent_selection

  let add_persistent_cmdline =
    Journal.register "Slicing.Request.add_persistent_cmdline"
      (Datatype.func Datatype.unit Datatype.unit)
    SlicingCmds.add_persistent_cmdline

  (** {2 No needs of Journalization} *)

  let is_request_empty_internal = SlicingProject.is_request_empty

  let add_slice_selection_internal = SlicingSelect.add_ff_selection
  let add_selection_internal = SlicingSelect.add_fi_selection

  (** {2 Debug} *)

  let pretty = SlicingProject.print_proj_worklist

end
(* ---------------------------------------------------------------------- *)
(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

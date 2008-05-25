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

(** Those functions were previously outside the slicing module to show how to
 * use the slicing API. So, there are supposed to use the slicing module through
 * Db.Slicing only. There are mainly high level functions which make easier
 * to achieve simple tasks. *)

exception Unknown_data of string
open Cil
open Cil_types
open Db
open Db_types

(** Utilities for [kinstr]. *)
module Kinstr: sig
  val iter_from_func : (stmt -> unit) -> kernel_function -> unit
  val fold_from_func : ('a -> stmt -> 'a) -> 'a -> kernel_function -> 'a
  val filter_from_func : (stmt -> bool) -> kernel_function -> stmt list
  val get_called_funcs : stmt -> kernel_function list
  val is_call_to : stmt -> kernel_function -> bool
  val is_rw_zone : (Locations.Zone.t option * Locations.Zone.t option) -> stmt -> Locations.Zone.t option * Locations.Zone.t option
  end
  =
struct
  (** Iter on statements of a kernel function *)
  let iter_from_func f kf =
    let definition = Kernel_function.get_definition kf
    and visitor = object
      inherit nopCilVisitor as super
      method vstmt stmt = f stmt; super#vstmt stmt
        (* speed up *)
      method vvdec _ = SkipChildren (* via visitCilFunction *)
      method vspec _ = SkipChildren (* via visitCilFunction *)
      method vcode_annot _ = SkipChildren (* via Code_annot stmt *)
      method vloop_annot _ = SkipChildren (* via Loop stmt *)
      method vexpr _ = SkipChildren (* via stmt such as Return, IF, ... *)
      method vlval _ = SkipChildren (* via stmt such as Set, Call, Asm, ... *)
      method vattr _ = SkipChildren (* via Asm stmt *)
      method vvrbl _ = assert false
      method voffs _ = assert false
      method vinitoffs _ = assert false
      method vglob _ = assert false
      method vinit _ = assert false
      method vtype _ = assert false
      method vattrparam _ = assert false
      method vlogic_type _ = assert false
      method vterm _ = assert false
      method vterm_node _ = assert false
      method vterm_lval _ = assert false
      method vterm_lhost _ = assert false
      method vterm_offset _ = assert false
      method vlogic_info _ = assert false
      method vlogic_var _ = assert false
      method vquantifiers _ = assert false
      method vpredicate _ = assert false
      method vpredicate_named _ = assert false
      method vpredicate_info _ = assert false
      method vc_initializer _ = assert false
      method vbehavior _ = assert false
      method vtype_annot _ = assert false
      method vannotation _ = assert false
    end
    in ignore (visitCilFunction (visitor:>cilVisitor) definition)

  (** Fold on statements of a kernel function *)
  let fold_from_func f acc kf =
    let ac = ref acc in
    let fold ki = ignore (ac := f (!ac) ki)
    in iter_from_func fold kf ; !ac

  (** Filter statements of a kernel function *)
  let filter_from_func f kf =
    fold_from_func (fun a ki -> if f ki then ki::a else a) [] kf

  (** Functions that may be called (directly or indirectly via pointer) by the statement.*)
  let get_called_funcs ki =
    match ki.skind with
      | Instr (Call (_,expr_f,_,_)) ->
          snd (!Value.expr_to_kernel_function
                   (Kstmt ki)
                   ~with_alarms:CilE.warn_none_mode
                   ~deps:None
                   expr_f)
       | _ -> []

  (** Is statement call (direct or indirect via pointer) to [kf] *)
  let is_call_to ki kf =
    List.exists (fun caller -> caller == kf) (get_called_funcs ki)

  let get_rw_zone ki =
    assert (Db.Value.is_computed ());
    let lval_process read_zone kstmt lv =
      let deps, looking_for =
        (* The modified locationss are [looking_for], those address are
           function of [deps]. *)
        !Db.Value.lval_to_loc_with_deps
           ~with_alarms:CilE.warn_none_mode
          ~skip_base_deps:false
          ~deps:read_zone
          kstmt
          lv
      in deps, Locations.valid_enumerate_bits looking_for
    in match ki.skind with
      | Switch (exp,_,_,_)
      | If (exp,_,_,_) ->
          !Db.From.find_deps_no_transitivity (Kstmt ki) exp, Locations.Zone.bottom
      | Instr (Set (lv,exp,_)) ->
          let kstmt = Kstmt ki in
          let read_zone = !Db.From.find_deps_no_transitivity kstmt exp
          in lval_process read_zone kstmt lv
      | Instr (Call (lvaloption,funcexp,argl,_)) ->
          let kstmt = Kstmt ki in
          let read_zone = !Db.From.find_deps_no_transitivity kstmt funcexp in
          let read_zone = List.fold_right (fun arg inputs ->
                                             let arg_inputs = !Db.From.find_deps_no_transitivity kstmt arg
	                                     in Locations.Zone.join inputs arg_inputs)
	    argl read_zone in
            (match lvaloption with
               | None ->read_zone , Locations.Zone.bottom
               | Some lv -> lval_process read_zone kstmt lv)
      | _ -> Locations.Zone.bottom, Locations.Zone.bottom

  let is_rw_zone (rd_zone_opt, wr_zone_opt) ki =
    let rd_zone, wr_zone = get_rw_zone ki in
    let inter_zone zone_opt zone =
      match zone_opt with
        | None -> zone_opt
        | Some zone_requested ->
            if Locations.Zone.intersects zone_requested zone
            then let inter = Locations.Zone.narrow zone_requested zone
            in Some inter
            else None
    in inter_zone rd_zone_opt rd_zone, inter_zone wr_zone_opt wr_zone
end

(** build recursively all the change_call for all the callers to kf in
 * order to call ff instead. *)
let prop_to_callers project (kf, ff) =
  let rec prop kf ff =
    let callers = !Db.Value.callers kf in
    let process_caller (kf_caller,_) =
      let ff_callers = match !Slicing.Slice.get_all project kf_caller with
      | [] -> [!Slicing.Slice.create project kf_caller]
      | l -> l
      in
      List.iter
        (fun caller ->
           !Slicing.Request.add_call_slice project ~caller ~to_call:ff;
           prop kf_caller caller)
        ff_callers;
    in
      List.iter process_caller callers
  in prop kf ff

let has_user_marks slice =
  let inputs_user_mark = !Slicing.Slice.get_user_mark_from_inputs slice in
  not (!Slicing.Mark.is_bottom inputs_user_mark)

(** Propagate each slice containing a user mark to all callers *)
let propagate_user_marks_to_callers project kf =
  let slices = !Slicing.Slice.get_all project kf in
  List.iter
    (fun slice ->
       if has_user_marks slice then prop_to_callers project (kf,slice))
    slices

(** Topologically propagate user marks to callers in whole project *)
let topologic_propagation project =
  !Slicing.Request.apply_all_internal project;
  !Db.Semantic_Callgraph.topologically_iter_on_functions
    (fun kf ->
       if Cmdline.Debug.get () > 0 then
	 Format.printf "Doing %a@." Kernel_function.pretty_name kf;
       propagate_user_marks_to_callers project kf;
       !Slicing.Request.apply_all_internal project)

let add_to_selection set selection _kf =
  !Db.Slicing.Select.add_to_selects_internal selection set ; set
  (* (selection, kf)::set *)

(** Registered as a slicing selection function:
    Add a selection of the pdg nodes. *)
let select_pdg_nodes set mark nodes kf =
  let selection = !Db.Slicing.Select.select_pdg_nodes_internal kf nodes mark
  in add_to_selection set selection kf

(** Registered as a slicing selection function:
    Add a selection of the statement. *)
let select_stmt set ~spare ki kf =
  let stmt_mark = !Db.Slicing.Mark.make
                    ~data:(not spare) ~addr:(not spare) ~ctrl:(not spare) in
  let selection = !Db.Slicing.Select.select_stmt_internal kf ki stmt_mark
  in add_to_selection set selection kf

(** Registered as a slicing selection function:
    Add a selection of calls to a [kf]. *)
let select_func_calls_to set ~spare kf =
  assert (Db.Value.is_computed ());
  let callers = !Db.Value.callers kf in
  let select_calls (caller,_) acc =
    Kinstr.fold_from_func (fun set ki ->
                           if Kinstr.is_call_to ki kf then
                             select_stmt set ~spare ki caller
                           else
                             set) acc caller
  in List.fold_right select_calls callers set


let select_min_call set ~spare ki kf =
  let stmt_mark =
    !Db.Slicing.Mark.make ~data:(not spare) ~addr:(not spare) ~ctrl:(not spare)
  in
  let selection = !Db.Slicing.Select.select_min_call_internal kf ki stmt_mark
  in add_to_selection set selection kf

(** Registered as a slicing selection function:
    Add a selection of calls to a [kf]. *)
let select_func_calls_into set ~spare kf =
  assert (Db.Value.is_computed ());
  let callers = !Db.Value.callers kf in
  let select_calls (caller,_) acc =
    Kinstr.fold_from_func (fun set ki ->
                           if Kinstr.is_call_to ki kf then
                             select_min_call set ~spare ki caller
                           else
                             set) acc caller
  in List.fold_right select_calls callers set

(** Registered as a slicing selection function:
    Add selection of function ouputs. *)
let select_func_zone set mark zone kf =
  let selection = !Db.Slicing.Select.select_output_zone_internal kf zone mark
  in add_to_selection set selection kf

(** Registered as a slicing selection function:
    Add a selection of the [kf] return statement. *)
let select_func_return set ~spare kf =
  try
    let ki = Kernel_function.find_return kf
    in select_stmt set ~spare ki kf
  with Kernel_function.No_Definition ->
    (* let zone = Value.find_return_loc kf : this zone doesn't exist... *)
    let pdg = !Db.Pdg.get kf in
    let out0_node = !Db.Pdg.find_ret_output_node pdg in
    let mark = !Db.Slicing.Mark.make
                    ~data:(not spare) ~addr:(not spare) ~ctrl:(not spare) in
      select_pdg_nodes set mark [out0_node] kf

(** Registered as a slicing selection function:
    Add a selection of the statement reachability.
    Note: add also a transparent selection on the whole statement. *)
let select_stmt_ctrl set ~spare ki kf =
  let ctrl_mark =
    !Db.Slicing.Mark.make ~data:false ~addr:false ~ctrl:(not spare) in
  let selection = !Db.Slicing.Select.select_stmt_internal kf ki ctrl_mark
  in add_to_selection set selection kf

(** Registered as a slicing selection function:
    Add a selections of data relative to a statement.
    Note: add also a transparent selection on the whole statement. *)
let select_stmt_zone set mark zone ~before ki kf =
  let selection =
    !Db.Slicing.Select.select_stmt_zone_internal kf ki ~before zone mark
  in let set = add_to_selection set selection kf
  in select_stmt_ctrl set ~spare:true ki kf

(** Registered as a slicing selection function:
    Add a selections of data relative to a statement.
    Note: add also a transparent selection on the whole statement. *)
let select_stmt_lval set mark lval_str ~before ki kf =
  assert (Db.Value.is_computed ());
  if Cilutil.StringSet.is_empty lval_str
  then set
  else
    let zone =
      Cilutil.StringSet.fold
        (fun lval_str acc ->
           let ki = Kernel_function.find_first_stmt kf in
           let lval_term = !Db.Properties.Interp.lval kf ki lval_str in
           let lval = !Db.Properties.Interp.term_lval_to_lval lval_term in
           let loc = !Db.Value.lval_to_loc ~with_alarms:CilE.warn_none_mode (Kstmt ki) lval in
           let zone = Locations.valid_enumerate_bits loc
           in Locations.Zone.join zone acc)
        lval_str Locations.Zone.bottom
    in select_stmt_zone set mark zone ~before ki kf

let select_lval_rw set mark ~rd ~wr _ki kf ki_opt=
  assert (Db.Value.is_computed ());
   let zone_option lval_str =
    if Cilutil.StringSet.is_empty lval_str
    then None
    else
      let zone =
        Cilutil.StringSet.fold
          (fun lval_str acc ->
             let ki = Kernel_function.find_first_stmt kf in
             let lval_term = !Db.Properties.Interp.lval kf ki lval_str in
             let lval = !Db.Properties.Interp.term_lval_to_lval lval_term in
             let loc = !Db.Value.lval_to_loc ~with_alarms:CilE.warn_none_mode (Kstmt ki) lval in
             let zone = Locations.valid_enumerate_bits loc
             in Locations.Zone.join zone acc)
          lval_str Locations.Zone.bottom
      in (* Format.printf "@\nselect_lval_rw sid=%d zone=%a@." ki.sid Locations.Zone.pretty zone; *)
        Some zone
   in match (zone_option rd), (zone_option wr) with
     | None, None -> set
     | (_, _) as zone_option_rw ->
         let ac = ref set in
         let iter kf ki =
           let rd_zone_opt, wr_zone_opt = Kinstr.is_rw_zone zone_option_rw ki in
           let select ~before zone_opt =
             match zone_opt with
               | None -> !ac
               | Some zone ->
                   (* Format.printf "@\nselect_lval_rw sid=%d before=%b zone=%a@." ki.sid before Locations.Zone.pretty zone; *)
                   select_stmt_zone !ac mark zone ~before ki kf ;
           in
             ac := select ~before:true rd_zone_opt ;
             ac := select ~before:false wr_zone_opt
         in (match ki_opt with
               | Some ki -> iter kf ki
               | None ->
		   Globals.Functions.iter
		     (fun kf ->
                        if Kernel_function.is_definition kf
			  && !Db.Value.is_called kf
			then
			  Kinstr.iter_from_func (iter kf) kf;));
           (* !Db.Slicing.Select.iter_selects_internal
              (fun sel -> Format.printf "@\nselect_lval_rw sel=%a" !Db.Slicing.Select.pretty sel) !ac; *)
           !ac

(** Registered as a slicing selection function:
    Add a selections of data relative to a statement.
    Note: add also a transparent selection on the whole statement. *)
let select_stmt_lval_rw set mark ~rd ~wr ki kf = select_lval_rw set mark ~rd ~wr ki kf (Some ki)

(**
    Add a selection of the declaration of [vi]. *)
let select_decl_var set mark vi kf =
  if vi.Cil_types.vglob (* no slicing request on globals *)
  then set
  else
    let pdg = !Db.Pdg.get kf in
    let node = !Db.Pdg.find_decl_var_node pdg vi in
      select_pdg_nodes set mark [node] kf

let select_ZoneAnnot_pragmas set ~spare pragmas kf =
  let set =
    Cilutil.StmtSet.fold  (* selection related to //@ slice pragma stmt *)
      (fun ki' acc -> select_stmt acc ~spare ki' kf)
      pragmas.Properties.Interp.To_zone.stmt set
  in Cilutil.StmtSet.fold  (* selection related to //@ slice pragma ctrl/expr *)
       (fun ki' acc -> select_stmt_ctrl acc ~spare ki' kf)
       pragmas.Properties.Interp.To_zone.ctrl set

let select_ZoneAnnot_zones_decl_vars set mark (zones,decl_vars) kf =
  let set = VarinfoSet.fold
    (fun vi acc -> select_decl_var acc mark vi kf) decl_vars set
  in List.fold_right
       (fun z acc -> (* selection related to the parsing/compilation of the annotation *)
          select_stmt_zone acc mark z.Properties.Interp.To_zone.zone
            ~before:z.Properties.Interp.To_zone.before z.Properties.Interp.To_zone.ki kf)
       zones set

(** Registered as a slicing selection function:
    Add selection of the annotations related to a statement.
    Note: add also a transparent selection on the whole statement. *)
let select_stmt_pred set mark pred ~before ki kf =
  let zones_decl_vars = !Properties.Interp.To_zone.from_stmt_pred pred ~before (ki, kf)
  in select_ZoneAnnot_zones_decl_vars set mark zones_decl_vars kf

(** Registered as a slicing selection function:
    Add selection of the annotations related to a statement.
    Note: add also a transparent selection on the whole statement. *)
let select_stmt_term set mark term ~before ki kf =
  let zones_decl_vars = !Properties.Interp.To_zone.from_stmt_term term ~before (ki, kf)
  in select_ZoneAnnot_zones_decl_vars set mark zones_decl_vars kf

(** Registered as a slicing selection function:
    Add selection of the annotations related to a statement.
    Note: add also a transparent selection on the whole statement. *)
let select_stmt_annot set mark ~spare annot ~before ki kf =
  let zones_decl_vars,pragmas =
    !Properties.Interp.To_zone.from_stmt_annot annot ~before (ki, kf)
  in let set = select_ZoneAnnot_pragmas set ~spare pragmas kf
  in select_ZoneAnnot_zones_decl_vars set mark zones_decl_vars kf

(** Registered as a slicing selection function:
    Add selection of the annotations related to a statement.
    Note: add also a transparent selection on the whole statement. *)
let select_stmt_annots set mark ~spare ~ai ~user_assert
    ~slicing_pragma ~loop_inv ~loop_var ki kf =
  let zones_decl_vars,pragmas =
    !Properties.Interp.To_zone.from_stmt_annots
      (Some (!Properties.Interp.To_zone.code_annot_filter
               ~ai ~user_assert ~slicing_pragma
              ~loop_inv ~loop_var ~others:false))
      (ki, kf)
  in let set = select_ZoneAnnot_pragmas set ~spare pragmas kf
  in select_ZoneAnnot_zones_decl_vars set mark zones_decl_vars kf

(** Registered as a slicing selection function:
    Add a selection of the annotations related to a statement.
    Note: add also a transparent selection on the whole statement. *)
let select_func_annots set mark ~spare ~ai ~user_assert ~slicing_pragma ~loop_inv ~loop_var kf =
  let zones_decl_vars,pragmas =
    !Properties.Interp.To_zone.from_func_annots Kinstr.iter_from_func
         (Some
            (!Properties.Interp.To_zone.code_annot_filter
               ~ai ~user_assert ~slicing_pragma ~loop_inv
               ~loop_var ~others:false))
      kf
  in let set = select_ZoneAnnot_pragmas set ~spare pragmas kf
  in select_ZoneAnnot_zones_decl_vars set mark zones_decl_vars kf

(** Registered as a slicing selection function:
    Add selection of function ouputs. *)
let select_func_lval set mark lval_str kf =
  if Cilutil.StringSet.is_empty lval_str
  then set
  else
    let ki = Kernel_function.find_return kf in
    select_stmt_lval set mark lval_str ~before:false ki kf

(** Registered as a slicing selection function:
    Add selection of function ouputs. *)
let select_func_lval_rw set mark ~rd ~wr kf =
  if Cilutil.StringSet.is_empty rd &&
    Cilutil.StringSet.is_empty wr
  then set
  else
    let ki = Kernel_function.find_return kf in
    select_lval_rw set mark ~rd ~wr ki kf None

(** Registered as a slicing request function:
    Add selections to all concerned slices, as slicing requests and apply them,
    kernel function by kernel function.
    Note:
    - the function begins by applying the remaining internal requests.
    - the requests added for the last kernel function are not applied. *)
let add_selection project set =
  let add_selection prev selection =
    let kf = !Slicing.Select.get_function selection in
    let r = match prev with
        None -> !Slicing.Request.apply_all_internal project ; Some (kf)
      | Some prev_kf -> if prev_kf == kf then prev else None
    and make_request slice =
      !Slicing.Request.add_slice_selection_internal project slice selection
    and slices =
      let slices = !Slicing.Slice.get_all project kf
      in if slices = [] then [!Slicing.Slice.create project kf] else slices
    in List.iter make_request slices ;
      r
  in ignore (Slicing.Select.fold_selects add_selection None set)

(** Registered as a slicing request function:
    Add selections that will be applied to all the slices of the function
    (already existing or created later)
    Note:
    - the function begins by applying the remaining internal requests.
    - the requests added for the last kernel function are not applied. *)
let add_persistent_selection project set =
  (* Format.printf "@\nadd_persistent_selection@."; *)
  let add_selection prev selection =
    let kf = !Slicing.Select.get_function selection in
    let r = match prev with
        None -> !Slicing.Request.apply_all_internal project ; Some (kf)
      | Some prev_kf -> if prev_kf == kf then prev else None
    in !Slicing.Request.add_selection_internal project selection; r
  in ignore (Slicing.Select.fold_selects add_selection None set)

let apply_all project ~propagate_to_callers =
  assert (not propagate_to_callers) ;
  try
    while (true)
      do
        (* Format.printf "@\napply_next_internal@."; *)
        !Db.Slicing.Request.apply_next_internal project
      done
  with Not_found -> ()

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)

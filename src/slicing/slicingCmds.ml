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

(** Those functions were previously outside the slicing module to show how to
 * use the slicing API. So, there are supposed to use the slicing module through
 * Db.Slicing only. There are mainly high level functions which make easier
 * to achieve simple tasks. *)

exception Unknown_data of string
open Cil
open Cilutil
open Cil_types
open Db

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
          Kernel_function.Hptset.elements
            (snd (!Value.expr_to_kernel_function
                   (Kstmt ki)
                   ~with_alarms:CilE.warn_none_mode
                   ~deps:None
                   expr_f))
       | _ -> []

  (** Is statement call (direct or indirect via pointer) to [kf] *)
  let is_call_to ki kf =
    List.exists (fun caller -> caller == kf) (get_called_funcs ki)

  (** Get directly read/writen [Zone.t] by the statement.
    * i.e. directly means when [ki] is a call,
      it doesn't don't look at the assigns clause of the called function. *)
  let get_rw_zone stmt = (* returns [Zone.t read],[Zone.t writen] *)
    assert (Db.Value.is_computed ());
    let lval_process read_zone stmt lv =
      (* returns [read_zone] joined to [Zone.t read] by [lv], [Zone.t writen] by [lv] *)
      let deps, looking_for =
        (* The modified locationss are [looking_for], those address are
           function of [deps]. *)
        !Db.Value.lval_to_loc_with_deps
           ~with_alarms:CilE.warn_none_mode
          ~deps:read_zone
          (Kstmt stmt)
          lv
      in deps, Locations.valid_enumerate_bits ~for_writing:true looking_for
    in match stmt.skind with
      | Switch (exp,_,_,_)
      | If (exp,_,_,_) ->
          (* returns  [Zone.t read] by condition [exp], [Zone.bottom] *)
          !Db.From.find_deps_no_transitivity stmt exp, Locations.Zone.bottom
      | Instr (Set (lv,exp,_)) ->
          (* returns  [Zone.t read] by [exp, lv], [Zone.t writen] by [lv] *)
          let read_zone = !Db.From.find_deps_no_transitivity stmt exp in
            lval_process read_zone stmt lv
      | Instr (Call (lvaloption,funcexp,argl,_)) ->
          (* returns  [Zone.t read] by [lvaloption, funcexp, argl], [Zone.t writen] by [lvaloption] *)
          let read_zone = !Db.From.find_deps_no_transitivity stmt funcexp in
          let add_args arg inputs =
            Locations.Zone.join inputs (!Db.From.find_deps_no_transitivity stmt arg) in
          let read_zone = List.fold_right add_args argl read_zone in
          let read_zone,write_zone =
            match lvaloption with
              | None ->read_zone , Locations.Zone.bottom
              | Some lv -> lval_process read_zone stmt lv
          in read_zone,write_zone
      | _ -> Locations.Zone.bottom, Locations.Zone.bottom

  (** Look at intersection of [rd_zone_opt]/[wr_zone_opt] with the
      directly read/writen [Zone.t] by the statement.
    * i.e. directly means when [ki] is a call,
      it doesn't don't look at the assigns clause of the called function. *)
  let is_rw_zone (rd_zone_opt, wr_zone_opt) stmt =
    let rd_zone, wr_zone = get_rw_zone stmt in
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
       SlicingParameters.debug ~level:3
         "doing topologic propagation for function: %a"
         Kernel_function.pretty kf;
       !Slicing.Request.apply_all_internal project)

let add_to_selection set selection =
  !Db.Slicing.Select.add_to_selects_internal selection set

(** Registered as a slicing selection function:
    Add a selection of the pdg nodes. *)
let select_pdg_nodes set mark nodes kf =
  let selection = !Db.Slicing.Select.select_pdg_nodes_internal kf nodes mark
  in add_to_selection set selection

(** Registered as a slicing selection function:
    Add a selection of the statement. *)
let select_stmt set ~spare stmt kf =
  let stmt_mark = !Db.Slicing.Mark.make
                    ~data:(not spare) ~addr:(not spare) ~ctrl:(not spare) in
  let selection = !Db.Slicing.Select.select_stmt_internal kf stmt stmt_mark
  in add_to_selection set selection

(** Add a selection to the entrance of the function [kf]
    and add a selection to its return if [~return] is true
    and add a selection to [~inputs] parts of its inputs
    and add a selection to [~ouputs] parts of its outputs*)
let select_entry_point_and_some_inputs_outputs set ~mark kf ~return ~outputs ~inputs =
  SlicingParameters.debug ~level:3
    "select_entry_point_and_some_inputs_outputs %a"
    Kernel_function.pretty kf ;
  let set = let selection = !Db.Slicing.Select.select_entry_point_internal kf mark in
    add_to_selection set selection
  in
  let set =
    if (Locations.Zone.equal Locations.Zone.bottom outputs)
    then set
    else let selection = !Db.Slicing.Select.select_modified_output_zone_internal kf outputs mark in
      add_to_selection set selection
  in
  let set =
    if (Locations.Zone.equal Locations.Zone.bottom inputs)
    then set
    else let selection = !Db.Slicing.Select.select_zone_at_entry_point_internal kf inputs mark in
      add_to_selection set selection
  in if return
    then let selection = !Db.Slicing.Select.select_return_internal kf mark in
      add_to_selection set selection
    else set

(** Add a selection to the entrance of the function [kf]
    and add a selection to its outputs if [~outputs] is true *)
let select_entry_point set ~spare kf ~outputs =
  if !Db.Value.is_called kf then
    let mark =
      !Db.Slicing.Mark.make ~data:(not spare) ~addr:(not spare) ~ctrl:(not spare)
    in
      assert (!Db.Value.is_called kf) ; (* otherwise [!Db.Outputs.get_external kf] gives weird results *)
      select_entry_point_and_some_inputs_outputs set ~mark kf
        ~return:outputs
        ~outputs:(if outputs then !Db.Outputs.get_external kf else Locations.Zone.bottom)
        ~inputs:Locations.Zone.bottom
  else set

(** Registered as a slicing selection function:
    Add a selection of calls to a [kf]. *)
let select_func_calls_to set ~spare kf =
  assert (Db.Value.is_computed ());
  let kf_entry, _library = Globals.entry_point () in
  if (kf_entry == kf) then
    select_entry_point set ~spare kf ~outputs:true
  else
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
      !Db.Slicing.Mark.make ~data:(not spare) ~addr:(not spare) ~ctrl:(not spare) in
    let selection = !Db.Slicing.Select.select_min_call_internal kf ki stmt_mark in
      add_to_selection set selection


(** Registered as a slicing selection function:
    Add a selection of calls to a [kf]. *)
let select_func_calls_into set ~spare kf =
  assert (Db.Value.is_computed ());
  let kf_entry, _library = Globals.entry_point () in
  if (kf_entry == kf) then
    select_entry_point set ~spare kf  ~outputs:false
  else
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
  let selection = !Db.Slicing.Select.select_zone_at_end_internal kf zone mark
  in add_to_selection set selection

(** Registered as a slicing selection function:
    Add a selection of the [kf] return statement. *)
let select_func_return set ~spare kf =
  try
    let ki = Kernel_function.find_return kf
    in select_stmt set ~spare ki kf
  with Kernel_function.No_Statement ->
    let mark =
      !Db.Slicing.Mark.make
        ~data:(not spare) ~addr:(not spare) ~ctrl:(not spare)
    in
    select_entry_point_and_some_inputs_outputs
      set
      ~mark
      kf
      ~return:true
      ~outputs:Locations.Zone.bottom
      ~inputs:Locations.Zone.bottom

(** Registered as a slicing selection function:
    Add a selection of the statement reachability.
    Note: add also a transparent selection on the whole statement. *)
let select_stmt_ctrl set ~spare ki kf =
  let ctrl_mark =
    !Db.Slicing.Mark.make ~data:false ~addr:false ~ctrl:(not spare) in
  let selection = !Db.Slicing.Select.select_stmt_internal kf ki ctrl_mark
  in add_to_selection set selection

(** Registered as a slicing selection function:
    Add a selection of data relative to a statement.
    Note: add also a transparent selection on the whole statement. *)
let select_stmt_zone set mark zone ~before ki kf =
  let selection =
    !Db.Slicing.Select.select_stmt_zone_internal kf ki ~before zone mark
  in let set = add_to_selection set selection
  in select_stmt_ctrl set ~spare:true ki kf

(** Registered as a slicing selection function:
    Add a selection of data relative to a statement.
    Variables of [lval_str] string are bounded
    relatively to the scope of the statement [~scope].
    The interpretation of the address of the lvalues is
    done just before the execution of the statement [~eval].
    The selection preserve the value of these lvalues before
    or after (c.f. boolean [~before]) the statement [ki].
    Note: add also a transparent selection on the whole statement. *)
let select_stmt_lval set mark lval_str ~before ki ~scope ~eval kf =
  assert (Db.Value.is_computed ());
  if Datatype.String.Set.is_empty lval_str
  then set
  else
    let zone =
      Datatype.String.Set.fold
        (fun lval_str acc ->
           let lval_term = !Db.Properties.Interp.lval kf scope lval_str in
           let lval =
             !Db.Properties.Interp.term_lval_to_lval ~result:None lval_term
           in
           let loc =
             !Db.Value.lval_to_loc
               ~with_alarms:CilE.warn_none_mode
               (Kstmt eval)
               lval
           in
           let zone = Locations.valid_enumerate_bits ~for_writing:false loc in
           Locations.Zone.join zone acc)
        lval_str
        Locations.Zone.bottom
    in
    select_stmt_zone set mark zone ~before ki kf

(** Add a selection of data relative to read/write accesses.
    Interpret the [~rd] lvalues and the [~wr] lvalues from [~scope], [~eval]
    statements of [kf]:
    - Variables of [lval_str] string are bounded
      relatively to the scope of the statement [~scope].
    - The interpretation of the address of the lvalues is
      done just before the execution of the statement [~eval].
    Find read/write accesses from the whole project if [ki_opt]=None.
    Otherwise, restrict the research among the direct effect of [ki_opt] statement.
    i.e. when [ki_opt] is a call, the selection doesn't look at the assigns clause
    of a call. *)
let select_lval_rw set mark ~rd ~wr ~scope ~eval kf ki_opt=
  assert (Db.Value.is_computed ());
   let zone_option ~for_writing lval_str =
    if Datatype.String.Set.is_empty lval_str
    then None
    else
      let zone =
        Datatype.String.Set.fold
          (fun lval_str acc ->
             let lval_term = !Db.Properties.Interp.lval kf scope lval_str in
             let lval = !Db.Properties.Interp.term_lval_to_lval ~result:None lval_term in
             let loc = !Db.Value.lval_to_loc ~with_alarms:CilE.warn_none_mode (Kstmt eval) lval in
             let zone = Locations.valid_enumerate_bits ~for_writing loc
             in Locations.Zone.join zone acc)
          lval_str Locations.Zone.bottom
      in SlicingParameters.debug ~level:3
           "select_lval_rw %a zone=%a"
           Kernel_function.pretty kf
           Locations.Zone.pretty zone;
        Some zone
   in
   let zone_rd_opt = zone_option ~for_writing:false rd in
   let zone_wr_opt = zone_option ~for_writing:true wr in
   match zone_rd_opt, zone_wr_opt with
     | None, None -> set
     | (_, _) as zone_option_rw ->
         let ac = ref set in
         let select_rw_from_stmt kf ki =
           let rd_zone_opt, wr_zone_opt = Kinstr.is_rw_zone zone_option_rw ki in
           let select_zone ~before zone_opt =
             match zone_opt with
               | None -> !ac
               | Some zone ->
                   SlicingParameters.debug ~level:3
                     "select_lval_rw sid=%d before=%b zone=%a"
                     ki.sid before Locations.Zone.pretty zone;
                   select_stmt_zone !ac mark zone ~before ki kf ;
           in
             ac := select_zone ~before:true rd_zone_opt ;
             ac := select_zone ~before:false wr_zone_opt
         in (match ki_opt with
               | Some ki -> select_rw_from_stmt kf ki
               | None ->
                   Globals.Functions.iter
                     (fun kf ->
                        if !Db.Value.is_called kf then
                          if not (!Db.Value.use_spec_instead_of_definition kf)
                          then (* Called function with source code: just looks at its stmt *)
                            Kinstr.iter_from_func (select_rw_from_stmt kf) kf
                          else begin (* Called function without source code: looks at its effect *)
                            let select_inter_zone fsel zone_opt zone =
                              match zone_opt with
                                | None -> ()
                                | Some zone_requested ->
                                    (* Format.printf "@\nselect_lval_rw zone_req=%a zone=%a@."
                                       Locations.Zone.pretty zone_requested
                                       Locations.Zone.pretty zone; *)
                                    if Locations.Zone.intersects zone_requested zone
                                    then let inter = Locations.Zone.narrow zone_requested zone
                                    in fsel inter
                                    else () in
                            let select_wr outputs =
                              ac := select_entry_point_and_some_inputs_outputs !ac ~mark kf
                                ~return:false ~outputs ~inputs:Locations.Zone.bottom
                            and select_rd inputs =
                              ac := select_entry_point_and_some_inputs_outputs !ac ~mark kf
                                ~return:false ~inputs ~outputs:Locations.Zone.bottom

                            in
                              assert (!Db.Value.is_called kf) ; (* otherwise [!Db.Outputs.get_external kf] gives weird results *)
                              select_inter_zone select_wr zone_wr_opt (!Db.Outputs.get_external kf) ;
                              select_inter_zone select_rd zone_rd_opt (!Db.Inputs.get_external kf)
                          end
                     ));
           !ac

(** Registered as a slicing selection function:
    Add a selection of rw accesses to lvalues relative to a statement.
    Variables of [~rd] and [~wr] string are bounded
    relatively to the scope of the statement [~scope].
    The interpretation of the address of the lvalues is
    done just before the execution of the statement [~eval].
    The selection preserve the [~rd] and ~[wr] accesses
    directly contained into the statement [ki].
    i.e. when [ki] is a call, the selection doesn't look at
    the assigns clause of the called function.
    Note: add also a transparent selection on the whole statement.*)
let select_stmt_lval_rw set mark ~rd ~wr ki ~scope ~eval kf =
  select_lval_rw set mark ~rd ~wr ~scope ~eval kf (Some ki)

(** Add a selection of the declaration of [vi]. *)
let select_decl_var set mark vi kf =
  let selection = !Db.Slicing.Select.select_decl_var_internal kf vi mark in
    add_to_selection set selection

let select_ZoneAnnot_pragmas set ~spare pragmas kf =
  let set =
    Cil_datatype.Stmt.Set.fold
      (* selection related to statement assign and //@ slice pragma stmt *)
      (fun ki' acc -> select_stmt acc ~spare ki' kf)
      pragmas.Properties.Interp.To_zone.stmt set
  in
  Cil_datatype.Stmt.Set.fold
    (* selection related to //@ slice pragma ctrl/expr *)
    (fun ki' acc -> select_stmt_ctrl acc ~spare ki' kf)
    pragmas.Properties.Interp.To_zone.ctrl
    set

let select_ZoneAnnot_zones_decl_vars set mark (zones,decl_vars) kf =
  let set =
    Cil_datatype.Varinfo.Set.fold
      (fun vi acc -> select_decl_var acc mark vi kf)
      decl_vars
      set
  in
  List.fold_right
    (fun z acc ->
      (* selection related to the parsing/compilation of the annotation *)
      select_stmt_zone acc mark
        z.Properties.Interp.To_zone.zone
        ~before:z.Properties.Interp.To_zone.before
        z.Properties.Interp.To_zone.ki
        kf)
    zones set

let get_or_raise (info_data_opt, info_decl) = match info_data_opt with
  | None ->
      (* TODO: maybe we can know how to use [info_decl] ? *)
      raise (Extlib.NotYetImplemented
                     !Logic_interp.To_zone.not_yet_implemented)
  | Some info_data -> info_data, info_decl

(** Registered as a slicing selection function:
    Add selection of the annotations related to a statement.
    Note: add also a transparent selection on the whole statement. *)
let select_stmt_pred set mark pred ki kf =
  let zones_decl_vars =
    !Properties.Interp.To_zone.from_pred pred
      (!Properties.Interp.To_zone.mk_ctx_stmt_annot kf ki)
  in select_ZoneAnnot_zones_decl_vars set mark (get_or_raise zones_decl_vars) kf

(** Registered as a slicing selection function:
    Add selection of the annotations related to a statement.
    Note: add also a transparent selection on the whole statement. *)
let select_stmt_term set mark term ki kf =
  let zones_decl_vars = !Properties.Interp.To_zone.from_term term (!Properties.Interp.To_zone.mk_ctx_stmt_annot kf ki)
  in select_ZoneAnnot_zones_decl_vars set mark (get_or_raise zones_decl_vars) kf

(** Registered as a slicing selection function:
    Add selection of the annotations related to a statement.
    Note: add also a transparent selection on the whole statement. *)
let select_stmt_annot set mark ~spare annot ki kf =
  let zones_decl_vars,pragmas =
    !Properties.Interp.To_zone.from_stmt_annot annot (ki, kf)
  in let set = select_ZoneAnnot_pragmas set ~spare pragmas kf
  in select_ZoneAnnot_zones_decl_vars set mark (get_or_raise zones_decl_vars) kf

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
  in select_ZoneAnnot_zones_decl_vars set mark (get_or_raise zones_decl_vars) kf

(** Registered as a slicing selection function:
    Add a selection of the annotations related to a function. *)
let select_func_annots set mark ~spare ~ai ~user_assert ~slicing_pragma ~loop_inv ~loop_var kf =
  let zones_decl_vars,pragmas =
    !Properties.Interp.To_zone.from_func_annots Kinstr.iter_from_func
         (Some
            (!Properties.Interp.To_zone.code_annot_filter
               ~ai ~user_assert ~slicing_pragma ~loop_inv
               ~loop_var ~others:false))
      kf
  in let set = select_ZoneAnnot_pragmas set ~spare pragmas kf
  in select_ZoneAnnot_zones_decl_vars set mark (get_or_raise zones_decl_vars) kf

(** Registered as a slicing selection function:
    Add selection of function ouputs.
    Variables of [lval_str] string are bounded
    relatively to the scope of the first statement of [kf].
    The interpretation of the address of the lvalues is
    done just before the execution of the first statement [kf].
    The selection preserve the value of these lvalues before
    execution of the return statement.  *)
let select_func_lval set mark lval_str kf =
  if Datatype.String.Set.is_empty lval_str
  then set
  else
    let ki_scope_eval = Kernel_function.find_first_stmt kf in
    select_stmt_lval
      set mark lval_str
      ~before:false
      (Kernel_function.find_return kf)
      ~scope:ki_scope_eval
      ~eval:ki_scope_eval
      kf

(** Registered as a slicing selection function:
    Add a selection of data relative to read/write accesses.
    Interpret the [~rd] lvalues and the [~wr] lvalues from [~scope], [~eval] statements of [kf]:
    - Variables of [lval_str] string are bounded
      relatively to the scope of the statement [~scope].
    - The interpretation of the address of the lvalues is
      done just before the execution of the statement [~eval].
    Find read/write accesses from the whole project if [ki_opt]=None. *)
let select_func_lval_rw set mark ~rd ~wr ~scope ~eval kf =
  if Datatype.String.Set.is_empty rd && Datatype.String.Set.is_empty wr
  then set
  else select_lval_rw set mark ~rd ~wr ~scope ~eval kf None

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
  in ignore (Slicing.Select.fold_selects_internal add_selection None set)

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
  in ignore (Slicing.Select.fold_selects_internal add_selection None set)

(** Registered as a slicing request function:
    Add selections that will be applied to all the slices of the function
    (already existing or created later)
    Note:
    - the function begins by applying the remaining internal requests.
    - the requests added for the last kernel function are not applied. *)
let add_persistent_cmdline project =
  SlicingParameters.feedback ~level:1
    "interpreting slicing requests from the command line...";
  begin try
    let selection = ref Db.Slicing.Select.empty_selects in
    let top_mark = !Db.Slicing.Mark.make ~addr:true ~ctrl:true ~data:true in
      Globals.Functions.iter
        (fun kf ->
           let add_selection opt select  =
             if Datatype.String.Set.mem (Kernel_function.get_name kf) (opt ())
             then selection := select !selection ~spare:false kf
           in
             add_selection
               SlicingParameters.Select.Return.get
               !Db.Slicing.Select.select_func_return;
             add_selection
               SlicingParameters.Select.Calls.get
               !Db.Slicing.Select.select_func_calls_to;
             add_selection
               SlicingParameters.Select.Pragma.get
               (fun s -> !Db.Slicing.Select.select_func_annots s top_mark
                  ~ai:false ~user_assert:false ~slicing_pragma:true
                  ~loop_inv:false ~loop_var:false);
             add_selection
               SlicingParameters.Select.Threat.get
               (fun s -> !Db.Slicing.Select.select_func_annots s top_mark
                  ~ai:true ~user_assert:false ~slicing_pragma:false
                  ~loop_inv:false ~loop_var:false);
             add_selection
               SlicingParameters.Select.Assert.get
               (fun s -> !Db.Slicing.Select.select_func_annots s top_mark
                  ~ai:false ~user_assert:true ~slicing_pragma:false
                  ~loop_inv:false ~loop_var:false);
             add_selection
               SlicingParameters.Select.LoopInv.get
               (fun s -> !Db.Slicing.Select.select_func_annots s top_mark
                  ~ai:false ~user_assert:false ~slicing_pragma:false
                  ~loop_inv:true ~loop_var:false);
             add_selection
               SlicingParameters.Select.LoopVar.get
               (fun s -> !Db.Slicing.Select.select_func_annots s top_mark
                  ~ai:false ~user_assert:false ~slicing_pragma:false
                  ~loop_inv:false ~loop_var:true);
        );
      if not (Datatype.String.Set.is_empty
                (SlicingParameters.Select.Value.get ()))
        ||
        not (Datatype.String.Set.is_empty
               (SlicingParameters.Select.RdAccess.get ()))
        ||
        not (Datatype.String.Set.is_empty
               (SlicingParameters.Select.WrAccess.get ()))
      then begin
        (* fprintf fmt "@\n[-slice-value] Select %s at end of the entry point %a@."
           lval_str Db.pretty_name kf; *)
        let kf = fst (Globals.entry_point ()) in
        let ki_scope_eval = Kernel_function.find_first_stmt kf in
          selection := !Db.Slicing.Select.select_func_lval !selection top_mark
            (SlicingParameters.Select.Value.get ()) kf;
          selection := !Db.Slicing.Select.select_func_lval_rw !selection top_mark
            ~rd:(SlicingParameters.Select.RdAccess.get ())
            ~wr:(SlicingParameters.Select.WrAccess.get ())
            ~scope:ki_scope_eval ~eval:ki_scope_eval kf ;
          SlicingParameters.Select.Value.clear () ;
          SlicingParameters.Select.RdAccess.clear () ;
          SlicingParameters.Select.WrAccess.clear () ;
      end;
      !Db.Slicing.Request.add_persistent_selection project !selection
  with Logic_interp.Error(_loc,msg) ->
    SlicingParameters.error "%s. Slicing requests from the command line are ignored" msg
  end;
  SlicingParameters.feedback ~level:2
    "done (interpreting slicing requests from the command line)."

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
compile-command: "make -C ../.."
End:
*)

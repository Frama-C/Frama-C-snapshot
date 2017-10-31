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

(** Those functions were previously outside the slicing module to show how to
 * use the slicing API. So, they are supposed to use the slicing module through
 * Db.Slicing only. There are mainly high level functions which make easier
 * to achieve simple tasks. *)

open Cil_types

type set = SlicingTypes.Fct_user_crit.t Cil_datatype.Varinfo.Map.t

let apply_all_actions () =
  SlicingParameters.debug ~level:1 "[Api.apply_all_internal]";
  SlicingParameters.feedback ~level:1 "applying all slicing requests...";
  SlicingParameters.debug ~level:2 "pending requests:@\n %t@\n"
    SlicingProject.print_proj_worklist;
  let r = SlicingProject.apply_all_actions () in
    SlicingParameters.feedback ~level:2 "done (applying all slicing requests).";
    r

let apply_next_action () =
  SlicingParameters.debug ~level:1 "[Api.apply_next_internal]";
  SlicingProject.apply_next_action ()

let apply_all ~propagate_to_callers =
  SlicingParameters.debug ~level:1 "[Api.apply_all]";
  assert (not propagate_to_callers) ;
  try
    while (true)
      do
        (* Format.printf "@\napply_next_internal@."; *)
        apply_next_action ()
      done
  with Not_found -> ()

let get_select_kf (fvar, _select) = Globals.Functions.get fvar

(** Utilities for [kinstr]. *)
module Kinstr: sig
  val iter_from_func : (stmt -> unit) -> kernel_function -> unit
  val is_rw_zone : (Locations.Zone.t option * Locations.Zone.t option) -> stmt -> Locations.Zone.t option * Locations.Zone.t option
  end
  =
struct
  (** Iter on statements of a kernel function *)
  let iter_from_func f kf =
    let definition = Kernel_function.get_definition kf in
    List.iter f definition.sallstmts

  (** Get directly read/written [Zone.t] by the statement.
    * i.e. directly means when [ki] is a call,
      it doesn't don't look at the assigns clause of the called function. *)
  let get_rw_zone stmt = (* returns [Zone.t read],[Zone.t written] *)
    assert (Db.Value.is_computed ());
    let lval_process read_zone stmt lv =
      (* returns [read_zone] joined to [Zone.t read] by [lv], [Zone.t written] by [lv] *)
      (* The modified locations are [looking_for], those address are
         function of [deps]. *)
      let state = Db.Value.get_stmt_state stmt in
      let deps, zloc, _exact =
        !Db.Value.lval_to_zone_with_deps_state
          state ~deps:(Some read_zone) ~for_writing:true lv
      in
      deps, zloc
    in
    let call_process lv f args _loc =
      (* returns  [Zone.t read] by [lv, f, args], [Zone.t written] by [lv] *)
      let read_zone = !Db.From.find_deps_no_transitivity stmt f in
      let add_args arg inputs =
        Locations.Zone.join inputs
          (!Db.From.find_deps_no_transitivity stmt arg) in
      let read_zone = List.fold_right add_args args read_zone in
      let read_zone,write_zone =
        match lv with
        | None -> read_zone , Locations.Zone.bottom
        | Some lv -> lval_process read_zone stmt lv
      in read_zone,write_zone
    in
    match stmt.skind with
      | Switch (exp,_,_,_)
      | If (exp,_,_,_) ->
          (* returns  [Zone.t read] by condition [exp], [Zone.bottom] *)
          !Db.From.find_deps_no_transitivity stmt exp, Locations.Zone.bottom
      | Instr (Set (lv,exp,_)) ->
          (* returns  [Zone.t read] by [exp, lv], [Zone.t written] by [lv] *)
          let read_zone = !Db.From.find_deps_no_transitivity stmt exp in
            lval_process read_zone stmt lv
      | Instr (Local_init (v, AssignInit i, _)) ->
        let rec collect zone i =
          match i with
          | SingleInit e ->
            Locations.Zone.join zone (!Db.From.find_deps_no_transitivity stmt e)
          | CompoundInit (_,l) ->
            List.fold_left
              (fun acc (_,i) -> collect acc i) zone l
        in
        let read_zone = collect Locations.Zone.bottom i in
        lval_process read_zone stmt (Cil.var v)
      | Instr (Call (lvaloption,funcexp,argl,l)) ->
        call_process lvaloption funcexp argl l
      | Instr (Local_init(v, ConsInit(f, args, k),l)) ->
        Cil.treat_constructor_as_func call_process v f args k l
      | _ -> Locations.Zone.bottom, Locations.Zone.bottom

  (** Look at intersection of [rd_zone_opt]/[wr_zone_opt] with the
      directly read/written [Zone.t] by the statement.
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

(** Topologically propagate user marks to callers in whole project *)
let topologic_propagation project =
  apply_all_actions project;
  Callgraph.Uses.iter_in_rev_order
    (fun kf ->
       SlicingParameters.debug ~level:3
         "doing topologic propagation for function: %a"
         Kernel_function.pretty kf;
       apply_all_actions project)

let add_to_selection set selection =
  SlicingSelect.Selections.add_to_selects selection set

(** Registered as a slicing selection function:
    Add a selection of the pdg nodes. *)
let select_pdg_nodes set mark nodes kf =
  let selection = SlicingSelect.select_pdg_nodes kf nodes mark
  in add_to_selection set selection

(** Registered as a slicing selection function:
    Add a selection of the statement. *)
let select_stmt set ~spare stmt kf =
  let stmt_mark = SlicingMarks.mk_user_mark
                    ~data:(not spare) ~addr:(not spare) ~ctrl:(not spare) in
  let selection = SlicingSelect.select_stmt_computation kf stmt stmt_mark
  in add_to_selection set selection

(** Add a selection to the entrance of the function [kf]
    and add a selection to its return if [~return] is true
    and add a selection to [~inputs] parts of its inputs
    and add a selection to [~outputs] parts of its outputs*)
let select_entry_point_and_some_inputs_outputs set ~mark kf ~return ~outputs ~inputs =
  SlicingParameters.debug ~level:3
    "select_entry_point_and_some_inputs_outputs %a"
    Kernel_function.pretty kf ;
  let set = let selection = SlicingSelect.select_entry_point kf mark in
    add_to_selection set selection
  in
  let set = 
    if (Locations.Zone.equal Locations.Zone.bottom inputs)
    then set
    else let selection = SlicingSelect.select_zone_at_entry kf inputs mark in
      add_to_selection set selection
  in if ((Locations.Zone.equal Locations.Zone.bottom outputs) && not return) ||
      (try
	let ki = Kernel_function.find_return kf
	in if Db.Value.is_reachable_stmt ki then
	    false
	  else
	    begin
	      SlicingParameters.feedback
		"@[Nothing to select for unreachable return stmt of %a@]"
		Kernel_function.pretty kf;
	      true
	    end
       with Kernel_function.No_Statement -> false)
    then set
    else
      let set =
	if (Locations.Zone.equal Locations.Zone.bottom outputs)
	then set
	else let selection = SlicingSelect.select_modified_output_zone kf outputs mark in
	  add_to_selection set selection
      in if return
	then let selection = SlicingSelect.select_return kf mark in
	  add_to_selection set selection
	else set

(* apply [select ~spare] on each callsite of [kf] and add the returned selection
   to [set]. *)
let generic_select_func_calls select_stmt set ~spare kf =
  assert (Db.Value.is_computed ());
  let callers = !Db.Value.callers kf in
  let select_calls acc (caller, stmts) =
    List.fold_left (fun acc s -> select_stmt acc ~spare s caller) acc stmts
  in
  List.fold_left select_calls set callers

(** Registered as a slicing selection function:
    Add a selection of calls to a [kf]. *)
let select_func_calls_into set ~spare kf =
  let add_to_select set ~spare select =
    let mark =
      let nspare = not spare in
      SlicingMarks.mk_user_mark ~data:nspare ~addr:nspare ~ctrl:nspare
    in add_to_selection set (select mark)
  in
  let kf_entry, _library = Globals.entry_point () in
  if Kernel_function.equal kf_entry kf then
    add_to_select set ~spare (SlicingSelect.select_entry_point kf)
  else
    let select_min_call set ~spare ki kf =
      add_to_select set ~spare (SlicingSelect.select_minimal_call kf ki) 
    in
    generic_select_func_calls select_min_call set ~spare kf

(** Registered as a slicing selection function:
    Add a selection of calls to a [kf]. *)
let select_func_calls_to set ~spare kf = 
  let kf_entry, _library = Globals.entry_point () in
  if Kernel_function.equal kf_entry kf then
    begin
      let mark =
	let nspare = not spare in
	SlicingMarks.mk_user_mark ~data:nspare ~addr:nspare ~ctrl:nspare
      in
      assert (Db.Value.is_computed ());
      let outputs = !Db.Outputs.get_external kf in
      select_entry_point_and_some_inputs_outputs set ~mark kf
	~return:true 
	~outputs
	~inputs:Locations.Zone.bottom
    end
  else
    generic_select_func_calls select_stmt set ~spare kf

(** Registered as a slicing selection function:
    Add selection of function outputs. *)
let select_func_zone set mark zone kf =
  let selection = SlicingSelect.select_zone_at_end kf zone mark
  in add_to_selection set selection

(** Registered as a slicing selection function:
    Add a selection of the [kf] return statement. *)
let select_func_return set ~spare kf =
  try
    let ki = Kernel_function.find_return kf
    in select_stmt set ~spare ki kf
  with Kernel_function.No_Statement ->
    let mark =
      SlicingMarks.mk_user_mark
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
    SlicingMarks.mk_user_mark ~data:false ~addr:false ~ctrl:(not spare) in
  let selection = SlicingSelect.select_stmt_computation kf ki ctrl_mark
  in add_to_selection set selection

(** Registered as a slicing selection function:
    Add a selection of data relative to a statement.
    Note: add also a transparent selection on the whole statement. *)
let select_stmt_zone set mark zone ~before ki kf =
  let selection =
    SlicingSelect.select_stmt_zone kf ki ~before zone mark
  in let set = add_to_selection set selection
  in select_stmt_ctrl set ~spare:true ki kf

(** Registered as a slicing selection function:
    Add a selection of data relative to a statement.
    Variables of [lval_str] string are bounded
    relatively to the whole scope of the function [kf].
    The interpretation of the address of the lvalues is
    done just before the execution of the statement [~eval].
    The selection preserve the value of these lvalues before
    or after (c.f. boolean [~before]) the statement [ki].
    Note: add also a transparent selection on the whole statement. *)
let select_stmt_lval set mark lval_str ~before ki ~eval kf =
  assert (Db.Value.is_computed ());
  if Datatype.String.Set.is_empty lval_str
  then set
  else
    let zone =
      Datatype.String.Set.fold
        (fun lval_str acc ->
           let lval_term = !Db.Properties.Interp.term_lval kf lval_str in
           let lval =
             !Db.Properties.Interp.term_lval_to_lval ~result:None lval_term
           in
           let state = Db.Value.get_stmt_state eval in
           let _deps, zone, _exact =
             !Db.Value.lval_to_zone_with_deps_state
               ~deps:None ~for_writing:false state lval
           in
           Locations.Zone.join zone acc)
        lval_str
        Locations.Zone.bottom
    in
    select_stmt_zone set mark zone ~before ki kf

(** Add a selection of data relative to read/write accesses.
    Interpret the [~rd] lvalues and the [~wr] lvalues from [~eval]
    statements of [kf]:
    - Variables of [lval_str] string are bounded
      relatively to the whole scope of the function [kf].
    - The interpretation of the address of the lvalues is
      done just before the execution of the statement [~eval].
    Find read/write accesses from the whole project if [ki_opt]=None.
    Otherwise, restrict the research among the direct effect of [ki_opt] statement.
    i.e. when [ki_opt] is a call, the selection doesn't look at the assigns clause
    of a call. *)
let select_lval_rw set mark ~rd ~wr ~eval kf ki_opt=
  assert (Db.Value.is_computed ());
   let zone_option ~for_writing lval_str =
    if Datatype.String.Set.is_empty lval_str
    then None
    else
      let zone =
        Datatype.String.Set.fold
          (fun lval_str acc ->
             let lval_term = !Db.Properties.Interp.term_lval kf lval_str in
             let lval = !Db.Properties.Interp.term_lval_to_lval ~result:None lval_term in
             let state = Db.Value.get_stmt_state eval in
             let _deps, zone, _exact =
               !Db.Value.lval_to_zone_with_deps_state
                 state ~for_writing ~deps:None lval
             in
             Locations.Zone.join zone acc)
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
    relatively to the whole scope of the function [kf].
    The interpretation of the address of the lvalues is
    done just before the execution of the statement [~eval].
    The selection preserve the [~rd] and ~[wr] accesses
    directly contained into the statement [ki].
    i.e. when [ki] is a call, the selection doesn't look at
    the assigns clause of the called function.
    Note: add also a transparent selection on the whole statement.*)
let select_stmt_lval_rw set mark ~rd ~wr ki ~eval kf =
  select_lval_rw set mark ~rd ~wr ~eval kf (Some ki)

(** Add a selection of the declaration of [vi]. *)
let select_decl_var set mark vi kf =
  let selection = SlicingSelect.select_decl_var kf vi mark in
    add_to_selection set selection

let select_ZoneAnnot_pragmas set ~spare pragmas kf =
  let set =
    Cil_datatype.Stmt.Set.fold
      (* selection related to statement assign and //@ slice pragma stmt *)
      (fun ki' acc -> select_stmt acc ~spare ki' kf)
      pragmas.Db.Properties.Interp.To_zone.stmt set
  in
  Cil_datatype.Stmt.Set.fold
    (* selection related to //@ slice pragma ctrl/expr *)
    (fun ki' acc -> select_stmt_ctrl acc ~spare ki' kf)
    pragmas.Db.Properties.Interp.To_zone.ctrl
    set

let select_ZoneAnnot_zones_decl_vars set mark (zones,decl_vars) kf =
  let set =
    Cil_datatype.Varinfo.Set.fold
      (fun vi acc -> select_decl_var acc mark vi kf)
      decl_vars.Db.Properties.Interp.To_zone.var
      set
  in
  let set =
    Cil_datatype.Logic_label.Set.fold
      (fun l acc ->   
	 let selection = SlicingSelect.select_label kf l mark
	 in add_to_selection acc selection)
      decl_vars.Db.Properties.Interp.To_zone.lbl
      set
  in
   List.fold_right
    (fun z acc ->
      (* selection related to the parsing/compilation of the annotation *)
      select_stmt_zone acc mark
        z.Db.Properties.Interp.To_zone.zone
        ~before:z.Db.Properties.Interp.To_zone.before
        z.Db.Properties.Interp.To_zone.ki
        kf)
    zones set

let get_or_raise (info_data_opt, info_decl) = match info_data_opt with
  | None ->
      (* TODO: maybe we can know how to use [info_decl] ? *)
      SlicingParameters.not_yet_implemented
	"%s" !Logic_interp.To_zone.not_yet_implemented
  | Some info_data -> info_data, info_decl

(** Registered as a slicing selection function:
    Add selection of the annotations related to a statement.
    Note: add also a transparent selection on the whole statement. *)
let select_stmt_pred set mark pred ki kf =
  let zones_decl_vars =
    !Db.Properties.Interp.To_zone.from_pred pred
      (!Db.Properties.Interp.To_zone.mk_ctx_stmt_annot kf ki)
  in
  select_ZoneAnnot_zones_decl_vars set mark (get_or_raise zones_decl_vars) kf

(** Registered as a slicing selection function:
    Add selection of the annotations related to a statement.
    Note: add also a transparent selection on the whole statement. *)
let select_stmt_term set mark term ki kf =
  let zones_decl_vars = 
    !Db.Properties.Interp.To_zone.from_term term
      (!Db.Properties.Interp.To_zone.mk_ctx_stmt_annot kf ki)
  in 
  select_ZoneAnnot_zones_decl_vars set mark (get_or_raise zones_decl_vars) kf

(** Registered as a slicing selection function:
    Add selection of the annotations related to a statement.
    Note: add also a transparent selection on the whole statement. *)
let select_stmt_annot set mark ~spare annot ki kf =
  let zones_decl_vars,pragmas =
    !Db.Properties.Interp.To_zone.from_stmt_annot annot (ki, kf)
  in let set = select_ZoneAnnot_pragmas set ~spare pragmas kf
  in select_ZoneAnnot_zones_decl_vars set mark (get_or_raise zones_decl_vars) kf

(** Registered as a slicing selection function:
    Add selection of the annotations related to a statement.
    Note: add also a transparent selection on the whole statement. *)
let select_stmt_annots set mark ~spare  ~threat ~user_assert ~slicing_pragma ~loop_inv ~loop_var ki kf =
  let zones_decl_vars,pragmas =
    !Db.Properties.Interp.To_zone.from_stmt_annots
      (Some (!Db.Properties.Interp.To_zone.code_annot_filter
               ~threat ~user_assert ~slicing_pragma
               ~loop_inv ~loop_var ~others:false))
      (ki, kf)
  in let set = select_ZoneAnnot_pragmas set ~spare pragmas kf
  in select_ZoneAnnot_zones_decl_vars set mark (get_or_raise zones_decl_vars) kf

(** Registered as a slicing selection function:
    Add a selection of the annotations related to a function. *)
let select_func_annots set mark ~spare ~threat ~user_assert ~slicing_pragma ~loop_inv ~loop_var kf =
  let zones_decl_vars,pragmas =
    !Db.Properties.Interp.To_zone.from_func_annots Kinstr.iter_from_func
         (Some
            (!Db.Properties.Interp.To_zone.code_annot_filter
               ~threat ~user_assert ~slicing_pragma ~loop_inv
               ~loop_var ~others:false))
      kf
  in let set = select_ZoneAnnot_pragmas set ~spare pragmas kf
  in select_ZoneAnnot_zones_decl_vars set mark (get_or_raise zones_decl_vars) kf

(** Registered as a slicing selection function:
    Add selection of function outputs.
    Variables of [lval_str] string are bounded
    relatively to the whole scope of the function [kf].
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
      ~eval:ki_scope_eval
      kf

(** Registered as a slicing selection function:
    Add a selection of data relative to read/write accesses.
    Interpret the [~rd] lvalues and the [~wr] lvalues from [~eval] 
    statements of [kf]:
    - Variables of [lval_str] string are bounded
      relatively to the whole scope of the function [kf].
    - The interpretation of the address of the lvalues is
      done just before the execution of the statement [~eval].
    Find read/write accesses from the whole project if [ki_opt]=None. *)
let select_func_lval_rw set mark ~rd ~wr ~eval kf =
  if Datatype.String.Set.is_empty rd && Datatype.String.Set.is_empty wr
  then set
  else select_lval_rw set mark ~rd ~wr ~eval kf None

(** Registered as a slicing request function:
    Add selections to all concerned slices, as slicing requests and apply them,
    kernel function by kernel function.
    Note:
    - the function begins by applying the remaining internal requests.
    - the requests added for the last kernel function are not applied. *)
let add_selection set =
  let add_selection prev selection =
    let kf = get_select_kf selection in
    let r = match prev with
        None -> apply_all_actions () ; Some (kf)
      | Some prev_kf -> if prev_kf == kf then prev else None
    and make_request slice =
      SlicingSelect.add_ff_selection slice selection
    and slices =
      let slices = SlicingProject.get_slices kf
      in if slices = [] then [SlicingProject.create_slice kf] else slices
    in List.iter make_request slices ;
      r
  in ignore (SlicingSelect.Selections.fold_selects_internal add_selection None set)

(** Registered as a slicing request function:
    Add selections that will be applied to all the slices of the function
    (already existing or created later)
    Note:
    - the function begins by applying the remaining internal requests.
    - the requests added for the last kernel function are not applied. *)
let add_persistent_selection set =
  (* Format.printf "@\nadd_persistent_selection@."; *)
  let add_selection prev selection =
    let kf = get_select_kf selection in
    let r = match prev with
        None -> apply_all_actions () ; Some (kf)
      | Some prev_kf -> if prev_kf == kf then prev else None
    in SlicingSelect.add_fi_selection selection; r
  in ignore (SlicingSelect.Selections.fold_selects_internal add_selection None set)

(** Registered as a slicing request function:
    Add selections that will be applied to all the slices of the function
    (already existing or created later)
    Note:
    - the function begins by applying the remaining internal requests.
    - the requests added for the last kernel function are not applied. *)
let add_persistent_cmdline () =
  SlicingParameters.feedback ~level:1
    "interpreting slicing requests from the command line...";
  begin try
    let selection = ref Cil_datatype.Varinfo.Map.empty in
    let top_mark = SlicingMarks.mk_user_mark ~addr:true ~ctrl:true ~data:true in
      Globals.Functions.iter
        (fun kf ->
           let add_selection opt select  =
             if Kernel_function.Set.mem kf (opt ()) then
               selection := select !selection ~spare:false kf
           in
             add_selection
               SlicingParameters.Select.Return.get
               select_func_return;
             add_selection
               SlicingParameters.Select.Calls.get
               select_func_calls_to;
             add_selection
               SlicingParameters.Select.Pragma.get
               (fun s -> select_func_annots s top_mark
                  ~threat:false ~user_assert:false ~slicing_pragma:true
                  ~loop_inv:false ~loop_var:false);
             add_selection
               SlicingParameters.Select.Threat.get
               (fun s -> select_func_annots s top_mark
                  ~threat:true ~user_assert:false ~slicing_pragma:false
                  ~loop_inv:false ~loop_var:false);
             add_selection
               SlicingParameters.Select.Assert.get
               (fun s -> select_func_annots s top_mark
                  ~threat:false ~user_assert:true ~slicing_pragma:false
                  ~loop_inv:false ~loop_var:false);
             add_selection
               SlicingParameters.Select.LoopInv.get
               (fun s -> select_func_annots s top_mark
                  ~threat:false ~user_assert:false ~slicing_pragma:false
                  ~loop_inv:true ~loop_var:false);
             add_selection
               SlicingParameters.Select.LoopVar.get
               (fun s -> select_func_annots s top_mark
                  ~threat:false ~user_assert:false ~slicing_pragma:false
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
          selection := select_func_lval !selection top_mark
            (SlicingParameters.Select.Value.get ()) kf;
          selection := select_func_lval_rw !selection top_mark
            ~rd:(SlicingParameters.Select.RdAccess.get ())
            ~wr:(SlicingParameters.Select.WrAccess.get ())
            ~eval:ki_scope_eval kf ;
          SlicingParameters.Select.Value.clear () ;
          SlicingParameters.Select.RdAccess.clear () ;
          SlicingParameters.Select.WrAccess.clear () ;
      end;
      add_persistent_selection !selection;
  with Logic_interp.Error(_loc,msg) ->
    SlicingParameters.error "%s. Slicing requests from the command line are ignored." msg
  end;
  SlicingParameters.feedback ~level:2
    "done (interpreting slicing requests from the command line)."

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

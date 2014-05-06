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

let debug n format = Sparecode_params.debug ~level:n format
let fatal fmt = Sparecode_params.fatal fmt

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(** The project is composed of [FctIndex] marked with [BoolMark]
* to be used by [Pdg.Register.F_Proj], and another table to store if a function
* is visible (usefull for Top PDG). *)
(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

module BoolMark = struct
  type prop_mode = Glob | Loc
  type t = bool * prop_mode
  type call_info = unit

  let bottom = false,Loc
  let top = true,Glob

  let visible (b,_) = b

  let mk glob = if glob then top else (true, Loc)

  let merge (b1,p1) (b2,p2) =
    let b = b1 || b2 in
    let p = match p1, p2 with
      | Glob, _ | _, Glob -> Glob
      | Loc, Loc -> Loc
    in (b, p)

  let equal (b1,p1:t) (b2,p2) = (b1 = b2) && (p1 = p2)

  let combine old_m new_m =
    let new_m = merge old_m new_m in
    let m_to_prop = if equal old_m new_m then bottom else new_m in
      (new_m, m_to_prop)

  let is_bottom b = (b = bottom)

  let pretty fmt (b,p) =
    Format.fprintf fmt "%s(%s)"
         (if b then "true" else "false")
         (match p with Glob -> "Glob" | Loc -> "Loc")
end

module KfTopVisi = struct
  include Cil_datatype.Kf.Hashtbl

  let add proj kf b = add (snd proj) kf b

  let find proj kf = find (snd proj) kf

  (** as soon as a TOP function is called, all its callees are called. *)
  let rec set proj kf = 
    try find proj kf
    with Not_found ->
      add proj kf ();
      debug 1 "select '%a' as fully visible (top or called by top)"
        Kernel_function.pretty kf;
      let callees = !Db.Users.get kf in
        Kernel_function.Hptset.iter (set proj) callees

  let get proj kf = try find proj kf; true with Not_found -> false
end

(** when we first compute marks to select outputs,
* we don't immediately propagate input marks to the calls,
* because some calls may be useless and we don't want to compute
* their inputs. We will check calls later on.
* But when we select annotations, we want to preserve all the calls that can
* lead to them : so, we propagate...
* *)
let call_in_to_check = ref []
let called_top = ref []

module Config = struct
  module M = BoolMark

    let mark_to_prop_to_caller_input call_opt pdg_caller sel_elem m =
      match m with
        | true, M.Glob -> Some m
        | true, M.Loc ->
            call_in_to_check :=
            (pdg_caller, call_opt, sel_elem, m) :: !call_in_to_check;
            None
        | _ -> fatal "cannot propagate invisible mark@."

    let mark_to_prop_to_called_output _call called_pdg =
      if PdgTypes.Pdg.is_top called_pdg then
        begin
          let kf = PdgTypes.Pdg.get_kf called_pdg in
          called_top := kf :: !called_top; 
          debug 1 "memo call to TOP '%a'" Kernel_function.pretty kf;
          (fun _ _ -> None)
        end
      else 
        fun _n m -> match m with
        | true, M.Glob -> Some (true, M.Loc)
        | true, M.Loc -> Some m
        | _ -> fatal "cannot propagate invisible mark@."

end

module ProjBoolMarks = Pdg.Register.F_Proj (Config)

type proj = ProjBoolMarks.t * unit KfTopVisi.t
type fct = ProjBoolMarks.fct

let new_project () = (ProjBoolMarks.empty (), KfTopVisi.create 10)

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(** Get stored information *)
(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

let proj_marks proj = fst proj

(** @raise Not_found when the function is not marked. It might be the case
* that it is nonetheless visible, but has no marks because of a Top PDG. *)
let get_marks proj kf =
  try KfTopVisi.find proj kf ; None
  with Not_found ->
    ProjBoolMarks.find_marks (proj_marks proj) (Kernel_function.get_vi kf)

(** Useful only if there has been some Pdg.Top *)
let kf_visible proj kf = 
  try KfTopVisi.find proj kf ; true
  with Not_found -> get_marks proj kf <> None

let rec key_visible fm key =
  try
    match key with
      | PdgIndex.Key.CallStmt call_id ->
          let call = PdgIndex.Key.call_from_id call_id in
            call_visible fm call
      | _ -> let m = PdgIndex.FctIndex.find_info fm key in
          BoolMark.visible m
  with Not_found -> false
and
  (** the call is visible if its control node is visible *)
  call_visible fm call =
  let key = PdgIndex.Key.call_ctrl_key call in
    key_visible fm key

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(** Build selections and propagate. *)
(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

(** Doesn't mark yet, but add what has to be marked in the selection,
* and keep things sorted. *)
let rec add_pdg_selection to_select pdg sel_mark = match to_select with
  | [] -> 
      let l = match sel_mark with None -> [] | Some m -> [m] in [(pdg, l)]
  | (p, ln) :: tl ->
      if Db.Pdg.from_same_fun p pdg 
      then 
        let ln = match sel_mark with None -> ln 
          | Some sel_mark -> sel_mark::ln 
        in (p, ln)::tl
      else (p, ln)::(add_pdg_selection tl pdg sel_mark)

let add_node_to_select glob to_select z_opt node =
  PdgMarks.add_node_to_select to_select (node, z_opt) (BoolMark.mk glob)

let add_nodes_and_undef_to_select
      glob (ctrl_nodes, decl_nodes, data_info) to_select =
  match data_info with
    | None -> to_select (* don't select anything (computation failed) *)
    | Some (data_nodes, undef) ->
        let to_select =
          List.fold_left (fun s n -> add_node_to_select glob s None n)
            to_select ctrl_nodes
        in
        let to_select =
          List.fold_left (fun s n -> add_node_to_select glob s None n)
            to_select decl_nodes
        in
        let to_select =
          List.fold_left (fun s (n,z_opt) -> add_node_to_select glob s z_opt n)
            to_select data_nodes
        in
        let m = (BoolMark.mk glob) in
        let to_select = PdgMarks.add_undef_in_to_select to_select undef m in
          to_select

(** Mark the function as visible 
* and add the marks according to the selection. 
  Notice that if the function has been marked as called by a visible top,
  we can skip the selection since the function has to be fully visible anyway.
**)
let select_pdg_elements proj pdg to_select =
  let kf = PdgTypes.Pdg.get_kf pdg in
    try KfTopVisi.find proj kf;
        debug 1 "function '%a' selected for top: skip selection"
          Kernel_function.pretty kf
    with Not_found ->
      debug 1 "add selection in function '%a'@." Kernel_function.pretty kf;
      ProjBoolMarks.mark_and_propagate (proj_marks proj) pdg to_select;
      List.iter (KfTopVisi.set proj) !called_top;
      called_top := []

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(** First step is finished: propagate in the calls. *)
(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

(** [proj] contains some function marks and [!call_in_to_check]
* is a list of call input marks to propagate when the call is visible.
* These marks come from the called function selection,
* but they are not automatically propagated because when a function is visible
* it doesn't mean that all the calls to that function are visible.
*
* So we first split the todo list ([!call_in_to_check]) into the nodes to mark
* which correspond to inputs of visible calls
* and the others that do not yet correspond to visible call
* but we keep them because it can happen later *)
let rec process_call_inputs proj =
  let rec process (to_select, unused) todo = match todo with
    | [] -> (to_select, unused)
    | (pdg_caller, call, sel, m) as e :: calls ->
        let kf_caller = PdgTypes.Pdg.get_kf pdg_caller in
        let visible, select = match call with
          | Some call ->
              let fm = match get_marks proj kf_caller with
                | None -> fatal "the caller should have marks@."
                | Some fm -> fm
              in
              let visible = call_visible fm call in
                visible, Some (sel, m)
          | None -> (* let see if the function is visible or not *)
              assert (PdgTypes.Pdg.is_top pdg_caller);
              KfTopVisi.get proj kf_caller, None
        in
        let res = if visible then
            let to_select = add_pdg_selection to_select pdg_caller select
            in (to_select, unused)
          else (to_select, e::unused)
        in process res calls
  in
  let to_select, new_list = process ([], []) !call_in_to_check in
    match to_select with
      | [] -> call_in_to_check := []
             (* nothing more to mark : finished ! we can forget [new_list] *)
      | _ ->
          call_in_to_check := new_list;
          List.iter (fun (pdg, sel) -> select_pdg_elements proj pdg sel)
                    to_select;
          process_call_inputs proj

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(** Main selection: select starting points and propagate. *)
(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

let select_entry_point proj _kf pdg =
  let ctrl = !Db.Pdg.find_entry_point_node pdg in
  let to_select = add_node_to_select true [] None ctrl in
    select_pdg_elements proj pdg to_select

let select_all_outputs proj kf pdg =
  let outputs = !Db.Outputs.get_external kf in
    debug 1 "selecting output zones %a@." Locations.Zone.pretty outputs;
  try
    let nodes, undef = !Db.Pdg.find_location_nodes_at_end pdg outputs in
    let nodes =
      try ((!Db.Pdg.find_ret_output_node pdg),None) :: nodes
      with Not_found -> nodes
    in
    let nodes_and_co = ([], [], Some (nodes, undef)) in
    let to_select = add_nodes_and_undef_to_select false nodes_and_co [] in
      select_pdg_elements proj pdg to_select
  with Not_found -> (* end is unreachable *) ()

(** used to visit all the annotations of a given function
 * and to find the PDG nodes to select so that the reachable annotations
 * can be visible *)
class annot_visitor ~filter pdg = object (self)

  inherit Visitor.frama_c_inplace

  val mutable to_select = []
  method get_select = to_select

  method! vcode_annot annot =
    let () =
      if filter annot then
      try
        let stmt = Extlib.the self#current_stmt in
        debug 1 "selecting annotation : %a @."
          Printer.pp_code_annotation annot;
        let info = !Db.Pdg.find_code_annot_nodes pdg stmt annot in
          to_select <- add_nodes_and_undef_to_select true info to_select
      with 
	Not_found -> () (* unreachable *)
      | Logic_interp.To_zone.NYI _ ->
	  Sparecode_params.warning ~current:true ~once:true
	    "Dropping annotation";
	  ()
    in Cil.SkipChildren
end

let select_annotations ~select_annot ~select_slice_pragma proj =
  let visit_fun kf =
    debug 1 "look for annotations in function %a@." Kernel_function.pretty kf;
    let pdg = !Db.Pdg.get kf in
      if PdgTypes.Pdg.is_top pdg then debug 1 "pdg top: skip annotations"
      else if PdgTypes.Pdg.is_bottom pdg 
      then debug 1 "pdg bottom: skip annotations"
      else begin
        let filter annot = match annot.Cil_types.annot_content with
          | Cil_types.APragma (Cil_types.Slice_pragma _) -> select_slice_pragma
          | Cil_types.AAssert _-> (* Never select alarms, they are not useful *)
              (match Alarms.find annot with
                 | None -> select_annot
                 | Some _ -> false)
          | _ -> select_annot 
        in
          try
            let f = Kernel_function.get_definition kf in
            let visit = new annot_visitor ~filter pdg in
            let fc_visit = (visit:>Visitor.frama_c_visitor) in
            let _ = Visitor.visitFramacFunction fc_visit f in
            let to_select = visit#get_select in
              if to_select <> [] then select_pdg_elements proj pdg to_select
          with Kernel_function.No_Definition -> () (* nothing to do *)
      end
  in
    Globals.Functions.iter visit_fun

let finalize proj =
  debug 1 "finalize call input propagation@.";
  process_call_inputs proj;
  assert (!call_in_to_check = [])

let select_useful_things ~select_annot ~select_slice_pragma kf_entry =
  let proj = new_project () in
  assert (!call_in_to_check = []);
    debug 1 "selecting function %a outputs and entry point@."
      Kernel_function.pretty kf_entry;
  let pdg = !Db.Pdg.get kf_entry in
    if PdgTypes.Pdg.is_top pdg 
    then KfTopVisi.set proj kf_entry 
    else if PdgTypes.Pdg.is_bottom pdg
    then debug 1 "unreachable entry point ?"
    else begin
      select_entry_point proj kf_entry pdg;
      select_all_outputs proj kf_entry pdg;
      if (select_annot || select_slice_pragma) then
        select_annotations ~select_annot ~select_slice_pragma proj;
      finalize proj
    end;
  proj

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

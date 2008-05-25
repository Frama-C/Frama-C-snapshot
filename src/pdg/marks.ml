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

open PdgTypes
open PdgIndex

(** compute the marks to propagate in the caller nodes from the marks of
 * a function inputs [in_marks]. 
 *)
let in_marks_to_caller pdg call m2m ?(rqs=[]) in_marks =
  let build pdg call rqs (in_key, m) =
    let new_rqs = match in_key with
      | Signature.InTop -> 
          (try [PdgMarks.SelNode (!Db.Pdg.find_call_topin_node pdg call)]
          with Not_found -> 
          (* Impossible : if there is a call to a function with a Top input,
          * the caller function should also have a Top input. *)
            assert false )
      | Signature.InCtrl -> 
          [PdgMarks.SelNode (!Db.Pdg.find_call_ctrl_node pdg call)]
      | Signature.InNum in_num -> 
          [PdgMarks.SelNode (!Db.Pdg.find_call_input_node pdg call in_num)]
      | Signature.InImpl zone -> 
          let nodes, undef =
            !Db.Pdg.find_location_nodes_at_stmt pdg call ~before:true zone
          in 
          let sel = List.map (fun n -> PdgMarks.SelNode n) nodes in
          let sel =
            if (Locations.Zone.equal Locations.Zone.bottom undef) then sel
            else (PdgMarks.SelIn undef)::sel
          in sel
    in let add acc s = match m2m s m with
        | None -> acc
        | Some m -> (s, m)::acc
    in 
    let rqs = List.fold_left add rqs new_rqs in
      rqs
  in
    List.fold_left (build pdg call) rqs in_marks

(** some new input marks has been added in a called function.
 * Build the list of what is to be propagated in the callers. *)
let translate_in_marks pdg_called in_new_marks 
       ?(m2m=fun _ _ _ m -> Some m) other_rqs =
    let kf_called = Globals.Functions.get (Pdg.get_var_fct pdg_called) in
    let translate pdg rqs call = 
      in_marks_to_caller pdg call (m2m call pdg) ~rqs in_new_marks
    in
    let build acc (caller, _) =
      let pgd_caller = !Db.Pdg.get caller in
      let call_stmts = !Db.Pdg.find_call_stmts ~caller kf_called in
      let caller_rqs = 
        List.fold_left (translate pgd_caller) [] call_stmts in
        (pgd_caller, caller_rqs)::acc
    in let rqs = 
      List.fold_left build other_rqs (!Db.Value.callers kf_called) in
      rqs (* TODO : more intelligent merge *)

let call_out_marks_to_called called_pdg m2m ?(rqs=[]) out_marks =
  let build rqs (out_key, m) =
    let nodes, undef = Sets.find_output_nodes called_pdg out_key in
    let sel = List.map (fun n -> PdgMarks.SelNode n) nodes in
    let sel =
      if (Locations.Zone.equal Locations.Zone.bottom undef) then sel
      else (PdgMarks.SelIn undef)::sel
    in
    let add acc s = match m2m s m with
        | None -> acc
        | Some m -> (s, m)::acc
    in 
    let rqs = List.fold_left add rqs sel in
      rqs
  in
    List.fold_left build rqs out_marks


let translate_out_mark m2m other_rqs (call, l) =
  let add_list l_out_m rqs called_kf =
    try
      let called_pdg = !Db.Pdg.get called_kf in
      let m2m = m2m call called_pdg in
      let node_marks = 
        call_out_marks_to_called called_pdg m2m ~rqs:[] l_out_m
      in (called_pdg, node_marks)::rqs
    with PdgTypes.Pdg.Top ->
      (* no PDG for this function : forget the new marks *)
      rqs
  in
  let all_called = Db.Value.call_to_kernel_function call in
    List.fold_left (add_list l) other_rqs all_called

  (** [add_new_marks_to_rqs pdg new_marks other_rqs] translates [new_marks]
  * that were computed during intraprocedural propagation into requests,
  * and add them to [other_rqs].
  *
  * The functions [in_m2m] and [out_m2m] can be used to modify the marks during
  * propagation :
  *- [in_m2m call_stmt call_in_node mark] : 
        provide the mark to propagate to the [call_in_node] 
        knowing that the mark of the called function has been modify to [mark]
  *- [out_m2m out_node mark] :
        provide the mark to propagate to the [out_node]
        knowing that a call output mark has been modify to [mark].
  *)
  let translate_marks_to_prop pdg new_marks 
       ?(in_m2m=fun _ _ _ m -> Some m) 
       ?(out_m2m=fun _ _ _ m -> Some m) 
        other_rqs =
    let in_marks, out_marks = new_marks in
    let other_rqs = translate_in_marks pdg in_marks ~m2m:in_m2m other_rqs in
      List.fold_left (translate_out_mark out_m2m) other_rqs out_marks


(** To also use interprocedural propagation, the user can instantiate this
* functor. This is, of course, not mandatory because one can want to use a more
* complex propagation (like slicing for instance, that has more than one
* version for a source function). *)
module F_Proj (C : PdgMarks.T_Config) : 
  PdgMarks.T_Proj with type t_mark = C.M.t 
	 and type t_fct = (C.M.t, C.M.t_call_info) PdgIndex.FctIndex.t
= struct

  module F = Db.Pdg.F_FctMarks (C.M)

  type t_mark = C.M.t
  type t_fct = (C.M.t, C.M.t_call_info) PdgIndex.FctIndex.t
  type t_fct_info = F.t
  type t = t_fct_info Cilutil.VarinfoHashtbl.t

  type request = (PdgTypes.Pdg.t * (PdgTypes.Node.t * F.t_mark) list)

  let empty = Cilutil.VarinfoHashtbl.create 10

  let pdg_fvar pdg = PdgTypes.Pdg.get_var_fct pdg

  let find_marks proj fct_var =
    try let f = Cilutil.VarinfoHashtbl.find proj fct_var in Some (F.get_idx f)
    with Not_found -> None

  let get proj pdg =
    let fct_var = pdg_fvar pdg in
    try Cilutil.VarinfoHashtbl.find proj fct_var
    with Not_found -> 
      let kf = Globals.Functions.get fct_var in
      let pdg = !Db.Pdg.get kf in
      let info = F.create pdg in
        Cilutil.VarinfoHashtbl.add proj fct_var info; 
        info

  (** Add the marks to the pdg nodes. 
  * @return a merge between the input [other_rqs] and the new requests produced.
  * *)
  let apply_fct_rqs proj (pdg, mark_list) other_rqs =
    match mark_list with 
      | [] -> (* don't want to build the marks when calling [get]
                when there is nothing to do... *) 
          other_rqs
      | _ ->
          let fm = get proj pdg in
          let to_prop = F.mark_and_propagate fm mark_list in 
          let rqs = translate_marks_to_prop pdg to_prop 
                      ~in_m2m:C.mark_to_prop_to_caller_input
                      ~out_m2m:C.mark_to_prop_to_called_output
                      other_rqs in
            rqs

  (** Add the marks to the pdg nodes and also apply all the produced requests
  * to do the interprocedural propagation. *)
  let mark_and_propagate proj pdg node_marks =
    let rec apply_all rqs = match rqs with 
      | [] -> ()
      | rq :: tl_rqs -> 
          let new_rqs = apply_fct_rqs proj rq tl_rqs in 
            apply_all new_rqs
    in apply_all [(pdg, node_marks)]
end



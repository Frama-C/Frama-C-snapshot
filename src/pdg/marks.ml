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

open PdgIndex
open Cil_datatype

(** compute the marks to propagate in the caller nodes from the marks of
 * a function inputs [in_marks].
 *)
let in_marks_to_caller pdg call m2m ?(rqs=[]) in_marks =
  let add_n_m acc n z_opt m =
    let select = PdgMarks.mk_select_node ~z_opt n in
    match m2m select m with
      | None -> acc
      | Some m -> PdgMarks.add_to_select acc select m
  in
  let build rqs (in_key, m) =
    match in_key with
      | Signature.InCtrl ->
          add_n_m rqs (!Db.Pdg.find_call_ctrl_node pdg call) None m
      | Signature.InNum in_num ->
          add_n_m rqs (!Db.Pdg.find_call_input_node pdg call in_num) None m
      | Signature.InImpl zone ->
          let nodes, undef =
            !Db.Pdg.find_location_nodes_at_stmt pdg call ~before:true zone
          in
          let rqs =
            List.fold_left (fun acc (n,z) -> add_n_m acc n z m) rqs nodes in
          let rqs = match undef with None -> rqs
            | Some z ->
               match m2m (PdgMarks.mk_select_undef_zone z) m with None -> rqs
                 | Some m -> PdgMarks.add_undef_in_to_select rqs undef m
          in rqs
  in List.fold_left build rqs in_marks

(** some new input marks has been added in a called function.
 * Build the list of what is to be propagated in the callers.
 * Be careful that some Pdg can be top : in that case, a list of mark is
 * returned (Beware that m2m has NOT been called in that case).
 * *)
let translate_in_marks pdg_called in_new_marks
       ?(m2m=fun _ _ _ m -> Some m) other_rqs =
    let kf_called = PdgTypes.Pdg.get_kf pdg_called in
    let translate pdg rqs call =
      in_marks_to_caller pdg call (m2m (Some call) pdg) ~rqs in_new_marks
    in
    let build rqs (caller, _) =
      let pdg_caller = !Db.Pdg.get caller in
      let caller_rqs =
        try
        let call_stmts = !Db.Pdg.find_call_stmts ~caller kf_called in
          (* TODO : more intelligent merge ? *)
        let rqs = List.fold_left (translate pdg_caller) [] call_stmts in
          PdgMarks.SelList rqs
      with PdgTypes.Pdg.Top ->
        let marks = List.fold_left (fun acc (_, m) -> m::acc) [] in_new_marks
        in PdgMarks.SelTopMarks marks (* #345 *)
    in
      (pdg_caller, caller_rqs)::rqs
    in
    let res = List.fold_left build other_rqs (!Db.Value.callers kf_called) in
      res

let call_out_marks_to_called called_pdg m2m ?(rqs=[]) out_marks =
  let build rqs (out_key, m) =
    let nodes, undef = Sets.find_output_nodes called_pdg out_key in
    let sel =
      List.map (fun (n, _z_opt) -> PdgMarks.mk_select_node ~z_opt:None n) nodes in
    let sel = match undef with None -> sel
      | Some undef -> (PdgMarks.mk_select_undef_zone undef)::sel
    in
    let add acc s = match m2m s m with
        | None -> acc
        | Some m -> (s, m)::acc
    in
    let rqs = List.fold_left add rqs sel in
      rqs
  in
    List.fold_left build rqs out_marks

let translate_out_mark _pdg m2m other_rqs (call, l) =
  let add_list l_out_m called_kf rqs  =
    let called_pdg = !Db.Pdg.get called_kf in
    let m2m = m2m (Some call) called_pdg in
    try
      let node_marks =
        call_out_marks_to_called called_pdg m2m ~rqs:[] l_out_m
      in (called_pdg, PdgMarks.SelList node_marks)::rqs
    with PdgTypes.Pdg.Top ->
      (* no PDG for this function : forget the new marks
      * because anyway, the source function will be called.
      * *)
      rqs
  in
  let all_called = Db.Value.call_to_kernel_function call in
  Kernel_function.Hptset.fold (add_list l)  all_called other_rqs

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
    let rqs =
      List.fold_left (translate_out_mark pdg out_m2m) other_rqs out_marks
    in rqs


(** To also use interprocedural propagation, the user can instantiate this
* functor. This is, of course, not mandatory because one can want to use a more
* complex propagation (like slicing for instance, that has more than one
* version for a source function). *)
module F_Proj (C : PdgMarks.Config) :
  PdgMarks.Proj with type mark = C.M.t
                  and type call_info = C.M.call_info
= struct

  module F = PdgMarks.F_Fct (C.M)

  type mark = C.M.t
  type call_info = C.M.call_info
  type fct = F.fi
  type fct_info = F.t
  type t = fct_info Varinfo.Hashtbl.t

  let empty () = Varinfo.Hashtbl.create 10

  let find_marks proj fct_var =
    try let f = Varinfo.Hashtbl.find proj fct_var in Some (F.get_idx f)
    with Not_found -> None

  let get proj pdg =
    let kf = PdgTypes.Pdg.get_kf pdg in
    let fct_var = Kernel_function.get_vi kf in
    try Varinfo.Hashtbl.find proj fct_var
    with Not_found ->
      let pdg = !Db.Pdg.get kf in
      let info = F.create pdg in
      Varinfo.Hashtbl.add proj fct_var info;
      info

  (** Add the marks to the pdg nodes.
  * @return a merge between the input [other_rqs] and the new requests produced.
  * *)
  let apply_fct_rqs proj (pdg, mark_list) other_rqs =
    match mark_list with
      | PdgMarks.SelList []
      | PdgMarks.SelTopMarks [] ->
          (* don't want to build the marks when calling [get]
             if there is nothing to do... *)
          other_rqs
      | PdgMarks.SelList mark_list ->
          let fm = get proj pdg in
          let to_prop = F.mark_and_propagate fm mark_list in
          let rqs = translate_marks_to_prop pdg to_prop
                      ~in_m2m:C.mark_to_prop_to_caller_input
                      ~out_m2m:C.mark_to_prop_to_called_output
                      other_rqs in
            rqs
      | PdgMarks.SelTopMarks _marks -> (* TODO #345 *)
          Pdg_parameters.not_yet_implemented "mark propagation in Top PDG"

  (** Add the marks to the pdg nodes and also apply all the produced requests
  * to do the interprocedural propagation. *)
  let mark_and_propagate proj pdg node_marks =
    let rec apply_all rqs = match rqs with
      | [] -> ()
      | rq :: tl_rqs ->
          let new_rqs = apply_fct_rqs proj rq tl_rqs in
            apply_all new_rqs
    in apply_all [(pdg, PdgMarks.SelList node_marks)]
end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

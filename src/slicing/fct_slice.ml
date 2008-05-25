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

(** This module deals with slice computation.
 * It computes a mapping between the PDG nodes and some marks
 * (see {!module:Fct_slice.FctMarks}),
 * and also manage interprocedural propagation ({!module:Fct_slice.CallInfo}).
 *
 * Most high level function, named [apply_xxx], 
 * like [apply_change_call], [apply_missing_outputs], ...,
 * correspond the actions defined in the
 * {{:../../slicing/index.html}specification report}.
 *
 * Many functions are modifying the marks of a slice,
 * so they can return a list of actions to be applied in order to deal with
 * the propagation in the calls and callers.
 *
 * Moreover, some function (named [get_xxx_mark]) are provided to retreive
 * the mark of the slice elements.
 * *)

(**/**)

open Cil_types

module T = SlicingTypes.Internals
module M = SlicingMacros
module Marks = SlicingMarks
module Act = SlicingActions

type t_fct_slice = T.t_fct_slice
type t_mark = T.t_pdg_mark
type t_crit = T.t_criterion

(**/**)

(** Manage the information related to a function call in a slice.
* It is composed of the called function if it has been established yet,
* and the call signature. Also deals with the [called_by] information. *)
module CallInfo : sig
  type t 
  type t_call_id =  T.t_fct_slice * T.t_call_id

  val get_info_call : t_call_id -> t
  val fold_calls : (T.t_call_id -> t -> 'a -> 'a) -> 
    T.t_fct_slice -> T.t_marks_index -> 'a -> 'a

  val get_call_f_called : t_call_id -> T.t_called_fct option
  val get_call_sig : t ->  Marks.t_sig_marks 
  val get_call_id : t ->  t_call_id

  val get_f_called : t -> T.t_called_fct option

  val something_visible : t -> bool
  val some_visible_out : t -> bool
  val is_topin_visible : t -> bool

  val remove_called_by : T.t_project -> t_call_id -> t -> unit
  val is_call_to_change : t -> T.t_called_fct option -> bool
  val change_call :  T.t_project -> T.t_marks_index -> t_call_id -> 
                     T.t_called_fct option -> unit

end = struct

  type t_call_id =  T.t_fct_slice * T.t_call_id
  type t = t_call_id * T.t_called_fct option * Marks.t_sig_marks

  let empty = (None, Marks.empty_sig)

  let get_f_called (_id,f,_sgn) = f
  let get_sig (_id,_f,sgn) = sgn
  let get_call_id (id,_f,_sgn) = id
  let get_call_mark (_id,_f,sgn) = Marks.get_in_ctrl_mark sgn

  (** find call information (ff_called option + signature of a call) *)
  let get_info_call call_id = 
    let ff, call = call_id in 
    let f, sgn = 
      try 
        let _, marks = ff.T.ff_marks in
        match PdgIndex.FctIndex.find_call marks call with
        | None, sgn -> None, sgn
        | Some (None), sgn -> None, sgn
        | Some (Some f), sgn -> Some f, sgn
      with PdgIndex.NotFound -> empty
    in (call_id, f, sgn)

  let get_call_f_called call_id = get_f_called (get_info_call call_id)

  let get_call_sig call_info = get_sig call_info

  let fold_calls f ff ff_marks acc = 
    let do_it call (c_opt,sgn) a = 
      let info = match c_opt with
      | None | Some (None) -> ((ff, call), None, sgn)
      | Some (Some f)  -> ((ff, call), Some f, sgn)
      in f call info a
    in
    PdgIndex.FctIndex.fold_calls do_it ff_marks acc

  let something_visible ci = Marks.something_visible (get_sig ci)

  let is_topin_visible ci = Marks.is_topin_visible (get_sig ci)

  let some_visible_out ci = Marks.some_visible_out (get_sig ci)

  let is_call_to_change ci f_to_call  =
    let old_called = get_f_called ci in
    match old_called, f_to_call with
      | None, None -> false
      | None, _ -> true
      | Some (T.CallSrc _), Some (T.CallSrc _)   -> false
      | Some (T.CallSrc _), _ -> true
      | Some (T.CallSlice _), Some (T.CallSrc _) -> true
      | Some (T.CallSlice _), None -> true
      | Some (T.CallSlice ff_called), Some (T.CallSlice ff_to_call) ->
          if (M.equal_ff ff_called ff_to_call) then false else true

  let indirectely_called_src_functions call_id = 
    let _, stmt = call_id in
    let funcexp = match stmt.skind with
      | Instr (Call (_,funcexp,_,_)) -> funcexp
      | _ -> assert false
    in
    let _, called_functions =  
      !Db.Value.expr_to_kernel_function ~with_alarms:CilE.warn_none_mode
        (Kstmt stmt) ~deps:(Some Locations.Zone.bottom) funcexp
    in called_functions

  (** [call_id] is a call to [g] in [f].
  * we don't want [f] to call [g] anymore, so we have to update [g] [called_by]
  * field.
  * *)
  let remove_called_by proj call_id call_info = 
    let rec remove called_by = match called_by with
      | [] -> []
      | e :: called_by -> if (M.same_ff_call call_id e) then called_by
        else e::(remove called_by)
    in
      if M.debug2 () then Format.printf "-> remove old_called@\n";
      let old_called = get_f_called call_info in
      match old_called with 
        | None -> ()
        | Some (T.CallSlice g) -> 
            g.T.ff_called_by <- remove g.T.ff_called_by 
        | Some (T.CallSrc (Some old_fi)) -> 
            old_fi.T.f_called_by <- remove old_fi.T.f_called_by 
        | Some (T.CallSrc (None)) -> 
            let called = indirectely_called_src_functions call_id in
            let update kf =
              let old_fi = M.get_kf_fi proj kf in
                old_fi.T.f_called_by <- remove old_fi.T.f_called_by
            in List.iter update called

  (** very low level function to change information of a call : 
  * no checks at all (they must have been done before).
  * [call] in [ff] is changed in order to call [to_call]. If some function was
  * previously called, update its [called_by] information. *)
  let change_call proj ff_marks call_id to_call =
    if M.debug2 () then Format.printf "CallInfo.change_call@\n";
    let call_info = get_info_call call_id in
    let something_to_do = is_call_to_change call_info to_call in
    if something_to_do then
      begin
        if M.debug2 () then Format.printf "-> remove old_called@\n";
        let _ = remove_called_by proj call_id call_info in  
        if M.debug2 () then Format.printf "-> add new_called@\n";
        let _ = match to_call with 
        | None -> () (* nothing to do *)
        | Some f ->
            begin match f with
            | (T.CallSrc None) -> 
                let called = indirectely_called_src_functions call_id in
                let update kf =
                  let fi = M.get_kf_fi proj kf in
                    fi.T.f_called_by <- call_id :: fi.T.f_called_by
                in List.iter update called
            | (T.CallSlice g) -> 
                g.T.ff_called_by <- call_id :: g.T.ff_called_by
            | (T.CallSrc (Some fi)) -> 
                fi.T.f_called_by <- call_id :: fi.T.f_called_by
            end
        in  
        let _ff, call = call_id in
        let new_call_info = to_call in
        PdgIndex.FctIndex.add_info_call ff_marks call new_call_info true
      end

end

(** [FctMarks] manages the mapping between a function elements and their
* marks. See {!module:PdgIndex.FctIndex} to know what an element is.
*)
module FctMarks : sig
  type t (* =  T.t_marks_index *)
  type t_mark = Marks.t_mark
  type t_pdg = PdgTypes.Pdg.t
  type t_node = PdgTypes.Node.t
  type t_node_key = PdgIndex.Key.t
  type t_to_prop 

  val empty : t_pdg -> T.t_ff_marks
  val empty_to_prop : t_to_prop

  (** build a new, empty, slice for the function *)
  val new_empty_slice : T.t_fct_info -> t_fct_slice
  val new_copied_slice : t_fct_slice -> t_fct_slice

  val new_empty_fi_marks : T.t_fct_info -> t
  val fi_marks :  T.t_fct_info -> t option
  val get_fi_node_mark : T.t_fct_info -> t_node_key -> t_mark
  val is_visible_top_input : T.t_fct_info -> bool

  (** build a new, slice for the function with some initial marks (they will be
  * copied)*)
  val new_init_slice : T.t_fct_info -> T.t_ff_marks -> t_fct_slice

  val get_ff_marks : t_fct_slice -> t

  (** merge the marks and clear all the calls : 
  * they will have to be processed by examine_calls.  *)
  val merge : t_fct_slice -> t_fct_slice -> T.t_ff_marks

  val get_node_mark : t_fct_slice -> t_node_key -> t_mark
  val get_node_marks : t_fct_slice -> t_node_key -> t_mark list

  val get_sgn : t_fct_slice -> Marks.t_sig_marks option

  val get_new_marks: t_fct_slice -> t_mark PdgMarks.t_select -> 
                                    t_mark PdgMarks.t_select

  val get_all_input_marks : t -> t_to_prop

  (** add the given mark to the node, and propagate to its dependencies *)
  val mark_and_propagate     : t -> ?to_prop:t_to_prop ->
      t_mark PdgMarks.t_select -> t_to_prop

  (** add a [Spare] mark to all the input nodes of the call and propagate *)
  val mark_spare_nodes       : t_fct_slice -> T.t_call_id -> t_to_prop

  (** Mark the output nodes can be made visible due to marks in their
   * dependencies. This can occurs if, for instance,
   * the user asked to select a data at the last point of a function. *)
  val mark_visible_output : t -> unit

  (** Some inputs must be visible when a parameter is used as a local variable.
  * ie. its input value is not used.
  * TODO : handle the difference between input value/decl in [Signature] *)
  val mark_visible_inputs : t -> t_to_prop -> t_to_prop

  val marks_for_caller_inputs : 
     t_pdg -> t -> Cil_types.stmt -> t_to_prop -> T.t_fct_info
     -> (t_mark PdgMarks.t_select) * bool

  val marks_for_call_outputs : t_to_prop -> 
    (Cil_types.stmt * (PdgIndex.Signature.t_out_key * t_mark) list) list

  val get_call_output_marks : 
      ?spare_info:CallInfo.t_call_id  option ->
      CallInfo.t -> (PdgIndex.Signature.t_out_key * t_mark) list 

  val persistant_in_marks_to_prop : T.t_fct_info -> t_to_prop ->
    t_mark PdgMarks.t_pdg_select

  (** [f] calls [g] and the call marks have been modified in [f].
  * Compute the marks that should be propagated in [g]. 
  *
  * This function is also use to choose the slice of [g] to call :
  * in that case, the first parameter holds the call output marks
  * that can be given by [get_call_output_marks].
  * *)
  val check_called_marks :
      (PdgIndex.Signature.t_out_key * t_mark) list -> t_fct_slice ->
    (t_mark PdgMarks.t_select) * bool


  val fold_calls : (T.t_call_id -> CallInfo.t -> 'a -> 'a) -> 
                   t_fct_slice -> 'a -> 'a

  val change_call :  T.t_project -> T.t_fct_slice -> T.t_call_id -> 
                     T.t_called_fct option -> unit

  val debug_ff_marks : Format.formatter -> t -> unit

  val debug_marked_ff : Format.formatter -> T.t_fct_slice -> unit

end = struct
  module FI = PdgIndex.FctIndex

  module Marks4Pdg = struct
    type t = Marks.t_mark
    type t_call_info = T.t_call_info
    let is_bottom = Marks.is_bottom_mark
    let merge m1 m2 = Marks.merge_marks [m1; m2]
    let combine = Marks.combine_marks
    let pretty = Marks.pretty_mark
  end
  module PropMark = Db.Pdg.F_FctMarks (Marks4Pdg)

  type t = PropMark.t (* = T.t_ff_marks*)
  type t_mark = PropMark.t_mark
  type t_pdg = PdgTypes.Pdg.t
  type t_node = PdgTypes.Node.t
  type t_node_key = PdgIndex.Key.t

  type t_to_prop = PropMark.t_mark_info_inter

  let empty pdg = PropMark.create pdg

  let empty_to_prop = PropMark.empty_to_prop

  (** @raise  SlicingTypes.NoPdg when the function PDG couldn't have been
  * computed. *)
  let new_slice fi marks =
    let ff_num = fi.T.fi_next_ff_num in
    let pdg =  M.get_fi_pdg fi in
      if (PdgTypes.Pdg.is_top pdg) then raise SlicingTypes.NoPdg;
      let marks = match marks with None -> empty pdg
        | Some (pdg, marks) -> (pdg, FI.copy marks)
      in
      let ff = {  T.ff_fct = fi ; T.ff_id = ff_num ; 
                  T.ff_marks = marks ; T.ff_called_by = [] } in
        fi.T.fi_slices <- ff :: fi.T.fi_slices ;
        fi.T.fi_next_ff_num <- ff_num + 1;
        ff

  let new_copied_slice ff = 
    try
      let fi = M.ff_fi ff in
        new_slice fi (Some ff.T.ff_marks)
    with SlicingTypes.NoPdg -> assert false

  (** @raise  SlicingTypes.NoPdg (see [new_slice]) *)
  let new_init_slice fi marks = new_slice fi (Some marks)

  (** @raise  SlicingTypes.NoPdg (see [new_slice]) *)
  let new_empty_slice fi = new_slice fi None

  let new_empty_fi_marks fi =
    let marks = empty (M.get_fi_pdg fi) in
    fi.T.fi_init_marks <- Some marks ; marks

  let fi_marks fi = fi.T.fi_init_marks

  let get_ff_marks ff = ff.T.ff_marks

  let get_marks (fm:t) = PropMark.get_idx fm

  let merge ff1 ff2 =
    let pdg1, fm1 = ff1.T.ff_marks in
    let pdg2, fm2 = ff2.T.ff_marks in
      assert (Db.Pdg.from_same_fun pdg1 pdg2) ;
    let merge_marks m1 m2 = Marks.merge_marks [m1; m2] in
    let merge_call_info _c1 _c2 = None in
    let fm = FI.merge fm1 fm2 merge_marks merge_call_info in
      (pdg1, fm)

  let get_mark fm node_key =
    try FI.find_info (get_marks fm) node_key
    with PdgIndex.NotFound -> Marks.bottom_mark

  let get_node_mark ff node_key =
    let fm = ff.T.ff_marks in get_mark fm node_key

  let get_fi_node_mark fi node_key =
    match fi_marks fi with None -> Marks.bottom_mark
      | Some fm -> get_mark fm node_key

  let get_node_marks ff node_key = 
    let fm = ff.T.ff_marks in
    FI.find_all (get_marks fm) node_key

  let get_sgn ff = let fm = ff.T.ff_marks in Some (FI.sgn (get_marks fm))

  let get_all_input_marks fm = 
    let fm = get_marks fm in
    let in_marks = Marks.get_all_input_marks (FI.sgn fm) in 
    let out_marks = [] in
    (in_marks, out_marks)

  let fold_calls process ff acc = 
    let fm = ff.T.ff_marks in
    CallInfo.fold_calls process ff (get_marks fm) acc

  let change_call proj ff call newf =
    let ff_marks = get_ff_marks ff in
    let marks = get_marks ff_marks in
    CallInfo.change_call proj marks (ff, call) newf

  (** mark the node with the given mark and propagate it to its dependencies *)
  let mark_and_propagate (fct_marks:t) 
        ?(to_prop=PropMark.empty_to_prop) to_select  =
    PropMark.mark_and_propagate fct_marks ~to_prop to_select

  (** compute the marks to propagate in [pdg_caller] when the called function
 * have the [to_prop] marks.
 * @param init_marks are used to compute [more_inputs] only :
 * a persistant input mark is not considered as a new input.
 * *)
  let marks_for_caller_inputs pdg_caller old_marks call to_prop fi_to_call =
    let in_info, _ = to_prop in
    let new_input = ref false in
    let m2m s m = 
      let key = match s with
        | PdgMarks.SelIn loc -> PdgIndex.Key.implicit_in_key loc 
        | PdgMarks.SelNode n -> !Db.Pdg.node_key n 
      in
      let old_m = get_mark old_marks key in
      let new_m = Marks.missing_input_mark ~call:old_m ~called:m in
        if M.debug2 () then
          Format.printf 
            "marks_for_caller_inputs for %a : old=%a new=%a -> %a@\n"
            !Db.Pdg.pretty_key key Marks.pretty_mark old_m Marks.pretty_mark m
            Marks.pretty_mark
            (match new_m with None -> Marks.bottom_mark | Some m -> m);
      let _ = match new_m with 
        | Some _new_m when Marks.is_bottom_mark old_m -> 
            let init_m = get_fi_node_mark fi_to_call key in
              if Marks.is_bottom_mark init_m then new_input := true
        | _ -> ()
      in new_m 
    in
    let new_input_marks = 
      Pdg.Register.in_marks_to_caller pdg_caller call m2m in_info in
    new_input_marks, !new_input

  let marks_for_call_outputs (_, out_info) = out_info

  let is_visible_top_input fi =
    let m = get_fi_node_mark fi (PdgIndex.Key.top_input) in 
      not (Marks.is_bottom_mark m)

  let get_call_output_marks ?(spare_info=None) call_info =
    let sig_call = CallInfo.get_call_sig call_info in
    let add1 acc (k,m) = (k,m)::acc in
    let call_out_marks = PdgIndex.Signature.fold_all_outputs add1 [] sig_call in
      match spare_info with 
        | None -> call_out_marks
        | Some (ff_call, call) ->
            let pdg = M.get_ff_pdg ff_call in
        let spare = Marks.mk_gen_spare in
        let rec add2 marks n = 
          match !Db.Pdg.node_key n with
            | PdgIndex.Key.SigCallKey (_, n_key) ->
               begin
                match n_key with 
                  | (PdgIndex.Signature.In _) -> marks
                  | (PdgIndex.Signature.Out key) ->
                      match marks with 
                        | [] -> [(key, spare)]
                        | (k, m):: marks -> 
                        if PdgIndex.Signature.equal_out_key k key then 
                          let m = if Marks.is_bottom_mark m then spare else m 
                          in (k, m):: marks
                          else (k, m)::(add2 marks n)
                end
            | _ -> assert false
        in 
          PdgTypes.Pdg.fold_call_nodes add2 call_out_marks pdg call 

  let check_called_marks new_call_marks ff_called =
    let ff_marks = get_ff_marks ff_called in
    let pdg, _ = ff_marks  in
    let new_output = ref false in
    let m2m s m = match s with
      | PdgMarks.SelIn _ -> 
          (* let nkey = PdgIndex.Key.implicit_in_key l in *)
          (* As we are looking for some call output node,
          * even if the data is not entirely defined by the function,
          * it has already been taken into account in the "from". *)
          None
      | PdgMarks.SelNode n -> 
          let nkey = !Db.Pdg.node_key n in
          let old_m = get_mark ff_marks nkey in
          let m_opt = Marks.missing_output_mark ~call:m ~called:old_m in
          let _ = match m_opt with 
            | Some _new_m when Marks.is_bottom_mark old_m -> 
                new_output := true
            | _ -> ()
          in
            if M.debug2 () then
              Format.printf "check_called_marks for %a : old=%a new=%a -> %a@\n"
                !Db.Pdg.pretty_key nkey 
                Marks.pretty_mark old_m
                Marks.pretty_mark m
                Marks.pretty_mark
                (match m_opt with None -> Marks.bottom_mark | Some m -> m);
            m_opt 
    in let new_called_marks =
      Pdg.Register.call_out_marks_to_called pdg m2m new_call_marks
    in new_called_marks, !new_output

  let persistant_in_marks_to_prop fi to_prop  =
    let in_info, _ = to_prop in
    if M.debug2 () then
      Format.printf "[persistant_marks_to_prop] from %s@\n" (M.fi_name fi);
    let m2m _call _pdg_caller _n m = 
      (* if M.debug2 () then
        Format.printf "  in_m2m %a in %s ?@\n" 
          PdgIndex.Key.pretty (!Db.Pdg.node_key n) (M.pdg_name pdg_caller); *)
      Marks.missing_input_mark ~call:Marks.bottom_mark ~called:m
    in
    let pdg = M.get_fi_pdg fi in
    let pdg_node_marks = 
      Pdg.Register.translate_in_marks pdg ~m2m in_info [] in
      pdg_node_marks

  let get_new_marks ff nodes_marks = 
    let fm = get_ff_marks ff in
    let add_if_new acc (n, m) =
      let nkey = match n with
        | PdgMarks.SelNode n -> !Db.Pdg.node_key n 
        | PdgMarks.SelIn l -> PdgIndex.Key.implicit_in_key l
      in
      let oldm = get_mark fm nkey in
      let newm = Marks.minus_marks m oldm in
      (* Format.printf "get_new_marks for %a : old=%a new=%a -> %a@\n"
        !Db.Pdg.pretty_key nkey Marks.pretty_mark oldm
        Marks.pretty_mark m Marks.pretty_mark newm; *)
      if not (Marks.is_bottom_mark newm) then (n, newm)::acc else acc
    in List.fold_left add_if_new [] nodes_marks

  (** We know that the 'call' element is visible.
  * We have to check that all the associated nodes and
  * the dependencies of these nodes are, at least, marked as 'spare'.
  *)
  let mark_spare_nodes ff call =
    let ff_marks = get_ff_marks ff in
    let pdg, _ = ff_marks  in
    let m_spare = Marks.mk_gen_spare in
    let nodes = !Db.Pdg.find_stmt_nodes pdg call in
    let node_marks = List.map (fun n -> (PdgMarks.SelNode n, m_spare)) nodes in
    let to_prop = mark_and_propagate ff_marks node_marks in
      to_prop

   (** TODO :
   * this function should disapear when the parameter declarations will
   * be handled... *)
  let mark_visible_inputs ff_marks to_prop =
    let pdg, _ = ff_marks  in
    let kf = M.get_pdg_kf pdg in
    let param_list = Kernel_function.get_formals kf in
    let rec check_in_params n params = match params with
      | [] -> []
      | _ :: params -> 
          let node = !Db.Pdg.find_input_node pdg n in
          let dpds = !Db.Pdg.direct_dpds pdg node in
          let get_n_mark n = get_mark ff_marks (PdgTypes.Node.elem_key n) in
          let dpds_marks = List.map get_n_mark dpds in
          let m = Marks.inter_marks dpds_marks in
          let marks = check_in_params (n+1) params in
            if not (Marks.is_bottom_mark m) then
              begin
                if M.debug2 () then 
                  Format.printf "check_visible_inputs %a -> %a@\n" 
                    (!Db.Pdg.pretty_node true) node Marks.pretty_mark m;
                (PdgMarks.SelNode node, m)::marks
              end
            else marks
    in let new_marks = check_in_params 1 param_list in
      mark_and_propagate ff_marks ~to_prop new_marks 

  let mark_visible_output ff_marks =
    let pdg, _ = ff_marks  in
      try
        let out_node = !Db.Pdg.find_ret_output_node pdg in
        let dpds = !Db.Pdg.direct_dpds pdg out_node in
        let get_n_mark n = get_mark ff_marks (PdgTypes.Node.elem_key n) in
        let dpds_marks = List.map get_n_mark dpds in
        let m = Marks.inter_marks dpds_marks in
          if not (Marks.is_bottom_mark m) then
            begin
              if M.debug2 () then 
                Format.printf "check_visible_outputs %a -> %a@\n" 
                  (!Db.Pdg.pretty_node true) out_node Marks.pretty_mark m;
              let to_prop = 
                mark_and_propagate ff_marks [(PdgMarks.SelNode out_node, m)] in
                assert (to_prop = PropMark.empty_to_prop); ()
            end
      with PdgIndex.NotFound -> ()

  let debug_ff_marks fmt fm =
    let pdg, fm = fm in
    let print_node node =
      let node_key = PdgTypes.Node.elem_key node in
      let m = 
        try 
          try FI.find_info fm node_key
          with PdgIndex.CallStatement -> assert false
        with PdgIndex.NotFound -> Marks.bottom_mark
      in
        Format.fprintf fmt "%a : %a@\n" (!Db.Pdg.pretty_node true) node 
          Marks.pretty_mark m
    in
      !Db.Pdg.iter_nodes print_node pdg

  let debug_marked_ff fmt ff =
    Format.fprintf fmt "Print slice = %s@\n" (M.ff_name ff);
    let ff_marks =  ff.T.ff_marks in 
      debug_ff_marks fmt ff_marks

end

(*-----------------------------------------------------------------------*)
(** {2 xxx } *)

(** Inform about the called slice or else calls to source functions. *) 
let get_called_slice ff call =
  let call_id = (ff, call) in
  let f_called = CallInfo.get_call_f_called call_id in
  match f_called with
  | None -> None, false
  | Some (T.CallSrc _) -> None, true
  | Some (T.CallSlice g) -> Some g, false

(*-----------------------------------------------------------------------*)
(** {2 xxx } *)

let pretty_node_marks fmt marks =
  let print fmt (n, m) =  
    (!Db.Pdg.pretty_node true) fmt n; Marks.pretty_mark fmt m
  in
    Format.fprintf fmt "%a" (fun fmt x -> List.iter (print fmt) x) marks

let check_outputs call_id called_ff add_spare =
  let (ff_call, call) = call_id in
  if M.debug2 () then 
    Format.printf "[slicing] check %s outputs for call %d in %s@\n"
      (M.ff_name called_ff) call.sid (M.ff_name ff_call);
  let call_info = CallInfo.get_info_call call_id in
  let spare_info = if add_spare then Some call_id else None in
  let out_call = FctMarks.get_call_output_marks ~spare_info call_info in
  let new_marks, more = FctMarks.check_called_marks out_call called_ff in
    (*
  if M.debug2 () then 
    Format.printf "[slicing:check_outputs] need : %a@\n"
      pretty_node_marks new_marks;
  *)
  (new_marks, more)


(** [ff] marks have changed : check if the call to [ff_called] is still ok. *)
let check_ff_called ff call new_marks_in_call_outputs ff_called =
  let call_id = (ff, call) in
  let is_this_call (c, _) = (c.sid = call.sid) in
  let new_call_marks = 
    try
      let _, new_call_marks = 
        List.find is_this_call new_marks_in_call_outputs in
        new_call_marks
    with Not_found -> (* no new marks for this call *) []
  in
  let missing_outputs = 

    match new_call_marks with
    | [] -> (* why do we check this is there is no new mark ??? *)
        check_outputs call_id ff_called false
    | _ -> 
        FctMarks.check_called_marks new_call_marks ff_called 
  in match missing_outputs with
    | ([], false) -> None
    | _ -> 
        let missing_out_act =
          Act.mk_crit_missing_outputs ff call missing_outputs 
        in Some missing_out_act

(** Examine the call statements after the modification of [ff] marks.
  * If one node is visible we have to choose which function to call,
  * or to check if it is ok is something is called already.
  *
  * @return a list of actions if needed.
  *)
let examine_calls ff new_marks_in_call_outputs =
  if M.debug2 () then Format.printf "examine_calls@\n";
  let process_this_call call call_info filter_list =
    if CallInfo.something_visible call_info then
      begin
      if M.debug2 () then Format.printf "  examine visible call %d@\n" call.sid;
      let f_called = CallInfo.get_f_called call_info in
      let filter_list = match f_called with
        | None -> 
            (* have to chose a function to call here *)
            if M.debug2 () then Format.printf "  -> add choose_call@\n";
            (Act.mk_crit_choose_call ff call) :: filter_list
        | Some (T.CallSrc _) -> 
            (* the source function compute every outputs, so nothing to do *)
            if M.debug2 () then Format.printf "  -> source called : nothing to do@\n";
            filter_list
        | Some (T.CallSlice ff_called) ->
            (* call to a sliced function : check if it's still ok,
            * or create new [missing_output] action  *)
            if M.debug2 () then Format.printf "  -> slice called -> check@\n";
            let new_filter =
              check_ff_called ff call new_marks_in_call_outputs ff_called
            in match new_filter with None -> filter_list
              | Some f -> f :: filter_list
      in filter_list
      end
    else (* the call is not visible : nothing to do *)
      begin
        if M.debug2 () then Format.printf "  invisible call -> OK@\n";
        filter_list
      end
  in FctMarks.fold_calls process_this_call ff []

(** build a new empty slice in the given [fct_info].
* If the function has some persistant selection, let's copy it in the new slice.
* Notice that there can be at most one slice for the application entry point
* (main), but we allow to have several slice for a library entry point.
* @param build_actions (bool) is useful if the function has some persistent
* selection : if the new slice marks will be modified just after that, 
* it is not useful to do [examine_calls], but if it is finished, 
* we must generate those actions to choose the calls. 
* @raise SlicingTypes.ExternalFunction if the function has no source code,
*        because there cannot be any slice for it.
    @raise SlicingTypes.NoPdg  (see [new_slice])
*)
let make_new_ff fi build_actions =
  let new_ff fi =
    let some_marks, ff = match  fi.T.fi_init_marks with 
      | None -> false, FctMarks.new_empty_slice fi
      | Some marks -> true, FctMarks.new_init_slice fi marks
    in
    let new_filters = 
      (if build_actions && some_marks then examine_calls ff [] else []) 
    in
      if M.debug1 () then Format.printf "[make_new_ff] = %s@\n" (M.ff_name ff);
      (ff, new_filters)
  in 
  let fname = M.fi_name fi in
  let kf_entry, _ = Globals.entry_point () in
    if fname = Kernel_function.get_name kf_entry then
      match fi.T.fi_slices with 
        | [] -> new_ff fi
        | ff :: [] -> ff, []
        | _ -> M.bug "Entry point shouldn't have several slices"
    else 
      new_ff fi

let copy_slice ff = 
  let kf_entry, _ = Globals.entry_point () in
  if (M.ff_src_name ff) = Kernel_function.get_name kf_entry then
    raise SlicingTypes.OnlyOneEntryPointSlice
  else
    FctMarks.new_copied_slice ff

(** [ff] marks have just been modified :
* check if the [calls] to [ff] compute enough inputs,
* and create [MissingInputs] actions if not. *)
let add_missing_inputs_actions ff calls to_prop actions =
  let fi = M.ff_fi ff in
  let check_call actions call_id =
    let (ff_call, call) = call_id in
    let call_info = CallInfo.get_info_call (ff_call, call) in
    let ff_called = CallInfo.get_f_called call_info in
    let _ = match ff_called with 
      | Some (T.CallSlice ff_called) -> assert (M.equal_ff ff_called ff)
      | _ -> assert false
    in
    let pdg_caller = M.get_ff_pdg ff_call in
    let old_marks = FctMarks.get_ff_marks ff_call in
    let missing_inputs = 
      FctMarks.marks_for_caller_inputs pdg_caller old_marks call to_prop fi
    in
      match missing_inputs with 
        | ([], false) -> actions
        | _ -> let new_action = 
            Act.mk_crit_missing_inputs ff_call call missing_inputs in
            new_action :: actions
  in let actions = List.fold_left check_call actions calls in
  if M.debug2 () then 
    begin
    match actions with 
    | [] -> Format.printf "no missing input@\n"
    |  _ -> Format.printf "add_missing_inputs_actions@\n"
    end;
  actions

(** {2 Adding marks} *)

(** [ff] marks have been modified : we have to check if the calls and the
* callers are ok. Create new actions if there is something to do. 
* Notice that the action creations are independent from the options.
* They will by  used during the applications.
* *)
let after_marks_modifications ff to_prop =
  if M.debug2 () then Format.printf "before after_marks_modifications@\n";
  if M.debug2 () then Format.printf "%a@\n" FctMarks.debug_marked_ff ff;
  let new_filters = [] in
  let calls = M.get_calls_to_ff ff in
  let new_filters = add_missing_inputs_actions ff calls to_prop new_filters in
  let call_outputs = FctMarks.marks_for_call_outputs to_prop in
  let new_filters = (Act.mk_crit_examines_calls ff call_outputs)::new_filters in
  if M.debug2 () then 
    begin match new_filters with 
    | [] -> Format.printf "[after_marks_modifications] no new filters@\n"
    | _ -> Format.printf "[after_marks_modifications] some new filters@\n";
    end;
  new_filters

let apply_examine_calls ff call_outputs = examine_calls ff call_outputs

(** quite internal function that only computes the marks.
* Dont't use it alone because it doesn't take care of the calls and so on.
* See [apply_add_marks] or [add_marks_to_fi] for higher level functions. *)
let add_marks fct_marks nodes_marks =
  if M.debug2 () then Format.printf "add_marks@\n";
  let to_prop = FctMarks.mark_and_propagate fct_marks nodes_marks in
  FctMarks.mark_visible_output fct_marks;
  let to_prop = FctMarks.mark_visible_inputs fct_marks to_prop in
  to_prop

(** main function to build or modify a slice.  
  * @return a list of the filters to add to the worklist.
*)
let apply_add_marks ff nodes_marks =
  if M.debug3 () then 
    Format.printf "apply_add_marks@\n-BEFORE:@\n%a" FctMarks.debug_marked_ff ff;
  (*let pdg = M.get_ff_pdg ff in*)
  let to_prop = add_marks (FctMarks.get_ff_marks ff) nodes_marks in
  let new_filters = after_marks_modifications ff to_prop in
  new_filters

(** a function that doesn't modify anything but test if the [nodes_marks]
* are already in the slice or not.
* @return the [nodes_marks] that are not already in.
*)
let filter_already_in ff selection =
  FctMarks.get_new_marks ff selection

(** when the user adds persistent marks to a function,
* he might want to propagate them to the callers,
* but, anyway, we don't want to propagate persistent marks to the calls
* for the same reason (if we mark [x = g ();] in [f], we don't necessarily want 
* all versions of [g] to have a visible [return] for instance).
**)
let prop_persistant_marks proj fi to_prop prop_to_callers actions =
  if prop_to_callers then
    let pdg_node_marks = FctMarks.persistant_in_marks_to_prop fi to_prop in
    let add_act acc (pdg, node_marks) =
      let kf = M.get_pdg_kf pdg in
      let fi = M.get_kf_fi proj kf in
      let a = Act.mk_crit_prop_persit_marks fi node_marks in
        a::acc
    in List.fold_left add_act actions pdg_node_marks
  else actions

(** add the marks to the persistent marks to be used when new slices will be 
* created. The actions to add the marks to the existing slices are generated 
* in slicingProject. If it is the first persistent selection for this function,
* and [propagate=true], also generates the actions to make every calls to this
* function visible. 
* @raise SlicingTypes.ExternalFunction if the function has no source code,
*        because there cannot be any slice for it. *)
let add_marks_to_fi proj fi nodes_marks propagate actions =
  if M.debug2 () then Format.printf "add_marks_to_fi (persistant)@\n";
  let marks, are_new_marks =
    match FctMarks.fi_marks fi with 
      | Some m -> m, false
      | None -> 
          let init_marks = FctMarks.new_empty_fi_marks fi in
            init_marks, true
  in
  let to_prop = add_marks marks nodes_marks in
  let actions = if FctMarks.is_visible_top_input fi 
  then begin
    if M.debug1 () then
      Format.printf "... top input in %s@\n" (M.fi_name fi);
    (Act.mk_crit_fct_top fi)::actions
  end
  else
    let actions = if propagate && are_new_marks then 
      (* (not are_new_marks), the calls must already be selected *)
      (Act.mk_appli_select_calls fi)::actions else actions 
    in prop_persistant_marks proj fi to_prop propagate actions 
  in
      actions

(** {3 Choosing the function to call} *)

(** Build a new action [ChangeCall] (if needed) *)
let add_change_call_action ff call call_info f_to_call actions =
  if M.debug2 () then 
    Format.printf "[slicing] add_change_call_action : ";
  let add_change_call = CallInfo.is_call_to_change call_info (Some f_to_call)
  in
    if add_change_call then
      begin
        let change_call_action = Act.mk_crit_change_call ff call f_to_call in
        if M.debug2 () then Format.printf "%a@\n"
                              Act.print_crit change_call_action;
          change_call_action :: actions
      end
    else 
      begin
        if M.debug2 () then Format.printf "not needed@\n";
        actions
      end

(** choose among the already computed slice if there is a function that computes
* just enought outputs (what ever their marks are). If not, create a new one *)
let choose_precise_slice fi_to_call call_info =
  let (caller, call) = CallInfo.get_call_id call_info in
  let pdg_caller = M.get_ff_pdg caller in
  let caller_marks = FctMarks.get_ff_marks caller in
  let out_call = FctMarks.get_call_output_marks call_info in
  let rec find slices = match slices with
    |  [] -> 
        make_new_ff fi_to_call true
    | ff :: slices -> 
        let _missing_outputs, more_outputs = 
          FctMarks.check_called_marks out_call ff
        in
          if more_outputs 
          then (* not enough outputs in [ff] *)
            begin
              if M.debug2 () then 
                Format.printf 
                  "[choose_precise_slice] %s ? not enought outputs@\n" 
                  (M.ff_name ff);
              find slices
            end
          else
            begin
              let ff_marks = FctMarks.get_ff_marks ff in
              let input_marks = FctMarks.get_all_input_marks ff_marks in
              let _ , more_inputs = 
                FctMarks.marks_for_caller_inputs pdg_caller caller_marks 
                  call input_marks fi_to_call
              in 
                if more_inputs 
                then (* [ff] needs too many inputs *)
                  begin
                    if M.debug2 () then 
                      Format.printf 
                        "[choose_precise_slice] %s ? too many inputs@\n" 
                        (M.ff_name ff);
                    find slices
                  end
                else 
                  begin
                    if M.debug2 () then 
                      Format.printf 
                        "[choose_precise_slice] %s ? ok@\n" 
                        (M.ff_name ff);
                    ff , []
                  end
            end
  in
  let slices = M.fi_slices fi_to_call in
    find slices

(** choose the function to call according to the slicing level of the function
 * to call *)
let choose_f_to_call fbase_to_call call_info =
  if M.debug2 () then Format.printf "choose_f_to_call@\n";
  (*
  let sig_call = CallInfo.get_sig call_info in
  let _m_call = CallInfo.get_call_mark call_info in
  *)
  let choose_min_slice fi_to_call =
    if M.debug2 () then Format.printf "MinimizeNbSlice -> choose_min_slice@\n";
    let slices = M.fi_slices fi_to_call in
      match slices with 
        | [] -> make_new_ff fi_to_call true
        | ff :: [] -> ff, []
        | _ -> (* TODO : choose a slice *)
	    Extlib.not_yet_implemented "choose_min_slice with several slices"
  in 
  let choose_full_slice fi_to_call =
    if M.debug2 () then Format.printf "PropagateMarksOnly -> choose_full_slice@\n";
      match M.fi_slices fi_to_call with 
        | [] -> make_new_ff fi_to_call true
               (* the signature is computed in [apply_choose_call]
                * (missing_outputs) *)
        | ff :: [] -> ff, []
        | _ -> (* TODO : choose a slice *)
            Extlib.not_yet_implemented "choose_full_slice with several slices"
  in 
  let to_call, new_filters = match fbase_to_call with
    | None -> (* if we don't know the called function :
                 either it is a call through a pointer or an external or
                         variadic function.
                 we don't try to slice it : we keep the source call *)
        if M.debug1 () then 
          Format.printf "[slicing] unknown called function : keep src@\n";
        T.CallSrc None, []
    | Some fi_to_call when CallInfo.is_topin_visible call_info ->
        Cil.log "[slicing] top input visible for %s : call source function@\n"
          (M.fi_name fi_to_call);
        T.CallSrc None, []
    | Some fi_to_call ->
        try
          let slicing_level = M.fi_slicing_level fi_to_call in
            if M.debug1 () then 
              Format.printf "[slicing] choose_call with level %s@\n"
                (M.str_level_option slicing_level);
            match slicing_level with
            | T.DontSlice ->
                if M.debug2 () then Format.printf "DontSliceCalls -> call src@\n";
                T.CallSrc fbase_to_call, []
            | T.DontSliceButComputeMarks ->
                let ff_to_call, new_filters = choose_full_slice fi_to_call in
                  (T.CallSlice ff_to_call), new_filters
            | T.MinNbSlice ->
                let ff_to_call, new_filters = choose_min_slice fi_to_call in
                  (T.CallSlice ff_to_call), new_filters
            | T.MaxNbSlice ->
                let ff_to_call, new_filters = 
                  choose_precise_slice fi_to_call call_info in
                  (T.CallSlice ff_to_call), new_filters
        with SlicingTypes.NoPdg -> 
          Cil.log "[slicing]unable to compute %s PDG : call source function@\n"
            (M.fi_name fi_to_call);
          T.CallSrc None, []
  in to_call, new_filters

(** we are about to call [ff] for [sig_call] : let's first add some more output
* marks in [ff] if needed. *)
let check_called_outputs call_id ff actions =
  let level = M.ff_slicing_level ff in
  let add_spare = (level = T.DontSliceButComputeMarks) in
  let missing_outputs, _more_outputs = check_outputs call_id ff add_spare in
  let actions = 
    match missing_outputs with
    | [] -> actions
    | _ ->
        let add_outputs = Act.mk_crit_add_output_marks ff missing_outputs in 
        add_outputs :: actions
  in actions

(** Choose the function (slice or source) to call according to the
*   slicing level of the called function.
* Does nothing if there is already a called function :
*   this is useful because we can sometime generate several [choose_call]
*   for the same call, and we want to do something only the first time.
* Build an action [change_call] to really call it.
* If the chosen function doesn't compute enough output,
*   build an action to add outputs to it.
* *)
let apply_choose_call proj ff call =
  if M.debug2 () then Format.printf "apply_choose_call for call-%d@\n" call.sid;
  let call_id = ff, call in
  let call_info = CallInfo.get_info_call (ff, call) in
    if ((CallInfo.get_f_called call_info) = None) then
      begin
        if CallInfo.something_visible call_info then
          let fbase_to_call = M.get_fi_call proj call in
          let f_to_call, actions = 
            choose_f_to_call fbase_to_call call_info in
          let actions = 
            add_change_call_action ff call call_info f_to_call actions in
          let actions = match f_to_call with
            | T.CallSrc _ -> actions
            | T.CallSlice ff -> 
                check_called_outputs call_id ff actions
          in actions
        else
          begin
            if M.debug2 () then 
              Format.printf "  -> invisible call : nothing to do @\n";
            []
          end
      end
    else 
      begin
        if M.debug2 () then 
          Format.printf "  -> already call something : nothing to do@\n";
        []
      end

(** {4 Calls input/output marks} *)

(** propagate the [input_marks] in the inputs of [call] in [ff]. *)
let modif_call_inputs ff _call input_marks =
  (*
  if M.debug1 () then 
    Format.printf "[slicing] modif_call_inputs : %a@\n"
          pretty_node_marks input_marks;
  *)
  add_marks (FctMarks.get_ff_marks ff) input_marks

(** [modif_call_inputs] and then, check the calls and the callers *)
let apply_modif_call_inputs ff call missing_inputs =
  if M.debug2 () then Format.printf "apply_modif_call_inputs@\n";
  let input_marks, _more_inputs = missing_inputs in
  let to_prop = modif_call_inputs ff call input_marks in
  let new_filters = after_marks_modifications ff to_prop in
    new_filters

(** [ff] calls a slice [g] that needs more inputs than those computed by [ff].
* The slicing level of [ff] is used in order to know if we have to modify [ff] 
* or to call another function. *)
let apply_missing_inputs proj ff call missing_inputs =
  let input_marks, more_inputs = missing_inputs in
  if M.debug1 () then
    Format.printf "[slicing] apply_missing_inputs (%s) @\n"
          (if more_inputs then "more" else "marks");
  let rec visible_top in_marks = match in_marks with 
    | [] -> false
    | (sel, m)::tl -> 
        assert (not (Marks.is_bottom_mark m));
        match sel with 
          | PdgMarks.SelNode n 
              when (!Db.Pdg.node_key n = PdgIndex.Key.top_input) -> true
          | _ -> visible_top tl
  in let is_top_visible = visible_top input_marks in
  let level = M.ff_slicing_level ff in
    if is_top_visible || (more_inputs && level = T.MaxNbSlice) then
      (* if adding marks doesn't change the visibility of the inputs,
      * let's keep the same called function. If it adds visible inputs,
      * let's choose another one *)
      begin
        FctMarks.change_call proj ff call None;
        apply_choose_call proj ff call 
      end
    else 
        apply_modif_call_inputs ff call missing_inputs

(** [ff] calls a slice [g] that doesn't compute enough outputs for the [call].
* The missing marks are [output_marks].
* The slicing level has to be used to choose either to modify the called
* function [g] or to change it.
*)
let apply_missing_outputs proj ff call output_marks more_outputs =
  if M.debug2 () then Format.printf "apply_missing_outputs@\n";
  let ff_g = match CallInfo.get_call_f_called (ff, call) with
      | Some (T.CallSlice g) -> g
      | _ -> (* we shouldn't be here *) assert false
  in
  let g_slicing_level = M.ff_slicing_level ff_g in
    if more_outputs && g_slicing_level = T.MaxNbSlice
    then
      begin
        (* the easiest way is to ignore the called function and to use
        * [choose_call] *)
        FctMarks.change_call proj ff call None;
        apply_choose_call proj ff call
      end
    else
        apply_add_marks ff_g output_marks
            


(** {3 Changing the function to call} *)

(** check if [f_to_call] is ok for this call, and if so,
* change the function call and propagate missing marks in the inputs
* if needed.
* @raise ChangeCallErr if [f_to_call] doesn't compute enought outputs.
*)
let apply_change_call proj ff call f_to_call =
  if M.debug1 () then 
    Format.printf "[slicing] apply_change_call@\n";
  let pdg = M.get_ff_pdg ff in
  let to_call, to_prop =
    match f_to_call with
      | T.CallSlice ff_to_call ->
          let to_call_sig = FctMarks.get_sgn ff_to_call in
          let top = match to_call_sig with None -> false
            | Some to_call_sig -> Marks.is_topin_visible to_call_sig
          in
          if top then begin
            Cil.log "[slicing] top input in %s -> call source function@\n"
              (M.ff_name ff_to_call);
            let to_prop = FctMarks.mark_spare_nodes ff call in
              T.CallSrc (Some (M.ff_fi ff_to_call)), to_prop
          end
          else begin 
            let f = match check_outputs (ff, call) ff_to_call false with
              | ([], false) -> f_to_call
              | _ -> raise (SlicingTypes.ChangeCallErr 
                              "not enough computed output")
            in
            (* find [f_to_call] input marks *)
            let marks = FctMarks.get_ff_marks ff_to_call in
            let input_marks = FctMarks.get_all_input_marks marks in
            let ff_marks = FctMarks.get_ff_marks ff in
            let missing_inputs, _more = 
              FctMarks.marks_for_caller_inputs pdg ff_marks call input_marks 
                                               (M.ff_fi ff_to_call) 
            in
            let to_prop = modif_call_inputs ff call missing_inputs in
              f, to_prop
          end
      | T.CallSrc _ ->
          let to_prop = FctMarks.mark_spare_nodes ff call in
          f_to_call, to_prop
  in
    FctMarks.change_call proj ff call (Some to_call);
    let new_filters = after_marks_modifications ff to_prop in
    new_filters


(** When the user wants to make a [change_call] to a function that doesn't
 * compute enough outputs, he can call [check_outputs_before_change_call] in
* order to build the action the add those outputs. *)
let check_outputs_before_change_call _proj caller call ff_to_call =
  let call_id = caller, call in
  let actions = [] in
  let actions = check_called_outputs call_id ff_to_call actions in
  actions

(*-----------------------------------------------------------------------*)
(** {2 Merge, remove, ...} *)

(** Build a new slice which marks are a join between [ff1] marks and [ff2]
* marks. The result [ff] is not called at the end of this action.
* [examine_calls] is called to generate the actions to choose the calls. *)
let merge_slices ff1 ff2 =
  let fi = M.ff_fi ff1 in
    assert (M.equal_fi fi (M.ff_fi ff2)); (* TODO : raise exception *)
  let ff, _ = 
    try make_new_ff fi false  
        (* [ff] can already have some persistent selection,
        * but we can safely forget then because they then have to also be in
        * [ff1] and [ff2]. *)
    with SlicingTypes.NoPdg -> assert false
  in
  ff.T.ff_marks <- FctMarks.merge ff1 ff2;
  let to_prop = FctMarks.empty_to_prop (* ff is new, so it isn't called,
                      and all its calls are reset to None... *) in
  let new_filters = after_marks_modifications ff to_prop in
  ff, new_filters

(** [ff] has to be removed. We have to check if it is not called
* and to remove the called function in [ff]. 
* @raise SlicingTypes.CantRemoveCalledFf if the slice is called.
* *)
let clear_ff proj ff = 
  let clear_call call_stmt call_info _ = 
    CallInfo.remove_called_by proj (ff, call_stmt) call_info in
  match ff.T.ff_called_by with 
    | [] -> 
        FctMarks.fold_calls clear_call ff ()
    | _ -> raise SlicingTypes.CantRemoveCalledFf

(*-----------------------------------------------------------------------*)
(** {2 Getting the slice marks} *)

let get_node_mark ff node =
  try FctMarks.get_node_mark ff (PdgTypes.Node.elem_key node)
  with PdgIndex.NotFound ->  Marks.bottom_mark

let get_local_var_mark ff var = 
  try
      FctMarks.get_node_mark ff (PdgIndex.Key.decl_var_key var) 
  with PdgIndex.NotFound ->  Marks.bottom_mark

let get_param_mark ff n = 
    try
      match FctMarks.get_sgn ff with None -> Marks.bottom_mark
        | Some sgn ->  Marks.get_input_mark sgn n
    with PdgIndex.NotFound ->  Marks.bottom_mark

let get_label_mark ff label_stmt label =
  try
    let key = PdgIndex.Key.label_key label_stmt label in
    FctMarks.get_node_mark ff key
  with PdgIndex.NotFound ->  Marks.bottom_mark

let get_stmt_mark ff stmt =
  try 
    let stmt_key = PdgIndex.Key.stmt_key stmt in
    let marks = FctMarks.get_node_marks ff stmt_key in 
    let marks = match stmt_key with 
      | PdgIndex.Key.Stmt _ -> marks
      | PdgIndex.Key.CallStmt _ -> marks
      | _ -> assert false
    in
    Marks.merge_marks marks
  with PdgIndex.NotFound ->     
    match stmt.Cil_types.skind with 
    | Cil_types.Block _ -> 
        (* block are always visible for syntaxic reasons *)
        Marks.mk_gen_spare
    | _ -> Marks.bottom_mark

let get_top_input_mark fi =
  try
    let key = PdgIndex.Key.top_input in
    FctMarks.get_fi_node_mark fi key
  with PdgIndex.NotFound ->  Marks.bottom_mark

let merge_inputs_m1_mark ff =
  let ff_sig = 
    match FctMarks.get_sgn ff with Some s -> s 
    | None -> M.bug "Should have a signature !"
  in Marks.merge_inputs_m1_mark ff_sig

let get_input_loc_under_mark ff loc = 
  let ff_sig = 
    match FctMarks.get_sgn ff with Some s -> s 
    | None -> M.bug "Should have a signature !"
  in Marks.get_input_loc_under_mark ff_sig loc

(*-----------------------------------------------------------------------*)
(** {2 Printing} (see also {!PrintSlice}) *)

let print_ff_sig fmt ff =
  Format.fprintf fmt "%s : " (M.ff_name ff);
  match FctMarks.get_sgn ff with 
    | None -> Format.fprintf fmt "<not computed>@\n"
    | Some s -> Format.fprintf fmt "%a@\n" Marks.pretty_sig s
(*-----------------------------------------------------------------------*)

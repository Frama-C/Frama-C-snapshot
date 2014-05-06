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

(** This file provides useful things to help to associate an information
* (called mark) to PDG elements and to propagate it across the
* dependencies.
*)

open PdgIndex

type select_elem =
  | SelNode of PdgTypes.Node.t * Locations.Zone.t option
                                     (** zone is [Some z] only for nodes that
                                     * represent call output in case we want to
                                     * select less than the whole OutCall *)
  | SelIn of Locations.Zone.t

type 'tm select = (select_elem * 'tm) list

type 'tm pdg_select_info = SelList of  'tm select | SelTopMarks of 'tm list
type 'tm pdg_select = (PdgTypes.Pdg.t * 'tm pdg_select_info) list

type 'tm info_caller_inputs = (Signature.in_key * 'tm) list

type 'tm info_called_outputs =
    (Cil_types.stmt * (Signature.out_key * 'tm) list) list

type 'tm info_inter = 'tm info_caller_inputs * 'tm info_called_outputs

let mk_select_node ?(z_opt=None) node = SelNode (node, z_opt)
let mk_select_undef_zone zone = SelIn zone

let add_to_select select sel m = (sel, m)::select

let add_node_to_select select (node,z_opt) m =
  add_to_select select (mk_select_node ~z_opt node) m

let add_undef_in_to_select select undef m =
  match undef with
    | None -> select
    | Some loc ->
        if (Locations.Zone.equal Locations.Zone.bottom loc) then select
        else add_to_select select (mk_select_undef_zone loc) m

(** Type of the module that the user has to provide to describe the marks.  *)
module type Mark = sig
  type t
  type call_info

  val is_bottom : t -> bool

  val merge : t -> t -> t

  val combine : t -> t -> (t * t)

  val pretty : Format.formatter -> t -> unit
end


module type Fct = sig

  type mark
  type call_info
  type fi = (mark, call_info) PdgIndex.FctIndex.t
  type t = PdgTypes.Pdg.t * fi

  val create : PdgTypes.Pdg.t -> t
  val get_idx : t -> fi

  type mark_info_inter = mark info_inter

  val empty_to_prop : mark_info_inter

  val mark_and_propagate :
    t -> ?to_prop:mark_info_inter -> mark select -> mark_info_inter
end


(** If the marks provided by the user respect some constraints (see [Mark]),
* we have that, after the marks propagation,
* the mark of a node are always smaller than the sum of the marks of its
* dependencies. It means that the mark of the statement [x = a + b;]
* have to be smaller that the mark of [a] plus the mark of [b] at this point.
*
* If the marks are used for visibility for instance,
* it means that if this statement is visible,
* so must be the computation of [a] and [b], but [a] and/or [b] can be
* visible while [x] is not.
*)
module F_Fct (M : Mark)
  : Fct with type mark = M.t
           and type call_info = M.call_info

= struct

  type mark = M.t
  type call_info = M.call_info
  type fi = (mark, call_info) PdgIndex.FctIndex.t
  type t = PdgTypes.Pdg.t * fi

  type mark_info_inter = mark info_inter

  let empty_to_prop = ([], [])

  let create pdg =
    let idx = (PdgIndex.FctIndex.create 17) (* TODO Pdg.get_index_size pdg *)
    in (pdg, idx)

  let get_idx (_pdg, idx) = idx

  (** add the given mark to the node.
     @return [Some m] if [m] has to be propagated in the node dependencies,
             [None] otherwise.
   *)
  let add_mark _pdg fm node_key mark =
    Kernel.debug
      ~level:2
      "[pdgMark] add_mark %a -> %a @\n"
      PdgIndex.Key.pretty node_key M.pretty mark ;
    let mark_to_prop =
      try
        begin (* simple node *)
          let new_mark, mark_to_prop =
            try
              let old_mark = PdgIndex.FctIndex.find_info fm node_key in
              let new_m, m_prop = M.combine old_mark mark in
              (new_m, m_prop)
            with Not_found -> (mark, mark)
          in PdgIndex.FctIndex.add_or_replace fm node_key new_mark;
          mark_to_prop
        end
      with PdgIndex.CallStatement -> (* call statement *) assert false
    in mark_to_prop

  let add_in_to_to_prop to_prop in_key mark =
    let rec add marks = match marks with
      | [] -> [(in_key, mark)]
      | (k, m)::tl ->
          let cmp =
            try Signature.cmp_in_key in_key k
            with PdgIndex.Not_equal ->
              (* k and in_key are 2 different InImpl : look for in_key in tl *)
              (* TODO : we could try to group several InImpl... *)
              1
          in
            if cmp = 0 then (in_key, M.merge m mark)::tl
            else if cmp < 0 then (in_key, mark) :: marks
            else (k, m)::(add tl)
    in
    let in_marks, out_marks = to_prop in
    let new_in_marks = add in_marks in
    new_in_marks, out_marks

  (** the new marks [to_prop] are composed of two lists :
  * - one [(in_key, mark) list] means that the mark has been added in the input,
  * - one [call, (out_key, m) list] that means that [m] has been added
  * to the [out_key] output of the call.
  *
  * This function [add_to_to_prop] groups similar information,
  * and keep the list sorted.
  *)
  let add_to_to_prop to_prop key mark =
    let rec add_out_key l key = match l with
      | [] -> [(key, mark)]
      | (k, m) :: tl ->
          let cmp =
            match key, k with
            | Signature.OutLoc z, Signature.OutLoc zone ->
                if Locations.Zone.equal z zone then 0 else 1
            | _ -> Signature.cmp_out_key key k
          in
            if cmp = 0 then (key, M.merge m mark)::tl
            else if cmp < 0 then (key, mark) :: l
            else (k, m)::(add_out_key tl key)
    in
    let rec add_out out_marks call out_key = match out_marks with
      | [] -> [ (call, [(out_key, mark)]) ]
      | (c, l)::tl ->
            if call.Cil_types.sid = c.Cil_types.sid
            then (c, add_out_key l out_key)::tl
            else (c, l)::(add_out tl call out_key)
    in
      match key with
        | Key.SigCallKey (call, Signature.Out out_key) ->
            let in_marks, out_marks = to_prop in
            let call = Key.call_from_id call in
            let new_out_marks = add_out out_marks call out_key in
              (in_marks, new_out_marks)
        | Key.SigKey (Signature.In in_key) ->
            let to_prop = add_in_to_to_prop to_prop in_key mark in
              to_prop
        | _ -> (* nothing to do *) to_prop


  (** mark the nodes and their dependencies with the given mark.
  *  Stop when reach a node which is already marked with this mark.
  * @return the modified marks of the function inputs,
  * and of the call outputs for interprocedural propagation.
  * *)
  let rec add_node_mark_rec pdg fm node_marks to_prop =
    let mark_node_and_dpds to_prop (node, z_opt, mark) =
      Kernel.debug ~level:2
          "[pdgMark] add mark to node %a" PdgTypes.Node.pretty node;
      let node_key = PdgTypes.Node.elem_key node in
      let node_key = match z_opt with
        | None -> node_key
        | Some z ->
            match node_key with
            | Key.SigCallKey (call, Signature.Out (Signature.OutLoc out_z)) ->
                let z = Locations.Zone.narrow z out_z in
                Key.call_output_key (Key.call_from_id call) z
            | _ -> node_key
      in
      let mark_to_prop = add_mark pdg fm node_key mark in
      if (M.is_bottom mark_to_prop) then begin
        Kernel.debug ~level:2
          "[pdgMark] mark_and_propagate = stop propagation !@\n";
        to_prop
      end else begin
        Kernel.debug ~level:2
          "[pdgMark] mark_and_propagate = to propagate %a@\n"
          M.pretty mark_to_prop;
        let to_prop = add_to_to_prop to_prop node_key mark_to_prop in
        let dpds_info = PdgTypes.Pdg.get_all_direct_dpds pdg node in
        let node_marks =
          List.map (fun (n, z) -> (n, z, mark_to_prop)) dpds_info in
        add_node_mark_rec pdg fm node_marks to_prop
      end
    in
    List.fold_left mark_node_and_dpds to_prop node_marks

  let mark_and_propagate fm ?(to_prop=empty_to_prop) select =
    let pdg, idx = fm in
    let process to_prop (sel, mark) = match sel with
      | SelNode (n, z_opt) ->
          Kernel.debug ~level:2
            "[pdgMark] mark_and_propagate start with %a@\n"
            PdgTypes.Node.pretty_with_part (n, z_opt);
          add_node_mark_rec pdg idx [(n, z_opt, mark)] to_prop
      | SelIn loc ->
          let in_key = Key.implicit_in_key loc in
          Kernel.debug ~level:2
            "[pdgMark] mark_and_propagate start with %a@\n"
            Key.pretty in_key;
          let mark_to_prop = add_mark pdg idx in_key mark in
          if M.is_bottom mark_to_prop then to_prop
          else add_to_to_prop to_prop in_key mark_to_prop
    in
    List.fold_left process to_prop select

end

module type Proj = sig
  type t

  type mark
  type call_info
  type fct = (mark, call_info) PdgIndex.FctIndex.t

  val empty : unit -> t
  val find_marks : t -> Cil_types.varinfo -> fct option
  val mark_and_propagate : t -> PdgTypes.Pdg.t -> mark select -> unit
end

type 'mark m2m = select_elem -> 'mark -> 'mark option
type 'mark call_m2m = Cil_types.stmt option -> PdgTypes.Pdg.t -> 'mark m2m

module type Config = sig
  module M : Mark
  val mark_to_prop_to_caller_input : M.t call_m2m
  val mark_to_prop_to_called_output : M.t call_m2m
end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

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

(** *)

open Cil_types

exception AddError
exception CallStatement
exception Not_equal

let is_call_stmt stmt =
  match stmt.skind with Instr (Call _) -> true | _ -> false

module Signature = struct
  type in_key = InCtrl | InNum of int | InImpl of Locations.Zone.t
  type out_key = OutRet | OutLoc of Locations.Zone.t
  type key = In of in_key | Out of out_key

  type 'info t =
      { in_ctrl : 'info option ;
        in_params : (int * 'info) list ;
        (** implicit inputs :
            Maybe we should use [Lmap_bitwise.Make_bitwise] ?
            but that would make things a lot more complicated... :-? *)
        in_implicits : (Locations.Zone.t * 'info) list ;
        out_ret : 'info option ;
        outputs : (Locations.Zone.t * 'info) list }

  module Str_descr = struct
    open Structural_descr
    let in_key = t_sum [| [| p_int |]; [| Locations.Zone.packed_descr |] |]
    let out_key = t_sum [| [| Locations.Zone.packed_descr |] |]
    let key = t_sum [| [| pack in_key |]; [| pack out_key |] |]

    let t d_info = t_record
          [| pack (t_option d_info);
             pack (t_list (t_tuple [| p_int; pack d_info |]));
             pack (t_list (t_tuple [| Locations.Zone.packed_descr;
                                      pack d_info |]));
             pack (t_option d_info);
             pack (t_list (t_tuple [| Locations.Zone.packed_descr;
                                      pack d_info |]));
          |]
  end

  let empty = { in_ctrl = None ;
                in_params = [] ; in_implicits = [] ;
                out_ret = None; outputs = [] }

  let in_key n = In (InNum n)
  let in_impl_key loc = In (InImpl loc)
  let in_top_key = in_impl_key (Locations.Zone.top)
  let in_ctrl_key = In InCtrl
  let out_ret_key = Out OutRet
  let out_key out = Out (OutLoc out)

  let mk_undef_in_key loc = InImpl loc

  let copy sgn = sgn

  (** InCtrl < InNum < InImpl *)
  let cmp_in_key k1 k2 = match k1, k2 with
    | (InImpl z1), (InImpl z2) when Locations.Zone.equal z1 z2 -> 0
    | (InImpl _), (InImpl _) -> raise Not_equal
    | (InImpl _), _ -> 1
    | _, (InImpl _) -> -1
    | InNum n1, InNum n2 -> n1 - n2
    | (InNum _), _ -> 1
    | _, (InNum _) -> -1
    | InCtrl, InCtrl -> 0

  (** OutRet < OutLoc *)
  let cmp_out_key k1 k2 = match k1, k2 with
    | OutRet, OutRet -> 0
    | OutRet, (OutLoc _) -> -1
    | (OutLoc _), OutRet -> 1
    | OutLoc l1, OutLoc l2 when Locations.Zone.equal l1 l2 -> 0
    | OutLoc _, OutLoc _ -> raise Not_equal

  let equal_out_key k1 k2 =
    try (0 = cmp_out_key k1 k2) with Not_equal -> false

  (** add a mapping between [num] and [info] in [lst].
  * if we already have something for [num], use function [merge] *)
  let add_in_list lst num info merge =
    let new_e = (num, info) in
    let rec add_to_l l =
      match l with [] -> [new_e]
        | (ne, old_e) as e :: tl ->
            if ne = num then
              let e = merge old_e info in (num, e)::tl
            else if ne < num then e :: (add_to_l tl) else new_e :: l
    in add_to_l lst

  let add_loc l_loc loc info merge =
    let rec add lst = match lst with
      | [] -> [(loc, info)]
      | (l, e)::tl ->
          if Locations.Zone.equal l loc then
            let new_e = merge e info in (loc, new_e)::tl
          else
            begin
              (*
              if (Locations.Zone.intersects l loc) then
                begin
                  Format.printf "[pdg] implicit inputs intersect : %a and %a\n"
                    Locations.Zone.pretty l Locations.Zone.pretty loc;
                  assert false
                end;
                   *)
              (l, e)::(add tl)
            end
    in add l_loc

  let add_replace replace _old_e new_e =
    if replace then new_e else raise AddError

  let add_input sgn n info ~replace =
    { sgn with in_params =
        add_in_list sgn.in_params n info (add_replace replace) }

  let add_impl_input sgn loc info ~replace =
    { sgn with in_implicits =
        add_loc sgn.in_implicits loc info (add_replace replace) }

  let add_output sgn loc info ~replace =
    { sgn with outputs =
        add_loc sgn.outputs loc info (add_replace replace) }

  let add_in_ctrl sgn info ~replace =
    let new_info = match sgn.in_ctrl with None -> info
      | Some old -> add_replace replace old info
    in { sgn with in_ctrl = Some new_info }

  let add_out_ret sgn info ~replace =
    let new_info = match sgn.out_ret with None -> info
      | Some old -> add_replace replace old info
    in { sgn with out_ret = Some new_info }

  let add_info sgn key info ~replace =
    match key with
      | In InCtrl -> add_in_ctrl sgn info replace
      | In (InNum n) -> add_input sgn n info replace
      | In (InImpl loc) -> add_impl_input sgn loc info replace
      | Out OutRet -> add_out_ret sgn info replace
      | Out (OutLoc k) -> add_output sgn k info replace

  let find_input sgn n =
    try
      assert (n <> 0); (* no input 0 : use find_in_ctrl *)
      List.assoc n sgn.in_params
    with Not_found ->
      raise Not_found

  let find_output sgn out_key =
    let rec find l = match l with
      | [] -> raise Not_found
      | (loc, e)::tl ->
          if Locations.Zone.equal out_key loc then e
          else find tl
    in
      find sgn.outputs

  let find_out_ret sgn = match sgn.out_ret with
    | Some i -> i
    | None -> raise Not_found

  let find_in_ctrl sgn = match sgn.in_ctrl with
    | Some i -> i
    | None -> raise Not_found

  (** try to find an exact match with loc.
  * we shouldn't try to find a zone that we don't have... *)
  let find_implicit_input sgn loc =
    let rec find l = match l with
      | [] -> raise Not_found
      | (in_loc, e)::tl ->
          if Locations.Zone.equal in_loc loc then e
          else find tl
    in
    find sgn.in_implicits

  let find_in_top sgn =
    find_implicit_input sgn Locations.Zone.top

  let find_in_info sgn in_key =
    match in_key with
    | InCtrl -> find_in_ctrl sgn
    | (InNum n) -> find_input sgn n
    | (InImpl loc) -> find_implicit_input sgn loc

  let find_out_info sgn out_key =
    match out_key with
    | OutRet -> find_out_ret sgn
    | (OutLoc k) -> find_output sgn k

  let find_info sgn key =
    match key with
    | In in_key -> find_in_info sgn in_key
    | Out out_key -> find_out_info sgn out_key

  let fold_outputs f acc sgn = List.fold_left f acc sgn.outputs

  let fold_all_outputs f acc sgn =
    let acc = match sgn.out_ret with
      | None -> acc
      | Some info -> f acc (OutRet, info)
    in
    List.fold_left (fun acc (k, i) -> f acc ((OutLoc k), i)) acc sgn.outputs

  let fold_num_inputs f acc sgn = List.fold_left f acc sgn.in_params

  let fold_impl_inputs f acc sgn = List.fold_left f acc sgn.in_implicits

  let fold_matching_impl_inputs loc f acc sgn =
    let test acc (in_loc, info) =
      if (Locations.Zone.intersects in_loc loc) then f acc (in_loc, info)
      else acc
    in List.fold_left test acc sgn.in_implicits

  let fold_all_inputs f acc sgn =
    let acc = match  sgn.in_ctrl with
      | None -> acc
      | Some info -> f acc (InCtrl, info) in
    let acc =
      fold_num_inputs (fun acc (n, info) -> f acc ((InNum n), info)) acc sgn
    in
    fold_impl_inputs (fun acc (l, info) -> f acc ((InImpl l), info)) acc sgn

  let fold f acc sgn =
    let acc =
      fold_all_inputs  (fun acc (n, info) -> f acc (In n, info)) acc sgn
    in
    fold_all_outputs (fun acc (n, info) -> f acc (Out n, info)) acc sgn

  let merge sgn1 sgn2 merge_info =
    let merge_elem lst (k, info) = add_in_list lst k info merge_info in
    let inputs = fold_num_inputs merge_elem sgn1.in_params sgn2 in
    let outputs = fold_outputs merge_elem sgn1.outputs sgn2 in
    let in_ctrl = match sgn1.in_ctrl, sgn2.in_ctrl with
      | None, _ -> sgn2.in_ctrl
      | _, None -> sgn1.in_ctrl
      | Some i1, Some i2 -> Some (merge_info i1 i2)
    in
    assert (sgn1.in_implicits = [] && sgn2.in_implicits = []);
    let out_ret = match sgn1.out_ret, sgn2.out_ret with
      | None, _ -> sgn2.out_ret
      | _, None -> sgn1.out_ret
      | Some i1, Some i2 -> Some (merge_info i1 i2)
    in
    { in_ctrl = in_ctrl; in_params = inputs ;
      in_implicits = [] ;
      out_ret = out_ret ; outputs = outputs }

  let pretty_in_key fmt key = match key with
    | (InNum n)  -> Format.fprintf fmt "In%d" n
    | InCtrl -> Format.fprintf fmt "InCtrl"
    | InImpl loc ->
        Format.fprintf fmt "@[<hv 1>In(%a)@]" Locations.Zone.pretty loc

  let pretty_out_key fmt key = match key with
    | OutRet -> Format.fprintf fmt "OutRet"
    | OutLoc loc ->
        Format.fprintf fmt "@[<hv 1>Out(%a)@]" Locations.Zone.pretty loc

  let pretty_key fmt key = match key with
    | In in_key  -> pretty_in_key fmt in_key
    | Out key -> pretty_out_key fmt key

  let pretty pp fmt sgn =
    let print _ (k,i) = 
      Format.fprintf fmt "@[<hv>(%a:@ %a)@]" pretty_key k pp i 
    in
    fold print () sgn

end

module Key = struct

  type key =
    | SigKey of Signature.key
        (** input/output nodes of the function *)
    | VarDecl of Cil_types.varinfo
        (** local, parameter or global variable definition *)
    | Stmt of Cil_types.stmt
        (** simple statement (not call) excluding its label (stmt.id) *)
    | CallStmt of Cil_types.stmt
        (** call statement *)
    | Label of stmt * Cil_types.label
        (** Labels are considered as function elements by themselves. *)
    | SigCallKey of Cil_types.stmt * Signature.key
        (** Key for an element of a call (input or output).
         * The call is identified by the statement. *)

  let entry_point = SigKey (Signature.in_ctrl_key)
  let top_input = SigKey (Signature.in_top_key)
  let param_key num_in = SigKey (Signature.in_key num_in)
  let implicit_in_key loc = SigKey (Signature.in_impl_key loc)
  let output_key = SigKey (Signature.out_ret_key)

  (** this is for the nodes inside undefined functions *)
  let out_from_key loc = SigKey (Signature.out_key loc)

  let decl_var_key var = VarDecl var
  let label_key label_stmt label = Label (label_stmt,label)
  let call_key call = CallStmt call
  let stmt_key stmt = if is_call_stmt stmt then call_key stmt else Stmt stmt

  let call_input_key call n = SigCallKey (call, (Signature.in_key n))
  let call_outret_key call = SigCallKey (call, (Signature.out_ret_key))
  let call_output_key call loc =
    SigCallKey (call, (Signature.out_key loc))
  let call_ctrl_key call = SigCallKey (call, (Signature.in_ctrl_key))
  let call_topin_key call = SigCallKey (call, (Signature.in_top_key))

  let call_from_id call_id = call_id

  let stmt key =
    match key with
      | SigCallKey (call, _) -> Some call
      | CallStmt call -> Some call
      | Stmt stmt -> Some stmt
      | Label (stmt, _) -> Some stmt
      | _ -> None

  (* see PrintPdg.pretty_key : can't be here because it uses Db... *)
  let pretty_node fmt k =
    let print_stmt fmt s =
      let str =
        match s.skind with
        | Switch (exp,_,_,_) | If (exp,_,_,_) ->
          Pretty_utils.to_string Printer.pp_exp exp
        | Loop _ -> "while(1)"
        | Block _ -> "block"
        | Goto _ | Break _ | Continue _ | Return _ | Instr _ ->
          Pretty_utils.sfprintf "@[<h 1>%a@]" 
	    (Printer.without_annot Printer.pp_stmt) s
        | UnspecifiedSequence _ -> "unspecified sequence"
        | TryExcept _ | TryFinally _  -> "ERROR"
      in
      Format.fprintf fmt "%s" str
    in
    match k with
    | CallStmt call ->
      let call = call_from_id call in
      Format.fprintf fmt "Call%d : %a" call.sid print_stmt call
    | Stmt s -> print_stmt fmt s
    | Label (_,l) -> Printer.pp_label fmt l
    | VarDecl v -> Format.fprintf fmt "VarDecl : %a" Printer.pp_varinfo v
    | SigKey k -> Signature.pretty_key fmt k
    | SigCallKey (call, sgn) ->
      let call = call_from_id call in
      Format.fprintf fmt "Call%d-%a : %a"
        call.sid Signature.pretty_key sgn print_stmt call

  include Datatype.Make
        (struct
          include Datatype.Serializable_undefined
          type t = key
          let name = "PdgIndex.Key"
          open Cil_datatype
          let reprs =
            List.fold_left
              (fun acc v ->
                List.fold_left
                  (fun acc s -> Stmt s :: acc)
                  (VarDecl v :: acc)
                  Stmt.reprs)
              []
              Varinfo.reprs
          open Structural_descr
          let structural_descr =
            let p_key = pack Signature.Str_descr.key in
            t_sum
              [|
                [| p_key |];
                [| Varinfo.packed_descr |];
                [| Stmt.packed_descr |];
                [| Cil_datatype.Stmt.packed_descr |];
                [| Cil_datatype.Stmt.packed_descr; Label.packed_descr |];
                [| Cil_datatype.Stmt.packed_descr; p_key |];
              |]
          let rehash = Datatype.identity
          let pretty = pretty_node
          let mem_project = Datatype.never_any_project
         end)

end

(* [Key] restricted to [Stmt], [VarDecl] and [Label] constructors. Hash tables
   are built upon this type, and we currently have no full hash/equality
   function for [Key.t]. *)
module RKey = struct

  include Key

  let hash = function
    | Key.VarDecl v -> 17 * Cil_datatype.Varinfo.hash v
    | Key.Stmt s -> 29 * Cil_datatype.Stmt.hash s
    | Key.Label (s, _l) ->
      (* Intentionnaly buggy: ignore the label and consider only the statement.
         There seems to be bug in the pdg, only one 'case :' per statement is
         present. This avoids removing the other 'case' clauses
         (see tests/slicing/switch.c *)
         53 * Cil_datatype.Stmt.hash s (* 7 * Cil_datatype.Label.hash l *)
    | _ -> assert false

  let equal k1 k2 = match k1, k2 with
    | Key.VarDecl v1, Key.VarDecl v2 -> Cil_datatype.Varinfo.equal v1 v2
    | Key.Stmt s1, Key.Stmt s2 -> Cil_datatype.Stmt.equal s1 s2
    | Key.Label (s1, _l1), Key.Label (s2, _l2) ->
        (* See [hash] above *)
      Cil_datatype.Stmt.equal s1 s2  (* && Cil_datatype.Label.equal l1 l2 *)
    | _ -> false
end

module H = struct
  include Hashtbl.Make(RKey)
  let structural_descr =
    Structural_descr.t_hashtbl_unchanged_hashs (Descr.str RKey.descr)
end

module FctIndex = struct
 
  type ('node_info, 'call_info) t = {
    (** inputs and ouputs of the function *)
    mutable sgn : 'node_info Signature.t ;
    (** calls signatures *)
    mutable calls : 
      (Cil_types.stmt * ('call_info option * 'node_info Signature.t)) list ;
    (** everything else *)
    other : 'node_info H.t
  }

  open Structural_descr
  let t_descr ~ni:d_ninfo ~ci:d_cinfo =
    t_record
      [| pack (Signature.Str_descr.t d_ninfo);
         pack (t_list (t_tuple [| Cil_datatype.Stmt.packed_descr;
                                  pack (t_tuple [|
                                          pack (t_option d_cinfo);
                                          pack (Signature.Str_descr.t d_ninfo);
                                        |])
                               |]));
         pack (H.structural_descr d_ninfo);
      |]

  let sgn idx = idx.sgn

  let create nb = { sgn = Signature.empty; calls = []; other = H.create nb }

  let copy idx = { sgn = Signature.copy idx.sgn;
                   calls = idx.calls;
                   other = H.copy idx.other }

  let merge_info_calls calls1 calls2 merge_a merge_b =
    let merge_info  (b1, sgn1) (b2, sgn2) =
      let b = match b1, b2 with None, _ -> b2 | _, None -> b1
        | Some b1, Some b2 -> Some (merge_b b1 b2)
      in let sgn = Signature.merge sgn1 sgn2 merge_a in
        (b, sgn)
    in
    let rec merge l1 l2 = match l1, l2 with
      | [], _ -> l2
      | _, [] -> l1
      | ((call1, info1) as c1) :: tl1,
        ((call2, info2) as c2) :: tl2 ->
          let id1 = call1.sid in
          let id2 = call2.sid in
            if id1 = id2 then
              let info = merge_info info1 info2 in
                (call1, info) :: (merge tl1 tl2)
            else if id1 < id2 then c1 :: (merge tl1 l2)
            else c2 :: (merge l1 tl2)
    in merge calls1 calls2

  let merge idx1 idx2 merge_a merge_b =
    let sgn = Signature.merge idx1.sgn idx2.sgn merge_a in
    let table = H.copy idx1.other in
    let add k a2 =
      let a =
        try let a1 = H.find table k in merge_a a1 a2
        with Not_found -> a2
      in H.replace table k a
    in H.iter add idx2.other;
    let calls = merge_info_calls idx1.calls idx2.calls merge_a merge_b in
      {sgn = sgn; calls = calls; other = table}

  let add_info_call idx call e ~replace =
    let sid = call.sid in
    let rec add l = match l with
      | [] -> [(call, (Some e, Signature.empty))]
      | ((call1, (_e1, sgn1)) as c1) :: tl ->
          let sid1 = call1.sid in
          if sid = sid1 then
            (if replace then (call, (Some e, sgn1)) :: tl else raise AddError)
          else if sid < sid1 then
            (call, (Some e, Signature.empty)) :: l
          else c1 :: (add tl)
    in idx.calls <- add idx.calls

  let add_info_call_key idx key =
    match key with
    | Key.CallStmt call -> add_info_call idx call
        | _ -> assert false

  let add_info_sig_call calls call k e replace =
    let new_sgn old = Signature.add_info old k e replace in
    let rec add l = match l with
      | [] -> [(call, (None, new_sgn Signature.empty))]
      | ((call1, (e1, sgn1)) as c1) :: tl ->
          let sid = call.sid in
          let sid1 = call1.sid in
          if sid = sid1
          then (call, (e1, new_sgn sgn1)) :: tl
          else if sid < sid1
          then (call, (None, new_sgn Signature.empty)) :: l
          else (c1 :: (add tl))
    in add calls

  let find_call idx call =
    let rec find l = match l with
      | [] ->  raise Not_found
      | (call1, e1) :: tl ->
          let sid = call.sid in
          let sid1 = call1.sid in
          if sid = sid1 then e1
          else if sid < sid1 then raise Not_found
          else find tl
    in
    find idx.calls

  let find_call_key idx key =
    match key with
    | Key.CallStmt call -> find_call idx call
        | _ -> assert false

  let find_info_call idx call =
    let (e1, _sgn1) = find_call idx call in
      match e1 with Some e -> e | None -> raise Not_found

  let find_info_call_key idx key =
    match key with
    | Key.CallStmt call -> find_info_call idx call
    | _ -> assert false

  let find_info_sig_call idx call k =
    let (_e1, sgn1) = find_call idx call in
      Signature.find_info sgn1 k

  let find_all_info_sig_call idx call =
      let (_e1, sgn1) = find_call idx call in
      Signature.fold (fun l (_k,i) -> i::l) [] sgn1

  let add_replace idx key e replace =
    let hfct = if replace then H.replace else H.add in
    match key with
      | Key.SigKey k ->
          idx.sgn <- Signature.add_info idx.sgn k e replace
      | Key.CallStmt _ -> raise CallStatement (* see add_info_call *)
      | Key.SigCallKey (call, k) ->
          idx.calls <- add_info_sig_call idx.calls call k e replace
      | Key.VarDecl _ | Key.Stmt _ | Key.Label _ -> hfct idx.other key e

  let add idx key e = add_replace idx key e false

  let add_or_replace idx key e = add_replace idx key e true

  let length idx = H.length idx.other

  let find_info idx key =
    match key with
      | Key.SigKey k -> Signature.find_info idx.sgn k
      | Key.CallStmt _ -> raise CallStatement (* see find_info_call *)
      | Key.SigCallKey (call, k) -> find_info_sig_call idx call k
      | Key.VarDecl _ | Key.Stmt _ | Key.Label _ ->
          (try H.find idx.other key
           with Not_found -> raise Not_found)

  let find_all idx key =
    match key with
      | Key.CallStmt call -> find_all_info_sig_call idx call
      | _ -> let info = find_info idx key in [info]

  let find_label idx lab =
    let collect k info res = match k with
      | Key.Label (_,k_lab) -> 
          if Cil_datatype.Label.equal k_lab lab then  info :: res else res
      | _ -> res
    in
    let infos = H.fold collect idx.other [] in
      match infos with
          info :: [] -> info | [] -> raise Not_found | _ -> assert false

  let fold_calls f idx acc =
    let process acc (call, (_i, _sgn as i_sgn)) = f call i_sgn acc in
    List.fold_left process acc idx.calls

  let fold f idx acc =
    let acc = Signature.fold 
                (fun acc (k, info) -> f (Key.SigKey k) info acc) 
                acc idx.sgn in
    let acc = H.fold (fun k info acc -> f k info acc) idx.other acc in
    List.fold_left 
      (fun acc (call, (_, sgn)) -> 
        Signature.fold (fun acc (k, info) -> 
          f (Key.SigCallKey (call, k)) info acc) 
          acc sgn) 
      acc idx.calls

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

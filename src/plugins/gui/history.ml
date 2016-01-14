(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

type history_elt =
  | Global of global
  | Localizable of Pretty_source.localizable

module HistoryElt = struct
  include Datatype.Make
    (struct
       include Datatype.Undefined
       type t = history_elt
       let name = "History.history_elt"
       let reprs = List.map (fun g -> Global g) Cil_datatype.Global.reprs
       let mem_project = Datatype.never_any_project
       let equal e1 e2 = match e1, e2 with
         | Global g1, Global g2 -> Cil_datatype.Global.equal g1 g2
         | Localizable l1, Localizable l2 ->
             Pretty_source.Localizable.equal l1 l2
         | (Global _ | Localizable _), __ -> false
     end)
  (* Identify two elements that belong to the same function *)
  let in_same_fun e1 e2 =
    let f = function
      | Global (GFunDecl (_, vi, _) | GFun ({svar = vi}, _)) ->
          (try Some (Globals.Functions.get vi)
           with Not_found -> None)
      | Localizable l ->
          Pretty_source.kf_of_localizable l
      | _ -> None
    in
    match f e1 with
      | None -> false
      | Some f1 -> match f e2 with
          | None -> false
          | Some f2 -> Kernel_function.equal f1 f2
end

type history = {
  back: history_elt list;
  current: history_elt option;
  forward: history_elt list;
}

let default_history = {
  back = [];
  current = None;
  forward = [];
}

module History =
  Datatype.Make
    (struct
       include Datatype.Undefined
       type t = history
       let name = "History.history"
       let reprs = [default_history]
       let mem_project = Datatype.never_any_project
       let pretty fmt h =
         Format.fprintf fmt "back %d, cur %b, forward %d"
           (List.length h.back) (h.current <> None) (List.length h.forward)
     end)

include History

module CurrentHistory =
  State_builder.Ref
    (History)
    (struct
       let name = "History.CurrentHistory"
       let dependencies = [Ast.self]
       let default _ = default_history
     end)

(* This is correct because the implementation makes sur that [.current = None]
   implies [.forward = [] && .back = []] *)
let is_empty () = (CurrentHistory.get ()).current = None
let can_go_back () = (CurrentHistory.get ()).back <> []
let can_go_forward () = (CurrentHistory.get ()).forward <> []

let display_elt = ref (fun _ -> ())
let set_display_elt_callback f = display_elt := f

let show_current () =
  let h = CurrentHistory.get () in
  Extlib.may !display_elt h.current;
  CurrentHistory.set h

let back () =
  let h = CurrentHistory.get () in
  match h.current, h.back with
    | Some cur, prev :: prevs ->
        let h' = {back = prevs; current = Some prev; forward= cur::h.forward} in
        !display_elt prev;
        CurrentHistory.set h'

    | None, prev :: prevs ->
        let h' = { back = prevs; current = Some prev ; forward = h.forward } in
        !display_elt prev;
        CurrentHistory.set h'

    | _, [] -> ()

let forward () =
  let h = CurrentHistory.get () in
  match h.current, h.forward with
    | Some cur, next :: nexts ->
        let h' = { back = cur::h.back; current = Some next; forward = nexts} in
        !display_elt next;
        CurrentHistory.set h'

    | None, next :: nexts ->
        let h' = { back = h.back; current = Some next; forward = nexts } in
        !display_elt next;
        CurrentHistory.set h'

    | _, [] -> ()

let on_current_history () =
  let h = CurrentHistory.get () in
  fun f -> CurrentHistory.set h; f ()

let get_current () = (CurrentHistory.get ()).current

let push cur =
  let h = CurrentHistory.get () in
  let h' = match h.current with
    | None -> { back = h.back; current = Some cur; forward = [] }
    | Some prev ->
        if HistoryElt.equal cur prev
        then h
        else if HistoryElt.in_same_fun cur prev then
          { h with current = Some cur }
        else
          { back = prev :: h.back; current = Some cur; forward = [] }
  in
  CurrentHistory.set h'

let set_forward els =
  let h = CurrentHistory.get () in
  let h' = { h with forward = els } in
  CurrentHistory.set h'

let selected_localizable () =
  match (CurrentHistory.get ()).current with
    | None | Some (Global _) -> None
    | Some (Localizable loc) -> Some loc

let create_buttons (menu_manager : Menu_manager.menu_manager) =
  let refresh = menu_manager#refresh in
  menu_manager#add_plugin ~title:"Navigation"
     [
       Menu_manager.toolmenubar
         ~sensitive:can_go_back ~icon:`GO_BACK
         ~label:"Back" ~tooltip:"Go to previous visited source location"
         (Menu_manager.Unit_callback (fun () -> back (); refresh ()));
       Menu_manager.toolmenubar
         ~sensitive:can_go_forward ~icon:`GO_FORWARD
         ~label:"Forward" ~tooltip:"Go to next visited source location"
         (Menu_manager.Unit_callback (fun () -> forward (); refresh ()));
     ]


exception Found_global of global

(* We build a 'fake' global for [kf], so as to be able to search for it
   in the AST. Do not use Kernel_function.get_global, as [kf] is no longer
   in the proper project when kf_to_global is called, which leads to crashes. *)
let kf_to_global kf = match kf.fundec with
  | Definition (d, loc) -> GFun(d,loc)
  | Declaration (spec, vi, _, loc) -> GFunDecl(spec, vi,loc)

let translate_history_elt old_helt =
  let test_name_file old_name new_name old_loc new_loc =
    old_name = new_name &&
    (fst old_loc).Lexing.pos_fname = (fst new_loc).Lexing.pos_fname
  in
  let global old_g =
    let iter new_g =
      let open Cil_types in
      (** In the same file, same constructor and same original name *)
      match old_g,
            new_g with
      | (GType(                      {torig_name = old_name},          old_loc),
         GType(                      {torig_name = new_name},          new_loc))
      | (GEnumTag(                   {eorig_name = old_name},          old_loc),
         GEnumTag(                   {eorig_name = new_name},          new_loc))
      | (GEnumTagDecl(               {eorig_name = old_name},          old_loc),
         GEnumTagDecl(               {eorig_name = new_name},          new_loc))
      | (GCompTag(                   {corig_name = old_name},          old_loc),
         GCompTag(                   {corig_name = new_name},          new_loc))
      | (GCompTagDecl(               {corig_name = old_name},          old_loc),
         GCompTagDecl(               {corig_name = new_name},          new_loc))
      | (GVarDecl(                     {vorig_name = old_name},          old_loc),
         GVarDecl(                     {vorig_name = new_name},          new_loc))
      | (GFunDecl(_,                 {vorig_name = old_name},          old_loc),
         GFunDecl(_,                 {vorig_name = new_name},          new_loc))
      | (GVar(                       {vorig_name = old_name},_,        old_loc),
         GVar(                       {vorig_name = new_name},_,        new_loc))
      | (GFun({svar=                 {vorig_name = old_name}},         old_loc),
         GFun({svar=                 {vorig_name = new_name}},         new_loc))
      | (GAnnot(Dtype(                  {lt_name = old_name},_),       old_loc),
         GAnnot(Dtype(                  {lt_name = new_name},_),       new_loc))
      | (GAnnot(Daxiomatic(                        old_name,_,_),      old_loc),
         GAnnot(Daxiomatic(                        new_name,_,_),      new_loc))
      | (GAnnot(Dlemma(                            old_name,_,_,_,_,_),old_loc),
         GAnnot(Dlemma(                            new_name,_,_,_,_,_),new_loc))
      | (GAnnot(Dfun_or_pred({l_var_info= {lv_name=old_name}},_),      old_loc),
         GAnnot(Dfun_or_pred({l_var_info= {lv_name=new_name}},_),      new_loc))

        when test_name_file old_name new_name old_loc new_loc ->
        raise (Found_global new_g)

      | GAsm _, GAsm _
      | GText _, GText _
      | GPragma _, GPragma _
      | GAnnot(Dvolatile _,_),     GAnnot(Dvolatile _,_)
      | GAnnot(Dinvariant _,_),    GAnnot(Dinvariant _,_)
      | GAnnot(Dtype_annot _,_),   GAnnot(Dtype_annot _,_)
      | GAnnot(Dmodel_annot _,_),  GAnnot(Dmodel_annot _,_)
      | GAnnot(Dcustom_annot _,_), GAnnot(Dcustom_annot _,_)
        -> (** they have no names *) ()
      | _ -> (** different constructors *) ()
    in
    try
      List.iter iter (Ast.get ()).globals;
      None
    with Found_global new_g -> Some new_g
  in
  let open Pretty_source in
  let open Cil_datatype in
  let global_Global g = Extlib.opt_map (fun x -> Global x) (global g) in
  match old_helt with
  | Global old_g -> global_Global old_g
  | Localizable (PGlobal old_g) -> global_Global old_g
  | Localizable(PVDecl(Some kf,_)) ->
    global_Global (kf_to_global kf)
  | Localizable ( PStmt(kf,_) | PLval(Some kf,_,_) | PExp(Some kf,_,_)
                | PTermLval(Some kf,_,_,_) as loc) ->
    begin match global (kf_to_global kf) with
    | None ->
      (** The kernel function can't be found nothing to say *)
      None
    | Some g ->
      (** Try to stay at the same offset in the function *)
      let old_kf_loc = fst (Kernel_function.get_location kf) in
      let old_loc = match ki_of_localizable loc with
        | Kstmt s -> fst (Stmt.loc s)
        | Kglobal -> (* fallback *) old_kf_loc
      in
      let offset = old_loc.Lexing.pos_lnum - old_kf_loc.Lexing.pos_lnum in
      let new_kf_loc = fst (Global.loc g) in
      let new_loc = {new_kf_loc with
                     Lexing.pos_lnum = new_kf_loc.Lexing.pos_lnum + offset;
                     Lexing.pos_cnum = old_loc.Lexing.pos_cnum;
                    }
      in
      match Pretty_source.loc_to_localizable new_loc with
      | None -> (** the line is unknown *)
        Some (Global g)
      | Some locali ->
        begin match kf_of_localizable locali with
          | None -> (** not in a kf so return the start of the function *)
            Some (Global g)
          | Some kf when not (Global.equal (kf_to_global kf) g) ->
            (** Fall in the wrong global, so return the start of the function *)
            Some (Global g)
          | _ ->
            (** Fall in the correct global *)
            Some (Localizable locali)
      end
    end
  | Localizable (PLval(None,_,_) | PExp(None,_,_) | PTermLval(None,_,_,_)
                | PVDecl(None,_)) -> (** no names useful? *) None
  | Localizable (PIP _ ) -> (** no names available *) None

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

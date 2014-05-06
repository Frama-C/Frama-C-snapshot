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
      | Global (GVarDecl (_, vi, _) | GFun ({svar = vi}, _)) ->
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

let apply_on_selected f =
  match (CurrentHistory.get ()).current with
    | None | Some (Global _) -> ()
    | Some (Localizable loc) -> f loc

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


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

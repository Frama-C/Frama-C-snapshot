(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(* -------------------------------------------------------------------------- *)
(* --- PO List View                                                       --- *)
(* -------------------------------------------------------------------------- *)

open Wpo

module Windex = Indexer.Make(Wpo.S)

class model =
  object(self)
    val mutable index = Windex.empty
    method reload = index <- Windex.empty
    method add w = index <- Windex.add w index
    method size = Windex.size index
    method index w = Windex.index w index
    method get k = Windex.get k index
    method coerce = (self :> Wpo.t Wtable.listmodel)
  end

let render_prover_result p =
  let icn_stock name = [`STOCK_ID name] in
  let icn_status s = [`PIXBUF(Gtk_helper.Icon.get (Gtk_helper.Icon.Feedback s))] in
  let icn_na      = [`PIXBUF(Gtk_helper.Icon.get Gtk_helper.Icon.Unmark)] in
  let icn_none    = icn_stock "" in
  let icn_valid   = icn_status Property_status.Feedback.Valid in
  let icn_unknown = icn_status Property_status.Feedback.Unknown in
  let icn_invalid = icn_status Property_status.Feedback.Invalid in
  let icn_failed  = icn_stock "gtk-dialog-warning" in
  let icn_cut     = icn_stock "gtk-cut" in
  let icn_running = icn_stock "gtk-execute" in
  let open VCS in
  let icon_of_verdict = function
    | Checked | NoResult -> icn_none
    | Valid    -> icn_valid
    | Invalid  -> icn_invalid
    | Unknown  -> icn_unknown
    | Failed   -> icn_failed
    | Timeout | Stepout -> icn_cut
    | Computing _ -> icn_running
  in fun w ->
    match Wpo.get_result w p , p with
    | { verdict=NoResult } , Qed -> icn_na
    | { verdict=NoResult } , Tactical ->
        begin
          match ProverScript.get w with
          | `None -> icn_na
          | `Script -> icn_stock "gtk-media-play"
          | `Proof -> icn_stock "gtk-edit"
          | `Saved -> icn_stock "gtk-file"
        end
    | { verdict=r } , _ -> icon_of_verdict r

class pane (gprovers:GuiConfig.provers) =
  let model = new model in
  let list = new Wtable.list ~headers:true ~rules:true model#coerce in
  object(self)

    method coerce = list#coerce
    method reload = list#reload

    method add wpo =
      begin
        model#add wpo ;
        list#insert_row wpo ;
      end

    method size = model#size
    method index = model#index
    method get = model#get

    method update_all = list#update_all
    method update w = list#update_row w

    (* -------------------------------------------------------------------------- *)
    (* --- Prover Columns Management                                          --- *)
    (* -------------------------------------------------------------------------- *)

    val mutable provers : (VCS.prover * GTree.view_column) list = []

    method private prover_of_column c =
      let id = c#misc#get_oid in
      try Some(fst(List.find (fun (_,c0) -> id = c0#misc#get_oid) provers))
      with Not_found -> None

    method private column_of_prover p =
      try Some(snd(List.find (fun (p0,_) -> p=p0) provers))
      with Not_found -> None

    method private create_prover p =
      begin
        let title = VCS.title_of_prover p in
        let column = list#add_column_pixbuf ~title [] (render_prover_result p)
        in if p <> VCS.Qed then provers <- (p,column) :: provers
      end

    method private configure (dps:Why3.Whyconf.Sprover.t) =
      begin
        (* Removing Useless Columns *)
        List.iter
          (fun (vcs,column) ->
             match vcs with
             | VCS.Why3 p ->
                 column#set_visible (Why3.Whyconf.Sprover.mem p dps) ;
                 (* ignore (list#view#remove_column column) *)
             | _ -> ()
          ) provers ;
        (* Installing Missing Columns *)
        Why3.Whyconf.Sprover.iter
          (fun dp ->
             let prv = VCS.Why3 dp in
             match self#column_of_prover prv with
             | None -> self#create_prover prv
             | Some _ -> ()
          ) dps ;
      end

    initializer
      begin
        let render w =
          [`TEXT (Pretty_utils.to_string Wpo.pp_index w.po_idx)] in
        ignore (list#add_column_text ~title:"Module" [] render) ;
        let render w =
          [`TEXT (Pretty_utils.to_string Wpo.pp_title w)] in
        ignore (list#add_column_text ~title:"Goal" [] render) ;
        let render w = [`TEXT (Wpo.get_model w |> WpContext.MODEL.descr)] in
        ignore (list#add_column_text ~title:"Model" [] render) ;
        List.iter
          self#create_prover
          [ VCS.Qed ; VCS.Tactical ] ;
        let prv = Wp_parameters.Provers.get () in
        if List.mem "native:alt-ergo" prv then
          self#create_prover VCS.NativeAltErgo ;
        if List.mem "native:coq" prv then
          self#create_prover VCS.NativeCoq ;
        ignore (list#add_column_empty) ;
        list#set_selection_mode `MULTIPLE ;
        gprovers#connect self#configure ;
        self#configure gprovers#get ;
      end

    method private on_cell f w c = f w (self#prover_of_column c)

    method on_click f = list#on_click (self#on_cell f)
    method on_double_click f = list#on_double_click (self#on_cell f)
    method on_right_click f = list#on_right_click (self#on_cell f)
    method on_selection f = list#on_selection (fun () -> f list#count_selected)
    method iter_selected = list#iter_selected
    method count_selected = list#count_selected

    method show w =
      let col = list#view#get_column 1 in
      list#set_focus w col

  end

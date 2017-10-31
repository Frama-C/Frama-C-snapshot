(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

type state =
  | Empty
  | Proof of ProofEngine.tree
  | Forking of ProofEngine.tree * ProofEngine.fork * Task.pool
  | Composer of ProofEngine.tree * GuiTactic.composer * GuiSequent.target
  | Browser of ProofEngine.tree * GuiTactic.browser * GuiSequent.target

(* -------------------------------------------------------------------------- *)
(* --- Autofocus Management                                               --- *)
(* -------------------------------------------------------------------------- *)

type mode = [ `Refresh | `Autofocus | `ViewModel | `ViewAll | `ViewRaw ]

module Config = Gtk_helper.Configuration

class autofocus =
  let options = [
    `Refresh , "Refresh" ;
    `Autofocus , "Autofocus" ;
    `ViewAll , "Full Context" ;
    `ViewModel , "Unmangled Memory" ;
    `ViewRaw , "Raw Obligation" ;
  ] in
  let values = [
    `Refresh , "REFRESH" ;
    `Autofocus , "AUTOFOCUS" ;
    `ViewAll , "VIEW_ALL" ;
    `ViewModel , "VIEW_MODEL" ;
    `ViewRaw , "VIEW_RAW" ;
  ] in
  object(self)
    inherit [mode] Widget.menu ~default:`Autofocus ~options ()
    initializer
      Wutil.later
        begin fun () ->
          Config.config_values
            ~key:"GuiGoal.autofocus"
            ~default:`Autofocus ~values self
        end
  end

(* -------------------------------------------------------------------------- *)
(* --- Goal Panel                                                         --- *)
(* -------------------------------------------------------------------------- *)

class pane (proverpane : GuiConfig.provers) =
  let icon = new Widget.image GuiProver.no_status in
  let status = new Widget.label () in
  let text = new Wtext.text () in
  let scripter = new GuiProof.printer text in
  let printer = new GuiSequent.focused text in
  let composer = new GuiComposer.composer printer in
  let browser = new GuiComposer.browser printer in
  let layout = new Wutil.layout in
  let palette = new Wpalette.panel () in
  let help = new Widget.button
    ~label:"Tactics" ~border:false ~tooltip:"List Available Tactics" () in
  let delete = new Widget.button
    ~icon:`DELETE ~tooltip:"Delete current proof" () in
  let cancel = new Widget.button
    ~icon:`UNDO ~tooltip:"Undo Proof Steps" () in
  let forward = new Widget.button
    ~icon:`MEDIA_FORWARD ~tooltip:"Go ahead among pending goals" () in
  let next = new Widget.button
    ~icon:`MEDIA_NEXT ~tooltip:"Goto next pending goal" () in
  let prev = new Widget.button
    ~icon:`MEDIA_PREVIOUS ~tooltip:"Goto previous pending goal" () in
  let play_script = new Widget.button
    ~icon:`REVERT_TO_SAVED ~tooltip:"Replay Session Script" () in
  let save_script = new Widget.button
    ~icon:`SAVE ~tooltip:"Save Script" () in
  let autofocus = new autofocus in
  let strategies = new GuiTactic.strategies () in
  object(self)

    val mutable state : state = Empty
    val mutable provers : GuiProver.prover list = []
    val mutable tactics : GuiTactic.tactic list = []

    initializer
      begin
        let toolbar =
          Wbox.(toolbar
                  [ w prev ; w next ; w cancel ; w forward ;
                    w autofocus ; w play_script ; w save_script ;
                    w ~padding:6 icon ; h ~padding:6 status ]
                  [ w help ; w delete ]) in
        layout#populate (Wbox.panel ~top:toolbar ~right:palette#widget text) ;
        provers <-
          VCS.([ new GuiProver.prover ~console:text ~prover:AltErgo ] @
               List.map
                 (fun dp -> new GuiProver.prover text (ProverWhy3.prover dp))
                 proverpane#get) ;
        List.iter (fun p -> palette#add_tool p#tool) provers ;
        palette#add_tool strategies#tool ;
        Strategy.iter strategies#register ;
        Tactical.iter
          (fun tac ->
             let gtac = new GuiTactic.tactic tac printer#pp_selection in
             tactics <- gtac :: tactics ;
             palette#add_tool gtac#tool) ;
        tactics <- List.rev tactics ;
        self#register_provers proverpane#get ;
        printer#on_selection (fun () -> self#update) ;
        scripter#on_click self#goto ;
        scripter#on_backtrack self#backtrack ;
        proverpane#connect self#register_provers ;
        delete#connect (fun () -> self#interrupt ProofEngine.reset) ;
        cancel#connect (fun () -> self#interrupt ProofEngine.cancel) ;
        forward#connect (fun () -> self#forward) ;
        next#connect (fun () -> self#navigate succ) ;
        prev#connect (fun () -> self#navigate pred) ;
        save_script#connect (fun () -> self#save_script) ;
        play_script#connect (fun () -> self#play_script) ;
        autofocus#connect self#autofocus ;
        composer#connect (fun () -> self#update) ;
        browser#connect (fun () -> self#update) ;
        help#connect (fun () -> self#open_help) ;
      end

    (* ---------------------------------------------------------------------- *)
    (* --- Behavior                                                       --- *)
    (* ---------------------------------------------------------------------- *)

    val mutable helpmode = false
    method private open_help =
      helpmode <- true ;
      self#update

    method private quit_help =
      helpmode <- false ;
      self#update

    method private compose cc =
      match state with
      | Proof proof ->
          composer#clear ;
          let tgt = printer#unselect in
          state <- Composer(proof,cc,tgt) ;
          self#update
      | _ -> ()

    method private browse cc =
      match state with
      | Proof proof ->
          browser#clear ;
          let tgt = printer#unselect in
          state <- Browser(proof,cc,tgt) ;
          self#update
      | _ -> ()

    method private interrupt cancel =
      match state with
      | Empty -> ()
      | Proof proof | Composer(proof,_,_) | Browser(proof,_,_) ->
          cancel proof ;
          printer#reset ;
          self#update
      | Forking (proof,_,pool) ->
          cancel proof ;
          Task.iter Task.cancel pool ;
          state <- Proof proof ;
          printer#reset ;
          self#update

    method private forward =
      match state with
      | Empty | Forking _ | Composer _ | Browser _ -> ()
      | Proof p -> ProofEngine.forward p ; self#update

    method private goto s =
      match state with
      | Empty | Forking _ | Composer _ | Browser _ -> ()
      | Proof p -> ProofEngine.goto p s ; self#update

    method private navigate f =
      match state with
      | Empty | Forking _ | Composer _ | Browser _ -> ()
      | Proof p ->
          match ProofEngine.current p with
          | `Leaf (k,_) -> ProofEngine.goto p (`Leaf(f k)) ; self#update
          | `Main | `Internal _ -> ()

    method private autofocus = function
      | `Autofocus ->
          printer#set_focus_mode true ;
          printer#set_state_mode true ;
          self#update
      | `ViewRaw ->
          printer#set_focus_mode false ;
          printer#set_state_mode false ;
          self#update
      | `ViewModel ->
          printer#set_focus_mode true ;
          printer#set_state_mode false ;
          self#update
      | `ViewAll ->
          printer#set_focus_mode false ;
          printer#set_state_mode true ;
          self#update
      | `Refresh ->
          helpmode <- false ;
          printer#reset ;
          let mode =
            match printer#get_focus_mode , printer#get_state_mode with
            | true , true -> `Autofocus
            | false , false -> `ViewRaw
            | true , false -> `ViewModel
            | false , true -> `ViewAll
          in
          autofocus#set mode ; self#update

    method private play_script =
      match state with
      | Proof p ->
          ProofEngine.reset p ;
          ProverScript.spawn
            ~callback:
              (fun wpo prv res ->
                 text#printf "[%a] %s : %a@."
                   VCS.pp_prover prv Wpo.(wpo.po_name) VCS.pp_result res)
            ~success:
              (fun _ _ ->
                 ProofEngine.forward p ;
                 self#update)
            (ProofEngine.main p) ;
          let server = ProverTask.server () in
          Task.launch server
      | Empty | Forking _ | Composer _ | Browser _ -> ()

    method private save_script =
      match state with
      | Proof p ->
          let main = ProofEngine.main p in
          let json = ProofScript.encode (ProofEngine.script p) in
          ProofSession.save main json ;
          ProofEngine.set_saved p true ;
          self#update
          (*
          text#clear ;
          text#printf "@{<bf>Session:@} '%a'@." ProofSession.pretty main ;
          text#printf "@[<hov 2>@{<bf>Script:@}@ %a@]@." Json.pp json ;
          self#update_statusbar ;
          *)
      | Empty | Forking _ | Composer _ | Browser _ -> ()

    (* ---------------------------------------------------------------------- *)
    (* --- Prover Controllers                                             --- *)
    (* ---------------------------------------------------------------------- *)

    method private register_provers dps =
      begin
        (* register missing provers *)
        let prvs = List.map ProverWhy3.prover dps in
        (* set visible provers *)
        List.iter
          (fun prover ->
             let prv = prover#prover in
             match prover#prover with
             | VCS.Why3 _ -> prover#set_visible (List.mem prv prvs)
             | _ -> ()
          ) provers ;
        (* add missing provers *)
        List.iter
          (fun prv ->
             if List.for_all (fun p -> p#prover <> prv) provers then
               begin
                 let prover = new GuiProver.prover ~console:text ~prover:prv in
                 begin match state with
                   | Proof p -> prover#update (ProofEngine.main p)
                   | Empty | Forking _ | Composer _ | Browser _ -> prover#clear
                 end ;
                 provers <- provers @ [ prover ] ;
                 palette#add_tool prover#tool ;
               end
          ) prvs ;
      end

    (* ---------------------------------------------------------------------- *)
    (* --- External API                                                   --- *)
    (* ---------------------------------------------------------------------- *)

    method select = function
      | None ->
          state <- Empty ; self#update
      | Some w ->
          let pw = ProofEngine.proof ~main:w in
          let changed = match state with
            | Empty -> true
            | Proof p -> p != pw
            | Forking _ | Composer _ | Browser _ -> false
          in
          if changed then
            begin
              printer#reset ;
              self#update_provers None ;
              self#update_tactics None ;
              state <- Proof pw ;
              self#update ;
            end

    (* ---------------------------------------------------------------------- *)
    (* --- Repaint                                                        --- *)
    (* ---------------------------------------------------------------------- *)

    method coerce = layout#coerce

    method private update_provers = function
      | None ->
          List.iter (fun prover -> prover#clear) provers
      | Some wpo ->
          List.iter (fun prover -> prover#update wpo) provers

    method private update_tactics = function
      | None ->
          printer#set_target Tactical.Empty ;
          strategies#connect None ;
          List.iter (fun tactic -> tactic#clear) tactics
      | Some(model,sequent,sel) ->
          strategies#connect (Some (self#strategies sequent)) ;
          let select (tactic : GuiTactic.tactic) =
            let process = self#apply in
            let composer = Model.with_model model self#compose in
            let browser = Model.with_model model self#browse in
            tactic#select ~process ~composer ~browser sel
          in
          Model.with_model model (List.iter select) tactics ;
          let tgt =
            if List.exists (fun tactics -> tactics#targeted) tactics
            then sel else Tactical.Empty in
          printer#set_target tgt

    method private update_scriptbar =
      match state with
      | Empty | Forking _ ->
          begin
            save_script#set_enabled false ;
            play_script#set_enabled false ;
          end
      | Proof proof | Composer(proof,_,_) | Browser(proof,_,_) ->
          begin
            let main = ProofEngine.main proof in
            let play = ProofSession.exists main in
            let save = not (ProofEngine.saved proof) in
            play_script#set_enabled play ;
            save_script#set_enabled save ;
          end

    method private update_statusbar =
      match state with
      | Empty ->
          begin
            icon#set_icon GuiProver.no_status ;
            next#set_enabled false ;
            prev#set_enabled false ;
            cancel#set_enabled false ;
            delete#set_enabled false ;
            forward#set_enabled false ;
            status#set_text "No Status" ;
            help#set_enabled false ;
          end
      | Proof proof | Forking(proof,_,_)
      | Composer(proof,_,_) | Browser(proof,_,_) ->
          begin
            delete#set_enabled true ;
            help#set_enabled
              (match state with Proof _ -> not helpmode | _ -> false) ;
            match ProofEngine.status proof with
            | `Main ->
                icon#set_icon GuiProver.ko_status ;
                next#set_enabled false ;
                prev#set_enabled false ;
                cancel#set_enabled false ;
                forward#set_enabled false ;
                status#set_text "Non Proved Property" ;
            | `Proved ->
                icon#set_icon GuiProver.ok_status ;
                next#set_enabled false ;
                prev#set_enabled false ;
                cancel#set_enabled false ;
                forward#set_enabled false ;
                status#set_text "Proved Goal" ;
            | `Pending 0 ->
                icon#set_icon GuiProver.ok_status ;
                next#set_enabled false ;
                prev#set_enabled false ;
                forward#set_enabled false ;
                cancel#set_enabled true ;
                status#set_text "Proof Terminated" ;
            | `Pending n ->
                icon#set_icon GuiProver.ko_status ;
                forward#set_enabled true ;
                cancel#set_enabled true ;
                match ProofEngine.current proof with
                | `Main | `Internal _ ->
                    next#set_enabled false ;
                    prev#set_enabled false ;
                    if n = 1 then
                      Pretty_utils.ksfprintf status#set_text "One Pending Goal"
                    else
                      Pretty_utils.ksfprintf status#set_text "%d Pending Goals" n
                | `Leaf(k,_) ->
                    prev#set_enabled (0 < k) ;
                    next#set_enabled (k+1 < n) ;
                    if k = 0 && n = 1 then
                      Pretty_utils.ksfprintf status#set_text
                        "Last Pending Goal"
                    else
                      Pretty_utils.ksfprintf status#set_text
                        "%d/%d Pending Goals" (succ k) n
          end

    method private update_tacticbar =
      match state with
      | Empty | Forking _ ->
          self#update_provers None ;
          self#update_tactics None ;
      | Proof proof ->
          let wpo = ProofEngine.head proof in
          if Wpo.is_trivial wpo then
            begin
              self#update_provers None ;
              self#update_tactics None ;
            end
          else
            begin
              self#update_provers (Some wpo) ;
              let sequent = printer#sequent in
              let select = printer#selection in
              let model = wpo.Wpo.po_model in
              self#update_tactics (Some(model,sequent,select)) ;
            end
      | Composer _ | Browser _ -> ()

    method private update_proofview =
      match state with
      | Empty -> text#clear
      | Proof _ when helpmode ->
          begin
            text#clear ;
            let callback () = self#quit_help in
            text#printf "@\n@{<bf>Available Tactics:@} %t@\n@\n"
              (printer#button ~title:"Close" ~callback) ;
            text#hrule ;
            let pp_item pp fmt tac =
              Format.fprintf fmt "[ @{<bf>%a@} ] @{<it>%s@}@\n"
                pp tac#title tac#descr in
            Pretty_utils.pp_items
              ~title:(fun tac -> tac#title)
              ~iter:Tactical.iter
              ~pp_item text#fmt ;
            text#hrule ;
          end
      | Proof proof ->
          begin
            text#clear ;
            scripter#tree proof ;
            text#hrule ;
            text#printf "%t@." (printer#goal (ProofEngine.head proof)) ;
            text#hrule ;
            scripter#status proof ;
          end
      | Composer(proof,cc,tgt) ->
          begin
            text#clear ;
            let quit () =
              state <- Proof proof ;
              printer#restore tgt ;
              self#update in
            text#printf "%t@." (composer#print cc ~quit) ;
            text#hrule ;
            text#printf "%t@."
              (printer#goal (ProofEngine.head proof)) ;
          end
      | Browser(proof,cc,tgt) ->
          begin
            text#clear ;
            let quit () =
              state <- Proof proof ;
              printer#restore tgt ;
              self#update in
            text#printf "%t@." (browser#print cc ~quit) ;
            text#hrule ;
            text#printf "%t@."
              (printer#goal (ProofEngine.head proof)) ;
          end
      | Forking _ -> ()

    method update =
      begin
        self#update_statusbar ;
        self#update_proofview ;
        self#update_scriptbar ;
        self#update_tacticbar ;
      end

    (* ---------------------------------------------------------------------- *)
    (* --- Splitter                                                       --- *)
    (* ---------------------------------------------------------------------- *)

    method private commit () =
      match state with
      | Empty | Proof _ | Composer _ | Browser _ -> ()
      | Forking(proof,fork,pool) ->
          let n = Task.size pool in
          if n = 0 then
            begin
              ignore (ProofEngine.commit fork) ;
              ProofEngine.validate proof ;
              ProofEngine.forward proof ;
              state <- Proof proof ;
              printer#reset ;
              self#update ;
            end

    method private schedule pool provers goal =
      if provers = [] then
        self#commit ()
      else
        Prover.spawn goal
          ~callback:
            begin fun wpo prv res ->
              text#printf "[%a] %a : %a@."
                VCS.pp_prover prv Wpo.pp_title wpo VCS.pp_result res
            end
          ~success:(fun _ _ -> Wutil.later self#commit)
          ~pool provers

    method private fork proof fork =
      Wutil.later
        begin fun () ->
          let provers = VCS.[ BatchMode, AltErgo ] in
          let pool = Task.pool () in
          ProofEngine.iter (self#schedule pool provers) fork ;
          let server = ProverTask.server () in
          state <- Forking(proof,fork,pool) ;
          Task.launch server ;
        end

    method private apply tactic selection process =
      match state with
      | Empty | Forking _ | Composer _ | Browser _ -> ()
      | Proof proof ->
          Wutil.later
            begin fun () ->
              let title = tactic#title in
              let tactic = ProofScript.jtactic ~title tactic selection in
              let anchor = ProofEngine.anchor proof () in
              self#fork proof (ProofEngine.fork proof ~anchor tactic process)
            end

    method private search proof = function
      | None -> text#printf "No tactic found.@\n"
      | Some fork -> self#fork proof fork

    method private strategies sequent hs =
      match state with
      | Empty | Forking _ | Composer _ | Browser _ -> ()
      | Proof proof ->
          Wutil.later
          begin fun () ->
            let anchor = ProofEngine.anchor proof () in
            let pool = new Strategy.pool in
            Model.with_model
              (ProofEngine.model anchor)
              (List.iter (fun h -> h#search pool#add sequent)) hs ;
            self#search proof (ProverSearch.first proof ~anchor pool#sort)
          end

    method private backtrack node =
      match state with
      | Empty | Forking _ | Composer _ | Browser _ -> ()
      | Proof proof ->
          ProofEngine.goto proof (`Node node) ;
          let fork = ProverSearch.backtrack proof ~anchor:node ~loop:true in
          self#search proof fork

  end

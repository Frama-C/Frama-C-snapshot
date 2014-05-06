(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

open ProverWhy3
  
(* ------------------------------------------------------------------------ *)
(* ---  Prover List in Configuration                                    --- *)
(* ------------------------------------------------------------------------ *)

class provers config =
object(self)
  inherit [dp list] Toolbox.selector []
    
  method private load () =
    let open Gtk_helper.Configuration in
    let rec collect w = function
      | ConfString s -> ProverWhy3.parse s :: w
      | ConfList fs -> List.fold_left collect w fs
      | _ -> w in
    try
      let data = Gtk_helper.Configuration.find config in
      self#set (List.rev (collect [] data))
    with Not_found -> ()
      
  method private save () =
    let open Gtk_helper.Configuration in
    Gtk_helper.Configuration.set config
      (ConfList (List.map (fun dp -> ConfString dp.dp_prover) self#get))
      
  initializer 
    begin
      self#load () ;
      self#on_event self#save ;
    end

end

(* ------------------------------------------------------------------------ *)
(* ---  WP Provers Configuration Panel                                  --- *)
(* ------------------------------------------------------------------------ *)

class dp_chooser
  ~(main:Design.main_window_extension_points) 
  ~(available:provers)
  ~(enabled:provers)
  =
  let dialog = new Toolbox.dialog 
    ~title:"Why3 Provers" 
    ~window:main#main_window
    ~resize:false () in
  let array = new Toolbox.warray () in
object(self)
  
  val mutable provers = []

  method private enable dp e =
    let rec hook dp e = function
      | [] -> [dp,e]
      | head :: tail ->
	  if fst head = dp then (dp,e) :: tail
	  else head :: hook dp e tail
    in provers <- hook dp e provers

  method private lookup dp = 
    try List.assoc dp provers
    with Not_found -> false
    
  method private entry dp =
    let text = Printf.sprintf "%s (%s)" dp.dp_name dp.dp_version in
    let sw = new Toolbox.switchbox () in
    let lb = new Toolbox.label ~align:`Left ~text () in
    sw#set (self#lookup dp) ;
    sw#connect (self#enable dp) ;
    let hbox = GPack.hbox ~spacing:10 ~homogeneous:false () in
    hbox#pack ~expand:false sw#coerce ;
    hbox#pack ~expand:true lb#coerce ;
    (object
       method widget = hbox#coerce
       method update () = sw#set (self#lookup dp)
       method delete () = ()
     end)

  method private configure dps =
    begin
      available#set dps ;
      array#set dps ;
      provers <- List.map (fun dp -> dp , self#lookup dp) dps ;
      array#update () ;
    end

  method private detect () = ProverWhy3.detect_provers self#configure

  method private select () =
    let dps = List.fold_right
      (fun (dp,e) dps -> if e then dp :: dps else dps)
      provers []
    in enabled#set dps

  method run () =
    available#send self#configure () ;
    List.iter (fun dp -> self#enable dp true) enabled#get ;
    array#update () ;
    dialog#run ()

  initializer
    begin
      dialog#button ~action:(`ACTION self#detect) ~label:"Detect Provers" () ;
      dialog#button ~action:(`CANCEL) ~label:"Cancel" () ;
      dialog#button ~action:(`APPLY) ~label:"Apply" () ;
      array#create self#entry ;
      dialog#add_block array#coerce ;
      dialog#on_value `APPLY self#select ;
    end

end

(* ------------------------------------------------------------------------ *)
(* ---  WP Prover Switch Panel                                          --- *)
(* ------------------------------------------------------------------------ *)

type mprover =
  | NoProver
  | AltErgo
  | Coq
  | Why3ide
  | Why3 of dp

class dp_button ~(available:provers) ~(enabled:provers) =
  let render = function
    | NoProver -> "None"
    | AltErgo -> "Alt-Ergo (native)"
    | Coq -> "Coq (native,ide)"
    | Why3ide -> "Why3 (ide)"
    | Why3 dp -> Printf.sprintf "Why3: %s (%s)" dp.dp_name dp.dp_version
  in
  let items = [ NoProver ; AltErgo ; Coq ; Why3ide ] in
  let button = new Toolbox.menulist ~default:AltErgo ~render ~items () in
object(self)
  method coerce = button#coerce
  method set_enabled = button#set_enabled

  method private import =
    match Wp_parameters.Provers.get () with
      | [] -> ()
      | spec :: _ ->
	  match VCS.prover_of_name spec with
	    | Some (VCS.Why3 p) ->
		let dps = available#get in
		let dp = ProverWhy3.find p dps in
		if not (List.mem dp dps) then available#set (dps @ [dp]) ;
		let en = dp :: enabled#get in
		enabled#set 
		  (List.filter (fun q -> List.mem q en) available#get)
	    | _ -> ()

  method private set_provers dps = 
    button#set_items (items @ List.map (fun dp -> Why3 dp) dps)

  method private get_selection = function
    | NoProver -> "none"
    | AltErgo -> "alt-ergo"
    | Coq -> "coqide"
    | Why3ide -> "why3ide"
    | Why3 dp -> "why3:" ^ dp.dp_prover
	  
  method private set_selection = function
    | [] -> ()
    | spec :: _ ->
	match VCS.prover_of_name spec with
	  | None | Some VCS.Qed -> button#set NoProver
	  | Some VCS.AltErgo -> button#set AltErgo
	  | Some VCS.Coq -> button#set Coq
	  | Some VCS.Why3ide -> button#set Why3ide
	  | Some (VCS.Why3 spec) ->
	      let dp = ProverWhy3.find spec enabled#get in
	      button#set (Why3 dp)
	
  val mutable last = []
  val mutable init = true
	
  method update () =
    begin
      if init then self#import ;
      let current = Wp_parameters.Provers.get () in
      if current <> last then
	self#set_selection (Wp_parameters.Provers.get ()) ;
      last <- current ;
      if init then
	begin
	  self#set_provers enabled#get ;
	  enabled#connect self#set_provers ;
	  init <- false ;
	end
    end
      
  initializer
    begin
      button#connect
	(fun mp -> Wp_parameters.Provers.set [self#get_selection mp]) ;
    end

end

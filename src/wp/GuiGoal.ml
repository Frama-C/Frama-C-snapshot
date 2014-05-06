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

(* -------------------------------------------------------------------------- *)
(* --- PO Details View                                                    --- *)
(* -------------------------------------------------------------------------- *)

type prover_state =
  | PS_nogoal
  | PS_click_to_play of Wpo.t
  | PS_click_to_log of Wpo.t
  | PS_click_to_stop of Wpo.t * (unit -> unit)

type display_state =
  | DSP_nogoal
  | DSP_goal of Wpo.t * VCS.prover option

let icon = function
  | VCS.NoResult -> `REMOVE
  | VCS.Failed -> `DIALOG_WARNING
  | VCS.Valid -> `YES
  | VCS.Unknown | VCS.Invalid -> `NO
  | VCS.Computing _ -> `EXECUTE
  | VCS.Timeout | VCS.Stepout -> `CUT

class prover prv =
  let label  = VCS.name_of_prover prv in
  let button = new Toolbox.button ~label () in
object(self)
  val mutable state = PS_nogoal
  val mutable run = (fun _ _ -> ())
  val mutable log = (fun _ _ -> ())
  method widget = (button :> Toolbox.widget)    
  method set_display = function
    | DSP_nogoal -> 
	begin
	  state <- PS_nogoal ; 
	  button#set_relief false ; 
	  button#set_icon None ;
	  button#set_enabled false ;
	end
    | DSP_goal (w,p) ->
	button#set_enabled true ;
	let v = (Wpo.get_result w prv).VCS.verdict in
	begin
	  match v with
	    | VCS.NoResult ->
		begin
		  state <- PS_click_to_play w ;
		  button#set_relief true ;
		  button#set_icon (Some `MEDIA_PLAY) ;
		end
	    | VCS.Computing kill -> 
		let me = match p with None -> false | Some p -> p=prv in
		if me then
		  begin
		    state <- PS_click_to_stop(w,kill) ;
		    button#set_relief true ;
		    button#set_icon (Some `MEDIA_STOP) ;
		  end
		else
		  begin
		    state <- PS_click_to_log w ;
		    button#set_relief false ;
		    button#set_icon (Some `EXECUTE) ;
		  end
	    | _ ->
		let me = match p with None -> false | Some p -> p=prv in
		if me then 
		  begin
		    state <- PS_click_to_play w ;
		    button#set_relief true ;
		    button#set_icon (Some (icon v)) ;
		  end
		else
		  begin
		    state <- PS_click_to_log w ;
		    button#set_relief false ;
		    button#set_icon (Some (icon v)) ;
		  end
		    
	end

  method on_run f = run <- f
  method on_log f = log <- f

  method click = match state with
    | PS_nogoal -> ()
    | PS_click_to_log w -> log w prv
    | PS_click_to_play w -> run w prv
    | PS_click_to_stop(w,kill) -> kill () ; log w prv

  initializer 
    begin
      self#set_display DSP_nogoal ;
      button#connect (fun () -> self#click) ;
    end

end

class pane () =
  let goal = new Toolbox.button ~tooltip:"Proof Obligation" ~icon:`FILE () in
  let title = GMisc.label ~xalign:0.0 ~text:"Goal" () in
  let text = new Toolbox.text () in
  let hbox = GPack.hbox ~show:true () in
  let vbox = GPack.vbox ~show:true () in
  let provers = List.map (new prover) 
    [VCS.AltErgo ; VCS.Coq ; VCS.Why3ide] in
object(self)
  
  val mutable state = DSP_nogoal
  val mutable run = fun _ _ -> ()
  val mutable src = fun (_:Wpo.t option) -> ()
    
  initializer
    begin
      hbox#pack ~expand:false goal#coerce ;
      hbox#pack ~padding:3 ~expand:true ~fill:true title#coerce ;
      let tabs = List.map (fun p -> p#widget) provers in
      let rack = new Toolbox.rack tabs in
      hbox#pack ~expand:false rack#coerce ;
      vbox#pack ~expand:false hbox#coerce ;
      vbox#pack ~expand:true ~fill:true text#coerce ;
      (* Connections *)
      goal#connect (fun () -> self#goal) ;
      List.iter (fun p -> p#on_log self#log) provers ;
      List.iter (fun p -> p#on_run self#run) provers ;
    end

  method private goal = 
    match state with
      | DSP_nogoal | DSP_goal(_,None) -> ()
      | DSP_goal(w,Some _) -> state <- DSP_goal(w,None) ; self#update

  method private log w p =
    begin
      state <- DSP_goal(w,Some p) ; 
      self#update ; 
    end

  method private run w p =
    begin
      state <- DSP_goal(w,Some p) ;
      run w p ;
      self#update ; 
    end

  method on_run f = run <- f
  method on_src f = src <- f

  method select = function
    | None -> state <- DSP_nogoal ; self#update
    | Some w -> state <- DSP_goal(w,None) ; self#update

  method update =
    text#clear ;
    begin
      List.iter (fun p -> p#set_display state) provers ;
      match state with 
	| DSP_nogoal -> 
	    begin
	      title#set_text "No Goal" ;
	    end
	| DSP_goal(w,None) ->
	    begin
	      title#set_text (Pretty_utils.to_string Wpo.pp_title w) ;
	      Wpo.pp_goal text#fmt w ;
	      Format.pp_print_flush text#fmt () ;
	    end
	| DSP_goal(w,Some p) ->
	    begin
	      title#set_text (Pretty_utils.to_string Wpo.pp_title w) ;
	      Wpo.pp_logfile text#fmt w p ;
	      Format.pp_print_flush text#fmt () ;
	    end
    end
      
  method coerce = vbox#coerce

end

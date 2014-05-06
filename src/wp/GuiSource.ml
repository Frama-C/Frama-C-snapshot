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
(* --- Source Interaction for WP                                          --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Cil_datatype
open Pretty_source
open Wpo

type selection =
  | S_none
  | S_fun of Kernel_function.t
  | S_prop of Property.t
  | S_call of call 

and call = {
  s_caller : Kernel_function.t ;
  s_called : Kernel_function.t ;
  s_stmt : Stmt.t ;
}

let selection_of_localizable = function
  | PStmt( kf , stmt )
  | PLval( Some kf , Kstmt stmt , _ )
  | PTermLval( Some kf , Kstmt stmt , _ ) ->
      begin
        match stmt with
          | { skind=Instr(Call(_,e,_,_)) } ->
              begin
                match Kernel_function.get_called e with
                  | None -> S_none
                  | Some called ->
		      S_call { 
			s_called = called ; 
			s_caller = kf ; 
			s_stmt = stmt ; 
		      }
                end
          | _ -> S_none
      end
  | PVDecl (Some kf,{vglob=true}) -> S_fun kf
  | PIP ip -> S_prop ip
  | PVDecl _ | PLval _ | PTermLval _ | PGlobal _ -> S_none

let kind_of_property = function
  | Property.IPLemma _ -> "lemma"
  | Property.IPCodeAnnot _ -> "annotation"
  | Property.IPPredicate( Property.PKRequires _ , _ , Kglobal , _ ) -> 
      "precondition for callers"
  | _ -> "property"
      
(* -------------------------------------------------------------------------- *)
(* --- Popup Menu for WP                                                  --- *)
(* -------------------------------------------------------------------------- *)
      
let is_rte_generated kf =
  List.for_all (fun (_, _, lookup) -> lookup kf) (!Db.RteGen.get_all_status ())
  
let is_rte_precond kf =
  let _, _, lookup = !Db.RteGen.get_precond_status () in 
  lookup kf

class popup () =
object(self)

  val mutable click : selection -> unit = (fun _ -> ())
  val mutable prove : selection -> unit = (fun _ -> ())

  method on_click f = click <- f
  method on_prove f = prove <- f

  method private add_rte 
    (menu : GMenu.menu GMenu.factory) 
    (main : Design.main_window_extension_points) title action kf =
    ignore (menu#add_item title 
	      ~callback:(fun () -> !action kf ; main#redisplay ()))
      
  method private rte_popup menu main loc =
    match loc with
      | PVDecl (Some kf,{vglob=true}) ->
	  if not (is_rte_generated kf) then
	    self#add_rte menu main "Insert WP-safety guards" 
	      Db.RteGen.do_all_rte kf ; 
	  if not (is_rte_precond kf) then
	    self#add_rte menu main "Insert all callees contract" 
	      Db.RteGen.do_precond kf;
      | PStmt(kf,({ skind=Instr(Call _) })) ->
	  if not (is_rte_precond kf) then
	    self#add_rte menu main "Insert callees contract (all calls)" 
	      Db.RteGen.do_precond kf;
      | _ -> ()

  method private wp_popup (menu : GMenu.menu GMenu.factory) = function
    | S_none -> ()
    | s -> 
	let target = match s with
	  | S_none -> "none"
	  | S_prop ip -> kind_of_property ip
	  | S_call _ -> "call preconditions"
	  | S_fun _ -> "function annotations"
	in
	let title = Printf.sprintf "Prove %s by WP" target in
	ignore (menu#add_item title ~callback:(fun () -> prove s))

  method register 
    (menu : GMenu.menu GMenu.factory)
    (main : Design.main_window_extension_points)
    ~(button:int) (loc:Pretty_source.localizable) =
    begin match button with
      | 1 -> 
	  begin
	    match selection_of_localizable loc with
	      | S_none -> ()
	      | s -> click s
	  end
      | 3 ->
	  begin
	    self#wp_popup menu (selection_of_localizable loc) ;
	    self#rte_popup menu main loc ;
	  end
      | _ -> ()
    end

end

(* -------------------------------------------------------------------------- *)
(* --- Source Highlighter for WP                                          --- *)
(* -------------------------------------------------------------------------- *)

module PATH = Stmt.Set
module DEPS = Property.Set

let apply_tag name attr buffer start stop =
  let tg = Gtk_helper.make_tag buffer name attr in
  Gtk_helper.apply_tag buffer tg start stop

let apply_goal = apply_tag "wp.goal" [`BACKGROUND "lightblue"]
let apply_effect = apply_tag "wp.effect" [`BACKGROUND "lightblue"]
let apply_path = apply_tag "wp.path" [`BACKGROUND "yellow"]
let apply_depend = apply_tag "wp.depend" [`BACKGROUND "pink"]

let instructions path =
  PATH.filter
    (fun s -> match s.skind with
       | Instr _ -> true
       | _ -> false)
    path

let lemmas ls = 
  List.fold_left (fun s l -> DEPS.add (LogicUsage.ip_lemma l) s) DEPS.empty ls

class highlighter (main:Design.main_window_extension_points) =
object(self)

  val mutable goal = None (* orange *)
  val mutable effect = None (* blue *)
  val mutable path = PATH.empty (* yellow *)
  val mutable deps = DEPS.empty (* green *)
  val mutable current = None

  method private clear =
    begin
      goal <- None ;
      effect <- None ;
      path <- PATH.empty ;
      deps <- DEPS.empty ;
    end

  method private scroll () =
    main#rehighlight () ;
    match goal with
      | None -> ()
      | Some ip -> main#scroll (PIP ip)

  method set s =
    let moved = match current, s with
      | None , None -> false
      | Some s0 , Some s1 -> s0.po_gid <> s1.po_gid
      | None , Some _ | Some _ , None -> true
    in if moved then
      begin
	current <- s ;
	self#clear ;
	match s with
	  | None -> Gtk_helper.later main#rehighlight ;
	  | Some { Wpo.po_pid = pid ; Wpo.po_formula = f } ->
	      begin
		match f with
		  | GoalCheck _ -> ()
		  | GoalLemma l -> 
		      deps <- lemmas l.VC_Lemma.depends
		  | GoalAnnot a ->
		      effect <- a.VC_Annot.effect ;
		      path <- instructions a.VC_Annot.path ;
		      deps <- a.VC_Annot.deps ;
	      end ;
	      if not (WpPropId.is_check pid) then
		( let ip = WpPropId.property_of_id pid in
		  goal <- Some ip ) ;
	      Gtk_helper.later self#scroll ;
      end

  method update = main#rehighlight ()
	    
  method highlight 
    (buffer : GSourceView2.source_buffer)
    (loc : Pretty_source.localizable)
    ~(start:int) ~(stop:int) =
    begin match loc with
      | PStmt( _ , stmt ) ->
	  begin
	    match effect with
	      | Some(s,_) when Stmt.equal stmt s -> 
		  apply_effect buffer start stop
	      | _ ->
		  if PATH.mem stmt path then
		    apply_path buffer start stop
	  end
      | PIP ip -> 
	  begin
	    match goal with
	      | Some g when Property.equal g ip ->
		  apply_goal buffer start stop
	      | _ ->
		  if DEPS.mem ip deps then
		    apply_depend buffer start stop
	  end
      | PGlobal _|PVDecl _|PTermLval _|PLval _ -> ()
    end

end

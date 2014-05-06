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
(* --- Dependencies of Logic Definitions                                  --- *)
(* -------------------------------------------------------------------------- *)

open Cil
open Cil_types
open Cil_datatype
open Clabels
open Visitor

(* -------------------------------------------------------------------------- *)
(* --- Name Utilities                                                     --- *)
(* -------------------------------------------------------------------------- *)

let trim name =
  let rec first s k n =
    if k < n && s.[k]='_' then first s (succ k) n else k in
  let rec last s k =
    if k >= 0 && s.[k]='_' then last s (pred k) else k in
  let n = String.length name in
  if n > 0 then
    if ( name.[0]='_' || name.[n-1]='_' ) then
      let p = first name 0 n in
      let q = last name (pred n) in
      if p <= q then 
	let name = String.sub name p (q+1-p) in
	match name.[0] with
	  | '0' .. '9' -> "_" ^ name
	  | _ -> name
      else "_"
    else name
  else "_"

(* -------------------------------------------------------------------------- *)
(* --- Definition Blocks                                                  --- *)
(* -------------------------------------------------------------------------- *)

type logic_lemma = {
  lem_name : string ;
  lem_position : Lexing.position ;
  lem_axiom : bool ;
  lem_types : string list ;
  lem_labels : logic_label list ;
  lem_property : predicate named ;
  lem_depends : logic_lemma list ; 
  (* global lemmas declared before in AST order (in reverse order) *)
}

type axiomatic = {
  ax_name : string ;
  ax_position : Lexing.position ;
  ax_property : Property.t ;
  mutable ax_types : logic_type_info list ;
  mutable ax_logics : logic_info list ;
  mutable ax_lemmas : logic_lemma list ;
  mutable ax_reads : Varinfo.Set.t ; (* read-only *)
}

type logic_section =
  | Toplevel of int
  | Axiomatic of axiomatic

let is_global_axiomatic ax =
  ax.ax_types = [] &&
  ax.ax_logics = [] &&
  ax.ax_lemmas <> []

module SMap = Datatype.String.Map
module TMap = Logic_type_info.Map
module LMap = Logic_info.Map
module LSet = Logic_info.Set

(* -------------------------------------------------------------------------- *)
(* --- Usage and Dependencies                                             --- *)
(* -------------------------------------------------------------------------- *)

type inductive_case = {
  ind_logic : logic_info ;
  ind_case : string ;
  mutable ind_call : LabelSet.t LabelMap.t ;
}

type database = {
  mutable cases : inductive_case list LMap.t ;
  mutable clash : LSet.t SMap.t ;
  mutable names : string LMap.t ;
  mutable types : logic_section TMap.t ;
  mutable logics : logic_section LMap.t ;
  mutable lemmas : (logic_lemma * logic_section) SMap.t ;
  mutable recursives : LSet.t ;
  mutable axiomatics : axiomatic SMap.t ;
  mutable proofcontext : logic_lemma list ;
}

let empty_database () = {
  cases = LMap.empty ;
  names = LMap.empty ;
  clash = SMap.empty ;
  types = TMap.empty ;
  logics = LMap.empty ;
  lemmas = SMap.empty ;
  recursives = LSet.empty ;
  axiomatics = SMap.empty ;
  proofcontext = [] ;
}

module DatabaseType = Datatype.Make
  (struct
     type t = database
     include Datatype.Serializable_undefined
     let reprs = [empty_database ()]
     let name = "Wp.LogicUsage.DatabaseType"
   end)

module Database = State_builder.Ref(DatabaseType)
  (struct
     let name = "Wp.LogicUsage.Database"
     let dependencies = [Ast.self;Annotations.code_annot_state]
     let default = empty_database
   end)

let pp_logic fmt l = Printer.pp_logic_var fmt l.l_var_info

(* -------------------------------------------------------------------------- *)
(* --- Overloading                                                        --- *)
(* -------------------------------------------------------------------------- *)

let basename x = trim x.vorig_name

let compute_logicname l =
  let d = Database.get () in
  try LMap.find l d.names
  with Not_found ->
    let base = l.l_var_info.lv_name in
    let over = 
      try SMap.find base d.clash 
      with Not_found -> LSet.empty (*TODO: Undected usage -> overloading issue *)
    in
    match LSet.elements over with
      | [] | [_] -> d.names <- LMap.add l base d.names ; base
      | symbols ->
	  let rec register k = function
	    | l::ls ->
		let name = Printf.sprintf "%s_%d_" base k in
		d.names <- LMap.add l name d.names ;
		register (succ k) ls
	    | [] -> ()
	  in register 1 symbols ; LMap.find l d.names

let is_overloaded l =
  let d = Database.get () in
  try LSet.cardinal (SMap.find l.l_var_info.lv_name d.clash) > 1
  with Not_found -> false

let pp_profile fmt l =
  Format.fprintf fmt "%s" l.l_var_info.lv_name ;
  match l.l_profile with
    | [] -> ()
    | x::xs -> 
	Format.fprintf fmt "@[<hov 1>(%a" Printer.pp_logic_type x.lv_type ;
	List.iter
	  (fun y -> Format.fprintf fmt ",@,%a" 
	     Printer.pp_logic_type y.lv_type)
	  xs ;
	Format.fprintf fmt ")@]"

(* -------------------------------------------------------------------------- *)
(* --- Utilities                                                          --- *)
(* -------------------------------------------------------------------------- *)

let ip_lemma l = 
  (if l.lem_axiom then Property.ip_axiom else Property.ip_lemma)
    (l.lem_name,l.lem_labels,l.lem_types,
     l.lem_property,(l.lem_position,l.lem_position))
    
let lemma_of_global proof = function
  | Dlemma(name,axiom,labels,types,pred,loc) -> {
      lem_name = name ;
      lem_position = fst loc ;
      lem_types = types ;
      lem_labels = labels ;
      lem_axiom = axiom ;
      lem_property = pred ;
      lem_depends = proof ;
    }
  | _ -> assert false

let populate a proof = function
  | Dfun_or_pred(l,_) -> a.ax_logics <- l :: a.ax_logics
  | Dtype(t,_) -> a.ax_types <- t :: a.ax_types
  | Dlemma _ as g -> a.ax_lemmas <- lemma_of_global proof g :: a.ax_lemmas
  | _ -> ()

let ip_of_axiomatic g =
  match Property.ip_of_global_annotation_single g with
    | None -> assert false
    | Some ip -> ip

let axiomatic_of_global proof = function
  | Daxiomatic(name,globals,loc) as g ->
      let a = {
	ax_name = name ;
	ax_position = fst loc ;
	ax_property = ip_of_axiomatic g ;
	ax_reads = Varinfo.Set.empty ;
	ax_types = [] ; ax_lemmas = [] ; ax_logics = [] ;
      } in
      List.iter (populate a proof) globals ;
      a.ax_types <- List.rev a.ax_types ;
      a.ax_logics <- List.rev a.ax_logics ;
      a.ax_lemmas <- List.rev a.ax_lemmas ;
      a
  | _ -> assert false

let register_logic d section l =
  let name = l.l_var_info.lv_name in
  let over =
    try LSet.add l (SMap.find name d.clash)
    with Not_found -> LSet.singleton l in
  begin
    d.clash <- SMap.add name over d.clash ;
    d.logics <- LMap.add l section d.logics ;
  end

let register_lemma d section l =
  begin
    d.lemmas <- SMap.add l.lem_name (l,section) d.lemmas ;
  end

let register_type d section t =
  begin
    d.types <- TMap.add t section d.types ;
  end

let register_axiomatic d a =
  begin
    d.axiomatics <- SMap.add a.ax_name a d.axiomatics ;
  end

let register_cases l inds =
  let d = Database.get () in
  d.cases <- LMap.add l inds d.cases

(* -------------------------------------------------------------------------- *)
(* --- Adding a label called in an inductive case                         --- *)
(* -------------------------------------------------------------------------- *)

(* calls : LabelSet.t LabelMap.t
   Given an inductive phi{...A...}
   In case H{...B...}, have a call to phi{...B...} 
   Then: ( A \in calls[B] ).
*)

let add_call calls (l_a,l_b) =
  let a = Clabels.c_label l_a in
  let b = Clabels.c_label l_b in
  let s = 
    try LabelSet.add a (LabelMap.find b calls) 
    with Not_found -> LabelSet.singleton a 
  in 
  LabelMap.add b s calls

(* -------------------------------------------------------------------------- *)
(* --- Visitor                                                            --- *)
(* -------------------------------------------------------------------------- *)

class visitor =
object(self)

  inherit Visitor.frama_c_inplace

  val database = Database.get ()
  val mutable caller : logic_info option = None
  val mutable axiomatic : axiomatic option = None
  val mutable inductive : inductive_case option = None
  val mutable toplevel = 0

  method private section = 
    match axiomatic with
      | None -> Toplevel toplevel
      | Some a -> Axiomatic a

  method private do_var x =
    match axiomatic with
      | None -> ()
      | Some a -> a.ax_reads <- Varinfo.Set.add x a.ax_reads

  method private do_lvar x =
    try self#do_call (Logic_env.find_logic_cons x) []
    with Not_found -> ()

  method private do_call l labels =
    match inductive with
      | Some case ->
	if Logic_info.equal l case.ind_logic then
	  case.ind_call <- List.fold_left add_call case.ind_call labels
      | None ->
	  match caller with
	    | None -> ()
	    | Some f ->
		if Logic_info.equal f l then
		  database.recursives <- LSet.add f database.recursives
		    
  method private do_case l (case,_labels,_types,pnamed) =
    begin
      let indcase = {
	ind_logic = l ;
	ind_case = case ;
	ind_call = LabelMap.empty ;
      } in
      inductive <- Some indcase ;
      ignore (visitFramacPredicateNamed (self :> frama_c_visitor) pnamed) ;
      inductive <- None ; indcase
    end

  (* --- LVALUES --- *)

  method! vlval = function
    | (Var x,_) -> self#do_var x ; DoChildren
    | _ -> DoChildren

  method! vterm_lval = function
    | (TVar { lv_origin=Some x } , _ ) -> self#do_var x ; DoChildren
    | (TVar x , _ ) -> self#do_lvar x ; DoChildren 
    | _ -> DoChildren

  (* --- TERMS --- *)

  method! vterm_node = function
    | Tapp(l,labels,_) -> self#do_call l labels ; DoChildren
    | _ -> DoChildren

  (* --- PREDICATE --- *)

  method! vpredicate = function
    | Papp(l,labels,_) -> self#do_call l labels ; DoChildren
    | _ -> DoChildren

  method! vannotation global =
    match global with

      (* --- AXIOMATICS --- *)

      | Daxiomatic _ -> 
	  begin
	    let pf = database.proofcontext in
	    let ax = axiomatic_of_global pf global in
	    register_axiomatic database ax ;
	    axiomatic <- Some ax ;
	    DoChildrenPost 
	      (fun g -> 
		 if not (is_global_axiomatic ax) then
		   database.proofcontext <- pf ;
		 axiomatic <- None ;
		 toplevel <- succ toplevel ;
		 g)
	  end

      (* --- LOGIC INFO --- *)

      | Dfun_or_pred(l,_) ->
	  begin
	    register_logic database self#section l ;
	    match l.l_body with
	      | LBnone when axiomatic = None -> SkipChildren

	      | LBnone | LBreads _ | LBterm _ | LBpred _ ->
		  caller <- Some l ;
		  DoChildrenPost (fun g -> caller <- None ; g)

	      | LBinductive cases ->
		  register_cases l (List.map (self#do_case l) cases) ;
		  SkipChildren
	  end

      (* --- LEMMAS --- *)
	    
      | Dlemma _ ->
	  let lem = lemma_of_global database.proofcontext global in
	  register_lemma database self#section lem ;
	  database.proofcontext <- lem :: database.proofcontext ;
	  SkipChildren

      | Dtype(t,_) ->
	  register_type database self#section t ;
	  SkipChildren

      (* --- OTHERS --- *)

      | Dvolatile _
      | Dinvariant _
      | Dtype_annot _
      | Dmodel_annot _
      | Dcustom_annot _ 
	-> SkipChildren

  method! vfunc _ = SkipChildren
	  
end

let compute () =
  Wp_parameters.feedback "Collecting axiomatic usage" ;
  Visitor.visitFramacFile (new visitor) (Ast.get ())

(* -------------------------------------------------------------------------- *)
(* --- External API                                                       --- *)
(* -------------------------------------------------------------------------- *)

let (compute,_) = 
  State_builder.apply_once "LogicUsage.compute" 
    [Ast.self;Annotations.code_annot_state] compute

let is_recursive l =
  compute () ; 
  let d = Database.get () in
  LSet.mem l d.recursives

let get_induction_labels l case =
  compute () ;
  try 
    let d = Database.get () in
    let cases = LMap.find l d.cases in
    try (List.find (fun i -> i.ind_case = case) cases).ind_call
    with Not_found ->
      Wp_parameters.fatal "No case '%s' for inductive '%s'"
	case l.l_var_info.lv_name
  with Not_found ->
    Wp_parameters.fatal "Non-inductive '%s'" l.l_var_info.lv_name

let axiomatic a =
  compute () ;
  try
    let d = Database.get () in
    SMap.find a d.axiomatics
  with Not_found ->
    Wp_parameters.fatal "Axiomatic '%s' undefined" a

let section_of_type t =
  compute () ;
  try
    let d = Database.get () in
    TMap.find t d.types
  with Not_found ->
    Wp_parameters.fatal "Logic type '%s' undefined" t.lt_name

let section_of_logic l =
  compute () ;
  try
    let d = Database.get () in
    LMap.find l d.logics
  with Not_found ->
    Wp_parameters.fatal "Logic '%a' undefined" pp_logic l

let get_lemma l =
  compute () ;
  try
    let d = Database.get () in
    SMap.find l d.lemmas
  with Not_found ->
    Wp_parameters.fatal "Lemma '%s' undefined" l

let iter_lemmas f =
  compute () ;
  let d = Database.get () in
  SMap.iter (fun _name (lem,_) -> f lem) d.lemmas

let logic_lemma l = fst (get_lemma l)

let section_of_lemma l = snd (get_lemma l)

let proof_context () =
  (* No need for compute: if no lemma, database is empty ! *)
  let d = Database.get () in
  d.proofcontext

(* -------------------------------------------------------------------------- *)
(* --- Dump API                                                           --- *)
(* -------------------------------------------------------------------------- *)

let dump_type fmt t = Format.fprintf fmt " * type '%s'@\n" t.lt_name

let dump_profile fmt kind l =
  begin
    Format.fprintf fmt " * %s '%s'@\n" kind (compute_logicname l) ;
    if is_overloaded l then
      Format.fprintf fmt "   profile %a@\n" pp_profile l ;
    if is_recursive l then
      Format.fprintf fmt "   recursive@\n" ;
  end

let dump_logic fmt d l =
  begin
    try
      let cases = LMap.find l d.cases in
      dump_profile fmt "inductive" l ;
      List.iter
	(fun ind ->
	   Format.fprintf fmt "   @[case %s:" ind.ind_case ;
	   LabelMap.iter
	     (fun l s ->
		Format.fprintf fmt "@ @[<hov 2>{%a:" Clabels.pretty l ;
		LabelSet.iter (fun l -> Format.fprintf fmt "@ %a" 
				 Clabels.pretty l) s ;
		Format.fprintf fmt "}@]"
	     ) ind.ind_call ;
	   Format.fprintf fmt "@]@\n"
	) cases ;
    with Not_found ->
      let kind = if l.l_type = None then "predicate" else "function" in
      dump_profile fmt kind l ;
  end

let dump_lemma fmt l =
  if l.lem_axiom then 
    Format.fprintf fmt " * axiom '%s'@\n" l.lem_name
  else 
    Format.fprintf fmt " * lemma '%s'@\n" l.lem_name

let get_name l = compute () ; compute_logicname l

let pp_section fmt = function
  | Toplevel 0 -> Format.fprintf fmt "Toplevel"
  | Toplevel n -> Format.fprintf fmt "Toplevel(%d)" n
  | Axiomatic a -> Format.fprintf fmt "Axiomatic '%s'" a.ax_name

let dump () =
  compute () ;
  Log.print_on_output 
    begin fun fmt ->
      let d = Database.get () in
      SMap.iter
	(fun _ a -> 
	   Format.fprintf fmt "Axiomatic %s {@\n" a.ax_name ;
	   List.iter (dump_type fmt) a.ax_types ;
	   List.iter (dump_logic fmt d) a.ax_logics ;
	   List.iter (dump_lemma fmt) a.ax_lemmas ;
	   Format.fprintf fmt "}@\n"
	) d.axiomatics ;
      TMap.iter
	(fun t s ->
	   Format.fprintf fmt " * type '%s' in %a@\n"
	     t.lt_name pp_section s)
	d.types ;
      LMap.iter
	(fun l s -> 
	   Format.fprintf fmt " * logic '%a' in %a@\n" 
	     pp_logic l pp_section s)
	d.logics ;
      SMap.iter
	(fun l (lem,s) ->
	   Format.fprintf fmt " * %s '%s' in %a@\n"
	     (if lem.lem_axiom then "axiom" else "lemma")
	     l pp_section s)
	d.lemmas ;
      Format.fprintf fmt "-------------------------------------------------@." ;
    end

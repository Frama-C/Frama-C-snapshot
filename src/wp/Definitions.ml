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
(* --- Logic Definitions                                                  --- *)
(* -------------------------------------------------------------------------- *)

open LogicUsage
open Cil_types
open Cil_datatype
open Ctypes
open Qed.Logic
open Lang
open Lang.F

type trigger = (var,lfun) Qed.Engine.ftrigger
type typedef = (tau,field,lfun) Qed.Engine.ftypedef

type cluster = {
  c_id : string ;
  c_title : string ;
  c_position : Lexing.position option ;
  mutable c_age : int ;
  mutable c_records : compinfo list ;
  mutable c_types : logic_type_info list ;
  mutable c_symbols : dfun list ;
  mutable c_lemmas : dlemma list ;
}

and dlemma = {
  l_name  : string ;
  l_cluster : cluster ;
  l_assumed : bool ;
  l_types : int ;
  l_forall : var list ;
  l_triggers : trigger list list (* OR of AND triggers *) ;
  l_lemma : pred ;
}

and dfun = {
  d_lfun   : lfun ;
  d_cluster : cluster ;
  d_types  : int ;
  d_params : var list ;
  d_definition : definition ;
}

and definition =
  | Logic of tau (** return type of an abstract function *)
  | Value of tau * recursion * term
  | Predicate of recursion * pred
  | Inductive of dlemma list

and recursion = Def | Rec

module Trigger =
struct

  open Qed.Engine

  let rec of_exp mode t =
    match F.repr t with
      | Var x -> TgVar x
      | Aget(a,k) -> TgGet(of_exp Cterm a,of_exp Cterm k)
      | Aset(a,k,v) -> TgSet(of_exp Cterm a,of_exp Cterm k,of_exp Cterm v)
      | Fun(f,ts) -> 
	  let ts = List.map (of_exp Cterm) ts in
	  begin
	    match mode with
	      | Cterm -> TgFun(f,ts)
	      | Cprop -> TgProp(f,ts)
	  end
      | _ -> TgAny

  let of_term t = of_exp Cterm t
  let of_pred p = of_exp Cprop (F.e_prop p)

  let rec collect xs = function
    | TgAny -> xs
    | TgVar x -> Vars.add x xs
    | TgGet(a,k) -> collect (collect xs a) k
    | TgSet(a,k,v) -> collect (collect (collect xs a) k) v
    | TgFun(_,ts) | TgProp(_,ts) -> List.fold_left collect xs ts

  let vars = collect Vars.empty

  let binders p =
    let rec collect xs p = 
      match F.repr p with
	| Bind( Forall , x , p ) -> collect (x::xs) p
	| Imply( hs , p ) -> 
	    let xs , p = collect xs p in
	    xs , F.e_imply hs p
	| _ -> List.rev xs , p
    in collect [] p

  let plug tgs p =
    let vars,lemma = binders (F.e_prop p) in
    let used = List.fold_left (List.fold_left collect) Vars.empty tgs in
    let xs , ys = List.partition (fun x -> Vars.mem x used) vars in
    xs , F.p_forall ys (F.p_bool lemma)

end

(* -------------------------------------------------------------------------- *)
(* --- Registry                                                           --- *)
(* -------------------------------------------------------------------------- *)

module Cluster = Model.Index
  (struct
     type key = string
     type data = cluster
     let name = "Definitions.Cluster"
     let compare = String.compare
     let pretty = Format.pp_print_string
   end)

module Symbol = Model.Index
  (struct
     type key = lfun
     type data = dfun
     let name = "Definitions.Symbol"
     let compare = Lang.Fun.compare
     let pretty = Lang.Fun.pretty
   end)

module Lemma = Model.Index
  (struct
     type key = string
     type data = dlemma
     let name = "Definitions.Lemma"
     let compare = String.compare
     let pretty = Format.pp_print_string
   end)

let touch c = c.c_age <- succ c.c_age
let compare_symbol f g = Fun.compare f.d_lfun g.d_lfun
let compare_lemma a b = String.compare a.l_name b.l_name

let () =
  begin
    Symbol.callback 
      (fun _ f ->
	 touch f.d_cluster ;
	 f.d_cluster.c_symbols <- f :: f.d_cluster.c_symbols) ;
    Lemma.callback
      (fun _ a ->
	 touch a.l_cluster ;
	 a.l_cluster.c_lemmas <- a :: a.l_cluster.c_lemmas) ;
  end

let define_symbol f = Symbol.define f.d_lfun f
let update_symbol f = Symbol.update f.d_lfun f

let find_lemma l = Lemma.find l.lem_name
let compile_lemma cc l = Lemma.compile (fun _name -> cc l) l.lem_name
let define_lemma l = Lemma.define l.l_name l

let define_type c t =
  begin
    touch c ;
    c.c_types <- t :: c.c_types ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Helpers                                                            --- *)
(* -------------------------------------------------------------------------- *)

let cluster_id c = c.c_id
let cluster_title c = c.c_title
let cluster_position c = c.c_position
let cluster_age c = c.c_age
let cluster_compare a b = String.compare a.c_id b.c_id
let pp_cluster fmt c = Format.pp_print_string fmt c.c_id

let newcluster ~id ?title ?position () =
  {
    c_id = id ;
    c_title = (match title with Some t -> t | None -> id) ;
    c_position = position ;
    c_age = 0 ;
    c_types = [] ;
    c_records = [] ;
    c_symbols = [] ;
    c_lemmas = [] ;
  }

let cluster ~id ?title ?position () =
  Cluster.memoize (fun id -> newcluster ~id ?title ?position ()) id

let axiomatic ax =
  Cluster.memoize 
    (fun id ->
       let title = Printf.sprintf "Axiomatic '%s'" ax.ax_name in
       let position = ax.ax_position in
       let cluster = newcluster ~id ~title ~position () in
       cluster)
    (Printf.sprintf "A_%s" ax.ax_name)

let section = function
  | Toplevel 0 -> cluster ~id:"Axiomatic" ~title:"Global Definitions" ()
  | Toplevel n -> 
      let id = "Axiomatic" ^ string_of_int n in
      let title = Printf.sprintf "Global Definitions (continued #%d)" n in
      cluster ~id ~title ()
  | Axiomatic ax -> axiomatic ax
	
let compinfo c =
  Cluster.memoize
    (fun id ->
       let title = 
	 if c.cstruct 
	 then Printf.sprintf "Struct '%s'" c.cname
	 else Printf.sprintf "Union '%s'" c.cname in
       let cluster = newcluster ~id ~title () 
       in cluster.c_records <- [c] ; cluster)
    (Lang.comp_id c)

let matrix = function
  | C_array _ -> assert false
  | C_comp c -> compinfo c
  | C_int _ | C_float _ | C_pointer _ ->
      Cluster.memoize
	(fun id -> newcluster ~id ~title:"Basic Arrays" ()) "Matrix"

let call_fun lfun cc es =
  Symbol.compile (Lang.local cc) lfun ;
  e_fun lfun es
    
let call_pred lfun cc es =
  Symbol.compile (Lang.local cc) lfun ;
  p_call lfun es

(* -------------------------------------------------------------------------- *)
(* --- Cluster Dependencies                                               --- *)
(* -------------------------------------------------------------------------- *)

module DT = Logic_type_info.Set
module DR = Compinfo.Set
module DS = Datatype.String.Set
module DF = FCSet.Make(Lang.Fun)
module DC = FCSet.Make
  (struct
     type t = cluster
     let compare = cluster_compare
   end)

(* -------------------------------------------------------------------------- *)
(* --- Markers (test and set)                                             --- *)
(* -------------------------------------------------------------------------- *)

type axioms = cluster * logic_lemma list

class virtual visitor main =
object(self)

  val mutable terms    = Tset.empty
  val mutable types    = DT.empty
  val mutable comps    = DR.empty
  val mutable symbols  = DF.empty
  val mutable dlemmas  = DS.empty
  val mutable lemmas   = DS.empty
  val mutable clusters = DC.empty
  val mutable theories = DS.empty
  val mutable locals = DC.add main DC.empty

  method set_local c = locals <- DC.add c locals

  method do_local c =
    if DC.mem c locals then true else
      (self#vcluster c ; false)

  method private vtypedef = function
    | None -> ()
    | Some (LTsum cs) -> 
      List.iter (fun c -> self#vadt (Lang.atype c.ctor_type)) cs
    | Some (LTsyn lt) -> self#vtau (Lang.tau_of_ltype lt)

  method vtype t =
    if not (DT.mem t types) then
      begin
	types <- DT.add t types ;
	let c = section (LogicUsage.section_of_type t) in
	if self#do_local c then
	  begin
	    self#vtypedef t.lt_def ;
	    let def = match t.lt_def with
	      | None -> Qed.Engine.Tabs
	      | Some (LTsyn lt) -> Qed.Engine.Tdef (Lang.tau_of_ltype lt)
	      | Some (LTsum cs) -> 
		  let cases = List.map
		    (fun ct ->
		       Lang.CTOR ct , 
		       List.map Lang.tau_of_ltype ct.ctor_params
		    ) cs in
		  Qed.Engine.Tsum cases
	    in self#on_type t def ;
	  end
      end

  method vcomp r = 
    if not (DR.mem r comps) then
      begin
	comps <- DR.add r comps ;
	let c = compinfo r in
	if self#do_local c then
	  begin
	    let fts = List.map
	      (fun f ->
		 let t = Lang.tau_of_ctype f.ftype in
		 self#vtau t ; Cfield f , t
	      ) r.cfields 
	    in self#on_comp r fts ;
	  end
      end

  method vfield = function
    | Mfield(a,_,_,_) -> self#vlibrary a.ext_library
    | Cfield f -> self#vcomp f.fcomp

  method vadt = function
    | Mtype a | Mrecord(a,_) -> self#vlibrary a.ext_library
    | Comp r -> self#vcomp r
    | Atype t -> self#vtype t

  method vtau = function
    | Prop | Bool | Int | Real | Tvar _ -> ()
    | Array(a,b) -> self#vtau a ; self#vtau b
    | Record _ -> assert false
    | Data(a,ts) -> self#vadt a ; List.iter self#vtau ts

  method vparam x = self#vtau (tau_of_var x)

  method vterm t = 
    if not (Tset.mem t terms) then
      begin
	terms <- Tset.add t terms ;
	F.e_iter self#vterm t ;
	match F.repr t with
	  | Fun(f,_) -> self#vsymbol f
	  | Rget(_,f) -> self#vfield f
	  | Rdef fts -> List.iter (fun (f,_) -> self#vfield f) fts
	  | Var x | Bind(_,x,_) -> self#vparam x
	  | True | False | Kint _ | Kreal _ 
	  | Times _ | Add _ | Mul _ | Div _ | Mod _ 
	  | Eq _ | Neq _ | Leq _ | Lt _ 
	  | Aget _ | Aset _ 
	  | And _ | Or _ | Not _ | Imply _ | If _ | Apply _ -> ()
      end
	
  method vpred p = self#vterm (F.e_prop p)

  method private vdefinition = function
    | Logic t -> self#vtau t
    | Value(t,_,e) -> self#vtau t ; self#vterm e
    | Predicate(_,p) -> self#vpred p
    | Inductive _ -> ()

  method private vproperties = function
    | Logic _ | Value _ | Predicate _ -> ()
    | Inductive cases -> List.iter self#vdlemma cases

  method private vdfun d =
    begin
      List.iter self#vparam d.d_params ;
      self#vdefinition d.d_definition ;
      self#on_dfun d ;
      self#vproperties d.d_definition ;
    end

  method private vlfun f =
    try
      let d = Symbol.find f in
      let c = d.d_cluster in
      if self#do_local c then self#vdfun d
    with Not_found ->
      Wp_parameters.fatal "Undefined symbol '%a'" Fun.pretty f

  method vsymbol f =
    if not (DF.mem f symbols) then
      begin
	symbols <- DF.add f symbols ;
	match f with
	  | Model { m_source = Extern e  } -> self#vlibrary e.ext_library
	  | Model { m_source = Generated _ } | ACSL _ -> self#vlfun f
	  | CTOR c -> self#vadt (Lang.atype c.ctor_type)
      end

  method private vtrigger = function
    | Qed.Engine.TgAny -> ()
    | Qed.Engine.TgVar x -> self#vparam x
    | Qed.Engine.TgGet(a,k) ->
	begin
	  self#vtrigger a ;
	  self#vtrigger k ;
	end
    | Qed.Engine.TgSet(a,k,v) ->
	begin
	  self#vtrigger a ;
	  self#vtrigger k ;
	  self#vtrigger v ;
	end
    | Qed.Engine.TgFun(f,tgs)
    | Qed.Engine.TgProp(f,tgs) -> 
	self#vsymbol f ; List.iter self#vtrigger tgs

  method private vdlemma a =
    if not (DS.mem a.l_name dlemmas) then
      begin
	dlemmas <- DS.add a.l_name dlemmas ;
	List.iter self#vparam a.l_forall ;
	List.iter (List.iter self#vtrigger) a.l_triggers ;
	self#vpred a.l_lemma ;
	self#on_dlemma a ;
      end

  method vlemma lem =
    let l = lem.lem_name in
    if not (DS.mem l lemmas) then
      begin
	lemmas <- DS.add l lemmas ;
	try
	  let a = Lemma.find l in
	  if self#do_local a.l_cluster then self#vdlemma a
	with Not_found ->
	  Wp_parameters.fatal "Lemma '%s' undefined" l
      end

  method vcluster c =
    if not (DC.mem c clusters) then
      begin
	clusters <- DC.add c clusters ;
	self#on_cluster c ;
      end

  method vlibrary thy =
    if not (DS.mem thy theories) then
      begin
	theories <- DS.add thy theories ;
	try
	  let deps = LogicBuiltins.dependencies thy in
	  List.iter self#vlibrary deps ;
	  self#on_library thy ;
	with Not_found ->
          Wp_parameters.fatal
	    ~current:false "Unknown library '%s'" thy
      end

  method vgoal (axioms : axioms option) prop =
    match axioms with
      | None ->
        (** Print a goal *)
	  begin
	    let hs = LogicUsage.proof_context () in
	    List.iter self#vlemma hs ;
	    self#vpred prop ;
	  end
      | Some(cluster,hs) ->
        (** Print the goal corresponding to a lemma *)
	  begin
	    self#section (cluster_title cluster) ;
	    self#set_local cluster ;
	    List.iter self#vlemma hs ;
	    self#vpred prop ;
	  end

  method vself = (** Print a cluster *)
    begin
      List.iter self#vcomp main.c_records ;
      List.iter self#vtype main.c_types ;
      List.iter (fun d -> self#vsymbol d.d_lfun) main.c_symbols ;
      List.iter (fun l -> self#vdlemma l) main.c_lemmas ;
    end

  method virtual section : string -> unit
  method virtual on_library : string -> unit
  method virtual on_cluster : cluster -> unit
  method virtual on_type : logic_type_info -> typedef -> unit
  method virtual on_comp : compinfo -> (field * tau) list -> unit
  method virtual on_dlemma : dlemma -> unit
  method virtual on_dfun : dfun -> unit

end

(* -------------------------------------------------------------------------- *)

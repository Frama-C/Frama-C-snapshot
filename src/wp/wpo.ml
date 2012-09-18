(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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

open LogicUsage
open VCS
open Cil_types
open Cil_datatype
open Lang

type index =
  | Lemma of string
  | Function of kernel_function * string option

let bar = String.make 60 '-'
let flow = ref false

(* -------------------------------------------------------------------------- *)
(* --- Pretty Printers                                                    --- *)
(* -------------------------------------------------------------------------- *)

let pp_index fmt = function
  | Lemma a -> Format.fprintf fmt "Lemma %s" a
  | Function(f,None) -> Kernel_function.pretty fmt f
  | Function(f,Some b) -> Format.fprintf fmt "%a for %s:" Kernel_function.pretty f b

let pp_function fmt kf bhv =
  flow := true ;
  match bhv with
    | None ->
        Format.fprintf fmt
          "%s@\n  Function %s@\n%s@\n@\n"
          bar (Kernel_function.get_name kf) bar
    | Some bhv ->
        Format.fprintf fmt
          "%s@\n  Function %s with behavior %s@\n%s@\n@\n"
          bar (Kernel_function.get_name kf) bhv bar

let pp_warnings fmt ws =
  List.iter (fun w -> Format.fprintf fmt "%a@\n" Warning.pretty w) ws

let kf_context = function Lemma _ -> `Always | Function(kf,_) -> `Context kf

let pp_dependency context fmt d =
  Format.fprintf fmt " - Assumes %a" 
    (Description.pp_localized ~kf:context ~ki:false ~kloc:true) d

let pp_dependencies context fmt ds = 
  List.iter (fun d -> Format.fprintf fmt "%a@\n" (pp_dependency context) d) ds
    
let pp_depend fmt d =
  Format.fprintf fmt " - Assumes %a" 
    (Description.pp_localized ~kf:`Always ~ki:false ~kloc:true) d

(* ------------------------------------------------------------------------ *)
(* ---  Legacy Proof Obligations                                        --- *)
(* ------------------------------------------------------------------------ *)

module VC_Legacy =
struct

  type t = {
    mid : string ;  (* model identifier *)
    env : string ;  (* goal environment identifier *)
    dep : Property.t list ; (* dependencies *)
    wrn : Warning.t list ; (* warnings *)
  }

  let repr = { mid = "" ; env = "" ; dep = [] ; wrn = [] }

  (* --- Legacy: PO files --- *)

  let local base suffix =
    let dir = Wp_parameters.get_output () in
    Printf.sprintf "%s/%s%s" dir base suffix
      
  let file_for_lang base lang =
    let dir = Wp_parameters.get_output () in
    let suffix =
      match lang with
	| L_altergo -> "_ergo.why"
	| L_why -> ".why"
	| L_coq -> ".v"
    in Printf.sprintf "%s/%s%s" dir base suffix
	 
  let file_for_ctxt ~env = local env ".txt"
  let file_for_head ~gid = local gid "_head.txt"
  let file_for_body ~gid = local gid "_body.txt"
  let file_for_log_proof ~gid = function
    | Why s -> local gid ("_" ^s^".txt")
    | Coq -> local gid "_coq.txt"
    | AltErgo -> local gid "_ergo.txt"
    | WP -> local gid "_wp.txt"
	
  let file_for_log_check ~gid = function
    | L_why -> local gid "_why.txt"
    | L_coq -> local gid "_coq.txt"
    | L_altergo -> local gid "_ergo.txt"
	
  let file_for_goal ~gid lang = file_for_lang gid lang
  let file_for_env ~env lang = file_for_lang env lang
  let file_for_po ~gid lang = file_for_lang (gid ^ "_po") lang
    
  let file_for_model ~model lang =
    let suffix = match lang with
      | L_altergo -> "_ergo.why"
      | L_why -> "_model.why"
      | L_coq  -> "_model.v"
    in 
    Printf.sprintf "%s%s" (Wp_parameters.Share.file model) suffix
      
  (* --- Legacy: Coq files --- *)

  let coq_for_env ~env = env
  let coq_for_model ~model = model^"_model"
  let coqc_for_model ~model =
    let dir = Wp_parameters.get_output () in
    Printf.sprintf "%s/%s_model.v" dir model
  let coqlog_for_model ~model =
    let dir = Wp_parameters.get_output () in
    Printf.sprintf "%s/%s_model_coq.txt" dir model
  let coqlog_for_env ~env =
    let dir = Wp_parameters.get_output () in
    Printf.sprintf "%s/%s_coq.txt" dir env

  (* --- Legacy: printing --- *)

  let pp_environment fmt ~env =
    Format.fprintf fmt
      "%s@\n  Proof Environment %s@\n%s@\n@\n%a"
      bar (String.capitalize env) bar
      Command.pp_from_file (file_for_ctxt ~env)

  let pp_formula fmt gid propid context vcd =
    Format.fprintf fmt "Environment: %s@\n" (String.capitalize vcd.env) ;
    List.iter (fun d -> Format.fprintf fmt "%a@\n" (pp_dependency context) d) vcd.dep ;
    List.iter (fun w -> Format.fprintf fmt "%a@\n" Warning.pretty w) vcd.wrn ;
    Format.fprintf fmt " + Proves %a@\n" (WpPropId.pretty_context context) propid ;
    Command.pp_from_file fmt (file_for_body ~gid)
    
end
  
(* ------------------------------------------------------------------------ *)
(* ---  Proof Obligations Definition                                    --- *)
(* ------------------------------------------------------------------------ *)

module DISK =
struct

  let file ~pid ~model ?prover ?suffix ~ext () =
    let dir = Wp_parameters.get_output () in
    let id = WpPropId.get_propid pid in
    let mid = Model.get_id model in
    let buffer = Buffer.create 80 in
    let fmt = Format.formatter_of_buffer buffer in
    Format.fprintf fmt "%s/%s/%s" dir mid id ;
    (match prover with None -> () | Some p -> 
       Format.fprintf fmt "_%s" (name_of_prover p)) ;
    (match suffix with None -> () | Some s ->
       Format.fprintf fmt "_%s" s) ;
    Format.fprintf fmt ".%s" ext ;
    Format.pp_print_flush fmt ();
    Buffer.contents buffer

  let file_logout ~pid ~model ~prover = file ~pid ~model ~prover ~ext:"out" ()
  let file_logerr ~pid ~model ~prover = file ~pid ~model ~prover ~ext:"err" ()
  let file_goal ~pid ~model ~prover = 
    let ext = match prover with
      | WP -> "qed"
      | AltErgo -> "mlw"
      | Why _ -> "why"
      | Coq -> "v"
    in file ~pid ~model ~prover ~ext ()
	
  let dump_file fmt title file =
    if Sys.file_exists file then 
      begin
	Format.fprintf fmt "--- %s ---------------------------------@\n" title ;
	Command.pp_from_file fmt file
      end

  let cache_log ~pid ~model ~prover ~result =
    (*TODO: put a cache here *)
    let dir = Wp_parameters.get_output () in
    let file = Printf.sprintf "%s/log.txt" dir in
    Command.print_file file 
      begin fun fmt ->
	Format.fprintf fmt "[%a] Goal %a : %a@\n" 
	  pp_prover prover WpPropId.pp_propid pid pp_result result ;
	dump_file fmt "StdOut" (file_logout ~pid ~model ~prover) ;
	dump_file fmt "StdErr" (file_logerr ~pid ~model ~prover) ;
      end ;
    file

  let cache_descr pretty =
    (*TODO: put a cache here *)
    let dir = Wp_parameters.get_output () in
    let file = Printf.sprintf "%s/goal.txt" dir in
    Command.print_file file (fun fmt -> pretty fmt) ; file

end

module GOAL =
struct

  type t = {
    mutable simplified : bool ;
    mutable hyps : Hypotheses.t ;
    mutable goal : F.pred ;
    mutable proof : F.pred ;
  }

  let make hyps goal = {
    simplified = false ;
    hyps = hyps ;
    goal = goal ;
    proof = F.p_false ;
  }

  let is_trivial g = g.goal == F.p_true (* simplified or not *)

  let compute g =
    if not g.simplified then
      begin
	g.simplified <- true ;
	if Wp_parameters.Qed.get () then
	  begin
	    let hyps,goal = Hypotheses.simplify_goal g.hyps g.goal in
	    g.proof <- Hypotheses.close hyps goal ;
	    if g.proof != F.p_true then
	      begin
		g.hyps <- hyps ;
		g.goal <- goal ;
	      end ;
	  end
	else
	  g.proof <- Hypotheses.close g.hyps g.goal ;
      end
	    
  let proof g = compute g ; g.proof
  let descr g = compute g ; g.hyps , g.goal

end

module VC_Lemma =
struct

  open Lang
  open Definitions
  
  type t = {
    model : Model.t ;
    lemma : Definitions.dlemma ;
    depends : logic_lemma list ;
  }

  let is_trivial vc = vc.lemma.l_lemma == F.p_true

  let pretty fmt vc results =
    begin
      Format.fprintf fmt "Lemma %s:@\n" vc.lemma.l_name ;
      if vc.depends <> [] then
	begin
	  Format.fprintf fmt "@[<hov 2>Assume" ;
	  List.iter
	    (fun a -> Format.fprintf fmt "@ '%s'" a.lem_name)
	    vc.depends ;
	  Format.fprintf fmt "@]@." ;
	end ;
      Format.fprintf fmt "Prove @[<hov 2>%a@]@." F.pp_pred vc.lemma.l_lemma ;
      List.iter
	(fun (prover,result) ->
	   if result.verdict <> NoResult then
	     Format.fprintf fmt "Prover %a returns %a@\n"
	       pp_prover prover
	       pp_result result
	) results ;
    end
 
  let cache_descr vc results =
    DISK.cache_descr (fun fmt -> pretty fmt vc results)

end
  
module VC_Annot =
struct

  type t = {
    model : Model.t ;
    goal : GOAL.t ;
    tags : Splitter.tag list ;
    warn : Warning.t list ;
    deps : Property.Set.t ;
    path : Stmt.Set.t ;
    effect : stmt option ;
  }
      
  let is_trivial vcq = GOAL.is_trivial vcq.goal
  let is_simplified vcq = GOAL.proof vcq.goal == Lang.F.p_true

  let pp_effect fmt = function
    | None -> ()
    | Some s -> 
	let loc = fst (Stmt.loc s) in
	let line = loc.Lexing.pos_lnum in
	Format.fprintf fmt "Effect at line %d@\n" line

  let pretty fmt pid vc results =
    begin
      Format.fprintf fmt "Goal %a:@\n" WpPropId.pretty pid ;
      pp_effect fmt vc.effect ;
      if vc.tags <> [] then
	begin
	  Format.fprintf fmt "@[<hov 2>Tags:" ;
	  List.iter (fun tg -> Format.fprintf fmt "@ %a" Splitter.pretty tg) vc.tags ;
	  Format.fprintf fmt "@].@\n" ;
	end ;
      pp_warnings fmt vc.warn ;
      let hyps,goal = GOAL.descr vc.goal in
      Format.fprintf fmt "@[<hv 0>%a@ Prove @[<hov 2>%a@]@]@." 
	Hypotheses.pretty hyps F.pp_pred goal ;
      List.iter
	(fun (prover,result) ->
	   if result.verdict <> NoResult then
	     Format.fprintf fmt "Prover %a returns %a@\n"
	       pp_prover prover
	       pp_result result
	) results ;
    end
 
  let cache_descr ~pid vc results =
    DISK.cache_descr (fun fmt -> pretty fmt pid vc results)

end

(* ------------------------------------------------------------------------ *)
(* ---  Proof Obligations Database                                      --- *)
(* ------------------------------------------------------------------------ *)

type formula = 
  | Legacy of VC_Legacy.t 
  | GoalLemma of VC_Lemma.t
  | GoalAnnot of VC_Annot.t

type po = t and t = {
  po_gid   : string ;  (* goal identifier *)
  po_name  : string ;  (* goal informal name *)
  po_idx   : index ;   (* goal index *)
  po_pid   : WpPropId.prop_id ; (* goal target property *)
  po_updater : Emitter.t ; (* property status updater *)
  po_formula : formula ; (* proof obligation *)
}

module PODatatype =
  Datatype.Make(
    struct
      type t = po
      include Datatype.Undefined
      let name = "Wpo.po"
      let repr phi = { 
	po_idx = Function(List.hd Kernel_function.reprs,Some "default") ;
        po_pid = List.hd WpPropId.PropId.reprs;
        po_gid = "xxx";
        po_updater = List.hd Emitter.reprs;
        po_name = "dummy";
	po_formula = phi ;
      } 
      let reprs = [ repr (Legacy VC_Legacy.repr) ]
    end)

module ProverType =
  Datatype.Make
    (struct
      type t = prover
      include Datatype.Undefined
      let name = "Wpo.prover"
      let reprs = [ AltErgo; Coq; WP; Why "z3" ]
     end)

module ResultType =
  Datatype.Make
    (struct
        type t = result
        include Datatype.Undefined
        let name = "Wpo.result"
        let reprs =
	  List.map VCS.result
	    [ Valid ; Invalid ; Unknown ; Timeout ; Computing ; Failed ]
     end)

(* -------------------------------------------------------------------------- *)
(* --- Proof Collector                                                    --- *)
(* -------------------------------------------------------------------------- *)

module Hproof = Hashtbl.Make(Datatype.Pair(Datatype.String)(Property))
  (* Table indexed by ( Model name , Property proved ) *)

module Results =
struct
  type t = (prover,result) Hashtbl.t
  let create () = Hashtbl.create 7
  let replace = Hashtbl.replace
  let get (t:t) p = try Hashtbl.find t p with Not_found -> VCS.no_result
  let iter = Hashtbl.iter
end

(* -------------------------------------------------------------------------- *)
(* --- Wpo Database                                                       --- *)
(* -------------------------------------------------------------------------- *)

module WPO =
struct
  type t = po
  let hash t = Hashtbl.hash t.po_gid
  let equal a b = (a.po_gid = b.po_gid)
  let compare a b =
    let c = String.compare a.po_name b.po_name in
    if c<>0 then c else
      let c = WpPropId.compare_prop_id a.po_pid b.po_pid in
      if c<>0 then c else String.compare a.po_gid b.po_gid
end
      
module Index =
struct
  type t = index
  let compare a b =
    match a,b with
      | Lemma a , Lemma b -> String.compare a b
      | Lemma _ , Function _ -> (-1)
      | Function _ , Lemma _ -> 1
      | Function(f,a) , Function(g,b) -> 
	  let c = 
            if Kernel_function.equal f g then 0 else
	      String.compare 
		(Kernel_function.get_name f) 
		(Kernel_function.get_name g)
	  in
	  if c=0 then match a,b with
	    | Some a,Some b -> String.compare a b
	    | None,Some _ -> (-1)
	    | Some _,None -> 1
	    | None,None -> 0
	  else c
end

module Gmap = Map.Make(Index)
module Emap = Map.Make(String)
module WPOset = Set.Make(WPO)
module Hpo = Hashtbl.Make(WPO)

type system = {
  mutable environments : int Emap.t ;
  (* context name -> # of environment *)
  mutable last : (string * int * string) option ;
  (* last environment generated *)
  mutable index : WPOset.t Gmap.t ;
  (* all Wpo added, indexed by kernel-function and behavior *)
  proofs : WpAnnot.proof Hproof.t ;
  (* proof collector *)
  results : Results.t Hpo.t ;
  (* results collector *)
}


let create_system () =
  {
    last = None ;
    environments = Emap.empty ;
    index = Gmap.empty ;
    proofs = Hproof.create 131 ;
    results = Hpo.create 131 ;
  }

let clear_system system =
  begin
    system.index <- Gmap.empty ;
    Hproof.clear system.proofs ;
    Hpo.clear system.results ;
  end

module SYSTEM = State_builder.Ref
  (Datatype.Make
     (struct
        include Datatype.Undefined
        type t = system
         let name = "Wpo.SYSTEM.Datatype"
         let reprs = [ create_system () ]
         let mem_project = Datatype.never_any_project
        end))
    (struct
       let name = "Wpo.SYSTEM.System"
       let dependencies = [ Ast.self ]
       let default = create_system
     end)

let clear () = clear_system (SYSTEM.get ())

(* -------------------------------------------------------------------------- *)
(* --- Getters                                                            --- *)
(* -------------------------------------------------------------------------- *)

let get_gid =
  Dynamic.register
    ~plugin:"Wp" "Wpo.get_gid" ~journalize:false
    (Datatype.func PODatatype.ty Datatype.string)
    (fun g -> g.po_gid)

let get_property =
  Dynamic.register
    ~plugin:"Wp" "Wpo.get_property" ~journalize:false
    (Datatype.func PODatatype.ty Property.ty)
    (fun g -> WpPropId.property_of_id g.po_pid)

let get_index w = w.po_idx
let get_label w = WpPropId.label_of_prop_id w.po_pid
let get_model = function
  | { po_formula = Legacy { VC_Legacy.mid = mid } } -> mid
  | { po_formula = GoalAnnot { VC_Annot.model = m } } 
  | { po_formula = GoalLemma { VC_Lemma.model = m } } 
    -> Model.get_name m
let get_depend = function 
  | { po_formula = Legacy { VC_Legacy.dep = ips } } -> ips
  | { po_formula = GoalAnnot { VC_Annot.deps = ips } } -> 
      Property.Set.elements ips
  | { po_formula = GoalLemma { VC_Lemma.depends = ips } } -> 
      List.map LogicUsage.ip_lemma ips

(* ------------------------------------------------------------------------ *)
(* ---  WPO Construction                                                --- *)
(* ------------------------------------------------------------------------ *)

(* A WPO is uniquely determined by :
   1. The model name (unique per updater by construction)
   2. The kernel-function
   3. The behavior
   4. The target prop-id
*)

let gid ~model ~propid =
  let gname = WpPropId.get_propid propid in
  Printf.sprintf "%s_%s" model gname

(* -------------------------------------------------------------------------- *)
(* --- Registry of POs                                                    --- *)
(* -------------------------------------------------------------------------- *)

let env_name model k = Printf.sprintf "%s_env%d" model k

let new_env ~context =
  let system = SYSTEM.get () in
  let k =
    try succ (Emap.find context system.environments)
    with Not_found -> 1 in
  system.environments <- Emap.add context k system.environments ;
  let env = env_name context k in
  system.last <- Some (context,k,env) ; env

let release_env ~env =
  let system = SYSTEM.get () in
  match system.last with
    | Some (model,k0,env0) ->
        if env0 = env then
          system.environments <-
            Emap.add model (pred k0) system.environments
    | None -> ()

let add g =
  let system = SYSTEM.get () in
  begin
    let pset =
      try Gmap.find g.po_idx system.index
      with Not_found -> WPOset.empty
    in
    if WPOset.mem g pset then
      begin
        let pi = ( get_model g , WpPropId.property_of_id g.po_pid ) in
        Hproof.remove system.proofs pi ;
        Hpo.remove system.results g ;
      end ;
    let pset = WPOset.add g pset in
    system.index <- Gmap.add g.po_idx pset system.index ;
  end

let is_trivial = function 
  | { po_formula = GoalAnnot vcq } -> VC_Annot.is_trivial vcq
  | { po_formula = GoalLemma vca } -> VC_Lemma.is_trivial vca
  | _ -> false


let warnings = function
  | { po_formula = Legacy vcl } -> vcl.VC_Legacy.wrn
  | { po_formula = GoalAnnot vcq } -> vcq.VC_Annot.warn
  | { po_formula = GoalLemma _ } -> []

let is_valid = function { verdict=Valid } -> true | _ -> false
let get_time = function { prover_time=t } -> t
let get_steps= function { prover_steps=n } -> n

let is_verdict r = match r.verdict with
  | Valid | Unknown | Invalid | Timeout | Stepout | Failed -> true
  | NoResult | Computing -> false

let get_proof g =
  let system = SYSTEM.get () in
  let target = WpPropId.property_of_id g.po_pid in
  let status =
    try
      let pi = ( get_model g , target ) in
      let proof = Hproof.find system.proofs pi in
      WpAnnot.is_proved proof
    with Not_found -> false
  in status , target

let set_po_result g r =
  let system = SYSTEM.get () in
  try
    let pi = ( get_model g , WpPropId.property_of_id g.po_pid ) in
    let proof =
      try Hproof.find system.proofs pi
      with Not_found ->
        let proof = WpAnnot.create_proof g.po_pid in
        Hproof.add system.proofs pi proof ; proof
    in
    if is_valid r then WpAnnot.add_proof proof g.po_pid (get_depend g) ;
    let status = 
      if WpAnnot.is_proved proof then Property_status.True 
      else Property_status.Dont_know
    in
    let target = WpAnnot.target proof in
    let depends = WpAnnot.dependencies proof in
    Property_status.emit g.po_updater ~hyps:depends target status ;
  with err ->
    Wp_parameters.failure "Update-status failed (%s)" (Printexc.to_string err) ;
    raise err
      
let set_result g p r =
  let system = SYSTEM.get () in
  begin
    let rs =
      try Hpo.find system.results g
      with Not_found ->
        let rs = Results.create () in
        Hpo.add system.results g rs ; rs
    in
    Results.replace rs p r ;
    set_po_result g r ;
    if p = WP then Wp_parameters.result "[WP:simplified] Goal %s : Valid" g.po_gid ;
  end

let get_result g p : VCS.result =
  let system = SYSTEM.get () in
  try Results.get (Hpo.find system.results g) p
  with Not_found -> VCS.no_result

let get_result =
  Dynamic.register ~plugin:"Wp" "Wpo.get_result" ~journalize:false
    (Datatype.func2 PODatatype.ty ProverType.ty ResultType.ty)
    get_result

let is_valid =
  Dynamic.register ~plugin:"Wp" "Wpo.is_valid" ~journalize:false
    (Datatype.func ResultType.ty Datatype.bool) is_valid

let get_results g =
  let system = SYSTEM.get () in
  try
    let a = ref [] in
    Results.iter (fun p r -> a:=(p,r)::!a) (Hpo.find system.results g) ; !a
  with Not_found -> []

(* -------------------------------------------------------------------------- *)
(* --- Proof Obligations : Pretty-printing                                --- *)
(* -------------------------------------------------------------------------- *)

let pp_goal fmt w =
  begin
    match w.po_formula with
      | Legacy vcd -> 
	  Format.fprintf fmt "@[<v 0>Proof Obligation %s:@]@\n" w.po_name ;
	  List.iter
	    (fun (prover,result) ->
	       if result.verdict <> NoResult then
		 Format.fprintf fmt "Prover %a returns %a@\n"
		   pp_prover prover
		   pp_result result
	    ) (get_results w) ;
	  let context = kf_context w.po_idx in
	  VC_Legacy.pp_formula fmt w.po_gid w.po_pid context vcd
      | GoalAnnot vcq -> 
	  VC_Annot.pretty fmt w.po_pid vcq (get_results w)
      | GoalLemma vca -> 
	  VC_Lemma.pretty fmt vca (get_results w)
  end

let pp_goal_flow fmt g =
  begin
    if not !flow then Format.pp_print_newline fmt () ;
    pp_goal fmt g ;
    Format.fprintf fmt "@\n%s@." bar ;
    flow := false ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Iterator                                                           --- *)
(* -------------------------------------------------------------------------- *)

type part =
  | Pnone
  | Plemma of string
  | Pbehavior of kernel_function * string option


let iter ?on_environment ?on_lemma ?on_behavior ?on_goal () =
  let system = SYSTEM.get () in
  begin
    match on_environment with
    | None -> ()
    | Some phi ->
	Emap.iter
          (fun m k ->
             for i = 1 to k do
               phi (env_name m i)
          done)
          system.environments
  end ;
  let current = ref Pnone in
  let apply_lemma a = match on_lemma with None -> () | Some phi -> phi a in
  let apply_behavior f bhv = match on_behavior with None -> () | Some phi -> phi f bhv in
  let on_part idx =
    match !current , idx with
      | Plemma a , Lemma b when a=b -> ()
      | _ , Lemma b -> apply_lemma b ; current := Plemma b
      | Pbehavior(f,None) , Function(g,None) when Kernel_function.equal f g -> ()
      | Pbehavior(f,Some a) , Function(g,Some b) when Kernel_function.equal f g && a=b -> ()
      | _ , Function(g,bhv) -> apply_behavior g bhv ; current := Pbehavior(g,bhv)
  in
  Gmap.iter
    (fun index poset -> 
       if not (WPOset.is_empty poset) then
	 begin
	   on_part index ;
	   match on_goal with
	     | None -> ()
	     | Some phi -> WPOset.iter phi poset
	 end
    ) system.index

let iter_on_goals =
  Dynamic.register ~plugin:"Wp" "Wpo.iter_on_goals"
    (Datatype.func (Datatype.func PODatatype.ty Datatype.unit) Datatype.unit)
    ~journalize:true
    (fun on_goal -> iter ~on_goal ())

let goals_of_property prop =
  let res = ref [] in
  let on_goal po =
    let pid = po.po_pid in
    let p = WpPropId.property_of_id pid in
    if Property.equal p prop then res:= po :: !res
  in
  iter ~on_goal ();
  !res

let goals_of_property =
  Dynamic.register ~plugin:"Wp" "Wpo.goals_of_property"
    (Datatype.func Property.ty (Datatype.list PODatatype.ty))
    ~journalize:false
    goals_of_property

let prover_of_name =
  Dynamic.register ~plugin:"Wp" "Wpo.prover_of_name" ~journalize:false
    (Datatype.func Datatype.string (Datatype.option ProverType.ty))
    VCS.prover_of_name

(* -------------------------------------------------------------------------- *)
(* --- Prover and Files                                                   --- *)
(* -------------------------------------------------------------------------- *)

let get_logfile w prover result =
  match w.po_formula with 
    | Legacy _ -> VC_Legacy.file_for_log_proof ~gid:w.po_gid prover
    | GoalAnnot { VC_Annot.model = model } 
    | GoalLemma { VC_Lemma.model = model } 
      -> DISK.cache_log ~pid:w.po_pid ~model ~prover ~result

let _ =
  Dynamic.register ~plugin:"Wp" "Wpo.file_for_log_proof" ~journalize:false
    (Datatype.func2
       ~label1:("gid",None) Datatype.string ProverType.ty Datatype.string)
    (fun gid p -> VC_Legacy.file_for_log_proof ~gid p)

let get_files w =
  let results = get_results w in
  let descr_files = match w.po_formula with
    | Legacy vcd -> [   
	"Obligation"  , VC_Legacy.file_for_body ~gid:w.po_gid ;
	"Description" , VC_Legacy.file_for_head ~gid:w.po_gid ;
	"Environment" , VC_Legacy.file_for_ctxt ~env:vcd.VC_Legacy.env ;
      ]
    | GoalAnnot vcq -> 
	[ "Goal" , VC_Annot.cache_descr ~pid:w.po_pid vcq results ] 
    | GoalLemma vca -> 
	[ "Lemma" , VC_Lemma.cache_descr vca results ] 
  in
  let result_files = 
    List.fold_right
      (fun (prover,result) files ->
	 if prover <> VCS.WP && result.verdict <> VCS.Computing then
	   let filename = get_logfile w prover result in
	   if filename <> "" && Sys.file_exists filename then
	     let title = name_of_prover prover in
	     (title,filename) :: files
	   else files
	 else files
      ) results []
  in
  descr_files @ result_files

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

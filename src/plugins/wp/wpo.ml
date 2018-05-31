(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

open LogicUsage
open VCS
open Cil_types
open Cil_datatype
open Lang

type index =
  | Axiomatic of string option
  | Function of kernel_function * string option

let bar = String.make 60 '-'
let flow = ref false

(* -------------------------------------------------------------------------- *)
(* --- Pretty Printers                                                    --- *)
(* -------------------------------------------------------------------------- *)

let pp_index fmt = function
  | Axiomatic None -> Format.pp_print_string fmt "Axiomatics"
  | Axiomatic (Some a) -> Format.pp_print_string fmt a
  | Function(f,None) -> Kernel_function.pretty fmt f
  | Function(f,Some b) -> Format.fprintf fmt "%a for %s:" Kernel_function.pretty f b

let pp_axiomatics fmt ax =
  flow := true ;
  match ax with
  | None -> Format.fprintf fmt "%s@\n  Global@\n%s@\n@\n" bar bar
  | Some a -> Format.fprintf fmt "%s@\n  Axiomatic '%s'@\n%s@\n@\n" bar a bar

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

let kf_context = function Axiomatic _ -> `Always | Function(kf,_) -> `Context kf

let pp_dependency context fmt d =
  Format.fprintf fmt " - Assumes %a"
    (Description.pp_localized ~kf:context ~ki:false ~kloc:true) d

let pp_dependencies context fmt ds =
  List.iter (fun d -> Format.fprintf fmt "%a@\n" (pp_dependency context) d) ds

let pp_depend fmt d =
  Format.fprintf fmt " - Assumes %a"
    (Description.pp_localized ~kf:`Always ~ki:false ~kloc:true) d

(* ------------------------------------------------------------------------ *)
(* ---  Proof Obligations Definition                                    --- *)
(* ------------------------------------------------------------------------ *)

module DISK =
struct

  let file ~id ~model ?prover ?suffix ~ext () =
    let dir = Wp_parameters.get_output () in
    let mid = Model.get_id model in
    let buffer = Buffer.create 80 in
    let fmt = Format.formatter_of_buffer buffer in
    Format.fprintf fmt "%s/%s/%s" dir mid id ;
    (match prover with None -> () | Some p ->
      Format.fprintf fmt "_%s" (filename_for_prover p)) ;
    (match suffix with None -> () | Some s ->
      Format.fprintf fmt "_%s" s) ;
    Format.fprintf fmt ".%s" ext ;
    Format.pp_print_flush fmt ();
    Buffer.contents buffer

  let file_logout ~pid ~model ~prover =
    let id = WpPropId.get_propid pid in
    file ~id ~model ~prover ~ext:"out" ()
  let file_logerr ~pid ~model ~prover =
    let id = WpPropId.get_propid pid in
    file ~id ~model ~prover ~ext:"err" ()
  let file_goal ~pid ~model ~prover =
    let ext = match prover with
      | Qed -> "qed"
      | AltErgo -> "mlw"
      | Why3 _ -> "why"
      | Why3ide -> "why"
      | Coq -> "v"
      | Tactical -> "tac"
    in
    let id = WpPropId.get_propid pid in
    file ~id ~model ~prover ~ext ()

  let file_kf ~kf ~model ~prover =
    let ext = match prover with
      | Qed -> "qed"
      | AltErgo -> "mlw"
      | Why3 _ -> "why"
      | Why3ide -> "why"
      | Coq -> "v"
      | Tactical -> "tac"
    in
    let id = (Kf.vi kf).vname in
    file ~id ~model ~prover ~ext ()

  let dump_file fmt title file =
    if Sys.file_exists file then
      begin
        Format.fprintf fmt "--- %s ---------------------------------@\n" title ;
        Command.pp_from_file fmt file
      end

  let pretty ~pid ~model ~prover ~result fmt =
    begin
      Format.fprintf fmt "[%a] Goal %a : %a@\n"
        pp_prover prover WpPropId.pp_propid pid pp_result result ;
      dump_file fmt "StdOut" (file_logout ~pid ~model ~prover) ;
      dump_file fmt "StdErr" (file_logerr ~pid ~model ~prover) ;
    end

  let cache_log ~pid ~model ~prover ~result =
    (*TODO: put a cache here *)
    let dir = Wp_parameters.get_output () in
    let file = Printf.sprintf "%s/log.txt" dir in
    Command.print_file file (pretty ~pid ~model ~prover ~result) ;
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
    mutable time : float ;
    mutable simplified : bool ;
    mutable sequent : Conditions.sequent ;
    mutable obligation : F.pred ;
  }

  let empty = Conditions.empty

  let dummy = {
    time = 0.0 ;
    simplified = false ;
    sequent = empty , F.p_false ;
    obligation = F.p_false ;
  }

  let trivial = {
    time = 0.0 ;
    simplified = true ;
    sequent = empty , F.p_true ;
    obligation = F.p_true ;
  }

  let make sequent = {
    time = 0.0 ;
    simplified = false ;
    sequent = sequent ;
    obligation = F.p_false ;
  }

  let is_trivial g = Conditions.is_trivial g.sequent

  let apply phi g = g.sequent <- phi g.sequent

  let default_simplifiers = [
    Wp_parameters.SimplifyIsCint.get, Cint.is_cint_simplifier ;
    Wp_parameters.SimplifyLandMask.get, Cint.mask_simplifier ;
  ]

  let preprocess g =
    if Wp_parameters.Let.get () then
      begin
        apply Conditions.introduction g ;
        let fold acc (get,solver) = if get () then solver::acc else acc in
        let solvers = List.fold_left fold [] default_simplifiers in
        apply (Conditions.simplify ~solvers) g ;
        if Wp_parameters.Prune.get ()
        then apply (Conditions.pruning ~solvers) g ;
        if Wp_parameters.Filter.get ()
        then apply Conditions.filter g ;
        if Wp_parameters.Parasite.get ()
        then apply Conditions.parasite g
      end
    else
      begin
        if Wp_parameters.Clean.get ()
        then apply Conditions.clean g ;
      end ;
    if Conditions.is_trivial g.sequent then
      g.sequent <- Conditions.trivial ;
    g.obligation <- Conditions.close g.sequent

  let dkey = Wp_parameters.register_category "prover"
  let compute g =
    if not g.simplified then
      begin
        g.simplified <- true ;
        let timer = ref 0.0 in
        Wp_parameters.debug ~dkey "Simplify goal" ;
        Command.time ~rmax:timer preprocess g ;
        Wp_parameters.debug ~dkey "Simplification time: %a"
          Rformat.pp_time !timer ;
        g.time <- !timer ;
      end

  let compute_proof g = compute g ; g.obligation
  let compute_descr g = compute g ; g.sequent
  let get_descr g = g.sequent
  let qed_time g = g.time

end

module VC_Lemma =
struct

  open Definitions

  type t = {
    lemma : Definitions.dlemma ;
    depends : logic_lemma list ;
    mutable sequent : Conditions.sequent option ;
  }

  let is_trivial vc = vc.lemma.l_lemma == F.p_true

  let sequent vc =
    match vc.sequent with
    | Some s -> s
    | None ->
        let s = Conditions.lemma vc.lemma.l_lemma in
        vc.sequent <- Some s ; s
  
  let pretty fmt vc results =
    begin
      Format.fprintf fmt "Lemma %s:@\n" vc.lemma.l_name ;
      if vc.depends <> [] then
        begin
          Format.fprintf fmt "@[<hov 2>@{<bf>Assume@}:" ;
          List.iter
            (fun a -> Format.fprintf fmt "@ '%s'" a.lem_name)
            vc.depends ;
          Format.fprintf fmt "@]@." ;
        end ;
      let env = F.env (List.fold_right F.Vars.add vc.lemma.l_forall F.Vars.empty) in
      Format.fprintf fmt "@{<bf>Prove@}: @[<hov 2>%a@]@." (F.pp_epred env) vc.lemma.l_lemma ;
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
    (* Generally empty, but for Lemma sub-goals *)
    axioms : Definitions.axioms option ;
    goal : GOAL.t ;
    tags : Splitter.tag list ;
    warn : Warning.t list ;
    deps : Property.Set.t ;
    path : Stmt.Set.t ;
    effect : (stmt * WpPropId.effect_source) option ;
  }

  let repr = {
    axioms = None ;
    goal = GOAL.dummy ;
    tags = [] ;
    warn = [] ;
    deps = Property.Set.empty ;
    path = Stmt.Set.empty ;
    effect = None ;
  }

  let resolve vcq = GOAL.compute_proof vcq.goal == Lang.F.p_true
  let is_trivial vcq = GOAL.is_trivial vcq.goal

  let pp_effect fmt = function
    | None -> ()
    | Some(s,e) ->
        let loc = fst (Stmt.loc s) in
        let line = loc.Lexing.pos_lnum in
        let desc = match e with
          | WpPropId.FromCode -> "Effect"
          | WpPropId.FromCall -> "Call Effect"
          | WpPropId.FromReturn -> "Call Result"
        in
        Format.fprintf fmt "%s at line %d@\n" desc line

  let pretty fmt pid vc results =
    begin
      Format.fprintf fmt "@{<bf>Goal@} %a:@\n" WpPropId.pretty pid ;
      pp_effect fmt vc.effect ;
      if vc.tags <> [] then
        begin
          Format.fprintf fmt "@[<hov 2>@{<bf>Tags@}:" ;
          List.iter (fun tg -> Format.fprintf fmt "@ %a" Splitter.pretty tg) vc.tags ;
          Format.fprintf fmt "@].@\n" ;
        end ;
      pp_warnings fmt vc.warn ;
      Pcond.pretty fmt (GOAL.compute_descr vc.goal) ;
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
(* ---  VC-Check                                                        --- *)
(* ------------------------------------------------------------------------ *)

module VC_Check =
struct
  type t = { qed : F.term ; raw : F.term ; goal : F.pred }
  let pretty fmt v =
    Format.fprintf fmt "Class %d - instance %d@\n"
      (F.QED.id v.qed) (F.QED.id v.raw) ;
    Format.fprintf fmt "@[<hov 2>Prove %a@]@."
      F.pp_pred v.goal
end

(* ------------------------------------------------------------------------ *)
(* ---  Proof Obligations Database                                      --- *)
(* ------------------------------------------------------------------------ *)

type formula =
  | GoalLemma of VC_Lemma.t
  | GoalAnnot of VC_Annot.t
  | GoalCheck of VC_Check.t

type po = t and t = {
    po_gid   : string ;  (* goal identifier *)
    po_sid   : string ;  (* goal short identifier (without model) *)
    po_name  : string ;  (* goal informal name *)
    po_idx   : index ;   (* goal index *)
    po_model : Model.t ;
    po_pid   : WpPropId.prop_id ; (* goal target property *)
    po_formula : formula ; (* proof obligation *)
  }

let get_index w = w.po_idx
let get_label w = WpPropId.label_of_prop_id w.po_pid
let get_model x = x.po_model
let get_model_id w = Model.get_id (get_model w)
let get_model_name w = Model.get_descr (get_model w)
let get_depend = function
  | { po_formula = GoalAnnot { VC_Annot.deps = ips } } ->
      Property.Set.elements ips
  | { po_formula = GoalLemma { VC_Lemma.depends = ips } } ->
      List.map LogicUsage.ip_lemma ips
  | { po_formula = GoalCheck _ } -> []

let get_file_logout w prover =
  DISK.file_logout ~pid:w.po_pid ~model:(get_model w) ~prover

let get_file_logerr w prover =
  DISK.file_logerr ~pid:w.po_pid ~model:(get_model w) ~prover

module Index =
struct
  type t = index
  let cmpopt a b =
    match a,b with
    | Some a,Some b -> String.compare a b
    | None,Some _ -> (-1)
    | Some _,None -> 1
    | None,None -> 0
  let compare a b =
    match a,b with
    | Axiomatic a , Axiomatic b -> cmpopt a b
    | Axiomatic _ , Function _ -> (-1)
    | Function _ , Axiomatic _ -> 1
    | Function(f,a) , Function(g,b) ->
        let c =
          if Kernel_function.equal f g then 0 else
            String.compare
              (Kernel_function.get_name f)
              (Kernel_function.get_name g)
        in
        if c=0 then cmpopt a b else c
end

module S =
  Datatype.Make_with_collections
    (struct
      type t = po
      include Datatype.Undefined
      let hash a = FCHashtbl.hash a.po_gid
      let equal a b = (a.po_gid = b.po_gid)
      let compare a b =
        let c = Index.compare a.po_idx b.po_idx in
        if c<>0 then c else
          let c = WpPropId.compare_prop_id a.po_pid b.po_pid in
          if c<>0 then c else
            let ma = get_model_name a in
            let mb = get_model_name b in
            let c = String.compare ma mb in
            if c<>0 then c else
              String.compare a.po_gid b.po_gid
      let pretty fmt wpo = Format.pp_print_string fmt wpo.po_name
      let name = "Wpo.po"
      let reprs =
        [{
          po_idx = Function(List.hd Kernel_function.reprs,Some "default") ;
          po_pid = List.hd WpPropId.PropId.reprs;
          po_sid = "xxx";
          po_gid = "xxx";
          po_model = Model.repr ;
          po_name = "dummy";
          po_formula = GoalAnnot VC_Annot.repr ;
        }]
    end)
(* to get a "reasonable" API doc: *)
let () = Type.set_ml_name S.ty (Some "Wpo.po")

module WpoType = S
module ProverType =
  Datatype.Make
    (struct
      type t = prover
      include Datatype.Undefined
      let name = "Wpo.prover"
      let reprs = [ AltErgo; Coq; Qed; Why3 "z3" ]
    end)
(* to get a "reasonable" API doc: *)
let () = Type.set_ml_name ProverType.ty (Some "Wpo.prover")

module ResultType =
  Datatype.Make
    (struct
      type t = result
      include Datatype.Undefined
      let name = "Wpo.result"
      let reprs =
        List.map VCS.result
          [ Valid ; Invalid ; Unknown ; Timeout ; Failed ]
    end)
(* to get a "reasonable" API doc *)
let () = Type.set_ml_name ResultType.ty (Some "Wpo.result")

(* -------------------------------------------------------------------------- *)
(* --- Getters                                                            --- *)
(* -------------------------------------------------------------------------- *)

let get_gid =
  Dynamic.register
    ~plugin:"Wp" "Wpo.get_gid" ~journalize:false
    (Datatype.func WpoType.ty Datatype.string)
    (fun g -> g.po_gid)

let get_property =
  Dynamic.register
    ~plugin:"Wp" "Wpo.get_property" ~journalize:false
    (Datatype.func WpoType.ty Property.ty)
    (fun g -> WpPropId.property_of_id g.po_pid)

let qed_time wpo =
  match wpo.po_formula with
  | GoalCheck _ | GoalLemma _ -> 0.0
  | GoalAnnot { VC_Annot.goal = g } -> GOAL.qed_time g

(* -------------------------------------------------------------------------- *)
(* --- Proof Collector                                                    --- *)
(* -------------------------------------------------------------------------- *)

let is_check t = match t.po_formula with
  | GoalCheck _ -> true
  | _ -> false

let is_tactic t = WpPropId.is_tactic t.po_pid

module Hproof = Hashtbl.Make(Datatype.Pair(Datatype.String)(Property))
(* Table indexed by ( Model name , Property proved ) *)

module Results =
struct

  module Cmap = Map.Make(String)

  type t = {
    mutable dps : result Pmap.t ;
    mutable cps : result Cmap.t ;
    (* result per class of Why3 provers *)
  }

  let not_computing _ r =
    match r.verdict with VCS.Computing _ -> false | _ -> true

  let class_of_prover = function
    | Qed | Tactical | AltErgo | Coq | Why3ide -> None
    | Why3 dp ->
        let cp =
          try String.sub dp 0 (String.index dp ':')
          with Not_found -> dp
        in Some (Transitioning.String.uppercase_ascii cp)

  let create () = { dps = Pmap.empty ; cps = Cmap.empty }

  let get w p =
    try Pmap.find p w.dps
    with Not_found ->
      match class_of_prover p with
      | None -> VCS.no_result
      | Some cp ->
          try Cmap.find cp w.cps
          with Not_found -> VCS.no_result

  let clear w = w.dps <- Pmap.empty ; w.cps <- Cmap.empty
  
  let replace w p r =
    begin
      if p = Qed then
        begin
          w.dps <- Pmap.filter not_computing w.dps ;
          w.cps <- Cmap.filter not_computing w.cps ;
        end ;
      w.dps <- Pmap.add p r w.dps ;
      match class_of_prover p with
      | None -> ()
      | Some c -> w.cps <- Cmap.add c r w.cps
    end

  let list w =
    Pmap.fold
      (fun p r w ->
         if is_verdict r then (p,r)::w else w
      ) w.dps []

end

(* -------------------------------------------------------------------------- *)
(* --- Wpo Database                                                       --- *)
(* -------------------------------------------------------------------------- *)

module WPOset = WpoType.Set
module WPOmap = WpoType.Map
module Gmap = FCMap.Make(Index)
module Fmap = Kernel_function.Map
module Pmap = Property.Map

let index_wpo iadd iget k w m =
  let set = try iget k m with Not_found -> WPOset.empty in
  iadd k (WPOset.add w set) m

let unindex_wpo iadd iget k w m =
  try
    let set = iget k m in
    iadd k (WPOset.remove w set) m
  with Not_found -> m

type system = {
  mutable wpo_idx : WPOset.t Gmap.t ; (* index -> WPOs *)
  mutable wpo_kf : WPOset.t Fmap.t ; (* kf -> WPOs *)
  mutable wpo_ip : WPOset.t Pmap.t ; (* ip -> WPOs *)
  mutable age : int WPOmap.t ; (* wpo -> age *)
  mutable results : Results.t WPOmap.t ; (* results collector *)
  proofs : WpAnnot.proof Hproof.t ; (* proof collector *)
}

let create_system () =
  {
    wpo_idx = Gmap.empty ;
    wpo_kf = Fmap.empty ;
    wpo_ip = Pmap.empty ;
    results = WPOmap.empty ;
    age = WPOmap.empty ;
    proofs = Hproof.create 131 ;
  }

let clear_system system =
  begin
    system.wpo_idx <- Gmap.empty ;
    system.wpo_kf <- Fmap.empty ;
    system.wpo_ip <- Pmap.empty ;
    system.results <- WPOmap.empty ;
    system.age <- WPOmap.empty ;
    Hproof.clear system.proofs ;
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

(* ------------------------------------------------------------------------ *)
(* ---  WPO Construction                                                --- *)
(* ------------------------------------------------------------------------ *)

(* A WPO is uniquely determined by :
   1. The model name (unique per updater by construction)
   2. The kernel-function
   3. The behavior
   4. The target prop-id
*)

(* -------------------------------------------------------------------------- *)
(* --- Registry of POs                                                    --- *)
(* -------------------------------------------------------------------------- *)

let added = ref 0

let age g =
  let system = SYSTEM.get () in
  try WPOmap.find g system.age with Not_found -> 0

let current_age = ref (-1)

let add g =
  let system = SYSTEM.get () in
  begin
    let ip = WpPropId.property_of_id g.po_pid in
    let proof = ( get_model_id g , ip ) in
    Hproof.remove system.proofs proof ;
    let age = incr current_age; !current_age in
    system.age <- WPOmap.add g age system.age ;
    system.results <- WPOmap.remove g system.results ;
    system.wpo_idx <- index_wpo Gmap.add Gmap.find g.po_idx g system.wpo_idx ;
    system.wpo_ip <- index_wpo Pmap.add Pmap.find ip g system.wpo_ip ;
    begin
      match g.po_idx with
      | Function(kf,_) ->
          system.wpo_kf <- index_wpo Fmap.add Fmap.find kf g system.wpo_kf
      | _ -> ()
    end ;
    incr added ;
    if !added >= 100 then
      begin
        added := 0 ;
        Gmap.iter
          (fun _ ws -> WPOset.iter (fun _ -> incr added) ws)
          system.wpo_idx ;
        if not (Wp_parameters.has_dkey VCS.dkey_no_goals_info) then
          Wp_parameters.feedback ~ontty:`Feedback "Computing [%d goals...]" !added ;
        added := 0 ;
      end ;
  end

let remove_hook = ref []
let on_remove f = remove_hook := !remove_hook @ [f]

let remove g =
  let system = SYSTEM.get () in
  begin
    List.iter (fun f -> f g) !remove_hook ;
    let ip = WpPropId.property_of_id g.po_pid in
    system.wpo_idx <- unindex_wpo Gmap.add Gmap.find g.po_idx g system.wpo_idx ;
    system.wpo_ip <- unindex_wpo Pmap.add Pmap.find ip g system.wpo_ip ;
    begin
      match g.po_idx with
      | Function(kf,_) ->
          system.wpo_kf <- unindex_wpo Fmap.add Fmap.find kf g system.wpo_kf
      | Axiomatic _ -> ()
    end ;
    system.results <- WPOmap.remove g system.results ;
    Hproof.remove system.proofs (get_model_id g , ip ) ;
  end

let warnings = function
  | { po_formula = GoalAnnot vcq } -> vcq.VC_Annot.warn
  | { po_formula = GoalLemma _ } -> []
  | { po_formula = GoalCheck _ } -> []

let get_time = function { prover_time=t } -> t
let get_steps= function { prover_steps=n } -> n

let get_proof g =
  let system = SYSTEM.get () in
  let target = WpPropId.property_of_id g.po_pid in
  let status =
    try
      let pi = ( get_model_id g , target ) in
      let proof = Hproof.find system.proofs pi in
      WpAnnot.is_proved proof
    with Not_found -> false
  in status , target

let update_property_status g r =
  let system = SYSTEM.get () in
  try
    let pi = ( get_model_id g , WpPropId.property_of_id g.po_pid ) in
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
    let emitter = Model.get_emitter g.po_model in
    Property_status.emit emitter ~hyps:depends target status ;
  with err ->
    Wp_parameters.failure "Update-status failed (%s)" (Printexc.to_string err) ;
    raise err

let clear_results g =
  let system = SYSTEM.get () in
  try
    let rs = WPOmap.find g system.results in
    Results.clear rs ;
  with Not_found -> ()

let set_result g p r =
  let system = SYSTEM.get () in
  begin
    let rs =
      try WPOmap.find g system.results
      with Not_found ->
        let rs = Results.create () in
        system.results <- WPOmap.add g rs system.results ; rs
    in
    Results.replace rs p r ;
    if not (WpPropId.is_check g.po_pid) &&
       not (WpPropId.is_tactic g.po_pid)
    then
      update_property_status g r ;
  end

let has_verdict g p =
  let system = SYSTEM.get () in
  try VCS.is_verdict (Results.get (WPOmap.find g system.results) p)
  with Not_found -> false

let get_result g p : VCS.result =
  let system = SYSTEM.get () in
  try Results.get (WPOmap.find g system.results) p
  with Not_found -> VCS.no_result

let get_results g =
  let system = SYSTEM.get () in
  try Results.list (WPOmap.find g system.results)
  with Not_found -> []

let is_trivial g =
  match g.po_formula with
  | GoalLemma vc -> VC_Lemma.is_trivial vc
  | GoalAnnot vc -> VC_Annot.is_trivial vc
  | GoalCheck _ -> false

let resolve g =
  match g.po_formula with
  | GoalAnnot vc -> Model.with_model g.po_model VC_Annot.resolve vc
  | GoalLemma vc -> Model.with_model g.po_model VC_Lemma.is_trivial vc
  | GoalCheck _ -> false

let compute g =
  match g.po_formula with
  | GoalAnnot { VC_Annot.axioms ; VC_Annot.goal = goal } ->
      axioms , Model.with_model g.po_model GOAL.compute_descr goal
  | GoalLemma ({ VC_Lemma.depends = depends ; VC_Lemma.lemma = lemma } as w) ->
      let open Definitions in
      Some( lemma.l_cluster , depends ) ,
      Model.with_model g.po_model VC_Lemma.sequent w
  | GoalCheck { VC_Check.goal = goal } ->
      None , Model.with_model g.po_model Conditions.lemma goal

let is_proved g =
  is_trivial g || List.exists (fun (_,r) -> VCS.is_valid r) (get_results g)

let is_unknown g = List.exists 
    (fun (_,r) -> VCS.is_verdict r && not (VCS.is_valid r))
    ( get_results g )

let get_result =
  Dynamic.register ~plugin:"Wp" "Wpo.get_result" ~journalize:false
    (Datatype.func2 WpoType.ty ProverType.ty ResultType.ty)
    get_result

let is_valid =
  Dynamic.register ~plugin:"Wp" "Wpo.is_valid" ~journalize:false
    (Datatype.func ResultType.ty Datatype.bool) VCS.is_valid

(* -------------------------------------------------------------------------- *)
(* --- Proof Obligations : Pretty-printing                                --- *)
(* -------------------------------------------------------------------------- *)

let pp_title fmt w = Format.pp_print_string fmt w.po_name

let pp_goal_model fmt w =
  begin
    match w.po_formula with
    | GoalAnnot vcq ->
        VC_Annot.pretty fmt w.po_pid vcq (get_results w)
    | GoalLemma vca ->
        VC_Lemma.pretty fmt vca (get_results w)
    | GoalCheck vck ->
        VC_Check.pretty fmt vck
  end

let pp_goal fmt w = Model.with_model w.po_model (pp_goal_model fmt) w

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
  | Paxiomatic of string option
  | Pbehavior of kernel_function * string option


let iter ?ip ?index ?on_axiomatics ?on_behavior ?on_goal () =
  let system = SYSTEM.get () in
  let current = ref Pnone in
  let apply_lemma a =
    match on_axiomatics with None -> () | Some phi -> phi a in
  let apply_behavior f bhv =
    match on_behavior with None -> () | Some phi -> phi f bhv in
  let on_part idx =
    match !current , idx with
    | Paxiomatic a , Axiomatic b when a=b -> ()
    | _ , Axiomatic b -> apply_lemma b ; current := Paxiomatic b
    | Pbehavior(f,None) , Function(g,None) when Kernel_function.equal f g -> ()
    | Pbehavior(f,Some a) , Function(g,Some b) when Kernel_function.equal f g && a=b -> ()
    | _ , Function(g,bhv) -> apply_behavior g bhv ; current := Pbehavior(g,bhv)
  in
  let on_goals poset =
    if not (WPOset.is_empty poset) then
      begin
        match on_goal with
        | None -> ()
        | Some phi -> WPOset.iter phi poset
      end
  in
  match index,ip with
  | None,None ->
      Gmap.iter (fun idx ws -> on_part idx ; on_goals ws) system.wpo_idx
  | _,Some ip ->
      begin
        match on_goal with
        | None -> ()
        | Some phi ->
            let poset =
              try Pmap.find ip system.wpo_ip
              with Not_found -> WPOset.empty in
            WPOset.iter phi poset
      end
  | Some (Function(kf,None)),None ->
      begin
        try on_goals (Fmap.find kf system.wpo_kf)
        with Not_found -> ()
      end
  | Some idx,None ->
      begin
        try on_goals (Gmap.find idx system.wpo_idx)
        with Not_found -> ()
      end

let iter_on_goals =
  Dynamic.register ~plugin:"Wp" "Wpo.iter_on_goals"
    (Datatype.func (Datatype.func WpoType.ty Datatype.unit) Datatype.unit)
    ~journalize:true
    (fun on_goal -> iter ~on_goal ())

let goals_of_property prop =
  let system = SYSTEM.get () in
  let poset =
    try Pmap.find prop system.wpo_ip
    with Not_found -> WPOset.empty
  in
  WPOset.elements poset

let goals_of_property =
  Dynamic.register ~plugin:"Wp" "Wpo.goals_of_property"
    (Datatype.func Property.ty (Datatype.list WpoType.ty))
    ~journalize:false
    goals_of_property

let prover_of_name =
  Dynamic.register ~plugin:"Wp" "Wpo.prover_of_name" ~journalize:false
    (Datatype.func Datatype.string (Datatype.option ProverType.ty))
    VCS.prover_of_name

(* -------------------------------------------------------------------------- *)
(* --- Prover and Files                                                   --- *)
(* -------------------------------------------------------------------------- *)

let get_model w = w.po_model

let get_logfile w prover result =
  let model = get_model w in
  DISK.cache_log ~pid:w.po_pid ~model ~prover ~result

let _ =
  Dynamic.register ~plugin:"Wp" "Wpo.file_for_log_proof" ~journalize:false
    (Datatype.func2
       WpoType.ty ProverType.ty
       (Datatype.pair Datatype.string Datatype.string))
    (fun w p ->
       (DISK.file_logout w.po_pid (get_model w) p,
        DISK.file_logerr w.po_pid (get_model w) p))

let pp_logfile fmt w prover =
  let model = get_model w in
  let result = get_result w prover in
  DISK.pretty ~pid:w.po_pid ~model ~prover ~result fmt

let is_computing = function VCS.Computing _ -> true | _ -> false

let get_files w =
  let results = get_results w in
  let descr_files = match w.po_formula with
    | GoalAnnot vcq ->
        [ "Goal" , VC_Annot.cache_descr ~pid:w.po_pid vcq results ]
    | GoalLemma vca ->
        [ "Lemma" , VC_Lemma.cache_descr vca results ]
    | GoalCheck _ -> []
  in
  let result_files =
    List.fold_right
      (fun (prover,result) files ->
         if prover <> VCS.Qed && not (is_computing result.verdict) then
           let filename = get_logfile w prover result in
           if filename <> "" && Sys.file_exists filename then
             let title = title_of_prover prover in
             (title,filename) :: files
           else files
         else files
      ) results []
  in
  descr_files @ result_files

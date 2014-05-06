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
(* --- Compilation of ACSL Logic-Info                                     --- *)
(* -------------------------------------------------------------------------- *)

open LogicUsage
open LogicBuiltins
open Cil_types
open Cil_datatype
open Clabels
open Ctypes
open Lang
open Lang.F
open Memory
open Definitions

module Make( M : Memory.Model ) =
struct

  (* -------------------------------------------------------------------------- *)
  (* --- Definitions                                                        --- *)
  (* -------------------------------------------------------------------------- *)

  open M

  type value = M.loc Memory.value
  type logic = M.loc Memory.logic
  type sigma = M.Sigma.t
  type chunk = M.Chunk.t

  type signature = 
    | CST of Integer.t
    | SIG of sig_param list
  and sig_param =
    | Sig_value of logic_var (* to be replaced by the value *)
    | Sig_chunk of chunk * c_label (* to be replaced by the chunk variable *)

  (* -------------------------------------------------------------------------- *)
  (* --- Utilities                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  let rec wrap_lvar xs vs =
    match xs , vs with
      | x::xs , v::vs -> Logic_var.Map.add x v (wrap_lvar xs vs)
      | _ -> Logic_var.Map.empty

  let rec wrap_var xs vs =
    match xs , vs with
      | x::xs , v::vs -> Varinfo.Map.add x v (wrap_var xs vs)
      | _ -> Varinfo.Map.empty

  let rec wrap_mem = function
    | (label,mem) :: m -> LabelMap.add label mem (wrap_mem m)
    | [] -> LabelMap.empty

  let fresh_lvar ?basename ltyp =
    let tau = Lang.tau_of_ltype ltyp in
    let x = Lang.freshvar ?basename tau in
    let p = Cvalues.has_ltype ltyp (e_var x) in
    Lang.assume p ; x

  let fresh_cvar ?basename typ = 
    fresh_lvar ?basename (Ctype typ)

  (* -------------------------------------------------------------------------- *)
  (* --- Logic Frame                                                        --- *)
  (* -------------------------------------------------------------------------- *)

  type frame = {
    name : string ;
    pool : pool ;
    gamma : gamma ;
    kf : kernel_function option ;
    formals : value Varinfo.Map.t ;
    types : string list ;
    mutable triggers : trigger list ;
    mutable labels : sigma LabelMap.t ;
    mutable result : var option ;
    mutable status : var option ;
  }

  let pp_frame fmt f =
    begin
      Format.fprintf fmt "Frame '%s':@\n" f.name ;
      LabelMap.iter
	(fun l m ->
	   Format.fprintf fmt "@[<hov 4>Label '%a': %a@]@\n"
	     Clabels.pretty l Sigma.pretty m
	) f.labels ;
    end

  (* -------------------------------------------------------------------------- *)
  (* --- Frames Builders                                                    --- *)
  (* -------------------------------------------------------------------------- *)
    
  let logic_frame a types =
    {
      name = a ;
      pool = Lang.new_pool () ;
      gamma = Lang.new_gamma () ;
      formals = Varinfo.Map.empty ;
      types = types ;
      triggers = [] ;
      kf = None ;
      result = None ;
      status = None ;
      labels = LabelMap.empty ;
    }

  let frame kf =
    {
      name = Kernel_function.get_name kf ;
      types = [] ;
      pool = Lang.new_pool () ;
      gamma = Lang.new_gamma () ;
      formals = Varinfo.Map.empty ;
      triggers = [] ;
      kf = Some kf ;
      result = None ;
      status = None ;
      labels = LabelMap.empty ;
    }

  let call_pre kf vs mem =
    {
      name = "Pre " ^ Kernel_function.get_name kf ;
      types = [] ;
      pool = Lang.get_pool () ;
      gamma = Lang.get_gamma () ;
      formals = wrap_var (Kernel_function.get_formals kf) vs ;
      triggers = [] ;
      kf = None ;
      result = None ;
      status = None ;
      labels = wrap_mem [ Clabels.Pre , mem ] ;
    }

  let call_post kf vs seq =
    {
      name = "Post " ^ Kernel_function.get_name kf ;
      types = [] ;
      pool = Lang.get_pool () ;
      gamma = Lang.get_gamma () ;
      formals = wrap_var (Kernel_function.get_formals kf) vs ;
      triggers = [] ;
      kf = Some kf ;
      result = None ;
      status = None ;
      labels = wrap_mem [ Clabels.Pre , seq.pre ; Clabels.Post , seq.post ] ;
    }

  let frame_copy f =
    { f with
	pool = Lang.new_pool ~copy:f.pool () ;
	gamma = Lang.new_gamma ~copy:f.gamma () ;
	labels = LabelMap.map Sigma.copy f.labels ;
    }

  (* -------------------------------------------------------------------------- *)
  (* --- Current Frame                                                      --- *)
  (* -------------------------------------------------------------------------- *)

  let cframe : frame Context.value = Context.create "LogicSemantics.frame"
      
  let get_frame () = Context.get cframe

  let in_frame f cc =
    Context.bind Lang.poly f.types
      (Context.bind cframe f 
	 (Lang.local ~pool:f.pool ~gamma:f.gamma cc))

  let mem_at_frame frame label =
    assert (label <> Clabels.Here) ;
    try LabelMap.find label frame.labels
    with Not_found ->
      let s = M.Sigma.create () in
      frame.labels <- LabelMap.add label s frame.labels ; s

  let mem_frame label = mem_at_frame (Context.get cframe) label

  let formal x =
    let f = Context.get cframe in
    try Some (Varinfo.Map.find x f.formals)
    with Not_found -> None

  let return () =
    let f = Context.get cframe in
    match f.kf with
      | None -> Wp_parameters.fatal "No function in frame '%s'" f.name
      | Some kf ->
	  if Kernel_function.returns_void kf then
	    Wp_parameters.fatal "No result in frame '%s'" f.name ;
	  Kernel_function.get_return_type kf

  let result () =
    let f = Context.get cframe in
    match f.result with
      | Some x -> x
      | None ->
	  match f.kf with
	    | None -> Wp_parameters.fatal "No function in frame '%s'" f.name
	    | Some kf ->
		if Kernel_function.returns_void kf then
		  Wp_parameters.fatal "No result in frame '%s'" f.name ;
		let tr = Kernel_function.get_return_type kf in
		let basename = Kernel_function.get_name kf in
		let x = fresh_cvar ~basename tr in
		f.result <- Some x ; x

  let status () =
    let f = Context.get cframe in
    match f.status with
      | Some x -> x
      | None ->
	  let x = fresh_cvar ~basename:"status" Cil.intType in
	  f.status <- Some x ; x
	    
  let trigger tg =
    if tg <> Qed.Engine.TgAny then
      let f = Context.get cframe in
      f.triggers <- tg :: f.triggers

  let guards f = Lang.hypotheses f.gamma

  (* -------------------------------------------------------------------------- *)
  (* --- Environments                                                       --- *)
  (* -------------------------------------------------------------------------- *)

  type env = {
    vars : logic Logic_var.Map.t ; (* pure : not cvar *)
    lhere : sigma option ;
    current : sigma option ;
  }

  let plain_of_exp lt e =
    if Logic_typing.is_set_type lt then
      let te = Logic_typing.type_of_set_elem lt in
      Vset [Vset.Set(tau_of_ltype te,e)]
    else
      Vexp e

  let new_env lvars =
    let lvars = List.fold_left
      (fun lvars lv ->
	 let x = fresh_lvar ~basename:lv.lv_name lv.lv_type in
	 let v = Vexp(e_var x) in
	 Logic_var.Map.add lv v lvars)
      Logic_var.Map.empty lvars in
    { lhere = None ; current = None ; vars = lvars }

  let sigma e = match e.current with Some s -> s | None ->
    Warning.error "No current memory (missing \\at)"

  let move env s = { env with lhere = Some s ; current = Some s }

  let env_at env label = 
    let s = match label with
      | Clabels.Here ->  env.lhere
      | label -> Some(mem_frame label)
    in { env with current = s }

  let mem_at env label =
    match label with
      | Clabels.Here -> sigma env
      | _ -> mem_frame label

  let env_let env x v = { env with vars = Logic_var.Map.add x v env.vars }
  let env_letval env x = function
    | Loc l -> env_let env x (Vloc l)
    | Val e -> env_let env x (plain_of_exp x.lv_type e)

  (* -------------------------------------------------------------------------- *)
  (* --- Generic Compiler                                                   --- *)
  (* -------------------------------------------------------------------------- *)

  let param_of_lv lv =
    let t = Lang.tau_of_ltype lv.lv_type in
    freshvar ~basename:lv.lv_name t

  let rec profile_env vars sigv = function
    | [] -> { vars=vars ; lhere=None ; current=None } , List.rev sigv
    | lv :: profile ->
	let x = param_of_lv lv in
	let v = plain_of_exp lv.lv_type (e_var x) in
	profile_env (Logic_var.Map.add lv v vars) ((lv,x)::sigv) profile

  let default_label env = function
    | [l] -> move env (mem_frame (Clabels.c_label l))
    | _ -> env
	  
  let compile_step
      (name:string) 
      (types:string list) 
      (profile:logic_var list) 
      (labels:logic_label list)
      (cc : env -> 'a -> 'b)
      (filter : 'b -> var -> bool) 
      (data : 'a) 
      : var list * trigger list * 'b * sig_param list =
    let frame = logic_frame name types in
    in_frame frame
      begin fun () ->
	let env,sigv = profile_env Logic_var.Map.empty [] profile in
	let env = default_label env labels in
	let result = cc env data in
	let used = List.filter (fun (_,x) -> filter result x) sigv in
	let parp = List.map snd used in
	let sigp = List.map (fun (lv,_) -> Sig_value lv) used in
	let (parm,sigm) = 
	  LabelMap.fold
	    (fun label sigma ->
	       Heap.Set.fold_sorted
		 (fun chunk acc ->
		    if filter result (Sigma.get sigma chunk) then 
		      let (parm,sigm) = acc in
		      let x = Sigma.get sigma chunk in
		      let s = Sig_chunk(chunk,label) in
		      ( x::parm , s::sigm )
		    else acc) 
		 (Sigma.domain sigma))
	    frame.labels (parp,sigp)
	in
	parm , frame.triggers , result , sigm
      end ()

  let cc_term : (env -> Cil_types.term -> term) ref 
      = ref (fun _ _ -> assert false)
  let cc_pred : (bool -> env -> predicate named -> pred) ref 
      = ref (fun _ _ -> assert false)
  let cc_logic : (env -> Cil_types.term -> logic) ref 
      = ref (fun _ _ -> assert false)
  let cc_region : (env -> Cil_types.term -> loc sloc list) ref 
      = ref (fun _ _ -> assert false)

  let term env t = !cc_term env t
  let pred positive env t = !cc_pred positive env t
  let logic env t = !cc_logic env t
  let region env t = !cc_region env t
  let reads env ts = List.iter (fun t -> ignore (logic env t.it_content)) ts

  let bootstrap_term cc = cc_term := cc
  let bootstrap_pred cc = cc_pred := cc
  let bootstrap_logic cc = cc_logic := cc
  let bootstrap_region cc = cc_region := cc

  let in_term t x = F.occurs x t
  let in_pred p x = F.occursp x p
  let in_reads _ _ = true

  let is_recursive l =
    if LogicUsage.is_recursive l then Rec else Def 

  (* -------------------------------------------------------------------------- *)
  (* --- Registering User-Defined Signatures                                --- *)
  (* -------------------------------------------------------------------------- *)

  module Axiomatic = Model.Index
    (struct
       type key = string
       type data = unit
       let name = "LogicCompiler." ^ M.datatype ^ ".Axiomatic"
       let compare = String.compare
       let pretty = Format.pp_print_string
     end)

  module Signature = Model.Index
    (struct
       type key = logic_info
       type data = signature
       let name = "LogicCompiler." ^ M.datatype ^ ".Signature"
       let compare = Logic_info.compare
       let pretty fmt l = Logic_var.pretty fmt l.l_var_info
     end)

  (* -------------------------------------------------------------------------- *)
  (* --- Compiling Lemmas                                                   --- *)
  (* -------------------------------------------------------------------------- *)

  let compile_lemma cluster name ~assumed types labels lemma =
    let xs,tgs,prop,_ = compile_step name types [] labels (pred true) in_pred lemma in
    let xs,prop = Definitions.Trigger.plug [tgs] (p_forall xs prop) in 
    {
      l_name = name ;
      l_types = List.length types ;
      l_assumed = assumed ;
      l_triggers = [tgs] ;
      l_forall = xs ;
      l_cluster = cluster ;
      l_lemma = prop ;
    }

  (* -------------------------------------------------------------------------- *)
  (* --- Type Signature of Logic Function                                   --- *)
  (* -------------------------------------------------------------------------- *)

  let type_for_signature l ldef sigp =
    match l.l_type with
      | None -> ()
      | Some tr ->
	  match Cvalues.ldomain tr with
	    | None -> ()
	    | Some p ->
		let name = "T" ^ Lang.logic_id l in
		let vs = List.map e_var ldef.d_params in
		let rec conditions vs sigp =
		  match vs , sigp with
		    | v::vs , Sig_value lv :: sigp ->
			let cond = Cvalues.has_ltype lv.lv_type v in
			cond :: conditions vs sigp
		    | _ -> [] in
		let result = F.e_fun ldef.d_lfun vs in
		let lemma = p_hyps (conditions vs sigp) (p result) in
		let trigger = Trigger.of_term result in
		Definitions.define_lemma {
		  l_name = name ;
		  l_assumed = true ;
		  l_types = ldef.d_types ;
		  l_forall = ldef.d_params ;
		  l_triggers = [[trigger]] ;
		  l_cluster = ldef.d_cluster ;
		  l_lemma = lemma ;
		}

  (* -------------------------------------------------------------------------- *)
  (* --- Compiling Pure Logic Function                                      --- *)
  (* -------------------------------------------------------------------------- *)

  let compile_lbpure cluster l =
    let lfun = ACSL l in
    let tau = Lang.tau_of_return l in
    let parp = Lang.local (List.map param_of_lv) l.l_profile in
    let sigp = List.map (fun lv -> Sig_value lv) l.l_profile in
    let ldef = {
      d_lfun = lfun ;
      d_types = List.length l.l_tparams ;
      d_params = parp ; 
      d_cluster = cluster ;
      d_definition = Logic tau ;
    } in
    Definitions.update_symbol ldef ;
    Signature.update l (SIG sigp) ;
    parp,sigp

  (* -------------------------------------------------------------------------- *)
  (* --- Compiling Abstract Logic Function (in axiomatic with no reads)     --- *)
  (* -------------------------------------------------------------------------- *)

  let compile_lbnone cluster l vars =
    let lfun = ACSL l in
    let tau = Lang.tau_of_return l in
    let parp = Lang.local (List.map param_of_lv) l.l_profile in
    let sigp = List.map (fun lv -> Sig_value lv) l.l_profile in
    let (parm,sigm) = 
      if vars = [] then (parp,sigp)
      else 
	let heap = List.fold_left
	  (fun m x ->
	     let obj = object_of x.vtype in
	     Heap.Set.union m (M.domain obj (M.cvar x))
	  ) Heap.Set.empty vars 
	in List.fold_left
	     (fun acc l -> 
		let label = Clabels.c_label l in
		let sigma = Sigma.create () in
		Heap.Set.fold_sorted 
		  (fun chunk (parm,sigm) -> 
		     let x = Sigma.get sigma chunk in
		     let s = Sig_chunk (chunk,label) in
		     ( x::parm , s :: sigm )
		  ) heap acc
	     ) (parp,sigp) l.l_labels 
    in
    let ldef = {
      d_lfun = lfun ;
      d_types = List.length l.l_tparams ;
      d_params = parm ; 
      d_cluster = cluster ;
      d_definition = Logic tau ;
    } in
    Definitions.define_symbol ldef ;
    type_for_signature l ldef sigp ; SIG sigm

  (* -------------------------------------------------------------------------- *)
  (* --- Compiling Logic Function with Reads                                --- *)
  (* -------------------------------------------------------------------------- *)

  let compile_lbreads cluster l ts = 
    let lfun = ACSL l in
    let name = l.l_var_info.lv_name in
    let tau = Lang.tau_of_return l in
    let xs,_,(),s = 
      compile_step name l.l_tparams l.l_profile l.l_labels 
	reads in_reads ts 
    in
    let ldef = {
      d_lfun = lfun ;
      d_types = List.length l.l_tparams ;
      d_params = xs ;
      d_cluster = cluster ;
      d_definition = Logic tau ;
    } in
    Definitions.define_symbol ldef ; 
    type_for_signature l ldef s ; SIG s

  (* -------------------------------------------------------------------------- *)
  (* --- Compiling Recursive Logic Body                                     --- *)
  (* -------------------------------------------------------------------------- *)

  let compile_rec name l cc filter data =
    let types = l.l_tparams in
    let profile = l.l_profile in
    let labels = l.l_labels in
    let result = compile_step name types profile labels cc filter data in
    if LogicUsage.is_recursive l then
      begin
	let (_,_,_,s) = result in
	Signature.update l (SIG s) ; 
	compile_step name types profile labels cc filter data
      end
    else result

  (* -------------------------------------------------------------------------- *)
  (* --- Compiling Logic Function with Definition                           --- *)
  (* -------------------------------------------------------------------------- *)

  let compile_lbterm cluster l t =
    let name = l.l_var_info.lv_name in
    let tau = Lang.tau_of_return l in
    let xs,_,r,s = compile_rec name l term in_term t in
    match F.repr r with
      | Qed.Logic.Kint c -> CST c
      | _ ->
	  let ldef = {
	    d_lfun = ACSL l ;
	    d_types = List.length l.l_tparams ;
	    d_params = xs ;
	    d_cluster = cluster ;
	    d_definition = Value(tau,is_recursive l,r) ;
	  } in
	  Definitions.define_symbol ldef ;
	  type_for_signature l ldef s ; SIG s

  (* -------------------------------------------------------------------------- *)
  (* --- Compiling Logic Predicate with Definition                          --- *)
  (* -------------------------------------------------------------------------- *)

  let compile_lbpred cluster l p =
    let lfun = ACSL l in
    let name = l.l_var_info.lv_name in
    let xs,_,r,s = compile_rec name l (pred true) in_pred p in
    let ldef = {
      d_lfun = lfun ;
      d_types = List.length l.l_tparams ;
      d_params = xs ;
      d_cluster = cluster ;
      d_definition = Predicate(is_recursive l,r) ;
    } in
    Definitions.define_symbol ldef ; SIG s

  let heap_case labels_used support = function
    | Sig_value _ -> support
    | Sig_chunk(chk,l_case) ->
	let l_ind = 
	  try LabelMap.find l_case labels_used
	  with Not_found -> LabelSet.empty
	in
	let l_chk =
	  try Heap.Map.find chk support
	  with Not_found -> LabelSet.empty
	in
	Heap.Map.add chk (LabelSet.union l_chk l_ind) support

  (* -------------------------------------------------------------------------- *)
  (* --- Compiling Inductive Logic                                          --- *)
  (* -------------------------------------------------------------------------- *)

  let compile_lbinduction cluster l cases = (* unused *)
    (* Temporarily defines l to reads only its formals *)
    let parp,sigp = compile_lbpure cluster l in
    (* Compile cases with default definition and collect used chunks *)
    let support = List.fold_left
      (fun support (case,labels,types,lemma) ->
	 let _,_,_,s = compile_step case types [] labels (pred true) in_pred lemma in
	 let labels_used = LogicUsage.get_induction_labels l case in
	 List.fold_left (heap_case labels_used) support s)
      Heap.Map.empty cases in
    (* Make signature with collected chunks *)
    let (parm,sigm) = 
      let frame = logic_frame l.l_var_info.lv_name l.l_tparams in
      in_frame frame
	(fun () -> 
	   Heap.Map.fold_sorted
	     (fun chunk labels acc ->
		let basename = Chunk.basename_of_chunk chunk in
		let tau = Chunk.tau_of_chunk chunk in
		LabelSet.fold
		  (fun label (parm,sigm) ->
		     let x = Lang.freshvar ~basename tau in
		     x :: parm , Sig_chunk(chunk,label) :: sigm
		  ) labels acc)
	     support (parp,sigp) 
	) () in
    (* Set global Signature *)
    let lfun = ACSL l in
    let ldef = {
      d_lfun = lfun ;
      d_types = List.length l.l_tparams ;
      d_params = parm ;
      d_cluster = cluster ;
      d_definition = Logic Qed.Logic.Prop ;
    } in
    Definitions.update_symbol ldef ;
    Signature.update l (SIG sigm) ;
    (* Re-compile final cases *)
    let cases = List.map
      (fun (case,labels,types,lemma) -> 
	 compile_lemma cluster ~assumed:true case types labels lemma) 
      cases in
    Definitions.update_symbol { ldef with d_definition = Inductive cases } ;
    type_for_signature l ldef sigp (* sufficient *) ; SIG sigm
    
  let compile_logic cluster section l = 
    let s_rec = List.map (fun x -> Sig_value x) l.l_profile in
    Signature.update l (SIG s_rec) ;
    match l.l_body with
      | LBnone -> 
	  let vars = match section with
	    | Toplevel _ -> []
	    | Axiomatic a -> Varinfo.Set.elements a.ax_reads
	  in if l.l_labels <> [] && vars = [] then
	      Wp_parameters.warning ~once:true ~current:false
		"No definition for '%s' interpreted as reads nothing" 
		l.l_var_info.lv_name ;
	  compile_lbnone cluster l vars
      | LBterm t -> compile_lbterm cluster l t
      | LBpred p -> compile_lbpred cluster l p
      | LBreads ts -> compile_lbreads cluster l ts
      | LBinductive cases -> compile_lbinduction cluster l cases

  (* -------------------------------------------------------------------------- *)
  (* --- Retrieving Signature                                               --- *)
  (* -------------------------------------------------------------------------- *)

  let define_type = Definitions.define_type
  let define_logic c a = Signature.compile (compile_logic c a)
  let define_lemma c l = 
    if l.lem_labels <> [] && Wp_parameters.has_dkey "lemma" then
      Wp_parameters.warning ~source:l.lem_position 
	"Lemma '%s' has labels, consider using global invariant instead." 
	l.lem_name ;
    Definitions.define_lemma
      (compile_lemma c ~assumed:l.lem_axiom
	 l.lem_name l.lem_types l.lem_labels l.lem_property)

  let define_axiomatic cluster ax = 
    begin
      List.iter (define_type cluster) ax.ax_types ;
      List.iter (define_logic cluster (Axiomatic ax)) ax.ax_logics ;
      List.iter (define_lemma cluster) ax.ax_lemmas ;
    end

  let lemma l =
    try Definitions.find_lemma l
    with Not_found ->
      let section = LogicUsage.section_of_lemma l.lem_name in
      let cluster = Definitions.section section in
      begin
	match section with
	  | Toplevel _ -> define_lemma cluster l
	  | Axiomatic ax -> define_axiomatic cluster ax
      end ;
      Definitions.find_lemma l

  let signature phi =
    try Signature.find phi
    with Not_found ->
      let section = LogicUsage.section_of_logic phi in
      let cluster = Definitions.section section in
      match section with
	| Toplevel _ ->
	  Signature.memoize (compile_logic cluster section) phi
	| Axiomatic ax ->
	  (* force compilation of entire axiomatics *)
	  define_axiomatic cluster ax ;
	  try Signature.find phi
	  with Not_found -> 
	    Wp_parameters.fatal ~current:true
	      "Axiomatic '%s' compiled, but '%a' not" 
	      ax.ax_name Printer.pp_logic_var phi.l_var_info

  (* -------------------------------------------------------------------------- *)
  (* --- Binding Formal with Actual w.r.t Signature                         --- *)
  (* -------------------------------------------------------------------------- *)

  let rec bind_labels env labels : M.Sigma.t LabelMap.t = 
    match labels with
      | [] -> LabelMap.empty
      | (l1,l2) :: labels ->
	let l1 = Clabels.c_label l1 in
	let l2 = Clabels.c_label l2 in
	LabelMap.add l1 (mem_at env l2) (bind_labels env labels)
    
  let call_params env
      (phi:logic_info)
      (labels:(logic_label * logic_label) list)
      (sparam : sig_param list)
      (parameters:F.term list) 
      : F.term list =
    let mparams = wrap_lvar phi.l_profile parameters in
    let mlabels = bind_labels env labels in
    List.map
      (function
         | Sig_value lv -> Logic_var.Map.find lv mparams
	 | Sig_chunk(c,l) -> 
	   let sigma = 
	     try LabelMap.find l mlabels
	     with Not_found -> 
	       Wp_parameters.fatal "*** Label %a not-found@." Clabels.pretty l
	   in
	   M.Sigma.value sigma c
      ) sparam
      
  let call_fun env 
      (phi:logic_info) 
      (labels:(logic_label * logic_label) list)
      (parameters:F.term list) : F.term =
    match signature phi with
      | CST c -> e_zint c
      | SIG sparam -> 
	let es = call_params env phi labels sparam parameters in
	F.e_fun (ACSL phi) es

  let call_pred env 
      (phi:logic_info) 
      (labels:(logic_label * logic_label) list)
      (parameters:F.term list) : F.pred =
    match signature phi with
      | CST _ -> assert false
      | SIG sparam -> 
	  let es = call_params env phi labels sparam parameters in
	  F.p_call (ACSL phi) es

  (* -------------------------------------------------------------------------- *)
  (* --- Variable Bindings                                                  --- *)
  (* -------------------------------------------------------------------------- *)
	
  let logic_var env x = 
    try Logic_var.Map.find x env.vars
    with Not_found -> 
      try
	let cst = Logic_env.find_logic_cons x in
	let v = 
	  match LogicBuiltins.logic cst with
	    | ACSLDEF -> call_fun env cst [] []
	    | LFUN phi -> e_fun phi []
	    | CONST e -> e
	in plain_of_exp x.lv_type v
      with Not_found ->
	Wp_parameters.fatal "Unbound logic variable '%a'"
	  Printer.pp_logic_var x
      
end

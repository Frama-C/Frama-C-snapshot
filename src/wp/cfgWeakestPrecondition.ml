(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

(* -------------------------------------------------------------------------- *)
(* --- Generator of Preconditions                                         --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Wp_error

module Create
  (WpModel:
    sig
      include Mwp.S
      val model : string
    end)
  =
struct

  module F = WpModel.F
  module D = WpModel.L
  module E = Translate_expr.Create(WpModel)
  module L = Translate_prop.Create(WpModel)

  let predicate = Wp_error.protect_translation L.prop
  let expr = Wp_error.protect_translation E.expr
  let addr = Wp_error.protect_translation E.addr
  let cond = Wp_error.protect_translation E.prop
  let cast = Wp_error.protect_translation3 E.expr_cast

  type t_env = F.pool * L.env

  type t_prop = assigns_kind * property

  and property = {
    bindings : D.bindings ;
    property : F.pred ;
  }

  and assigns_kind =
    | NoAssigns
    | EffectAssigns of effect_assigns

  and effect_assigns = {
    a_pid : WpPropId.prop_id ;
    a_label : Clabels.c_label ;
    a_effect : F.var ; (* accumulated zones for effects *)
    a_locals : F.var ; (* accumulated zones for local variables *)
  }


  let empty = NoAssigns , {
    bindings = D.closed ;
    property = F.p_true ;
  }

  let zip (_,omega) =
    D.close omega.bindings omega.property

  let merge_assigns s1 s2 =
    match s1,s2 with
      | NoAssigns,NoAssigns -> NoAssigns
      | NoAssigns,a -> a
      | a,NoAssigns -> a
      | EffectAssigns a1 , EffectAssigns a2 when a1 == a2 -> s1
      | _ -> Wp_parameters.fatal "Merging different assigns goals"

  let merge_property f w1 w2 =
    {
      bindings=D.closed ;
      property=f
        (D.close w1.bindings w1.property)
        (D.close w2.bindings w2.property) ;
    }

  let is_empty (_,p) = F.is_true p.property

  let merge (s1,p1) (s2,p2) =
    if F.is_true p2.property then (s1,p1) else
      if F.is_true p1.property then (s2,p2) else
        merge_assigns s1 s2 , merge_property F.p_and p1 p2

  let pretty fmt wp = F.pp_pred fmt (zip wp)

  let new_env ?(lvars=[]) kf = 
    let pool = F.pool () in
      pool, L.add_logic_vars (L.env kf ()) pool lvars

  type closing =
    | Keep_opened
    | Close_context

  type assigns =
    | Keep_assigns
    | Clear_assigns
    | Label_assigns of Clabels.c_label
    | Goal_assigns of assigns_kind ref

  (* --- Utilities --- *)

  let pp_vars fmt = function
    | [] -> Format.pp_print_string fmt "-"
    | x::xs ->
        Format.fprintf fmt "@[<hov 2>%s" x.vname ;
        List.iter
          (fun x -> Format.fprintf fmt ",@,%s" x.vname)
          xs ;
        Format.fprintf fmt "@]"

  (* ------------------------------------------------------------------------ *)
  (* ---  Context Management                                              --- *)
  (* ------------------------------------------------------------------------ *)

    let close_property where context q = function
    | Keep_opened ->
        let bindings = D.pop where context in
        {
          bindings = bindings ;
          property = q ;
        }
    | Close_context ->
        {
          bindings = D.closed ;
          property = D.flush where context q ;
        }

    exception Failed

    let on_context (pool,env) (where:string)
        (akind,omega) closing assigns
        (f : L.env -> assigns_kind -> F.pred -> F.pred) : t_prop
        =
      let context = D.push where pool omega.bindings in
      try
        let wp = ref F.p_false in
        begin
          try
            let prop =
              try f env akind omega.property
              with Failed -> F.p_false
            in
            let huge =
            let m = Wp_parameters.Huge.get () in
            if m < 1 then 1 else if m > 29 then max_int else 1 lsl m
            in
            if F.huge_pred huge prop then
              (raise(Wp_error.Error("WP","Huge property"))) ;
            wp := prop ;
          with err ->
            let (source,reason) = Wp_error.protect err in
            Datalib.Collector.add_warning ~source ~reason ~severe:true
              "Abort goal generation" ;
        end ;
        (* f must be computed before, because of lazy bindings for assign goals *)
        let asgns =
          match assigns , akind with
            | Goal_assigns gref , _ -> !gref
            | Keep_assigns , a -> a
            | Clear_assigns , _ | _ , NoAssigns -> NoAssigns
            | Label_assigns l , EffectAssigns a when a.a_label = l ->
                let ze = WpModel.dzone_empty () in
                wp := D.subst a.a_effect ze (D.subst a.a_locals ze !wp) ;
                NoAssigns
            | Label_assigns _ , a -> a
        in
        asgns , close_property where context !wp closing
      with err ->
        D.kill where context ;
        raise err

  let label env lab wp =
    on_context env "label" wp Keep_opened (Label_assigns lab)
      (fun env _assigns p ->
         if lab = Clabels.Here then p else
           match L.find_mem env lab with
             | Some at ->
                 let here = L.mem_at env Clabels.Here in
                 let wp = WpModel.update ~at ~here p in
                 Clabels.label F.p_named lab wp
             | None -> p)

  let tag t (akind,p) = (akind,{ p with property = F.p_named t p.property })

  (* ------------------------------------------------------------------------ *)
  (* ---  Hypotheses and Goal Management                                  --- *)
  (* ------------------------------------------------------------------------ *)

  let merge_with f (a,p) (a',p') =
    merge_assigns a a' , merge_property f p p'

  let add_hyp env h wp =
    on_context env "add_hyp" wp Keep_opened Keep_assigns
      (fun env _assigns p ->
         if F.is_true p then p else
           let hid, hp = h in
	   let pid = WpPropId.property_of_id hid in
           match predicate env hp with
             | Result h ->
                 Datalib.Collector.add_depend pid ; F.p_implies h p
             | Warning(source,reason) ->
                 Datalib.Collector.add_warning ~source ~reason
                   "Ignored hypothesis %a" Description.pp_property pid ; p)

  let add_goal env g wp =
    on_context env "add_goal" wp Keep_opened Clear_assigns
      (fun env _assigns p ->
         if F.is_false p then p else
           let pid, pn = g in
           match predicate env pn with
             | Result g ->
                 F.p_and g p
             | Warning(source,reason) ->
                 Datalib.Collector.add_warning ~severe:true ~source ~reason
                   "Goal %a can not be translated"
                   Description.pp_property (WpPropId.property_of_id pid) ;
                 F.p_false)

  (* ------------------------------------------------------------------------ *)
  (* ---  Axiom Rule                                                      --- *)
  (* ------------------------------------------------------------------------ *)

  let add_axiom _id name labels pn =
    match
      Wp_error.protect_translation3 L.add_axiom name labels pn
    with
      | Result () -> ()
      | Warning(source,reason) ->
          Datalib.Collector.add_warning ~source ~reason
            "Ignored user-defined axiom '%s'" name

  (* ------------------------------------------------------------------------ *)
  (* ---  Initialisation Rule                                             --- *)
  (* ------------------------------------------------------------------------ *)

  exception SkipInit

  let compute_init_loc mem lv =
    match addr mem lv with
      | Warning(source,reason) ->
          Datalib.Collector.add_warning
            ~severe:false ~source ~reason
            "No translation for init l-value '%a'" !Ast_printer.d_lval lv ;
          raise SkipInit
      | Result loc -> loc

  let compute_init_value mem vexp =
    match expr mem vexp with
      | Warning(source,reason) ->
          Datalib.Collector.add_warning ~source ~reason
            "Ignored r-value of initializer" ;
          raise SkipInit
      | Result value -> value

  let init_value env lv typ e_opt wp =
    on_context env "init_value" wp Keep_opened Keep_assigns
      (fun env _assigns p ->
         try
           let mem = L.mem_at env Clabels.Here in
           let obj = Ctypes.object_of typ in
           let loc = compute_init_loc mem lv in
           let loaded = WpModel.logic_of_value (WpModel.load mem obj loc) in
           match e_opt with
             | None ->
                 begin 
		   match WpModel.symb_is_init obj with 
		     | Some p_name -> F.p_implies (F.p_app1 p_name loaded) p
		     | None -> p
		 end

             | Some vexp ->
                 let value =
                   WpModel.logic_of_value (compute_init_value mem vexp)
                 in
                 F.p_implies (WpModel.equal obj loaded value) p

         with SkipInit -> p)

  let init_range env lv typ_elt ka kb wp =
    on_context env "init_range" wp Keep_opened Keep_assigns
      (fun env _assigns p ->
         try
           let mem = L.mem_at env Clabels.Here in
           let obj = Ctypes.object_of typ_elt in
           let loc = compute_init_loc mem lv in
           let loaded = WpModel.logic_of_value (WpModel.load mem obj loc) in
           match WpModel.symb_is_init_range obj with 
	     | Some p_range ->
		 F.p_implies 
		   (F.p_app3 p_range loaded (F.e_int64 ka) (F.e_int64 kb)) p
	     | None -> p
         with SkipInit -> p)

  (* ------------------------------------------------------------------------ *)
  (* ---  Assignment Rule                                                 --- *)
  (* ------------------------------------------------------------------------ *)

  let assign env lv e wp =
    on_context env "assign" wp Keep_opened Keep_assigns
      (fun env assigns p ->
         let mem = L.mem_at env Clabels.Here in
         match addr mem lv with
           | Warning(source,reason) ->
               Datalib.Collector.add_warning
                 ~severe:true ~source ~reason
                 "No translation for l-value '%a'" !Ast_printer.d_lval lv ;
               F.p_false
           | Result l ->
               let te = Cil.typeOf e in
               let obj = Ctypes.object_of te in
               let wp =
                 match expr mem e with
                   | Warning(source,reason) ->
                       Datalib.Collector.add_warning ~source ~reason
                         "Ignored r-value of assignment" ;
                       let modified = F.Aloc(obj,l) in
                       D.havoc_static (WpModel.subst_havoc mem modified) p
                   | Result v ->
                       let t = Ctypes.object_of (Cil.typeOf e) in
                       WpModel.subst_lval mem t l v p
               in
               begin
                 match assigns with
                   | NoAssigns -> wp
                   | EffectAssigns a ->
                       let zl = WpModel.dzone_assigned mem (F.Aloc(obj,l)) in
                       let zs = WpModel.dzone_union (F.var a.a_effect) zl in
                       D.subst a.a_effect zs wp
               end )

  (* ------------------------------------------------------------------------ *)
  (* ---  Return Rule                                                     --- *)
  (* ------------------------------------------------------------------------ *)

  let return env e wp =
    on_context env "return" wp Keep_opened Keep_assigns
      (fun env _assigns p ->
         match e with
           | None -> p
           | Some e ->
               begin
                 let mem = L.mem_at env Clabels.Here in
                 match expr mem e with
                   | Warning(source,reason) ->
                       Datalib.Collector.add_warning ~source ~reason
                         "Ignored returned value" ;
                       L.subst_result env None p
                   | Result v ->
                       let ty_to = L.result_type env in
                       let ty_from = Cil.typeOf e in
                       let r_cast = cast ty_to ty_from v in
                       begin
                         match r_cast with
                           | Warning(source,reason) ->
                               Datalib.Collector.add_warning ~source ~reason
                                 "Ignored returned value (because of a cast)" ;
                               L.subst_result env None p
                           | Result v -> L.subst_result env (Some v) p
                       end
               end)

  (* ------------------------------------------------------------------------ *)
  (* ---  Conditional Rule                                                --- *)
  (* ------------------------------------------------------------------------ *)

  let test env e wpt wpf =
    let pt = zip wpt in
    let pf = zip wpf in
    let wpe = merge_assigns (fst wpt) (fst wpf) , snd empty in
    on_context env "test" wpe Keep_opened Keep_assigns
      (fun env _assigns _true ->
         match cond (L.mem_at env Clabels.Here) e with
           | Result b ->
               F.p_and
                 (F.p_named "Then" (F.p_implies b pt))
                 (F.p_named "Else" (F.p_implies (F.p_not b) pf))
           | Warning(source,reason) ->
               Datalib.Collector.add_warning ~source ~reason
                 "Ignored condition of if-statement (%a)" !Ast_printer.d_exp e ;
               F.p_and pt pf)

  (* ------------------------------------------------------------------------ *)
  (* ---  Switch Rule                                                     --- *)
  (* ------------------------------------------------------------------------ *)

  let case_of_exp m_here e =
    match Ctypes.get_int e with
      | Some k -> F.e_int64 k
      | None ->
          match expr m_here e with
            | Result( WpModel.V_int(_,term) ) -> term
            | Result( _ ) ->
                Datalib.Collector.add_warning
                  ~severe:true ~source:"wp"
                  ~reason:"non-integer expression"
                  "Can not translate switch statement@[@ (%a)@]"
                  !Ast_printer.d_exp e ;
                raise Failed
            | Warning(source,reason) ->
                Datalib.Collector.add_warning
                  ~severe:true ~source ~reason
                  "Can not translate switch statement@[@ (%a)@]"
                  !Ast_printer.d_exp e ;
                raise Failed

  let switch env e wp_cases wp_def =
    let cases = List.map (fun (es,wp) -> es , zip wp) wp_cases in
    let p_def = zip wp_def in
    let assigns =
      List.fold_left
        (fun ak (_es,wp) -> merge_assigns ak (fst wp))
        (fst wp_def) wp_cases in
    let wpe = assigns , { (snd empty) with property = p_def } in
    on_context env "swith" wpe Keep_opened Keep_assigns
      (fun env _assigns _true ->

         let m_here = L.mem_at env Clabels.Here in
         let typ_e = Cil.typeOf e in
         let int_e = case_of_exp m_here e in
         let t = WpModel.tau_of_object (Ctypes.object_of typ_e) in
         let var_e = D.fresh "k" (Formula.Acsl(t,Ctype typ_e)) in
         let val_e = F.var var_e in

         let (default,cases) =
           List.fold_left
             (fun (default,cases) (es_case,p_case) ->
                let ks = List.map (case_of_exp m_here) es_case in
                let hs = F.p_disj (List.map (F.p_eq val_e) ks) in
                let ds = F.p_conj (List.map (F.p_neq val_e) ks) in
                F.p_and ds default ,
                F.p_and (F.p_implies hs p_case) cases
             ) (F.p_true , F.p_true) cases
         in
         let wp_switch = F.p_and cases (F.p_implies default p_def) in
         D.subst var_e int_e wp_switch)

  (* ------------------------------------------------------------------------ *)
  (* ---  Scope Rule                                                      --- *)
  (* ------------------------------------------------------------------------ *)

  let scope env vars sc wp =
    on_context env "scope" wp Keep_opened Keep_assigns
      (fun env assigns p ->
         let mem = L.mem_at env Clabels.Here in
         let wp = WpModel.local_scope mem vars sc p in
         begin
           match assigns , sc with
             | EffectAssigns a , (Mcfg.SC_Block_in | Mcfg.SC_Function_frame) ->
                 let zs =
                   List.fold_left
                     (fun zs x ->
                        let te = Ctypes.object_of x.vtype in
                        let ax = F.Aloc(te,WpModel.cvar mem x) in
                        let zx = WpModel.dzone_assigned mem ax in
                        WpModel.dzone_union zs zx)
                     (F.var a.a_locals) vars
                 in
                 D.subst a.a_locals zs wp

             | _ -> wp
         end)

  (* ------------------------------------------------------------------------ *)
  (* ---  Build property to prove the FROMs                               --- *)
  (* ------------------------------------------------------------------------ *)

  (* TODO: ask Loïc if the parameters for [on_context] are correct
  * because I don't really understand what it means... [2011-07-06-Anne]. *)
  let build_prop_of_from wenv (pre:WpPropId.pred_info list) wp =
    on_context wenv "build_froms" wp Keep_opened Keep_assigns
      (fun env _assigns p ->
         let alpha, p' = F.p_more_alpha_cv F.empty_alpha p in
         let p = F.p_implies p p' in
         let add_pre (alpha, p) (id, pre) = 
	   let pid = WpPropId.property_of_id id in
	   match predicate env pre with
           | Result pre ->
               let pre =
                 match L.find_mem env Clabels.Pre with
                   | Some at ->
                       let here = L.mem_at env Clabels.Here in
                       WpModel.update ~at ~here pre
                   | None -> pre
               in
               Datalib.Collector.add_depend pid ;
               let p = F.p_implies pre p in
               let alpha', pre' = F.p_more_alpha_cv alpha pre in
               let p = F.p_implies pre' p in
                 alpha', p
           | Warning(source,reason) ->
               Datalib.Collector.add_warning ~source ~reason
                 "Ignored hypothesis %a" Description.pp_property pid ;
               alpha, p
         in
         let alpha, p = List.fold_left add_pre (alpha, p) pre in
         let vars = F.fold_alpha (fun _v v' acc -> v'::acc) alpha [] in
         let p = F.p_forall vars p in
         let p = F.p_forall (L.collect_logic_vars env) p in
             p
      )

  (* ------------------------------------------------------------------------ *)
  (* ---  Closing Goal                                                    --- *)
  (* ------------------------------------------------------------------------ *)

  let close env wp =
    on_context env "close" wp Close_context Clear_assigns
      (fun env _assigns p -> 
	 let pfinal = WpModel.quantify (L.mem_at env Clabels.Here) p in
	 let xs = F.freevars pfinal in
	 if xs <> [] then
	   (Datalib.Collector.add_warning 
	      ~severe:false ~source:"CFG" 
	      ~reason:"Some labels may escape the control flow"
	      "Generalization of un-labeled values" ;
	    F.p_forall xs pfinal)
	 else pfinal)

  (* ------------------------------------------------------------------------ *)
  (* --- Normal Assigns clauses                                           --- *)
  (* ------------------------------------------------------------------------ *)

  let rec translate_assigned_targets env assigned =
    match assigned with
      | [] -> Result []
      | lv::others ->
          match translate_assigned_targets env others with
            | Warning(p,m) -> Warning(p,m)
            | Result acc ->
                Wp_error.protect_translation
                  (fun e t -> L.assigned e t @ acc)
                  env lv

  let translate_assigned env assigned =
    let lvset = List.fold_left
      (fun lvset (loc,_from) ->
        Cil_datatype.Term.Set.add loc.it_content lvset)
      Cil_datatype.Term.Set.empty assigned
    in
    translate_assigned_targets env (Cil_datatype.Term.Set.elements lvset)

  type assigned = A_everything | A_region of WpModel.loc F.assigned list

  let assigned_of_assigns env assigned =
    match assigned with
      | WritesAny -> A_everything
      | Writes assigned ->
          match translate_assigned env assigned with
            | Result region -> A_region region
            | Warning(source,reason) ->
		Datalib.Collector.add_warning ~source ~reason
		  "Can not translate assign hypothesis, assigns everything instead" ;
		A_everything

  (* Region for proving assign close *)
  let assigned_for_assigns_goal kind label_from env assigned =
    let env_assigned =
      match kind with
        | WpPropId.StmtAssigns -> L.env_at env label_from
        | WpPropId.LoopAssigns -> env
    in
    match translate_assigned env_assigned assigned with
      | Warning(wsrc, msg) -> raise (Wp_error.Error(wsrc,msg))
      | Result region -> region

  (* Apply region during wp *)
  let havoc_region hkind caller_mem region prop =
    match region with
      | A_everything ->
          WpModel.quantify caller_mem prop
      | A_region region ->
          let hs =
            List.concat
              (List.map (WpModel.subst_havoc caller_mem) region) in
          begin
            match hkind with
              | WpPropId.StmtAssigns -> D.havoc_static hs prop
              | WpPropId.LoopAssigns -> D.havoc_inductive hs prop
          end

  (* ------------------------------------------------------------------------ *)
  (* --- Assigns Method                                                   --- *)
  (* ------------------------------------------------------------------------ *)

  let assigns_method () =
    let mth = Wp_parameters.get_assigns_method () in
    match mth with
      | Mcfg.NoAssigns -> mth
      | Mcfg.EffectAssigns when WpModel.effect_supported -> mth
      | _ ->
          if WpModel.assigns_supported
          then Mcfg.NormalAssigns
          else Mcfg.NoAssigns

  (* ------------------------------------------------------------------------ *)
  (* --- Generate Observational Assigns Goal                              --- *)
  (* ------------------------------------------------------------------------ *)

  let add_normal_assigns env pid label kind assigned wp =
    on_context env "add_assigns" wp Keep_opened Clear_assigns
      (fun env _noassigns prop ->
        try
          let label_from = Clabels.c_label label in
          let mem1 = L.mem_at env label_from in
          let mem2 = L.mem_at env Clabels.Here in
          let region = assigned_for_assigns_goal kind label_from env assigned in
          let goal = WpModel.assigns_goal mem1 region mem2 in
          F.p_and goal prop
        with e -> (* [VP 2011-02-03] Argl! *)
          let (source,reason) = Wp_error.protect e in
          Datalib.Collector.add_warning
            ~severe:true ~source ~reason
            "Goal for %a can not be translated"
            WpPropId.pretty pid ;
          F.p_false)

  (* ------------------------------------------------------------------------ *)
  (* ---  Prove assigns clauses with WpModel.dzones                       --- *)
  (* ------------------------------------------------------------------------ *)

  let add_effect_assigns env pid label kind assigned wp =
    let from = Clabels.c_label label in
    let goal = ref NoAssigns in
    on_context env "add_assigns" wp Keep_opened (Goal_assigns goal)
      (fun env _noassigns _prop ->
        try
          let asgns = assigned_for_assigns_goal kind from env assigned in
          let ze = D.fresh "ze" (Formula.Model WpModel.tau_of_dzone) in
          let zx = D.fresh "zx" (Formula.Model WpModel.tau_of_dzone) in
	  let mem = L.mem_at env from in
          let zs = List.fold_left
            (fun zs a ->
              let zx = WpModel.dzone_assigned mem a in
              WpModel.dzone_union zs zx)
            (F.var zx) asgns
          in
          goal := EffectAssigns {
            a_pid = pid ;
            a_label = from ;
            a_effect = ze ;
            a_locals = zx ;
          } ;
          WpModel.dzone_subset (F.var ze) zs
        with e -> (* [VP 2011-02-03] Argl! *)
          let (source,reason) = Wp_error.protect e in
          Datalib.Collector.add_warning
            ~severe:true ~source ~reason
            "Goal for %a can not be translated"
            WpPropId.pretty pid ;
          F.p_false)

  (* ------------------------------------------------------------------------ *)
  (* --- Dispatch Assigns Goal against selected method                    --- *)
  (* ------------------------------------------------------------------------ *)

  let add_assigns env assigns wp =
    let pid, a_desc = assigns in
    let label = a_desc.WpPropId.a_label in
    let kind = a_desc.WpPropId.a_kind in
    let assigned = a_desc.WpPropId.a_assigns in
    match assigned with
        WritesAny ->
          on_context env "add_assigns" wp Keep_opened
            (Goal_assigns (ref NoAssigns))
            (fun _ _ _ -> F.p_true) (* Nothing to prove *)
      | Writes assigns ->
        match assigns_method () with
          | Mcfg.NoAssigns ->
            Wp_parameters.abort "Unsupported assigns with the model"
          | Mcfg.NormalAssigns ->
            add_normal_assigns env pid label kind assigns wp
          | Mcfg.EffectAssigns ->
            add_effect_assigns env pid label kind assigns wp

  (* Assigns Hypothesis *)

  let check_assigns m goal region wp =
    match goal , region with
      | NoAssigns , _ -> wp
      | (EffectAssigns _ ) , A_everything ->
          Datalib.Collector.add_warning
            ~severe:true
            ~reason:"Assigns everything during calculus"
            "Can not prove the assign goal" ;
          F.p_false
      | EffectAssigns a , A_region zones ->
          let ze = List.fold_left
            (fun zs a ->
               let za = WpModel.dzone_assigned m a in
               WpModel.dzone_union zs za)
            (F.var a.a_effect) zones
          in
          D.subst a.a_effect ze wp

  let use_assigns env hid a_desc wp =
    on_context env "use_assigns" wp Close_context Keep_assigns
      (fun env assignsgoal p ->
         let kind = a_desc.WpPropId.a_kind in
         let assigned = a_desc.WpPropId.a_assigns in
	 (match hid with
	    | Some h -> Datalib.Collector.add_depend (WpPropId.property_of_id h)
	    | None -> ()) ;
        let region = assigned_of_assigns env assigned in
	let mem = L.mem_at env Clabels.Here in 
        let p0 = check_assigns mem assignsgoal region p in
        havoc_region kind mem region p0)

  (* -------------------------------------------------------------------------- *)
  (* --- CALL NEW API                                                       --- *)
  (* -------------------------------------------------------------------------- *)

  type callenv = {
    callsite : Clabels.c_label ; (* Clabels.CallAt stmt *)
    m_pre  : WpModel.mem ;
    m_post : WpModel.mem ;
    v_args : WpModel.value list ;
  }

  (* Local elements used in call_xxx *)
  let callenv env stmt args =
    let pre_label = Clabels.CallAt stmt.sid in
    let m_pre = L.mem_at env pre_label in
    let m_post = L.mem_at env Clabels.Here in
    let translate_arg e =
      match expr m_pre e with
        | Warning(source,reason) ->
            Datalib.Collector.add_warning ~source ~reason
              "Can not call function, no translation for parameter '%a'"
              !Ast_printer.d_exp e ;
            raise Failed
        | Result v -> v
    in {
      callsite = pre_label ;
      m_pre  = m_pre ;
      m_post = m_post ;
      v_args = List.map translate_arg args ;
    }

  (* Utility function to handle errors in call_xxx *)
  let do_prop env item p =
    match predicate env p with
      | Warning(source,reason) ->
          Datalib.Collector.add_warning
            ~source ~reason
            "Ignored %s for function call" item ;
          F.p_true
      | Result p -> p

  let do_properties env item idps =
    F.p_conj (List.map (fun (_id,p) -> do_prop env item p) idps)

  let rec do_hypothesis env item idps wp =
    match idps with
      | [] -> wp
      | (_id,h)::hs -> F.p_implies (do_prop env item h) (do_hypothesis env item hs wp)

  (* Apply a returned value from a function call *)
  let do_return call kf lvopt p_after =
    let t_result = Kernel_function.get_return_type kf in
    if Ctypes.is_void t_result then
      ( p_after , None )
    else
      let x_result = D.fresh "result"
        (Formula.Acsl ((WpModel.tau_of_object (Ctypes.object_of t_result)),
                      Ctype t_result)) in
      match lvopt with
        | None -> p_after , Some x_result
        | Some lv ->
            let lv_t = Ctypes.object_of t_result in
            let casted_result =
              cast t_result (Cil.typeOfLval lv)
                (WpModel.value_of_logic lv_t (F.var x_result))
            in
            let v_result = match casted_result with
              | Warning(source,reason) ->
                  Datalib.Collector.add_warning
                    ~severe:true ~source ~reason
                    "Can not cast the returned value" ;
                  raise Failed
              | Result res -> res
            in

            let p_with_result =
              begin
                (* TODO : check memory for computing return loc *)
                match addr call.m_post lv with
                  | Warning(source, reason) ->
                      Datalib.Collector.add_warning
                        ~severe:true ~source ~reason
                        "Can not assign the returned value, no translation for l-value" ;
                      raise Failed
                  | Result loc ->
                      WpModel.subst_lval call.m_post lv_t loc v_result p_after
              end
            in
            p_with_result , Some x_result

  (* ------------------------------------------------------------------------ *)
  (* --- CALL Rule                                                        --- *)
  (* ------------------------------------------------------------------------ *)

  let call_goal_precond caller_env stmt kf args ~pre wp =
    on_context caller_env "call_goal_precond" wp Keep_opened Keep_assigns
      (fun env _assigns p ->
         let call = callenv env stmt args in
         let env_pre = L.call_pre env kf call.v_args call.m_pre in
         let preconds = do_properties env_pre "pre-condition" pre in
         F.p_and preconds p)

  let add_dependencies = List.iter Datalib.Collector.add_depend

  let call_normal_only caller_env stmt lv kf args ~pre ~post ~assigns ~p_post =
    on_context caller_env "call_normal" p_post Keep_opened Keep_assigns
      (fun env assigns_method p_post ->
	 add_dependencies (WpAnnot.get_called_assigns kf) ;
	 add_dependencies (WpAnnot.get_called_post_conditions kf) ;
	 add_dependencies (WpAnnot.get_called_preconditions_at kf stmt) ;
         let call = callenv env stmt args in
         let p_after , x_result = do_return call kf lv p_post in
         let env_pre = L.call_pre env kf call.v_args call.m_pre in
         let env_post = L.call_post env kf call.v_args call.m_pre call.m_post x_result in
         let p_called = do_hypothesis env_post "post-condition" post p_after in
         let p_called = match x_result with
             | None -> p_called
             | Some x -> D.forall [x] p_called
         in
         let asgnd = assigned_of_assigns env_pre assigns in
         let p_havoc = havoc_region WpPropId.StmtAssigns call.m_post asgnd p_called in
         let p_before = do_hypothesis env_pre "pre-condition" pre p_havoc in
         check_assigns call.m_pre assigns_method asgnd p_before)

  let call_exit_only caller_env stmt kf args ~pre ~pexit ~assigns ~p_exit =
    on_context caller_env "call_normal" p_exit Keep_opened Keep_assigns
      (fun env assigns_method p_exit ->
	 add_dependencies (WpAnnot.get_called_assigns kf) ;
	 add_dependencies (WpAnnot.get_called_exit_conditions kf) ;
	 add_dependencies (WpAnnot.get_called_preconditions_at kf stmt) ;
         let call = callenv env stmt args in
         let x_status = L.exit_status env in
         let env_pre = L.call_pre env kf call.v_args call.m_pre in
         let env_exit = L.call_exit env kf call.v_args call.m_pre call.m_post x_status in
         let p_called = do_hypothesis env_exit "exit-condition" pexit p_exit in
         let p_called = D.forall [x_status] p_called in
         let asgnd = assigned_of_assigns env_pre assigns in
         let p_havoc = havoc_region WpPropId.StmtAssigns call.m_post asgnd p_called in
         let p_before = do_hypothesis env_pre "pre-condition" pre p_havoc in
         check_assigns call.m_pre assigns_method asgnd p_before)

  let call caller_env stmt lv kf args ~pre ~post ~pexit ~assigns ~p_post ~p_exit =
    let wp_post =
      if is_empty p_post then p_post
      else call_normal_only caller_env stmt lv kf args ~pre ~post ~assigns ~p_post
    in
    let wp_exit =
      if is_empty p_exit then p_exit
      else call_exit_only caller_env stmt kf args ~pre ~pexit ~assigns ~p_exit
    in
    merge wp_post wp_exit

(* --- End of Weakest Precondition Rules --- *)

end

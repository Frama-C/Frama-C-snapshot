(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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

(** Runtime Error annotation generation plugin *)

open Cil_types
open Cil_datatype

let precond_prefix = "pre" (* prefix for generated behaviors *)

(* assertion for preconditions *)
type orig_lval = (* StartOfOrig | *) AddrOfOrig | LvalOrig

let rec find_term_to_replace vinfo = function
  | [] -> None
  | (formal, term) :: tl ->
    if vinfo.vid = formal.vid then Some term else find_term_to_replace vinfo tl

exception AddrOfFormal
exception NoResult

(* for each lval, replace each logic_variable which stems from a C variable by
   the term corresponding to the variable at this point iff it is a formal *)
let treat_tlval fa_terms ret_opt origin tlval =
  let prefix_origin ntlval = match origin with
    | LvalOrig -> TLval ntlval
    | AddrOfOrig -> TAddrOf ntlval
  in
  let t_lhost, t_offset = tlval in
  match t_lhost with
  | TMem _st -> 
    let normalise_lval = function
    | TLval ((TMem {term_node=TAddrOf lv}), ofs) -> 
        TLval (Logic_const.addTermOffsetLval ofs lv)
    | TLval ((TMem {term_node=TStartOf lv}), ofs) -> 
        TLval (Logic_const.addTermOffsetLval (TIndex (Cil.lzero (), ofs)) lv)
    | x -> x
    in
      Cil.DoChildrenPost normalise_lval
  | TResult _ty -> 
    (* for post-conditions and assigns containing a \result *)
    (match ret_opt with
    | None -> raise NoResult (* BTS 692 *)
    | Some trm ->
      (* [VP] What happens if t_offset <> TNoOffset? *)
      Cil.ChangeTo (prefix_origin trm))
  | TVar { lv_origin = Some vinfo } when vinfo.vformal ->
    (match find_term_to_replace vinfo fa_terms with
    | None -> Cil.DoChildren (* ? can this happen ? is it correct ? *)
    | Some nt ->
      let make_li tmp_lvar = {
        l_var_info = tmp_lvar; l_body = LBterm nt;
        l_type = None; l_tparams = [];
        l_labels = []; l_profile = [];
      }
      in
      let make_tlet () =
        let tmp_lvar = Cil.make_temp_logic_var nt.term_type in
        Tlet
          (make_li tmp_lvar,
	   Logic_const.term
	     (prefix_origin (TVar tmp_lvar, t_offset))
	     nt.term_type)
      in
      let tlet_or_ident () =
        if t_offset = TNoOffset then
          (* Nothing to substitute afterwards. *)
          Cil.ChangeTo nt.term_node
        else
          (* May need substitution in t_offset. *)
          Cil.ChangeDoChildrenPost (make_tlet (), fun x -> x)
      in
      let add_offset lval = Logic_const.addTermOffsetLval t_offset lval in
      match nt.term_node with
      | TLval lv ->
	Cil.ChangeDoChildrenPost (prefix_origin (add_offset lv), fun x -> x)
      | TStartOf lv ->
        let lv = add_offset lv in
        let t = match origin with
          | LvalOrig -> TStartOf lv
          | AddrOfOrig -> TAddrOf lv
        in
        Cil.ChangeDoChildrenPost(t, fun x -> x)
      | TCastE(ty,{ term_node = TLval lv | TStartOf lv }) ->
        (match origin with
	| LvalOrig -> tlet_or_ident()
        | AddrOfOrig when t_offset = TNoOffset ->
          let t = Logic_const.taddrof lv (Cil.typeOfTermLval lv) in
          Cil.ChangeTo (TCastE(TPtr(ty,[]), t))
        | AddrOfOrig  ->
          let lh = TMem nt in
          Cil.ChangeDoChildrenPost (TAddrOf (lh,t_offset),fun x -> x))
      | _ when origin = AddrOfOrig ->
        Options.warn ~source:(fst nt.term_loc)
          "Cannot substitute a non-lval parameter under an addrof operation";
        raise AddrOfFormal
      | _  -> tlet_or_ident ())
  | _ -> 
    Cil.DoChildren

let replacement_visitor replace_pre fa_terms ret_opt = object
  (* for each term, replace each logic_variable which
     stems from a C variable by the term corresponding
     to the variable at this point iff it is a formal *)

  (*  BTS 1052: must use a copy visitor *)
  inherit Cil.genericCilVisitor (Cil.copy_visit (Project.current ()))

  method! vterm_node = function
  | TConst _ | TSizeOf _ | TSizeOfStr _
  | TAlignOf _ | Tnull | Ttype _ | Tempty_set -> Cil.SkipChildren

  | TLval tlval -> treat_tlval fa_terms ret_opt LvalOrig tlval
  | TAddrOf tlval -> treat_tlval fa_terms ret_opt AddrOfOrig tlval
  | TStartOf _ (* [VP] Neither parameters nor returned value can be
                  an array in a C function. Hence, TStartOf can not have
                  \result or a formal as base. *)
  | Tat _  -> 
    let normalize_at = function
      | Tat ({term_node=((TAddrOf (TVar {lv_kind=LVC},_)) as t)}, _)  -> t
      | Tat ({term_node=((TStartOf (TVar {lv_kind=LVC},_)) as t)}, _)  -> t
      | x -> x
    in
    Cil.DoChildrenPost normalize_at
  | _ -> Cil.DoChildren

  method! vlogic_label = function
  | StmtLabel _ -> Cil.DoChildren
  | LogicLabel _ as l when Logic_label.equal l Logic_const.pre_label ->
    Cil.ChangeDoChildrenPost(replace_pre, fun x->x)
  | LogicLabel _ -> Cil.DoChildren

end

let treat_pred replace_pre pred fa_terms (ret_opt : term_lval option)  =
  let visitor = replacement_visitor replace_pre fa_terms ret_opt in
  Cil.visitCilPredicate (visitor :> Cil.cilVisitor) pred
    
let treat_term replace_pre trm fa_terms ret_opt =
  let visitor = replacement_visitor replace_pre fa_terms ret_opt in
  Cil.visitCilTerm (visitor :> Cil.cilVisitor) trm

(* AST inplace visitor for runtime annotation generation *)

(* module for bypassing categories of annotation generation for certain
   expression ids ; 
   useful in a case such as

   signed char cx,cy,cz;
   cz = cx * cy;

   which translates to

   cz = (signed char) ((int) cx * (int) cz) ;

   which would in this case be annotated both by

   assert
   (((int )cx+(int )cy <= 2147483647) and
   ((int )cx+(int )cy >= (-0x7FFFFFFF-1)));

   and

   assert (((int )cx+(int )cy <= 127) and ((int )cx+(int )cy >= -128));

   while we only want to keep the second assert (comes from the cast,
   and is stronger)
*)

exception Untreated_assign

(* Used to generate fresh names for the behaviors introduced by -rte-precond *)
module KfPrecondBehaviors =
  Datatype.Triple_with_collections
    (Kernel_function) (* Caller *)
    (Kernel_function) (* Callee *)
    (Datatype.String) (* Behavior *)
    (struct let module_name = "Rte.KfBehaviors" end)

class annot_visitor kf = object (self)

  inherit Visitor.frama_c_inplace

  val mutable skip_set = Exp.Set.empty
  val mutable index_behavior = 0
  val behavior_names = KfPrecondBehaviors.Hashtbl.create 7

  method private mark_to_skip exp = skip_set <- Exp.Set.add exp skip_set
  method private must_skip exp = Exp.Set.mem exp skip_set

  method private do_mem_access () =
    Options.DoMemAccess.get () && not (Generator.Mem_access.is_computed kf)

  method private do_called_precond () =
    Options.DoCalledPrecond.get ()
  && not (Generator.Called_precond.is_computed kf)

  method private do_div_mod () =
    Options.DoDivMod.get () && not (Generator.Div_mod.is_computed kf)

  method private do_shift () =
    Options.DoShift.get () && not (Generator.Shift.is_computed kf)

  method private do_signed_overflow () =
    Kernel.SignedOverflow.get () && not (Generator.Signed.is_computed kf)

  method private do_unsigned_overflow () =
    Kernel.UnsignedOverflow.get () &&
      not (Generator.Unsigned_overflow.is_computed kf)

  method private do_downcast () =
    Kernel.SignedDowncast.get () && not (Generator.Downcast.is_computed kf)

  method private do_unsigned_downcast () = 
    Kernel.UnsignedDowncast.get () &&
      not (Generator.Unsigned_downcast.is_computed kf)

  method private do_float_to_int () =
    Options.DoFloatToInt.get () && not (Generator.Float_to_int.is_computed kf)

  method private queue_stmt_spec spec =
    let stmt = Extlib.the (self#current_stmt) in
    Queue.add
      (fun () ->
	let annot = Logic_const.new_code_annotation (AStmtSpec ([], spec)) in
	Annotations.add_code_annot Generator.emitter ~kf stmt annot)
      self#get_filling_actions

  method private mk_new_behavior_name kf_callee behav =
    let fname = Kernel_function.get_name kf_callee in
    let bname =
      if Cil.is_default_behavior behav then "" else "_" ^ behav.b_name
    in
    let key = kf, kf_callee, bname in
    let name = 
      try 
	let n = KfPrecondBehaviors.Hashtbl.find behavior_names key in
	incr n;
	precond_prefix ^ "_" ^ fname ^ bname ^ "_" ^ string_of_int !n
      with Not_found ->
	KfPrecondBehaviors.Hashtbl.add behavior_names key (ref 1);
	precond_prefix ^ "_" ^ fname ^ bname
    in
    Annotations.fresh_behavior_name kf name
      
  method private make_stmt_contract kf formals_actuals_terms ret_opt call_stmt =
    let tret_opt = match ret_opt with
      | None -> None
      | Some lv -> Some (Logic_utils.lval_to_term_lval ~cast:true lv)
    in
    let fun_transform_pred replace_pre p =
      let p' = Logic_const.pred_of_id_pred p in
      try
	let p_unnamed =
	  Logic_const.unamed
	    (treat_pred
               replace_pre
	       p'.content
	       formals_actuals_terms tret_opt)
	in
        Logic_const.new_predicate 
	  { content = p_unnamed.content ;
	    loc = p_unnamed.loc ;
	    name = p'.name }
      with 
      | AddrOfFormal
      | NoResult ->
        (* A warning has been emitted, we simply ignore the predicate here. *)
        Logic_const.new_predicate Logic_const.ptrue
    in
    let fun_transform_allocations allocs =  
      let treat_alloc it = 
	Logic_const.new_identified_term 
          (treat_term 
	     Logic_const.old_label it.it_content formals_actuals_terms tret_opt)
      in
      match allocs with
      | FreeAlloc (lfree_loc, lalloc_loc) ->
	FreeAlloc
	  (List.map treat_alloc lfree_loc, List.map treat_alloc lalloc_loc)
      | FreeAllocAny -> FreeAllocAny	  
    in
    let fun_transform_assigns assigns =
      (* substitute terms, then for each from extract lvals and
         keep those and only those as froms *)
      let treat_from it =
	let rec keep_it t = match t.term_node with
	  | TLval _ -> true
	  | Tat (loc,_) -> keep_it loc
	  | TCastE (_,te) -> keep_it te
          | TLogic_coerce (_,te) -> keep_it te
	  | Tinter locs
	  | Tunion locs -> 
	    (try
	       List.iter
		 (fun loc -> if not (keep_it loc) then raise Exit) 
		 locs;
	       true
	     with Exit -> false)
	  | _ -> false
	in 
	(* also, discard casts in froms *)
	let rec transform_term t = match t.term_node with
	  | TCastE (_,te) -> transform_term te
	  | _ -> t
	in
	let nterm =
          treat_term Logic_const.old_label 
            it.it_content formals_actuals_terms tret_opt
        in
	if keep_it nterm then 
	  [ Logic_const.new_identified_term (transform_term nterm) ]
	else
	  []
      in
      let treat_identified_term_zone_froms = function
	| FromAny -> FromAny
	| From l -> From (List.flatten (List.rev_map treat_from l))
      in
      let treat_assign (z,lz) =
	try
	  let nt =
	    treat_term Logic_const.old_label
	      z.it_content formals_actuals_terms tret_opt
          (* should be an lval *)
	  in
	  (* also treat union, inter and at terms *)
	  match nt.term_node with
	  | Tat _
	  | TLval _
	  | Tunion _ 
	  | Tinter _ -> 		  
	    Logic_const.new_identified_term nt,
	    treat_identified_term_zone_froms lz
	  | _ -> raise Untreated_assign
	with AddrOfFormal | NoResult -> 
	  raise Untreated_assign
      in 
      let treat_assigns_clause = function
	(* compute list of assigns as (terms, list of terms) ;
	   if empty list of terms => it's a Nothing, else Location ... *)
	(* then process to transform assign on \result *)
        | WritesAny -> WritesAny
        | Writes l -> 
	  try Writes (List.map treat_assign l)
	  with Untreated_assign -> WritesAny
      in
      let final_assigns_list = match ret_opt with
	| None ->
	  (* no return value: there should be no assign of \result *)
	  assigns
	| Some ret ->
	  let ret_type = Cil.typeOfLval ret in
	  let nlist_assigns =
	    (* if there is a assigns \at(\result,Post) \from x
	       replace by assigns \result \from x *)
	    match assigns with
	    | WritesAny -> WritesAny
	    | Writes assigns ->
              let rec change_at_result acc = function
                | [] -> Writes (List.rev acc)
                | (a,from) :: tl ->
		  let new_a = match a.it_content.term_node with
		    | Tat ({term_node=(TLval(TResult _,_) as trm)}, 
                           LogicLabel (_, "Post")) -> 
		      let ttype = Ctype ret_type
		      (* cf. bug #559 *)
		      (* Logic_utils.typ_to_logic_type
			 ret_type *)
		      in
		      Logic_const.new_identified_term 
                        (Logic_const.term trm ttype)
		    | _ -> a
		  in
                  change_at_result ((new_a,from) :: acc) tl
	      in
              change_at_result [] assigns
	  in
	  (* add assign on result iff no assigns(\result) already appears ;
	     treat_assign will then do the job *)
          let add_assigns_result () =
	    (* add assigns \result with empty list of froms to do the job *)
	    let ttype = Ctype ret_type in
	    (* bug #559 *)
	    (* Logic_utils.typ_to_logic_type ret_type *) 
	    let nterm = 
	      Logic_const.term (TLval (TResult ret_type, TNoOffset)) ttype 
	    in
	    Logic_const.new_identified_term nterm, FromAny
	  in
          match nlist_assigns with
          | WritesAny -> WritesAny
          | Writes l when
              List.exists (fun (a,_) -> Logic_utils.is_result a.it_content) l 
              ->
            nlist_assigns
          | Writes l -> Writes (add_assigns_result()::l)
      in 
      treat_assigns_clause final_assigns_list
    in
    let behaviors, default_assigns =
      (* calling get_spec on a function with a contract
	 but no code generates default assigns *)
      let spec = Annotations.funspec kf in
      (* [JS 2012/06/01] looks quite close of Infer_annotations.populate_spec,
	 but it is not equivalent... *)
      let bhvs = spec.spec_behavior in
      bhvs, 
      (* Looking for management of default assigns clause. *)
      (match Ast_info.merge_assigns_from_complete_bhvs ~warn:false bhvs [] with
	WritesAny -> 
	  (* Case 1: it isn't possible to find good assigns from ungarded
	     behaviors. S, looks at assigns from complete behaviors clauses. *)
	  (match Ast_info.merge_assigns_from_complete_bhvs 
	      ~warn:true ~ungarded:false bhvs spec.spec_complete_behaviors 
	   with
	   | WritesAny ->
	     (* Case 1.1: no better thing to do than nothing *)	     
	     None
	   | assigns -> 
	     (* Case 1.2: that assigns will be used as default assigns later. 
		note: a message has been emmited. *)
	     Some assigns)
      | _ -> (* Case 2: no special thing to do *)
	None)
    in
    try
      let new_behaviors = 
	let default_allocation_assigns = ref (None, None) in
	let new_bhvs =
	  List.fold_left
	    (fun acc bhv -> 
	      (* step 1: looking for managment of allocation and assigns
		 clause. *)
	      let allocation = 
		Some (fun_transform_allocations bhv.b_allocation) 
	      in
	      let assigns, allocation, name = 
		if Cil.is_default_behavior bhv then
		  match bhv with
		  | { b_post_cond = []; 
		      b_assumes = []; 
		      b_requires = []; 
		      b_assigns = WritesAny} ->
		    (* The default bhv contents only an allocation clause.
		       So, keeps it as the new default bhv. *)
		    (* here no call to mk_new_behavior_name, 
		       need to ensure same side-effect (populate englobing func
		       spec) *)
		    ignore (Annotations.funspec kf);
		    let assigns = match default_assigns with
		      | Some assigns -> 
			(* Use these assigns as default assigns *)
			assigns
		      | None -> 
			(* No special thing to do about assigns*)
			WritesAny
		    in 
		    assigns, allocation, Cil.default_behavior_name
		  | _ -> 
		    (* The default bhv contents other clauses.
		       So, extract the allocation clause for the new bhv
		       where the eventual default assigns will be set. *)
		    default_allocation_assigns := allocation, default_assigns;
		    bhv.b_assigns, None, self#mk_new_behavior_name kf bhv
		else 
		  bhv.b_assigns,allocation, self#mk_new_behavior_name kf bhv
	      in
              (* We store a mapping between the old and the copied requires,
                 in order to position some status dependencies between them *)
              let new_requires = ref [] in
              let requires = 
		List.map
                  (fun pred ->
                    let after = 
		      fun_transform_pred Logic_const.here_label pred 
		    in
                    new_requires := (pred, after) :: !new_requires;
                    after) 
		  bhv.b_requires
              in
	      let b = (* step 2: just map the current behavior *)
		(* As said before, assigns, allocation and names have a special
		   management *)
		Cil.mk_behavior
		  ~assigns:(fun_transform_assigns assigns) 
		  ~allocation 
		  ~name 
		  ~post_cond:(List.map
				(fun (k,p) -> k, 
                                  fun_transform_pred Logic_const.old_label p) 
				bhv.b_post_cond)
		  ~assumes:(List.map 
                              (fun_transform_pred Logic_const.here_label)
                              bhv.b_assumes)
		  ~requires
		  ~extended:[]
		  ()
	      in
              (* Update the dependencies between the original require, and the
                 copy at the syntactic call-site. Done once all the requires
                 and behaviors have been created by the visitore *)
              let requires_deps () =
                let kf_call = Kernel_function.find_englobing_kf call_stmt in
                let ki_call = Kstmt call_stmt in
                let aux (old, after) =
                  let old_ip = Property.ip_of_requires kf Kglobal bhv old in
                  let new_ip =Property.ip_of_requires kf_call ki_call b after in
                  Statuses_by_call.replace_call_precondition
                    old_ip call_stmt new_ip
                in
                List.iter aux !new_requires
              in
              Queue.add requires_deps self#get_filling_actions;
	      b :: acc) 
	    []
	    behaviors
	in
	(* step 3: adds the allocation clause into a default behavior *)
	match !default_allocation_assigns with
	| None,None -> new_bhvs
	| allocation,None -> Cil.mk_behavior ~allocation () :: new_bhvs
	| allocation,Some assigns -> 
	  Cil.mk_behavior
	    ~allocation ~assigns:(fun_transform_assigns assigns) ()
	  :: new_bhvs
      in
      match new_behaviors with
      | [] -> None
      | _ :: _ ->
	Some
	  { spec_behavior = List.rev new_behaviors ;
	    spec_variant = None ;
	    spec_terminates = None ;
	    spec_complete_behaviors = [] ;
	    spec_disjoint_behaviors = [] }
    with Exit -> 
      None

  method private generate_assertion: 'a 'b. ('a, 'b) Rte.alarm_gen -> 'a -> 'b =
    let remove_trivial = not (Options.Trivial.get ()) in
    let warning = Options.Warn.get () in
    fun f ->
      let kinstr = self#current_kinstr in
      f ~remove_trivial ~warning kf kinstr

  method! vstmt s = match s.skind with
  | UnspecifiedSequence l ->
    (* UnspecifiedSequences may contain lvals for side-effects, that
       give rise to spurious assertions *)
    let no_lval = List.map (fun (s, _, _, _, sref) -> s, [], [], [], sref) l in
    let s' = { s with skind = UnspecifiedSequence no_lval } in
    Cil.ChangeDoChildrenPost (s', fun _ -> s)
  | _ -> Cil.DoChildren

  (* assigned left values are checked for valid access *)
  method! vinst = function
  | Set (lval,_,_) ->
    if self#do_mem_access () then begin
      Options.debug "lval %a: validity of potential mem access checked\n"
	Printer.pp_lval lval;
      self#generate_assertion 
	(Rte.lval_assertion ~read_only:Alarms.For_writing)
	lval
    end;
    Cil.DoChildren
  | Call (ret_opt,funcexp,argl,_) ->
    (match ret_opt, self#do_mem_access () with
    | None, _ | Some _, false -> ()
    | Some ret, true -> 
      Options.debug "lval %a: validity of potential mem access checked\n"
	Printer.pp_lval ret;
      self#generate_assertion 
	(Rte.lval_assertion ~read_only:Alarms.For_writing) ret);
    if self#do_called_precond () then
      match funcexp.enode with
      | Lval (Var vinfo,NoOffset) ->
	let kf =  Globals.Functions.get vinfo in	    
	let do_no_implicit_cast () = 
	  let formals = Kernel_function.get_formals kf in
	  if List.length formals <> List.length argl then begin
	    Options.warn
	      "(%a) function call with # actuals <> # formals: not treated"
	      Printer.pp_stmt (Extlib.the (self#current_stmt));
	    Cil.DoChildren
	  end else
	    let formals_actuals_terms =
	      List.rev_map2
		(fun formal arg_exp ->
		  (formal,
		   Logic_utils.expr_to_term ~cast:true arg_exp))
		formals argl 
	    in
	    match self#make_stmt_contract
              kf formals_actuals_terms ret_opt
	      (Extlib.the (self#current_stmt)) 
	    with
	    | None -> Cil.DoChildren
	    | Some contract_stmt ->
	      self#queue_stmt_spec contract_stmt;
	      Cil.DoChildren
	in 
	(match ret_opt with
	| None -> do_no_implicit_cast ()
	| Some lv -> 
	  let kf_ret_type = Kernel_function.get_return_type kf in
	  let lv_type = Cil.typeOfLval lv in
	  if Cil.need_cast kf_ret_type lv_type then begin
	    Options.warn 
	      "(%a) function call with intermediate cast: not treated"
	      Printer.pp_stmt (Extlib.the (self#current_stmt));
	    Cil.DoChildren
	  end else
	    do_no_implicit_cast ())
      | Lval (Mem _,NoOffset) ->
	Options.warn "(%a) function called through a pointer: not treated"
	  Printer.pp_stmt (Extlib.the (self#current_stmt));
	Cil.DoChildren
      | _ -> assert false
    else 
      Cil.DoChildren
  | _ -> Cil.DoChildren

  method! vexpr exp =
    Options.debug "considering exp %a\n" Printer.pp_exp exp;
    match exp.enode with
    | BinOp((Div | Mod) as op, lexp, rexp, ty) ->
      (match Cil.unrollType ty with 
      | TInt(kind,_) -> 
	(* add assertion "divisor not zero" *)
	if self#do_div_mod () then
	  self#generate_assertion Rte.divmod_assertion rexp;
	if self#do_signed_overflow () && op = Div && Cil.isSigned kind then 
	  (* treat the special case of signed division overflow
	     (no signed modulo overflow) *)
	  self#generate_assertion Rte.signed_div_assertion (exp, lexp, rexp);
	Cil.DoChildren
      | _ -> Cil.DoChildren)

    | BinOp((Shiftlt | Shiftrt) as op, lexp, rexp,ttype ) ->
      (match Cil.unrollType ttype with 
      | TInt(kind,_) -> 
	if self#do_shift () then begin
	  let t = Cil.unrollType (Cil.typeOf exp) in
	  let size = Cil.bitsSizeOf t in
	    (* Not really a problem of overflow, but almost a similar to self#do_div_mod *)
            self#generate_assertion Rte.shift_alarm (rexp, Some size);
	end;
	if self#do_signed_overflow () && Cil.isSigned kind then
	  self#generate_assertion
	    Rte.signed_shift_assertion (exp, op, lexp, rexp);
	Cil.DoChildren
      | _ -> Cil.DoChildren)

    | BinOp((PlusA |MinusA | Mult) as op, lexp, rexp, ttype) ->
      (* may be skipped if the enclosing expression is a downcast to a signed
	 type *)
      (match Cil.unrollType ttype with 
      | TInt(kind,_) when Cil.isSigned kind -> 
    	if self#do_signed_overflow () && not (self#must_skip exp) then
	  self#generate_assertion Rte.mult_sub_add_assertion
	    (true, exp, op, lexp, rexp);
	Cil.DoChildren
      | TInt(kind,_) when not (Cil.isSigned kind) -> 
	if self#do_unsigned_overflow () then
	  self#generate_assertion  Rte.mult_sub_add_assertion
	    (false, exp, op, lexp, rexp);
	Cil.DoChildren
      | _ -> Cil.DoChildren)

    | UnOp(Neg, exp, ty) ->
      (* Note: if unary minus on unsigned integer is to be understood as
	 "subtracting the promoted value from the largest value
	 of the promoted type and adding one",
	 the result is always representable: so no overflow *)
      (match Cil.unrollType ty with 
      | TInt(kind,_) when Cil.isSigned kind -> 
	if self#do_signed_overflow () then
	  self#generate_assertion Rte.uminus_assertion exp;
      | _ -> ());
      Cil.DoChildren

    | Lval lval ->
      (* left values are checked for valid access *)
      Cil.DoChildrenPost
        (fun new_e ->
          (* Use Cil.DoChildrenPost so that inner expression and lvals are
	     checked first. The order of resulting assertions will be better. *)
          if self#do_mem_access () then begin
	    Options.debug
	      "exp %a is an lval: validity of potential mem access checked" 
	      Printer.pp_exp exp;
	    self#generate_assertion 
	      (Rte.lval_assertion ~read_only:Alarms.For_reading) lval
	  end;
          new_e)

    | CastE (ty, e) ->
      (match Cil.unrollType ty, Cil.unrollType (Cil.typeOf e) with 
      | TInt(kind,_), TInt (_, _) ->
        if Cil.isSigned kind then begin
	  if self#do_downcast () then
	    let alarm =
              self#generate_assertion Rte.signed_downcast_assertion (ty, e)
            in
	    if alarm then self#mark_to_skip e
	end
        else
	  if self#do_unsigned_downcast () then
	    let alarm =
	      self#generate_assertion Rte.unsigned_downcast_assertion (ty, e)
	    in
            if alarm then () (* self#mark_to_skip e *)

      | TInt _, TFloat _ ->
        if self#do_float_to_int () then
          let _alarm =
            self#generate_assertion Rte.float_to_int_assertion (ty, e)
          in
          ()

      | _ -> ());
      Cil.DoChildren

    | StartOf _
    | AddrOf _
    | Info _
    | UnOp _
    | Const _
    | BinOp _ -> Cil.DoChildren
    | SizeOf _
    | SizeOfE _
    | SizeOfStr _
    | AlignOf _
    | AlignOfE _ -> Cil.SkipChildren

end

let rte_annotations stmt = 
  Annotations.fold_code_annot
    (fun e a acc -> if Emitter.equal e Generator.emitter then a ::acc else acc)
    stmt
    []

let get_annotations from kf stmt x =
  Rte.reset_generated_annotations ();
  let old = !Rte.save_alarms in
  Rte.save_alarms := false;
  let o = object (self)
    inherit annot_visitor kf
    initializer self#push_stmt stmt
  end in
  ignore (from (o :> Cil.cilVisitor) x);
  Rte.save_alarms := old;
  Rte.generated_annotations ()

let do_stmt_annotations kf stmt =
  get_annotations Cil.visitCilStmt kf stmt stmt

let do_exp_annotations = get_annotations Cil.visitCilExpr

let check_compute kf get is_computed set update_acc =
(*  feedback "get %b / doall %b / is_computed %b"
    (get ())
    (DoAll.get ())
    (is_computed kf);*)
  if get () && not (is_computed kf) then
    true, (fun () -> update_acc (); set kf true)
  else
    false, update_acc
    
let annotate_kf kf =
  (* generates annotation for function kf on the basis of command-line
     options *) 
  Options.debug "annotating function %a" Kernel_function.pretty kf;
  (* add annotations *)
  match kf.fundec with
  | Declaration _ -> ()
  | Definition(f, _) -> 
    let update = ref (fun () -> ()) in
    let module Check
	(M: Generator.S)
	(P: sig val get: unit -> bool end)
      = struct
      let compute () = 
	let get () = P.get () in
	let b, u = check_compute kf get M.is_computed M.set !update in
	update := u;
	b
    end in
    let must_run1 = 
      let module C = Check(Generator.Signed)(Kernel.SignedOverflow) in 
      C.compute ()
    in
    let must_run2 = 
      let module C = Check(Generator.Mem_access)(Options.DoMemAccess) in 
      C.compute ()
    in
    let must_run3 =
      let module C = Check(Generator.Div_mod)(Options.DoDivMod) in 
      C.compute ()
    in
    let must_run3bis =
      let module C = Check(Generator.Shift)(Options.DoShift) in
      C.compute ()
    in
    let must_run4 =
      let module C = Check (Generator.Downcast)(Kernel.SignedDowncast) in 
      C.compute ()
    in 
    let must_run5 =
      let module C =
        Check(Generator.Unsigned_overflow)(Kernel.UnsignedOverflow) 
      in 
      C.compute ()
    in
    let must_run6 =
      let module C =
        Check(Generator.Unsigned_downcast)(Kernel.UnsignedDowncast) 
      in 
      C.compute ()
    in
    let must_run7 =
      let module C =
        Check(Generator.Float_to_int)(Options.DoFloatToInt) 
      in 
      C.compute ()
    in
    let must_run8 =
      let module C = Check(Generator.Called_precond)(Options.DoCalledPrecond) in
      C.compute ()
    in
    if must_run1 || must_run2 || must_run3  || must_run3bis
      || must_run4 || must_run5 
      || must_run6 || must_run7 || must_run8
    then begin
      Options.feedback "annotating function %a" Kernel_function.pretty kf;
      let vis = new annot_visitor kf in
      let nkf = Visitor.visitFramacFunction vis f in
      assert(nkf == f);
      !update ()
    end

let do_precond kf =
  (* annotate call sites with contracts, for a given function *)
  let old_signed = Generator.Signed.is_computed kf in
  Generator.Signed.set kf true;
  let old_mem = Generator.Mem_access.is_computed kf in
  Generator.Mem_access.set kf true;
  let old_divmod = Generator.Div_mod.is_computed kf in
  Generator.Div_mod.set kf true;
  let old_uo = Generator.Unsigned_overflow.is_computed kf in
  Generator.Unsigned_overflow.set kf true;
  let old_ud = Generator.Unsigned_downcast.is_computed kf in
  Generator.Unsigned_downcast.set kf true;
  let old_downcast = Generator.Downcast.is_computed kf in
  Generator.Downcast.set kf true;
  let old_float_to_int = Generator.Float_to_int.is_computed kf in
  Generator.Float_to_int.set kf true;
  (* TODO: Why is this option set to true without being restored later? *)
  Options.DoCalledPrecond.on ();
  annotate_kf kf;
  Generator.Signed.set kf old_signed;
  Generator.Mem_access.set kf old_mem;
  Generator.Div_mod.set kf old_divmod;
  Generator.Unsigned_overflow.set kf old_uo;
  Generator.Unsigned_downcast.set kf old_ud;
  Generator.Downcast.set kf old_downcast;
  Generator.Float_to_int.set kf old_float_to_int

let do_all_rte kf =
  (* annonate for all rte + unsigned overflows (which are not rte), for a given
     function *) 
  Options.DoAll.on ();
  Kernel.SignedOverflow.off ();
  let old_ud = Generator.Unsigned_downcast.is_computed kf in
  Generator.Unsigned_downcast.set kf true;
  let old_called = Generator.Called_precond.is_computed kf in
  Generator.Called_precond.set kf true;
  annotate_kf kf;
  Generator.Unsigned_downcast.set kf old_ud;
  Generator.Called_precond.set kf old_called
    
let do_rte kf = 
  (* annotate for rte only (not unsigned overflows and downcasts) for a given
     function *)
  Options.DoAll.on ();
  let old_uo = Generator.Unsigned_overflow.is_computed kf in
  Generator.Unsigned_overflow.set kf true;
  let old_ud = Generator.Unsigned_downcast.is_computed kf in
  Generator.Unsigned_downcast.set kf true;
  let old_called = Generator.Called_precond.is_computed kf in
  Generator.Called_precond.set kf true;
  annotate_kf kf;
  Generator.Unsigned_overflow.set kf old_uo;
  Generator.Unsigned_downcast.set kf old_ud;
  Generator.Called_precond.set kf old_called

let compute () =
  (* compute RTE annotations, whether Enabled is set or not *)
  Ast.compute () ;
  let include_function kf =
    let fsel = Options.FunctionSelection.get () in
    Datatype.String.Set.is_empty fsel
    || 
      let name = Kernel_function.get_name kf in
      Datatype.String.Set.mem name fsel
  in
  Globals.Functions.iter
    (fun kf ->
      if include_function kf && Kernel_function.is_definition kf then
	!Db.RteGen.annotate_kf kf)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

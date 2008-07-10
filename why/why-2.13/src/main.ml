(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU General Public                   *)
(*  License version 2, as published by the Free Software Foundation.      *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU General Public License version 2 for more details         *)
(*  (enclosed in the file GPL).                                           *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: main.ml,v 1.152 2008/05/28 13:51:26 marche Exp $ i*)

open Options
open Ptree
open Ast
open Cc
open Types
open Env
open Typing
open Format
open Error
open Report
open Misc
open Util
open Logic
open Logic_decl


(*s Prover dependent functions. *)


let declarationQueue = Queue.create ()

let reset () =
  (*Queue.clear declarationQueue ;*)
  Vcg.logs := []; 
  match prover () with
    | Pvs -> Pvs.reset ()
    | Coq _ -> Coq.reset ()
    | HolLight -> Holl.reset ()
    | Mizar -> Mizar.reset ()
    | Isabelle -> Isabelle.reset ()
    | Hol4 -> Hol4.reset ()
    | SmtLib ->  ()
    | Harvey | Simplify | Zenon | CVCLite  | Gappa 
    | Why | MultiWhy | Dispatcher | WhyProject -> ()

let add_loc = function
  | Dtype (loc, _, s)
  | Dlogic (loc, s, _)
  | Dpredicate_def (loc, s, _)
  | Dfunction_def (loc, s, _) 
  | Daxiom (loc, s, _) (* useful? *) -> Loc.add_ident s loc
  | Dgoal _ -> ()

let store_decl_into_a_queue d  = 
  Queue.push d declarationQueue


(** push the declarations  in the corresponding 
    prover and stores them (or their expansed version) into 
    declarationQueue **)
let push_decl vloc d = 
  add_loc d;
  if (not pruning) && (Options.pruning_hyp_v = -1) then
    begin 
      let pushing = 
	match prover () with
	  | Pvs -> Pvs.push_decl
	  | Coq _ -> Coq.push_decl 
	  | HolLight -> Holl.push_decl 
	  | Mizar -> Mizar.push_decl
	  | Isabelle -> Isabelle.push_decl
	  | Hol4 -> Hol4.push_decl
	  | Gappa -> Gappa.push_decl 
	  | Why | MultiWhy | WhyProject -> Pretty.push_decl 
	  | Dispatcher ->Dispatcher.push_decl
	  | Harvey -> Harvey.push_decl
	  | Simplify -> Simplify.push_decl
	  | Zenon -> Zenon.push_decl
	  | CVCLite -> Cvcl.push_decl
	  | SmtLib -> Smtlib.push_decl 
      in
      if defExpanding != NoExpanding  then 
	let decl = (PredDefExpansor.push d) in 
	List.iter pushing decl ;
	List.iter store_decl_into_a_queue decl ; 
      else
	begin 
	  store_decl_into_a_queue d ;
	  pushing d 
	end
    end
  else
    if defExpanding != NoExpanding then
      List.iter store_decl_into_a_queue (PredDefExpansor.push d)
    else
      store_decl_into_a_queue d 
	
	
	
let push_obligations vloc = 
  List.iter 
    (fun (loc,expl,id,s) -> 
      let (loc,expl,id,s) = 
	if pruning_hyp_v != -1 then 
	  Hypotheses_filtering.reduce (loc, expl, id, s) declarationQueue
	else
	  (loc, expl, id, s)
      in
      push_decl vloc (Dgoal(loc, expl, id, generalize_sequent s))
    ) 



let prover_is_coq = match prover () with Coq _ -> true | _ -> false

let push_validation id tt v = 
  if valid && prover_is_coq then Coq.push_validation id tt v

let is_pure_type_scheme s = is_pure_type_v s.scheme_type

let push_parameter id v tv = match prover () with
  | Coq _ -> 
      if valid then Coq.push_parameter id tv
  | Pvs | HolLight | Isabelle | Hol4 | Mizar
  | Harvey | Simplify | Zenon | SmtLib | Gappa 
  | CVCLite | Why | MultiWhy | Dispatcher | WhyProject -> 
      ()

let output fwe = 
  if wol then begin
    let cout = open_out (Options.out_file (fwe ^ ".wol")) in
    output_value cout !Vcg.logs;
    close_out cout
  end else if ocaml then 
    Pp.print_in_file Ocaml.output (Options.out_file "out")
  else begin match prover () with
    | Pvs -> Pvs.output_file fwe
    | Coq _ -> Coq.output_file fwe
    | HolLight -> Holl.output_file fwe
    | Mizar -> Mizar.output_file fwe    
    | Harvey -> Harvey.output_file fwe
    | Simplify -> Simplify.output_file fwe
    | CVCLite -> Cvcl.output_file fwe
    | Zenon -> Zenon.output_file fwe
    | SmtLib ->  Smtlib.output_file fwe
    | Isabelle -> Isabelle.output_file fwe
    | Hol4 -> Hol4.output_file fwe
    | Gappa -> Gappa.output_file fwe
    | Dispatcher -> ()
    | Why -> Pretty.output_file fwe
    | MultiWhy -> Pretty.output_files fwe
    | WhyProject -> Pretty.output_project fwe
  end



(** This method aims at translating the theory  according to
    the selected encoding  
    @q is the queue of  theory that will be modified
**)
let encode q = 
  let callEncoding d =  match prover () with
    | Pvs -> Pvs.push_decl d
    | Coq _ -> Coq.push_decl d
    | HolLight -> Holl.push_decl d
    | Mizar -> Mizar.push_decl d
    | Isabelle -> Isabelle.push_decl d
    | Hol4 -> Hol4.push_decl d
    | Gappa -> Gappa.push_decl d  
    | Why | MultiWhy | WhyProject -> Pretty.push_decl d
    | Dispatcher -> Dispatcher.push_decl d      
    | Harvey -> Harvey.push_decl d
    | Simplify -> Simplify.push_decl d
    | Zenon -> Zenon.push_decl d
    | CVCLite -> Cvcl.push_decl d
    | SmtLib -> Smtlib.push_decl d
  in
  Queue.iter callEncoding q 
 
(*s Processing of a single declaration [let id = p]. *)

let print_if_debug p x = if_debug_3 eprintf "  @[%a@]@." p x

let interp_program loc id p =
  reset_names ();
  let ploc = p.ploc in
  if_verbose_3 eprintf "*** interpreting program %a@." Ident.print id;

  if_debug eprintf "* typing with effects@.";
  let env = Env.empty_progs () in
  let p = Typing.typef Label.empty env p in
  if effect p <> Effect.bottom then
    raise_located ploc (GlobalWithEffects (id, effect p));
  let c = type_c_of_typing_info [] p.info in
  let c = 
    { c with c_post = optpost_app (change_label p.info.t_label "") c.c_post }
  in
  let v = c.c_result_type in
  check_for_not_mutable ploc v;
  Env.add_global id v None;
  print_if_debug print_type_c c;
  print_if_debug print_expr p;
  if type_only then raise Exit;

  if_debug eprintf "* weakest preconditions@.";
  let p,wp = 
    if fast_wp then
      let w = Fastwp.wp p in
      p, Some (wp_named p.info.t_loc w)
    else
      Wp.wp p 
  in
  print_if_debug print_wp wp;

  if wp_only then raise Exit;

  if ocaml then begin Ocaml.push_program id p; raise Exit end;

  (***
      if_debug eprintf "* functionalization@.";
      let ren = initial_renaming env in
      let cc = Mlize.trad p ren in
      if_debug_3 eprintf "  %a@\n  -----@." print_cc_term cc;
      let cc = Red.red cc in
      print_if_debug print_cc_term cc;
  ***)

  if_debug eprintf "* generating obligations@.";
  let ids = Ident.string id in

  let (name,beh,loc) as vloc =
    try 
      let (f,l,b,e,o) = 
	Hashtbl.find locs_table ids 
      in 
      let name = 
	match List.assoc "name" o with
	  | Rc.RCident s -> s
	  | Rc.RCstring s -> s
	  | _ -> raise Not_found		  
      in
      let beh = 
	match List.assoc "behavior" o with
	  | Rc.RCstring s -> s
	  | _ -> raise Not_found		  
      in
      (name,beh,(f,l,b,e))
    with Not_found -> ("function "^ids,"Correctness", Loc.extract loc)
      
  in
  Hashtbl.add program_locs ids vloc;

  (*let ol,v = Vcg.vcg ids cc in*)
  begin match wp with
    | None -> 
	if_debug eprintf "no WP => no obligation@."
    | Some wp -> 
	if_debug eprintf "VCs from WP...@?";
	let ol,pr = Vcg.vcg_from_wp loc ids name beh wp in
	if_debug eprintf "done@.";
	push_obligations vloc ol;
	push_validation (ids ^ "_wp") (TTpred wp.a_value) (CC_hole pr)
  end;

  if valid then
    let ren = initial_renaming env in
    let tt = Monad.trad_type_c ren env c in
    let ren = initial_renaming env in
    let cc = Red.red (Mlize.trad p ren) in
(*    Coq.push_parameter (ids ^ "_valid") tt; *)
      Coq.push_program (ids ^ "_functional") tt cc;
    (*** TODO
	 push_validation ids tt v;
	 if_verbose_2 eprintf "%d proof obligation(s)@\n@." (List.length ol);
    ***)
    flush stderr

(*s Processing of a program. *)

let add_external loc v id =
  if Env.is_global id then raise_located loc (Clash id);
  Env.add_global_gen id v None

let add_parameter v tv id =
  push_parameter (Ident.string id) v tv

let rec is_a_type_var = function
  | PTvar { type_val = None } -> true
  | PTvar { type_val = Some t } -> is_a_type_var t
  | _ -> false

let rec polymorphic_pure_type = function
  | PTvar { type_val = None } -> true
  | PTexternal (l,_) -> List.exists polymorphic_pure_type l
  | PTint | PTbool | PTreal | PTunit | PTvar _ -> false

let cannot_be_generalized = function
  | Ref _ -> true
  | PureType pt -> is_a_type_var pt
  | Arrow _ -> false

let logic_type_is_var = function
  | Function ([], pt) -> is_a_type_var pt
  | Function _ | Predicate _ -> false

let check_duplicate_params l =
  let rec loop l acc =
    match l with
      | [] -> ()
      | (loc,x,_)::rem ->
	  if Ident.Idset.mem x acc then
	    raise_located loc (ClashParam x)
	  else loop rem (Ident.Idset.add x acc)
  in
  loop l Ident.Idset.empty

let interp_decl ?(prelude=false) d = 
  let lab = Label.empty in
  match d with 
    | Program (loc,id, p) ->
	if Env.is_global id then raise_located p.ploc (Clash id);
	(try interp_program loc id p with Exit -> ())
    | Parameter (loc, ext, ids, v) ->
	let env = Env.empty_progs () in
	let v = Ltyping.type_v loc lab env v in
	if ext && is_mutable v then raise_located loc MutableExternal;
	let gv = Env.generalize_type_v v in
	let v_spec = snd (specialize_type_scheme gv) in
	if not (Vset.is_empty gv.scheme_vars) && cannot_be_generalized v_spec 
	then
	  raise_located loc CannotGeneralize;
	List.iter (add_external loc gv) ids;
	if ext && ocaml_externals || ocaml then 
	  Ocaml.push_parameters ids v_spec;
	if valid && not ext && not (is_mutable v_spec) then begin
	  let tv = Monad.trad_scheme_v (initial_renaming env) env gv in
	  List.iter (add_parameter gv tv) ids
	end
    | Exception (loc, id, v) ->
	let env = Env.empty_progs () in
	if is_exception id then raise_located loc (ClashExn id);
	let v = option_app (Ltyping.pure_type env) v in
	begin match v with
	  | Some pt when polymorphic_pure_type pt ->
	      raise_located loc CannotGeneralize
	  | _ -> () 
	end;
	add_exception id v
    | Logic (loc, ext, ids, t) ->
	let add id =
	  if is_global_logic id then raise_located loc (Clash id);
	  let t = Ltyping.logic_type t in
	  if logic_type_is_var t then raise_located loc CannotGeneralize;
	  let t = generalize_logic_type t in
	  add_global_logic id t;
	  if ext then 
	    begin 
	      Monomorph.add_external id ;
	    end
	  else
	    begin
	      push_decl ("","",Loc.dummy_floc) (Dlogic (Loc.extract loc, Ident.string id, t));
	    end 
	in
	List.iter add ids
    | Predicate_def (loc, id, pl, p) ->
	let env = Env.empty_logic () in
	if is_global_logic id then raise_located loc (Clash id);
	check_duplicate_params pl;
	let pl = List.map (fun (_,x,t) -> (x, Ltyping.pure_type env t)) pl in
	let t = Predicate (List.map snd pl) in
	let env' = List.fold_right (fun (x,pt) -> add_logic x pt) pl env in
	let p = Ltyping.predicate lab env' p in
	add_global_logic id (generalize_logic_type t);
	let p = generalize_predicate_def (pl,p) in
	push_decl ("","",Loc.dummy_floc) (Dpredicate_def (Loc.extract loc, Ident.string id, p))
    | Function_def (loc, id, pl, ty, e) ->
	let env = Env.empty_logic () in
	if is_global_logic id then raise_located loc (Clash id);
	check_duplicate_params pl;
	let pl = List.map (fun (_,x,t) -> (x, Ltyping.pure_type env t)) pl in
	let ty = Ltyping.pure_type env ty in
	let t = Function (List.map snd pl, ty) in
	let env' = List.fold_right (fun (x,pt) -> add_logic x pt) pl env in
	let e,ty' = Ltyping.term lab env' e in
	if not (eq_pure_type ty ty') then 
	  Ltyping.expected_type loc (PureType ty);
	add_global_logic id (generalize_logic_type t);
	let f = generalize_function_def (pl,ty,e) in
	push_decl ("","",Loc.dummy_floc) (Dfunction_def (Loc.extract loc, Ident.string id, f))
    | Axiom (loc, id, p) ->
	let env = Env.empty_logic () in
	let p = Ltyping.predicate lab env p in
	let p = generalize_predicate p in
	push_decl ("","",Loc.dummy_floc) (Daxiom (Loc.extract loc, Ident.string id, p))

    | Goal (loc, id, p) ->
	let env = Env.empty_logic () in
	let p = Ltyping.predicate lab env p in
	let s = ([], p) in
	let l = Loc.extract loc in
	let xpl =
	  { lemma_or_fun_name = Ident.string id;
	    behavior = "";
	    vc_loc = l;
	    vc_kind = EKLemma;
	  }
	in
	let (l,xpl,id,s) = 
	  if Options.pruning_hyp_v != -1 then
	    (Hypotheses_filtering.reduce (l, xpl, Ident.string id, s) declarationQueue)
	  else	  
	    (l, xpl, Ident.string id, s) 
	in
	let s= generalize_sequent s in 
	let dg = 
	  Dgoal (l, xpl, id, s) 
	in
	let ids = id in
	let vloc =
	  try 
	    let (f,l,b,e,o) = Hashtbl.find locs_table ids in 
	    let name = 
	      match List.assoc "name" o with
		| Rc.RCident s -> s
		| Rc.RCstring s -> s
		| _ -> raise Not_found		  
	    in
	    (name,"Validity",(f,l,b,e))
	  with Not_found -> ("goal "^ids,"Validity", Loc.extract loc)
	in
	Hashtbl.add program_locs ids vloc;
	push_decl ("","",Loc.dummy_floc) dg
	  

    | TypeDecl (loc, ext, vl, id) ->
	Env.add_type loc vl id;
	let vl = List.map Ident.string vl in
	if not ext then 
	  push_decl ("","",Loc.dummy_floc) (Dtype (Loc.extract loc, vl, Ident.string id))
	    
(*s Prelude *)

let load_file ?(prelude=false) f =
  let c = open_in f in
  let p = Lexer.parse_file (Lexing.from_channel c) in
  List.iter (interp_decl ~prelude) p;
  close_in c

let load_prelude () =
  try
    List.iter (load_file ~prelude:true) lib_files_to_load;
    begin match prover () with
      | Simplify when no_simplify_prelude -> Simplify.reset ()
      | _ -> ()
    end
  with e ->
    eprintf "anomaly while reading prelude: %a@." Report.explain_exception e;
    exit 1

(*s Processing of a channel / a file. *)

let why_parser f c = 
  let lb = Lexing.from_channel c in
  lb.Lexing.lex_curr_p <- { lb.Lexing.lex_curr_p with Lexing.pos_fname = f };
  Lexer.parse_file lb
    
let deal_channel parsef cin =
  let p = parsef cin in
  if not parse_only then List.iter interp_decl p

let single_file () = match prover () with
  | Simplify | Harvey | Zenon | CVCLite | Gappa | Dispatcher 
  | SmtLib | Why | MultiWhy | WhyProject -> true
  | Coq _ | Pvs | Mizar | Hol4 | HolLight | Isabelle -> false

let deal_file f =
  reset ();
  let cin = open_in f in 
  deal_channel (why_parser f) cin;
  close_in cin;
  let fwe = Filename.chop_extension f in
  if not (single_file ()) then output (Options.out_file fwe)

let main () =
  let t0 = Unix.times () in
  load_prelude ();
  if files = [] then begin
    deal_channel (why_parser "standard input") stdin;
    output (Options.out_file "out")
  end else begin
    List.iter deal_file files;
    if (pruning) or (Options.pruning_hyp_v != -1) then
      begin
	let q =  declarationQueue in 
	encode q ;
	if single_file () then 
	  let lf = Filename.chop_extension (last files) in
	  output (Options.out_file lf)  
      end
    else if single_file () then 
      let lf = Filename.chop_extension (last files) in
      output (Options.out_file lf)
  end;
  if show_time then
    let t1 = Unix.times () in
    printf "Why execution time : %3.2f@." 
      (t1.Unix.tms_utime -. t0.Unix.tms_utime)


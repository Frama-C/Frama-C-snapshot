(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

open Cil_types

let dkey = Options.dkey_dup

(* ********************************************************************** *)
(* Environment *)
(* ********************************************************************** *)

let fct_tbl: unit Kernel_function.Hashtbl.t = Kernel_function.Hashtbl.create 7
let is_generated kf = Kernel_function.Hashtbl.mem fct_tbl kf

let actions = Queue.create ()

module Global: sig
  val add_logic_info: logic_info -> unit
  val mem_logic_info: logic_info -> bool
  val reset: unit -> unit
end = struct

  let tbl = Cil_datatype.Logic_info.Hashtbl.create 7
  let add_logic_info x = Cil_datatype.Logic_info.Hashtbl.add tbl x ()
  let mem_logic_info x = Cil_datatype.Logic_info.Hashtbl.mem tbl x
  let reset () = Cil_datatype.Logic_info.Hashtbl.clear tbl

end

let reset () = 
  Kernel_function.Hashtbl.clear fct_tbl;
  Global.reset ();
  Queue.clear actions

(* ********************************************************************** *)
(* Duplicating property statuses *)
(* ********************************************************************** *)

let reemit = function
  | Property.IPBehavior _ | Property.IPAxiom _ | Property.IPAxiomatic _
  | Property.IPPredicate (Property.PKAssumes _, _, _, _) -> false
  | _ -> true

let copy_ppt old_prj new_prj old_ppt new_ppt =
  let module P = Property_status in
  let selection = State_selection.of_list [ P.self; Emitter.self ] in
  let emit s l =
    Project.on ~selection new_prj 
      (fun s ->
	let e = match l with [] -> assert false | e :: _ -> e in
	let emitter = Emitter.Usable_emitter.get e.P.emitter in
	match s with 
	| P.True | P.False_and_reachable | P.Dont_know ->
	  P.emit emitter ~hyps:e.P.properties new_ppt s
	| P.False_if_reachable -> 
	  (* in that case, the only hypothesis is "Reachable new_ppt" which must
	     be automatically added by the kernel *)
	  P.emit emitter ~hyps:[] new_ppt P.False_if_reachable)
      s
  in
  let copy () =
    match Project.on ~selection old_prj P.get old_ppt with
    | P.Never_tried -> ()
    | P.Best(s, l) -> emit s l
    | P.Inconsistent i ->
      emit P.True i.P.valid;
      emit P.False_and_reachable i.P.invalid
  in
  if reemit old_ppt && not (Options.Valid.get ()) then Queue.add copy actions

(* ********************************************************************** *)
(* Duplicating functions *)
(* ********************************************************************** *)

let dup_spec_status old_prj kf new_kf old_spec new_spec =
  let old_ppts = Property.ip_of_spec kf Kglobal ~active:[] old_spec in
  let new_ppts = Property.ip_of_spec new_kf Kglobal ~active:[] new_spec in
  List.iter2 (copy_ppt old_prj (Project.current ())) old_ppts new_ppts

let dup_funspec tbl bhv spec =
  (*  Options.feedback "DUP SPEC %a" Cil.d_funspec spec;*)
  let o = object
    inherit Cil.genericCilVisitor (Cil.copy_visit (Project.current ()))

    val already_visited = Cil_datatype.Logic_var.Hashtbl.create 7

    method !vlogic_info_use li =
      if Global.mem_logic_info li then
	Cil.ChangeDoChildrenPost
	  ({ li with l_var_info = li.l_var_info } (* force a copy *),
	   Cil.get_logic_info bhv)
      else
	Cil.JustCopy

    method !vterm_offset _ =
      Cil.DoChildrenPost
	(function
	(* no way to directly visit fieldinfo and model_info uses *)	
	| TField(fi, off) -> TField(Cil.get_fieldinfo bhv fi, off)
	| TModel(mi, off) -> TModel(Cil.get_model_info bhv mi, off)
	| off -> off)

    method !vlogic_var_use orig_lvi =
      match orig_lvi.lv_origin with
      | None -> 
	Cil.JustCopy
      | Some vi ->
	try
	  let new_lvi = 
	    Cil_datatype.Logic_var.Hashtbl.find already_visited orig_lvi 
	  in
	  Cil.ChangeTo new_lvi
	with Not_found ->
	  Cil.ChangeDoChildrenPost
	    ({ orig_lvi with lv_id = orig_lvi.lv_id } (* force a copy *),
	     fun lvi -> 
	       (* using [Cil.get_logic_var bhv lvi] is correct only because the
		  lv_id used to compare the lvi does not change between the
		  original one and this copy *)
	       try 
		 let new_vi = Cil_datatype.Varinfo.Hashtbl.find tbl vi in
		 Cil_datatype.Logic_var.Hashtbl.add 
		   already_visited orig_lvi lvi;
		 lvi.lv_id <- new_vi.vid;
		 lvi.lv_name <- new_vi.vname;
		 lvi.lv_origin <- Some new_vi;
		 new_vi.vlogic_var_assoc <- Some lvi;
		 lvi
	       with Not_found -> 
		 assert vi.vglob;
		 Cil.get_logic_var bhv lvi)

    method !videntified_term _ = 
      Cil.DoChildrenPost Logic_const.refresh_identified_term

    method !videntified_predicate _ = 
      Cil.DoChildrenPost Logic_const.refresh_predicate
  end in
  Cil.visitCilFunspec o spec

let dup_fundec loc spec bhv kf vi new_vi =
  new_vi.vdefined <- true;
  let formals = Kernel_function.get_formals kf in
  let mk_formal vi =
    let name =
      if vi.vname = "" then
        (* unamed formal parameter: must generate a fresh name since a fundec
           cannot have unnamed formals (see bts #2303). *)
        Env.Varname.get ~scope:Env.Function
          (Misc.mk_gen_name "unamed_formal")
      else
        vi.vname
    in
    Cil.copyVarinfo vi name
  in
  let new_formals = List.map mk_formal formals in
  let res =
    let ty = Kernel_function.get_return_type kf in
    if Cil.isVoidType ty then None
    else Some (Cil.makeVarinfo false false "__retres" ty)
  in
  let return =
    Cil.mkStmt ~valid_sid:true
      (Return(Extlib.opt_map (Cil.evar ~loc) res, loc))
  in
  let stmts = 
    [ Cil.mkStmtOneInstr ~valid_sid:true
	(Call(Extlib.opt_map Cil.var res,
	      Cil.evar ~loc vi, 
	      List.map (Cil.evar ~loc) new_formals, 
	      loc)); 
      return ]
  in
  let locals = match res with None -> [] | Some r -> [ r ] in
  let body = Cil.mkBlock stmts in
  body.blocals <- locals;
  let tbl = Cil_datatype.Varinfo.Hashtbl.create 7 in
  List.iter2 (Cil_datatype.Varinfo.Hashtbl.add tbl) formals new_formals;
  let new_spec = dup_funspec tbl bhv spec in
  { svar = new_vi;
    sformals = new_formals;
    slocals = locals;
    smaxid = List.length new_formals;
    sbody = body;
    smaxstmtid = None;
    sallstmts = [];
    sspec = new_spec }

let dup_global loc old_prj spec bhv kf vi new_vi = 
  let name = vi.vname in
  Options.feedback ~dkey ~level:2 "entering in function %s" name;
  let fundec = dup_fundec loc spec bhv kf vi new_vi  in
  let fct = Definition(fundec, loc) in
  let new_spec = fundec.sspec in
  let new_kf = { fundec = fct; spec = new_spec } in
  Kernel_function.Hashtbl.add fct_tbl new_kf ();
  Globals.Functions.register new_kf;
  Globals.Functions.replace_by_definition new_spec fundec loc;
  Annotations.register_funspec new_kf;
  dup_spec_status old_prj kf new_kf spec new_spec;
  Options.feedback ~dkey ~level:2 "function %s" name;
  GFun(fundec, loc), GFunDecl(new_spec, new_vi, loc)

(* ********************************************************************** *)
(* Visitor *)
(* ********************************************************************** *)

type position = Before_gmp | Gmp | After_gmp | Memory_model | Code

class dup_functions_visitor prj = object (self)
  inherit Visitor.frama_c_copy prj

  val fct_tbl = Cil_datatype.Varinfo.Hashtbl.create 7
  val mutable before_memory_model = Before_gmp
  val mutable new_definitions: global list = []
  (* new definitions of the annotated functions which will contain the
     translation of the E-ACSL constract *)

  method private before_memory_model = match before_memory_model with
  | Before_gmp | Gmp | After_gmp -> true
  | Memory_model | Code -> false

  method private insert_libc l =
    match new_definitions with
    | [] -> l
    | _ :: _ ->
      (* add the generated definitions of libc at the end of [l]. This way,
         we are sure that they have access to all of it (in particular, the
         memory model and GMP) *)
      let res = l @ new_definitions in
      new_definitions <- [];
      res

  method private next () =
    match before_memory_model with
    | Before_gmp -> ()
    | Gmp -> before_memory_model <- After_gmp
    | After_gmp -> ()
    | Memory_model -> before_memory_model <- Code
    | Code -> ()

  method !vcode_annot a =
    Cil.JustCopyPost
      (fun a' ->
	let get_ppt kf stmt a = Property.ip_of_code_annot kf stmt a in
	let kf = Extlib.the self#current_kf in
	let stmt = Extlib.the self#current_stmt in
	List.iter2
	  (fun p p' -> copy_ppt (Project.current ()) prj p p')
	  (get_ppt kf stmt a)
	  (get_ppt
	     (Cil.get_kernel_function self#behavior kf) 
	     (Cil.get_stmt self#behavior stmt)
	     a');
	a')

  method !vlogic_info_decl li = 
    Global.add_logic_info li;
    Cil.JustCopy

  method !vvrbl vi =
    try
      let new_vi = Cil_datatype.Varinfo.Hashtbl.find fct_tbl vi in
      Cil.ChangeTo new_vi
    with Not_found ->
      Cil.JustCopy

  method private is_unvariadic_function vi =
    match Cil.unrollType vi.vtype with
    | TFun(_, _, variadic, _) -> not variadic
    | _ -> false

  method !vglob_aux = function
  | GVarDecl(vi, loc) | GFunDecl(_, vi, loc) | GFun({ svar = vi }, loc)
      when self#is_unvariadic_function vi
	&& not (Misc.is_library_loc loc) 
	&& not (Cil.is_builtin vi)
	&& not (Cil_datatype.Varinfo.Hashtbl.mem fct_tbl vi)
	&& not (Cil.is_empty_funspec
		  (Annotations.funspec ~populate:false
		     (Extlib.the self#current_kf)))
	-> 
    self#next ();
    let name = Misc.mk_gen_name vi.vname in
    let new_vi = 
      Project.on prj (Cil.makeGlobalVar name) vi.vtype
    in
    Cil_datatype.Varinfo.Hashtbl.add fct_tbl vi new_vi;
    Cil.DoChildrenPost
      (fun l -> match l with
      | [ GVarDecl(vi, _) | GFunDecl(_, vi, _) | GFun({ svar = vi }, _) as g ]
        ->
	(match g with
        | GFunDecl _ ->
	  if not (Kernel_function.is_definition (Extlib.the self#current_kf))
            && vi.vname <> "malloc" && vi.vname <> "free" 
          then
	    Options.warning "@[annotating undefined function `%a':@ \
the generated program may miss memory instrumentation@ \
if there are memory-related annotations.@]"
	    Printer.pp_varinfo vi
	| GFun _ -> ()
	| _ -> assert false);
	let tmp = vi.vname in
	if tmp = Kernel.MainFunction.get () then begin
	  (* the new function becomes the new main: simply swap the name of both
	     functions *)
	  vi.vname <- new_vi.vname;
	  new_vi.vname <- tmp
	end;
	let kf = 
	  try 
	    Globals.Functions.get (Cil.get_original_varinfo self#behavior vi)
	  with Not_found -> 
	    Options.fatal
	      "unknown function `%s' while trying to duplicate it" 
	      vi.vname
	in
	let spec = Annotations.funspec ~populate:false kf in
	let vi_bhv = Cil.get_varinfo self#behavior vi in
        let new_g, new_decl =
          Project.on prj
            (dup_global loc (Project.current ()) spec self#behavior kf vi_bhv)
            new_vi
        in
        (* postpone the introduction of the new function definition to the
           end *)
        new_definitions <- new_g :: new_definitions;
        (* put the declaration before the original function in order to solve
           issue with recursive functions *)
        [ new_decl; g ]
      | _ -> assert false)
  | GVarDecl(_, loc) | GFunDecl(_, _, loc) | GFun(_, loc)
      when Misc.is_library_loc loc ->
    (match before_memory_model with
    | Before_gmp -> before_memory_model <- Gmp
    | Gmp | Memory_model -> ()
    | After_gmp -> before_memory_model <- Memory_model
    | Code -> () (* still processing the GMP and memory model headers,
                    but reading some libc code *));
    Cil.JustCopy
  | GVarDecl(vi, _) | GFunDecl(_, vi, _) | GFun({ svar = vi }, _)
      when Cil.is_builtin vi ->
    self#next ();
    Cil.JustCopy
  | _ ->
    self#next ();
    Cil.DoChildren

  method !vfile _ =
    Cil.DoChildrenPost
      (fun f ->
        match new_definitions with
        | [] -> f
        | _ :: _ ->
          (* required by the few cases where there is no global tagged as
             [Code] in the file. *)
          f.globals <- self#insert_libc f.globals;
          f)

  initializer
    Project.copy ~selection:(Parameter_state.get_selection ()) prj;
    reset ()

end

let dup () =
  Options.feedback ~level:2 "duplicating annotated functions";
  let prj =
    File.create_project_from_visitor
      "e_acsl_dup_functions" 
      (new dup_functions_visitor)
  in
  Queue.iter (fun f -> f ()) actions;
  prj

(*
Local Variables:
compile-command: "make"
End:
*)

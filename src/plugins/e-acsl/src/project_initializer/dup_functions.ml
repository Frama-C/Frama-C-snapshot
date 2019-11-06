(**************************************************************************)
(*                                                                        *)
(*  This file is part of the Frama-C's E-ACSL plug-in.                    *)
(*                                                                        *)
(*  Copyright (C) 2012-2019                                               *)
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
(* Duplicating functions *)
(* ********************************************************************** *)

let dup_funspec tbl bhv spec =
  (*  Options.feedback "DUP SPEC %a" Cil.d_funspec spec;*)
  let o = object
    inherit Cil.genericCilVisitor bhv

    val already_visited = Cil_datatype.Logic_var.Hashtbl.create 7

    method !vlogic_info_use li =
      if Global.mem_logic_info li then
	Cil.ChangeDoChildrenPost
	  ({ li with l_var_info = li.l_var_info } (* force a copy *),
	   Visitor_behavior.Get.logic_info bhv)
      else
	Cil.JustCopy

    method !vterm_offset _ =
      Cil.DoChildrenPost
	(function
	(* no way to directly visit fieldinfo and model_info uses *)	
	| TField(fi, off) -> TField(Visitor_behavior.Get.fieldinfo bhv fi, off)
	| TModel(mi, off) -> TModel(Visitor_behavior.Get.model_info bhv mi, off)
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
	       (* using [Visitor_behavior.Get.logic_var bhv lvi] is correct only because the
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
		 Visitor_behavior.Get.logic_var bhv lvi)

    method !videntified_term _ = 
      Cil.DoChildrenPost Logic_const.refresh_identified_term

    method !videntified_predicate _ = 
      Cil.DoChildrenPost Logic_const.refresh_predicate
  end in
  Cil.visitCilFunspec o spec

let dup_fundec loc spec bhv sound_verdict_vi kf vi new_vi =
  new_vi.vdefined <- true;
  let formals = Kernel_function.get_formals kf in
  let mk_formal vi =
    let name =
      if vi.vname = "" then
        (* unnamed formal parameter: must generate a fresh name since a fundec
           cannot have unnamed formals (see bts #2303). *)
        Varname.get
          ~scope:Varname.Function
          (Functions.RTL.mk_gen_name "unamed_formal")
      else
        vi.vname
    in
    Cil.copyVarinfo vi name
  in
  let new_formals = List.map mk_formal formals in
  let res =
    let ty = Kernel_function.get_return_type kf in
    if Cil.isVoidType ty then None
    else Some (Cil.makeVarinfo false false ~referenced:true "__retres" ty)
  in
  let return =
    Cil.mkStmt ~valid_sid:true
      (Return(Extlib.opt_map (Cil.evar ~loc) res, loc))
  in
  let stmts =
    let l =
      [ Cil.mkStmtOneInstr ~valid_sid:true
          (Call(Extlib.opt_map Cil.var res,
                Cil.evar ~loc vi,
                List.map (Cil.evar ~loc) new_formals,
                loc));
        return ]
    in
    if Functions.instrument kf then
      l
    else
      (* set the 'unsound_verdict' variable to 'false' whenever required *)
      let unsound =
        Cil.mkStmtOneInstr ~valid_sid:true
          (Set((Var sound_verdict_vi, NoOffset), Cil.zero ~loc, loc))
      in
      unsound :: l
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

let dup_global loc actions spec bhv sound_verdict_vi kf vi new_vi =
  let name = vi.vname in
  Options.feedback ~dkey ~level:2 "entering in function %s" name;
  let fundec = dup_fundec loc spec bhv sound_verdict_vi kf vi new_vi  in
  let fct = Definition(fundec, loc) in
  let new_spec = fundec.sspec in
  let new_kf = { fundec = fct; spec = new_spec } in
  Queue.add (fun () ->
      Kernel_function.Hashtbl.add fct_tbl new_kf ();
      Globals.Functions.register new_kf;
      Globals.Functions.replace_by_definition new_spec fundec loc;
      Annotations.register_funspec new_kf)
  actions;
  Options.feedback ~dkey ~level:2 "function %s" name;
  (* remove the specs attached to the previous kf iff it is a definition:
     it is necessary to keep stable the number of annotations in order to get
     [Keep_status] working fine. *)
  let kf = Visitor_behavior.Get.kernel_function bhv kf in
  if Kernel_function.is_definition kf then begin
    Queue.add
      (fun () ->
        let bhvs =
          Annotations.fold_behaviors (fun e b acc -> (e, b) :: acc) kf []
        in
        List.iter
          (fun (e, b) -> Annotations.remove_behavior ~force:true e kf b)
          bhvs;
        Annotations.iter_decreases
          (fun e _ -> Annotations.remove_decreases e kf)
          kf;
        Annotations.iter_terminates
          (fun e _ -> Annotations.remove_terminates e kf)
          kf;
        Annotations.iter_complete
          (fun e l -> Annotations.remove_complete e kf l)
          kf;
        Annotations.iter_disjoint
          (fun e l -> Annotations.remove_disjoint e kf l)
          kf)
      actions
  end;
  GFun(fundec, loc), GFunDecl(new_spec, new_vi, loc)

(* ********************************************************************** *)
(* Visitor *)
(* ********************************************************************** *)

type position = Before_gmp | Gmpz | After_gmp | Memory_model | Code

class dup_functions_visitor prj = object (self)
  inherit Visitor.frama_c_copy prj

  val unduplicable_functions =
    let white_list =
      [ "__builtin_va_arg";
        "__builtin_va_end";
        "__builtin_va_start";
        "__builtin_va_copy" ]
    in
    List.fold_left
      (fun acc s -> Datatype.String.Set.add s acc)
      Datatype.String.Set.empty
      white_list

  val fct_tbl = Cil_datatype.Varinfo.Hashtbl.create 7
  val mutable before_memory_model = Before_gmp
  val mutable new_definitions: global list = []
  (* new definitions of the annotated functions which will contain the
     translation of the E-ACSL contract *)

  val mutable sound_verdict_vi =
    let name = Functions.RTL.mk_api_name "sound_verdict" in
    let vi = Project.on prj (Cil.makeGlobalVar name) Cil.intType in
    vi.vstorage <- Extern;
    vi.vreferenced <- true;
    vi

  method private before_memory_model = match before_memory_model with
  | Before_gmp | Gmpz | After_gmp -> true
  | Memory_model | Code -> false

  method private insert_libc l =
    match new_definitions with
    | [] -> l
    | _ :: _ ->
      (* add the generated definitions of libc at the end of [l]. This way,
         we are sure that they have access to all of it (in particular, the
         memory model, GMP and the soundness variable).

         Also add the [__e_acsl_sound_verdict] variable at the beginning *)
      let res =
        GVarDecl(sound_verdict_vi, Cil_datatype.Location.unknown)
        :: l
        @ new_definitions in
      new_definitions <- [];
      res

  method private next () =
    match before_memory_model with
    | Before_gmp -> ()
    | Gmpz -> before_memory_model <- After_gmp
    | After_gmp -> ()
    | Memory_model -> before_memory_model <- Code
    | Code -> ()

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
  | GFunDecl(_, vi, loc) | GFun({ svar = vi }, loc)
      when (* duplicate a function iff: *)
        not (Cil_datatype.Varinfo.Hashtbl.mem fct_tbl vi)
        (* it is not already duplicated *)
        && not (Datatype.String.Set.mem vi.vname unduplicable_functions)
        (* it is duplicable *)
        && self#is_unvariadic_function vi (* it is not a variadic function *)
        && not (Misc.is_library_loc loc) (* it is not in the E-ACSL's RTL *)
        && not (Cil.is_builtin vi) (* it is not a Frama-C built-in *)
        &&
        (let kf =
           try Globals.Functions.get vi with Not_found -> assert false
         in
         not (Functions.instrument kf)
         (* either explicitely listed as to be not instrumented *)
         ||
         (* or: *)
         (not (Cil.is_empty_funspec
                 (Annotations.funspec ~populate:false
                    (Extlib.the self#current_kf)))
          (* it has a function contract *)
          && Functions.check kf
          (* its annotations must be monitored *)))
      ->
    self#next ();
    let name = Functions.RTL.mk_gen_name vi.vname in
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
            Globals.Functions.get (Visitor_behavior.Get_orig.varinfo self#behavior vi)
          with Not_found ->
            Options.fatal
              "unknown function `%s' while trying to duplicate it"
              vi.vname
        in
        let spec = Annotations.funspec ~populate:false kf in
        let vi_bhv = Visitor_behavior.Get.varinfo self#behavior vi in
        let new_g, new_decl =
          dup_global
            loc
            self#get_filling_actions
            spec
            self#behavior
            sound_verdict_vi
            kf
            vi_bhv
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
    | Before_gmp -> before_memory_model <- Gmpz
    | Gmpz | Memory_model -> ()
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

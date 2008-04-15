(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(**************************************************************************)

(* $Id: rewrite.ml,v 1.87 2008/11/24 16:09:49 uid570 Exp $ *)

(* Import from Cil *)
open Cil_types
open Cil
open Cilutil
open Ast_info
open Extlib

open Db_types
open Visitor

(* Utility functions *)
open Common

(*****************************************************************************)
(* Adds a default behavior for all functions                                 *)
(*****************************************************************************)

class add_default_behavior =
  object(self)
    inherit Visitor.generic_frama_c_visitor (Project.current())
      (Cil.inplace_visit())
    method vspec s =
      if not (List.exists (fun x -> x.b_name = Common.name_of_default_behavior)
                s.spec_behavior)
      then begin
        s.spec_behavior <-
          { b_name = name_of_default_behavior;
            b_assigns = [];
            b_assumes = [];
            b_ensures = [];
          } :: s.spec_behavior
      end;
      SkipChildren

    method vcode_annot _ = SkipChildren

    method vfile f =
      let init = Globals.Functions.get_glob_init f in
      ignore (visitFramacFunspec (self:>Visitor.frama_c_visitor) init.spec);
      DoChildren
  end

let add_default_behavior f =
  let vis = new add_default_behavior in Visitor.visitFramacFile vis f


(*****************************************************************************)
(* Rename entities to avoid conflicts with Jessie predefined names.          *)
(*****************************************************************************)

class renameEntities
  (add_variable : varinfo -> unit) (add_logic_variable : logic_var -> unit) =
  let types = TypeHashtbl.create 17 in
  let add_field fi =
    fi.fname <- unique_name fi.fname
  in
  let add_type ty =
    if TypeHashtbl.mem types ty then () else
      let compinfo = get_struct_info ty in
      compinfo.cname <- unique_name compinfo.cname;
      List.iter add_field compinfo.cfields;
      TypeHashtbl.add types ty ()
  in
object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vfunc f =
    List.iter add_variable f.slocals;
    DoChildren

  method vglob_aux = function
    | GCompTag(compinfo,_loc) 
    | GCompTagDecl(compinfo,_loc) ->
	add_type (TComp(compinfo,[]));
	SkipChildren
    | GVarDecl _ | GVar _ | GFun _ | GAnnot _ | GType _
    | GEnumTagDecl _ | GEnumTag _ | GAsm _ | GPragma _ | GText _ ->
	DoChildren

(* UNCOMMENT WHEN SHARING RESTORED (FS#385) *)

(*   method vpredicate = function *)
(*     | Pforall(vars,_p) | Pexists(vars,_p) -> *)
(* 	List.iter add_logic_variable vars; *)
(* 	DoChildren *)
(*     | _ -> DoChildren *)

  method vterm t = match t.term_node with
    | Tlambda(vars,_t1) ->
	List.iter add_logic_variable vars;
	DoChildren
    | _ -> DoChildren

  method vtsets = function
    | TSComprehension(_ts,vars,_p) ->
	List.iter add_logic_variable vars;
	DoChildren
    | _ -> DoChildren

  method vlogic_var_use v =
    let postaction v =
      (* Restore consistency between C variable name and logical name *)
      opt_app (fun cv -> v.lv_name <- cv.vname) () v.lv_origin; v
    in
    ChangeDoChildrenPost(v,postaction)
end

let rename_entities file =
  let add_variable v =
    v.vname <- unique_name v.vname;
    match v.vlogic_var_assoc with
      | None -> ()
      | Some lv -> lv.lv_name <- v.vname
  in
  let add_logic_variable v =
    assert (v.lv_origin  = None);
    v.lv_name <- unique_logic_name v.lv_name
  in
  Globals.Vars.iter (fun v _init -> add_variable v);
  Globals.Functions.iter
    (fun kf ->
       add_variable (Globals.Functions.get_vi kf);
       List.iter add_variable (Globals.Functions.get_params kf));
  Globals.Annotations.replace_all 
    (fun annot gen -> 
       let rec replace_annot annot = match annot with
(*
	 | Dpredicate_reads _
	 | Dpredicate_def _
	 | Dinductive_def _
	 | Dlogic_reads _
	 | Dlogic_axiomatic _
	 | Dlogic_def _ -> annot
*)
	 | Dfun_or_pred _ -> annot
	 | Daxiomatic(id, l) -> Daxiomatic(id, List.map replace_annot l)
	 | Dtype(name,args) ->
	     Dtype(unique_logic_name name, List.map unique_logic_name args)
	 | Dlemma(name,is_axiom,labels,poly,property) ->
	     Dlemma(unique_logic_name name,is_axiom,labels,poly,property)
	 | Dtype_annot info | Dinvariant info ->
	     info.l_name <- unique_logic_name info.l_name;
	     annot
       in replace_annot annot,gen
    );
  Logic_env.LogicInfo.iter
    (fun name _f ->
       let new_name = unique_logic_name name in
       if new_name <> name then
	 begin
	   let info = Logic_env.find_logic_function name in
	   info.l_name <- new_name;
	   Logic_env.remove_logic_function name;
	   Logic_env.add_logic_function info;
	   List.iter add_logic_variable info.l_profile
	 end);
 (*
 Logic_env.PredicateInfo.iter
    (fun name _p ->
       let new_name = unique_logic_name name in
       if new_name <> name then
	 begin
	   let info = Logic_env.find_predicate name in
	   info.p_name <- new_name;
	   Logic_env.remove_predicate name;
	   Logic_env.add_predicate info
	 end);
 *)
  let visitor = new renameEntities (add_variable) (add_logic_variable) in
  visitFramacFile visitor file


(*****************************************************************************)
(* Fill offset/size information in fields                                    *)
(*****************************************************************************)

class fillOffsetSizeInFields =
object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vglob_aux = function
    | GCompTag(compinfo,_loc) ->
	let basety = TComp(compinfo,[]) in
	let field fi nextoff =
	  let size_in_bits = 
	    match fi.fbitfield with
	      | Some siz -> siz
	      | None -> bitsSizeOf fi.ftype 
	  in
	  let offset_in_bits = fst (bitsOffset basety (Field(fi,NoOffset))) in
	  let padding_in_bits = nextoff - (offset_in_bits + size_in_bits) in
	  assert (padding_in_bits >= 0);
	  fi.fsize_in_bits <- Some size_in_bits;
	  fi.foffset_in_bits <- Some offset_in_bits;
	  fi.fpadding_in_bits <- Some padding_in_bits;
	  if compinfo.cstruct then
	    offset_in_bits
	  else nextoff (* union type *)
	in
	ignore(List.fold_right field compinfo.cfields (bitsSizeOf basety));
	SkipChildren
    | _ -> SkipChildren

end

let fill_offset_size_in_fields file =
  let visitor = new fillOffsetSizeInFields in
  visitFramacFile visitor file


(*****************************************************************************)
(* Replace addrof array with startof.                                        *)
(*****************************************************************************)

class replaceAddrofArray =
object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vexpr = function
    | AddrOf lv ->
	if isArrayType(typeOfLval lv) then
	  ChangeDoChildrenPost (StartOf lv, fun x -> x)
	else DoChildren
    | _ -> DoChildren

  method vterm t = match t.term_node with
    | TAddrOf tlv ->
	let ty = force_app_term_type pointed_type t.term_type in
	if isArrayType ty then
	  let t' = { t with
	    term_node = TStartOf tlv;
	    term_type = Ctype (element_type ty);
	  } in
	  ChangeDoChildrenPost (t', fun x -> x)
	else DoChildren
    | _ -> DoChildren

end

let replace_addrof_array file =
  let visitor = new replaceAddrofArray in
  visit_and_update_globals visitor file


(*****************************************************************************)
(* Replace string constants by global variables.                             *)
(*****************************************************************************)

class replaceStringConstants =

  let string_to_var = Hashtbl.create 17 in
  let wstring_to_var = Hashtbl.create 17 in

  (* Use the Cil translation on initializers. First translate to primitive
   * AST to later apply translation in [Cabs.blockInitializer].
   *)
  let string_cabs_init s =
    Cabs.SINGLE_INIT(Cabs.CONSTANT(Cabs.CONST_STRING s))
  in
  let wstring_cabs_init ws =
    Cabs.SINGLE_INIT(Cabs.CONSTANT(Cabs.CONST_WSTRING ws))
  in

  (* Name of variable should be as close as possible to the string it
   * represents. To that end, we just filter out characters not allowed
   * in C names, before we add a discriminating number if necessary.
   *)
  let string_var s =
    let name = unique_name ("__string_" ^ (filter_alphanumeric s [] '_')) in
    makeGlobalVar name (array_type charType)
  in
  let wstring_var () =
    let name = unique_name "__wstring_" in
    makeGlobalVar name (array_type theMachine.wcharType)
(*     makeGlobalVar name (array_type (TInt(IUShort,[]))) *)
  in

  let make_glob ~wstring v size inite =
    (* Apply translation from initializer in primitive AST to block of code,
     * simple initializer and type.
     *)
    let b,init,ty = Cabs2cil.blockInitializer Cilutil.LvalSet.empty v inite in
    (* Precise the array type *)
    v.vtype <- ty;
    (* Attach global variable and code for global initialization *)
    List.iter attach_globinit b.bstmts;
    attach_global (GVar(v,{init=Some init},CurrentLoc.get ()));
    (* Define a global string invariant *)
    let validstring =
      try 
	if wstring then
	  Logic_env.find_logic_function name_of_valid_wstring
	else
	  Logic_env.find_logic_function name_of_valid_string
      with Not_found -> assert false
    in
    let strlen = Logic_env.find_logic_function name_of_strlen in
    let strlen_type =
      match strlen.l_type with Some t -> t | None -> assert false
    in
    let wcslen = Logic_env.find_logic_function name_of_wcslen in
    let wcslen_type =
      match wcslen.l_type with Some t -> t | None -> assert false
    in
    let pstring = 
      Papp(validstring,[],[variable_term v.vdecl (cvar_to_lvar v)]) 
    in
    let tv = term_of_var v in
    let strsize = 
      if wstring then 
	mkterm (Tapp(wcslen,[],[tv])) wcslen_type v.vdecl 
      else
	mkterm (Tapp(strlen,[],[tv])) strlen_type v.vdecl 
    in
    let size = constant_term locUnknown (Int64.of_int size) in
    let psize = Prel(Req,strsize,size) in
    let p = Pand(predicate v.vdecl pstring,predicate v.vdecl psize) in
    let globinv =
      { l_name = unique_logic_name ("valid_" ^ v.vname);
	l_tparams = [];
	l_type = None;
        l_profile = [];
        l_labels = [ LogicLabel "Here" ];
        l_body = LBpred (predicate v.vdecl p)}
    in
    attach_globaction (fun () -> Logic_env.add_logic_function globinv);
    attach_global (GAnnot(Dinvariant globinv,v.vdecl));
    v
  in
object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vexpr = function
    | Const(CStr s) ->
	let v =
	  findOrAdd string_to_var s
	    (fun s -> 
	       make_glob ~wstring:false (string_var s) (String.length s) 
		 (string_cabs_init s))
	in
	ChangeTo (StartOf(Var v,NoOffset))
    | Const(CWStr ws) ->
	let v =
	  findOrAdd wstring_to_var ws
	    (fun ws -> 
	       make_glob ~wstring:true (wstring_var ()) (List.length ws - 1) 
		 (wstring_cabs_init ws))
	in
	ChangeTo (StartOf(Var v,NoOffset))
    | _ -> DoChildren

  method vglob_aux = function
    | GVar(v,{init=Some(SingleInit(Const _))},_) ->
	if isArrayType v.vtype then
	  (* Avoid creating an array for holding the initializer for another
	   * array. This initializer is later cut into individual
	   * initialization statements in [gather_initialization].
	   *)
	  SkipChildren
	else
	  DoChildren
    | _ -> DoChildren

end

let replace_string_constants file =
  let visitor = new replaceStringConstants in
  visit_and_update_globals visitor file


(*****************************************************************************)
(* Put all global initializations in the [globinit] file.                    *)
(* Replace global compound initializations by equivalent statements.         *)
(*****************************************************************************)

let gather_initialization file =
  do_and_update_globals
    (fun _ ->
      Globals.Vars.iter (fun v iinfo ->
	let s = match iinfo.init with
	  | Some ie ->
	      let b = Cabs2cil.blockInit (Var v, NoOffset) ie v.vtype in
	      b.bstmts
	  | None ->
	      if bitsSizeOf v.vtype lsr 3 < 100 then
		(* Enforce zero-initialization of global variables *)
		let ie = makeZeroInit v.vtype in
		let b = Cabs2cil.blockInit (Var v, NoOffset) ie v.vtype in
		b.bstmts
	      else
		(* FS#253: Big data structure, do not initialize individually.
		 * When casts to low-level are supported, call here [memset]
		 * or equivalent to zero the memory.
		 *)
		[]
	in
	(* Too big currently, postpone until useful *)
	ignore s;
(*  	List.iter attach_globinit s; *)
	iinfo.init <- None
      )) file


(*****************************************************************************)
(* Rewrite comparison of pointers into difference of pointers.               *)
(*****************************************************************************)

class rewritePointerCompare =
  let preaction_expr = function
    | BinOp((Lt | Gt | Le | Ge | Eq | Ne as op),e1,e2,ty)
	when isPointerType (typeOf e1) && not (is_null_expr e2) ->
	BinOp(op,BinOp(MinusPP,e1,e2,theMachine.ptrdiffType),
	      constant_expr 0L,ty)
    | e -> e
  in
object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vexpr e =
    ChangeDoChildrenPost (preaction_expr e, fun x -> x)

  method vterm =
    do_on_term (Some preaction_expr,None)

  method vtsets_elem =
    do_on_tsets_elem (Some preaction_expr,None)

  method vpredicate = function
    | Prel(rel,t1,t2)
	when app_term_type isPointerType false t1.term_type
	  && not (is_null_term t1 || is_null_term t2
		  || is_base_addr t1 || is_base_addr t2) ->
	let loc = range_loc t1.term_loc t2.term_loc in
	let tsub = {
	  term_node = TBinOp(MinusPP,t1,t2);
	  term_type = Ctype theMachine.ptrdiffType;
	  term_loc = loc;
	  term_name = [];
	} in
	let p = Prel(rel,tsub,constant_term loc 0L) in
	ChangeDoChildrenPost (p, fun x -> x)
    | _ -> DoChildren

end

let rewrite_pointer_compare file =
  let visitor = new rewritePointerCompare in
  visit_and_store_result_type visitFramacFile visitor file


(*****************************************************************************)
(* Rewrite cursor pointers into offsets from base pointers.                  *)
(*****************************************************************************)

(* Recognize the sum of a pointer variable and an integer offset *)
let rec destruct_pointer e = match stripInfo e with
  | Lval(Var v,NoOffset) | StartOf(Var v,NoOffset) | AddrOf(Var v,NoOffset) ->
      Some(v,None)
  | StartOf(Var v,Index(i,NoOffset)) | AddrOf(Var v,Index(i,NoOffset)) ->
      Some(v,Some i)
  | BinOp((PlusPI | IndexPI | MinusPI as op),e1,e2,_) ->
      begin match destruct_pointer e1 with
	| None -> None
	| Some(v,None) ->
	    begin match op with
	      | PlusPI | IndexPI -> Some(v,Some e2)
	      | MinusPI -> Some(v,Some(UnOp(Neg,e2,typeOf e2)))
	      | _ -> assert false
	    end
	| Some(v,Some off) ->
	    begin match op with
	      | PlusPI | IndexPI -> Some(v,Some(BinOp(PlusA,off,e2,typeOf e2)))
	      | MinusPI -> Some(v,Some(BinOp(MinusA,off,e2,typeOf e2)))
	      | _ -> assert false
	    end
      end
  | CastE(ty,e) ->
      let ety = typeOf e in
      if isPointerType ty && isPointerType ety
	&& (typeSig (typeRemoveAttributes ["const";"volatile"]
		       (unrollType (pointed_type ty)))
	    =
	    typeSig (typeRemoveAttributes ["const";"volatile"]
		       (unrollType (pointed_type ety)))) then
(* 	&& bitsSizeOf(pointed_type ty) = bitsSizeOf(pointed_type ety) then *)
	  destruct_pointer e
      else None
  | _ -> None

class collectCursorPointers
  (cursor_to_base : varinfo VarinfoHashtbl.t) (* local variable to base *)
  (formal_to_base : varinfo VarinfoHashtbl.t) (* formal variable to base *)
  (assigned_vars : VarinfoSet.t ref) (* variable is assigned (for formals) *)
  (ignore_vars : VarinfoSet.t ref) (* ignore info on these variables *) =

  let curFundec : fundec ref = ref (emptyFunction "@dummy@") in

  let candidate_var v =
    not v.vglob
    && ((isPointerType v.vtype && not v.vaddrof) || isArrayType v.vtype)
  in
  (* Variable should not be translated as base or cursor *)
  let add_ignore_vars v =
    if not (VarinfoSet.mem v !ignore_vars) then
      begin
	ignore_vars := VarinfoSet.add v !ignore_vars; signal_change ()
      end
  in
  (* Variable [v] used as cursor on base [vb] *)
  let add_cursor_to_base v vb =
    try
      let vb2 = VarinfoHashtbl.find cursor_to_base v in
      if not (VarinfoComparable.equal vb vb2) then add_ignore_vars v
    with Not_found ->
      VarinfoHashtbl.add cursor_to_base v vb; signal_change ()
  in
  (* Variable [v] assigned *)
  let add_assigned_vars v =
    if not (VarinfoSet.mem v !assigned_vars) then
      begin
	assigned_vars := VarinfoSet.add v !assigned_vars; signal_change ()
      end
  in

  (* Interpret difference of pointers as a hint that one is an cursor
   * of the other. *)
  let preaction_expr x =
    begin match x with
      | BinOp(MinusPP,e1,e2,_) when isPointerType (typeOf e1) ->
	  begin match destruct_pointer e1,destruct_pointer e2 with
	    | Some(v1,_),Some(v2,_) ->
		begin try
		  let vb1 = VarinfoHashtbl.find cursor_to_base v1 in
		  let vb2 = VarinfoHashtbl.find cursor_to_base v2 in
		  if not (VarinfoComparable.equal vb1 vb2)
		    && vb1.vformal && vb2.vformal then
		      (* One formal is an offset from the other.
			 Choose the first one in the list of parameters
			 as base. *)
		      let vbbase,vboff =
			match
			  List.fold_left
			    (fun acc v ->
			       match acc with Some _ -> acc | None ->
		      		 if VarinfoComparable.equal v vb1 then
				   Some(vb1,vb2)
				 else if VarinfoComparable.equal v vb2 then
				   Some(vb2,vb1)
				 else None
			    ) None !curFundec.sformals
			with None -> assert false | Some pair -> pair
		      in
		      VarinfoHashtbl.add formal_to_base vboff vbbase
		  else ()
		with Not_found -> () end
	    | _ -> ()
	  end
      | _ -> ()
    end; x
  in
object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vfunc f =
    curFundec := f;
    (* For simplicity, consider formals as self-cursors initially.
     * This is the way we declare bases (in the image of [cursor_to_base]).
     *)
    let formal v =
      if candidate_var v then add_cursor_to_base v v
    in
    let local v =
      (* Consider local arrays as candidate base pointers *)
      if isArrayType v.vtype then formal v
    in
    List.iter formal f.sformals;
    List.iter local f.slocals;
    DoChildren

  method vinst = function
    | Set((Var v,NoOffset),e,_loc) ->
	if candidate_var v then
	  begin
	    add_assigned_vars v;
	    match destruct_pointer e with
	      | None -> add_ignore_vars v
	      | Some(v2,_offset) ->
		  if VarinfoSet.mem v2 !ignore_vars then add_ignore_vars v
		  else try
		    let vb2 = VarinfoHashtbl.find cursor_to_base v2 in
		    try
		      let vb = VarinfoHashtbl.find cursor_to_base v in
		      if not (VarinfoComparable.equal vb vb2) then
			add_ignore_vars v
		    with Not_found -> add_cursor_to_base v vb2
		  with Not_found -> add_ignore_vars v
	  end;
	DoChildren
    | Set _ -> DoChildren
    | Call(Some(Var v,NoOffset),_f,_args,_loc) ->
	if candidate_var v then
	  begin
	    add_assigned_vars v; add_ignore_vars v
	  end;
	DoChildren
    | Call _ -> DoChildren
    | Asm _ | Skip _ | Code_annot _ -> SkipChildren

  method vexpr e =
    ignore(preaction_expr e); DoChildren

  method vterm = do_on_term (Some preaction_expr, None)

  method vtsets_elem = do_on_tsets_elem (Some preaction_expr, None)

end

class rewriteCursorPointers
  (cursor_to_base : varinfo VarinfoHashtbl.t)
  (formal_to_base : varinfo VarinfoHashtbl.t)
  (assigned_vars : VarinfoSet.t) =

  (* Correspondance between cursor variables and offset variables *)
  let cursor_to_offset : varinfo VarinfoHashtbl.t = VarinfoHashtbl.create 0 in

  (* Function [expr_offset] may raise exception [Not_found] if
   * no offset needed.
   *)
  let expr_offset v =
    if v.vformal then
      let voff = VarinfoHashtbl.find cursor_to_offset v in
      Lval(Var voff,NoOffset)
    else
      let voff = VarinfoHashtbl.find cursor_to_offset v in
      let vb = VarinfoHashtbl.find cursor_to_base v in
      if VarinfoHashtbl.mem formal_to_base vb then
	let voff2 = VarinfoHashtbl.find cursor_to_offset vb in
	BinOp(PlusA,Lval(Var voff,NoOffset),Lval(Var voff2,NoOffset),
	      theMachine.ptrdiffType)
      else Lval(Var voff,NoOffset)
  in
  (* Find basis for variable [v] *)
  let var_base v =
    if VarinfoHashtbl.mem cursor_to_offset v then
      if v.vformal then
	try VarinfoHashtbl.find formal_to_base v
	with Not_found -> v (* self-base *)
      else
	let vb = VarinfoHashtbl.find cursor_to_base v in
	try VarinfoHashtbl.find formal_to_base vb
	with Not_found -> vb
    else
      raise Not_found
  in
  let lval_base vb =
    if isArrayType vb.vtype then
      StartOf(Var vb,NoOffset)
    else
      Lval(Var vb,NoOffset)
  in
  let preaction_expr e = match e with
    | BinOp(MinusPP,e1,e2,_) ->
        begin try match destruct_pointer e1,destruct_pointer e2 with
          | None,_ | _,None -> e
          | Some(v1,offopt1),Some(v2,offopt2) ->
	      let vb1 = try var_base v1 with Not_found -> v1 in
	      let vb2 = try var_base v2 with Not_found -> v2 in
              if VarinfoComparable.equal vb1 vb2 then
	        let v1offopt =
		  try Some(expr_offset v1) with Not_found -> None in
	        let v2offopt =
		  try Some(expr_offset v2) with Not_found -> None in
                let offopt1 = match v1offopt,offopt1 with
                  | None,None -> None
                  | Some off,None | None,Some off -> Some off
                  | Some off1,Some off2 ->
                      Some(BinOp(PlusA,off1,off2,theMachine.ptrdiffType))
                in
                let offopt2 = match v2offopt,offopt2 with
                  | None,None -> None
                  | Some off,None | None,Some off -> Some off
                  | Some off1,Some off2 ->
                      Some(BinOp(PlusA,off1,off2,theMachine.ptrdiffType))
                in
                match offopt1,offopt2 with
                  | Some off1,Some off2 ->
		      BinOp(MinusA,off1,off2,theMachine.ptrdiffType)
                  | Some off1,None ->
		      off1
                  | None,Some off2 ->
	              UnOp(Neg,off2,theMachine.ptrdiffType)
                  | None,None ->
		      constant_expr 0L
              else e
	with Not_found -> e end
    | _ -> e
  in
  let postaction_expr e = match e with
    | Lval(Var v,NoOffset) ->
	begin try
	  (* Both [var_base] and [expr_offset] can raise [Not_found],
	   * the second one only on local array variables.
	   *)
	  let vb = var_base v in
	  BinOp(PlusPI,lval_base vb,expr_offset v,v.vtype)
	with Not_found -> e end
    | _ -> e
  in
object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vfunc f =
    let local v =
      if VarinfoHashtbl.mem cursor_to_base v && not (isArrayType v.vtype) then
	let name = unique_name ("__jc_off_" ^ v.vname) in
	let voff = makeLocalVar f ~insert:true name almost_integer_type in
	VarinfoHashtbl.add cursor_to_offset v voff
    in
    let formal v =
      if VarinfoHashtbl.mem formal_to_base v then
	(* Formal is a cursor of another formal *)
	begin
	  local v; (* Create an offset variable for this formal *)
	  let voff = VarinfoHashtbl.find cursor_to_offset v in
	  let vb = VarinfoHashtbl.find formal_to_base v in
	  let initst =
	    mkStmt(
	      Instr(Set((Var voff,NoOffset),
	      BinOp(MinusPP,Lval(Var v,NoOffset),lval_base vb,
		    theMachine.ptrdiffType),
			CurrentLoc.get())))
	  in
	  add_pending_statement ~beginning:true initst
	end
      else if VarinfoHashtbl.mem cursor_to_base v
	&& VarinfoSet.mem v assigned_vars then
	(* Formal is assigned and still a self-base, an offset is needed *)
	begin
	  local v; (* Create an offset variable for this formal *)
	  let voff = VarinfoHashtbl.find cursor_to_offset v in
	  let initst =
	    mkStmt(Instr(Set((Var voff,NoOffset),
			     constant_expr 0L,
			     CurrentLoc.get ())))
	  in
	  add_pending_statement ~beginning:true initst
	end
      else ()
    in
    List.iter formal f.sformals;
    List.iter local f.slocals;
    DoChildren

  method vinst = function
    | Set((Var v,NoOffset),e,loc) ->
	if v.vformal then
	  begin try
	    let voff = VarinfoHashtbl.find cursor_to_offset v in
	    (* At this point, [e] must be a pointer whose destruction through
	     * [destruct_pointer] does not return None.
	     *)
	    let eoff = match destruct_pointer e with
	      | None -> assert false
	      | Some(v2,Some e) ->
		  begin try BinOp(PlusA,expr_offset v2,e,almost_integer_type)
		  with Not_found -> assert false end
	      | Some(v2,None) ->
		  begin try expr_offset v2
		  with Not_found -> assert false end
	    in
	    ChangeDoChildrenPost
	      ([Set((Var voff,NoOffset),eoff,loc)], fun x -> x)
	  with Not_found -> DoChildren end
	else
	  (* local variable *)
	  begin try
	    let voff = VarinfoHashtbl.find cursor_to_offset v in
	    (* At this point, [e] must be a pointer whose destruction through
	     * [destruct_pointer] does not return None.
	     *)
	    let eoff = match destruct_pointer e with
	      | None -> assert false
	      | Some(v2,Some e) ->
		  begin try BinOp(PlusA,expr_offset v2,e,almost_integer_type)
		  with Not_found -> e end
	      | Some(v2,None) ->
		  begin try expr_offset v2
		  with Not_found -> constant_expr 0L end
	    in
	    ChangeDoChildrenPost
	      ([Set((Var voff,NoOffset),eoff,loc)], fun x -> x)
	  with Not_found -> DoChildren end
    | _ -> DoChildren

  method vexpr e =
    ChangeDoChildrenPost (preaction_expr e, postaction_expr)

  method vterm =
    do_on_term (Some preaction_expr,Some postaction_expr)

  method vtsets_elem =
    do_on_tsets_elem (Some preaction_expr,Some postaction_expr)

  method vspec _sp =
    (* Do not modify the function contract, where offset variables
     * are not known *)
    SkipChildren

end

let rewrite_cursor_pointers file =
  (* Variables to communicate between the collecting visitor and
   * the rewriting one. *)
  let cursor_to_base = VarinfoHashtbl.create 0 in
  let formal_to_base = VarinfoHashtbl.create 0 in
  let assigned_vars = ref VarinfoSet.empty in
  let ignore_vars = ref VarinfoSet.empty in

  (* Collect the cursor variables and their base *)
  let visitor =
    new collectCursorPointers
      cursor_to_base formal_to_base assigned_vars ignore_vars
  in
  visit_and_store_result_type visit_until_convergence visitor file;

  (* Normalize the information *)
  let rec transitive_basis v =
    try transitive_basis (VarinfoHashtbl.find formal_to_base v)
    with Not_found -> v
  in
  VarinfoHashtbl.iter
    (fun v _ -> VarinfoHashtbl.add formal_to_base v (transitive_basis v))
    formal_to_base;
  VarinfoSet.iter
    (fun v -> VarinfoHashtbl.remove cursor_to_base v) !ignore_vars;
  VarinfoHashtbl.iter
    (fun v vb -> if VarinfoSet.mem vb !ignore_vars then
      VarinfoHashtbl.remove cursor_to_base v) cursor_to_base;
  VarinfoHashtbl.iter
    (fun v vb -> if VarinfoSet.mem vb !ignore_vars then
      VarinfoHashtbl.remove formal_to_base v) formal_to_base;

  (* Rewrite cursor variables as offsets from their base variable *)
  let visitor =
    new rewriteCursorPointers
      cursor_to_base formal_to_base !assigned_vars
  in
  visit_and_store_result_type visitFramacFile
    (visit_and_push_statements_visitor visitor) file


(*****************************************************************************)
(* Rewrite cursor integers into offsets from base integers.                  *)
(*****************************************************************************)

(* Recognize the sum of an integer variable and an integer offset *)
let rec destruct_integer = function
  | Lval(Var v,NoOffset) -> Some(v,None)
  | BinOp((PlusA | MinusA as op),e1,e2,_) ->
      begin match destruct_integer e1 with
	| None -> None
	| Some(v,None) ->
	    begin match op with
	      | PlusA -> Some(v,Some e2)
	      | MinusA -> Some(v,Some(UnOp(Neg,e2,almost_integer_type)))
	      | _ -> assert false
	    end
	| Some(v,Some off) ->
	    begin match op with
	      | PlusA -> Some(v,Some(BinOp(PlusA,off,e2,almost_integer_type)))
	      | MinusA -> Some(v,Some(BinOp(MinusA,off,e2,almost_integer_type)))
	      | _ -> assert false
	    end
      end
  | CastE(ty,e) ->
      let ety = typeOf e in
      if isIntegralType ty && isIntegralType ety then
	destruct_integer e
      else None
  | _ -> None

class collectCursorIntegers
  (cursor_to_base : varinfo VarinfoHashtbl.t) (* local variable to base *)
  (assigned_vars : VarinfoSet.t ref) (* variable is assigned (for formals) *)
  (ignore_vars : VarinfoSet.t ref) (* ignore info on these variables *) =

  let candidate_var v =
    not v.vglob && (isIntegralType v.vtype && not v.vaddrof)
  in
  (* Variable should not be translated as base or cursor *)
  let add_ignore_vars v =
    if not (VarinfoSet.mem v !ignore_vars) then
      begin
	ignore_vars := VarinfoSet.add v !ignore_vars; signal_change ()
      end
  in
  (* Variable [v] used as cursor on base [vb] *)
  let add_cursor_to_base v vb =
    try
      let vb2 = VarinfoHashtbl.find cursor_to_base v in
      if not (VarinfoComparable.equal vb vb2) then add_ignore_vars v
    with Not_found ->
      VarinfoHashtbl.add cursor_to_base v vb; signal_change ()
  in
  (* Variable [v] assigned *)
  let add_assigned_vars v =
    if not (VarinfoSet.mem v !assigned_vars) then
      begin
	assigned_vars := VarinfoSet.add v !assigned_vars; signal_change ()
      end
  in
object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vfunc f =
    (* For simplicity, consider formals as self-cursors initially.
     * This is the way we declare bases (in the image of [cursor_to_base]).
     *)
    let formal v =
      if candidate_var v then add_cursor_to_base v v
    in
    List.iter formal f.sformals;
    DoChildren

  method vinst = function
    | Set((Var v,NoOffset),e,_loc) ->
	if candidate_var v then
	  begin
	    add_assigned_vars v;
	    match destruct_integer e with
	      | None -> add_ignore_vars v
	      | Some(v2,_offset) ->
		  if VarinfoSet.mem v2 !ignore_vars then add_ignore_vars v
		  else try
		    let vb2 = VarinfoHashtbl.find cursor_to_base v2 in
		    try
		      let vb = VarinfoHashtbl.find cursor_to_base v in
		      if not (VarinfoComparable.equal vb vb2) then
			add_ignore_vars v
		    with Not_found -> add_cursor_to_base v vb2
		  with Not_found -> add_ignore_vars v
	  end;
	SkipChildren
    | Set _ -> SkipChildren
    | Call(Some(Var v,NoOffset),_f,_args,_loc) ->
	if candidate_var v then
	  begin
	    add_assigned_vars v; add_ignore_vars v
	  end;
	SkipChildren
    | Call _ -> SkipChildren
    | Asm _ | Skip _ | Code_annot _ -> SkipChildren

end

class rewriteCursorIntegers
  (cursor_to_base : varinfo VarinfoHashtbl.t)
  (assigned_vars : VarinfoSet.t) =

  (* Correspondance between cursor variables and offset variables *)
  let cursor_to_offset : varinfo VarinfoHashtbl.t = VarinfoHashtbl.create 0 in

  let postaction_expr e = match e with
    | Lval(Var v,NoOffset) ->
	begin try
	  let vb = VarinfoHashtbl.find cursor_to_base v in
	  let voff = VarinfoHashtbl.find cursor_to_offset v in
	  BinOp(PlusA,Lval(Var vb,NoOffset),Lval(Var voff,NoOffset),v.vtype)
	with Not_found -> e end
    | _ -> e
  in
  let postaction_term t = match t.term_node with
    | TLval(TVar { lv_origin = Some v },TNoOffset) ->
	begin try
	  let vb = VarinfoHashtbl.find cursor_to_base v in
	  let voff = VarinfoHashtbl.find cursor_to_offset v in
	  let vt1 = term_of_var vb in
	  let vt2 = term_of_var voff in
	  let addt =
	    mkterm (TBinOp(PlusA,vt1,vt2)) Linteger t.term_loc
	  in
	  mkterm (TCastE(v.vtype,addt)) t.term_type t.term_loc
	with Not_found -> t end
    | _ -> t
  in
object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vfunc f =
    let local v =
      if VarinfoHashtbl.mem cursor_to_base v then
	let name = unique_name ("__jc_off_" ^ v.vname) in
	let voff = makeLocalVar f ~insert:true name almost_integer_type in
	VarinfoHashtbl.add cursor_to_offset v voff
    in
    let formal v =
      if VarinfoHashtbl.mem cursor_to_base v
	&& VarinfoSet.mem v assigned_vars then
	  (* Formal is assigned and still a self-base, an offset is needed *)
	  begin
	  local v; (* Create an offset variable for this formal *)
	  let voff = VarinfoHashtbl.find cursor_to_offset v in
	  let initst =
	    mkStmt(Instr(Set((Var voff,NoOffset),
			     constant_expr 0L,
			     CurrentLoc.get ())))
	  in
	  add_pending_statement ~beginning:true initst
	  end
      else ()
    in
    List.iter formal f.sformals;
    List.iter local f.slocals;
    DoChildren

  method vinst = function
    | Set((Var v,NoOffset),e,loc) ->
	begin try
	  let voff = VarinfoHashtbl.find cursor_to_offset v in
	  (* At this point, [e] must be an integer whose destruction through
	   * [destruct_integer] does not return None.
	   *)
	  let eoff = match destruct_integer e with
	    | None -> assert false
	    | Some(v2,Some e) ->
		begin try
		  let voff2 = VarinfoHashtbl.find cursor_to_offset v2 in
		  BinOp(PlusA,Lval(Var voff2,NoOffset),e,almost_integer_type)
		with Not_found -> e end
	    | Some(v2,None) ->
		begin try
		  let voff2 = VarinfoHashtbl.find cursor_to_offset v2 in
		  Lval(Var voff2,NoOffset)
		with Not_found -> constant_expr 0L end
	  in
	  ChangeDoChildrenPost
	    ([Set((Var voff,NoOffset),eoff,loc)], fun x -> x)
	with Not_found -> DoChildren end
    | _ -> DoChildren

  method vexpr e =
    ChangeDoChildrenPost (e,postaction_expr)

  method vterm t =
    ChangeDoChildrenPost (t,postaction_term)

  method vtsets_elem =
    do_on_tsets_elem_through_term (None,Some postaction_term)

  method vspec _sp =
    (* Do not modify the function contract, where offset variables
     * are not known *)
    SkipChildren

end

let rewrite_cursor_integers file =
  (* Variables to communicate between the collecting visitor and
   * the rewriting one. *)
  let cursor_to_base = VarinfoHashtbl.create 0 in
  let assigned_vars = ref VarinfoSet.empty in
  let ignore_vars = ref VarinfoSet.empty in

  (* Collect the cursor variables and their base *)
  let visitor =
    new collectCursorIntegers
      cursor_to_base assigned_vars ignore_vars
  in
  visit_until_convergence visitor file;

  (* Normalize the information *)
  VarinfoSet.iter
    (fun v -> VarinfoHashtbl.remove cursor_to_base v) !ignore_vars;
  VarinfoHashtbl.iter
    (fun v vb -> if VarinfoSet.mem vb !ignore_vars then
      VarinfoHashtbl.remove cursor_to_base v) cursor_to_base;

  (* Rewrite cursor variables as offsets from their base variable *)
  let visitor =
    new rewriteCursorIntegers cursor_to_base !assigned_vars
  in
  visit_and_store_result_type visitFramacFile
    (visit_and_push_statements_visitor visitor) file


(*****************************************************************************)
(* Annotate code with strlen.                                                *)
(*****************************************************************************)

(* All annotations are added as hints, by no means they should be trusted
   blindly, but they can be used if they are also proved *)

class annotateCodeStrlen(strlen : logic_info) =

  (* Store correspondance from temporaries to the corresponding string access *)

  let temps = VarinfoHashtbl.create 17 in

  (* Recognize access or test of string *)

  (* TODO: extend applicability of [destruct_string_access]. *)
  let lval_destruct_string_access ~through_tmp = function
    | Mem e, NoOffset when isCharPtrType(typeOf e) ->
	begin match destruct_pointer e with
	  | None -> None
	  | Some(v,Some off) -> Some(v,off)
	  | Some(v,None) -> Some(v,constant_expr 0L)
	end
    | Var v, off ->
	if isCharPtrType v.vtype then
	  match off with
	    | Index(i,NoOffset) -> Some (v,i)
	    | NoOffset
	    | Index _
	    | Field _ -> None
	else if isCharArrayType v.vtype then
	  match off with
	    | Index(i,NoOffset) -> Some (v,i)
	    | NoOffset
	    | Index _
	    | Field _ -> None
	else if through_tmp then
	  try Some(VarinfoHashtbl.find temps v) with Not_found -> None
	else None
    | _ -> None
  in
  let rec destruct_string_access ?(through_tmp=false) ?(through_cast=false) =
    function
      | Lval lv -> lval_destruct_string_access ~through_tmp lv
      | CastE(_,e) ->
	  if through_cast then
	    destruct_string_access ~through_tmp ~through_cast e
	  else None
      | _ -> None
  in
  let destruct_string_test ?(neg=false) e =
    let rec aux ~neg = function
      | UnOp(LNot,e,_) -> aux ~neg:(not neg) e
      | BinOp(Ne,e1,e2,_) when is_null_expr e2 -> aux ~neg e1
      | BinOp(Ne,e2,e1,_) when is_null_expr e2 -> aux ~neg e1
      | BinOp(Eq,e1,e2,_) when is_null_expr e2 -> aux ~neg:(not neg) e1
      | BinOp(Eq,e2,e1,_) when is_null_expr e2 -> aux ~neg:(not neg) e1
      | e ->
	  match
	    destruct_string_access ~through_tmp:true ~through_cast:true e
	  with
	    | Some(v,off) -> Some(neg,v,off)
	    | None -> None
    in match e with
      | BinOp(Eq,e1,e2,_) when is_non_null_expr e2 -> false, aux ~neg e1
      | BinOp(Eq,e2,e1,_) when is_non_null_expr e2 -> false, aux ~neg e1
      | _ -> true, aux ~neg e
  in

  (* Generate appropriate assertion *)

  let strlen_type =
    match strlen.l_type with Some t -> t | None -> assert false
  in

  let within_bounds ~strict v off =
    let rel1 =
      Logic_const.new_predicate (Logic_const.prel (Rle,lzero(),off))
    in
    let tv = term_of_var v in
    let app2 = mkterm (Tapp(strlen,[],[tv])) strlen_type  v.vdecl in
    let op = if strict then Rlt else Rle in
    let rel2 =
      Logic_const.new_predicate (Logic_const.prel (op,off,app2))
    in
    let app =
      Logic_const.new_predicate
	(Logic_const.pand (Logic_const.pred_of_id_pred rel1,
			   Logic_const.pred_of_id_pred rel2))
    in
    Logic_const.pred_of_id_pred
      { app with ip_name = [ name_of_hint_assertion ] }
  in
  let reach_upper_bound ~loose v off =
    let tv = term_of_var v in
    let app = mkterm (Tapp(strlen,[],[tv])) strlen_type v.vdecl in
    let op = if loose then Rle else Req in
    let rel =
      Logic_const.new_predicate (Logic_const.prel (op,app,off))
    in
    Logic_const.pred_of_id_pred
      { rel with ip_name = [ name_of_hint_assertion ] }
  in
object(self)

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vexpr e =
    begin match destruct_string_access e with None -> () | Some(v,off) ->
      if hasAttribute name_of_string_declspec (typeAttrs v.vtype) then
	(* A string should be accessed within its bounds *)
	let off =
	  !Db.Properties.Interp.force_exp_to_term locUnknown off
	in
	let app = within_bounds ~strict:false v off in
	let cur_stmt = the self#current_stmt in
	Annotations.add_alarm cur_stmt ~before:true Alarms.Other_alarm app
    end;
    DoChildren

  method vstmt_aux s =
    let preaction s = match s.skind with
      | If(e,tbl,fbl,_loc) ->
	  begin match destruct_string_test e with _,None -> ()
	    | test_to_null,Some(neg,v,off) ->
		if hasAttribute name_of_string_declspec (typeAttrs v.vtype)
		then
		  (* A string should be tested within its bounds, and
		     depending on the result, the offset is either before
		     or equal to the length of the string *)
		  let off =
		    !Db.Properties.Interp.force_exp_to_term locUnknown off
		  in
		  let rel1 = within_bounds ~strict:true v off in
		  let supst = mkStmt(Instr(Skip(CurrentLoc.get()))) in
		  Annotations.add_alarm
		    supst ~before:true Alarms.Other_alarm rel1;

		  let rel2 = reach_upper_bound ~loose:false v off in
		  let eqst = mkStmt(Instr(Skip(CurrentLoc.get()))) in
		  Annotations.add_alarm
		    eqst ~before:true Alarms.Other_alarm rel2;

		  (* Rather add skip statement as blocks may be empty *)
		  if neg then
		    begin
		      fbl.bstmts <- supst :: fbl.bstmts;
		      if test_to_null then tbl.bstmts <- eqst :: tbl.bstmts
		    end
		  else
		    begin
		      tbl.bstmts <- supst :: tbl.bstmts;
		      if test_to_null then fbl.bstmts <- eqst :: fbl.bstmts
		    end
	  end; s
      | Instr(Set(lv,e,_loc)) when is_null_expr e ->
	  if Cmdline.Jessie.HintLevel.get () > 1 then
	    match lval_destruct_string_access ~through_tmp:true lv with
	      | None -> ()
	      | Some(v,off) ->
		  let off =
		    !Db.Properties.Interp.force_exp_to_term locUnknown off
		  in
		  (* Help ATP with proving the bound on [strlen(v)] by
		     asserting the obvious equality *)
		  let lv' = !Db.Properties.Interp.force_lval_to_term_lval
		    locUnknown lv
		  in
		  let e' = !Db.Properties.Interp.force_exp_to_term
		    locUnknown e
		  in
		  let lvt = mkterm (TLval lv') strlen_type locUnknown in
		  let rel =
		    Logic_const.new_predicate (Logic_const.prel (Req,lvt,e'))
		  in
		  let prel = Logic_const.pred_of_id_pred
		    { rel with ip_name = [ name_of_hint_assertion ] }
		  in
		  Annotations.add_alarm
		    s ~before:false Alarms.Other_alarm prel;
		  (* Further help ATP by asserting that index should be 
		     positive *)
(* 		  let rel = *)
(* 		    Logic_const.new_predicate  *)
(* 		      (Logic_const.prel (Rle,lzero(),off)) *)
(* 		  in *)
(* 		  let prel = Logic_const.pred_of_id_pred *)
(* 		    { rel with ip_name = [ name_of_hint_assertion ] } *)
(* 		  in *)
(* 		  Annotations.add_alarm *)
(* 		    s ~before:false Alarms.Other_alarm prel; *)
		  (* If setting a character to zero in a buffer, this should
		     be the new length of a string *)
		  let rel = reach_upper_bound ~loose:true v off in
		  Annotations.add_alarm
		    s ~before:false Alarms.Other_alarm rel
	  else ();
	  s
      | Instr(Set((Var v1,NoOffset),e,_loc)) ->
	  begin match
	    destruct_string_access ~through_tmp:true ~through_cast:true e
	  with
	    | None -> ()
	    | Some(v2,off) -> VarinfoHashtbl.add temps v1 (v2,off)
	  end; s
      | _ -> s
    in
    ChangeDoChildrenPost(preaction s,fun x -> x)

 end

let annotate_code_strlen file =
  try
    let strlen = Logic_env.find_logic_function name_of_strlen in
    let visitor = new annotateCodeStrlen strlen in
    visitFramacFile visitor file
  with Not_found -> assert false


(*****************************************************************************)
(* Annotate functions from declspec.                                         *)
(*****************************************************************************)

open Genlex

class annotateFunFromDeclspec =

  let recover_from_attr_param params attrparam =
    let rec aux = function
      | AInt i ->
	  constant_term locUnknown (Int64.of_int i)
      | AUnOp(Neg,AInt i) ->
	  constant_term locUnknown (Int64.of_int (-i))
      | AStr s
      | ACons(s,[]) ->
	  begin try
	    let v = List.find (fun v -> v.vname = s) params in
	    term_of_var v
	  with Not_found -> failwith "No recovery" end
      | ABinOp(bop,attr1,attr2) ->
	  mkterm (TBinOp(bop,aux attr1,aux attr2)) Linteger locUnknown
      | ACons _
      | ASizeOf _
      | ASizeOfE _
      | ASizeOfS _
      | AAlignOf _
      | AAlignOfE _
      | AAlignOfS _
      | AUnOp _
      | ADot _
      | AStar _
      | AAddrOf _
      | AIndex _
      | AQuestion _ -> failwith "No recovery" (* Not yet supported *)
    in
    aux attrparam
  in
  let recover_from_attribute params attr =
    match attr with
      | Attr(name,attrparams) ->
	  begin try
	    Some(name, List.map (recover_from_attr_param params) attrparams)
	  with Failure "No recovery" -> None end
      | AttrAnnot _ -> None
  in

  (* Add precondition based on declspec on parameters *)
  let annotate_var params acc v =
    List.fold_left
      (fun acc attr ->
	 match recover_from_attribute params attr with
	   | None -> acc
	   | Some(name,args) ->
	       if name = "valid" || name = "valid_range" then
		 let p = match name with
		   | "valid" ->
		       assert (args = []);
		       let ts =
			 TSSingleton
			   (TSLval(TSVar(cvar_to_lvar v),TSNoOffset))
		       in
		       Pvalid(ts)
		   | "valid_range" ->
		       let t1,t2 = match args with
			 | [ t1; t2 ] -> t1,t2
			 | _ -> assert false
		       in
		       Pvalid_range(term_of_var v,t1,t2)
		   | _ -> assert false
		 in
		 let app =
		   Logic_const.new_predicate (Logic_const.unamed p)
		 in
		 app :: acc
	       else
		 try
		   let p = Logic_env.find_logic_function name in
		   assert (List.length p.l_profile = List.length(args) + 1);
		   assert (List.length p.l_labels <= 1);
		   let args = term_of_var v :: args in
		   let app =
		     Logic_const.new_predicate
		       (Logic_const.unamed (Papp(p,[],args)))
		   in
		   app :: acc
		 with Not_found -> acc
      ) acc (typeAttrs v.vtype)
  in

  let annotate_fun v =
    let funspec = Kernel_function.get_spec (Globals.Functions.get v) in
    let params = Globals.Functions.get_params (Globals.Functions.get v) in
    let return_ty = getReturnType v.vtype in

    let req =
      List.fold_left (annotate_var params) funspec.spec_requires params
    in
    funspec.spec_requires <- req;

    let default_behavior =
      List.find
        (fun x -> x.b_name = name_of_default_behavior) funspec.spec_behavior
    in
    let ens =
      List.fold_left
	(fun acc attr ->
	   match recover_from_attribute params attr with
	     | None -> acc
	     | Some(name,args) ->
		 if name = "valid" || name = "valid_range" then
		   let p = match name with
		     | "valid" ->
			 assert (args = []);
			 let ts =
			   TSSingleton(TSLval(TSResult,TSNoOffset))
			 in
			 Pvalid(ts)
		     | "valid_range" ->
			 let t1,t2 = match args with
			   | [ t1; t2 ] -> t1,t2
			   | _ -> assert false
			 in
			 let res =
			   mkterm
			     (TLval(TResult,TNoOffset))
			     (Ctype return_ty) v.vdecl
			 in
			 Pvalid_range(res,t1,t2)
		     | _ -> assert false
		   in
		   let app =
		     Logic_const.new_predicate (Logic_const.unamed p)
		   in
		   app :: acc
		 else
		   try
		     let p = Logic_env.find_logic_function name in
		     assert (List.length p.l_profile = List.length(args) + 1);
		     assert (List.length p.l_labels <= 1);
		     let res =
		       mkterm
			 (TLval(TResult,TNoOffset)) (Ctype return_ty) v.vdecl
		     in
		     let args = res :: args in
		     let app =
		       Logic_const.new_predicate
			 (Logic_const.unamed (Papp(p,[],args)))
		     in
		     app :: acc
		   with Not_found -> acc
	) default_behavior.b_ensures (typeAttrs return_ty)
    in
    default_behavior.b_ensures <- ens;
  in
object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vglob_aux = function
    | GFun(f,_) ->
	annotate_fun f.svar;
	SkipChildren
    | GVarDecl(_,v,_)
    | GVar(v,_,_) as g ->
	if isFunctionType v.vtype && not v.vdefined then
	  (annotate_fun v; SkipChildren)
	else
    	  let inv = annotate_var [] [] v in
	  let postaction gl =
	    match inv with [] -> gl | _ ->
	      (* Define a global string invariant *)
	      let inv =
		List.map (fun p -> Logic_const.unamed p.ip_content) inv
	      in
	      let p = Logic_const.new_predicate (Logic_const.pands inv) in
	      let globinv =
		{ l_name = unique_logic_name ("valid_" ^ v.vname);
		  l_tparams = [];
		  l_type = None;
		  l_profile = [];
		  l_labels = [ LogicLabel "Here" ];
		  l_body = LBpred (predicate v.vdecl p.ip_content)}
	      in
	      attach_globaction (fun () -> Logic_env.add_logic_function globinv);
	      gl @ [GAnnot(Dinvariant globinv,v.vdecl)]
	  in
	  ChangeDoChildrenPost ([g], postaction)
    | GAnnot _ -> DoChildren
    | GCompTag _ | GType _ | GCompTagDecl _ | GEnumTagDecl _
    | GEnumTag _ | GAsm _ | GPragma _ | GText _ ->
	SkipChildren
end

let annotate_fun_from_declspec file =
  let visitor = new annotateFunFromDeclspec in
  visit_and_update_globals visitor file

(*****************************************************************************)
(* Annotate code with overflow checks.                                       *)
(*****************************************************************************)

class annotateOverflow =
object(self)

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vexpr = function
    | BinOp((Shiftlt | Shiftrt as op),e1,e2,_ty) ->
	let cur_stmt = the self#current_stmt in
	let is_left_shift = match op with Shiftlt -> true | _ -> false in
	let ty1 = typeOf e1 in
	(* Ideally, should strip only casts introduced by the compiler, not
	 * user casts. Since this information is not available, be
	 * conservative here.
	 *)
	let e1' = stripCastsButLastInfo e1 in
	let e2' = stripCastsButLastInfo e2 in
	(* Check that signed shift has a positive right operand *)
	if isSignedInteger ty1 then
	  begin match possible_value_of_integral_expr e2' with
	    | Some i when i >= 0L -> ()
	    | _ ->
		let check = BinOp(Ge,e2',constant_expr 0L,intType) in
		let check =
		  !Db.Properties.Interp.force_exp_to_predicate locUnknown check
		in
		Annotations.add_alarm
		  cur_stmt ~before:true Alarms.Shift_alarm check
	  end
	else ();
	(* Check that shift has not too big a right operand. *)
	let max_right = Int64.of_int (integral_type_size_in_bits ty1) in
	begin match possible_value_of_integral_expr e2' with
	  | Some i when i < max_right -> ()
	  | _ ->
	      let max_right = constant_expr max_right in
	      let check = BinOp(Lt,e2',max_right,intType) in
	      let check =
		!Db.Properties.Interp.force_exp_to_predicate locUnknown check
	      in
	      Annotations.add_alarm
		cur_stmt ~before:true Alarms.Shift_alarm check
	end;
	(* Check that signed left shift has a positive left operand *)
	if is_left_shift && isSignedInteger ty1 then
	  begin match possible_value_of_integral_expr e1' with
	    | Some i when i >= 0L -> ()
	    | _ ->
		let check = BinOp(Ge,e1',constant_expr 0L,intType) in
		let check =
		  !Db.Properties.Interp.force_exp_to_predicate locUnknown check
		in
		Annotations.add_alarm
		  cur_stmt ~before:true Alarms.Shift_alarm check
	  end
	else ();
	(* Check that signed left shift has not a left operand that is bigger
	 * than the maximal value for the type right shifted by its right
	 * operand.
	 *)
	if is_left_shift && isSignedInteger ty1 then
	  let max_int = Int64.of_string
	    (Big_int.string_of_big_int (max_value_of_integral_type ty1))
	  in
	  begin match possible_value_of_integral_expr e2' with
	    | Some i when i >= 0L && i < 64L ->
		(* Only use optimization where [Int64.shift_right] is
		 * defined in OCaml
		 *)
		let i = Int64.to_int i in
		let max_left = constant_expr (Int64.shift_right max_int i) in
		let check = BinOp(Le,e1',max_left,intType) in
		let check =
		  !Db.Properties.Interp.force_exp_to_predicate locUnknown check
		in
		Annotations.add_alarm
		  cur_stmt ~before:true Alarms.Shift_alarm check
	    | _ ->
		let max_int = constant_expr max_int in
		let max_left = BinOp(Shiftrt,max_int,e2',intType) in
		let check = BinOp(Le,e1',max_left,intType) in
		let check =
		  !Db.Properties.Interp.force_exp_to_predicate locUnknown check
		in
		Annotations.add_alarm
		  cur_stmt ~before:true Alarms.Shift_alarm check
	  end
	else ();
	DoChildren
    | _ -> DoChildren

end

let annotate_overflow file =
  let visitor = new annotateOverflow in
  visitFramacFile visitor file


(*****************************************************************************)
(* Rewrite type void* into char*.                                            *)
(*****************************************************************************)

class rewriteVoidPointer =
object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vtype ty =
    if isVoidPtrType ty then
      let attr = typeAttr ty in
      ChangeTo (typeAddAttributes attr charPtrType)
    else if isCharType ty then
      (* All (un)signed chars changed into char for now ... *)
      let attr = typeAttr ty in
      ChangeTo (typeAddAttributes attr charType)
    else DoChildren

end

class debugVoid =
object
  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super
  method vtsets_elem ts = match ts with
    | TSLval(TSResult,_) -> DoChildren
    | _ ->
	assert (not (app_term_type isVoidPtrType false (typeOfTsetsElem ts)));
	DoChildren
end

let rewrite_void_pointer file =
  let visitor = new rewriteVoidPointer in
  visitFramacFile visitor file


(*****************************************************************************)
(* Rewrite the C file for Jessie translation.                                *)
(*****************************************************************************)

(* class collectTiti (table : varinfo VarinfoHashtbl.t) = *)
(* object *)

(*   inherit Visitor.generic_frama_c_visitor *)
(*     (Cil.inplace_visit ()) (Project.current ()) as super *)

(*   method vfunc f = *)
(*     let var v = VarinfoHashtbl.add table v v in *)
(*     List.iter var f.sformals; *)
(*     List.iter var f.slocals; *)
(*     DoChildren *)

(* end *)

(* class rewriteTiti (table : varinfo VarinfoHashtbl.t) = *)
(* object *)

(*   inherit Visitor.generic_frama_c_visitor *)
(*     (Cil.inplace_visit ()) (Project.current ()) as super *)

(*   method vlval (host,_off) = match host with *)
(*     | Var v -> *)
(* 	begin try ignore (VarinfoHashtbl.find table v) *)
(* 	with Not_found -> () end; DoChildren *)
(*     | _ -> DoChildren *)

(* end *)

(* let rewrite_titi file = *)
(*   let table = VarinfoHashtbl.create 17 in *)
(*   let visitor = new collectTiti table in *)
(*   visitFramacFile (visitor :> cilVisitor) file; *)
(*   let visitor = new rewriteTiti table in *)
(*   visit_and_push_statements visitFramacFile visitor file *)

let rewrite file =
  if checking then check_types file;
  (* adds a behavior named [name_of_default_behavior] to all functions if
     it does not already exist.
   *)
  if Cmdline.Debug.get () >= 1 then
    Format.printf "Adding default behavior to all functions@.";
  add_default_behavior file;
  if checking then check_types file;
  (* Annotate functions from declspec. *)
  (* Before [rename_entities] so that original parameter names are available *)
  if Cmdline.Debug.get () >= 1 then
    Format.printf "Annotate functions from declspec@.";
  annotate_fun_from_declspec file;
  if checking then check_types file;
  (* Rename entities to avoid conflicts with Jessie predefined names.
     Should be performed before any call to [Cil.cvar_to_lvar] destroys
     sharing among logic variables.
  *)
  if Cmdline.Debug.get () >= 1 then
    Format.printf "Rename entities@.";
  rename_entities file;
  if checking then check_types file;
  (* Fill offset/size information in fields *)
  if Cmdline.Debug.get () >= 1 then
    Format.printf "Fill offset/size information in fields@.";
  fill_offset_size_in_fields file;
  if checking then check_types file;
  (* Replace addrof array with startof. *)
  if Cmdline.Debug.get () >= 1 then
    Format.printf "Replace addrof array with startof@.";
  replace_addrof_array file;
  if checking then check_types file;
  (* Replace string constants by global variables. *)
  if Cmdline.Debug.get () >= 1 then
    Format.printf "Replace string constants by global variables@.";
  replace_string_constants file;
  if checking then check_types file;
  (* Put all global initializations in the [globinit] file. *)
  (* Replace global compound initializations by equivalent statements. *)
  if Cmdline.Debug.get () >= 1 then
    Format.printf "Put all global initializations in the [globinit] file@.";
  gather_initialization file;
  if checking then check_types file;
  (* Rewrite comparison of pointers into difference of pointers. *)
  if Cmdline.Jessie.InferAnnot.get () <> "" then
    begin
      if Cmdline.Debug.get () >= 1 then
	Format.printf
	  "Rewrite comparison of pointers into difference of pointers@.";
      rewrite_pointer_compare file;
      if checking then check_types file
    end;
  (* Rewrite type void* and (un)signed char* into char*. *)
  if Cmdline.Debug.get () >= 1 then
    Format.printf "Rewrite type void* and (un)signed char* into char*@.";
  rewrite_void_pointer file;
  if checking then check_types file;
  (* Rewrite cursor pointers into offsets from base pointers. *)
  (* order: after [rewrite_pointer_compare] *)
  if Cmdline.Jessie.InferAnnot.get () <> "" then
    begin
      if Cmdline.Debug.get () >= 1 then
	Format.printf
	  "Rewrite cursor pointers into offsets from base pointers@.";
      rewrite_cursor_pointers file;
      if checking then check_types file
    end;
  (* Rewrite cursor integers into offsets from base integers. *)
  if Cmdline.Jessie.InferAnnot.get () <> "" then
    begin
      if Cmdline.Debug.get () >= 1 then
	Format.printf 
	  "Rewrite cursor integers into offsets from base integers@.";
      rewrite_cursor_integers file;
      if checking then check_types file
    end;
  (* Annotate code with strlen. *)
  if Cmdline.Jessie.HintLevel.get () > 0 then
    begin 
      if Cmdline.Debug.get () >= 1 then
	Format.printf "Annotate code with strlen@.";
      annotate_code_strlen file;
      if checking then check_types file
    end;
  (* Annotate code with overflow checks. *)
  if Cmdline.Debug.get () >= 1 then
    Format.printf "Annotate code with overflow checks@.";
  annotate_overflow file;
  if checking then check_types file;


(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j bin/toplevel.byte"
End:
*)

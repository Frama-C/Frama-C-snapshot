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

(* $Id: rewrite.ml,v 1.49 2008/05/23 16:55:40 uid570 Exp $ *)

(* Import from Cil *)
open Cil_types
open Cil
open Cilutil
open Ast_info
open Extlib

(* Utility functions *)
open Common


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
  visit_and_update_globals (visitor :> cilVisitor) file


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
    makeGlobalVar name (array_type !wcharType)
  in

  let make_glob v inite =
    (* Apply translation from initializer in primitive AST to block of code,
     * simple initializer and type.
     *)
    let b,init,ty = Cabs2cil.blockInitializer v inite in
    (* Precise the array type *)
    v.vtype <- ty;
    (* Attach global variable and code for global initialization *)
    List.iter attach_globinit b.bstmts;
    attach_global (GVar(v,{init=Some init},!currentLoc));
    v
  in
object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vexpr = function
    | Const(CStr s) ->
	let v =
	  findOrAdd string_to_var s
	    (fun s -> make_glob (string_var s) (string_cabs_init s))
	in
	ChangeTo (StartOf(Var v,NoOffset))
    | Const(CWStr ws) ->
	let v =
	  findOrAdd wstring_to_var ws
	    (fun ws -> make_glob (wstring_var ()) (wstring_cabs_init ws))
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
  visit_and_update_globals (visitor :> cilVisitor) file


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
(* 	List.iter attach_globinit s; *)
	iinfo.init <- None
      )) file


(*****************************************************************************)
(* Rewrite comparison of pointers into difference of pointers.               *)
(*****************************************************************************)

class rewritePointerCompare =
  let preaction_expr = function
    | BinOp((Lt | Gt | Le | Ge | Eq | Ne as op),e1,e2,ty)
	when isPointerType (typeOf e1) && not (is_null_expr e2) ->
	BinOp(op,BinOp(MinusPP,e1,e2,!ptrdiffType),constant_expr 0L,ty)
    | e -> e
  in
object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vexpr e =
    ChangeDoChildrenPost (preaction_expr e, fun x -> x)

  method vterm =
    do_on_term (Some preaction_expr,None)

  method vpredicate = function
    | Prel(rel,t1,t2)
	when app_term_type isPointerType false t1.term_type
	  && not (is_null_term t2) ->
	let loc = range_loc t1.term_loc t2.term_loc in
	let tsub = {
	  term_node = TBinOp(MinusPP,t1,t2);
	  term_type = Ctype !ptrdiffType;
	  term_loc = loc;
	  term_name = [];
	} in
	let p = Prel(rel,tsub,constant_term loc 0L) in
	ChangeDoChildrenPost (p, fun x -> x)
    | _ -> DoChildren

end

let rewrite_pointer_compare file =
  let visitor = new rewritePointerCompare in
  visitCilFile (visitor :> cilVisitor) file


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

  let curFundec : fundec ref = ref dummyFunDec in

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
  let preaction_expr x = match x with
    | BinOp(MinusPP,e1,e2,_) when isPointerType (typeOf e1) ->
	begin match destruct_pointer e1,destruct_pointer e2 with
	  | Some(v1,_),Some(v2,_) ->
	      begin try
		let vb1 = VarinfoHashtbl.find cursor_to_base v1 in
		let vb2 = VarinfoHashtbl.find cursor_to_base v2 in
		if not (VarinfoComparable.equal vb1 vb2)
		  && vb1.vformal && vb2.vformal then
		  (* One formal is an offset from the other. Choose the first
		   * one in the list of parameters as base.
		   *)
		  let vbbase,vboff =
		    match
		      List.fold_left (fun acc v ->
			match acc with Some _ -> acc | None ->
		      	  if VarinfoComparable.equal v vb1 then Some(vb1,vb2)
			  else if VarinfoComparable.equal v vb2 then Some(vb2,vb1)
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
    preaction_expr e; DoChildren

  method vterm t =
    let e,_ = !Db.Properties.Interp.force_term_to_exp t in
    app_under_info preaction_expr e;
    DoChildren

  method vtsets_elem ts =
    let e,_ = !Db.Properties.Interp.force_tsets_elem_to_exp ts in
    app_under_info preaction_expr e;
    DoChildren

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
	BinOp(PlusA,Lval(Var voff,NoOffset),Lval(Var voff2,NoOffset),!ptrdiffType)
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
                      Some(BinOp(PlusA,off1,off2,!ptrdiffType))
                in
                let offopt2 = match v2offopt,offopt2 with
                  | None,None -> None
                  | Some off,None | None,Some off -> Some off
                  | Some off1,Some off2 ->
                      Some(BinOp(PlusA,off1,off2,!ptrdiffType))
                in
                match offopt1,offopt2 with
                  | Some off1,Some off2 ->
		      BinOp(MinusA,off1,off2,!ptrdiffType)
                  | Some off1,None ->
		      off1
                  | None,Some off2 ->
	              UnOp(Neg,off2,!ptrdiffType)
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
	let voff = makeLocalVar f ~insert:true name !ptrdiffType in
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
	      BinOp(MinusPP,Lval(Var v,NoOffset),lval_base vb,!ptrdiffType),
	      !currentLoc)))
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
	    mkStmt(Instr(Set((Var voff,NoOffset),constant_expr 0L,!currentLoc)))
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
		  (* Only recognize self-assignment for formals *)
		  assert (VarinfoComparable.equal v v2);
		  begin try BinOp(PlusA,expr_offset v2,e,!ptrdiffType)
		  with Not_found -> assert false end
	      | Some(v2,None) ->
		  (* Only recognize self-assignment for formals *)
		  assert (VarinfoComparable.equal v v2);
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
		  begin try BinOp(PlusA,expr_offset v2,e,!ptrdiffType)
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
  visit_until_convergence (visitor :> cilVisitor) file;

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
  visit_and_push_statements visitCilFile visitor file


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
	      | MinusA -> Some(v,Some(UnOp(Neg,e2,typeOf e2)))
	      | _ -> assert false
	    end
	| Some(v,Some off) ->
	    begin match op with
	      | PlusA -> Some(v,Some(BinOp(PlusA,off,e2,typeOf e2)))
	      | MinusA -> Some(v,Some(BinOp(MinusA,off,e2,typeOf e2)))
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
object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vfunc f =
    let local v =
      if VarinfoHashtbl.mem cursor_to_base v then
	let name = unique_name ("__jc_off_" ^ v.vname) in
	let voff = makeLocalVar f ~insert:true name intType in
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
	    mkStmt(Instr(Set((Var voff,NoOffset),constant_expr 0L,!currentLoc)))
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
		  BinOp(PlusA,Lval(Var voff2,NoOffset),e,intType)
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

  method vterm =
    do_on_term (None,Some postaction_expr)

  method vtsets_elem =
    do_on_tsets_elem (None,Some postaction_expr)

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
  visit_until_convergence (visitor :> cilVisitor) file;

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
  visit_and_push_statements visitCilFile visitor file


(*****************************************************************************)
(* Annotate code with strlen.                                                *)
(*****************************************************************************)

class annotateCodeStrlen
  (fstrlen : logic_info) (foffset_min : logic_info) (foffset_max : logic_info) =

  (* TODO: extend applicability of [destruct_string_access]. *)
  let lval_destruct_string_access = function
    | Mem e,NoOffset when isCharType(pointed_type(typeOf e)) ->
	begin match destruct_pointer e with
	  | None -> None
	  | Some(v,Some off) -> Some(v,off)
	  | Some(_,None) -> None
	end
    | Var v,Index(i,NoOffset) -> Some (v,i)
    | _ -> None
  in
  let rec destruct_string_access = function
    | Lval lv -> lval_destruct_string_access lv
    | CastE(_,e) -> destruct_string_access e
    | _ -> None
  in
  let rec destruct_string_test ?(neg=false) = function
    | UnOp(LNot,e,_) ->
	destruct_string_test ~neg:(not neg) e
    | BinOp(Ne,e1,e2,_) when is_null_expr e2 ->
	destruct_string_test ~neg e1
    | BinOp(Eq,e1,e2,_) when is_null_expr e2 ->
	destruct_string_test ~neg:(not neg) e1
    | e ->
	match destruct_string_access e with
	  | Some(v,off) -> Some(neg,v,off)
	  | None -> None
  in
  let asrt_to_insert = StmtHashtbl.create 0 in
  let add_pending_asrt pend_asrt stmt =
    try
      let pendls = StmtHashtbl.find asrt_to_insert stmt in
      StmtHashtbl.replace asrt_to_insert stmt (pend_asrt :: pendls)
    with Not_found ->
      StmtHashtbl.replace asrt_to_insert stmt [pend_asrt]
  in
  let attach_pending_asrt stmt =
    try
      let al = StmtHashtbl.find asrt_to_insert stmt in
      List.iter (Annotations.add_assert stmt ~before:true) al
    with Not_found -> ()
  in
object(self)

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vfunc f =
    let strings = List.filter
      (fun v -> hasAttribute "string" (typeAttrs v.vtype)) f.sformals
    in
    let string_reqs =
      List.map (fun v ->
	let lv = cvar_to_lvar v in
	let tv =
	  if app_term_type isArrayType false lv.lv_type then
	    let ptrty = 
	      TPtr(force_app_term_type element_type lv.lv_type,[])
	    in
	    mkterm (TStartOf(TVar lv,TNoOffset)) (Ctype ptrty) v.vdecl 
	  else
	    mkterm (TLval(TVar lv,TNoOffset)) lv.lv_type v.vdecl 
	in
	let tstrlen = Tapp(fstrlen,[],[tv]) in
	let tstrlen = mkterm tstrlen fstrlen.l_type v.vdecl in
	let pstrlen = {
	  name = [];
	  loc = Lexing.dummy_pos,Lexing.dummy_pos;
	  content = Prel(Rge,tstrlen,constant_term !currentLoc 0L);
	} in
	let toffset_min = Tapp(foffset_min,[], [tv]) in
	let toffset_min = mkterm toffset_min foffset_min.l_type v.vdecl in
	let poffset_min = {
	  name = [];
	  loc = Lexing.dummy_pos,Lexing.dummy_pos;
	  content = Prel(Rle,toffset_min,constant_term !currentLoc 0L);
	} in
	let toffset_max = Tapp(foffset_max,[],[tv]) in
	let toffset_max = mkterm toffset_max foffset_max.l_type v.vdecl in
	let poffset_max = {
	  name = [];
	  loc = Lexing.dummy_pos,Lexing.dummy_pos;
	  content = Prel(Rle,tstrlen,toffset_max);
	} in
	let pand =  Logic_const.unamed (Pand(poffset_min,poffset_max)) in
        Logic_const.new_predicate (Logic_const.unamed (Pand(pstrlen,pand)))
      ) strings
    in
    f.sspec.spec_requires <- string_reqs @ f.sspec.spec_requires;
    DoChildren

  method vexpr e =
    begin match destruct_string_access e with
      | None -> ()
      | Some(v,off) ->
	  if hasAttribute "string" (typeAttrs v.vtype) then
	    let toff = !Db.Properties.Interp.force_exp_to_term locUnknown off in
	    let lv = cvar_to_lvar v in
	    let tv =
	      if app_term_type isArrayType false lv.lv_type then
		let ptrty = 
		  TPtr(force_app_term_type element_type lv.lv_type,[])
		in
		mkterm (TStartOf(TVar lv,TNoOffset)) (Ctype ptrty) v.vdecl 
	      else
		mkterm (TLval(TVar lv,TNoOffset)) lv.lv_type v.vdecl 
	    in
	    let tstrlen = Tapp(fstrlen,[], [tv]) in
	    let tstrlen = mkterm tstrlen fstrlen.l_type v.vdecl in
	    let supp = {
	      name = ["hint"];
	      loc = Lexing.dummy_pos,Lexing.dummy_pos;
	      content = Prel(Rge,tstrlen,toff);
	    } in
	    let cur_stmt = the self#current_stmt in
	    add_pending_asrt supp cur_stmt
    end;
    DoChildren

  method vstmt_aux s =
    let preaction s = match s.skind with
      | If(e,tbl,fbl,_loc) ->
	  begin match destruct_string_test e with
	    | None -> s
	    | Some(neg,v,off) ->
		let toff = !Db.Properties.Interp.force_exp_to_term locUnknown off in
		let lv = cvar_to_lvar v in
		let tv =
		  if app_term_type isArrayType false lv.lv_type then
		    let ptrty = 
		      TPtr(force_app_term_type element_type lv.lv_type,[])
		    in
		    mkterm (TStartOf(TVar lv,TNoOffset)) (Ctype ptrty) v.vdecl 
		  else
		    mkterm (TLval(TVar lv,TNoOffset)) lv.lv_type v.vdecl 
		in
		let tstrlen = Tapp(fstrlen,[],[tv]) in
		let tstrlen = mkterm tstrlen fstrlen.l_type v.vdecl in
		let supp = {
		  name = ["hint"];
		  loc = Lexing.dummy_pos,Lexing.dummy_pos;
		  content = Prel(Rgt,tstrlen,toff);
		} in
		let supa = Logic_const.new_code_annotation(AAssert supp) in
		let eqp = {
		  name = ["hint"];
		  loc = Lexing.dummy_pos,Lexing.dummy_pos;
		  content = Prel(Req,tstrlen,toff);
		} in
		let eqa = Logic_const.new_code_annotation(AAssert eqp) in
		let supst = mkStmt(Instr(Code_annot(supa,!currentLoc))) in
		let eqst = mkStmt(Instr(Code_annot(eqa,!currentLoc))) in
		if neg then
		  begin
		    fbl.bstmts <- supst :: fbl.bstmts;
		    tbl.bstmts <- eqst :: tbl.bstmts
		  end
		else
		  begin
		    tbl.bstmts <- supst :: tbl.bstmts;
		    fbl.bstmts <- eqst :: fbl.bstmts
		  end;
		s
	  end
      | _ -> s
    in
    let postaction s =
      let posts = match s.skind with
	| Instr(Set(lv,e,_)) when is_null_expr e ->
	    begin match lval_destruct_string_access lv with
	      | None -> s
	      | Some(v,off) ->
		  let toff = !Db.Properties.Interp.force_exp_to_term locUnknown off in
		  let lv = cvar_to_lvar v in
		  let tv = 
		    if app_term_type isArrayType false lv.lv_type then
		      let ptrty = 
			TPtr(force_app_term_type element_type lv.lv_type,[])
		      in
		      mkterm (TStartOf(TVar lv,TNoOffset)) (Ctype ptrty) v.vdecl 
		    else
		      mkterm (TLval(TVar lv,TNoOffset)) lv.lv_type v.vdecl 
		  in
		  let tstrlen = Tapp(fstrlen,[],[tv]) in
		  let tstrlen = mkterm tstrlen fstrlen.l_type v.vdecl in
		  let toffset_min = Tapp(foffset_min,[],[tv]) in
		  let toffset_min =
		    mkterm toffset_min foffset_min.l_type v.vdecl in
		  let supp = {
		    name = ["hint"];
		    loc = Lexing.dummy_pos,Lexing.dummy_pos;
		    content = Prel(Rge,tstrlen,toffset_min);
		  } in
		  let supa = Logic_const.new_code_annotation(AAssert supp) in
		  let supst = mkStmt(Instr(Code_annot(supa,!currentLoc))) in
		  let infp = {
		    name = ["hint"];
		    loc = Lexing.dummy_pos,Lexing.dummy_pos;
		    content = Prel(Rle,tstrlen,toff);
		  } in
		  let infa = Logic_const.new_code_annotation(AAssert infp) in
		  let infst = mkStmt(Instr(Code_annot(infa,!currentLoc))) in
		  mkStmt(Block(mkBlock[s;supst;infst]))
	    end
	| _ -> s
      in
      attach_pending_asrt s;
      posts
    in
    ChangeDoChildrenPost(preaction s,postaction)

end

let annotate_code_strlen file =
  let find_logic_info (f:file) (name:string) : logic_info option =
    let rec search = function
      | GAnnot(Dlogic_reads(info,_poly,_params,_ty,_reads),_) :: _
          when info.l_name = name -> Some info
      | _ :: rest -> search rest (* tail recursive *)
      | [] -> None
    in
    search f.globals
  in
  match find_logic_info file "strlen" with None -> () | Some fstrlen ->
  match find_logic_info file "offset_min" with None -> () | Some foffset_min ->
  match find_logic_info file "offset_max" with None -> () | Some foffset_max ->
    let visitor = new annotateCodeStrlen fstrlen foffset_min foffset_max in
    visitCilFile (visitor :> cilVisitor) file


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
	let max_int = Int64.of_string
	    (Big_int.string_of_big_int (max_value_of_integral_type ty1))
	in
	if is_left_shift && isSignedInteger ty1 then
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
  visitCilFile (visitor :> cilVisitor) file


(*****************************************************************************)
(* Rewrite type void* into char*.                                            *)
(*****************************************************************************)

class rewriteVoidPointer =
object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vtype ty =
    if isVoidPtrType ty then
      ChangeTo charPtrType
    else DoChildren

end

let rewrite_void_pointer file =
  let visitor = new rewriteVoidPointer in
  visitCilFile (visitor :> cilVisitor) file


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
(*   visitCilFile (visitor :> cilVisitor) file; *)
(*   let visitor = new rewriteTiti table in *)
(*   visit_and_push_statements visitCilFile visitor file *)

let rewrite file =
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
  if Cmdline.Debug.get () >= 1 then
    Format.printf "Rewrite comparison of pointers into difference of pointers@.";
  rewrite_pointer_compare file;
  if checking then check_types file;
  (* Rewrite type void* into char*. *)
(*
   No cast allowed for the moment.
   rewrite_void_pointer file;
*)
  if checking then check_types file;
  (* Rewrite cursor pointers into offsets from base pointers. *)
  (* order: after [rewrite_pointer_compare] *)
  if Cmdline.Debug.get () >= 1 then
    Format.printf "Rewrite cursor pointers into offsets from base pointers@.";
  rewrite_cursor_pointers file;
  if checking then check_types file;
  (* Rewrite cursor integers into offsets from base integers. *)
(*
  Comment out until bug on hashtbl solved, e.g. on [undef.c]
  rewrite_cursor_integers file;
*)
  if checking then check_types file;
  (* Annotate code with strlen. *)
  if Cmdline.Debug.get () >= 1 then
    Format.printf "Annotate code with strlen@.";
  annotate_code_strlen file;
  if checking then check_types file;
  (* Annotate code with overflow checks. *)
  if Cmdline.Debug.get () >= 1 then
    Format.printf "Annotate code with overflow checks@.";
  annotate_overflow file;
  if checking then check_types file;


(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)

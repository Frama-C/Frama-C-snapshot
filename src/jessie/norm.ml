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

(* $Id: norm.ml,v 1.112 2008/11/19 17:41:56 uid570 Exp $ *)

(* TODO:

   - In both retyping phases that add a level of indirection to locals:
       - Retype variables of structure type
       - Retype variables and fields whose address is taken
     If the returned value dereferences a local, take the returned value in
     a temporary before deallocating memory for the local variable and
     returning. Mostly an issue of consistency: since it is a local variable
     involved, it is retyped as a reference and no check is issued for
     validity of dereference.
     See ex roux3.c from Jessie test base.
     Thanks to Pierre Roux for noting this.

*)

(* Import from Cil *)
open Cil_types
open Cil
open Cilutil
open Ast_info
open Extlib

open Visitor

(* Utility functions *)
open Common
open Integer

(*****************************************************************************)
(* Retype variables of array type.                                           *)
(*****************************************************************************)

(* TODO: retype predicate \valid_index and \valid_range
 * E.g. in example array.c, "\valid_index(t,1)" for "t" being typed as
 * "int t[3][3]", should be transformed into "\valid_range(t,3,5)".
 *)
class retypeArrayVariables =

  (* Variables originally of array type *)
  let varset = ref VarinfoSet.empty in
  let lvarset = ref LogicVarSet.empty in

  (* Correspondance between variables and "straw" variables, that are used to
   * replace the variable after some changes have been made on the AST,
   * so that further exploration does not change it anymore. The "straw"
   * variables are replaced by regular one before returning the modified AST.
   *)
  let var_to_strawvar = VarinfoHashtbl.create 17 in
  let strawvar_to_var = VarinfoHashtbl.create 17 in
  let lvar_to_strawlvar = LogicVarHashtbl.create 17 in
  let strawlvar_to_lvar = LogicVarHashtbl.create 17 in

  (* Variables to allocate *)
  let allocvarset = ref VarinfoSet.empty in
  let alloclvarset = ref LogicVarSet.empty in

  (* Remember original array type even after variable modified *)
  let var_to_array_type : typ VarinfoHashtbl.t = VarinfoHashtbl.create 0 in
  let lvar_to_array_type = LogicVarHashtbl.create 17 in

  (* As the rule would be reentrant, do not rely on the fact it is idempotent,
   * and rather change the variable into its "straw" counterpart, as it is
   * done in [preaction_expr]. Also make sure every top expression inside
   * a left-value is an [Info], so that terms that were converted to
   * expressions before treatment can be safely converted back.
   *)
  let preaction_lval (host,off as lv) =
    match host with
      | Var v ->
	  if VarinfoSet.mem v !varset then begin
	    let strawv = VarinfoHashtbl.find var_to_strawvar v in
	    let host = Mem(mkInfo(Lval(Var strawv,NoOffset))) in
	    let off =
	      lift_offset (VarinfoHashtbl.find var_to_array_type v) off
	    in
	    host, off
	  end else
	    lv (* For terms, also corresponds to the case for Result *)
      | Mem _ -> lv
  in

  let postaction_lval (host,off as lv) =
    match host with
      | Var strawv ->
	  begin try
	    let v = VarinfoHashtbl.find strawvar_to_var strawv in
	    Var v, off
	  with Not_found -> lv end
      | Mem _ -> lv
  in

  let rec preaction_expr e = match e with
    | StartOf(Var v,off) ->
	if VarinfoSet.mem v !varset then begin
	  let ty = VarinfoHashtbl.find var_to_array_type v in
	  let strawv = VarinfoHashtbl.find var_to_strawvar v in
	  match lift_offset ty off with
	    | NoOffset -> Lval(Var strawv,NoOffset)
	    | Index(ie,NoOffset) ->
		let ptrty = TPtr(element_type ty,[]) in
		BinOp(PlusPI,Lval(Var strawv,NoOffset),ie,ptrty)
	    | Index _ | Field _ ->
		(* Field with address taken treated separately *)
		StartOf(Mem(Lval(Var strawv,NoOffset)),off)
	end else e
    | AddrOf(Var v,off) ->
	if VarinfoSet.mem v !varset then begin
	  let ty = VarinfoHashtbl.find var_to_array_type v in
	  let strawv = VarinfoHashtbl.find var_to_strawvar v in
	  match lift_offset ty off with
	    | Index(ie,NoOffset) ->
		let ptrty = TPtr(element_type ty,[]) in
		BinOp(PlusPI,Lval(Var strawv,NoOffset),ie,ptrty)
	    | NoOffset -> assert false
	    | Index _ | Field _ ->
		(* Field with address taken treated separately *)
		AddrOf(Mem(Lval(Var strawv,NoOffset)),off)
	end else e
    | BinOp(PlusPI,e1,e2,opty) ->
	begin match stripInfo e1 with
	  | StartOf(Var v,off) ->
	      let rec findtype ty = function
		| NoOffset -> ty
		| Index(_, roff) ->
		    findtype (direct_element_type ty) roff
		| Field _ -> raise Not_found
	      in
	      if VarinfoSet.mem v !varset then
		let ty = VarinfoHashtbl.find var_to_array_type v in
		(* Do not replace [v] by [strawv] here, as the sub-expression
		 * [e1] should be treated first. Do it right away so that
		 * the resulting AST is well-typed, which is crucial to apply
		 * this on expressions obtained from terms.
		 *)
		let e1 = preaction_expr e1 in
		let ty = findtype ty off in
		let subty = direct_element_type ty in
		if isArrayType subty then
		  let siz = array_size subty in
		  let e2 = BinOp(Mult,e2,constant_expr siz,intType) in
		  BinOp(PlusPI,e1,e2,opty)
		else e
	      else e
	  | _ -> e
	end
    | _ -> e
  in

(*
  let preaction_tlval (host,off as lv) =
    match host with
        TVar v when LogicVarSet.mem v !lvarset ->
          let strawv = LogicVarHashtbl.find lvar_to_strawlvar v in
          let host =
            TMem(
              mkterm (TLval (TVar strawv,TNoOffset)) strawv.lv_type locUnknown)
          in
          let off =
            lift_toffset
              (LogicVarHashtbl.find lvar_to_array_type v) off
          in host,off
      | TVar _ | TResult | TMem _ -> lv
  in let preaction_tslval (host,off as lv) =
    match host with
        TSVar v when LogicVarSet.mem v !lvarset ->
          let strawv = LogicVarHashtbl.find lvar_to_strawlvar v in
          let host = TSMem(TSLval(TSVar strawv,TSNoOffset)) in
          let off =
            lift_tsoffset
              (LogicVarHashtbl.find lvar_to_array_type v) off
          in host,off
      | TSVar _ | TSResult | TSMem _ -> lv
  in let postaction_tlval (host,off as lv) =
    match host with
        TVar strawv ->
          begin try
            let v = LogicVarHashtbl.find strawlvar_to_lvar strawv in
            TVar v, off
          with Not_found -> lv end
      | TMem _ | TResult -> lv
  in let postaction_tslval (host,off as lv) =
    match host with
        TSVar strawv ->
          begin try
            let v = LogicVarHashtbl.find strawlvar_to_lvar strawv in
            TSVar v, off
          with Not_found -> lv end
      | TSMem _ | TSResult -> lv
  in let rec preaction_term t = match t.term_node with
      TStartOf(TVar v,off) when LogicVarSet.mem v !lvarset ->
        let ty = LogicVarHashtbl.find lvar_to_array_type v in
        let strawv = LogicVarHashtbl.find lvar_to_strawlvar v in
        let tlval = { t with term_node = TLval(TVar strawv, TNoOffset);
                        term_type = strawv.lv_type}
        in begin
          match lift_toffset ty off with
              TNoOffset -> tlval
            | TIndex(ie,TNoOffset) ->
              let ptrty = TPtr(element_type ty,[]) in
              { t with
                  term_type = Ctype ptrty;
                  term_node = TBinOp(PlusPI,tlval,ie)}
            | TIndex _ | TField _ ->
                { t with term_node = TStartOf(TMem(tlval),off) }
        end
    | TAddrOf(TVar v, off) when LogicVarSet.mem v !lvarset ->
         let ty = LogicVarHashtbl.find lvar_to_array_type v in
        let strawv = LogicVarHashtbl.find lvar_to_strawlvar v in
        let tlval = { t with term_node = TLval(TVar strawv, TNoOffset);
                        term_type = strawv.lv_type}
        in begin
          match lift_toffset ty off with
              TIndex(ie,TNoOffset) ->
                let ptrty = TPtr(element_type ty,[]) in
                { t with
                  term_type = Ctype ptrty;
                  term_node = TBinOp(PlusPI,tlval,ie)}
            | TNoOffset -> assert false
            | TIndex _ | TField _ ->
                { t with term_node = TAddrOf(TMem tlval, off) }
        end
    | TBinOp(PlusPI,({term_node = TStartOf(TVar v,off)} as e1),e2)
        when LogicVarSet.mem v !lvarset ->
        let ty = LogicVarHashtbl.find lvar_to_array_type v in
        let e1 = preaction_term e1 in
        let ty = typeTermOffset (Ctype ty) off in
        let subty = direct_element_type (force_app_term_type (fun x -> x) ty)
        in
        let e2 =
          if isArrayType subty then
            let siz = array_size subty in
            { e2 with term_node = TBinOp(Mult,e2, constant_term locUnknown siz)}
          else e2
        in { t with term_node = TBinOp(PlusPI,e1,e2) }
     | TConst _ | TLval _ | TSizeOf _ | TSizeOfE _ | TSizeOfStr _
    | TAlignOf _ | TAlignOfE _ | TUnOp _ | TBinOp _ | TCastE _
    | TAddrOf _ | TStartOf _ | Tapp _ | Tlambda _ | TDataCons _ | Tif _
    | Told _ | Tat _ | Tbase_addr _ | Tblock_length _ | Tnull
    | TCoerce _ | TCoerceE _ | TUpdate _ | Ttypeof _ | Ttype _ | Ttsets _ -> t

  in let rec preaction_tsets t = match t with
      TSStartOf(TSVar v,off) when LogicVarSet.mem v !lvarset ->
        let ty = LogicVarHashtbl.find lvar_to_array_type v in
        let strawv = LogicVarHashtbl.find lvar_to_strawlvar v in
        let tslval = TSLval(TSVar strawv, TSNoOffset) in
        begin
          match lift_tsoffset ty off with
              TSNoOffset -> tslval
            | TSIndex(ie,TSNoOffset) -> TSAdd_index(tslval,ie)
            | TSRange(ie1,ie2,TSNoOffset) -> TSAdd_range(tslval,ie1,ie2)
            | TSIndex _ | TSField _ | TSRange _ -> TSStartOf(TSMem(tslval),off)
        end
    | TSAddrOf(TSVar v, off) when LogicVarSet.mem v !lvarset ->
         let ty = LogicVarHashtbl.find lvar_to_array_type v in
         let strawv = LogicVarHashtbl.find lvar_to_strawlvar v in
         let tlval = TSLval(TSVar strawv, TSNoOffset)
         in begin
          match lift_tsoffset ty off with
              TSIndex(ie,TSNoOffset) -> TSAdd_index(tlval,ie)
            | TSRange(ie1,ie2,TSNoOffset) -> TSAdd_range(tlval,ie1,ie2)
            | TSNoOffset -> assert false
            | TSIndex _ | TSField _ | TSRange _ -> TSAddrOf(TSMem tlval, off)
        end
    | TSAdd_index(TSStartOf(TSVar v,off) as e1,e2)
        when LogicVarSet.mem v !lvarset ->
        let ty = LogicVarHashtbl.find lvar_to_array_type v in
        let e1 = preaction_tsets e1 in
        let ty = typeTsetsOffset (Ctype ty) off in
        let subty = direct_element_type (force_app_term_type (fun x -> x) ty)
        in
        if isArrayType subty then
          let siz = array_size subty in
          let e2 =
            { e2 with term_node = TBinOp(Mult,e2,constant_term locUnknown siz)}
          in TSAdd_index(e1,e2)
        else t
    | TSAdd_range(TSStartOf(TSVar v,off) as e1,e2,e3)
        when LogicVarSet.mem v !lvarset ->
        let ty = LogicVarHashtbl.find lvar_to_array_type v in
        let e1 = preaction_tsets e1 in
        let ty = typeTsetsOffset (Ctype ty) off in
        let subty = direct_element_type (force_app_term_type (fun x -> x) ty)
        in
        if isArrayType subty then
          let siz = array_size subty in
          let e2 =
            opt_map
              (fun e2 ->
                 { e2 with term_node =
                     TBinOp(Mult,e2,constant_term locUnknown siz)}) e2
          in
          let e3 =
            opt_map
              (fun e3 ->
                 { e3 with term_node =
                     TBinOp(Mult,e3,constant_term locUnknown siz)}) e3
          in
          TSAdd_range(e1,e2,e3)
        else t
    | TSLval _ | TSStartOf _ | TSAddrOf _ | TSConst _ | TSAdd_index _
    | TSAdd_range _ | TSCastE _ | TSat _ | TSapp _ -> t
in
*)
object(self)

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vvdec v =
    if isArrayType v.vtype && not (VarinfoSet.mem v !varset) then
      begin
	assert (not v.vformal);
	VarinfoHashtbl.add var_to_array_type v v.vtype;
	let elemty = element_type v.vtype in
	(* Store information that variable was originally of array type *)
	varset := VarinfoSet.add v !varset;
	(* Change the variable type *)
	let newty =
	  if array_size v.vtype > 0L then
	    begin
	      (* Change the type into "reference" type, that behaves almost like
	       * a pointer, except validity is ensured.
	       *)
	      let size = constant_expr (array_size v.vtype) in
	      (* Schedule for allocation *)
	      allocvarset := VarinfoSet.add v !allocvarset;
	      mkTRefArray(elemty,size,[]);
	    end
	  else
	    (* Plain pointer type to array with zero size *)
	    TPtr(v.vtype,[]);
	in
	attach_globaction (fun () -> v.vtype <- newty);
	(* Create a "straw" variable for this variable, with the correct type *)
 	let strawv = makePseudoVar newty in
	VarinfoHashtbl.add var_to_strawvar v strawv;
	VarinfoHashtbl.add strawvar_to_var strawv v;
      end;
    DoChildren

  method vlogic_var_decl lv =
    if not (LogicVarHashtbl.mem lvar_to_strawlvar lv) &&
      app_term_type isArrayType false lv.lv_type then
      begin
        LogicVarHashtbl.add lvar_to_array_type lv
          (force_app_term_type (fun x -> x) lv.lv_type);
        let elemty = force_app_term_type element_type lv.lv_type in
        lvarset := LogicVarSet.add lv !lvarset;
        let newty =
          if force_app_term_type array_size lv.lv_type > 0L then
            begin
              let size =
                constant_expr (force_app_term_type array_size lv.lv_type)
              in alloclvarset := LogicVarSet.add lv !alloclvarset;
              mkTRefArray(elemty,size,[])
            end
          else TPtr(elemty,[])
        in attach_globaction (fun () -> lv.lv_type <- Ctype newty);
        let strawlv = match lv.lv_origin with
            None -> make_temp_logic_var (Ctype newty)
          | Some v -> cvar_to_lvar (VarinfoHashtbl.find var_to_strawvar v)
        in
        LogicVarHashtbl.add lvar_to_strawlvar lv strawlv;
        LogicVarHashtbl.add strawlvar_to_lvar strawlv lv
      end;
      DoChildren

  method vglob_aux g = match g with
    | GVar(v,_init,loc) ->
	(* Make sure variable declaration is treated before definition *)
	ignore (visitFramacVarDecl (self:>frama_c_visitor) v);
	if VarinfoSet.mem v !allocvarset then
	  (* Allocate memory for new reference variable *)
	  let ty = VarinfoHashtbl.find var_to_array_type v in
	  let elemty = element_type ty in
	  let size = array_size ty in
	  let ast = mkalloc_array_statement v elemty (array_size ty) loc in
	  attach_globinit ast;
	  (* Define a global validity invariant *)
	  let p =
	    Pvalid_range(
	      variable_term v.vdecl (cvar_to_lvar v),
	      constant_term v.vdecl 0L,
	      constant_term v.vdecl (size - 1L))
	  in
	  let globinv =
            { l_name = unique_logic_name ("valid_" ^ v.vname);
	      l_tparams = [];
	      l_type = None;
              l_profile = [];
              l_labels = [ LogicLabel "Here" ];
              l_body = LBpred (predicate v.vdecl p)}
	  in
          attach_globaction (fun () -> Logic_env.add_logic_function globinv);
	  ChangeTo [g;GAnnot(Dinvariant globinv,v.vdecl)]
	else DoChildren
    | GVarDecl _ | GFun _ | GAnnot _ -> DoChildren
    | GCompTag _ | GType _ | GCompTagDecl _ | GEnumTagDecl _
    | GEnumTag _ | GAsm _ | GPragma _ | GText _ -> SkipChildren

  method vfunc f =
    (* First change type of local array variables *)
    List.iter (ignore $ visitFramacVarDecl (self:>frama_c_visitor)) f.slocals;
    List.iter (ignore $ visitFramacVarDecl (self:>frama_c_visitor)) f.sformals;
    (* Then allocate/deallocate memory for those that need it *)
    List.iter (fun v ->
      if VarinfoSet.mem v !allocvarset then
	let ty = VarinfoHashtbl.find var_to_array_type v in
	let elemty = element_type ty in
	let ast = mkalloc_array_statement v elemty (array_size ty) v.vdecl in
	add_pending_statement ~beginning:true ast;
	let fst = mkfree_statement v v.vdecl in
	add_pending_statement ~beginning:false fst
    ) f.slocals;
    DoChildren

  method vlval lv =
    ChangeDoChildrenPost (preaction_lval lv, postaction_lval)

  method vterm_lval =
    do_on_term_lval (Some preaction_lval, Some postaction_lval)

  method vtsets_lval =
    do_on_tsets_lval (Some preaction_lval, Some postaction_lval)

  method vexpr e =
    ChangeDoChildrenPost (preaction_expr e, fun x -> x)

  method vterm =
    do_on_term (Some preaction_expr, None)

  method vtsets_elem =
    do_on_tsets_elem (Some preaction_expr, None)

end

let retype_array_variables file =
  (* Enforce the prototype of malloc to exist before visiting anything.
     It might be useful for allocation pointers from arrays
  *)
  ignore (Common.malloc_function ());
  ignore (Common.free_function ());
  let visitor = new retypeArrayVariables in
  visit_and_push_statements visit_and_update_globals visitor file


(*****************************************************************************)
(* Retype logic functions/predicates with structure parameters or return.    *)
(*****************************************************************************)

(* logic parameter:
 * - change parameter type to pointer to structure
 * - change uses of parameters in logical annotations
 * TODO: take care of logic variables introduced by let.
 *)
class retypeLogicFunctions =

  let varset = ref LogicVarSet.empty in
  let this_name : string ref = ref "" in
  let change_this_type = ref false in  (* Should "this" change? *)
  let new_result_type = ref (Ctype voidType) in
  let change_result_type = ref false in (* Should Result change? *)

  let var lv =
    match lv.lv_type with
      | Ltype _ | Lvar _ | Linteger | Lreal | Larrow _ -> ()
      | Ctype ty ->
	  if isStructOrUnionType ty then
	    begin
	      varset := LogicVarSet.add lv !varset;
	      lv.lv_type <- Ctype(mkTRef ty);
	      match lv.lv_origin with
		| None -> ()
		| Some v ->
		    (* For some obscure reason, logic function/predicate
		     * parameters are C variables.
		     *)
		    v.vtype <- mkTRef ty
	    end
  in

  let postaction_term_lval (host,off) =
    let host = match host with
      | TVar lv ->
	  (* Oddly, "this" variable in type invariant is not declared
	     before use. Change its type on the fly.
	  *)
	  if !change_this_type && lv.lv_name = !this_name then
	    var lv;
	  if LogicVarSet.mem lv !varset then
	    let tlval =
	      mkterm (TLval(host,TNoOffset)) lv.lv_type locUnknown
	    in
	    TMem tlval
	  else host
      | TResult ->
	  if !change_result_type then
	    let tlval =
	      mkterm (TLval(host,TNoOffset)) !new_result_type locUnknown
	    in
	    TMem tlval
	  else host
      | TMem _t -> host
    in
    host, off
  in
object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vannotation =
    let return_type ty =
      change_result_type := false;
      match ty with
	| Ctype rt when isStructOrUnionType rt ->
	    begin
	      change_result_type := true;
	      let ty = Ctype(mkTRef rt) in
	      new_result_type := ty;
	      ty
	    end
	| Ctype _ | Ltype _ | Lvar _ | Linteger | Lreal | Larrow _ -> ty
    in
    let rec annot = function
(*
      | Dpredicate_reads(_name,_poly,params,_)
      | Dpredicate_def(_name,_poly,params,_) 
      | Dinductive_def(_name,_poly,params,_) ->
	  List.iter var params;
	  DoChildren
      | Dlogic_reads(info,poly,params,rt,tlocs) ->
	  List.iter var params;
	  let rt = return_type rt in
	  ChangeDoChildrenPost
            (Dlogic_reads(info,poly,params,rt,tlocs), fun x -> x)
      | Dlogic_def(name,poly,params,rt,t) ->
	  List.iter var params;
	  let rt = return_type rt in
	  ChangeDoChildrenPost
	    (Dlogic_def(name,poly,params,rt,t), fun x -> x)
      | Dlogic_axiomatic(name,poly,params,rt,axioms) ->
	  List.iter var params;
	  let rt = return_type rt in
	  ChangeDoChildrenPost
	    (Dlogic_axiomatic(name,poly,params,rt,axioms), fun x -> x)
*)
      | Dfun_or_pred li ->
	  List.iter var li.l_profile;
	  begin
	    match li.l_type with
	      | None -> DoChildren
	      | Some rt ->
		  let li' = { li with l_type = Some (return_type rt)}  in
		  ChangeDoChildrenPost (Dfun_or_pred li', fun x -> x)
	  end
      | Dtype_annot annot ->
	  begin match (List.hd annot.l_profile).lv_type with
	    | Ctype ty when isStructOrUnionType ty ->
		change_this_type := true;
		this_name := (List.hd annot.l_profile).lv_name;
		let annot = { annot with
                                l_profile = [{ (List.hd annot.l_profile) with
                                                 lv_type = Ctype(mkTRef ty)}];
		}
                in
		ChangeDoChildrenPost
		  (Dtype_annot annot, fun x -> change_this_type := false; x)
	    | Ctype _ | Ltype _ | Lvar _ | Linteger | Lreal | Larrow _ ->
                DoChildren
	  end
      | Dtype _ | Dlemma _ | Dinvariant _ -> DoChildren
      | Daxiomatic _ -> DoChildren (* FIXME: correct ? *)

    in annot

  method vterm_lval tlv =
    ChangeDoChildrenPost (tlv, postaction_term_lval)

  method vtsets_lval tslv =
    (* No need to perform all actions as preaction or postaction as when
     * terms and tsets are converted to expressions. Here there is no
     * environment for untranslated sub-trees in the AST that still need
     * to be transformed. Still, for simplicity, we do so instead of calling
     * [visitFramacTerm] and returning [ChangeTo...]
     *)
    let postaction_tslval tslv =
      let tlv = !Db.Properties.Interp.force_tsets_lval_to_term_lval tslv in
      let tlv = postaction_term_lval tlv in
      !Db.Properties.Interp.force_back_term_lval_to_tsets_lval tlv
    in
    ChangeDoChildrenPost (tslv, postaction_tslval)

  method vterm t =
    let preaction_term t =
      match t.term_node with
	| Tapp(callee,labels,args) ->
	    let args = List.map (fun arg ->
	      (* Type of [arg] has not been changed. *)
	      match arg.term_type with
		| Ltype _ | Lvar _ | Linteger | Lreal | Larrow _ -> arg
		| Ctype ty ->
		    if isStructOrUnionType ty then
		      match arg.term_node with
			| TLval lv ->
			    (* Arguments translated under address here may
			     * be translated back to dereference when treating
			     * left-values. This is why we add a normalization
			     * in [postaction_term]. *)
			    {
			      arg with
				term_node = TAddrOf lv;
				term_type = Ctype(mkTRef ty);
			    }
			| _ -> assert false (* Should not be possible *)
		    else arg
	    ) args in
	    { t with term_node = Tapp(callee,labels,args); }
	| _ -> t
    in
    (* Renormalize the term tree. *)
    let postaction_term t =
      match t.term_node with
	| TAddrOf(TMem t,TNoOffset) -> t
	| _ -> t
    in
    ChangeDoChildrenPost (preaction_term t, postaction_term)

  method vpredicate = function
    | Papp(callee,labels,args) ->
	let args = List.map (fun arg ->
	  (* Type of [arg] has not been changed. *)
	  match arg.term_type with
	    | Ltype _ | Lvar _ | Linteger | Lreal | Larrow _ -> arg
	    | Ctype ty ->
		if isStructOrUnionType ty then
		  match arg.term_node with
		    | TLval lv ->
			{
			  arg with
			    term_node = TAddrOf lv;
			    term_type = Ctype(mkTRef ty);
			}
		    | _ -> assert false (* Should not be possible *)
		else arg
	) args in
	ChangeDoChildrenPost (Papp(callee,labels,args), fun x -> x)
    | _ -> DoChildren

end

let retype_logic_functions file =
  let visitor = new retypeLogicFunctions in
  visitFramacFile visitor file


(*****************************************************************************)
(* Expand structure copying through parameter, return or assignment.         *)
(*****************************************************************************)

let return_vars = VarinfoHashtbl.create 17

(* parameter:
 * - if function defined, add local copy variable of structure type
 * - change parameter type to pointer to structure
 * - change type at call-site to take address of structure arguments
 * - change uses of parameters in logical annotations
 * return:
 * - change return type to pointer to structure
 * - add temporary variable for call
 * - free allocated memory for return after call
 * assignment:
 * - recursively decompose into elementary assignments
 *)
class expandStructAssign () =

  let pairs = ref [] in
  let new_return_type = ref None in
  let return_var = ref None in
  let curFundec : fundec ref = ref (emptyFunction "@dummy@") in

  let postaction_term_lval (host,off) =
    let host = match host with
      | TResult ->
	  begin match !new_return_type with None -> TResult | Some rt ->
	    let tlval =
	      mkterm (TLval(TResult,TNoOffset)) (Ctype rt) locUnknown in
	    TMem tlval
	  end
      | TVar v ->
	  begin match v.lv_origin with
	  | None -> host
	      (* TODO: recognize \result variable, and change its use if
		 of reference type here. *)
	  | Some cv ->
	      try
		let newv = List.assoc cv !pairs in
		let newlv = cvar_to_lvar newv in
		(* Type of [newv] is set at that point. *)
		let tlval =
		  mkterm (TLval(TVar newlv,TNoOffset)) (Ctype newv.vtype)
		    locUnknown
		in
		TMem tlval
	      with Not_found -> TVar v
	  end
      | TMem _t -> host
    in
    host, off
  in

  let rec expand_assign lv e ty loc =
    match unrollType ty with
      | TComp(mcomp,_) ->
	  let field fi =
	    let newlv = addOffsetLval (Field(fi,NoOffset)) lv in
	    let newe = match e with
	      | Lval elv -> Lval(addOffsetLval (Field(fi,NoOffset)) elv)
	      | _ ->
		  (* Other possibilities like [CastE] should have been
		     transformed at this point. *)
		  assert false
	    in
	    expand_assign newlv newe fi.ftype loc
	  in
	  List.flatten (List.map field mcomp.cfields)
      | TArray _ ->
	  let elem i =
	    let cste = constant_expr i in
	    let newlv = addOffsetLval (Index(cste,NoOffset)) lv in
	    let newe = match e with
	      | Lval elv -> Lval (addOffsetLval (Index(cste,NoOffset)) elv)
	      | _ ->
		  (* Other possibilities like [CastE] should have been
		     transformed at this point. *)
		  assert false
	    in
	    expand_assign newlv newe (direct_element_type ty) loc
	  in
	  let rec all_elem acc i =
	    if i >= 0L then all_elem (elem i @ acc) (i - 1L) else acc
	  in
	  assert (not (is_reference_type ty));
	  all_elem [] (direct_array_size ty - 1L)
      | _ -> [Set (lv, e, loc)]
  in

  let rec expand lv ty loc =
    match unrollType ty with
      | TComp(mcomp,_) ->
	  let field fi =
	    let newlv = addOffsetLval (Field(fi,NoOffset)) lv in
	    expand newlv fi.ftype loc
	  in
	  List.flatten (List.map field mcomp.cfields)
      | TArray _ ->
	  let elem i =
	    let cste = constant_expr i in
	    let newlv = addOffsetLval (Index(cste,NoOffset)) lv in
	    expand newlv (direct_element_type ty) loc
	  in
	  let rec all_elem acc i =
	    if i >= 0L then all_elem (elem i @ acc) (i - 1L) else acc
	  in
	  assert (not (is_reference_type ty));
	  all_elem [] (direct_array_size ty - 1L)
      | _ -> [ lv ]
  in

object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vglob_aux =
    let retype_func fvi =
      let formal (n,ty,a) =
	let ty = if isStructOrUnionType ty then mkTRef ty else ty in
	n, ty, a
      in
      let rt,params,isva,a = splitFunctionTypeVI fvi in
      let params = match params with
	| None -> None
	| Some p -> Some(List.map formal p)
      in
      let rt = if isStructOrUnionType rt then mkTRef rt else rt in
      fvi.vtype <- TFun(rt,params,isva,a)
    in
    function
      | GVarDecl(_spec,v,_attr) ->
	  if isFunctionType v.vtype && not v.vdefined then retype_func v;
	  DoChildren
      | GFun _
      | GAnnot _ -> DoChildren
      | GVar _  | GCompTag _ | GType _ | GCompTagDecl _ | GEnumTagDecl _
      | GEnumTag _ | GAsm _ | GPragma _ | GText _ -> SkipChildren

  method vfunc f =
    curFundec := f;
    let var v =
      if isStructOrUnionType v.vtype then
	let newv = copyVarinfo v (unique_name ("v_" ^ v.vname)) in
	newv.vtype <- mkTRef newv.vtype;
	v.vformal <- false;
	let rhs = Lval(mkMem (Lval(Var newv,NoOffset)) NoOffset) in
	let copy = mkassign_statement (Var v,NoOffset) rhs v.vdecl in
	add_pending_statement ~beginning:true copy;
	pairs := (v,newv) :: !pairs;
	[v], newv
      else
	[], v
    in
    (* Insert copy statements. *)
    let locvl,formvl = List.split (List.map var f.sformals) in
    (* Set locals and formals. *)
    let locvl = List.flatten locvl in
    f.slocals <- locvl @ f.slocals;
    setFormals f formvl;
    (* Add local variable for return *)
    let rt = getReturnType f.svar.vtype in
    if isStructOrUnionType rt then
      let rv = makeTempVar !curFundec rt in
      return_var := Some rv;
      VarinfoHashtbl.add return_vars rv ()
    else
      return_var := None;
    (* Change return type. *)
    new_return_type :=
      if isStructOrUnionType rt then Some(mkTRef rt) else None;
    let rt = if isStructOrUnionType rt then mkTRef rt else rt in
    setReturnType f rt;
    DoChildren

  method vbehavior b =
    let lval lv = expand lv (typeOfLval lv) locUnknown in
    let tsets_elem = function
      | TSLval tslv ->
	  let lv,env = 
	    !Db.Properties.Interp.force_tsets_lval_to_lval 
	      (Common.get_result_type ()) tslv 
	  in
	  let lvlist = lval lv in
	  let tslvlist =
	    List.map (!Db.Properties.Interp.force_back_lval_to_tsets_lval env)
	      lvlist
	  in
	  List.map (fun
		      tslv -> TSLval tslv
		   ) tslvlist
      | TSStartOf _
      | TSConst _
      | TSAdd_index _ (* TODO *)
      | TSAdd_range _ (* TODO *)
      | TSCastE _
      | TSat _
      | TSAddrOf _
      | TSapp _ as tselem -> [ tselem ]
    in
    let tsets = function
      | TSSingleton tselem ->
	  List.map (fun tselem -> TSSingleton tselem) (tsets_elem tselem)
      | TSEmpty
      | TSUnion _ (* TODO *)
      | TSInter _ (* TODO *)
      | TSComprehension _ as ts -> [ ts ]
    in
    let zone = function
      | Location idts ->
	  List.map (fun ts -> Location { idts with its_content = ts })
	    (tsets idts.its_content)
      | Nothing -> [ Nothing ]
    in
    let assign (z,froms) =
      let zl = zone z in
      let froms = List.flatten (List.map zone froms) in
      List.map (fun z -> z, froms) zl
    in
    b.b_assigns <- List.flatten (List.map assign b.b_assigns);
    DoChildren

  method vstmt_aux s = match s.skind with
    | Return(Some e,loc) ->
	(* Type of [e] has not been changed by retyping formals and return. *)
	if isStructOrUnionType (typeOf e) then
(* 	  match e with *)
(* 	    | Lval lv -> *)
(* 		let skind = Return(Some(Cabs2cil.mkAddrOfAndMark lv),loc) in *)
(* 		ChangeTo { s with skind = skind; } *)
(* 	    | _ -> assert false (\* Should not be possible *\) *)
	  let lv = Var(the !return_var),NoOffset in
	  let ret = mkStmt (Return(Some(Cabs2cil.mkAddrOfAndMark lv),loc)) in
	  let assigns = expand_assign lv e (typeOf e) loc in
	  let assigns = List.map (fun i -> mkStmt(Instr i)) assigns in
	  let block = Block (mkBlock (assigns @ [ret])) in
	  ChangeTo { s with skind = block }
	else SkipChildren
    | _ -> DoChildren

  method vinst =
    function
      | Set(lv,e,loc) ->
	  (* Type of [e] has not been changed by retyping formals and return. *)
	  if isStructOrUnionType (typeOf e) then
	    ChangeTo (expand_assign lv e (typeOf e) loc)
	  else SkipChildren
      | Call(lvo,callee,args,loc) ->
	  let args = List.map (fun arg ->
	    (* Type of [arg] has not been changed. *)
	    if isStructOrUnionType (typeOf arg) then
	      match arg with
	      | Lval lv -> Cabs2cil.mkAddrOfAndMark lv
	      | _ -> assert false (* Should not be possible *)
	    else arg
	  ) args in
	  begin match lvo with
	    | None ->
		(* TODO: free memory for structure return, even if not used.
		   Check that no temporary is added in every case, which would
		   make treatment here useless. *)
		let call = Call (lvo, callee, args, loc) in
		ChangeTo [call]
	    | Some lv ->
		(* Type of [lv] has not been changed. *)
		let lvty = typeOfLval lv in
		if isStructOrUnionType lvty then
		  let tmpv = makeTempVar !curFundec (mkTRef lvty) in
		  let tmplv = Var tmpv, NoOffset in
		  let call = Call(Some tmplv,callee,args,loc) in
		  let deref = Lval(mkMem (Lval(Var tmpv,NoOffset)) NoOffset) in
		  let assign = mkassign lv deref loc in
		  let free = mkfree tmpv loc in
		  ChangeTo [call;assign;free]
		else
		  let call = Call(lvo,callee,args,loc) in
		  ChangeTo [call]
	  end
      | Asm _ | Skip _ | Code_annot _ -> SkipChildren

  method vterm_lval tlv =
    ChangeDoChildrenPost (tlv, postaction_term_lval)

  method vtsets_lval tslv =
    let postaction_tslval tslv =
      let tlv = !Db.Properties.Interp.force_tsets_lval_to_term_lval tslv in
      let tlv = postaction_term_lval tlv in
      !Db.Properties.Interp.force_back_term_lval_to_tsets_lval tlv
    in
    ChangeDoChildrenPost (tslv, postaction_tslval)

  method vterm t =
    (* Renormalize the term tree. *)
    let postaction t =
      match t.term_node with
	| TAddrOf(TMem t,TNoOffset) -> t
	| _ -> t
    in
    ChangeDoChildrenPost (t, postaction)

end

let expand_struct_assign file =
  let visitor = new expandStructAssign () in
  visit_and_store_result_type visitFramacFile
    (visit_and_push_statements_visitor visitor) file


(*****************************************************************************)
(* Retype variables of structure type.                                       *)
(*****************************************************************************)

(* DO NOT CHANGE neither formal parameters nor return type of functions.
 *
 * global variable:
 * - change type to reference to structure
 * - prepend allocation in [globinit] function
 * - changes left-values to reflect new type
 * local variable:
 * - change type to reference to structure
 * - prepend allocation at function entry
 * - TODO: postpend release at function exit
 * - changes left-values to reflect new type
 *)

class retypeStructVariables =

  let varset = ref VarinfoSet.empty in
  let lvarset = ref LogicVarSet.empty in

  let postaction_lval (host,off) =
    let host = match host with
      | Var v ->
	  if VarinfoSet.mem v !varset then
	    Mem(mkInfo(Lval(Var v,NoOffset)))
	  else
	    Var v
      | Mem _e -> host
    in
    host, off
  in
  let postaction_tlval (host,off) =
    let add_deref host v =
      TMem(mkterm (TLval (host,TNoOffset)) v.lv_type locUnknown)
    in
    let host = match host with
      | TVar v ->
	  if LogicVarSet.mem v !lvarset then
            add_deref host v
	  else
	    opt_app
	      (fun cv ->
		 if VarinfoSet.mem cv !varset then
		   add_deref host v
		 else host
	      ) host v.lv_origin
      | TMem _ | TResult -> host
    in
    host, off
  in
  let postaction_tslval (host,off) =
    let add_deref host =
      TSMem(TSLval(host,TSNoOffset))
    in
    let host = match host with
      | TSVar v ->
	  if LogicVarSet.mem v !lvarset then
	    add_deref host
	  else
	    opt_app
	      (fun cv ->
		 if VarinfoSet.mem cv !varset then
		   add_deref host
		 else host
	      ) host v.lv_origin
      | TSMem _ | TSResult -> host
    in host,off
in
object(self)

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vvdec v =
    if isStructOrUnionType v.vtype && not v.vformal then
      begin
	v.vtype <- mkTRef v.vtype;
	varset := VarinfoSet.add v !varset
      end;
    DoChildren

  method vquantifiers vl =
    List.iter (fun v ->
		 (* Only iterate on logic variable with C type *)
		 if app_term_type (fun _ -> true) false v.lv_type then
		   match v.lv_origin with
		     | None -> ()
		     | Some v -> ignore (self#vvdec v)
		 else ()
	      ) vl;
    DoChildren

  method vlogic_var_decl v =
    let newty =
      app_term_type
	(fun ty ->
	   Ctype (if isStructOrUnionType ty then mkTRef ty else ty))
	v.lv_type v.lv_type
    in
    v.lv_type <- newty;
    DoChildren

  method vglob_aux = function
    | GVar (_,_,_) as g ->
	let postaction = function
	  | [GVar (v,_,_)] ->
	      if VarinfoSet.mem v !varset then
		(* Allocate memory for new reference variable *)
		let ast = mkalloc_statement v (pointed_type v.vtype) v.vdecl in
		attach_globinit ast;
		(* Define a global validity invariant *)
		let p =
		  Pvalid_range(
		    variable_term v.vdecl (cvar_to_lvar v),
		    constant_term v.vdecl 0L,
		    constant_term v.vdecl 0L)
		in
	        let globinv =
                  { l_name = unique_logic_name ("valid_" ^ v.vname);
		    l_tparams = [];
		    l_type = None;
                    l_profile = [];
                    l_labels = [ LogicLabel "Here" ];
                    l_body = LBpred (predicate v.vdecl p)}
	        in
                attach_globaction (fun () -> Logic_env.add_logic_function globinv);
	        [g; GAnnot(Dinvariant globinv,v.vdecl)]
	      else [g]
	  | _ -> assert false
	in
	ChangeDoChildrenPost ([g], postaction)
    | GVarDecl _ | GFun _ | GAnnot _ -> DoChildren
    | GCompTag _ | GType _ | GCompTagDecl _ | GEnumTagDecl _
    | GEnumTag _ | GAsm _ | GPragma _ | GText _ -> SkipChildren

  method vfunc f =
    (* First change type of local structure variables *)
    List.iter (ignore $ visitFramacVarDecl (self:>frama_c_visitor)) f.slocals;
    List.iter (ignore $ visitFramacVarDecl (self:>frama_c_visitor)) f.sformals;
    (* Then allocate/deallocate memory for those that need it *)
    List.iter (fun v ->
      if VarinfoSet.mem v !varset then
	let ast = mkalloc_statement v (pointed_type v.vtype) v.vdecl in
	add_pending_statement ~beginning:true ast;
	(* do not deallocate variable used in returning a structure *)
	if not (VarinfoHashtbl.mem return_vars v) then
	  let fst = mkfree_statement v v.vdecl in
	  add_pending_statement ~beginning:false fst
    ) f.slocals;
    DoChildren

  method vlval lv =
    ChangeDoChildrenPost (lv, postaction_lval)

  method vterm_lval lv = ChangeDoChildrenPost(lv, postaction_tlval)

  method vtsets_lval lv = ChangeDoChildrenPost(lv, postaction_tslval)

  method vexpr e =
    (* Renormalize the expression tree. *)
    let postaction = function
      | AddrOf(Mem e,NoOffset) -> e
      | e -> e
    in
    ChangeDoChildrenPost (e, postaction)

  method vterm t =
    (* Renormalize the term tree. *)
    let postaction t =
      match t.term_node with
	| TAddrOf(TMem t,TNoOffset) -> t
	| _ -> t
    in
    ChangeDoChildrenPost (t, postaction)

end

let retype_struct_variables file =
  let visitor = new retypeStructVariables in
  visit_and_push_statements visit_and_update_globals visitor file


(*****************************************************************************)
(* Retype variables and fields whose address is taken.                       *)
(*****************************************************************************)

(* global variable:
 * - change type from [t] to [t*]
 * - prepend allocation in [globinit] function
 * local variable:
 * - change type from [t] to [t*]
 * - prepend allocation at function entry
 * - TODO: postpend release at function exit
 * formal parameter:
 * - make it a local variable, with previous treatment
 * - replace by a new parameter with same type
 * - prepend initialisation at function entry, after allocation
 * - TODO: decide whether formal parameter address can be taken in
 *   annotations. Currently, if address only taken in annotations,
 *   [vaddrof] would not be set. Plus there is no easy means of translating
 *   such annotation to Jessie.
 * field:
 * - change type from [t] to [t*]
 * - TODO: allocation/release
 *)
class retypeAddressTaken =

  let varset = ref VarinfoSet.empty in
  let lvarset = ref LogicVarSet.empty in
  let fieldset = ref FieldinfoSet.empty in

  let retypable_var v =
    v.vaddrof
    && not (isArrayType v.vtype)
    && not (is_reference_type v.vtype)
  in
  let retypable_lvar v =
    match v.lv_origin with None -> false | Some v -> retypable_var v
  in
  (* Only retype fields with base/pointer type, because fields of
   * struct/union type will be retyped in any case later on.
   *)
  let retypable_field fi =
    fi.faddrof
    && not (is_reference_type fi.ftype)
    && not (isArrayType fi.ftype)
    && not (isStructOrUnionType fi.ftype)
  in
  let retype_var v =
    if retypable_var v then
      begin
	v.vtype <- mkTRef v.vtype;
	assert (isPointerType v.vtype);
	varset := VarinfoSet.add v !varset
      end
  in
  let retype_lvar v =
    if retypable_lvar v then begin
      v.lv_type <- Ctype (force_app_term_type mkTRef v.lv_type);
      lvarset := LogicVarSet.add v !lvarset
    end
  in
  let retype_field fi =
    if retypable_field fi then
      begin
	fi.ftype <- mkTRef fi.ftype;
	assert (isPointerType fi.ftype);
	fieldset := FieldinfoSet.add fi !fieldset
      end
  in

  let postaction_lval (host,off) =
    let host = match host with
      | Var v ->
	  if VarinfoSet.mem v !varset then
	    begin
	      assert (isPointerType v.vtype);
	      Mem(mkInfo(Lval(Var v,NoOffset)))
	    end
	  else host
      | Mem _e -> host
    in
    (* Field retyped can only appear as the last offset, as it is of
     * base/pointer type.
     *)
    match lastOffset off with
    | Field(fi,_) ->
	if FieldinfoSet.mem fi !fieldset then
	  (assert (isPointerType fi.ftype);
	  mkMem (mkInfo(Lval(host,off))) NoOffset)
	else
	  host,off
    | _ ->
	host,off
  in

  let postaction_tlval (host,off) =
    let add_deref host ty =
      force_app_term_type (fun ty -> assert (isPointerType ty)) ty;
      TMem(mkterm (TLval (host,TNoOffset)) ty locUnknown)
    in
    let host = match host with
      | TVar v ->
	  if LogicVarSet.mem v !lvarset then
	    add_deref host v.lv_type
	  else
	    opt_app
	      (fun cv ->
		 if VarinfoSet.mem cv !varset then
		   add_deref host (Ctype cv.vtype)
		 else host
	      ) host v.lv_origin
      | TResult | TMem _ -> host
    in match lastTermOffset off with
      | TField (fi,_) ->
          if FieldinfoSet.mem fi !fieldset then
            (TMem(Logic_const.mk_dummy_term
                    (TLval(host,off)) fi.ftype),TNoOffset)
          else host,off
      | TIndex _ | TNoOffset -> host,off
  in
  let postaction_tslval (host,off) =
    let add_deref host =
      TSMem (TSLval(host,TSNoOffset))
    in
    let host = match host with
      | TSVar v ->
	  if LogicVarSet.mem v !lvarset then
	    add_deref host
	  else
	    opt_app
	      (fun cv ->
		 if VarinfoSet.mem cv !varset then
		   add_deref host
		 else host
	      ) host v.lv_origin
      | TSResult | TSMem _ -> host
    in match lastTsetsOffset off with
      | TSField (fi,_) ->
          if FieldinfoSet.mem fi !fieldset then
            (TSMem(TSLval(host,off)), TSNoOffset)
          else host,off
      | TSIndex _ | TSRange _ | TSNoOffset -> host,off
  in

  let postaction_expr e = match e with
    | AddrOf(Var _v,NoOffset) ->
	assert false (* Host should have been turned into [Mem] *)
    | AddrOf(Mem e1,NoOffset) ->
	e1
    | AddrOf(_host,off) ->
	begin match lastOffset off with
	  | Field(fi,_) ->
	      if FieldinfoSet.mem fi !fieldset then
		(* Host should have been turned into [Mem], with NoOffset *)
		assert false
	      else
		e
	  | Index _ -> e
	  | NoOffset -> assert false (* Should be unreachable *)
	end
    | _ -> e
  in

  let postaction_term t = match t.term_node with
    | TAddrOf((TVar _ | TResult), TNoOffset) -> assert false
    | TAddrOf(TMem t1,TNoOffset) -> t1
    | TAddrOf(_,off) ->
        begin match lastTermOffset off with
          | TField(fi,_) ->
              if FieldinfoSet.mem fi !fieldset then assert false
              else t
          | TIndex _ -> t
          | TNoOffset -> assert false (*unreachable*)
        end
    | _ -> t
  in
  let postaction_tsets t = match t with
    | TSAddrOf((TSVar _ | TSResult), TSNoOffset) -> assert false
    | TSAddrOf(TSMem t1,TSNoOffset) -> t1
    | TSAddrOf(_,off) ->
        begin match lastTsetsOffset off with
          | TSField(fi,_) ->
              if FieldinfoSet.mem fi !fieldset then assert false
              else t
          | TSIndex _ | TSRange _ -> t
          | TSNoOffset -> assert false (*unreachable*)
        end
    | _ -> t
  in
  let varpairs : (varinfo * varinfo) list ref = ref [] in
  let in_funspec = ref false in
object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vglob_aux = function
    | GVar(v,_,_) ->
	if retypable_var v then
	  begin
	    retype_var v;
	    let ast = mkalloc_statement v (pointed_type v.vtype) v.vdecl in
	    attach_globinit ast
	  end;
	SkipChildren
    | GVarDecl (_,v,_) ->
	(* No problem with calling [retype_var] more than once, since
	   subsequent calls do nothing on reference type. *)
	if not (isFunctionType v.vtype || v.vdefined) then retype_var v;
	SkipChildren
    | GFun _ -> DoChildren
    | GAnnot _ -> DoChildren
    | GCompTag(compinfo,_loc) ->
	List.iter retype_field compinfo.cfields;
	SkipChildren
    | GType _ | GCompTagDecl _ | GEnumTagDecl _ | GEnumTag _
    | GAsm _ | GPragma _ | GText _ -> SkipChildren

  method vfunc f =
    (* Change types before code. *)
    let formals,locals,pairs =
      List.fold_right (fun v (fl,ll,pl) ->
	if retypable_var v then
	  let newv = copyVarinfo v ("v_" ^ v.vname) in
	  newv.vaddrof <- false;
	  v.vformal <- false;
	  (newv::fl,v::ll,(v,newv)::pl)
	else (v::fl,ll,pl)
      ) f.sformals ([],[],[])
    in

    varpairs := pairs;
    setFormals f formals;
    f.slocals <- locals @ f.slocals;
    List.iter retype_var f.slocals;

    List.iter (fun v ->
      (* allocate/deallocate locals *)
      if VarinfoSet.mem v !varset then
	begin
	  let ast = mkalloc_statement v (pointed_type v.vtype) v.vdecl in
	  add_pending_statement ~beginning:true ast;
	  (* do not deallocate variable used in returning a structure *)
	  if not (VarinfoHashtbl.mem return_vars v) then
	    let fst = mkfree_statement v v.vdecl in
	    add_pending_statement ~beginning:false fst
	end;
      (* allocate/deallocate formals *)
      begin try
	(* [varpairs] holds pairs of (local,formal) to initialize due to
	 * the transformation for formals whose address is taken.
	 *)
	let fv = List.assoc v !varpairs in
	let lhs = mkMem (Lval(Var v,NoOffset)) NoOffset in
	let rhs = Lval(Var fv,NoOffset) in
	let assign = mkassign_statement lhs rhs v.vdecl in
	add_pending_statement ~beginning:true assign
      with Not_found -> () end
    ) f.slocals;
    DoChildren

  method vspec funspec =
    in_funspec := true;
    ChangeDoChildrenPost (funspec, fun x -> in_funspec := false; x)

  method vlogic_var_use v =
    if !in_funspec then
      match v.lv_origin with
	| None -> SkipChildren
	| Some cv ->
	    try
	      let fv = List.assoc cv !varpairs in
	      ChangeTo (cvar_to_lvar fv)
	    with Not_found -> SkipChildren
    else
      begin
	if retypable_lvar v then retype_lvar v;
	DoChildren
      end

  method vlogic_var_decl v = if retypable_lvar v then retype_lvar v; DoChildren

  method vlval lv = ChangeDoChildrenPost (lv, postaction_lval)

  method vterm_lval lv = ChangeDoChildrenPost (lv, postaction_tlval)

  method vtsets_lval lv = ChangeDoChildrenPost (lv, postaction_tslval)

  method vexpr e = ChangeDoChildrenPost(e, postaction_expr)

  method vterm t = ChangeDoChildrenPost(t,postaction_term)

  method vtsets_elem t = ChangeDoChildrenPost(t,postaction_tsets)

end

let retype_address_taken file =
  let visitor = new retypeAddressTaken in
  visit_and_push_statements visit_and_update_globals visitor file


(*****************************************************************************)
(* Retype fields of type structure and array.                                *)
(*****************************************************************************)

(* We translate C left-values so that they stick to the Jessie semantics for
 * left-values. E.g., a C left-value
 *     s.t.i
 * which is translated in CIL as
 *     Var s, Field(t, Field(i, NoOffset))
 * is translated as
 *     Mem (Mem s, Field(t, NoOffset)), Field(i, NoOffset)
 * so that it is the same as the C left-value
 *     s->t->i
 *
 * Introduce reference at each structure subfield.
 * Does not modify union fields on purpose : union should first be translated
 * into inheritance before [retypeFields] is called again.
 *)
class retypeFields =

  let field_to_array_type : typ FieldinfoHashtbl.t = FieldinfoHashtbl.create 0 in

  let postaction_lval (host,off) =
    let rec offset_list = function
      | NoOffset -> []
      | Field (fi,off) ->
	  (Field (fi, NoOffset)) :: offset_list off
      | Index (e, Field (fi,off)) ->
	  (Index (e, Field (fi, NoOffset))) :: offset_list off
      | Index (_idx, NoOffset) as off -> [off]
      | Index (idx, (Index _ as off)) ->
	  assert (not !flatten_multi_dim_array);
	  Index(idx,NoOffset) :: offset_list off
    in
    let rec apply_lift_offset = function
      | Field (fi,roff) ->
	  begin try
	    let ty = FieldinfoHashtbl.find field_to_array_type fi in
	    let roff = apply_lift_offset (lift_offset ty roff) in
	    Field (fi,roff)
	  with Not_found ->
	    let roff = apply_lift_offset roff in
	    Field (fi,roff)
	  end
      | Index (idx,roff) ->
	  let roff = apply_lift_offset roff in
	  Index (idx,roff)
      | NoOffset -> NoOffset
    in
    let off =
      if !flatten_multi_dim_array then apply_lift_offset off else off
    in
    (* [initlv] : topmost lval
     * [initlist] : list of offsets to apply to topmost lval
     *)
    let initlv,initlist = match offset_list off with
      | [] -> (host, NoOffset), []
      | fstoff :: roff -> (host, fstoff), roff
    in
    List.fold_left
      (fun curlv -> function
	 | NoOffset ->
	     assert false (* should not occur *)
	 | Field(_,_)
	 | Index(_, Field (_,_))
	 | Index(_, NoOffset) as nextoff ->
	     Mem(mkInfo(Lval curlv)),nextoff
	 | Index (_, Index _) -> assert false
      ) initlv initlist
  in

  (* Renormalize the expression tree. *)
  let postaction_expr = function
    | AddrOf(Mem e,NoOffset) | StartOf(Mem e,NoOffset) -> e
    | AddrOf(Mem _e,Field(_fi,off) as lv) | StartOf(Mem _e,Field(_fi,off) as lv) ->
	assert (off = NoOffset);
	(* Only possibility is that field is of structure or union type,
	 * otherwise [retype_address_taken] would have taken care of it.
	 * Do not check it though, because type was modified in place.
	 *)
	Lval lv
    | AddrOf(Mem e,Index(ie,NoOffset)) ->
	let ptrty = TPtr(typeOf e,[]) in
	BinOp(PlusPI,e,ie,ptrty)
    | StartOf(Mem _e,Index(_ie,NoOffset) as lv) ->
	Lval lv
    | AddrOf(Mem _e,Index(_ie,Field(_,NoOffset)) as lv)
    | StartOf(Mem _e,Index(_ie,Field(_,NoOffset)) as lv) ->
	Lval lv
    | AddrOf(Mem _e,Index(_ie,_)) | StartOf(Mem _e,Index(_ie,_)) ->
	assert false
    | e -> e
  in
object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vglob_aux = function
    | GCompTag (compinfo,_) ->
	let field fi =
	  if isStructOrUnionType fi.ftype then
	    fi.ftype <- mkTRef fi.ftype
	  else if isArrayType fi.ftype then
	    begin
	      FieldinfoHashtbl.replace field_to_array_type fi fi.ftype;
	      if not !flatten_multi_dim_array then
		fi.ftype <- reference_of_array fi.ftype
	      else
(* 		if array_size fi.ftype > 0L then *)
		  let size = constant_expr (array_size fi.ftype) in
		  fi.ftype <- mkTRefArray(element_type fi.ftype,size,[])
(* 		else *)
(* 		  (\* Array of zero size, e.g. in struct array hack. *\) *)
(* 		  fi.ftype <- TPtr(element_type fi.ftype,[]) *)
	    end
	in
	List.iter field compinfo.cfields;
	SkipChildren
    | GFun _ | GAnnot _ | GVar _ | GVarDecl _ -> DoChildren
    | GType _ | GCompTagDecl _ | GEnumTagDecl _
    | GEnumTag _ | GAsm _ | GPragma _ | GText _ -> SkipChildren

  method vlval lv =
    ChangeDoChildrenPost (lv, postaction_lval)

  method vterm_lval =
    do_on_term_lval (None,Some postaction_lval)

  method vtsets_lval =
    do_on_tsets_lval (None,Some postaction_lval)

  method vexpr e =
    ChangeDoChildrenPost(e, postaction_expr)

  method vterm =
    do_on_term (None,Some postaction_expr)

  method vtsets_elem =
    do_on_tsets_elem (None,Some postaction_expr)

end

let retype_fields file =
  let visitor = new retypeFields in 
  visit_and_store_result_type visitFramacFile visitor file


(*****************************************************************************)
(* Retype type tags.                                                         *)
(*****************************************************************************)

class retypeTypeTags =
object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vterm t = match t.term_node with
    | Ttype ty -> ChangeTo ({ t with term_node = Ttype(TPtr(ty,[])) })
    | _ -> DoChildren

end

let retype_type_tags file =
  let visitor = new retypeTypeTags in visitFramacFile visitor file

(*****************************************************************************)
(* Retype pointers to base types.                                            *)
(*****************************************************************************)

let debugtab = Hashtbl.create 0

(* Retype pointer to base type T to pointer to struct S with:
 * - if T is [TVoid], no field in S
 * - otherwise, a single field of type T in S
 *)
class retypeBasePointer =

  (* Correspondance between a base type and its wrapper structure type *)
  let type_wrappers : typ TypeHashtbl.t = TypeHashtbl.create 17 in
  (* Store which types are wrapper types *)
  let auto_type_wrappers = ref TypeSet.empty in

  let is_wrapper_type ty = TypeSet.mem ty !auto_type_wrappers in

  let new_wrapper_for_type_no_sharing ty =
    (* Choose name t_P for the wrapper and t_M for the field *)
    let name = type_name ty in
    let wrapper_name = name ^ "P" in
    let field_name = name ^ "M" in
    let compinfo =
      if isVoidType ty then mkStructEmpty wrapper_name
      else mkStructSingleton wrapper_name field_name ty
    in
    let tdef = GCompTag(compinfo,locUnknown) in
    let tdecl = TComp(compinfo,[]) in
    attach_global tdef;
    tdef, tdecl
  in
object(self)

  (* Helper methods called on the [self] object *)

  method new_wrapper_for_type ty =
    (* Currently, do not make any difference between a pointer to const T
     * or volatile T and a pointer to T.
     *)
    let ty = typeRemoveAttributes ["const";"volatile"] (unrollType ty) in
    try
      TypeHashtbl.find type_wrappers ty
    with Not_found ->
      (* Construct a new wrapper for this type *)
      let wrapper_def,wrapper_type = new_wrapper_for_type_no_sharing ty in
      TypeHashtbl.replace type_wrappers ty wrapper_type;
      auto_type_wrappers := TypeSet.add wrapper_type !auto_type_wrappers;
      (* Treat newly constructed type *)
      let store_current_global = !currentGlobal in
      ignore (visitFramacGlobal (self:>frama_c_visitor) wrapper_def);
      currentGlobal := store_current_global;
      (* Return the wrapper type *)
      wrapper_type

  (* Performs the necessary in-place modifications to [ty] so that
   * the translation to Jessie is easy.
   * Returns [Some newty] if the modified type imposes adding a field
   * access to a dereference on an object of type [ty].
   * Returns [None] in all other cases, in particular for non-wrapper
   * types that are allowed as either type of variable or type of field.
   *)
  method private wrap_type_if_needed ty =
    match ty with
      | TPtr(_elemty,attr) ->
	  (* Do not use [_elemty] directly but rather [pointed_type ty] in order
	   * to get to the array element in references, i.e. pointers to arrays.
	   *)
	  let elemty = pointed_type ty in
	  if is_wrapper_type elemty then
	    Some ty
	  else if isStructOrUnionType elemty then
	    None (* Already in a suitable form for Jessie translation. *)
	  else if is_array_reference_type ty then
	    (* Do not lose the information that this type is a reference *)
	    let size = constant_expr (reference_size ty) in
	    assert (not (!flatten_multi_dim_array && is_reference_type elemty));
	    Some(mkTRefArray(self#new_wrapper_for_type elemty,size,[]))
	  else if is_reference_type ty then
	    (* Do not lose the information that this type is a reference *)
	    Some(mkTRef(self#new_wrapper_for_type elemty))
	  else
	    (* Here is the case where a transformation is needed *)
	    Some(TPtr(self#new_wrapper_for_type elemty,attr))
      | TArray _ -> None (* TODO: change in assert false *)
      | TFun _ -> None
      | TNamed(typeinfo,_attr) ->
	  begin match self#wrap_type_if_needed typeinfo.ttype with
	    | Some newtyp ->
		typeinfo.ttype <- newtyp;
		Some ty
	    | None -> None
	  end
      | TComp(compinfo,_attr) ->
	  let field fi =
	    match self#wrap_type_if_needed fi.ftype with
	      | Some newtyp ->
		  fi.ftype <- newtyp
	      | None -> ()
	  in
	  List.iter field compinfo.cfields;
	  None
      | TVoid _ | TInt _ | TFloat _ | TEnum _ | TBuiltin_va_list _ -> None

  method private postaction_lval lv =
    match lv with
      | Var _, NoOffset -> lv
      | Var _, _ -> 
	  assert false
      | Mem e, NoOffset ->
	  begin match self#wrap_type_if_needed (typeOf e) with
	    | Some newtyp ->
		let newfi = get_unique_field (pointed_type newtyp) in
		let newlv = Mem e, Field (newfi, NoOffset) in
		(* Check new left-value is well-typed. *)
(* 		begin try ignore (typeOfLval newlv) with _ -> assert false end; *)
		newlv
	    | None -> lv
	  end
      | Mem e, (Index(ie,_) as off) ->
	  if is_last_offset off then
	    match self#wrap_type_if_needed (typeOf e) with
	      | Some newtyp ->
		  let newfi = get_unique_field (pointed_type newtyp) in
		  let newlv =
		    if is_array_reference_type newtyp then
		      lv
		    else
		      Mem(BinOp(PlusPI,e,ie,newtyp)), NoOffset
		  in
		  let newlv = addOffsetLval (Field (newfi, NoOffset)) newlv in
(* 		  begin try ignore (typeOfLval newlv) with _ -> assert false end; *)
		  newlv
	      | None -> lv
	  else lv
      | Mem _, Field _ -> lv

  (* Usual methods in visitor interface. *)

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vfile _ =
    Common.struct_type_for_void := self#new_wrapper_for_type voidType;
    DoChildren

  method vtype ty =
    let ty = match self#wrap_type_if_needed ty with
      | Some newty -> newty
      | None -> ty
    in
    if isFunctionType ty then
      (* Applies changes in particular to parameter types in function types. *)
      ChangeDoChildrenPost (ty, fun x -> x)
    else
      ChangeTo ty

  method vglob_aux =
    let retype_return v =
      let retyp = getReturnType v.vtype in
      let newtyp = visitFramacType (self:>frama_c_visitor) retyp in
      if newtyp != retyp then setReturnTypeVI v newtyp
    in
    function
      | GType (typeinfo, _) ->
	  ignore (self#wrap_type_if_needed (TNamed (typeinfo, [])));
	  SkipChildren
      | GCompTag (compinfo, _) ->
	  ignore (self#wrap_type_if_needed (TComp (compinfo, [])));
	  SkipChildren
      | GFun (f, _) ->
	  retype_return f.svar;
	  DoChildren
      | GVarDecl (_, v, _) ->
	  if isFunctionType v.vtype && not v.vdefined then
	    retype_return v;
	  DoChildren
      | GVar _
      | GAnnot _ -> DoChildren
      | GCompTagDecl _ | GEnumTag _ | GEnumTagDecl _
      | GAsm _ | GPragma _ | GText _  -> SkipChildren

  method vlval lv =
    ChangeDoChildrenPost (lv, self#postaction_lval)

  method vterm_lval =
    do_on_term_lval (None,Some self#postaction_lval)

  method vtsets_lval =
    do_on_tsets_lval (None,Some self#postaction_lval)

end

let retype_base_pointer file =
  let visitor = new retypeBasePointer in
  visit_and_store_result_type visit_and_update_globals
    (visitor :> frama_c_visitor) file


(*****************************************************************************)
(* Remove useless casts.                                                     *)
(*****************************************************************************)

class removeUselessCasts =
  let preaction_expr etop =
    match stripInfo etop with
      | CastE(ty,e) ->
	  let ety = typeOf e in
	  if isPointerType ty && isPointerType ety then
	    (* Ignore type qualifiers *)
	    let tysig =
	      typeSig (typeRemoveAttributes ["const";"volatile"]
			 (unrollType (pointed_type ty)))
	    in
	    let etysig =
	      typeSig (typeRemoveAttributes ["const";"volatile"]
			 (unrollType (pointed_type ety)))
	    in
	    if Cilutil.equals tysig etysig then
	      e
	    else etop
	  else etop
      | _ -> etop
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

end

let remove_useless_casts file =
  let visitor = new removeUselessCasts in
  visit_and_store_result_type visitFramacFile visitor file


(*****************************************************************************)
(* Translate union fields into structures                                    *)
(*****************************************************************************)

let generated_union_types = TypeHashtbl.create 0

class translateUnions =
  let field_to_equiv_type : typ FieldinfoHashtbl.t
      = FieldinfoHashtbl.create 0
  in
  let new_field_type fi =
    let tname = unique_name (fi.fname ^ "P") in
    let fname = unique_name (fi.fname ^ "M") in
    let padding = the fi.fpadding_in_bits in
    let mcomp =
      mkStructSingleton ~padding tname fname fi.ftype in
    let tdef = GCompTag (mcomp, CurrentLoc.get ()) in
    let tdecl = TComp (mcomp, []) in
    TypeHashtbl.add generated_union_types tdecl ();
    FieldinfoHashtbl.add field_to_equiv_type fi tdecl;
    fi.ftype <- tdecl;
    tdef
  in

  let postaction_offset = function
    | Field(fi,off) as off' ->
	begin try
	  let ty = FieldinfoHashtbl.find field_to_equiv_type fi in
	  let newfi = get_unique_field ty in
	  Field(fi,Field(newfi,off))
	with Not_found -> off' end
    | off -> off
  in
object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vglob_aux = function
    | GCompTag (compinfo,_) as g when not compinfo.cstruct ->
	let fields = compinfo.cfields in
	let field fi = new_field_type fi in
	let fty = List.map field fields in
	ChangeTo (g::fty)
    | GFun _ | GAnnot _ | GVar _ | GVarDecl _ -> DoChildren
    | GCompTag _ | GType _ | GCompTagDecl _ | GEnumTagDecl _
    | GEnumTag _ | GAsm _ | GPragma _ | GText _ -> SkipChildren

  method voffs off =
    ChangeDoChildrenPost(off,postaction_offset)

  method vterm_offset =
    do_on_term_offset (None, Some postaction_offset)

  method vtsets_offset =
    do_on_tsets_offset (None, Some postaction_offset)

end

let translate_unions file =
  let visitor = new translateUnions in
  visit_and_store_result_type visitFramacFile visitor file

(*****************************************************************************)
(* Remove array address.                                                     *)
(*****************************************************************************)

class removeArrayAddress =
object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vexpr e =
    let preaction e = match e with
      | AddrOf(Mem ptre,Index(ie,NoOffset)) ->
	  let ptrty = typeOf e in
	  BinOp (PlusPI, ptre, ie, ptrty)
      | _ -> e
    in
    ChangeDoChildrenPost (preaction e, fun x -> x)

  method vterm t =
    let preaction t = match t.term_node with
      | TAddrOf(TMem ptrt,TIndex(it,TNoOffset)) ->
	  { t with term_node = TBinOp (PlusPI, ptrt, it); }
      | _ -> t
    in
    ChangeDoChildrenPost (preaction t, fun x -> x)

  (* TODO: translate to add tsets easily *)

end

let remove_array_address file =
  let visitor = new removeArrayAddress in
  visitFramacFile visitor file


(*****************************************************************************)
(* Normalize the C file for Jessie translation.                              *)
(*****************************************************************************)

open Pervasives

let normalize file =
  if checking then check_types file;
  (* Retype variables of array type. *)
  (* order: before [expand_struct_assign] and any other pass which calls
     [typeOf], because "t[i]" with [StartOf] if type of "t" is "int t[a][b]"
     is not typed correctly by Cil (raises error StartOf on non-array type).
     See, e.g., example array_addr.c. *)
  if Cmdline.Debug.get () >= 1 then
    Format.printf "Retype variables of array type@.";
  retype_array_variables file;
  if checking then check_types file;
  (* Retype logic functions/predicates with structure parameters or return. *)
  if Cmdline.Debug.get () >= 1 then
    Format.printf "Retype logic functions/predicates@.";
  retype_logic_functions file;
  if checking then check_types file;
  (* Expand structure copying through parameter, return or assignment. *)
  (* order: before [retype_address_taken], before [retype_struct_variables] *)
  if Cmdline.Debug.get () >= 1 then
    Format.printf "Expand structure copying@.";
  expand_struct_assign file;
  if checking then check_types file;
  (* Retype variables of structure type. *)
  if Cmdline.Debug.get () >= 1 then
    Format.printf "Retype variables of structure type@.";
  retype_struct_variables file;
  if checking then check_types file;
  (* Retype variables and fields whose address is taken. *)
  (* order: after [retype_struct_variables] *)
  if Cmdline.Debug.get () >= 1 then
    Format.printf "Retype variables and fields whose address is taken@.";
  retype_address_taken file;
  if checking then check_types file;
  (* Expand structure copying through assignment. *)
  (* Needed because sequence [expand_struct_assign; retype_struct_variables;
     retype_address_taken] may recreate structure assignments. *)
  (* order: after [retype_address_taken] *)
  if Cmdline.Debug.get () >= 1 then
    Format.printf "Expand structure copying through assignment@.";
  expand_struct_assign file;
  if checking then check_types file;
  (* Translate union fields into structures. *)
  if Cmdline.Debug.get () >= 1 then
    Format.printf "Translate union fields into structures@.";
  translate_unions file;
  if checking then check_types file;
  (* Retype fields of type structure and array. *)
  (* order: after [expand_struct_assign] and [retype_address_taken]
   * before [translate_unions] *)
  if Cmdline.Debug.get () >= 1 then
    Format.printf "Retype fields of type structure and array@.";
  retype_fields file;
  if checking then check_types file;
  (* Retype fields of type structure and array. *)
  (* order: after [translate_unions] *)
  if Cmdline.Debug.get () >= 1 then
    Format.printf "Retype fields of type structure and array@.";
  retype_fields file;
  if checking then check_types file;
  (* Remove array address. *)
  (* order: before [retype_base_pointer] *)
  if Cmdline.Debug.get () >= 1 then
    Format.printf "Remove array address@.";
  remove_array_address file;
  if checking then check_types file;
  (* Retype type tags. *)
  (* order: before [retype_base_pointer] *)
  if Cmdline.Debug.get () >= 1 then
    Format.printf "Retype type tags@.";
  retype_type_tags file;
  if checking then check_types file;
  (* Retype pointers to base types. *)
  (* order: after [retype_fields] *)
  if Cmdline.Debug.get () >= 1 then
    Format.printf "Retype pointers to base types@.";
  retype_base_pointer file;
  if checking then check_types file;
  (* Remove useless casts. *)
  if Cmdline.Debug.get () >= 1 then
    Format.printf "Remove useless casts@.";
  remove_useless_casts file;
  if checking then check_types file;

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j bin/toplevel.byte"
End:
*)

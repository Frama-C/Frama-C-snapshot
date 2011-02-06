(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

open Db_types
open Cil_types
open Cilutil
open Cil

let pretty_vname fmt vi = !Ast_printer.d_ident fmt vi.vname

(* ************************************************************************** *)
(** {2 Expressions} *)
(* ************************************************************************** *)

let is_integral_const = function
  | CInt64 _ | CEnum _ | CChr _ -> true
  | CStr _ | CWStr _ | CReal _ -> false

let rec possible_value_of_integral_const = function
  | CInt64 (i,_,_) -> Some i
  | CEnum {eival = e} -> possible_value_of_integral_expr e
  | CChr c -> Some (Int64.of_int (Char.code c))
  | _ -> None

and possible_value_of_integral_expr e =
  match (stripInfo e).enode with
    | Const c -> possible_value_of_integral_const c
    | _ -> None

let value_of_integral_const c =
  match possible_value_of_integral_const c with
    | None -> assert false
    | Some i -> i

let value_of_integral_expr e =
  match possible_value_of_integral_expr e with
    | None -> assert false
    | Some i -> i

let constant_expr ~loc i = new_exp ~loc (Const(CInt64(i,IInt,None)))

let rec is_null_expr e = match (stripInfo e).enode with
  | Const c when is_integral_const c ->
      value_of_integral_const c = Int64.zero
  | CastE(_,e) -> is_null_expr e
  | _ -> false

let rec is_non_null_expr e = match (stripInfo e).enode with
  | Const c when is_integral_const c ->
      value_of_integral_const c <> Int64.zero
  | CastE(_,e) -> is_non_null_expr e
  | _ -> false

(* ************************************************************************** *)
(** {2 Logical terms} *)
(* ************************************************************************** *)

let possible_value_of_integral_term t =
  match t.term_node with
    | TConst c -> possible_value_of_integral_const c
    | _ -> None

let term_lvals_of_term t =
  let l = ref [] in
  ignore
    (Cil.visitCilTerm
       (object
	  inherit nopCilVisitor
	  method vterm_lval lv =
	    l := lv :: !l;
	    DoChildren
	end)
       t);
  !l

let is_trivial_predicate = function Ptrue -> true | _ -> false

let is_trivial_term v =
  match v.term_node with Tnull -> true | _ -> false

let is_trivial_named_predicate p = is_trivial_predicate p.content

let is_trivial_annotation = function
  | AAssert (_,a) -> is_trivial_named_predicate a
  | APragma _ | AStmtSpec _ | AInvariant _ | AVariant _
  | AAssigns _
(* | ALoopBehavior _ *)
    -> false

let is_trivial_rooted_assertion = function
  | User ca | AI(_, ca) -> is_trivial_annotation ca.annot_content

let behavior_postcondition b k =
  let assumes =
    Logic_const.pold
      (Logic_const.pands (List.map Logic_const.pred_of_id_pred b.b_assumes))
  in
  let postcondition =
    Logic_const.pands
      (Extlib.filter_map (fun (x,_) -> x = k)
         (Logic_const.pred_of_id_pred $ snd) b.b_post_cond)
  in
  Logic_const.pimplies (assumes,postcondition)

let behavior_precondition b =
  let assumes = Logic_const.pands (List.rev_map Logic_const.pred_of_id_pred b.b_assumes)
  in
  let requires =Logic_const.pands (List.rev_map Logic_const.pred_of_id_pred b.b_requires)
  in
  Logic_const.pimplies (assumes,requires)

let precondition spec =
  Logic_const.pands (List.map behavior_precondition spec.spec_behavior)

let merge_assigns (l : funbehavior list) =
  let unguarded_behaviors =
    List.filter (fun l -> l.b_assumes = []) l in
  match unguarded_behaviors with
    | [] -> (* No unguarded behavior -> assigns evything *) WritesAny
    | l -> (* Let's check if there is an "assigns everything" *)
	if List.exists (fun b -> b.b_assigns=WritesAny) l then WritesAny
	else match l with
	  | [{b_assigns=r}] -> r
	  | {b_name=n;b_assigns=r}::_ ->
	      Cil.warn "keeping only assigns of behavior %s" n;
	      r
	  | [] -> assert false
(*
    List.fold_left (fun a b -> Logic_utils.merge_assigns a b.b_assigns) [] l
*)
let variable_term loc v =
  {
    term_node = TLval(TVar v,TNoOffset);
    term_loc = loc;
    term_type = v.lv_type;
    term_name = [];
  }

let constant_term loc i =
  {
    term_node = TConst(CInt64(i,IInt,None));
    term_loc = loc;
    term_type = Ctype intType;
    term_name = [];
  }

let rec is_null_term t = match t.term_node with
  | TConst c when is_integral_const c ->
      value_of_integral_const c = Int64.zero
  | TCastE(_,t) -> is_null_term t
  | _ -> false

(* ************************************************************************** *)
(** {2 Predicates} *)
(* ************************************************************************** *)

let predicate loc p =
  {
    name = [];
    loc = loc;
    content = p;
  }

(* ************************************************************************** *)
(** {2 Annotations} *)
(* ************************************************************************** *)

let before_after_content = function Before x | After x -> x

let lift_annot_func f a = match before_after_content a with
  | User p | AI (_,p) -> f p

let lift_annot_list_func f l =
  let add l x = match before_after_content x with
    | User p | AI(_,p) -> p :: l
  in
  let l' = List.fold_left add [] l in
  f (List.rev l')

(* ************************************************************************** *)
(** {2 Statements} *)
(* ************************************************************************** *)

let is_loop_statement s = match s.skind with Loop _ -> true | _ -> false

let get_sid s = match s with
  | Kglobal -> assert false
  | Kstmt s -> s.sid

let mkassign lv e loc = Set(lv,e,loc)

let mkassign_statement lv e loc = mkStmt (Instr(mkassign lv e loc))

let is_block_local v b = List.exists (fun vv -> v.vid = vv.vid) b.blocals

(* ************************************************************************** *)
(** {2 Functions} *)
(* ************************************************************************** *)

let is_function_type vi = isFunctionType vi.vtype

module Function = struct

  let formal_args called_vinfo = match called_vinfo.vtype with
    | TFun (_,Some argl,_,_) ->
	argl
    | TFun _ ->
	[]
    | _ -> assert false

  let is_formal v fundec =
    List.exists (fun vv -> v.vid = vv.vid) fundec.sformals

  let is_local v fundec = List.exists (fun vv -> v.vid = vv.vid) fundec.slocals

  let is_formal_or_local v fundec =
    (not v.vglob) && ((is_formal v fundec) || (is_local v fundec))

  let is_formal_of_prototype v vi =
    let formals = try getFormalsDecl vi with Not_found -> [] in
    List.exists (fun x -> x.vid = v.vid) formals

  let is_definition = function
    | Definition _ -> true
    | Declaration _ -> false

  let get_vi = function
    | Definition (d, _) -> d.svar
    | Declaration (_,vi,_, _) -> vi

  let get_name f = (get_vi f).vname
  let get_id f = (get_vi f).vid

end

(* ************************************************************************** *)
(** {2 Types} *)
(* ************************************************************************** *)

let array_type ?length ?(attr=[]) ty = TArray(ty,length,empty_size_cache (),attr)

let direct_array_size ty =
  match unrollType ty with
    | TArray(_ty,Some size,_,_) -> value_of_integral_expr size
    | TArray(_ty,None,_,_) -> 0L
    | _ -> assert false

let rec array_size ty =
  match unrollType ty with
    | TArray(elemty,Some _,_,_) ->
	if isArrayType elemty then
	  Int64.mul (direct_array_size ty) (array_size elemty)
	else direct_array_size ty
    | TArray(_,None,_,_) -> 0L
    | _ -> assert false

let direct_element_type ty = match unrollType ty with
  | TArray(eltyp,_,_,_) -> eltyp
  | _ -> assert false

let element_type ty =
  let rec elem_type ty = match unrollType ty with
    | TArray(eltyp,_,_,_) -> elem_type eltyp
    | _ -> ty
  in
  match unrollType ty with
    | TArray(eltyp,_,_,_) -> elem_type eltyp
    | _ -> assert false

let direct_pointed_type ty =
  match unrollType ty with
    | TPtr(elemty,_) -> elemty
    | _ -> assert false

let pointed_type ty =
  match unrollType (direct_pointed_type ty) with
    | TArray _ as arrty -> element_type arrty
    | ty -> ty

(* ************************************************************************** *)
(** {2 Predefined} *)
(* ************************************************************************** *)

let is_cea_function name =
  (String.length name >= 4 &&
    name.[0] = 'C' && name.[1] = 'E' &&
      name.[2] = 'A' && name.[3] = '_') ||
    ((String.length name >= 17) &&
	( (String.sub name 0 17) = "Frama_C_show_each"))

let is_cea_alloc_with_validity name = name = "Frama_C_alloc_size"
let is_cea_dump_function name = name = "CEA_DUMP" || name = "Frama_C_dump_each"

let is_frama_c_builtin n =
  is_cea_dump_function n ||
    is_cea_function n ||
    is_cea_alloc_with_validity n

let () = Cil.add_special_builtin_family is_frama_c_builtin

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(* $Id: ast_info.ml,v 1.36 2008/11/18 16:37:29 uid562 Exp $ *)

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
  match stripInfo e with
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

let constant_expr i = Const(CInt64(i,IInt,None))

let rec is_null_expr e = match stripInfo e with
  | Const c when is_integral_const c ->
      value_of_integral_const c = Int64.zero
  | CastE(_,e) -> is_null_expr e
  | _ -> false

let rec is_non_null_expr e = match stripInfo e with
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
  | AAssert (_,a,_)
  | AAssume a -> is_trivial_named_predicate a
  | APragma _ | AStmtSpec _ | AInvariant _ | AVariant _ | AAssigns _ -> false

let is_trivial_rooted_assertion = function
  | User ca | AI(_, ca) -> is_trivial_annotation ca.annot_content
  | WP _ -> false

let behavior_postcondition b =
  let assumes =
    Logic_const.pold
      (Logic_const.pands (List.map Logic_const.pred_of_id_pred b.b_assumes))
  in
  let postcondition =
    Logic_const.pands (List.map Logic_const.pred_of_id_pred b.b_ensures)
  in
  Logic_const.pimplies (assumes,postcondition)

let merge_assigns l =
  List.fold_left (fun a b -> Logic_const.merge_assigns a b.b_assigns) [] l

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

let lift_annot_func f x a = match before_after_content a with
  | WP _ -> x
  | User p | AI (_,p) -> f p

let lift_annot_list_func f l =
  let add l x = match before_after_content x with
    | WP _ -> l
    | User p | AI(_,p) -> p :: l
  in
  let l' = List.fold_left add [] l in
  f (List.rev l')

module Datatype_Annotation =
  Project.Datatype.Imperative
    (struct
       type t = rooted_code_annotation before_after
       let copy _ = assert false (* TODO *)
       let name = "rooted_code_annotation before_after"
     end)

(* ************************************************************************** *)
(** {2 Statements} *)
(* ************************************************************************** *)

let is_loop_statement s = match s.skind with Loop _ -> true | _ -> false

let get_sid s = match s with
  | Kglobal -> assert false
  | Kstmt s -> s.sid

(** Returns the location of a [Cil_types.stmt].
    In case of a [Block] returns the location of its first localized
    statement.*)
let rec loc_stmt s = match s.skind with
| Instr i -> get_instrLoc i
| Block {bstmts=s::_} | UnspecifiedSequence ((s,_,_)::_) -> loc_stmt s
| Return (_,location)
| Goto (_,location)
| Break location
| Continue location
| If (_,_,_,location)
| Switch (_,_,_,location)
| Loop (_,_, location,_,_)
| TryFinally (_,_,location)
| TryExcept (_,_,_,location) -> location
| Block {bstmts=[]} | UnspecifiedSequence [] -> locUnknown

let mkassign lv e loc = Set(lv,e,loc)

let mkassign_statement lv e loc = mkStmt (Instr(mkassign lv e loc))

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

let array_type ?length ?(attr=[]) ty = TArray(ty,length,attr)

let direct_array_size ty =
  match unrollType ty with
    | TArray(_ty,Some size,_attr) -> value_of_integral_expr size
    | TArray(_ty,None,_attr) -> 0L
    | _ -> assert false

let rec array_size ty =
  match unrollType ty with
    | TArray(elemty,Some _e,_attr) ->
	if isArrayType elemty then
	  Int64.mul (direct_array_size ty) (array_size elemty)
	else direct_array_size ty
    | TArray(_ty,None,_attr) -> 0L
    | _ -> assert false

let direct_element_type ty = match unrollType ty with
  | TArray(eltyp,_,_) -> eltyp
  | _ -> assert false

let element_type ty =
  let rec elem_type ty = match unrollType ty with
    | TArray(eltyp,_,_) -> elem_type eltyp
    | _ -> ty
  in
  match unrollType ty with
    | TArray(eltyp,_,_) -> elem_type eltyp
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
    ((String.length name >= 18) &&
	( (String.sub name 0 18) = "Frama_C_show_each_"))

let is_cea_alloc name = name = "Frama_C_alloc_infinite"
let is_cea_alloc_with_validity name = name = "Frama_C_alloc_size"
let name_cea_offset = "Frama_C_offset"
let is_cea_offset name =  name = name_cea_offset
let is_cea_dump_function name = name = "CEA_DUMP" || name = "Frama_C_dump_each"
let is_frama_c_base_aligned name = name = "Frama_C_is_base_aligned"

let is_frama_c_builtin n =
  is_cea_dump_function n ||
    is_cea_function n ||
    is_cea_alloc n ||
    is_cea_alloc_with_validity n ||
    is_cea_offset n ||
    is_frama_c_base_aligned n

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)

(**************************************************************************)
(*                                                                        *)
(*  This file is part of the Frama-C's E-ACSL plug-in.                    *)
(*                                                                        *)
(*  Copyright (C) 2012-2018                                               *)
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

module RTL = Functions.RTL
open Cil_types
open Cil_datatype

(* ************************************************************************** *)
(** {2 Handling the E-ACSL's C-libraries, part I} *)
(* ************************************************************************** *)

let library_files () =
  List.map
    (fun d -> Options.Share.file ~error:true d)
    [ "e_acsl_gmp_api.h";
      "e_acsl.h" ]

let normalized_library_files =
  lazy (List.map Datatype.Filepath.of_string (library_files ()))

let is_library_loc (loc, _) =
  List.mem loc.Filepath.pos_path (Lazy.force normalized_library_files)

let library_functions = Datatype.String.Hashtbl.create 17
let register_library_function vi =
  Datatype.String.Hashtbl.add library_functions vi.vname vi

let reset () = Datatype.String.Hashtbl.clear library_functions

(* ************************************************************************** *)
(** {2 Builders} *)
(* ************************************************************************** *)

exception Unregistered_library_function of string
let get_lib_fun_vi fname =
  try Datatype.String.Hashtbl.find library_functions fname
    with Not_found ->
      try Builtins.find fname
      with Not_found ->
        (* could not happen in normal mode, but could be raised when E-ACSL is
           used as a library *)
        raise (Unregistered_library_function fname)

let mk_call ~loc ?result fname args =
  let vi = get_lib_fun_vi fname in
  let f = Cil.evar ~loc vi in
  vi.vreferenced <- true;
  let make_args args ty_params =
    List.map2
      (fun (_, ty, _) arg ->
        let e =
          match ty, Cil.unrollType (Cil.typeOf arg), arg.enode with
          | TPtr _, TArray _, Lval lv -> Cil.new_exp ~loc (StartOf lv)
          | TPtr _, TArray _, _ -> assert false
          | _, _, _ -> arg
        in
        Cil.mkCast ~force:false ~newt:ty ~e)
      ty_params
      args
  in
  let args = match vi.vtype with
    | TFun(_, Some params, _, _) -> make_args args params
    | TFun(_, None, _, _) -> []
    | _ -> assert false
  in
  Cil.mkStmtOneInstr ~valid_sid:true (Call(result, f, args, loc))

let mk_deref ~loc lv =
  Cil.new_exp ~loc (Lval(Mem(lv), NoOffset))

type annotation_kind =
  | Assertion
  | Precondition
  | Postcondition
  | Invariant
  | RTE

let kind_to_string loc k =
  Cil.mkString
    ~loc
    (match k with
    | Assertion -> "Assertion"
    | Precondition -> "Precondition"
    | Postcondition -> "Postcondition"
    | Invariant -> "Invariant"
    | RTE -> "RTE")

(* Build a C conditional doing a runtime assertion check. *)
let mk_e_acsl_guard ?(reverse=false) kind kf e p =
  let loc = p.pred_loc in
  let msg =
    Kernel.Unicode.without_unicode
      (Format.asprintf "%a@?" Printer.pp_predicate) p
  in
  let line = (fst loc).Filepath.pos_lnum in
  let e =
    if reverse then e else Cil.new_exp ~loc:e.eloc (UnOp(LNot, e, Cil.intType))
  in
  mk_call
    ~loc
    (RTL.mk_api_name "assert")
    [ e;
      kind_to_string loc kind;
      Cil.mkString ~loc (RTL.get_original_name kf);
      Cil.mkString ~loc msg;
      Cil.integer loc line ]

let mk_block prj stmt b =
  let mk b = match b.bstmts with
    | [] ->
      (match stmt.skind with
      | Instr(Skip _) -> stmt
      | _ -> assert false)
    | [ s ] -> s
    |  _ :: _ -> Cil.mkStmt ~valid_sid:true (Block b)
  in
  Project.on prj mk b

(* ************************************************************************** *)
(** {2 Handling \result} *)
(* ************************************************************************** *)

let result_lhost kf =
  let stmt =
    try Kernel_function.find_return kf
    with Kernel_function.No_Statement -> assert false
  in
  match stmt.skind with
  | Return(Some { enode = Lval (lhost, NoOffset) }, _) -> lhost
  | _ -> assert false

let result_vi kf = match result_lhost kf with
  | Var vi -> vi
  | Mem _ -> assert false

(* ************************************************************************** *)
(** {2 Handling the E-ACSL's C-libraries, part II} *)
(* ************************************************************************** *)

let mk_full_init_stmt ?(addr=true) vi =
  let loc = vi.vdecl in
  let mk = mk_call ~loc (RTL.mk_api_name "full_init") in
  match addr, Cil.unrollType vi.vtype with
  | _, TArray(_,Some _, _, _) | false, _ -> mk [ Cil.evar ~loc vi ]
  | _ -> mk [ Cil.mkAddrOfVi vi ]

let mk_initialize ~loc (host, offset as lv) = match host, offset with
  | Var _, NoOffset -> mk_call ~loc
    (RTL.mk_api_name "full_init")
    [ Cil.mkAddrOf ~loc lv ]
  | _ ->
    let typ = Cil.typeOfLval lv in
    mk_call ~loc
      (RTL.mk_api_name "initialize")
      [ Cil.mkAddrOf ~loc lv; Cil.new_exp loc (SizeOf typ) ]

let mk_named_store_stmt name ?str_size vi =
  let ty = Cil.unrollType vi.vtype in
  let loc = vi.vdecl in
  let store = mk_call ~loc (RTL.mk_api_name name) in
  match ty, str_size with
  | TArray(_, Some _,_,_), None ->
    store [ Cil.evar ~loc vi ; Cil.sizeOf ~loc ty ]
  | TPtr(TInt(IChar, _), _), Some size -> store [ Cil.evar ~loc vi ; size ]
  | _, None -> store [ Cil.mkAddrOfVi vi ; Cil.sizeOf ~loc ty ]
  | _, Some _ -> assert false

let mk_store_stmt ?str_size vi =
  mk_named_store_stmt "store_block" ?str_size vi

let mk_duplicate_store_stmt ?str_size vi =
  mk_named_store_stmt "store_block_duplicate" ?str_size vi

let mk_delete_stmt vi =
  let loc = vi.vdecl in
  let mk = mk_call ~loc (RTL.mk_api_name "delete_block") in
  match Cil.unrollType vi.vtype with
  | TArray(_, Some _, _, _) -> mk [ Cil.evar ~loc vi ]
  | _ -> mk [ Cil.mkAddrOfVi vi ]

let mk_mark_readonly vi =
  let loc = vi.vdecl in
  mk_call ~loc (RTL.mk_api_name "mark_readonly") [ Cil.evar ~loc vi ]

(* ************************************************************************** *)
(** {2 Other stuff} *)
(* ************************************************************************** *)

let term_addr_of ~loc tlv ty =
  Logic_const.taddrof ~loc tlv (Ctype (TPtr(ty, [])))

let reorder_ast () =
  let ast = Ast.get() in
  let is_from_library = function
    | GType(ti, _) when ti.tname = "size_t"
      || RTL.is_rtl_name ti.tname -> true
    | GCompTag (ci, _) when RTL.is_rtl_name ci.cname -> true
    | GFunDecl(_, _, loc) | GVarDecl(_, loc) when is_library_loc loc -> true
    | _ -> false in
  let rtl, other = List.partition is_from_library ast.globals in
  ast.globals <- rtl @ other

let cty = function
  | Ctype ty -> ty
  | lty -> Options.fatal "Expecting a C type. Got %a" Printer.pp_logic_type lty

let rec ptr_index ?(loc=Location.unknown) ?(index=(Cil.zero loc)) exp =
  let arith_op = function
    | MinusPI -> MinusA
    | PlusPI -> PlusA
    | IndexPI -> PlusA
    | _ -> assert false in
  match exp.enode with
  | BinOp(op, lhs, rhs, _) ->
    (match op with
    (* Pointer arithmetic: split pointer and integer parts *)
    | MinusPI | PlusPI | IndexPI ->
      let index = Cil.mkBinOp exp.eloc (arith_op op) index rhs in
      ptr_index ~index lhs
    (* Other arithmetic: treat the whole expression as pointer address *)
    | MinusPP | PlusA | MinusA | Mult | Div | Mod
    | BAnd | BXor | BOr | Shiftlt | Shiftrt
    | Lt | Gt | Le | Ge | Eq | Ne | LAnd | LOr -> (exp, index))
  | CastE _ -> ptr_index ~loc ~index (Cil.stripCasts exp)
  | Info (exp, _) -> ptr_index ~loc ~index exp
  | Const _ | StartOf _ | AddrOf _ | Lval _ | UnOp _ -> (exp, index)
  | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _
    -> assert false

let term_of_li li =  match li.l_body with
| LBterm t -> t
| LBnone | LBreads _ | LBpred _ | LBinductive _ ->
  Options.fatal "li.l_body does not match LBterm(t) in Misc.term_of_li"

let is_set_of_ptr_or_array lty =
  if Logic_const.is_set_type lty then
    let lty = Logic_const.type_of_element lty in
    Logic_utils.isLogicPointerType lty || Logic_utils.isLogicArrayType lty
  else
    false

exception Range_found_exception
let is_range_free t =
  try
    let has_range_visitor = object inherit Visitor.frama_c_inplace
      method !vterm t = match t.term_node with
      | Trange _ -> raise Range_found_exception
      | _ -> Cil.DoChildren
    end
    in
    ignore (Visitor.visitFramacTerm has_range_visitor t);
    true
  with Range_found_exception ->
    false

let is_bitfield_pointers lty =
  let is_bitfield_pointer = function
    | Ctype typ ->
      begin match Cil.unrollType typ with
        | TPtr(typ, _) ->
          let attrs = Cil.typeAttrs typ in
          Cil.hasAttribute Cil.bitfield_attribute_name attrs
        | _ ->
          false
      end
    | Ltype _ | Lvar _ | Linteger | Lreal | Larrow _ ->
      false
  in
  if Logic_const.is_set_type lty then
    is_bitfield_pointer (Logic_const.type_of_element lty)
  else
    is_bitfield_pointer lty

exception Lv_from_vi_found
let term_has_lv_from_vi t =
  try
    let o = object inherit Visitor.frama_c_inplace
      method !vlogic_var_use lv = match lv.lv_origin with
      | None -> Cil.DoChildren
      | Some _ -> raise Lv_from_vi_found
    end
    in
    ignore (Visitor.visitFramacTerm o t);
    false
  with Lv_from_vi_found ->
    true

type pred_or_term = PoT_pred of predicate | PoT_term of term

let mk_ptr_sizeof typ loc =
  match Cil.unrollType typ with
  | TPtr (t', _) -> Cil.new_exp ~loc (SizeOf t')
  | _ -> assert false

let finite_min_and_max i = match Ival.min_and_max i with
  | Some min, Some max -> min, max
  | None, _ | _, None -> assert false

(*
Local Variables:
compile-command: "make"
End:
*)

(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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
open Cil


(* --- Cil builders --- *)

let function_declaration ~loc name typ mk_spec =
  (* Build the varinfo *)
  let vi = makeGlobalVar (Cabs2cil.fresh_global name) typ in
  vi.vdecl <- loc;
  (* Build the formals *)
  setFormalsDecl vi typ;
  let formals = getFormalsDecl vi in
  let spec = mk_spec formals in
  (* Build the declaration statement *)
  let glob = GFunDecl (spec, vi, vi.vdecl) in
  vi, glob

let vi_assign ~loc vi exp =
  let instr = Set((Var vi, NoOffset), exp, loc) in
  Cil.mkStmtOneInstr ~valid_sid:true instr

let array_assign ~loc vi values =
  let assign_cell i exp =
    let instr = Set((Var vi, Index (Cil.integer ~loc i, NoOffset)), exp, loc)
    in Cil.mkStmtOneInstr ~valid_sid:true instr
  in
  List.mapi assign_cell values

let array_init ~loc fundec scope name elem_typ values =
  let size = max (List.length values) 1 in (* In C, Array size >= 1 *)
  let esize = Cil.integer ~loc size in
  let typ = TArray (elem_typ, Some esize, Cil.empty_size_cache (), []) in
  let vi = Cil.makeLocalVar fundec ~scope name typ in
  vi, array_assign loc vi values

let call ~loc lval callee args =
  let instr = Call (lval, (Cil.evar ~loc callee), args, loc) in
  Cil.mkStmtOneInstr ~valid_sid:true instr


(* --- Logic builders --- *)

let logic_elval ~loc lval =
  Logic_const.term ~loc (TLval lval) (typeOfTermLval lval)

let logic_var vi =
  TVar (Cil.cvar_to_lvar vi), TNoOffset

let logic_evar ~loc vi =
  logic_elval ~loc (TVar (Cil.cvar_to_lvar vi), TNoOffset)

let logic_varmem ~loc vi =
  TMem (logic_evar ~loc vi), TNoOffset

let logic_varfield ~loc vi fieldinfo =
  TMem (logic_evar ~loc vi), TField (fieldinfo, TNoOffset)

let logic_varrange ~loc vi =
  let tstart = Some (Logic_const.tint ~loc Integer.zero)
  and tend = None in
  let range = Logic_const.trange ~loc (tstart, tend) in
(*  TVar (Cil.cvar_to_lvar vi), TIndex (range, TNoOffset) *)
  let binop = Logic_const.term ~loc
    (TBinOp (IndexPI, logic_evar ~loc vi, range)) (Ctype vi.vtype) in
  TMem binop, TNoOffset

let logic_return typ =
  TResult typ, TNoOffset

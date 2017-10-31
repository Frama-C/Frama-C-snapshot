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
open Cil


(* --- Cil builders --- *)

let function_declaration ?vattr ~loc name typ mk_spec =
  (* Build the varinfo *)
  let vi = makeGlobalVar name typ in
  Extlib.may (fun extra_vattr -> vi.vattr <- vi.vattr @ extra_vattr) vattr;
  vi.vdecl <- loc;
  (* Build the formals *)
  setFormalsDecl vi typ;
  let formals = getFormalsDecl vi in
  let spec = mk_spec formals in
  (* Build the declaration statement *)
  let glob = GFunDecl (spec, vi, vi.vdecl) in
  vi, glob

let vi_init ~loc vi exp = Local_init(vi, AssignInit (SingleInit exp), loc)

let array_init ~loc fundec scope name elem_typ values =
  let size = max (List.length values) 1 in (* In C, Array size >= 1 *)
  let esize = Cil.integer ~loc size in
  let typ = TArray (elem_typ, Some esize, Cil.empty_size_cache (), []) in
  let vi = Cil.makeLocalVar fundec ~scope name typ in
  let initl =
    match values with
      | [] -> [ Index (Cil.zero ~loc, NoOffset), Cil.makeZeroInit ~loc elem_typ]
      | _ ->
        List.mapi
          (fun i exp -> Index (Cil.integer ~loc i, NoOffset), SingleInit exp)
          values
  in
  vi.vdefined <- true;
  vi, Local_init(vi, AssignInit(CompoundInit(typ,initl)), loc)

let call ~loc lval callee args =
  let instr = Call (lval, (Cil.evar ~loc callee), args, loc) in
  Cil.mkStmtOneInstr ~valid_sid:true instr


(* --- Logic builders --- *)

let lvar vi =
  TVar (Cil.cvar_to_lvar vi), TNoOffset

let tlval ~loc lval =
  Logic_const.term ~loc (TLval lval) (typeOfTermLval lval)

let tvar ~loc vi =
  tlval ~loc (lvar vi)

let tvarmem ~loc vi =
  TMem (tvar ~loc vi), TNoOffset

let tvarfield ~loc vi fieldinfo =
  TMem (tvar ~loc vi), TField (fieldinfo, TNoOffset)

let tresult typ =
  TResult typ, TNoOffset

let tzero ~loc = Cil.lzero ~loc ()
let tone ~loc = Cil.lone ~loc ()

let tbinop ~loc binop t1 t2 =
  Logic_const.term ~loc (TBinOp (binop, t1, t2)) t1.term_type

let tminus ~loc t1 t2 =
  tbinop ~loc MinusA t1 t2

let tplus ~loc t1 t2 =
  tbinop ~loc PlusA t1 t2

let trange ~loc tstart tend =
  Logic_const.trange ~loc (tstart, tend)

let trange_from_vi ~loc vi =
  let var = tvar ~loc vi
  and range = trange ~loc (Some (tzero ~loc)) None in
  TMem (tbinop IndexPI ~loc var range), TNoOffset

exception NotAFunction

let tapp ~loc logic_info labels args =
  let ltyp = match logic_info.l_type with
  | None -> raise NotAFunction
  | Some ltyp -> ltyp
  in
  Logic_const.term ~loc (Tapp (logic_info, labels, args)) ltyp

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
open Options
module List = Extends.List
module Typ = Extends.Typ
module Build = Va_build


(* Types of variadic parameter and argument *)

let vpar_typ attr =
  TPtr (TPtr (TVoid [], [Attr ("const", [])]), attr)
let vpar_name = "__va_params"
let vpar =
  (vpar_name, vpar_typ [], [])


(* Translation of variadic types (not deeply) *)

let translate_type = function
| TFun (ret_typ, args, is_variadic, attributes) ->
    let new_args =
      if is_variadic
      then Some (Cil.argsToList args @ [vpar])
      else args
    in
    TFun (ret_typ, new_args, false, attributes)      

| TBuiltin_va_list attr -> vpar_typ attr

| typ -> typ


(* Adding the vpar parameter to variadic functions *)

let add_vpar vi =
  let formals = Cil.getFormalsDecl vi in
  (* Add the vpar formal once *)
  if not (List.exists (fun vi -> vi.vname = vpar_name) formals) then
  begin
    (* Register the new formal *)
    let new_formal = Cil.makeFormalsVarDecl vpar in
    let new_formals = formals @ [new_formal] in
    Cil.unsafeSetFormalsDecl vi new_formals
  end


(* Translation of va_* builtins  *)

let translate_va_builtin caller inst =
  let vi, args, loc = match inst with
  | Call(_, {enode = Lval(Var vi, _)}, args, loc) ->
      vi, args, loc
  | _ -> assert false
  in

  let translate_va_start () =
    let va_list = match args with
    | [{enode=Lval va_list}] -> va_list
    | _ -> Self.fatal "Unexpected arguments to va_start"
    and varg =
      try Extlib.last (Cil.getFormalsDecl caller.svar)
      with Invalid_argument _ -> Self.abort
        "Using va_start macro in a function which is not variadic."
    in
    [ Set (va_list, Cil.evar ~loc varg, loc) ]
  in

  let translate_va_copy () =
    let dest, src = match args with
    | [{enode=Lval dest}; src] -> dest, src
    | _ -> Self.fatal "Unexpected arguments to va_copy"
    in
    [ Set (dest, src, loc) ]
  in

  let translate_va_arg () =
    let va_list, typ, lval = match args with
    | [{enode=Lval va_list};
       {enode=SizeOf typ};
       {enode=CastE(_, {enode=AddrOf lval})}] -> va_list, typ, lval
    | _ -> Self.fatal "Unexpected arguments to va_arg"
    in
    (* Check validity of type *)
    if Cil.isIntegralType typ then begin
      let promoted_type = Cil.integralPromotion typ in
      if promoted_type <> typ then
        Self.warning ~current:true
          "Wrong type argument in va_start: %a is promoted to %a when used \
           in the variadic part of the arguments. (You should pass %a to \
           va_start)"
          Printer.pp_typ typ
          Printer.pp_typ promoted_type
          Printer.pp_typ promoted_type
    end;
    (* Build the replacing instruction *)
    let mk_lval_exp lval = Cil.new_exp ~loc (Lval lval)  in
    let mk_mem exp = mk_lval_exp (Cil.mkMem ~addr:exp ~off:NoOffset) in
    let mk_cast exp typ = Cil.mkCast ~force:false ~e:exp ~newt:typ in 
    let src = mk_mem (mk_cast (mk_mem (mk_lval_exp va_list)) (TPtr (typ,[])))
    in
    [ Set (lval, src, loc);
      Set (va_list, Cil.increm (mk_lval_exp va_list) 1, loc) ]
  in

  begin match vi.vname with
  | "__builtin_va_start" -> translate_va_start ()
  | "__builtin_va_copy" -> translate_va_copy ()
  | "__builtin_va_arg" -> translate_va_arg ()
  | "__builtin_va_end" -> [] (* No need to do anything for va_end *)
  | _ -> assert false
  end


(* Translation of calls to variadic functions *)

let translate_call ~fundec block loc mk_call callee pars =

  (* Log translation *)
  Self.result ~current:true ~level:2
    "Generic translation of call to variadic function.";

  (* Split params into static and variadic part *)
  let static_size = List.length (Typ.params (Cil.typeOf callee)) - 1 in
  let s_exps, v_exps = List.break static_size pars in

  (* Create temporary variables to hold parameters *)
  let add_var i exp =
    let typ = Cil.typeOf exp
    and name = "__va_arg" ^ string_of_int i in
    let res = Cil.makeLocalVar fundec ~scope:block name typ in
    res.vdefined <- true;
    res
  in
  let vis = List.mapi add_var v_exps in

  (* Assign parameters to these *)
  let instrs = List.map2 (Build.vi_init ~loc) vis v_exps in

  (* Build an array to store addresses *)
  let addrs = List.map Cil.mkAddrOfVi vis in
  let vargs, assigns = Build.array_init ~loc fundec block
    "__va_args" Cil.voidPtrType addrs
  in
  let instrs = instrs @ [assigns] in

  (* Translate the call *)
  let exp_vargs = Cil.mkAddrOrStartOf ~loc (Cil.var vargs) in
  let new_arg = Cil.mkCast ~force:false ~e:exp_vargs ~newt:(vpar_typ []) in
  let new_args = s_exps @ [new_arg] in
  let call = mk_call callee new_args in
  instrs @ [call]

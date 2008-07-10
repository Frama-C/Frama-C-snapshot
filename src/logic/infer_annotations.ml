(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA   (Commissariat à l'Énergie Atomique)                           *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Cil
open Cil_types
open Db
open Db_types

let tsets_to_tsets =
  function [] -> [Nothing]
    | l -> List.map (fun x -> Location x) l

let assigns_from_prototype vi =
  (*Format.printf "looking for %s prototype@." vi.vname;*)
  let formals = try let formals = getFormalsDecl vi.vid in
  (* Do ignore anonymous names *)
  List.filter (fun vi -> vi.vname <> "") formals
  with Not_found -> [] (* this may happen for function pointer used as formal parameters.*)
  in
  let rtyp, _, _, _ = splitFunctionTypeVI vi in
  let pointer_args,basic_args =
    List.partition (fun vi -> isPointerType vi.vtype) formals in
  (* Remove pointer to pointer types and pointer to void *)
  let pointer_args =
    List.filter (fun vi -> not (isVoidPtrType vi.vtype 
                                || isPointerType (typeOf_pointed vi.vtype))) pointer_args
  in
  let get_length full_typ =
    let attr = typeAttr full_typ in
    findAttribute "arraylen" attr
  in

  let mk_star_v v =
    let typ = unrollType v.vtype in
    match get_length typ with
        [AInt length] ->
(*           let loc = vi.vdecl.byte, vi.vdecl.byte+1 in *)
          let wrap_const c =
            {term_node = TConst (CInt64 (Int64.of_int c,IUInt,None));
             term_loc = vi.vdecl;
             term_name = [];
             term_type = Ctype uintType;}
          in
          Logic_const.new_location
            (TSSingleton(
               TSLval(
                 TSMem(
                   TSAdd_range(TSLval(TSVar(cvar_to_lvar v),TSNo_offset),
                               Some (wrap_const 0),
                               Some (wrap_const (length - 1)))),TSNo_offset)))
      | _ ->
(*           let loc = vi.vdecl.byte, vi.vdecl.byte+1 in *)
          let cell = TSLval((TSVar(cvar_to_lvar v),TSNo_offset)) in
          Logic_const.new_location
            (TSSingleton(TSLval(TSMem cell,TSNo_offset)))
  in
  let to_assign =
    List.map
      mk_star_v
      (List.filter
         (fun v ->
            let pointed_type = typeOf_pointed v.vtype in
            not (hasAttribute "const" (typeAttrs pointed_type))
            && not (Cil.isVoidType pointed_type)
         )
         pointer_args)
  in
  let pointer_args_content =
    List.map
      mk_star_v
      pointer_args
  in
  let inputs =
    tsets_to_tsets
      (pointer_args_content
       @(List.map
           (fun v ->
              Logic_const.new_location
                (TSSingleton(TSLval(TSVar (cvar_to_lvar v),TSNo_offset))))
           basic_args))
  in
  let arguments =
    List.map (fun content -> Location content, inputs) to_assign
  in
  let deps =
    match rtyp with
    | TVoid _ -> (* assigns all pointer args from basic args and content of pointer args *)
        arguments
    | _ -> (* assigns result from basic args and content of pointer args *)
        (Location
           (Logic_const.new_location
              (TSSingleton(
                 TSLval(TSResult,TSNo_offset)))), inputs)::arguments
  in
  match deps with [] -> [Nothing,[]] | l -> l

let populate_funspec kf =
  assert (not (Kernel_function.is_definition kf));
  let name = Kernel_function.get_name kf in
  let assigns = assigns_from_prototype (Kernel_function.get_vi kf) in
  let set_assigns behavior = match behavior.b_assigns with
    | [] ->
	if not (Ast_info.is_frama_c_builtin name) then begin
          let pretty_behavior = if behavior.b_name = "default" then "" else
            " for behavior " ^ behavior.b_name
          in
          CilE.log_once
            "No code for function %a, default assigns generated%s"
            Kernel_function.pretty_name kf pretty_behavior;
        end;
	behavior.b_assigns <- assigns
    | _ -> ()
  in
  match kf.spec.spec_behavior with
  | [] ->
      if not (Ast_info.is_frama_c_builtin name) then begin
        CilE.log_once
          "No code for function %a, default assigns generated"
          Kernel_function.pretty_name kf;
      end;
      kf.spec.spec_behavior <- [{b_name = "generated"; b_ensures = [] ;
                                 b_assumes = []; b_assigns = assigns}]
  | _ ->
      List.iter
        set_assigns
        kf.spec.spec_behavior

let () = Kernel_function.populate_spec := populate_funspec

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)

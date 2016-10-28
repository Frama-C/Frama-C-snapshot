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
open Va_types
open Extlib
module Typ = Extends.Typ

(* List of builting function names to translate *)

let va_builtins = [
  "__builtin_va_start";
  "__builtin_va_copy";
  "__builtin_va_arg";
  "__builtin_va_end"]


(* In place visitor for translation *)

let translate_variadics (file : file) =
  (* Environment filled with global symbols. *)
  let env = Environment.from_file file in

  (* Table associating varinfo of variadic functions to a variadic_function 
     description *)
  let module Table = Cil_datatype.Varinfo.Hashtbl in
  let classification : variadic_function Table.t = Table.create 17 in
  let v = object
    inherit Cil.nopCilVisitor

    method! vglob glob =
      begin match glob with
      | GFunDecl(_, vi, _) | GFun ({svar = vi}, _) ->
          if not (Table.mem classification vi) then begin
            let vf = Classify.classify env vi in
            may (Table.add classification vi) vf
          end;
          Cil.SkipChildren
      | _ ->
          Cil.SkipChildren
      end
  end
  in
  Cil.visitCilFile v file;

  (* The translating visitor *)
  let v = object (self)
    inherit Cil.nopCilVisitor

    method! vtype _typ =
      Cil.DoChildrenPost (Generic.translate_type)

    (* Translate types and signatures *)
    method! vglob glob =
      begin match glob with
      | GFunDecl(_, vi, _) ->
          if Table.mem classification vi then
            Generic.add_vpar vi;
          Cil.DoChildren

      | GFun ({svar = vi} as fundec, _) ->
          if Table.mem classification vi then begin
            Generic.add_vpar vi;
            fundec.sformals <- Cil.getFormalsDecl vi;
          end;
          Standard.new_globals := [];
          Cil.DoChildrenPost (fun globs ->
            List.rev (globs @ !Standard.new_globals))

      | _ ->
          Cil.DoChildren
      end

    (* Replace variadic calls *)
    method! vstmt s =
      let fundec = the self#current_func in
      begin match s.skind with
      | Instr(Call(_, {enode = Lval(Var vi, _)}, _, _)) ->
          begin try           
            let vf = Table.find classification vi in
            let s' = try 
              let call_translator = match vf.vf_class with
              | Overload o -> Standard.overloaded_call ~fundec o
              | Aggregator a -> Standard.aggregator_call ~fundec a
              | FormatFun f -> Standard.format_fun_call ~fundec env f
              | _ -> raise Standard.Translate_call_exn
              in
              call_translator vf s
            with Standard.Translate_call_exn ->
              Generic.translate_call ~fundec s
            in
            File.must_recompute_cfg fundec;
            Cil.ChangeTo (s')
          with Not_found ->
            Cil.DoChildren
          end

      | Instr(Call(_, callee, _, _)) ->
          let is_variadic =
            try
              let last = Extends.List.last (Typ.params (Cil.typeOf callee)) in
               last = Generic.vpar
            with Extends.List.EmptyList -> false
          in
          if is_variadic then begin
            let s' = Generic.translate_call fundec s in
            File.must_recompute_cfg fundec;
            Cil.ChangeTo (s')
          end else
            Cil.DoChildren

      | _-> Cil.DoChildren
      end

    (* Replace va__* builtins *)
    method! vinst inst =
      let fundec = the self#current_func in
      begin match inst with
      | Call(_, {enode = Lval(Var vi, _)}, _, _)
            when List.mem vi.vname va_builtins ->
          File.must_recompute_cfg fundec;
          Cil.ChangeTo (Generic.translate_va_builtin fundec inst)
      | _ ->
          Cil.DoChildren
      end
  end
  in
  Cil.visitCilFile v file

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
open Va_types
open Options
open Extlib
module Typ = Extends.Typ

(* List of builtin function names to translate *)

let va_builtins = [
  "__builtin_va_start";
  "__builtin_va_copy";
  "__builtin_va_arg";
  "__builtin_va_end"]

let is_framac_builtin vi =
  Ast_info.is_frama_c_builtin vi.vname ||
  Extlib.string_prefix "__FRAMAC_" vi.vname (* Mthread prefixes *)


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
      | GFunDecl(_, vi, _) | GFun ({svar = vi}, _)
            when not (is_framac_builtin vi) ->
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

    val curr_block = Stack.create ()

    method! vblock b =
      Stack.push b curr_block;
      Cil.DoChildrenPost (fun b -> ignore (Stack.pop curr_block); b)

    method private enclosing_block () =
      try Stack.top curr_block
      with Stack.Empty -> Options.Self.fatal "No enclosing block here"

    method! vtype _typ =
      Cil.DoChildrenPost (Generic.translate_type)

    (* Translate types and signatures *)
    method! vglob glob =
      begin match glob with
      | GFunDecl(_, vi, _) when is_framac_builtin vi ->
          Self.result ~level:2 ~current:true
            "Variadic builtin %s left untransformed." vi.vname;
          Cil.SkipChildren

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

    method! vstmt s =
      match s.skind with
      | Instr (Call _) ->
        (* Separate locals created by a variadic call in their own block.
           This can't be done for Local_init(x,ConsInit _,_), as this
           instruction must be kept a direct child of the enclosing block,
           that determines the scope of x.
        *)
        let block = Cil.mkBlock [] in
        Stack.push block curr_block;
        let keep_block_if_needed s =
          ignore (Stack.pop curr_block);
          match s.skind with
          | Block b' ->
            (* We have introduced several instructions, and potentially locals.
               Scope of locals is in [block], that will replace b'. *)
            block.bstmts <- b'.bstmts;
            s.skind <- Block block;
            s
          | _ -> s
        in
        Cil.DoChildrenPost keep_block_if_needed
      | _ -> Cil.DoChildren

    (* Replace variadic calls *)
    method! vinst i =
      let fundec = the self#current_func in
      let loc = Cil_datatype.Instr.loc i in
      let block = self#enclosing_block () in
      let make_new_args mk_call f args =
        let vf = Table.find classification f in
        try
          let call_translator = match vf.vf_class with
            | Overload o -> Standard.overloaded_call ~fundec o
            | Aggregator a -> Standard.aggregator_call ~fundec a
            | FormatFun f -> Standard.format_fun_call ~fundec env f
            | _ -> raise Standard.Translate_call_exn
          in
          call_translator block loc mk_call vf args
        with Standard.Translate_call_exn ->
          Generic.translate_call
            ~fundec block loc mk_call (Cil.evar ~loc f) args
      in
      begin match i with
      | Call(_, {enode = Lval(Var vi, _)}, _, _)
            when List.mem vi.vname va_builtins ->
          File.must_recompute_cfg fundec;
          Cil.ChangeTo (Generic.translate_va_builtin fundec i)
      | Call(lv, {enode = Lval(Var vi, NoOffset)}, args, loc) ->
        begin
          try
            let mk_call f args = Call (lv, f, args, loc) in
            let res = make_new_args mk_call vi args in
            File.must_recompute_cfg fundec;
            Cil.ChangeTo res
          with Not_found ->
            Cil.DoChildren
        end

      | Call(lv, callee, args, loc) ->
        let is_variadic =
          try
            let last = Extends.List.last (Typ.params (Cil.typeOf callee)) in
            last = Generic.vpar
          with Extends.List.EmptyList -> false
        in
        if is_variadic then begin
          let mk_call f args = Call (lv, f, args, loc) in
          let res =
            Generic.translate_call ~fundec block loc mk_call callee args
          in
          File.must_recompute_cfg fundec;
          Cil.ChangeTo res
        end else
          Cil.DoChildren
      | Local_init(v, ConsInit(c, args, kind), loc) ->
        begin
          try
            let mk_call f args =
              let args =
                match kind, args with
                | Constructor, [] ->
                  Options.Self.fatal
                    "Constructor %a is expected to have at least one argument"
                    Cil_printer.pp_varinfo c
                | Constructor, _::tl -> tl
                | Plain_func, args -> args
              in
              let f =
                match f.enode with
                | Lval (Var f, NoOffset) -> f
                | _ ->
                  Options.Self.fatal
                    "Constructor cannot be translated as indirect call"
              in
              Local_init(v,ConsInit(f,args,kind),loc)
            in
            let args =
              match kind with
                | Plain_func -> args
                | Constructor -> Cil.mkAddrOfVi v :: args
            in
            let res = make_new_args mk_call c args in
            File.must_recompute_cfg fundec;
            Cil.ChangeTo res
          with Not_found ->
            Cil.DoChildren
        end
      | _-> Cil.DoChildren
      end

    method! vexpr exp =
      begin match exp.enode with
      | AddrOf (Var vi, NoOffset)
        when Extends.Cil.is_variadic_function vi && is_framac_builtin vi ->
          Self.not_yet_implemented
            "The variadic plugin doesn't handle calls to a pointer to the \
             variadic builtin %s."
            vi.vname
      | _ -> Cil.DoChildren
      end
  end
  in
  Cil.visitCilFile v file

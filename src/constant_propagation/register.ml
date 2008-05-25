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

open Cil
open Cil_types
exception Cannot_expand

(** This visitor also performs a deep copy. *)
class propagate project fnames = object(self)
  inherit Visitor.frama_c_copy project
  method private on_current_stmt nothing f =
    match self#current_stmt with
    | None -> nothing
    | Some stmt -> f (Kstmt stmt)

  method vfunc fundec =
    let name = fundec.svar.vname in
    if (Cilutil.StringSet.is_empty fnames ||
          Cilutil.StringSet.mem name fnames)
    then
      begin
        Format.printf "[constant propagation] for function %s@." (fundec.svar.vname);
        DoChildren
      end
    else
      SkipChildren

  method vstmt_aux s = match s.skind with
    | Return _ -> SkipChildren
    | _ -> DoChildren

  method vexpr expr =
    self#on_current_stmt
      DoChildren
      (fun ki ->
         if Cmdline.Debug.get () > 0 then
           Format.printf "Replacing %a ?@."
             !Ast_printer.d_exp expr;
         let evaled = !Db.Value.access_expr ki expr in
         try
           let k,m = Cvalue_type.V.find_lonely_binding evaled in
             begin
               match k with
               | Base.Var (vi,_) | Base.Initialized_Var (vi,_) when not vi.vlogic ->
                   (* This is a pointer coming for C code *)
                let base =  mkAddrOrStartOf (var vi) in
                   let offset = Ival.project_int m in (* these are bytes *)
                   let shifted =
                     if Abstract_interp.Int.is_zero offset then base
                     else
                       let sizeof_pointed =
                         try
			   Int_Base.project
			     (if isArrayType vi.vtype then
				Bit_utils.osizeof_pointed vi.vtype
                              else
				Bit_utils.osizeof vi.vtype)
                         with
                         | Int_Base.Error_Top
                         | Int_Base.Error_Bottom -> raise Cannot_expand
                       in
                       increm64
                         base
                         (Abstract_interp.Int.to_int64
                            (if
                               Abstract_interp.Int.is_zero
                                 (Abstract_interp.Int.pos_rem
                                    offset
                                    sizeof_pointed)
                             then (Abstract_interp.Int.pos_div
                                    offset
                                    sizeof_pointed)
                             else raise Cannot_expand
                             ))
                   in
                   ChangeTo (mkCast
                               ~e:shifted
                               ~newt:(typeOf expr))
               | Base.Null ->
                   let e =
                     begin
                       try
                         (* This is an integer *)
                         let v = Ival.project_int m in
                         if Cmdline.Debug.get () > 0 then
                           Format.printf "Replacing %a with %a@."
                             !Ast_printer.d_exp expr
                             Abstract_interp.Int.pretty v;
                         (* Give it the right type ! *)
                         try
                           kinteger64 IULongLong (Abstract_interp.Int.to_int64 v)
                         with Failure _ -> raise Cannot_expand
                       with Ival.Not_Singleton_Int->
			 (* TODO: floats *)
                         raise Cannot_expand
                     end
                   in
                   ChangeTo (mkCast
                               ~e
                               ~newt:(typeOf expr))
               | Base.Cell_class _ | Base.String _
               | Base.Var _ | Base.Initialized_Var _ -> DoChildren

             end
         with Not_found | Cannot_expand -> DoChildren)

  method vlval lv =
    let simplify (host,offs as lv) = match host with
    | Mem e -> mkMem e offs (* canonicalize *)
    | Var _ -> lv
    in ChangeDoChildrenPost(lv, simplify)

end

let run_propagation fnames =
  !Db.Value.compute ();
  let fresh_project = Project.create "propagated" in
  File.init_project_from_visitor
    fresh_project
    (fun prj -> new propagate prj fnames);
  let options = 
    let a o = Project.Selection.add o Kind.Do_Not_Select_Dependencies in
    let add_opt = Project.Selection.empty in
    let add_opt = a Cmdline.MinValidAbsoluteAddress.self add_opt in
    let add_opt = a Cmdline.MaxValidAbsoluteAddress.self add_opt in
    let add_opt = a Cmdline.AutomaticContextMaxDepth.self add_opt in
    let add_opt = a Cmdline.AllocatedContextValid.self add_opt in
    let add_opt = a Cmdline.IgnoreOverflow.self add_opt in
    let add_opt = a Cmdline.UnsafeArrays.self add_opt in
    let add_opt = a Cmdline.LibEntry.self add_opt in
      a Cmdline.MainFunction.self add_opt
  in
  Project.copy ~only:options fresh_project;
  fresh_project

let options =
  [ "-semantic-const-folding",
    Arg.Unit Cmdline.Constant_Propagation.SemanticConstFolding.on,
    ": force semantic constant propagation and pretty print the new source code.";

    "-semantic-const-fold",
    Arg.String Cmdline.Constant_Propagation.SemanticConstFold.add,
    "f : propagate constants in f.";
  ]

let () =
  Db.Constant_Propagation.run_propagation := run_propagation;
  Options.add_plugin
    ~name:"semantic constant folding"
    ~descr:"propagates semantically constants"
    options


(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)

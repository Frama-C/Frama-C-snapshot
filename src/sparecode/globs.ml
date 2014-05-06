(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

let dkey = Sparecode_params.register_category "globs"

let debug format = Sparecode_params.debug ~dkey ~level:2 format
let debug' format = Sparecode_params.debug ~dkey ~level:3 format

let used_variables = Hashtbl.create 257
let var_init = Hashtbl.create 257
let used_typeinfo = Hashtbl.create 257
let used_compinfo = Hashtbl.create 257
let used_enuminfo = Hashtbl.create 257

let clear_tables () =
  Hashtbl.clear used_variables;
  Hashtbl.clear var_init;
  Hashtbl.clear used_typeinfo;
  Hashtbl.clear used_compinfo;
  Hashtbl.clear used_enuminfo

class collect_visitor = object (self)

  inherit Visitor.frama_c_inplace

  method! vtype t = match t with
    | TNamed(ti,_) ->
        (* we use the type name because directe typeinfo comparision
        * doesn't wok. Anyway, CIL renames types if several type have the same
        * name... *)
        if Hashtbl.mem used_typeinfo ti.tname then SkipChildren
        else begin
          debug "add used typedef %s@." ti.tname;
          Hashtbl.add used_typeinfo ti.tname ();
          ignore (visitCilType (self:>Cil.cilVisitor) ti.ttype);
          DoChildren
        end
    | TEnum(ei,_) ->
        if Hashtbl.mem used_enuminfo ei.ename then SkipChildren
        else begin
          debug "add used enum %s@." ei.ename;
          Hashtbl.add used_enuminfo ei.ename (); DoChildren
        end
    | TComp(ci,_,_) ->
        if Hashtbl.mem used_compinfo ci.cname then SkipChildren
        else begin
          debug "add used comp %s@." ci.cname;
          Hashtbl.add used_compinfo ci.cname ();
          List.iter
            (fun f -> ignore (visitCilType (self:>Cil.cilVisitor) f.ftype))
            ci.cfields;
          DoChildren
        end
    | _ -> DoChildren

  method! vvrbl v =
    if v.vglob && not (Hashtbl.mem used_variables v) then begin
      debug "add used var %s@." v.vname;
      Hashtbl.add used_variables v ();
      ignore (visitCilType (self:>Cil.cilVisitor) v.vtype);
      try
        let init = Hashtbl.find var_init v in
          ignore (visitCilInit (self:>Cil.cilVisitor) v NoOffset init)
      with Not_found -> ()
    end;
    DoChildren

  method! vglob_aux g = match g with
    | GFun (f, _) ->
        debug "add function %s@." f.svar.vname;
        Hashtbl.add used_variables f.svar ();
        Cil.DoChildren
    | GAnnot _ -> Cil.DoChildren
    | GVar (v, init, _) ->
        let _ = match init.init with | None -> ()
          | Some init ->
              begin
                Hashtbl.add var_init v init;
                if Hashtbl.mem used_variables v then
                  (* already used before its initialization (see bug #758) *)
                  ignore (visitCilInit (self:>Cil.cilVisitor) v NoOffset init)
              end
        in Cil.SkipChildren
    | GVarDecl(_,v,_) when isFunctionType v.vtype -> DoChildren
    | _ -> Cil.SkipChildren

end

class filter_visitor prj = object

  inherit Visitor.generic_frama_c_visitor (Cil.copy_visit prj)

  method! vglob_aux g =
    match g with
      | GFun (_f, _loc) (* function definition *)
        -> Cil.DoChildren (* keep everything *)
      | GVar (v, _, _loc) (* variable definition *)
      | GVarDecl (_, v, _loc) -> (* variable/function declaration *)
          if Hashtbl.mem used_variables v then DoChildren
          else begin
            debug "remove var %s@." v.vname;
            ChangeTo []
          end
      | GType (ti, _loc) (* typedef *) ->
          if Hashtbl.mem used_typeinfo ti.tname then DoChildren
          else begin
            debug "remove typedef %s@." ti.tname;
            ChangeTo []
          end
      | GCompTag (ci, _loc) (* struct/union definition *)
      | GCompTagDecl (ci, _loc) (* struct/union declaration *) ->
          if Hashtbl.mem used_compinfo ci.cname then DoChildren
          else begin
            debug "remove comp %s@." ci.cname;
            ChangeTo []
          end
      | GEnumTag (ei, _loc) (* enum definition *)
      | GEnumTagDecl (ei, _loc) (* enum declaration *) ->
          if Hashtbl.mem used_enuminfo ei.ename then DoChildren
          else begin
            debug "remove enum %s@." ei.ename;
            DoChildren (* ChangeTo [] *)
          end
      | _ -> Cil.DoChildren
  end

module Result =
  State_builder.Hashtbl
    (Datatype.String.Hashtbl)
    (Project.Datatype)
    (struct
       let name = "Sparecode without unused globals"
       let size = 7
       let dependencies = [ Ast.self ] (* delayed, see below *)
     end)

let () =
  Cmdline.run_after_extended_stage
    (fun () ->
       State_dependency_graph.add_codependencies
         ~onto:Result.self
         [ !Db.Pdg.self; !Db.Outputs.self_external ])

let rm_unused_decl =
  Result.memo
    (fun new_proj_name ->
       clear_tables ();
       let visitor = new collect_visitor in
       Visitor.visitFramacFileSameGlobals visitor (Ast.get ());
       debug "filtering done@.";
       let visitor = new filter_visitor in
       let new_prj = File.create_project_from_visitor new_proj_name visitor in
       let ctx = Parameter_state.get_selection_context () in
       Project.copy ~selection:ctx new_prj;
       new_prj)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

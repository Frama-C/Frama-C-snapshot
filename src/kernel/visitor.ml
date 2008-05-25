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

(* $Id: visitor.ml,v 1.27 2008/05/22 08:12:06 uid562 Exp $ *)

open Cil
open Cil_types
open Db_types

let is_definition v =
  Ast_info.Function.is_definition (Globals.Functions.get v).fundec

(* ************************************************************************* *)
(** {2 Visitors} *)
(* ************************************************************************* *)

(** Class type for a Db-aware visitor. *)
class type frama_c_visitor = object
  inherit Cil.cilVisitor
  method vstmt_aux: Cil_types.stmt -> Cil_types.stmt Cil.visitAction
  method vglob_aux: Cil_types.global -> Cil_types.global list Cil.visitAction
  method vrooted_code_annotation:
    Db_types.rooted_code_annotation ->
    Db_types.rooted_code_annotation list visitAction
  method is_annot_before: bool
end

(** Extension to the cil visitor that is aware of kernel function
    and annotation db. This is done by defining auxiliary methods that can be
    redefined in inherited classes, while the corresponding ones from
    {!Cil.cilVisitor} {b must} retain their values as defined here. Otherwise,
    annotations may not be visited properly. *)
class generic_frama_c_visitor prj behavior: frama_c_visitor =

  let childrenRooted_code_annotation (vis:frama_c_visitor) rca =
    match rca with
      User ca ->
        let ca' = Cil.visitCilCodeAnnotation (vis:>Cil.cilVisitor) ca in
        if ca != ca' then User ca' else rca
    | AI (cause,ca) ->
        let ca' = Cil.visitCilCodeAnnotation (vis:>Cil.cilVisitor) ca in
        if ca != ca' then AI(cause,ca') else rca
    | WP _ -> rca (*TODO Virgile: visitor for WP types ??? *)
  in
  let visitRooted_code_annotation vis ca =
    Cil.doVisitList vis
      (fun x -> x)
      vis#vrooted_code_annotation childrenRooted_code_annotation ca
  in

object(self)
  inherit Cil.genericCilVisitor ~prj behavior
    (* top of the stack indicates if we are before or after the current
       statement. *)
  val before = Stack.create ()

  method is_annot_before =
    Stack.is_empty before (* global annotation *) || Stack.top before

  method vrooted_code_annotation _ = DoChildren

  method private vstmt stmt =
    let annots = Annotations.get stmt in
    let res = self#vstmt_aux stmt in
    let abefore,aafter =
      List.fold_left
        (fun (b,a) x ->
           match x with Before x -> (x::b,a) | After x -> (b,x::a))
        ([],[]) annots
    in
    let make_children_annot () =
      Stack.push true before;
      let abefore' =
        List.filter (fun x -> not (Ast_info.is_trivial_rooted_assertion x))
          (List.flatten
             (List.rev_map (visitRooted_code_annotation (self:>frama_c_visitor))
                abefore))
      in
      ignore (Stack.pop before); Stack.push false before;
      let aafter' =
        List.filter (fun x -> not (Ast_info.is_trivial_rooted_assertion x))
          (List.flatten
             (List.rev_map
                (visitRooted_code_annotation (self:>frama_c_visitor)) aafter))
      in
      ignore(Stack.pop before);
      (abefore',aafter')
    in
    let change_stmt stmt (abefore',aafter') =
      if abefore' <> [] || aafter' <> [] then
        Queue.add
          (fun () ->
	     Annotations.reset_stmt stmt;
	     List.iter (fun x -> (Annotations.add stmt) (Before x)) abefore';
	     List.iter (fun x -> (Annotations.add stmt) (After x)) aafter')
          self#get_filling_actions
    in
    match res with
        SkipChildren -> change_stmt stmt (abefore,aafter); res
      | DoChildren ->
          ChangeDoChildrenPost
            (stmt,
             fun stmt -> change_stmt stmt (make_children_annot()); stmt)
      | ChangeTo _ -> res
      | ChangeDoChildrenPost (stmt,f) ->
          ChangeDoChildrenPost
            (stmt,
             (fun stmt ->
                let annots = make_children_annot() in
                change_stmt stmt annots; f stmt))

  method vstmt_aux _ = DoChildren
  method vglob_aux _ = DoChildren

  method vglob g =
    let res = self#vglob_aux g in
    let make_funspec () =
      match g with
          GVarDecl(_,v,_) when Cil.isFunctionType v.vtype ->
            if not (is_definition v) then begin
	      let kf = Globals.Functions.get v in
	      (* if there is a definition, the spec is visited in
		 the corresponding GFun
	      *)
	      let spec' = visitCilFunspec (self:>Cil.cilVisitor) kf.spec in
              Some spec'
	    end
	    else None
        | GFun(f,_) ->
            let kf = Globals.Functions.get f.svar in
            let spec' = visitCilFunspec (self:>Cil.cilVisitor) kf.spec in
            Some spec'
        | _ -> None
    in
    let get_spec () =
      match g with
          GVarDecl(_,v,_) when Cil.isFunctionType v.vtype ->
            if not (is_definition v) then begin
	      let kf = Globals.Functions.get v in
	      (* if there is a definition, the spec is visited in
		 the corresponding GFun
	      *)
              Some kf.spec
	    end
	    else None
        | GFun(f,_) ->
            let kf = Globals.Functions.get f.svar in
            Some kf.spec
        | _ -> None
    in
    let change_glob ng spec =
      let cond = Cil.is_copy_behavior self#behavior in
      match ng with
        GVar(vi,init,_) ->
          if cond then
            Queue.add (fun () -> Globals.Vars.add vi init)
              self#get_filling_actions
      | GVarDecl(_,v,l) when Cil.isFunctionType v.vtype ->
         if cond &&
	   not (is_definition (Cil.get_original_varinfo self#behavior v)) then
             begin
	       Queue.add
                 (fun () ->
                    Globals.Functions.replace_by_declaration
                      (Extlib.the spec) v l;
                 )
	         self#get_filling_actions
	     end
      | GVarDecl (_,({vstorage=Extern} as v),_) ->
          if cond then
            Queue.add (fun () -> Globals.Vars.add_decl v)
              self#get_filling_actions
      | GFun(f,l) ->
          if cond then begin
	    Queue.add
	      (fun () ->
	         if Cmdline.Debug.get () > 0 then
 		   Format.eprintf
		     "@[Adding definition %s (vid: %d) for project %s@\n\
                      body: %a@\n@]@."
		     f.svar.vname f.svar.vid (Project.name (Project.current()))
                     !Ast_printer.d_block f.sbody
                 ;
	         if is_definition f.svar then
		   failwith "trying to redefine an existing kernel function"
	         else
		   Globals.Functions.replace_by_definition (Extlib.the spec) f l
              )
	      self#get_filling_actions
          end
      | _ -> ()
    in
    match res with
      SkipChildren -> let spec = get_spec() in change_glob g spec; res
    | DoChildren ->
	ChangeDoChildrenPost
	  ([g],
           fun g -> let spec = make_funspec () in
           List.iter (fun g -> change_glob g spec) g; g)
    | ChangeTo _ -> res
    | ChangeDoChildrenPost (g,f) ->
	ChangeDoChildrenPost
	  (g,
           fun g->
             let spec = make_funspec () in
             List.iter (fun g -> change_glob g spec) g; f g)
end

class frama_c_copy prj = generic_frama_c_visitor prj (Cil.copy_visit ())

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)

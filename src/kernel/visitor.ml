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

(* $Id: visitor.ml,v 1.34 2008/11/20 12:47:11 uid562 Exp $ *)

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
  inherit cilVisitor
  method vstmt_aux: Cil_types.stmt -> Cil_types.stmt visitAction
  method vglob_aux: Cil_types.global -> Cil_types.global list visitAction
  method vrooted_code_annotation:
    Db_types.rooted_code_annotation ->
    Db_types.rooted_code_annotation list visitAction
  method is_annot_before: bool
  method current_kf: kernel_function option
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
        let ca' = visitCilCodeAnnotation (vis:> cilVisitor) ca in
        if ca != ca' then User ca' else rca
    | AI (cause,ca) ->
        let ca' = visitCilCodeAnnotation (vis:> cilVisitor) ca in
        if ca != ca' then AI(cause,ca') else rca
    | WP _ -> rca (*TODO Virgile: visitor for WP types ??? *)
  in
  let visitRooted_code_annotation vis ca =
    doVisitList vis
      (fun x -> x)
      vis#vrooted_code_annotation childrenRooted_code_annotation ca
  in

object(self)
  inherit genericCilVisitor ~prj behavior as super
    (* top of the stack indicates if we are before or after the current
       statement. *)
  val before = Stack.create ()

  val mutable current_kf = None

  method private set_current_kf kf = current_kf <- Some kf

  method private reset_current_kf () = current_kf <- None

  method current_kf = current_kf

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
             let current_annots = Annotations.get stmt in
             (* remove all annotations that are physically equal to
                one of the annotations that existed before the visit.
                They will be readded if needed. Keep annotations that
                have been added by visiting the statement itself (by vstmt_aux)
              *)
             let new_annots =
               List.filter
                 (fun x -> not (List.memq x annots)) current_annots
             in
             Annotations.reset_stmt stmt;
             List.iter (Annotations.add stmt) new_annots;
	     List.iter (fun x -> Annotations.add stmt (Before x)) abefore';
	     List.iter (fun x -> (Annotations.add stmt) (After x)) aafter')
          self#get_filling_actions
    in
    match res with
        SkipChildren -> change_stmt stmt (abefore,aafter); res
      | DoChildren ->
          ChangeDoChildrenPost
            (stmt,
             fun stmt -> change_stmt stmt (make_children_annot()); stmt)
      | ChangeTo _ | ChangeToPost _ -> res
      | ChangeDoChildrenPost (stmt,f) ->
          ChangeDoChildrenPost
            (stmt,
             (fun stmt ->
                let annots = make_children_annot() in
                change_stmt stmt annots; f stmt))

  method vstmt_aux _ = DoChildren
  method vglob_aux _ = DoChildren

  method vglob g =
    let has_kf =
      match g with
          GVarDecl(_,v,_) when isFunctionType v.vtype ->
            self#set_current_kf (Globals.Functions.get v); true
        | GFun(f,_) -> self#set_current_kf (Globals.Functions.get f.svar); true
        | _ -> false
    in
    let res = self#vglob_aux g in
    let make_funspec () =
      match g with
          GVarDecl(_,v,_) when isFunctionType v.vtype ->
            if not (is_definition v) then begin
	      let spec' = visitCilFunspec (self:> cilVisitor)
                (Extlib.the current_kf).spec in
              Some spec'
	    end
	    else None
        | GFun _ ->
            let spec' = visitCilFunspec (self:> cilVisitor)
              (Extlib.the current_kf).spec in
            Some spec'
        | _ -> None
    in
    let get_spec () =
      match g with
          GVarDecl(_,v,_) when isFunctionType v.vtype ->
            if not (is_definition v) then begin
              Some (Extlib.the current_kf).spec
	    end
	    else None (* visited in the corresponding definition *)
        | GFun _ -> Some (Extlib.the current_kf).spec
        | _ -> None
    in
    let change_glob ng spec =
      let cond = is_copy_behavior self#behavior in
       match ng with
           GVar(vi,init,_) ->
             if cond then
               Queue.add (fun () -> Globals.Vars.add vi init)
                 self#get_filling_actions
         | GVarDecl(_,v,l) when isFunctionType v.vtype ->
             let spec = match spec with
                 None -> Cil.empty_funspec ()
               | Some spec -> spec
             in
             let orig_spec = (Extlib.the current_kf).spec in
             if cond || (not (Cil.is_empty_funspec spec) &&
                           not (Cil.is_empty_funspec orig_spec) &&
                           spec != orig_spec)
             then
	       Queue.add
                 (fun () ->
                    Globals.Functions.replace_by_declaration spec v l)
	         self#get_filling_actions;

         | GVarDecl (_,({vstorage=Extern} as v),_) ->
             if cond then
               Queue.add (fun () -> Globals.Vars.add_decl v)
                 self#get_filling_actions
         | GFun(f,l) ->
             if cond then begin
               let spec =
                 match spec with
                     None -> Cil.empty_funspec ()
                   | Some spec -> spec
               in
	       Queue.add
	         (fun () ->
	            if Cmdline.Debug.get () > 0 then
 		      Format.eprintf
		        "@[Adding definition %s (vid: %d) for project %s@\n\
                              body: %a@\n@]@."
		        f.svar.vname f.svar.vid
                        (Project.name (Project.current()))
                        !Ast_printer.d_block f.sbody
                    ;
	            if is_definition f.svar then
		      failwith
                        "trying to redefine an existing kernel function"
	            else
		      Globals.Functions.replace_by_definition spec f l
                 )
	         self#get_filling_actions
             end
         | _ -> ()
    in
    match res with
      SkipChildren ->
        change_glob g (get_spec()); if has_kf then self#reset_current_kf(); res
    | DoChildren ->
	ChangeDoChildrenPost
	  ([g],
           fun g -> let spec = make_funspec () in
           List.iter (fun g -> change_glob g spec) g;
           if has_kf then self#reset_current_kf();
           g)
    | ChangeTo l ->
        List.iter (fun g -> change_glob g None) l;
        if has_kf then self#reset_current_kf();
        res
    | ChangeToPost (l,f) ->
        ChangeToPost(l,
                     fun l ->
                       List.iter (fun g -> change_glob g None) l;
                       if has_kf then self#reset_current_kf(); f l)
    | ChangeDoChildrenPost (g,f) ->
	ChangeDoChildrenPost
	  (g,
           fun g->
             let spec = make_funspec () in
             List.iter (fun g -> change_glob g spec) g;
             if has_kf then self#reset_current_kf();
             f g)
end

class frama_c_copy prj = generic_frama_c_visitor prj (copy_visit ())

let visitFramacFileCopy vis f = visitCilFileCopy (vis:>cilVisitor) f

let visitFramacFile vis f = visitCilFile (vis:>cilVisitor) f

let visitFramacFileSameGlobals vis f =
  visitCilFileSameGlobals (vis:>cilVisitor) f

let visitFramacGlobal vis g =
  let g' = visitCilGlobal (vis:>cilVisitor) g in
  vis#fill_global_tables; g'

let visitFramacFunction vis f =
  let f' = visitCilFunction (vis:>cilVisitor) f in
  vis#fill_global_tables; f'

let visitFramacExpr vis e =
  let e' = visitCilExpr (vis:>cilVisitor) e in
  vis#fill_global_tables; e'

let visitFramacLval vis l =
  let l' = visitCilLval (vis:>cilVisitor) l in
  vis#fill_global_tables; l'

let visitFramacOffset vis o =
  let o' = visitCilOffset (vis:>cilVisitor) o in
  vis#fill_global_tables; o'

let visitFramacInitOffset vis o =
  let o' = visitCilInitOffset (vis:>cilVisitor) o in
  vis#fill_global_tables; o'

let visitFramacInstr vis i =
  let i' = visitCilInstr (vis:>cilVisitor) i in
  vis#fill_global_tables; i'

let visitFramacStmt vis s =
  let s' = visitCilStmt (vis:>cilVisitor) s in
  vis#fill_global_tables; s'

let visitFramacBlock vis b =
  let b' = visitCilBlock (vis:>cilVisitor) b in
  vis#fill_global_tables; b'

let visitFramacType vis t =
  let t' = visitCilType (vis:>cilVisitor) t in
  vis#fill_global_tables; t'

let visitFramacVarDecl vis v =
  let v' = visitCilVarDecl (vis:>cilVisitor) v in
  vis#fill_global_tables; v'

let visitFramacInit vis v o i =
  let i' = visitCilInit (vis:>cilVisitor) v o i in
  vis#fill_global_tables; i'

let visitFramacAttributes vis a =
  let a' = visitCilAttributes (vis:>cilVisitor) a in
  vis#fill_global_tables; a'

let visitFramacAnnotation vis a =
  let a' = visitCilAnnotation (vis:>cilVisitor) a in
  vis#fill_global_tables; a'

let visitFramacCodeAnnotation vis c =
  let c' = visitCilCodeAnnotation (vis:>cilVisitor) c in
  vis#fill_global_tables; c'

let visitFramacAssigns vis a =
  let a' = visitCilAssigns (vis:>cilVisitor) a in
  vis#fill_global_tables; a'

let visitFramacFunspec vis f =
  let f' = visitCilFunspec (vis:>cilVisitor) f in
  vis#fill_global_tables; f'

let visitFramacLogicType vis l =
  let l' = visitCilLogicType (vis:>cilVisitor) l in
  vis#fill_global_tables; l'

let visitFramacPredicate vis p =
  let p' = visitCilPredicate (vis:>cilVisitor) p in
  vis#fill_global_tables; p'

let visitFramacPredicateNamed vis p =
  let p' = visitCilPredicateNamed (vis:>cilVisitor) p in
  vis#fill_global_tables; p'

let visitFramacTsets vis t =
  let t' = visitCilTsets (vis:>cilVisitor) t in
  vis#fill_global_tables; t'

let visitFramacTsetsElem vis t =
  let t' = visitCilTsetsElem (vis:>cilVisitor) t in
  vis#fill_global_tables; t'


let visitFramacTsetsOffset vis t =
  let t' = visitCilTsetsOffset (vis:>cilVisitor) t in
  vis#fill_global_tables; t'

let visitFramacTerm  vis t =
  let t' = visitCilTerm (vis:>cilVisitor) t in
  vis#fill_global_tables; t'

let visitFramacTermOffset vis t =
  let t' = visitCilTermOffset (vis:>cilVisitor) t in
  vis#fill_global_tables; t'

let visitFramacLogicInfo vis l =
  let l' = visitCilLogicInfo (vis:>cilVisitor) l in
  vis#fill_global_tables; l'

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)

(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

open Extlib
open Cil
open Cil_types

let is_definition v =
  Ast_info.Function.is_definition (Globals.Functions.get v).fundec

(* ************************************************************************* *)
(** {2 Visitors} *)
(* ************************************************************************* *)

(** Class type for a Db-aware visitor. *)
class type frama_c_visitor = object
  inherit cilVisitor
  method frama_c_plain_copy: frama_c_visitor
  method vstmt_aux: Cil_types.stmt -> Cil_types.stmt visitAction
  method vglob_aux: Cil_types.global -> Cil_types.global list visitAction
  method vrooted_code_annotation:
    rooted_code_annotation ->
    rooted_code_annotation list visitAction
  method is_annot_before: bool
  method current_kf: kernel_function option
  method set_current_kf: kernel_function -> unit
  method reset_current_kf: unit -> unit
end

(** Extension to the cil visitor that is aware of kernel function
    and annotation db. This is done by defining auxiliary methods that can be
    redefined in inherited classes, while the corresponding ones from
    {!Cil.cilVisitor} {b must} retain their values as defined here. Otherwise,
    annotations may not be visited properly. *)
class internal_generic_frama_c_visitor 
  current_kf prj behavior: frama_c_visitor =

  let childrenRooted_code_annotation (vis:frama_c_visitor) rca =
    match rca with
      User ca ->
        let ca' = visitCilCodeAnnotation (vis:> cilVisitor) ca in
        if ca != ca' then User ca' else rca
    | AI (cause,ca) ->
      let ca' = visitCilCodeAnnotation (vis:> cilVisitor) ca in
      if ca != ca' then AI(cause,ca') else rca
  in
  let visitRooted_code_annotation (vis: frama_c_visitor) ca =
    doVisitList vis vis#frama_c_plain_copy
      (fun x -> x)
      vis#vrooted_code_annotation childrenRooted_code_annotation ca
  in
object(self)
  inherit genericCilVisitor ~prj behavior as super
  (* top of the stack indicates if we are before or after the current
     statement. *)
  val before = Stack.create ()

  method frama_c_plain_copy = 
    new internal_generic_frama_c_visitor current_kf prj behavior

  method plain_copy_visitor = (self#frama_c_plain_copy :> Cil.cilVisitor)

  method set_current_kf kf = current_kf := Some kf

  method reset_current_kf () = current_kf := None

  method current_kf = !current_kf

  method is_annot_before =
    Stack.is_empty before (* global annotation *) || Stack.top before

  method vrooted_code_annotation _ = DoChildren

  method private vstmt stmt =
    let annots = Annotations.get_all_annotations stmt in
    let res = self#vstmt_aux stmt in
    let compare_rooted x y =
      let id1 = match x with User ca | AI(_,ca) -> ca.annot_id in
      let id2 = match y with User ca | AI(_,ca) -> ca.annot_id in
      if id1 < id2 then -1 else if id2 < id1 then 1 else 0
    (* Annotations will be visited and more importantly added in the
       same order as they were in the original AST.  *)
    in
    let abefore = List.sort compare_rooted annots in
    let make_children_annot vis =
      Stack.push true before;
      let res_before, remove_before =
        List.fold_left
          (fun (res,remove) x ->
            let curr_res, keep_curr =
               (* only keeps non-trivial non-already existing annotations *)
              List.fold_left
                (fun (res,keep) y ->
                  let current = x == y in
                  let res =
                    if
                      (* if x is trivial, keep all annotations, including
                         trivial ones. *)
                      (not (Ast_info.is_trivial_rooted_assertion y)
                       || (Ast_info.is_trivial_rooted_assertion x))
                      &&
                      (not current || Cil.is_copy_behavior vis#behavior)
                    then y::res else res
                  in (res, keep || current))
                ([],false)
                (visitRooted_code_annotation (vis:>frama_c_visitor) x)
            in
            (res @ curr_res, if keep_curr then remove else x::remove)
          )
          ([],[])
          abefore
      in
      ignore (Stack.pop before);
      (res_before, remove_before)
    in
    let change_stmt stmt (res_before, remove) =
      if (res_before <> [] || remove <> []) then begin
	let kf = Extlib.the self#current_kf in
        let new_kf = Cil.get_kernel_function self#behavior kf in
        Queue.add
          (fun () ->
	    let add_annot = Annotations.add new_kf stmt [] in
            if remove <> [] then 
              Annotations.filter
                ~reset:true
                (fun _ _ annot -> not (List.memq annot remove))
		kf
                stmt;
	List.iter add_annot (List.rev res_before))
          self#get_filling_actions
      end
    in
    let post_action f stmt =
      let annots = make_children_annot self in
      let stmt = f stmt in
      change_stmt stmt annots; stmt
    in
    let copy stmt =
      change_stmt stmt
        (make_children_annot self#frama_c_plain_copy); stmt
    in
    let plain_post = post_action (fun x -> x) in
    match res with
    | SkipChildren -> res
    | JustCopy -> JustCopyPost copy
    | JustCopyPost f -> JustCopyPost (f $ copy)
    | DoChildren -> ChangeDoChildrenPost (stmt, plain_post)
    | ChangeTo _ | ChangeToPost _ -> res
    | ChangeDoChildrenPost (stmt,f) ->
      ChangeDoChildrenPost (stmt, post_action f)

  method vstmt_aux _ = DoChildren
  method vglob_aux _ = DoChildren

  method vglob g =
    let fundec, has_kf = match g with
      | GVarDecl(_,v,_) when isFunctionType v.vtype ->
        let v = Cil.get_original_varinfo self#behavior v in
        let kf = Globals.Functions.get v in
        (* Just make a copy of current kernel function in case it is needed *)
        let new_kf = Cil.memo_kernel_function self#behavior kf in
        if Cil.is_copy_behavior self#behavior then
          new_kf.spec <- Cil.empty_funspec ();
        self#set_current_kf kf;
        None, true
      | GFun(f,_) ->
        let v = Cil.get_original_varinfo self#behavior f.svar in
        let kf = Globals.Functions.get v in
        let new_kf = Cil.memo_kernel_function self#behavior kf in
        if Cil.is_copy_behavior self#behavior then
          new_kf.spec <- Cil.empty_funspec ();
        self#set_current_kf kf;
        Some f, true
      | _ -> None, false
    in
    let res = self#vglob_aux g in
    let make_funspec () =
      match g with
      | GVarDecl(_,v,_) when isFunctionType v.vtype ->
        let v = Cil.get_original_varinfo self#behavior v in
        if not (is_definition v) then begin
          let spec = (Extlib.the self#current_kf).spec in
          let spec' = visitCilFunspec (self:> cilVisitor) spec in
          Some spec'
        end else
          None
      | GFun _ ->
        let spec' = visitCilFunspec (self:> cilVisitor)
          (Extlib.the self#current_kf).spec
        in
        Some spec'
      | _ -> None
    in
    let get_spec () =
      match g with
        GVarDecl(_,v,_) when isFunctionType v.vtype ->
          let v = Cil.get_original_varinfo self#behavior v in
          if not (is_definition v) then begin
            Some (Extlib.the self#current_kf).spec
          end
          else None (* visited in the corresponding definition *)
      | GFun _ -> Some (Extlib.the self#current_kf).spec
      | _ -> None
    in
    let change_glob ng spec =
      let cond = is_copy_behavior self#behavior in
      match ng with
      | GVar(vi,init,_) ->
          if cond then
            Queue.add
              (fun () -> Globals.Vars.add vi init)
              self#get_filling_actions
      | GVarDecl(_,v,l) when isFunctionType v.vtype ->
        let spec = match spec with
            None -> Cil.empty_funspec ()
          | Some spec -> spec
        in
        let kf = Extlib.the self#current_kf in
        let orig_spec = kf.spec in
        let new_kf = Cil.get_kernel_function self#behavior kf in
        if cond || (not (Cil.is_empty_funspec spec) &&
                      not (Cil.is_empty_funspec orig_spec) &&
                      spec != orig_spec)
        then
          Queue.add
            (fun () ->
              (* NB: we can't really know whether v is associated to new_kf,
                 but if this is not the case, it is the responsibility of the
                 child visitor to properly update kf table while doing its
                 own transformations.
               *)
              Globals.Functions.register new_kf;
              Globals.Functions.replace_by_declaration spec v l)
            self#get_filling_actions;

      | GVarDecl (_,({vstorage=Extern} as v),_) (* when not (isFunctionType
                                                   v.vtype *) ->
        if cond then
          Queue.add
            (fun () -> Globals.Vars.add_decl v)
            self#get_filling_actions
      | GFun(f,l) ->
        if cond then begin
          let spec =
            match spec with
              None -> Cil.empty_funspec ()
            | Some spec -> spec
          in
          let new_kf = 
            Cil.get_kernel_function self#behavior (Extlib.the self#current_kf)
          in
          Queue.add
            (fun () ->
              Kernel.debug
                "@[Adding definition %s (vid: %d) for project %s@\n\
                      body: %a@\n@]@."
                f.svar.vname f.svar.vid
                (Project.get_name (Project.current()))
                !Ast_printer.d_block f.sbody
              ;
              Globals.Functions.register new_kf;
              Globals.Functions.replace_by_definition spec f l)
            self#get_filling_actions
        end
      | _ -> ()
    in
    let post_action g =
      let spec = lazy (make_funspec ()) in
      Extlib.may self#set_current_func fundec;
      List.iter (fun g -> change_glob g (Lazy.force spec)) g;
      if has_kf then self#reset_current_kf();
      Extlib.may (fun _ -> self#reset_current_func ()) fundec;
      g
    in
    let post_change_to g =
      List.iter (fun g -> change_glob g None) g;
      if has_kf then self#reset_current_kf();
      g
    in
    match res with
      SkipChildren ->
        change_glob g (get_spec());
        if has_kf then self#reset_current_kf(); res
    | JustCopy -> JustCopyPost post_action
    | JustCopyPost f -> JustCopyPost (f $ post_action)
    | DoChildren -> ChangeDoChildrenPost([g],post_action)
    | ChangeTo l -> ChangeToPost (l,post_change_to)
    | ChangeToPost (l,f) -> ChangeToPost (l, f $ post_change_to)
    | ChangeDoChildrenPost (g,f) -> ChangeDoChildrenPost (g, post_action $ f)
end

class generic_frama_c_visitor prj bhv =
  let current_kf = ref None in
  internal_generic_frama_c_visitor current_kf prj bhv

class frama_c_copy prj = generic_frama_c_visitor prj (copy_visit ())

class frama_c_inplace =
  generic_frama_c_visitor (Project.current()) (inplace_visit())

let visitFramacFileCopy vis f = visitCilFileCopy (vis:>cilVisitor) f

let visitFramacFile vis f = visitCilFile (vis:>cilVisitor) f

let visitFramacFileSameGlobals vis f =
  visitCilFileSameGlobals (vis:>cilVisitor) f

let visitFramacGlobal vis g =
  let g' = visitCilGlobal (vis:>cilVisitor) g in
  vis#fill_global_tables; g'

let visitFramacFunction vis f =
  vis#set_current_kf (Globals.Functions.get f.svar);
  let f' = visitCilFunction (vis:>cilVisitor) f in
  vis#reset_current_kf ();
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

let visitFramacFrom vis a =
  let a' = visitCilFrom (vis:>cilVisitor) a in
  vis#fill_global_tables; a'

let visitFramacDeps vis a =
  let a' = visitCilDeps (vis:>cilVisitor) a in
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

let visitFramacIdPredicate vis p =
  let p' = visitCilIdPredicate (vis:>cilVisitor) p in
  vis#fill_global_tables; p'

let visitFramacPredicates vis p =
  let p' = visitCilPredicates (vis:>cilVisitor) p in
  vis#fill_global_tables; p'

let visitFramacTerm  vis t =
  let t' = visitCilTerm (vis:>cilVisitor) t in
  vis#fill_global_tables; t'

let visitFramacTermOffset vis t =
  let t' = visitCilTermOffset (vis:>cilVisitor) t in
  vis#fill_global_tables; t'

let visitFramacTermLhost vis t =
  let t' = visitCilTermLhost (vis:>cilVisitor) t in
  vis#fill_global_tables; t'

let visitFramacTermLval vis t =
  let t' = visitCilTermLval (vis:>cilVisitor) t in
  vis#fill_global_tables; t'

let visitFramacLogicInfo vis l =
  let l' = visitCilLogicInfo (vis:>cilVisitor) l in
  vis#fill_global_tables; l'

let visitFramacBehavior vis b =
  let b' = visitCilBehavior (vis:>cilVisitor) b in
  vis#fill_global_tables; b'

let visitFramacBehaviors vis b =
  let b' = visitCilBehaviors (vis:>cilVisitor) b in
  vis#fill_global_tables; b'

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

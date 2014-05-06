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

open Extlib
open Cil
open Cil_types

(* ************************************************************************* *)
(** {2 Visitors} *)
(* ************************************************************************* *)

(** Class type for a Db-aware visitor. *)
class type frama_c_visitor = object
  inherit cilVisitor
  method frama_c_plain_copy: frama_c_visitor
  method vstmt_aux: Cil_types.stmt -> Cil_types.stmt visitAction
  method vglob_aux: Cil_types.global -> Cil_types.global list visitAction
  method current_kf: kernel_function option
  (** @plugin development guide *)

  method set_current_kf: kernel_function -> unit
  method reset_current_kf: unit -> unit
end

(** Extension to the cil visitor that is aware of kernel function
    and annotation db. This is done by defining auxiliary methods that can be
    redefined in inherited classes, while the corresponding ones from
    {!Cil.cilVisitor} {b must} retain their values as defined here. Otherwise,
    annotations may not be visited properly. *)
class internal_generic_frama_c_visitor fundec queue current_kf behavior: frama_c_visitor =

object(self)
  inherit internal_genericCilVisitor fundec behavior queue

  method frama_c_plain_copy =
    new internal_generic_frama_c_visitor fundec queue current_kf behavior

  method! plain_copy_visitor =
    assert (self#frama_c_plain_copy#get_filling_actions == 
              self#get_filling_actions);
    (self#frama_c_plain_copy :> Cil.cilVisitor)

  method set_current_kf kf = current_kf := Some kf

  method reset_current_kf () = current_kf := None

  method current_kf = !current_kf

  method! private vstmt stmt =
    let annots =
      Annotations.fold_code_annot (fun e a acc -> (e, a) :: acc) stmt []
    in
    let res = self#vstmt_aux stmt in
    (* Annotations will be visited and more importantly added in the
       same order as they were in the original AST.  *)
    let abefore =
      List.sort 
        (fun (_,a) (_,b) -> Cil_datatype.Code_annotation.compare a b)
        annots
    in
    let make_children_annot vis =
      let res_before, remove_before =
        List.fold_left
          (fun (res,remove) (e, x) ->
            let curr_res, keep_curr =
              (* only keeps non-trivial non-already existing annotations *)
              List.fold_left
                (fun (res,keep) y ->
                  let current = x == y in
                  let res =
                    if
                      (* if x is trivial, keep all annotations, including
                         trivial ones. *)
                      (not (Logic_utils.is_trivial_annotation y)
                       || (Logic_utils.is_trivial_annotation x))
                      &&
                      (not current || Cil.is_copy_behavior vis#behavior)
                    then (e, y) :: res else res
                  in (res, keep || current))
                ([],false)
                (* TODO: make visitCilCodeAnnotation return a list of 
                   annotations? *)
                [visitCilCodeAnnotation (vis:>cilVisitor) x]
            in
            (res @ curr_res, if keep_curr then remove else (e, x) :: remove)
          )
          ([],[])
          abefore
      in
      (res_before, remove_before)
    in
    let change_stmt stmt (res_before, remove) =
      if (res_before <> [] || remove <> []) then begin
        let kf = Extlib.the self#current_kf in
        let new_kf = Cil.get_kernel_function self#behavior kf in
        Queue.add
          (fun () ->
	    let apply f = List.iter (fun (e, a) -> f e ~kf:new_kf stmt a) in
	    (* eta-expansions below required to OCaml type system *)
	    apply (fun e ~kf -> Annotations.remove_code_annot e ~kf) remove;
	    apply (fun e ~kf -> Annotations.add_code_annot e ~kf) res_before)
          self#get_filling_actions
      end
    in
    let post_action f stmt =
      let annots = make_children_annot self in
      let stmt = f stmt in
      change_stmt stmt annots;
      stmt
    in
    let copy stmt =
      change_stmt stmt(make_children_annot self#frama_c_plain_copy);
      stmt
    in
    let plain_post = post_action (fun x -> x) in
    match res with
    | SkipChildren -> res
    | JustCopy -> JustCopyPost copy
    | JustCopyPost f -> JustCopyPost (f $ copy)
    | DoChildren -> DoChildrenPost plain_post
    | DoChildrenPost f -> DoChildrenPost (f $ plain_post)
    | ChangeTo _ | ChangeToPost _ -> res
    | ChangeDoChildrenPost (stmt,f) ->
      ChangeDoChildrenPost (stmt, post_action f)

  method vstmt_aux _ = DoChildren
  method vglob_aux _ = DoChildren

  method private vbehavior_annot ?e b =
    let kf = Extlib.the self#current_kf in
    let treat_elt emit elt acc =
      match e with
        | None -> (emit, elt) :: acc
        | Some e when Emitter.equal e emit -> (emit, elt) :: acc
        | Some _ -> acc
    in
    let fold_elt fold = fold treat_elt kf b.b_name [] in
    let old_requires = fold_elt Annotations.fold_requires in
    let old_assumes = fold_elt Annotations.fold_assumes in
    let old_ensures = fold_elt Annotations.fold_ensures in
    let old_assigns = fold_elt Annotations.fold_assigns in
    let old_allocates = fold_elt Annotations.fold_allocates in
    let res = self#vbehavior b in
    let new_kf = Cil.get_kernel_function self#behavior kf in
    let add_queue a = Queue.add a self#get_filling_actions in
    let visit_clauses vis f =
      (* Ensures that we have a table associated to new_kf in Annotations. *)
      add_queue
        (fun () ->
          ignore (Annotations.behaviors ~populate:false new_kf));
      let module Fold =
          struct
            type 'a t =
                { apply: 'b. (Emitter.t -> 'a -> 'b -> 'b) ->
                         Kernel_function.t -> string -> 'b -> 'b }
          end
      in
      let visit_elt visit e elt (f,acc) =
        let new_elt = visit (vis:>Cil.cilVisitor) elt in
        (* We'll add the elts afterwards, so as to keep lists in their
           original order as much as we can. see fold_elt below.
        *)
        f ||  new_elt != elt || new_kf != kf,
        (e,new_elt) :: acc
      in
      let check_elt visit e' elt acc =
        match e with
          | None -> visit_elt visit e' elt acc
          | Some e when Emitter.equal e e' -> visit_elt visit e' elt acc
          | Some _ -> acc
      in
      let fold_elt fold visit remove add append dft =
        let (changed, res) =
          fold.Fold.apply (check_elt visit) kf b.b_name (false,[])
        in
        if changed then begin
          add_queue
            (fun () ->
              fold.Fold.apply
                (fun e' x () ->
                  match e with
                    | None -> remove e' new_kf x
                    | Some e when Emitter.equal e e' -> remove e' new_kf x
                    | _ -> ())
                new_kf b.b_name ();
              List.iter (fun (e,x) -> add e new_kf b.b_name x) res)
        end;
        List.fold_left (fun acc (_,x) -> append x acc) dft res
      in
      let req =
        fold_elt
          { Fold.apply = Annotations.fold_requires }
          Cil.visitCilIdPredicate
          Annotations.remove_requires
          (fun e kf b r -> Annotations.add_requires e kf b [r])
          (fun x l -> x :: l) []
      in
      b.b_requires <- req;
      let assumes =
        fold_elt
          { Fold.apply = Annotations.fold_assumes }
          Cil.visitCilIdPredicate
          Annotations.remove_assumes
          (fun e kf b a -> Annotations.add_assumes e kf b [a])
          (fun x l -> x :: l) []
      in
      b.b_assumes <- assumes;
      let visit_ensures vis (k,p as e) =
        let new_p = Cil.visitCilIdPredicate (vis:>Cil.cilVisitor) p in
        if p != new_p then (k,new_p) else e
      in
      let ensures =
        fold_elt
          { Fold.apply = Annotations.fold_ensures }
          visit_ensures
          Annotations.remove_ensures
          (fun e kf b p -> Annotations.add_ensures e kf b [p])
          (fun x l -> x :: l) []
      in
      b.b_post_cond <- ensures;
      let add_assigns e kf b a =
        match a with
          | WritesAny -> ()
          | _ -> Annotations.add_assigns ~keep_empty:false e kf b a
      in
      let concat_assigns new_a a =
        match new_a, a with
          | WritesAny, a | a, WritesAny -> a
          | Writes a1, Writes a2 -> Writes (a2 @ a1)
      in
      let a =
        fold_elt
          { Fold.apply = Annotations.fold_assigns }
          Cil.visitCilAssigns
          Annotations.remove_assigns
          add_assigns
          concat_assigns
          WritesAny
      in
      b.b_assigns <- a;
      let concat_allocation new_a a =
        match new_a, a with
          | FreeAllocAny, a | a, FreeAllocAny -> a
          | FreeAlloc(a1,a2), FreeAlloc(a3,a4) -> FreeAlloc (a3@a1,a4@a2)
      in
      let a =
        fold_elt
          { Fold.apply = Annotations.fold_allocates }
          Cil.visitCilAllocation
          Annotations.remove_allocates
          Annotations.add_allocates
          concat_allocation
          FreeAllocAny
      in
      b.b_allocation <- a;
      f b
    in
    let remove_and_add get remove add fold old b =
      let emitter = match e with None -> Emitter.end_user | Some e -> e in
      let elts = get b in
      List.iter
        (fun (e,x) ->
          if not (List.memq x elts) then
            add_queue (fun () -> remove e new_kf x))
        old;
      let module M = struct exception Found of Emitter.t end in
      let already_there x =
        fold (fun e y () -> if x == y then raise (M.Found e)) new_kf b.b_name ()
      in
      List.iter
        (fun x ->
          add_queue
            (fun () ->
              try
                already_there x;
                add emitter new_kf b.b_name x
              with M.Found e ->
                (* We keep x at its right place inside b. *)
                remove e new_kf x;
                add e new_kf b.b_name x))
        (List.rev elts);
    in
    let register_annots b f =
      add_queue
        (fun () -> ignore (Annotations.behaviors ~populate:false new_kf));
      remove_and_add
        (fun b -> b.b_requires)
        Annotations.remove_requires
        (fun e kf b r -> Annotations.add_requires e kf b [r])
        Annotations.fold_requires
        old_requires b;
      remove_and_add
        (fun b -> b.b_assumes)
        Annotations.remove_assumes
        (fun e kf b r -> Annotations.add_assumes e kf b [r])
        Annotations.fold_assumes
        old_assumes b;
      remove_and_add
        (fun b -> b.b_post_cond)
        Annotations.remove_ensures
        (fun e kf b r -> Annotations.add_ensures e kf b [r])
        Annotations.fold_ensures
        old_ensures b;
      remove_and_add
        (fun b -> match b.b_assigns with WritesAny -> [] | a -> [a])
        Annotations.remove_assigns
        (fun e kf b a ->
          match a with
            | WritesAny -> ()
            | Writes _ -> Annotations.add_assigns ~keep_empty:false e kf b a)
        Annotations.fold_assigns
        old_assigns b;
      remove_and_add
        (fun b -> match b.b_allocation with FreeAllocAny -> [] | a -> [a])
        Annotations.remove_allocates
        Annotations.add_allocates
        Annotations.fold_allocates
        old_allocates b;
      f b
    in
    match res with
      | SkipChildren -> b
      | JustCopy -> visit_clauses self#plain_copy_visitor Extlib.id
      | JustCopyPost f -> visit_clauses self#plain_copy_visitor f
      | ChangeTo b -> register_annots b Extlib.id
      | ChangeToPost (b,f) -> register_annots b f
      | ChangeDoChildrenPost (b,f) ->
          register_annots (Cil.childrenBehavior (self:>Cil.cilVisitor) b) f
      | DoChildren -> visit_clauses self Extlib.id
      | DoChildrenPost f -> visit_clauses self f

  method private vfunspec_annot () =
    let kf = Extlib.the self#current_kf in
    let new_kf = Cil.get_kernel_function self#behavior kf in
    let old_behaviors =
      Annotations.fold_behaviors (fun e b acc -> (e,b)::acc) kf []
    in
    let old_complete =
      Annotations.fold_complete (fun e c acc -> (e,c)::acc) kf []
    in
    let old_disjoint =
      Annotations.fold_disjoint (fun e d acc -> (e,d)::acc) kf []
    in
    let old_terminates =
      Annotations.fold_terminates (fun e t _ -> Some (e,t)) kf None
    in
    let old_decreases =
      Annotations.fold_decreases (fun e d _ -> Some (e,d)) kf None
    in
    let spec =
      { spec_behavior = snd (List.split old_behaviors);
        spec_complete_behaviors = snd (List.split old_complete);
        spec_disjoint_behaviors = snd (List.split old_disjoint);
        spec_terminates =
          (Extlib.opt_map snd) old_terminates;
        spec_variant = 
          (Extlib.opt_map snd) old_decreases
      }
    in
    let res = self#vspec spec in
    let do_children () =
      let new_behaviors =
        List.rev_map
          (fun (e,b) ->
            let b' = self#vbehavior_annot ~e b in
            if b != b' || kf != new_kf then begin
              Queue.add
                (fun () ->
                  Annotations.add_behaviors
                    ~register_children:false e new_kf [b'])
                self#get_filling_actions;
            end;
            b')
          old_behaviors
      in
      let new_terminates =
        Extlib.opt_map
          (fun (e,t) ->
            let t' = Cil.visitCilIdPredicate (self:>Cil.cilVisitor) t in
            if t != t' || kf != new_kf then
              Queue.add (fun () ->
                Annotations.remove_terminates e new_kf;
                Annotations.add_terminates e new_kf t')
                self#get_filling_actions
            ;
          t')
          old_terminates
      in
      let new_decreases =
        Extlib.opt_map
          (fun (e,(d,s as acc)) ->
            let d' = Cil.visitCilTerm (self:>Cil.cilVisitor) d in
            if d != d' || kf != new_kf then begin
              let res = (d',s) in
              Queue.add
                (fun () ->
                  Annotations.remove_decreases e new_kf;
                  Annotations.add_decreases e new_kf res;
                )
                self#get_filling_actions;
              res
            end else acc
          )
          old_decreases
      in
      if kf != new_kf then begin
        List.iter
          (fun (e,c) ->
            Queue.add (fun () -> Annotations.add_complete e new_kf c)
              self#get_filling_actions)
          (List.rev old_complete);
        List.iter
          (fun (e,d) ->
            Queue.add (fun () -> Annotations.add_disjoint e new_kf d)
              self#get_filling_actions)
          (List.rev old_disjoint)
      end;
      { spec with
        spec_behavior = new_behaviors;
        spec_terminates = new_terminates;
        spec_variant = new_decreases }
    in
    let change_do_children spec =
      let new_behaviors =
        Cil.mapNoCopy self#vbehavior_annot spec.spec_behavior
      in
      let new_terminates =
        Cil.optMapNoCopy (Cil.visitCilIdPredicate (self:>Cil.cilVisitor))
          spec.spec_terminates
      in
      let new_decreases =
       Cil.optMapNoCopy
          (fun (d,s as acc) ->
            let d' = Cil.visitCilTerm (self:>Cil.cilVisitor) d in
            if d != d' then (d',s) else acc)
          spec.spec_variant
      in
      { spec with
        spec_behavior = new_behaviors;
        spec_terminates = new_terminates;
        spec_variant = new_decreases }
    in
    let register_new_components new_spec =
      let add_spec_components () =
        let populate = false in
        let new_behaviors = Annotations.behaviors ~populate new_kf in
        List.iter
          (fun b ->
            if
              (List.for_all
                 (fun x -> x.b_name <> b.b_name || Cil.is_empty_behavior x)
                 new_behaviors)
            then begin
              Annotations.add_behaviors ~register_children:false 
                Emitter.end_user new_kf [b]
            end)
          new_spec.spec_behavior;
        let new_complete = Annotations.complete ~populate new_kf in
        List.iter
          (fun c ->
            if not (List.memq c new_complete) then begin
              Annotations.add_complete Emitter.end_user new_kf c
            end)
          new_spec.spec_complete_behaviors;
        let new_disjoint = Annotations.disjoint ~populate new_kf in
        List.iter
          (fun d ->
            if not (List.memq d new_disjoint) then
              Annotations.add_disjoint Emitter.end_user new_kf d)
          new_spec.spec_disjoint_behaviors;
        let new_terminates = Annotations.terminates ~populate new_kf in
        (match new_terminates, new_spec.spec_terminates with
          | None, None -> ()
          | Some _, None -> ()
          | None, Some p ->
              Annotations.add_terminates Emitter.end_user new_kf p
          | Some p1, Some p2 when p1 == p2 -> ()
          | Some p1, Some p2 ->
              Kernel.fatal
                "Visit of spec of function %a gives \
                 inconsistent terminates clauses@\n\
                 Registered @[%a@]@\nReturned @[%a@]"
                Kernel_function.pretty new_kf
                Printer.pp_identified_predicate p1
                Printer.pp_identified_predicate p2);
        let new_decreases = Annotations.decreases ~populate new_kf in
        (match new_decreases, new_spec.spec_variant with
          | None, None -> ()
          | Some _, None -> ()
          | None, Some p ->
              Annotations.add_decreases Emitter.end_user new_kf p
          | Some p1, Some p2 when p1 == p2 -> ()
          | Some p1, Some p2 ->
              Kernel.fatal
                "Visit of spec of function %a gives \
                 inconsistent variant clauses@\n\
                 Registered %d@\n%a@\nReturned %d@\n%a"
                Kernel_function.pretty new_kf
                (Obj.magic p1)
                Printer.pp_decreases p1
                (Obj.magic p2)
                Printer.pp_decreases p2)
      in
      List.iter
        (fun (e,c) ->
          if not (List.memq c new_spec.spec_complete_behaviors) then
            Queue.add
              (fun () -> Annotations.remove_complete e new_kf c)
              self#get_filling_actions)
        old_complete;
      List.iter
        (fun (e,d) ->
          if not (List.memq d new_spec.spec_disjoint_behaviors) then
            Queue.add
              (fun () -> Annotations.remove_disjoint e new_kf d)
              self#get_filling_actions)
        old_disjoint;
      List.iter
        (fun (e,b) ->
          if not (List.memq b new_spec.spec_behavior) then begin
            Queue.add
              (fun () ->
                if
                  List.exists (fun x -> x.b_name = b.b_name)
                    new_spec.spec_behavior
                then Annotations.remove_behavior_components e new_kf b
                else Annotations.remove_behavior e new_kf b)
              self#get_filling_actions
          end
        )
        old_behaviors;
      Extlib.may
        (fun (e,t) ->
          if not (Extlib.may_map
                    ~dft:false (fun t' -> t == t') new_spec.spec_terminates)
          then
            Queue.add
              (fun () -> Annotations.remove_terminates e new_kf)
              self#get_filling_actions)
        old_terminates;
      Extlib.may
        (fun (e,d) ->
          if not (Extlib.may_map
                    ~dft:false (fun d' -> d == d') new_spec.spec_variant)
          then
            Queue.add
              (fun () -> Annotations.remove_decreases e new_kf)
              self#get_filling_actions)
        old_decreases;
      Queue.add add_spec_components self#get_filling_actions;
    in
    match res with
      | SkipChildren -> register_new_components spec
      | ChangeTo spec -> register_new_components spec
      | ChangeToPost (spec,f) -> 
          register_new_components spec; ignore (f spec)
      | JustCopy ->
          register_new_components
            (Cil.visitCilFunspec self#plain_copy_visitor spec)
      | JustCopyPost f ->
          (register_new_components
             (Cil.visitCilFunspec self#plain_copy_visitor spec));
          ignore (f spec)
      | DoChildren -> ignore (do_children ())
      | DoChildrenPost f -> ignore (f (do_children ()))
      | ChangeDoChildrenPost(spec, f) ->
          let res = change_do_children spec in
          register_new_components res;
          ignore (f res)

  method! vglob g =
    let fundec, has_kf = match g with
      | GVarDecl(_,v,_) when isFunctionType v.vtype ->
        let kf = try Globals.Functions.get v with Not_found ->
          Kernel.fatal "No kernel function for %s(%d)" v.vname v.vid
        in
        (* Just make a copy of current kernel function in case it is needed *)
        let new_kf = Cil.memo_kernel_function self#behavior kf in
        if Cil.is_copy_behavior self#behavior then
          new_kf.spec <- Cil.empty_funspec ();
        self#set_current_kf kf;
        None, true
      | GFun(f,_) ->
        let v = Cil.get_original_varinfo self#behavior f.svar in
        let kf = 
	  try Globals.Functions.get v 
	  with Not_found ->
	    Kernel.fatal "Visitor does not find function %s in %a"
	      v.vname
	      Project.pretty (Project.current ())
	in
        let new_kf = Cil.memo_kernel_function self#behavior kf in
        if Cil.is_copy_behavior self#behavior then
          new_kf.spec <- Cil.empty_funspec ();
        self#set_current_kf kf;
        Some f, true
      | _ -> None, false
    in
    let res = self#vglob_aux g in
    let make_funspec () = match g with
      | GVarDecl(_,v,_)
          when isFunctionType v.vtype && Ast.is_last_decl g ->
          self#vfunspec_annot ();
      | GFun _ when Ast.is_last_decl g ->
          self#vfunspec_annot ()
      | _ -> ()
    in
    (* NB: we'll loose track of the emitter of an annotation.
       Anyway, this is only used for SkipChildren and JustCopy/JustCopyPost
       (and for a copy visitor)
       If user sticks to DoChildren, s/he'll still have the proper
       correspondance between annotations and emitters.
    *)
    let get_spec () = match g with
      | GVarDecl(_,v,_)
          when isFunctionType v.vtype && Ast.is_last_decl g ->
	let spec =
	  Annotations.funspec ~populate:false (Extlib.the self#current_kf)
	in
        Some (Cil.visitCilFunspec self#plain_copy_visitor spec)
      | GFun _ when Ast.is_last_decl g ->
	let spec =
	  Annotations.funspec ~populate:false (Extlib.the self#current_kf)
	in
	Some (Cil.visitCilFunspec self#plain_copy_visitor spec)
      | _ -> None
    in
    let change_glob ng spec =
      let cond = is_copy_behavior self#behavior in
      match ng with
        | GVar(vi,init,_) ->
            if cond then
              Queue.add
                (fun () ->
                  try
                    Globals.Vars.add vi init
                  with Globals.Vars.AlreadyExists (vi,_) ->
                    Kernel.fatal
                      "Visitor is trying to insert global variable %a that \
                     already exists in current project"
                      Cil_datatype.Varinfo.pretty vi)
                self#get_filling_actions
        | GVarDecl(_,v,l) when isFunctionType v.vtype ->
            (match self#current_kf with
              | Some kf ->
                  let new_kf = Cil.get_kernel_function self#behavior kf in
                  if cond then begin
                    Queue.add
                      (fun () ->
                        if Cil.hasAttribute "FC_BUILTIN" v.vattr then
                          Cil.Frama_c_builtins.add v.vname v;
                        if Cil_datatype.Varinfo.equal v
                          (Kernel_function.get_vi new_kf)
                        then begin
                          let dft =
                            Annotations.funspec ~populate:false new_kf
                          in
                          let dft =
                            { dft with spec_behavior = dft.spec_behavior }
                          in
                          let spec = Extlib.opt_conv dft spec in
                          Globals.Functions.register new_kf;
                          Globals.Functions.replace_by_declaration spec v l;
                      (* Format.printf "registered spec:@\n%a@." Printer.pp_funspec
                         (Annotations.funspec ~populate:false new_kf) *)
                        end else begin
                          Globals.Functions.replace_by_declaration
                            (Cil.empty_funspec()) v l
                        end)
                      self#get_filling_actions;
                    if
                      Cil_datatype.Varinfo.equal v
                        (Kernel_function.get_vi new_kf) && Extlib.has_some spec
                    then
                      Queue.add
                        (fun () ->
                          Annotations.register_funspec ~force:true new_kf)
                        self#get_filling_actions;
                  end
              | None -> ()
              (* User is responsible for registering the new function *)
            )
      | GVarDecl (_,({vstorage=Extern} as v),_) (* when not (isFunctionType
                                                   v.vtype) *) ->
        if cond then
          Queue.add
            (fun () ->
              try
                Globals.Vars.add_decl v
              with Globals.Vars.AlreadyExists (vi,_) ->
                Kernel.fatal
                  "Visitor is trying to insert global variable %a that \
                     already exists in current project"
                  Cil_datatype.Varinfo.pretty vi)
            self#get_filling_actions
      | GFun(f,l) ->
        if cond then begin
          match self#current_kf with
            | Some kf ->
                let new_kf = Cil.get_kernel_function self#behavior kf in
                Queue.add
                  (fun () ->
                    Kernel.debug
                      "@[Adding definition %s (vid: %d) for project %s@\n\
                         body: %a@\n@]@."
                      f.svar.vname f.svar.vid
                      (Project.get_name (Project.current()))
                      Printer.pp_block f.sbody;
                    if cond && Cil.hasAttribute "FC_BUILTIN" f.svar.vattr then
                      Cil.Frama_c_builtins.add f.svar.vname f.svar;
                    if  Cil_datatype.Varinfo.equal f.svar
                      (Kernel_function.get_vi new_kf)
                    then begin
                      Globals.Functions.register new_kf;
                      let spec =
                        Extlib.opt_conv
                          (Annotations.funspec ~populate:false new_kf) spec
                      in
                      Globals.Functions.replace_by_definition spec f l
                    end else
                      Globals.Functions.replace_by_definition
                        (Cil.empty_funspec ()) f l
                  )
                  self#get_filling_actions;
                if Cil_datatype.Varinfo.equal f.svar
                  (Kernel_function.get_vi new_kf)
                  && Extlib.has_some spec
                then
                  Queue.add
                    (fun () -> Annotations.register_funspec ~force:true new_kf)
                    self#get_filling_actions;
            | None -> () (* User has to register the new function *)
        end
      | GAnnot (na,_) when cond ->
	let e = match g with
          | GAnnot (a,_) -> Annotations.emitter_of_global a
          | _ -> Emitter.end_user
        in
        Queue.add
	  (fun () ->
            try
              (* Annotations might have already been added by the user. *)
              ignore (Annotations.emitter_of_global na)
            with Not_found ->
              Annotations.unsafe_add_global e na;
          )
          self#get_filling_actions
      | _ -> ()
    in
    let post_action g =
      Extlib.may self#set_current_func fundec;
      let spec = get_spec () in
      List.iter (fun g -> change_glob g spec) g;
      if has_kf then self#reset_current_kf();
      Extlib.may (fun _ -> self#reset_current_func ()) fundec;
      g
    in
    let post_change_to g =
      List.iter (fun g -> change_glob g None) g;
      if has_kf then self#reset_current_kf();
      g
    in
    let post_do_children f g =
      Extlib.may self#set_current_func fundec;
      make_funspec ();
      let res = f g in
      (* Spec registration is already handled at the vfunspec level. *)
      List.iter (fun g -> change_glob g None) res;
      if has_kf then self#reset_current_kf();
      Extlib.may (fun _ -> self#reset_current_func ()) fundec;
      res
    in
    match res with
    | SkipChildren ->
        change_glob g None;
        if has_kf then self#reset_current_kf();
        res
    | JustCopy -> JustCopyPost post_action
    | JustCopyPost f -> JustCopyPost (post_action $ f)
    | DoChildren -> DoChildrenPost (post_do_children Extlib.id)
    | DoChildrenPost f -> DoChildrenPost (post_do_children f)
    | ChangeTo l -> ChangeToPost (l,post_change_to)
    | ChangeToPost (l,f) -> ChangeToPost (l, post_change_to $ f)
    | ChangeDoChildrenPost (l,f) -> ChangeDoChildrenPost (l, post_do_children f)
end

class generic_frama_c_visitor bhv =
  let current_kf = ref None in
  let current_fundec = ref None in
  let queue = Queue.create () in
  internal_generic_frama_c_visitor current_fundec queue current_kf bhv

class frama_c_copy prj = generic_frama_c_visitor (copy_visit prj)

class frama_c_inplace =
  generic_frama_c_visitor (inplace_visit())

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

let visitFramacIdTerm  vis t =
  let t' = visitCilIdTerm (vis:>cilVisitor) t in
  vis#fill_global_tables; t'

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

let visitFramacModelInfo vis m =
  let m' = visitCilModelInfo (vis:>cilVisitor) m in
  vis#fill_global_tables; m'

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

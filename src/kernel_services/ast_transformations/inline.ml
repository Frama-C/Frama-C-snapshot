(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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
module Kernel_file = File

let dkey = Kernel.register_category "inline"

let wkey = Kernel.register_warn_category "inline"

let () = Parameter_customize.set_group Kernel.normalisation
module InlineCalls =
  Kernel.Kernel_function_set(struct
    let option_name = "-inline-calls"
    let module_name = "InlineCalls"
    let arg_name = "f1, ..., fn"
    let help = "inline calls to functions f1, ..., fn"
  end)

let () = Parameter_customize.set_group Kernel.normalisation
module RemoveInlined =
  Kernel.Kernel_function_set(struct
    let option_name = "-remove-inlined"
    let module_name = "RemoveInlined"
    let arg_name = "f1, ..., fn"
    let help = "remove inlined functions f1, ..., fn, which must have been \
                given to " ^ InlineCalls.option_name ^
               ". Warning: this does not check if the given functions were \
                fully inlined."
  end)

let inline_parameter_category = object
  method fold: 'a. (kernel_function -> 'a -> 'a) -> 'a -> 'a =
    fun f acc ->
    Globals.Functions.fold
      (fun kf acc ->
         let vi = Kernel_function.get_vi kf in
         match kf.fundec with
         | Definition _ -> if vi.vinline then f kf acc else acc
         | Declaration _ -> acc)
      acc
  method mem kf =
    Kernel_function.is_definition kf && (Kernel_function.get_vi kf).vinline
end

let _ =
  InlineCalls.Category.add "inline" [Globals.Functions.self]
    inline_parameter_category

let _ =
  RemoveInlined.Category.add "inline" [Globals.Functions.self]
    inline_parameter_category


let inline_call loc caller callee return args =
  let caller_fd = Kernel_function.get_definition caller in
  let caller_labels = ref (Kernel_function.find_all_labels caller) in
  let fresh_label lab =
    let (_,lab) =
      Extlib.make_unique_name
        (fun x -> Datatype.String.Set.mem x !caller_labels)
        ~sep:"_" ~start:0 lab
    in
    caller_labels:= Datatype.String.Set.add lab !caller_labels; lab
  in
  let o = object(self)
    inherit Visitor.frama_c_refresh (Project.current ())

    method! vvdec _ =
      Cil.DoChildrenPost (fun vi -> Cil.refresh_local_name caller_fd vi; vi)

    method! vvrbl v =
      if v.vglob then
        Cil.ChangeTo (Visitor_behavior.Get_orig.varinfo self#behavior v)
      else Cil.DoChildren

    method! vterm_lval (host,offset) =
      match host, return with
      | TResult _, Some lv ->
        let tlv = Logic_utils.lval_to_term_lval ~cast:false lv in
        let offset = Visitor.visitFramacTermOffset self offset in
        Cil.ChangeToPost
          (Logic_const.addTermOffsetLval offset tlv, Extlib.id)
      | TResult _, None ->
        Kernel.fatal
          "inlining non-void returning function without lval to store result"
      | _ -> Cil.DoChildren

    method! vfunc _ =
      self#set_current_kf caller;
      Cil.DoChildrenPost
        (fun fd ->
           caller_fd.slocals <-
             caller_fd.slocals @ fd.sformals @ fd.slocals;
           let add_init vi arg =
             vi.vdefined <- true;
             Cil.mkStmtOneInstr ~valid_sid:true
               (Local_init (vi,AssignInit (SingleInit arg),loc))
           in
           let inits = List.map2 add_init fd.sformals args in
           let spec = Annotations.funspec ~populate:false callee in
           if Cil.is_empty_funspec spec then begin
             fd.sbody.blocals <- fd.sformals @ fd.sbody.blocals;
             fd.sbody.bstmts <- inits @ fd.sbody.bstmts;
           end else begin
             (* put a statement contract on top of the function's body,
                but only after we have assigned the formals. Not that there
                is no need to rename behaviors: they will only shadow behaviors
                of the caller within callee's body, just as we need.
             *)
             let body = Cil.mkStmt ~valid_sid:true (Block fd.sbody) in
             let spec' = Visitor.visitFramacFunspec self spec in
             let ca = Logic_const.new_code_annotation (AStmtSpec([],spec')) in
             Annotations.add_code_annot Emitter.kernel ~kf:caller body ca;
             let new_body = Cil.mkBlock (inits @ [ body ]) in
             new_body.blocals <- fd.sformals;
             fd.sbody <- new_body
           end;
           fd)

    method !vstmt_aux _ =
      Cil.DoChildrenPost
        (fun stmt ->
           stmt.labels <-
             List.map
               (function
                 | Label (s,l,f) -> Label (fresh_label s,l,f)
                 | (Case _ | Default _) as lab -> lab)
               stmt.labels;
           (* Replace return by an assignment; or remove it if useless *)
           (match stmt.skind with
            | Return(exp, loc) ->
              let skind =
                match return, exp with
                | None, None  -> Instr (Skip loc)
                | None, Some exp ->
                  (* Keep the expression in case it could lead to an alarm *)
                  (Cil.mkPureExpr ~valid_sid:true ~fundec:caller_fd exp).skind
                | Some ret, Some exp -> Instr (Set(ret, exp, loc))
                | Some _, None ->
                  Kernel.fatal
                    "trying to assign the result of a void returning function"
              in
              stmt.skind <- skind
            | _ -> ());
           stmt)

    (* local statics are still owned by the original function, not the
       inlined version. *)
    method! vblock b = b.bstatics <- []; Cil.DoChildren
  end
  in
  let callee_fd =
    Visitor.visitFramacFunction o (Kernel_function.get_definition callee)
  in
  callee_fd.sbody

let is_variadic_function vi = match vi.vtype with
  | TFun(_, _, is_v, _) -> is_v
  | _ -> false

let inliner functions_to_inline = object (self)
  inherit Visitor.frama_c_inplace

  val call_stack = Stack.create ()
  val mutable already_visited = Cil_datatype.Kf.Set.empty
  val mutable block_stack = Stack.create ()
  val mutable inlined_calls = Cil_datatype.Stmt.Set.empty
  method private add_inlined_call s =
    inlined_calls <- Cil_datatype.Stmt.Set.add s inlined_calls

  method private recursive_call_limit kf =
    let nb_calls =
      Stack.fold
        (fun res kf' -> if Cil_datatype.Kf.equal kf kf' then res + 1 else res)
        0 call_stack
    in
    nb_calls >= 1 (* TODO: be more flexible. *)

  method! vblock b =
    Stack.push b block_stack;
    Cil.DoChildrenPost (fun b -> ignore (Stack.pop block_stack); b)

  (* inline the given [stmt], which must be a call, in the given [caller] *)
  method private inline stmt init_kind return f args =
    let callee =
      try Globals.Functions.get f
      with Not_found ->
        Kernel.fatal
          ~current:true "Expecting a function, got %a" Printer.pp_varinfo f
    in
    if Kernel_function.Set.mem callee functions_to_inline &&
       not (self#recursive_call_limit callee)
    then begin
      if is_variadic_function f then begin
        Kernel.warning ~wkey ~current:true ~once:true
          "Ignoring inlining option for variadic function %a"
          Printer.pp_varinfo f;
        Cil.DoChildren
      end else
        begin
          Stack.push callee call_stack;
          let loc = Cil_datatype.Stmt.loc stmt in
          let needs_assign, return_aux, args =
            match init_kind, return with
            | None, Some _ -> false, return, args
            | None, None ->
              let rt, _, _,_ = Cil.splitFunctionTypeVI f in
              if Cil.isVoidType rt then false,return, args
              else begin
                let scope = Stack.top block_stack in
                let v =
                  Cil.makeLocalVar
                    (Extlib.the self#current_func)
                    ~scope ~temp:true "__inline_tmp" rt
                in
                true, Some (Cil.var v), args
              end
            | Some Plain_func, Some lv ->
              let t = Cil.typeOfLval lv in
              let scope = Stack.top block_stack in
              let v =
                Cil.makeLocalVar
                  (Extlib.the self#current_func)
                  ~scope ~temp:true "__inline_tmp" t
              in
              true, Some (Cil.var v), args
            | Some Constructor, Some (Var r, NoOffset) ->
              (* Inlining will prevent r to be syntactically seen as initialized
                 or const: *)
              r.vdefined <- false;
              r.vtype <- Cil.typeRemoveAttributes ["const"] r.vtype;
              false, None, (Cil.mkAddrOf loc (Cil.var r)) :: args
            | Some _, _ ->
              Kernel.fatal "Attempt to initialize an inexistent varinfo"
          in
          let block =
            inline_call
              (Cil_datatype.Stmt.loc stmt)
              (Extlib.the self#current_kf)
              callee return_aux args
          in
          let skind =
            if needs_assign then begin
              match return_aux, return with
              | Some (Var aux, NoOffset), Some (Var r, NoOffset) ->
                let b =
                  Cil.mkBlockNonScoping [
                    Cil.mkStmt ~valid_sid:true (Block block);
                    Cil.mkStmtOneInstr ~valid_sid:true
                      (Local_init
                         (r, AssignInit (SingleInit (Cil.evar ~loc aux)),loc))]
                in
                Block b
              | Some (Var _, NoOffset), None ->
                (* the auxiliary variable is just here for translating
                   \result in case we have a function contract. However,
                   the result of the inlined function itself is ignored
                   by the caller.
                *)
                Block block
              | _ ->
                Kernel.fatal
                  "Unexpected lval while translating \
                   return instruction of inlined function "
            end else Block block
          in
          stmt.skind <- skind;
          self#add_inlined_call stmt;
          let do_after stmt = ignore (Stack.pop call_stack); stmt in
          (* Do not visit the body if the inlining has already been done in
             the current kf. Otherwise, each subsequent call to a recursive
             inlined function would be inlined once again. *)
          if Cil_datatype.Kf.Set.mem callee already_visited then
            Cil.ChangeToPost (stmt, do_after)
          else
            Cil.DoChildrenPost do_after
        end
    end else Cil.DoChildren

  method !vstmt_aux stmt =
    match stmt.skind with
    | UnspecifiedSequence _ ->
      Cil.DoChildrenPost
        (fun s ->
           s.skind <-
             (match s.skind with
              | UnspecifiedSequence l ->
                (* TODO: also adds writes/reads from inlined calls.
                   Probably requires a complete refactoring of unspecified
                   sequences though. *)
                UnspecifiedSequence
                  (List.map
                     (fun (s,w,r,t,stmts) ->
                        (s,w,r,t,
                         List.filter
                           (fun x ->
                              not (Cil_datatype.Stmt.Set.mem !x inlined_calls))
                           stmts))
                     l)
              | k -> k);
           s)
    | Instr(Call(return, f, args, _)) ->
      (match f.enode with
       | Lval (Var vi, NoOffset) ->
         self#inline stmt None return vi args
       | _ ->
         Kernel.warning ~wkey ~current:true ~once:true
           "Ignoring call via function pointer";
         Cil.DoChildren)
    | Instr(Local_init(v, ConsInit (f, args, kind), _)) ->
      self#inline stmt (Some kind) (Some (Cil.var v)) f args
    | _ -> Cil.DoChildren

  method! vfunc _ =
    Cil.DoChildrenPost
      (fun f ->
         let kf = Globals.Functions.get f.svar in
         already_visited <- Cil_datatype.Kf.Set.add kf functions_to_inline; f)

end

let remove_local_statics = object
  inherit Visitor.frama_c_inplace
  method! vblock b =
    b.bstatics <- List.filter
        (fun v -> not (Cil.hasAttribute Cabs2cil.fc_local_static v.vattr)) b.bstatics;
    Cil.DoChildren
  method! vvrbl v =
    v.vattr <- Cil.dropAttribute Cabs2cil.fc_local_static v.vattr;
    Cil.DoChildren
end

let remove_inlined_visitor fds_to_remove = object
  inherit Visitor.frama_c_inplace
  method! vglob_aux = function
    | GFun(fd, _) ->
      if Cil_datatype.Fundec.Set.mem fd fds_to_remove then begin
        List.iter (fun stmt ->
            Annotations.iter_code_annot
              (fun e annot -> Annotations.remove_code_annot e stmt annot) stmt
          ) fd.sallstmts;
        Globals.Functions.remove fd.svar;
        let kf = Globals.Functions.get fd.svar in
        List.iter (fun vi ->
            Globals.Vars.remove vi
          ) (Kernel_function.get_statics kf);
        Cil.ChangeTo []
      end
      else
        Cil.SkipChildren
    | _ -> Cil.SkipChildren
end

let inline_calls ast =
  if not (InlineCalls.is_empty ()) then begin
    let functions = InlineCalls.get() in
    Visitor.visitFramacFileSameGlobals (inliner functions) ast;
    Cil_datatype.Kf.Set.iter
      (fun kf -> ignore (Visitor.visitFramacKf remove_local_statics kf))
      functions;
    let fds_to_remove =
      Cil_datatype.Kf.Set.fold (fun kf acc ->
          if not (Cil_datatype.Kf.Set.mem kf functions) then begin
            Kernel.warning ~wkey:Kernel.wkey_cmdline
              "%s: function %a not given to %s, will not remove it"
              RemoveInlined.option_name Kernel_function.pretty kf
              InlineCalls.option_name;
            acc
          end else
            Cil_datatype.Fundec.Set.add (Kernel_function.get_definition kf) acc)
        (RemoveInlined.get ()) Cil_datatype.Fundec.Set.empty
    in
    File.reorder_custom_ast ast;
    Ast.mark_as_changed ();
    Cfg.clearFileCFG ~clear_id:false ast;
    Cfg.computeFileCFG ast;
    Visitor.visitFramacFile (remove_inlined_visitor fds_to_remove) ast;
    Ast.mark_as_changed ();
    Cfg.clearFileCFG ~clear_id:false ast;
    Cfg.computeFileCFG ast;
    Kernel.feedback ~dkey "inlining done"
  end

let inline_transform =
  Kernel_file.register_code_transformation_category "inlining"

let () =
  Kernel_file.add_code_transformation_after_cleanup
    ~deps:[(module InlineCalls.As_string:Parameter_sig.S)]
    inline_transform inline_calls

(* -------------------------------------------------------------------------- *)
(*                Inlining of predicates and logic functions                  *)
(* -------------------------------------------------------------------------- *)

open Cil_datatype

exception CannotInline

type inline_env = {
  (** Returns true for predicate and logic functions to be inlined. Other
      predicates and functions are left unchanged. *)
  inline: logic_info -> bool;

  (** logic argument of the predicate -> term that replaces it, plus the label
      at which it must be evaluated. *)
  map_param: (term * logic_label) Logic_var.Map.t;

  (** logic label of the predicate -> label at call site *)
  map_label: logic_label Logic_label.Map.t;

  (** predicates and functions already inlined once, to prevent loops on
      recursive definitions *)
  already_seen: Logic_info.Set.t;

  (** current default label, Here at the beginning *)
  curr_label: logic_label;
}

(* Specification of the following inliner: the resulting term/predicate
   contains only Papp or Tapp nodes for which [env.inline] does not hold.
   Hence, an evaluation engine that understands these functions and predicates
   can fully evaluate the inlined predicate/term.

   To implement this specification, we fail eagerly when encountering a
   recursive definition, or a fully unknown predicate/function without a body.*)

(**  Visitors for inlining defined predicates and logical function *)
class logic_inliner env = object (self)
  inherit Visitor.frama_c_copy(Project.current())

  method! vlogic_label label =
    match Logic_label.Map.find label env.map_label with
    | exception Not_found -> Cil.JustCopy
    | x -> Cil.ChangeTo x

  method private inline_label l =
    try Logic_label.Map.find l env.map_label with Not_found -> l

  method private inline_labels labels =
    List.map self#inline_label labels

  method private inline_args args =
    let one t = Visitor.visitFramacTerm (self :> Visitor.frama_c_copy) t in
    List.map one args

  (* Builds the environnement to enter into the body of [li] with actual
     labels and arguments [labels] and [args] respectively. Assumes that
     both have already been inlined. *)
  method private new_env li args labels =
    let map_param =
      List.fold_left2
        (fun m v t -> Logic_var.Map.add v (t, env.curr_label) m)
        Logic_var.Map.empty li.l_profile args
    in
    let map_label =
      List.fold_left2
        (fun m l1 l2 -> Logic_label.Map.add l1 l2 m)
        Logic_label.Map.empty li.l_labels labels
    in {
      env with (* only inline and curr_label is kept *)
      map_param;
      map_label;
      already_seen = Logic_info.Set.add li env.already_seen;
    }

  (* 'Freeze' [off] so that its evaluation occurs at the given label.
      Does something only for [TIndex]. *)
  method private freeze_off off lbl =
    match off with
    | TNoOffset -> TNoOffset
    | TField (fi, o) -> TField (fi, self#freeze_off o lbl)
    | TModel (mi, o) -> TModel (mi, self#freeze_off o lbl)
    | TIndex (i, o) ->
      let i' = Logic_const.tat ~loc:i.term_loc (i, lbl) in
      TIndex (i', self#freeze_off o lbl)

  method private add_at t lbl =
    if Logic_label.equal lbl env.curr_label
    then t
    else Logic_const.tat ~loc:t.term_loc (t, lbl)

  method! vterm t =
    match t.term_node with
    | Tat (t', l) ->
      let l = self#inline_label l in
      let vis = new logic_inliner { env with curr_label = l } in
      let t' = Visitor.visitFramacTerm vis t' in
      Cil.ChangeTo (self#add_at t' l)
    | TLval (TVar v, TNoOffset) -> begin
        match Logic_var.Map.find v env.map_param with
        | exception Not_found -> Cil.JustCopy
        | (x, lbl)  ->
          (* Replace [v] by [x], making sure it is evaluated at [lbl] *)
          Cil.ChangeTo (self#add_at x lbl)
      end
    | TLval (TVar v, off) -> begin
        match Logic_var.Map.find v env.map_param with
        | exception Not_found -> Cil.DoChildren
        | (x, lbl)  ->
          match x.term_node with
          | TLval (h, offx) ->
            (* First, inline inside [off] *)
            let off =
              Visitor.visitFramacTermOffset (self :> Visitor.frama_c_copy) off
            in
            (* We need to compute [h ++ offx ++ off] with the proper labels.
               [h ++ offx] must be evaluated at [lbl], [off] at the current
               label. Thus, we build [\at (h ++ offx ++ \at(off, cur), lbl)]. *)
            let off' =
              if Logic_label.equal env.curr_label lbl then off
              else self#freeze_off off env.curr_label
            in
            let offx' = Logic_const.addTermOffset off' offx in
            let x' = {t with term_node = TLval(h, offx')} in
            Cil.ChangeTo (self#add_at x' lbl)
          | _ -> (* we cannot handle casts on aggregates, functions
                    returning aggregates, etc, because we cannot buid
                    the corresponding term without a TLet *)
            raise CannotInline
      end
    | Tapp({ l_body = LBterm body } as li, labels, args) -> begin
        if Logic_info.Set.mem li env.already_seen then raise CannotInline;
        let args =   self#inline_args args in
        let labels = self#inline_labels labels in
        if env.inline li
        then
          let env = self#new_env li args labels in
          let vis = new logic_inliner env in
          Cil.ChangeTo (Visitor.visitFramacTerm vis body)
        else Cil.ChangeTo {t with term_node = Tapp(li, labels, args) }
      end
    | Tapp (li, _, _) when env.inline li ->
      raise CannotInline
    | _ -> Cil.DoChildren

  method! vpredicate p =
    match p.pred_content with
    | Pat (p', l) ->
      let l = self#inline_label l in
      let vis = new logic_inliner { env with curr_label = l } in
      let p' = Visitor.visitFramacPredicate vis p' in
      Cil.ChangeTo (Logic_const.pat ~loc:p.pred_loc (p', l))
    | Papp ({ l_body = LBpred body } as li, labels, args) -> begin
        if Logic_info.Set.mem li env.already_seen then raise CannotInline;
        let args =   self#inline_args args in
        let labels = self#inline_labels labels in
        if env.inline li
        then
          let env = self#new_env li args labels in
          let vis = new logic_inliner env in
          Cil.ChangeTo (Visitor.visitFramacPredicate vis body)
        else Cil.ChangeTo {p with pred_content = Papp(li, labels, args) }
      end
    | Papp (li, _, _) when env.inline li ->
      raise CannotInline
    | _ -> Cil.DoChildren

end

let inliner curr_label inline =
  new logic_inliner {
    inline;
    map_param = Logic_var.Map.empty;
    map_label = Logic_label.Map.empty;
    already_seen = Logic_info.Set.empty;
    curr_label = curr_label;
  }

let inline_term ~inline ?(current = BuiltinLabel Here) term =
  let current_loc = Cil_const.CurrentLoc.get () in
  try Some (Visitor.visitFramacTerm (inliner current inline) term)
  with CannotInline ->
    (* The visitor changes and resets the reference to the current location.
       If an exception is raised during the visit, the current location must be
       reset by the caller. *)
    Cil_const.CurrentLoc.set current_loc;
    None

let inline_predicate ~inline ?(current = BuiltinLabel Here) pred =
  let current_loc = Cil_const.CurrentLoc.get () in
  try Some (Visitor.visitFramacPredicate (inliner current inline) pred)
  with CannotInline ->  Cil_const.CurrentLoc.set current_loc; None

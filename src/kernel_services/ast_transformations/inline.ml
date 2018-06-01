(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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
        Cil.ChangeTo (Cil.get_original_varinfo self#behavior v)
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
      Transitioning.Stack.fold
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

let inline_calls ast =
  if not (InlineCalls.is_empty ()) then begin
    let functions = InlineCalls.get() in
    Visitor.visitFramacFileSameGlobals (inliner functions) ast;
    File.reorder_custom_ast ast;
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

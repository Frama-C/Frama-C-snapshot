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

module E_acsl_label = Label
open Cil_types
open Cil_datatype

let dkey = Options.dkey_translation

(* ************************************************************************** *)
(* Visitor *)
(* ************************************************************************** *)

(* local references to the below visitor and to [do_visit] *)
let function_env = ref Env.dummy
let dft_funspec = Cil.empty_funspec ()
let funspec = ref dft_funspec

(* extend the environment with statements which allocate/deallocate memory
   blocks *)
module Memory: sig
  val store: ?before:stmt -> Env.t -> kernel_function -> varinfo list -> Env.t
  val duplicate_store:
    ?before:stmt -> Env.t -> kernel_function -> Varinfo.Set.t -> Env.t
  val delete_from_list:
    ?before:stmt -> Env.t -> kernel_function -> varinfo list -> Env.t
  val delete_from_set:
    ?before:stmt -> Env.t -> kernel_function -> Varinfo.Set.t -> Env.t
end = struct

  let tracking_stmt ?before fold mk_stmt env kf vars =
    fold
      (fun vi env ->
      if Mmodel_analysis.must_model_vi ~kf vi then
        let vi = Cil.get_varinfo (Env.get_behavior env) vi in
          Env.add_stmt ?before env (mk_stmt vi)
      else
        env)
      vars
    env

  let store ?before env kf vars =
    tracking_stmt
      ?before
      List.fold_right (* small list *)
      Misc.mk_store_stmt
    env
      kf
      vars

  let duplicate_store ?before env kf vars =
    tracking_stmt
      ?before
      Varinfo.Set.fold
      Misc.mk_duplicate_store_stmt
      env
      kf
      vars

  let delete_from_list ?before env kf vars =
    tracking_stmt
      ?before
      List.fold_right (* small list *)
      Misc.mk_delete_stmt
      env
      kf
      vars

  let delete_from_set ?before env kf vars =
    tracking_stmt
      ?before
      Varinfo.Set.fold
      Misc.mk_delete_stmt
      env
      kf
      vars

end

(* the main visitor performing e-acsl checking and C code generator *)
class e_acsl_visitor prj generate = object (self)

  inherit Visitor.generic_frama_c_visitor
    (if generate then Cil.copy_visit prj else Cil.inplace_visit ())

  val mutable main_fct = None
  (* fundec of the main entry point, in the new project [prj].
     [None] while the global corresponding to this fundec has not been
     visited *)

  val mutable is_initializer = false
  (* Global flag set to [true] if a currently visited node
     belongs to a global initializer and set to [false] otherwise *)

  val global_vars: (offset * init option) Varinfo.Hashtbl.t =
    Varinfo.Hashtbl.create 7
  (* Hashtable mapping global variables (as Cil_type.varinfo) to their
     initializers aiming to capture memory allocated by global variable
     declarations and initialization. At runtime the memory blocks
     corresponding to space occupied by global are recorded via a call to
     [__gen_e_acsl_globals_init] instrumented at the beginning of the
     [main] function. Each variable stored by [global_vars] will be handled in
     the body of [__gen_e_acsl_globals_init] as follows:
     __e_acsl_store_block(...); // Record a memory block used by the variable
     __e_acsl_full_init(...);   // ... and mark it as initialized memory

     NOTE: In [global_vars] keys belong to the original project while values
     belong to the new one *)

  method private reset_env () =
    function_env := Env.empty (self :> Visitor.frama_c_visitor)

  method !vfile _f =
    (* copy the options used during the visit in the new project: it is the
       right place to do this: it is still before visiting, but after
       that the visitor internals reset all of them :-(. *)
    let cur = Project.current () in
    let selection =
      State_selection.of_list
        [ Options.Gmp_only.self; Options.Check.self; Options.Full_mmodel.self;
          Kernel.SignedOverflow.self; Kernel.UnsignedOverflow.self;
          Kernel.SignedDowncast.self; Kernel.UnsignedDowncast.self;
          Kernel.Machdep.self ]
    in
    if generate then Project.copy ~selection ~src:cur prj;
    Cil.DoChildrenPost
      (fun f ->
        (* extend [main] with forward initialization and put it at end *)
        if generate then begin
          let must_init =
            not (Literal_strings.is_empty ())
            ||
              try
                Varinfo.Hashtbl.iter
                  (fun old_vi (_, i) -> match i with None | Some _ ->
                    if Mmodel_analysis.must_model_vi old_vi then raise Exit)
                  global_vars;
                false
              with Exit ->
                true
          in
          if must_init then begin
            let build_initializer () =
              Options.feedback ~dkey ~level:2 "building global initializer.";
              let env = Env.push !function_env in
              let stmts, env =
                Varinfo.Hashtbl.fold_sorted
                  (fun old_vi (_, i) (stmts, env) ->
                    let new_vi = Cil.get_varinfo self#behavior old_vi in
                    (* [model] creates an initialization statement
                       of the form [__e_acsl_full_init(...)] for every global
                       variable which needs to be tracked and is not a Frama-C
                       builtin. Further the statement is appended to the
                       provided list of statements ([blk]) *)
                    let model blk =
                      if Mmodel_analysis.must_model_vi old_vi then
                        let blk =
                          if Kernel.LibEntry.get () then blk
                          else
                            Misc.mk_initialize ~loc:Location.unknown
                              (Cil.var new_vi)
                            :: blk
                        in
                        Misc.mk_store_stmt new_vi :: blk
                      else
                        stmts
                    in
                    match i with
                    | None -> model stmts, env
                    | Some (CompoundInit _) -> assert false
                    | Some (SingleInit e) ->
                      let _, env = self#literal_string env e in stmts, env)
                  global_vars
                  ([ ], env)
              in
              let stmts =
                (* literal strings initialization *)
                Literal_strings.fold
                  (fun s vi stmts ->
                    let loc = Location.unknown in
                    let e = Cil.new_exp ~loc:loc (Const (CStr s)) in
                    let str_size = Cil.new_exp loc (SizeOfStr s) in
                    Cil.mkStmtOneInstr ~valid_sid:true (Set(Cil.var vi, e, loc))
                    :: Misc.mk_store_stmt ~str_size vi
                    :: Misc.mk_full_init_stmt ~addr:false vi
                    :: Misc.mk_mark_readonly vi
                    :: stmts)
                  stmts
              in
              let return =
                Cil.mkStmt ~valid_sid:true (Return(None, Location.unknown)) in
              (* Generate init statements for temporal analysis *)
              let tinit_stmts = Varinfo.Hashtbl.fold
                (fun vi (off, init) acc ->
                  match init with
                  | Some init ->
                    let stmt = Temporal.generate_global_init vi off init env in
                      (match stmt with | Some stmt -> stmt :: acc | None -> acc)
                  | None -> acc)
                global_vars
                [return]
              in
              let stmts = stmts @ tinit_stmts in
              (* Create a new code block with generated statements *)
              let (b, env), stmts = match stmts with
                | [] -> assert false
                | stmt :: stmts ->
                  Env.pop_and_get env stmt ~global_clear:true Env.Before, stmts
              in
              function_env := env;
              let stmts = Cil.mkStmt ~valid_sid:true (Block b) :: stmts in
              let blk = Cil.mkBlock stmts in
              (* Create [__e_acsl_globals_init] function with definition
               for initialization of global variables *)
              let fname = (Misc.mk_api_name "globals_init") in
              let vi =
                Cil.makeGlobalVar ~source:true
                  fname
                  (TFun(Cil.voidType, Some [], false, []))
              in
              vi.vdefined <- true;
              (* There is no contract associated with the function *)
              let spec = Cil.empty_funspec () in
              (* Create function definition which has the list of the
               * generated initialization statements *)
              let fundec =
                { svar = vi;
                  sformals = [];
                  slocals = [];
                  smaxid = 0;
                  sbody = blk;
                  smaxstmtid = None;
                  sallstmts = [];
                  sspec = spec }
              in
              self#add_generated_variables_in_function fundec;
              let fct = Definition(fundec, Location.unknown) in
              (* Create and register [__e_acsl_globals_init] as kernel
                 function *)
              let kf =
                { fundec = fct; spec = spec }
              in
              Globals.Functions.register kf;
              Globals.Functions.replace_by_definition
                spec fundec Location.unknown;
              let cil_fct = GFun(fundec, Location.unknown) in
              if Mmodel_analysis.use_model () then
                match main_fct with
                | Some main ->
                  let exp = Cil.evar ~loc:Location.unknown vi in
                  (* Create [__e_acsl_globals_init();] call *)
                  let stmt =
                    Cil.mkStmtOneInstr ~valid_sid:true
                      (Call(None, exp, [], Location.unknown))
                  in
                  vi.vreferenced <- true;
                  (* Add [__e_acsl_globals_init();] call as the first statement
                   to the [main] function *)
                  main.sbody.bstmts <- stmt :: main.sbody.bstmts;
                  let new_globals =
                    List.fold_right
                      (fun g acc -> match g with
                      | GFun({ svar = vi }, _)
                          when Varinfo.equal vi main.svar ->
                        acc
                      | _ -> g :: acc)
                      f.globals
                      [ cil_fct; GFun(main, Location.unknown) ]
                  in
                  (* add the literal string varinfos as the very first
                     globals *)
                  let new_globals =
                    Literal_strings.fold
                      (fun _ vi l ->
                        GVar(vi, { init = None }, Location.unknown) :: l)
                      new_globals
                  in
                  f.globals <- new_globals
                | None ->
                  Kernel.warning "@[no entry point specified:@ \
you must call function `%s' and `__e_acsl_memory_clean by yourself.@]"
                    fname;
                  f.globals <- f.globals @ [ cil_fct ]
            in
            Project.on prj build_initializer ()
          end; (* must_init *)
          (* Add a call to [__e_acsl_memory_init] that initializes memory
             storage and potentially records program arguments. Parameters to
             [__e_acsl_memory_init] are addresses of program arguments or
             NULLs if [main] is declared without arguments. *)
          let build_mmodel_initializer () =
            let loc = Location.unknown in
            let nulls = [ Cil.zero loc ; Cil.zero loc ] in
            let handle_main main =
              let args =
                (* record arguments only if the second has a pointer type, so a
                   argument strings can be recorded. This is sufficient to
                   capture C99 compliant arguments and GCC extensions with
                   environ. *)
                match main.sformals with
                | [] ->
                  (* no arguments to main given *)
                  nulls
                | _argc :: argv :: _ when Cil.isPointerType argv.vtype ->
                  (* grab addresses of arguments for a call to the main
                     initialization function, i.e., [__e_acsl_memory_init] *)
                  List.map Cil.mkAddrOfVi main.sformals;
                | _ :: _ ->
                  (* some non-standard arguments. *)
                  nulls
              in
              let ptr_size = Cil.sizeOf loc Cil.voidPtrType in
              let args = args @ [ ptr_size ] in
              let init = Misc.mk_call loc (Misc.mk_api_name "memory_init") args in
              main.sbody.bstmts <- init :: main.sbody.bstmts
            in
            Extlib.may handle_main main_fct
          in
          Project.on prj build_mmodel_initializer ();
          (* reset copied states at the end to be observationally
              equivalent to a standard visitor. *)
          Project.clear ~selection ~project:prj ();
        end; (* generate *)
        f)

  method !vglob_aux = function
  | GVarDecl(vi, _) | GVar(vi, _, _)
  | GFunDecl(_, vi, _) | GFun({ svar = vi }, _)
      when Misc.is_library_loc vi.vdecl || Builtins.mem vi.vname ->
    if generate then
      Cil.JustCopyPost
        (fun l ->
          let new_vi = Cil.get_varinfo self#behavior vi in
          Misc.register_library_function new_vi;
          Builtins.update vi.vname new_vi;
          l)
    else begin
      Misc.register_library_function vi;
      Cil.SkipChildren
    end
  | GVarDecl(vi, _) | GVar(vi, _, _) | GFun({ svar = vi }, _)
      when Cil.is_builtin vi ->
    if generate then Cil.JustCopy else Cil.SkipChildren
  | g when Misc.is_library_loc (Global.loc g) ->
    if generate then Cil.JustCopy else Cil.SkipChildren
  | g ->
    let do_it = function
      | GVar(vi, _, _) ->
        vi.vghost <- false
      | GFun({ svar = vi } as fundec, _) ->
        vi.vghost <- false;
        Builtins.update vi.vname vi;
        (* remember that we have to remove the main later (see method
           [vfile]) *)
        if vi.vorig_name = Kernel.MainFunction.get () then
          main_fct <- Some fundec
      | GVarDecl(vi, _) | GFunDecl(_, vi, _) ->
        (* do not convert extern ghost variables, because they can't be linked,
           see bts #1392 *)
        if vi.vstorage <> Extern then
          vi.vghost <- false
      | _ ->
        ()
    in
    (match g with
    | GVar(vi, _, _) | GVarDecl(vi, _) | GFun({ svar = vi }, _)
      (* Track function addresses but the main function that is tracked
         internally via RTL *)
        when vi.vorig_name <> Kernel.MainFunction.get () ->
      (* Make a unique mapping for each global variable omitting initializers.
       Initializers (used to capture literal strings) are added to
       [global_vars] via the [vinit] visitor method (see comments below). *)
      Varinfo.Hashtbl.replace global_vars vi (NoOffset, None)
    | _ -> ());
    if generate then Cil.DoChildrenPost(fun g -> List.iter do_it g; g)
    else Cil.DoChildren

  (* Add mappings from global variables to their initializers in [global_vars].
     Note that the below function captures only [SingleInit]s. All compound
     initializers containing SingleInits (except for empty compound
     initializers) are unrapped and thrown away. *)
  method !vinit vi off _ =
    if generate then
      if Mmodel_analysis.must_model_vi vi then begin
        is_initializer <- vi.vglob;
        Cil.DoChildrenPost
          (fun i ->
            (match is_initializer with
            (* Note the use of [add] instead of [replace]. This is because a
             single variable can be associated with multiple initializers
             and all of them need to be captured. *)
            | true ->
              (match i with
              (* Case of an empty CompoundInit, treat it as if there were
               * no initializer at all *)
              | CompoundInit(_,[]) -> ()
              | CompoundInit(_,_) | SingleInit _ ->
                Varinfo.Hashtbl.add global_vars vi (off, (Some i)))
            | false-> ());
            is_initializer <- false;
          i)
      end else
        Cil.JustCopy
    else
      Cil.SkipChildren

  method !vvdec vi =
    (try
       let old_vi = Cil.get_original_varinfo self#behavior vi in
       let old_kf = Globals.Functions.get old_vi in
       funspec :=
         Cil.visitCilFunspec
         (self :> Cil.cilVisitor)
         (Annotations.funspec old_kf)
     with Not_found ->
       ());
    Cil.SkipChildren

  method private add_generated_variables_in_function f =
    assert generate;
    let vars = Env.get_generated_variables !function_env in
    self#reset_env ();
    let locals, blocks =
      List.fold_left
        (fun (local_vars, block_vars as acc) (v, scope) -> match scope with
        | Env.Global -> acc
        | Env.Function -> v :: local_vars, v :: block_vars
        | Env.Local_block -> v :: local_vars, block_vars)
        (f.slocals, f.sbody.blocals)
        vars
    in
    f.slocals <- locals;
    f.sbody.blocals <- blocks

  method !vfunc f =
    if generate then begin
      Exit_points.generate f;
      let kf = Extlib.the self#current_kf in
      Options.feedback ~dkey ~level:2 "entering in function %a."
        Kernel_function.pretty kf;
      List.iter (fun vi -> vi.vghost <- false) f.slocals;
      Cil.DoChildrenPost
        (fun f ->
          Exit_points.clear ();
          self#add_generated_variables_in_function f;
          Options.feedback ~dkey ~level:2 "function %a done."
            Kernel_function.pretty kf;
          f)
    end else
      Cil.DoChildren

  method private is_return old_kf stmt =
    let old_ret =
      try Kernel_function.find_return old_kf
      with Kernel_function.No_Statement -> assert false
    in
    Stmt.equal stmt (Cil.get_stmt self#behavior old_ret)

  method private is_first_stmt old_kf stmt =
    try
      Stmt.equal
        (Cil.get_original_stmt self#behavior stmt)
        (Kernel_function.find_first_stmt old_kf)
    with Kernel_function.No_Statement ->
      assert false

  method private is_main old_kf =
    try
      let main, _ = Globals.entry_point () in
      Kernel_function.equal old_kf main
    with Globals.No_such_entry_point _s ->
      (* [JS 2013/05/21] already a warning in pre-analysis *)
      (*      Options.warning ~once:true "%s@ \
              @[The generated program may be incomplete.@]"
              s;*)
      false

  method private literal_string env e =
    assert generate;
    let env_ref = ref env in
    let o = object
      inherit Cil.genericCilVisitor (Cil.copy_visit (Project.current ()))
      method !vexpr e = match e.enode with
      (* the guard below could be optimized: if no annotation **depends on this
         string**, then it is not required to monitor it.
         (currently, the guard says: "no annotation uses the memory model" *)
      | Const(CStr s) when Mmodel_analysis.use_model () ->
        (try
           let vi = Literal_strings.find s in
           (* if the literal string was already created, just get it. *)
           let exp = Cil.evar ~loc:e.eloc vi in
           Cil.ChangeTo exp
         with Not_found ->
           (* never seen this string before: replace it by a new global var *)
           let loc = e.eloc in
           let vi, exp, env =
             Env.new_var
               ~loc
               ~scope:Env.Global
               ~name:"literal_string"
               env
               None
               Cil.charPtrType
               (fun _ _ -> [] (* done in the initializer, see {!vglob_aux} *))
           in
           Literal_strings.add s vi;
           env_ref := env;
           Cil.ChangeTo exp)
      | _ ->
        Cil.DoChildren
    end in
    let e = Cil.visitCilExpr o e in
    e, !env_ref

  method !vstmt_aux stmt =
    Options.debug ~level:4 "proceeding stmt (sid %d) %a@."
      stmt.sid Stmt.pretty stmt;
    let kf = Extlib.the self#current_kf in
    let is_main = self#is_main kf in
    let env = Env.push !function_env in
    let env = match stmt.skind with
      | Loop _ -> Env.push_loop env
      | _ -> env
    in
    let env =
      if self#is_first_stmt kf stmt then
        (* JS: should be done in the new project? *)
        let env =
          if generate && not is_main then
            let env = Memory.store env kf (Kernel_function.get_formals kf) in
            Temporal.handle_function_parameters kf env
          else
            env
        in
        (* translate the precondition of the function *)
        if Dup_functions.is_generated (Extlib.the self#current_kf) then
          Project.on prj (Translate.translate_pre_spec kf Kglobal env) !funspec
        else
          env
      else
        env
    in

    let env, new_annots =
      Annotations.fold_code_annot
        (fun _ old_a (env, new_annots) ->
          let a =
            (* [VP] Don't use Visitor here, as it will fill the queue in the
               middle of the computation... *)
            Cil.visitCilCodeAnnotation (self :> Cil.cilVisitor) old_a
          in
          let env =
            Project.on
              prj
              (Translate.translate_pre_code_annotation kf stmt env)
              a
          in
          env, a :: new_annots)
        (Cil.get_original_stmt self#behavior stmt)
        (env, [])
    in

    (* Add [__e_acsl_store_duplicate] calls for local variables which
     * declarations are bypassed by gotos. Note: should be done before
     * [vinst] method (which adds initializers) is executed, otherwise
     * init calls appear before store calls. *)
    let duplicates = Exit_points.store_vars stmt in
    let env =
      if generate then Memory.duplicate_store ~before:stmt env kf duplicates
      else env
    in
    function_env := env;

    let mk_block stmt =
      (* be careful: since this function is called in a post action, [env] has
         been modified from the time where pre actions have been executed.
         Use [function_env] to get it back. *)
      let env = !function_env in
      let env =
        if generate then
          (* Add temporal analysis instrumentations *)
          let env = Temporal.handle_stmt stmt env in
          (* Add initialization statements and store_block statements stemming
             from Local_init *)
            self#handle_instructions stmt env kf
        else
          env
      in
      let env =
        if stmt.ghost && generate then begin
          stmt.ghost <- false;
          (* translate potential RTEs of ghost code *)
          let rtes = Rte.stmt ~warn:false kf stmt in
          Translate.translate_rte_annots Printer.pp_stmt stmt kf env rtes
        end else
          env
      in
      let mk_post_env env =
        (* [fold_right] to preserve order of generation of pre_conditions *)
        Project.on
          prj
          (List.fold_right
             (fun a env ->
               Translate.translate_post_code_annotation kf stmt env a)
             new_annots)
          env
      in
      (* handle loop invariants *)
      let new_stmt, env, must_mv = Loops.preserve_invariant prj env kf stmt in
      let new_stmt, env =
        (* Remove local variables which scopes ended via goto/break/continue. *)
        let del_vars = Exit_points.delete_vars stmt in
        let env =
          if generate then Memory.delete_from_set ~before:stmt env kf del_vars
          else env
        in
        if self#is_return kf stmt then
          (* must generate the post_block before including [stmt] (the 'return')
             since no code is executed after it. However, since this statement
             is pure (Cil invariant), that is semantically correct. *)
          let env = mk_post_env env in
          (* also handle the postcondition of the function and clear the env *)
          let env =
            if Dup_functions.is_generated (Extlib.the self#current_kf) then
              Project.on
                prj
                (Translate.translate_post_spec kf Kglobal env)
                !funspec
            else
              env
          in
          (* de-allocating memory previously allocating by the kf *)
          (* JS: should be done in the new project? *)
          if generate then
            (* Remove recorded function arguments *)
            let fargs = Kernel_function.get_formals kf in
            let env =
              if generate then Memory.delete_from_list env kf fargs
              else env
            in
            let b, env =
              Env.pop_and_get env new_stmt ~global_clear:true Env.After
            in
            if is_main && Mmodel_analysis.use_model () then begin
              let stmts = b.bstmts in
              let l = List.rev stmts in
              match l with
              | [] -> assert false (* at least the 'return' stmt *)
              | ret :: l ->
                let loc = Stmt.loc stmt in
                let delete_stmts =
                  Varinfo.Hashtbl.fold_sorted
                    (fun old_vi (_, i) acc ->
                      if Mmodel_analysis.must_model_vi old_vi then
                        let new_vi = Cil.get_varinfo self#behavior old_vi in
                        (* Since there are multiple entries for same variables
                         do delete only variables without initializers, this
                         ensures that each variable is released once only *)
                         (match i with
                         | None -> Misc.mk_delete_stmt new_vi :: acc
                         | Some _ -> acc)
                      else
                        acc)
                    global_vars
                    [ Misc.mk_call ~loc (Misc.mk_api_name "memory_clean") [];
                      ret ]
                in
                b.bstmts <- List.rev l @ delete_stmts
            end;
            let new_stmt = Misc.mk_block prj stmt b in
            (* move the labels of the return to the new block in order to
               evaluate the postcondition when jumping to them. *)
            E_acsl_label.move
              (self :> Visitor.generic_frama_c_visitor) stmt new_stmt;
            new_stmt, env
          else
            stmt, env
        else (* i.e. not (is_return stmt) *)
          if generate then begin
            (* must generate [pre_block] which includes [stmt] before generating
               [post_block] *)
            let pre_block, env =
              Env.pop_and_get
                ~split:true
                env
                new_stmt
                ~global_clear:false
                Env.After
            in
            let env = mk_post_env (Env.push env) in
            let post_block, env =
              Env.pop_and_get
                env
                (Misc.mk_block prj new_stmt pre_block)
                ~global_clear:false
                Env.Before
            in
            let post_block = Cil.transient_block post_block in
            Misc.mk_block prj new_stmt post_block, env
          end else
            stmt, env
      in
      if must_mv then Loops.mv_invariants env ~old:new_stmt stmt;
      function_env := env;
      Options.debug ~level:4
      "@[new stmt (from sid %d):@ %a@]" stmt.sid Printer.pp_stmt new_stmt;
      if generate then new_stmt else stmt
    in
    Cil.ChangeDoChildrenPost(stmt, mk_block)

  method private handle_instructions stmt env kf =
    let add_initializer loc ?vi lv ?(post=false) stmt env kf =
      assert generate;
      let may_safely_ignore = function
        | Var vi, NoOffset -> vi.vglob || vi.vformal
        | _ -> false
      in
      if not (may_safely_ignore lv) && Mmodel_analysis.must_model_lval ~stmt ~kf lv then
        let before = Cil.memo_stmt self#behavior stmt in
        let new_stmt = Project.on prj (Misc.mk_initialize ~loc) lv in
        let new_stmt = Cil.memo_stmt self#behavior new_stmt in
        let env = Env.add_stmt ~post ~before env new_stmt in
        let env = match vi with
          | None -> env
          | Some vi ->
            let new_stmt = Project.on prj Misc.mk_store_stmt vi in
            let new_stmt = Cil.memo_stmt self#behavior new_stmt in
            Env.add_stmt ~post ~before env new_stmt
        in
        env
      else
        env
    in
    match stmt.skind with
    | Instr(Set(lv, _, loc)) -> add_initializer loc lv stmt env kf
    | Instr(Local_init(vi, _, loc)) ->
      let lv = (Var(vi), NoOffset) in
      add_initializer loc ~vi lv ~post:true stmt env kf
    | Instr(Call (Some lv, _, _, loc)) ->
      if not (Misc.is_generated_kf kf) then
        add_initializer loc lv ~post:false stmt env kf
      else env
    | _ -> env

  method !vblock blk =
    let handle_memory new_blk =
      match new_blk.blocals with
      | [] -> new_blk
      | _ :: _ ->
        let kf = Extlib.the self#current_kf in
        let add_locals stmts =
          List.fold_left
            (fun acc vi ->
              if Mmodel_analysis.must_model_vi ~bhv:self#behavior ~kf vi then
                Misc.mk_delete_stmt vi :: acc
              else
                acc)
            stmts
            new_blk.blocals
        in
        let rec insert_in_innermost_last_block blk = function
          | { skind = Return _ } as ret :: ((potential_clean :: tl) as l) ->
            (* keep the return (enclosed in a generated block) at the end;
               preceded by clean if any *)
            let init, tl =
              if self#is_main kf && Mmodel_analysis.use_model () then
                [ potential_clean; ret ], tl
              else
                [ ret ], l
            in
            blk.bstmts <-
              List.fold_left (fun acc v -> v :: acc) (add_locals init) tl
          | { skind = Block b } :: _ ->
            insert_in_innermost_last_block b (List.rev b.bstmts)
          | l -> blk.bstmts <-
            List.fold_left (fun acc v -> v :: acc) (add_locals []) l
        in
        insert_in_innermost_last_block new_blk (List.rev new_blk.bstmts);
        new_blk.bstmts <-
          List.fold_left
          (fun acc vi ->
            if Mmodel_analysis.must_model_vi vi && not vi.vdefined then
              let vi = Cil.get_varinfo self#behavior vi in
              Misc.mk_store_stmt vi :: acc
            else acc)
          new_blk.bstmts
          blk.blocals;
        new_blk
    in
    if generate then Cil.DoChildrenPost handle_memory else Cil.DoChildren

  (* Processing expressions for the purpose of replacing literal strings found
   in the code with variables generated by E-ACSL. *)
  method !vexpr _ =
    if generate then begin
      match is_initializer with
      (* Do not touch global initializers because they accept only constants *)
      | true -> Cil.DoChildren
      (* Replace literal strings elsewhere *)
      | false -> Cil.DoChildrenPost
        (fun e ->
          let e, env = self#literal_string !function_env e in
          function_env := env;
          e)
    end else
      Cil.SkipChildren

  method !vterm _ =
    Cil.DoChildrenPost
      (fun t ->
        (match t.term_node with
        | Tat(_, StmtLabel s_ref) ->
          (* the label may have been moved,
             so move the corresponding reference *)
          s_ref := E_acsl_label.new_labeled_stmt !s_ref
        | _ -> ());
        t)

  method !vpredicate_node _ =
    Cil.DoChildrenPost
      (function
      | Pat(_, StmtLabel s_ref) as p ->
          (* the label may have been moved,
             so move the corresponding reference *)
        s_ref := E_acsl_label.new_labeled_stmt !s_ref;
        p
      | p -> p);

  initializer
    Misc.reset ();
    Literal_strings.reset ();
    self#reset_env ();
    Translate.set_original_project (Project.current ())
end

let do_visit ?(prj=Project.current ()) generate =
  (* The main visitor proceeds by tracking declarations belonging to the
     E-ACSL runtime library and then using these declarations to generate
     statements used in instrumentation. The following code reorders AST
     so declarations belonging to E-ACSL library appear atop of any location
     requiring instrumentation. *)
  Misc.reorder_ast ();
  Options.feedback ~level:2 "%s annotations in %a."
    (if generate then "translating" else "checking")
    Project.pretty prj;
  let vis =
    Extlib.try_finally ~finally:Typing.clear (new e_acsl_visitor prj) generate
  in
  (* explicit type annotation in order to check that no new method is
     introduced by error *)
  (vis : Visitor.frama_c_visitor)

(*
Local Variables:
compile-command: "make"
End:
*)

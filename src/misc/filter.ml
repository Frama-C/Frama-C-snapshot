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

(* $Id: filter.ml,v 1.50 2008/05/22 08:12:06 uid562 Exp $ *)

open Db_types
open Cil
open Cil_types

let debug () = Cmdline.Debug.get() >= 1

module type T_RemoveInfo = sig
  type t_proj
  type t_fct

  val fct_info : t_proj -> Db_types.kernel_function -> t_fct list

  val fct_name :  varinfo -> t_fct -> string

  val param_visible : t_fct -> int -> bool
  val loc_var_visible : t_fct -> varinfo -> bool
  val inst_visible : t_fct -> stmt -> bool
  val label_visible : t_fct -> stmt -> label -> bool

  val annotation_visible: t_fct -> stmt -> before:bool ->
                          code_annotation -> bool

  val called_info : (t_proj * t_fct) -> stmt ->
    (Db_types.kernel_function * t_fct) option
  val res_call_visible : t_fct -> stmt -> bool
  val result_visible : Db_types.kernel_function -> t_fct -> bool
end

module F (Info : T_RemoveInfo) : sig

  val build_cil_file : Project.t ->  Info.t_proj -> unit

end = struct

  type t = (string, Cil_types.varinfo) Hashtbl.t

  let mk_new_stmt s kind = s.skind <- kind

  let mk_skip loc = Instr (Skip loc)

  let mk_stmt_skip st = mk_skip (Cil.get_stmtLoc st.skind)

  let rec is_empty_block keep_stmts block =
    let can_skip stmt =
      match stmt.skind with
        | Instr (Skip _) ->
            if debug () then Format.eprintf "@[Statement %d: can%s skip@]@."
              stmt.sid
              (if Cilutil.StmtSet.mem stmt keep_stmts then "'t" else "");
            not (Cilutil.StmtSet.mem stmt keep_stmts) &&
              stmt.labels = []
        | Block b -> is_empty_block keep_stmts b
        | _ -> false
    in List.for_all can_skip block.bstmts

  let rec mk_new_block keep_stmts s b loc =
    (* vblock has already cleaned up the statements (removed skip, etc...),
    * but now the block can still be empty or include only one statement. *)
    match b.bstmts with
      | []  | _ when is_empty_block keep_stmts b ->
          mk_new_stmt s (mk_skip loc)
      | stmt :: [] -> (* one statement only *)
          begin match stmt.skind with
            | Block b -> mk_new_block keep_stmts s b loc
            | _ -> mk_new_stmt s stmt.skind
          end
      | _ -> mk_new_stmt s (Block b)

  let mk_new_stmts_block keep_stmts old_st stmts =
    let block = Cil.mkBlock stmts in
      mk_new_block keep_stmts old_st block (Cil.get_stmtLoc old_st.skind)

  let rec filter_labels finfof st labels = match labels with
    | [] -> []
    | l :: labs ->
        if Info.label_visible finfof st l
        then l::(filter_labels finfof st labs)
        else filter_labels finfof st labs

  (** filter [params] according to [ff] input visibility.
  * Can be used to slice both the parameters, the call arguments,
  * and the param types.
  * Notice that this is just a filtering of the list.
  * It doesn't do any transformation of any kind on the element,
  * so at the end they are shared with the original list.
  * *)
  let filter_params finfo params =
    let do_param (n, new_params) var  =
      let new_params =
        if not (Info.param_visible finfo n) then new_params
        else new_params @ [var]
      in
        (n+1, new_params)
    in let _, new_params = List.fold_left do_param (1, []) params in
      new_params

  let ff_var fun_vars kf finfo =
    let fct_var = Kernel_function.get_vi kf in
    let name = Info.fct_name fct_var finfo in
      try
        let ff_var = Hashtbl.find fun_vars name in
          if debug () then
            Format.printf
              "[filter:ff_var] Use fct var %s:%d@\n" ff_var.vname ff_var.vid;
          ff_var
      with Not_found ->
        let ff_var = Cil.copyVarinfo fct_var name in
        if not (Info.result_visible kf finfo) then
          Cil.setReturnTypeVI ff_var Cil.voidType;
        (* Notice that we don't have to filter the parameter types here :
         * they will be update by [Cil.setFormals] later on. *)
        if debug () then
          Format.printf
            "[filter:ff_var] Mem fct var %s:%d@\n" ff_var.vname ff_var.vid;
        Hashtbl.add fun_vars name ff_var;
        ff_var

  let optim_if keep_stmts s cond_opt bthen belse loc =
    let empty_then = is_empty_block keep_stmts bthen in
    let empty_else = is_empty_block keep_stmts belse in
      if debug () then
        Format.printf "[filter:optim_if] sid:%d with \
                               %s cond, %s empty then, %s empty else@\n"
          s.sid
          (if cond_opt = None then "no" else "")
          (if empty_then then "" else "not")
          (if empty_else then "" else "not");
      match cond_opt, empty_then, empty_else with
        | _, true,true -> (* both blocks empty -> skip *)
            mk_new_stmt s (mk_skip loc)
        | None, false, true -> (* no cond and else empty -> block then *)
            (mk_new_block keep_stmts s bthen loc)
        | None, true, false -> (* no cond and then empty -> block else *)
            (mk_new_block keep_stmts s belse loc)
        | Some cond, _, _ ->
            let skind = If (cond, bthen, belse, loc) in
              (mk_new_stmt s skind)
        | None, false, false ->
            let skind = If (Cil.zero, bthen, belse, loc) in
              (mk_new_stmt s skind)

  (** This visitor is to be used to filter a function.
  * It does a deep copy of the source function without the invisible elements.
  * It also change the function declaration and filter the function calls.
  *
  * Many ideas come from [Cil.copyFunctionVisitor] but we were not able to
  * directly inherit from it since some processing would not have worked in our
  * context (like the [sid] computation for instance).
  * *)
class filter_visitor pinfo prj = object(self)

    inherit Visitor.generic_frama_c_visitor prj (Cil.copy_visit()) as super

    val mutable keep_stmts = Cilutil.StmtSet.empty
    val mutable kf = None
    val mutable fi = None
    val fi_table = Cilutil.VarinfoHashtbl.create 7
    val spec_table = Cilutil.VarinfoHashtbl.create 7
    val fun_vars = Hashtbl.create 7

    method private get_kf () = Extlib.the kf

    method private get_finfo () = Extlib.the fi

    method private add_stmt_keep stmt =
      keep_stmts <- Cilutil.StmtSet.add stmt keep_stmts

    (** Applied on each variable use :
    * must replace references to formal/local variables
    * and source function calls *)
    method vvrbl (v: varinfo) =
      if v.vglob
      then
        try let v' = (Hashtbl.find fun_vars v.vname) in
        Cil.ChangeTo v'
        with Not_found ->
          Cil.SkipChildren
      else Cil.SkipChildren (*copy has already been done by default visitor*)

    method private filter_formals formals =
      let formals = filter_params (self#get_finfo ()) formals in
      List.map
        (fun v ->
           let v' = Cil.copyVarinfo v v.vname in
           Cil.set_varinfo self#behavior v v'; v')
        formals

    method private filter_locals locals =
      let rec filter locals = match locals with
        | [] -> []
        | var :: locals ->
            let visible = Info.loc_var_visible (self#get_finfo ()) var in
            if debug () then
              Format.printf "[filter:local] %s -> %s@\n" var.vname
                            (if visible then "keep" else "remove");
            if visible
            then
              let var' = Cil.copyVarinfo var var.vname in
              Cil.set_varinfo self#behavior var var';
              var' :: (filter locals)
            else filter locals
      in let new_locals = filter locals in
        new_locals

    method vcode_annot v =
      let stmt =
        Cil.get_original_stmt self#behavior (Cilutil.valOf self#current_stmt)
      in
      let before = self#is_annot_before in
        if debug () then
          Format.printf "[filter:annotation] %s stmt %d : %a @\n"
            (if before then "before" else "after")
            stmt.sid !Ast_printer.d_code_annotation v;
      if Db.Value.is_accessible (Cil_types.Kstmt stmt) &&
         Info.annotation_visible (self#get_finfo ()) stmt before v
      then begin
        self#add_stmt_keep stmt;
        ChangeDoChildrenPost (v,Logic_const.refresh_code_annotation)
      end else begin
        if debug () then
          Format.printf
            "Ignoring annotation: %a@\n" !Ast_printer.d_code_annotation v;
        ChangeTo
          (Logic_const.new_code_annotation
             (AAssert
	       { name = []; loc = Lexing.dummy_pos,Lexing.dummy_pos;
	       content = Ptrue}))
      end

    method private process_call call_stmt call =
      let finfo = self#get_finfo () in
      let info = (pinfo, finfo) in
      let lval, _funcexp, args, loc = call in
      let called_info = Info.called_info info call_stmt in
      match called_info with
        | None -> call_stmt.skind
        | Some (called_kf, called_finfo) ->
            let var_slice = ff_var fun_vars called_kf called_finfo in
            let new_funcexp = Lval (Var var_slice, NoOffset) in
            let new_args = filter_params called_finfo args in
            let need_lval = Info.res_call_visible finfo call_stmt in
            let new_lval = if need_lval then lval else None in
            let new_call = Call (new_lval, new_funcexp, new_args, loc) in
              Instr (new_call)

    method vblock (b: block) =
      let optim b' =
        (* This optim must be performed after the sliced annotations have
           been put in the new table. Hence, we must put the action into
           the queue.
         *)
        Queue.add
          (fun () ->
             b'.bstmts <-
               List.filter
               (fun st -> not (Cil.is_skip st.skind) ||
                  st.labels <> [] || Annotations.get st <> []
                  || ((*Format.eprintf "Skipping %d@\n@." st.sid;*) false)
               )
               b'.bstmts)
        self#get_filling_actions;
        b'
      in
       Cil.ChangeDoChildrenPost (b, optim)

    method private change_sid s =
      let orig = Cil.get_original_stmt self#behavior s in
      assert (Cil.get_stmt self#behavior orig == s);
      let old = s.sid in
      let keep = Cilutil.StmtSet.mem s keep_stmts in
      keep_stmts <- Cilutil.StmtSet.remove s keep_stmts;
      s.sid <- Cil.new_sid();
      Cil.set_stmt self#behavior orig s;
      if keep then self#add_stmt_keep s;
      if debug () then
        Format.eprintf "@[finalize %d->%d = %a@]@\n@." old s.sid
          !Ast_printer.d_stmt s;

    method private process_invisible_stmt s =
      if debug () then
        Format.printf "[filter:process_invisible_stmt] sid:%d@\n" s.sid;
      (* invisible statement : but still have to visit the children if any *)
      let oldskind = s.skind in
      let do_after s =
        self#change_sid s;
        s.skind <- oldskind;
        (match s.skind with
         | If (_,bthen,belse,loc) ->
             let bthen = Cil.visitCilBlock (self:>Cil.cilVisitor) bthen in
             let belse = Cil.visitCilBlock (self:>Cil.cilVisitor) belse in
             optim_if keep_stmts s None bthen belse loc
         | Switch (_exp, body, _, loc) ->
             (* the switch is invisible : it can be translated into a block. *)
             let block =  Cil.visitCilBlock (self:>Cil.cilVisitor) body in
             (mk_new_block keep_stmts s block loc)
         | Loop (_, body, loc, _yst1, _st2) ->
             (* the loop test is invisible :
              * the body it can be translated into a simple block. *)
             let bloop =  Cil.visitCilBlock (self:>Cil.cilVisitor) body in
             (mk_new_block keep_stmts s bloop  loc)
         | Block _ -> assert false (* a block is always visible *)
         | TryFinally _ | TryExcept _ -> assert false (*TODO*)
         | Return (_,l) -> mk_new_stmt s (Return (None,l))
         | _ -> mk_new_stmt s (mk_stmt_skip s));
        s
      in
      s.skind <- mk_stmt_skip s;
      ChangeDoChildrenPost(s, do_after)

    method private process_visible_stmt s =
      if debug () then
        Format.printf "[filter:process_visible_stmt] sid:%d@\n" s.sid;
      (match s.skind with
         | Instr (Call (lval, funcexp, args, loc)) ->
             let call = (lval, funcexp, args, loc) in
             let new_call = self#process_call s call in
             mk_new_stmt s new_call
         | _ -> () (* copy the statement before modifying it *)
              (* mk_new_stmt s [] s.skind *)
      );
      let do_after s' =
        self#change_sid s';
        (match s'.skind with
          | If (cond,bthen,belse,loc) ->
              optim_if keep_stmts s' (Some cond) bthen belse loc
          | Block b  ->
              let loc = Cil.get_stmtLoc s'.skind in
              (* must be performed after the optimisation
                 of the block itself (see comment in vblock) *)
              Queue.add
                (fun () ->
                   if b.bstmts = [] &&  b.battrs = [] then
                     s'.skind <- (Instr (Skip loc)))
                self#get_filling_actions
          | _ -> ());
        s'
      in
      Cil.ChangeDoChildrenPost (s, do_after)

    method vstmt_aux s =
      let finfo = self#get_finfo () in
      let labels = filter_labels finfo s s.labels in
      s.labels <- labels;
      match s.skind with
        | Block _ | _ when Info.inst_visible finfo s ->
            self#process_visible_stmt s
        | _ -> self#process_invisible_stmt s

    method vvdec _v = SkipChildren (* everything is done elsewhere *)
(*      if debug () then
        Printf.printf "visiting declaration of var %s (id %d)" v.vname v.vid;
      (* Not all visited vdec are interesting. In fact, we're only interested in
         visiting the vdec of a prototype, but it can't be distinguished from
         the other globals.
       *)
      try
        fi <- Some (Cilutil.VarinfoHashtbl.find fi_table v);
        super#vvdec v
      with Not_found -> SkipChildren
*)
    method vfunc f =
      fi <- Some (Cilutil.VarinfoHashtbl.find fi_table f.svar);
      (* parameters *)
      let new_formals = self#filter_formals f.sformals in
      (* local declarations *)
      let new_locals = self#filter_locals f.slocals in
      let new_body = Cil.visitCilBlock (self:>Cil.cilVisitor) f.sbody
      in
      f.slocals <- new_locals;
      f.sbody <- new_body;
      Cil.setFormals f new_formals;
      (* clean up the environment if we have more than one copy of the
         function in the sliced code.
       *)
      Cil.reset_behavior_stmt self#behavior;
      keep_stmts <- Cilutil.StmtSet.empty;
      Cilutil.VarinfoHashtbl.add spec_table f.svar
        (visitCilFunspec (self:>Cil.cilVisitor) (self#get_kf()).spec);
      SkipChildren

   method private compute_fct_definitions f loc =
     let fvar = f.Cil_types.svar in
     kf <- Some (Globals.Functions.get fvar);
     let finfo_list = Info.fct_info pinfo (self#get_kf()) in
     let do_f finfo =
       let new_fct_var = ff_var fun_vars (self#get_kf()) finfo in
       Cilutil.VarinfoHashtbl.add fi_table new_fct_var finfo;
       if debug () then
           Format.eprintf "@[[filter:build_cil_fct] -> %s@\n@]@."
             (Info.fct_name (Kernel_function.get_vi (self#get_kf())) finfo);
       let action () =
         Queue.add
           (fun () ->
              let kf =
                Globals.Functions.get new_fct_var in
              kf.spec <- Cilutil.VarinfoHashtbl.find spec_table new_fct_var)
        self#get_filling_actions
       in
       GFun ({f with svar = new_fct_var},loc), action
     in List.map do_f finfo_list

   method private build_proto kf finfo loc =
     let spec = kf.spec in
     let defined =
       try let _ = Kernel_function.get_definition kf in true
       with Kernel_function.No_Definition -> false
     in
     let new_var = ff_var fun_vars kf finfo in
     Cilutil.VarinfoHashtbl.add fi_table new_var finfo;

     if debug () then
       Format.eprintf "@[[filter:build_cil_proto] -> %s@\n@]@." new_var.vname;
     let action =
       if not defined then begin
         let (rt,args,va,attrs) = Cil.splitFunctionType new_var.vtype in
         (match args with
              None -> ()
            | Some args ->
                let old_formals = Kernel_function.get_formals kf in
                let old_formals = filter_params finfo old_formals in
                let args = filter_params finfo args in
                let mytype = TFun(rt,Some args,va,attrs) in
                new_var.vtype <- mytype;
                Cil.setFormalsDecl new_var.vid mytype;
                let new_formals = Cil.getFormalsDecl new_var.vid in
                assert (List.length old_formals = List.length new_formals);
                List.iter2
                  (fun x y ->
                     Cil.set_varinfo self#behavior x y)
                  old_formals new_formals);
         let res = Cil.visitCilFunspec (self:>Cil.cilVisitor) spec in
         let action () = (* this has to be added in the queue after the
                            main visitor has put the new declaration in
                            the table
                          *)
           Queue.add
             (fun () ->
                let kf = Globals.Functions.get new_var in kf.spec <- res)
             self#get_filling_actions
         in action
       end
       else fun () -> ()
     in
     (* Set the new_var as an already known one. This prevents it from
        being copied and ensures that the link to the original one is preserved
        (NB: this also indicates that the visitor mechanism is a bit
        clumsy here)
      *)
     Cil.set_varinfo self#behavior new_var new_var;
     Cil.set_varinfo self#behavior (Ast_info.Function.get_vi kf.fundec) new_var;
     GVarDecl (Cil.empty_funspec(), new_var, loc), action

    method private compute_fct_prototypes (_,loc as fct_decl) =
      let fct_var, _ = fct_decl in
      let kf = Globals.Functions.get fct_var in
      let finfo_list = Info.fct_info pinfo kf in
      let build_cil_proto finfo =
        self#build_proto kf finfo loc
      in List.map build_cil_proto finfo_list

    method vglob_aux g =
      let post action g =
        List.iter (fun x -> x()) action; kf <- None; fi <- None; g
      in
      match g with
      | GFun (f, loc) ->
          let (new_functions,actions) =
            List.split (self#compute_fct_definitions f loc)
          in
          Cil.ChangeDoChildrenPost (new_functions, post actions)
      | GVarDecl (_, v, loc) ->
          begin
            match v.vtype with
              | TFun _ ->
                  let var_decl = (v, loc) in
                  let (new_decls,actions) =
                    List.split (self#compute_fct_prototypes var_decl)
                  in
                  Cil.ChangeDoChildrenPost (new_decls, post actions)
              | _ -> Cil.DoChildren
          end
      | _ -> Cil.DoChildren
  end

  let build_cil_file prj pinfo =
    if debug () then Format.printf "[filter:build_cil_file] ...@\n";
    let visitor = new filter_visitor pinfo in
    File.init_project_from_visitor prj visitor;
    if debug () then Format.printf "[filter:build_cil_file] done.@\n"
end

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)

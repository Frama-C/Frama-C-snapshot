(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

(* $Id: filter.ml,v 1.78 2009-03-05 15:42:45 uid562 Exp $ *)

open Db_types
open Cil
open Cilutil
open Cil_types
open Extlib

module type T_RemoveInfo = sig
  type t_proj
  type t_fct

  val fct_info : t_proj -> Db_types.kernel_function -> t_fct list

  val fct_name :  varinfo -> t_fct -> string

  val param_visible : t_fct -> int -> bool
  val body_visible : t_fct -> bool
  val loc_var_visible : t_fct -> varinfo -> bool
  val inst_visible : t_fct -> stmt -> bool
  val label_visible : t_fct -> stmt -> label -> bool

  val annotation_visible: t_fct -> stmt -> before:bool ->
                          code_annotation -> bool

  val fun_precond_visible : t_fct -> predicate -> bool
  val fun_postcond_visible : t_fct -> predicate -> bool
  val fun_variant_visible : t_fct -> term -> bool
  val fun_assign_visible : t_fct -> identified_term assigns -> bool

  val called_info : (t_proj * t_fct) -> stmt ->
    (Db_types.kernel_function * t_fct) option
  val res_call_visible : t_fct -> stmt -> bool
  val result_visible : Db_types.kernel_function -> t_fct -> bool
end

module F (Info : T_RemoveInfo) : sig

  val build_cil_file : string ->  Info.t_proj -> Project.t
end = struct

  type t = (string, Cil_types.varinfo) Hashtbl.t

  let mk_new_stmt s kind = s.skind <- kind

  let mk_skip loc = Instr (Skip loc)

  let mk_stmt_skip st = mk_skip (get_stmtLoc st.skind)

  let rec can_skip keep_stmts stmt =
    match stmt.skind with
      | Instr (Skip _) ->
          Kernel.debug ~level:2 "@[Statement %d: can%s skip@]@." stmt.sid
            (if StmtSet.mem stmt keep_stmts then "'t" else "");
          not (StmtSet.mem stmt keep_stmts) && stmt.labels = []
      | Block b -> is_empty_block keep_stmts b
      | UnspecifiedSequence seq -> is_empty_unspecified_sequence keep_stmts seq
      | _ -> false

  and is_empty_block keep_stmts block =
    List.for_all (can_skip keep_stmts) block.bstmts

  and is_empty_unspecified_sequence keep_stmts seq =
    List.for_all ((can_skip keep_stmts) $ (fun (x,_,_,_)->x)) seq

  let rec mk_new_block keep_stmts s b loc =
    (* vblock has already cleaned up the statements (removed skip, etc...),
    * but now the block can still be empty or include only one statement. *)
    match b.bstmts with
      | []  | _ when is_empty_block keep_stmts b ->
          mk_new_stmt s (mk_skip loc)
      | stmt :: [] -> (* one statement only *)
          begin match stmt.skind with
            | Block b -> mk_new_block keep_stmts s b loc
            | UnspecifiedSequence seq ->
                mk_new_unspecified_sequence keep_stmts s seq loc
            | _ -> mk_new_stmt s stmt.skind
          end
      | _ -> mk_new_stmt s (Block b)

  (* same as above, but for unspecified sequences. *)
  and mk_new_unspecified_sequence keep_stmts s seq loc =
    (* vblock has already cleaned up the statements (removed skip, etc...),
     * but now the block can still be empty or include only one statement. *)
    match seq with
      | [] -> mk_new_stmt s (mk_skip loc)
      | _ when is_empty_unspecified_sequence keep_stmts seq ->
          mk_new_stmt s (mk_skip loc)
      | [stmt,_,_,_] -> (* one statement only *)
          begin match stmt.skind with
            | UnspecifiedSequence seq ->
                mk_new_unspecified_sequence keep_stmts s seq loc
            | Block b -> mk_new_block keep_stmts s b loc
            | _ -> mk_new_stmt s stmt.skind
          end
      | _ -> mk_new_stmt s (UnspecifiedSequence seq)

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
        Kernel.debug ~level:2
	  "[filter:ff_var] Use fct var %s:%d@." ff_var.vname ff_var.vid;
          ff_var
      with Not_found ->
        let ff_var = Cil.copyVarinfo fct_var name in
        if not (Info.result_visible kf finfo) then
          Cil.setReturnTypeVI ff_var Cil.voidType;
        (* Notice that we don't have to filter the parameter types here :
         * they will be update by [Cil.setFormals] later on. *)
        Kernel.debug ~level:2 "[filter:ff_var] Mem fct var %s:%d@."
	  ff_var.vname ff_var.vid;
        Hashtbl.add fun_vars name ff_var;
        ff_var

  let optim_if keep_stmts s cond_opt bthen belse loc =
    let empty_then = is_empty_block keep_stmts bthen in
    let empty_else = is_empty_block keep_stmts belse in
    Kernel.debug ~level:2 "[filter:optim_if] sid:%d with \
                                %s cond, %s empty then, %s empty else@."
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

  let visible_lval vars_visible lval =
    let visitor = object
      inherit Visitor.generic_frama_c_visitor
        (Project.current()) (Cil.inplace_visit())
      method vvrbl v =
        if not v.vglob then
          ignore (Cilutil.VarinfoHashtbl.find vars_visible v);
        SkipChildren
    end
    in
      try
        ignore (Cil.visitCilLval (visitor:>Cil.cilVisitor) lval); true
      with Not_found -> false

  let filter_list is_visible visit l =
    let build e acc = if is_visible e then (visit e)::acc else acc
    in List.fold_right build l []

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

    val mutable keep_stmts = StmtSet.empty
    val mutable fi = None
    val fi_table = VarinfoHashtbl.create 7
    val spec_table = VarinfoHashtbl.create 7
    val fun_vars = Hashtbl.create 7
    val local_visible = Cilutil.VarinfoHashtbl.create 7
    val formals_table = VarinfoHashtbl.create 7

    method private get_finfo () = Extlib.the fi

    method private add_stmt_keep stmt =
      keep_stmts <- StmtSet.add stmt keep_stmts

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

    (*method vvdec _ = SkipChildren (* everything is done elsewhere *)*)

    method private add_formals_bindings v formals =
      VarinfoHashtbl.add formals_table v formals

    method private get_formals_bindings v =
      VarinfoHashtbl.find formals_table v

    method private filter_formals formals =
      let formals = filter_params (self#get_finfo ()) formals in
      List.map
        (fun v ->
           Cilutil.VarinfoHashtbl.add local_visible v ();
           let v' = Cil.copyVarinfo v v.vname in
           Cil.set_varinfo self#behavior v v';
           Cil.set_orig_varinfo self#behavior v' v;
           (match v.vlogic_var_assoc, v'.vlogic_var_assoc with
              None, None -> ()
            | Some lv, Some lv' ->
                Cil.set_logic_var self#behavior lv lv';
                Cil.set_orig_logic_var self#behavior lv' lv
            | _ -> assert false (* copy should be faithful *));
           v')
        formals

    method private filter_locals locals =
      let rec filter locals = match locals with
        | [] -> []
        | var :: locals ->
            let visible = Info.loc_var_visible (self#get_finfo ()) var in
            Kernel.debug ~level:2
	      "[filter:local] %s -> %s@." var.vname
              (if visible then "keep" else "remove");
            if visible
            then begin
              Cilutil.VarinfoHashtbl.add local_visible var ();
              let var' = Cil.copyVarinfo var var.vname in
              Cil.set_varinfo self#behavior var var';
              Cil.set_orig_varinfo self#behavior var' var;
              (match var.vlogic_var_assoc, var'.vlogic_var_assoc with
                 None, None -> ()
               | Some lv, Some lv' ->
                   Cil.set_logic_var self#behavior lv lv';
                   Cil.set_orig_logic_var self#behavior lv' lv
               | _ -> assert false (* copy should be faithful *));
              var' :: (filter locals)
            end else filter locals
      in let new_locals = filter locals in
      new_locals

    method vcode_annot v =
      let stmt =
        Cil.get_original_stmt self#behavior (valOf self#current_stmt)
      in
      let before = self#is_annot_before in
      Kernel.debug "[filter:annotation] %s stmt %d : %a @."
        (if before then "before" else "after")
        stmt.sid !Ast_printer.d_code_annotation v;
      if Db.Value.is_accessible (Cil_types.Kstmt stmt) &&
        Info.annotation_visible (self#get_finfo ()) stmt before v
      then begin
        self#add_stmt_keep stmt;
        ChangeDoChildrenPost (v,Logic_const.refresh_code_annotation)
      end else begin
        Kernel.debug "\t-> ignoring annotation: %a@."
          !Ast_printer.d_code_annotation v;
        ChangeTo
          (Logic_const.new_code_annotation
             (AAssert ([],
	               { name = []; loc = Lexing.dummy_pos,Lexing.dummy_pos;
	                 content = Ptrue},
                       {status=Unknown})))
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
          let new_funcexp = new_exp (Lval (Var var_slice, NoOffset)) in
          let new_args = filter_params called_finfo args in
          let need_lval = Info.res_call_visible finfo call_stmt in
          let new_lval = if need_lval then lval else None in
          let new_call = Call (new_lval, new_funcexp, new_args, loc) in
          Kernel.debug "[filter:process_call] call %s@." var_slice.vname;
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
               (fun st ->
		  not (Cil.is_skip st.skind)
		  || st.labels <> []
		  || Annotations.get_all st <> []
                    (*|| ((*Format.eprintf "Skipping %d@.@." st.sid;*) false)*)
               )
               b'.bstmts)
          self#get_filling_actions;
        b'
      in
      (* b.blocals still contains original varinfos at this stage. The
         remaining ones will be copied later in the visit. *)
      b.blocals <-
        List.filter (Info.loc_var_visible (self#get_finfo ())) b.blocals;
      Cil.ChangeDoChildrenPost (b, optim)

    method private change_sid s =
      let orig = Cil.get_original_stmt self#behavior s in
      assert (Cil.get_stmt self#behavior orig == s);
      let old = s.sid in
      let keep = StmtSet.mem s keep_stmts in
      keep_stmts <- StmtSet.remove s keep_stmts;
      s.sid <- Cil.Sid.next ();
      Cil.set_stmt self#behavior orig s;
      Cil.set_orig_stmt self#behavior s orig;
      if keep then self#add_stmt_keep s;
      Kernel.debug ~level:2
	"@[finalize %d->%d = %a@]@\n@." old s.sid !Ast_printer.d_stmt s;

    method private process_invisible_stmt s =
      Kernel.debug ~level:2 "[filter:process_invisible_stmt] sid:%d@." s.sid;
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
         | Block _ | UnspecifiedSequence _ ->
             assert false (* a block is always visible *)
         | TryFinally _ | TryExcept _ -> assert false (*TODO*)
         | Return (_,l) -> mk_new_stmt s (Return (None,l))
         | _ -> mk_new_stmt s (mk_stmt_skip s));
        s
      in
      s.skind <- mk_stmt_skip s;
      ChangeDoChildrenPost(s, do_after)

    method private process_visible_stmt s =
      Kernel.debug ~level:2 "[filter:process_visible_stmt] sid:%d@." s.sid;
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
         | Switch (e,b,c,l) ->
             let c' = List.filter (not $ (can_skip keep_stmts)) c in
             s'.skind <- Switch(e,b,c',l)
         | Block b ->
             let loc = get_stmtLoc s'.skind in
             (* must be performed after the optimisation
                of the block itself (see comment in vblock) *)
             Queue.add
               (fun () ->
                  if b.bstmts = [] &&  b.battrs = [] then
                    s'.skind <- (Instr (Skip loc)))
               self#get_filling_actions
         | UnspecifiedSequence _ ->
             let loc = get_stmtLoc s'.skind in
             Queue.add
               (fun () ->
                  match s'.skind with
                  | UnspecifiedSequence l ->
                      let res =
                        List.filter (fun (s,_,_,_) -> not (is_skip s.skind)) l
                      in
                      let res =
                        List.map
                          (fun (s,m,w,r) ->
                             (s,
                              List.filter (visible_lval local_visible) m,
                              List.filter (visible_lval local_visible) w,
                              List.filter (visible_lval local_visible) r))
                          res
                      in
                      (match res with
                         [] -> s'.skind <-  (Instr (Skip loc))
                       | _ -> s'.skind <- UnspecifiedSequence res)
                  | _ -> ())
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
      | Block _ | UnspecifiedSequence _ -> self#process_visible_stmt s
      | _ when Info.inst_visible finfo s -> self#process_visible_stmt s
      | _ -> self#process_invisible_stmt s

    method vfunc f =
      Kernel.debug "@[[filter:vfunc] -> %s@\n@]@." f.svar.vname;
      fi <- Some (VarinfoHashtbl.find fi_table f.svar);
      (* parameters *)
      let new_formals =
        try self#get_formals_bindings f.svar
          (* if there was a declaration, use the already computed
             formals list *)
        with Not_found ->
          self#filter_formals f.sformals
      in
      (* local declarations *)
      let new_locals = self#filter_locals f.slocals in
      let new_body = Cil.visitCilBlock (self:>Cil.cilVisitor) f.sbody
      in
      f.slocals <- new_locals;
      f.sbody <- new_body;
      Cil.setFormals f new_formals;
      (* clean up the environment if we have more than one copy of the
         function in the sliced code. *)
      Cil.reset_behavior_stmt self#behavior;
      keep_stmts <- StmtSet.empty;
      VarinfoHashtbl.clear local_visible;
      VarinfoHashtbl.add spec_table f.svar
        (visitCilFunspec (self:>Cil.cilVisitor)
           (Extlib.the self#current_kf).spec);
      SkipChildren

    method private visit_pred p =
      { p with ip_content =
          visitCilPredicate (self:>Cil.cilVisitor) p.ip_content }

    method vbehavior b =
      let finfo = self#get_finfo () in

      let pre_visible p =  Info.fun_precond_visible finfo p.ip_content in
      b.b_assumes <- filter_list pre_visible self#visit_pred b.b_assumes;

      let ensure_visible (_,p) = Info.fun_postcond_visible finfo p.ip_content in
      b.b_post_cond <-
	filter_list ensure_visible (fun (k,p) -> k,self#visit_pred p)
	b.b_post_cond;

      let assign_visible a = Info.fun_assign_visible finfo a in
      let assign_visit a = visitCilAssigns (self:>Cil.cilVisitor) a in
      b.b_assigns <- filter_list assign_visible assign_visit b.b_assigns;

      SkipChildren (* see the warning on [SkipChildren] in [vspec] ! *)

    method vspec spec =
      Kernel.debug "@[[filter:vspec] for %a @\n@]@."
	Kernel_function.pretty_name (Extlib.the self#current_kf);
      let finfo = self#get_finfo () in
      let require_visible p =  Info.fun_precond_visible finfo p.ip_content in
      spec.spec_requires <-
	filter_list require_visible self#visit_pred spec.spec_requires ;
      let b = Cil.visitCilBehaviors (self:>Cil.cilVisitor) spec.spec_behavior in
      let empty_behavior b =
	b.b_assumes = [] && b.b_post_cond = [] && b.b_assigns = []
      in
      let b = List.filter (not $ empty_behavior) b in
      spec.spec_behavior <- b;

      let new_variant = match spec.spec_variant with
	| None -> None
	| Some (t,n) -> if Info.fun_variant_visible finfo t
          then Some (visitCilTerm (self:>Cil.cilVisitor) t, n)
          else None
      in spec.spec_variant <- new_variant ;

      let new_term = match spec.spec_terminates with
	| None -> None
	| Some p -> if  Info.fun_precond_visible finfo p.ip_content
          then Some (self#visit_pred p)
          else None
      in
      spec.spec_terminates <- new_term ;
      spec.spec_complete_behaviors <- [] (* TODO ! *) ;
      spec.spec_disjoint_behaviors <- [] (* TODO ! *) ;
      SkipChildren (* Be very carefull that we can use [SkipChildren] here
                      only if everything that is in the new spec
                      has been visited above.
                      we need to put links to the appropriate copies of
                      variables (both pure C and logical ones)
                   *)

    method private build_proto finfo loc =
      let kf = Extlib.the self#current_kf in
      fi <- Some finfo;
      let new_var = ff_var fun_vars kf finfo in
      VarinfoHashtbl.add fi_table new_var finfo;
      Kernel.debug "@[[filter:build_cil_proto] -> %s@\n@]@." new_var.vname;
      let action =
	let (rt,args,va,attrs) = Cil.splitFunctionType new_var.vtype in
	let () =
          match args with
            None -> ()
          | Some args ->
              let old_formals = Kernel_function.get_formals kf in
              let old_formals = filter_params finfo old_formals in
              let args = filter_params finfo args in
              let mytype = TFun(rt,Some args,va,attrs) in
              let new_formals = List.map makeFormalsVarDecl args in
              self#add_formals_bindings new_var new_formals;
              new_var.vtype <- mytype;
              List.iter2
                (fun x y ->
                   Cil.set_varinfo self#behavior x y;
                   Cil.set_orig_varinfo self#behavior y x;
                   match x.vlogic_var_assoc with
                     None -> ();
                   | Some lv ->
                       let lv' = Cil.cvar_to_lvar y in
                       Cil.set_logic_var self#behavior lv lv';
                       Cil.set_orig_logic_var self#behavior lv' lv
                )
                old_formals new_formals;
              (* adds the new parameters to the formals decl table *)
              Queue.add
                (fun () -> Cil.unsafeSetFormalsDecl new_var new_formals)
                self#get_filling_actions
	in
	let res = Cil.visitCilFunspec (self:>Cil.cilVisitor) kf.spec in
	let action () =
          (* Replace the funspec copied by the default visitor, as
             varinfo of formals would not be taken into account correctly
             otherwise (everything would be mapped to the last set of
             formals...
          *)
          Queue.add (fun () -> let kf = Globals.Functions.get new_var in
                     kf.spec <- res) self#get_filling_actions
	in action
             (*end else fun () -> ()*)
      in
      let orig_var = Ast_info.Function.get_vi kf.fundec in
      (* The first copy is also the default one for varinfo that are not handled
         by ff_var but directly by the visitor
      *)
      if (Cil.get_varinfo self#behavior orig_var) == orig_var then
	Cil.set_varinfo self#behavior orig_var new_var;
      (* Set the new_var as an already known one, coming from the vi associated
         to the current kf.
      *)
      Cil.set_varinfo self#behavior new_var new_var;
      Cil.set_orig_varinfo self#behavior new_var orig_var;
      GVarDecl (Cil.empty_funspec(), new_var, loc), action

    method private compute_fct_prototypes (_fct_var,loc) =
      let finfo_list = Info.fct_info pinfo (Extlib.the self#current_kf) in
      Kernel.debug "@[[filter:compute_fct_prototypes] for %a (x%d)@\n@]@."
        Kernel_function.pretty_name (Extlib.the self#current_kf)
        (List.length finfo_list);
      let build_cil_proto finfo = self#build_proto finfo loc
      in List.map build_cil_proto finfo_list

    method private compute_fct_definitions f loc =
      let fvar = f.Cil_types.svar in
      let finfo_list = Info.fct_info pinfo (Extlib.the self#current_kf) in
      Kernel.debug "@[[filter:compute_fct_definitions] for %a (x%d)@\n@]@."
	Kernel_function.pretty_name
	(Extlib.the self#current_kf) (List.length finfo_list);
      let do_f finfo =
	if not (Info.body_visible finfo) then
          self#build_proto finfo loc
	else begin
          let new_fct_var = ff_var fun_vars (Extlib.the self#current_kf) finfo in
          (* Set the new_var as an already known one,
           * coming from the vi associated to the current kf.  *)
          Cil.set_varinfo self#behavior new_fct_var new_fct_var;
          Cil.set_orig_varinfo self#behavior new_fct_var fvar;
          VarinfoHashtbl.add fi_table new_fct_var finfo;
          Kernel.debug "@[[filter:build_cil_fct] -> %s@\n@]@."
            (Info.fct_name
	       (Kernel_function.get_vi (Extlib.the self#current_kf)) finfo);
          let action () =
            Queue.add
              (fun () ->
                 let kf = Globals.Functions.get new_fct_var in
                 kf.spec <- VarinfoHashtbl.find spec_table new_fct_var)
              self#get_filling_actions
          in let f = {f with svar = new_fct_var} in
	  (* [JS 2009/03/23] do not call self#vfunc in the assertion;
	     otherwise does not work whenever frama-c is compiled with
	     -no-assert *)
	  let res = self#vfunc f in
	  assert (res = SkipChildren);
          (* if this ever changes, we must do some work. *)
          GFun (f,loc), action
	end
      in
      List.map do_f finfo_list

    method vglob_aux g =
      let post action g =
        List.iter (fun x -> x()) action; fi <- None;
        Kernel.debug "[filter:post action] done.@.";
        g
      in
      match g with
      | GFun (f, loc) ->
          let (new_functions,actions) =
            List.split (self#compute_fct_definitions f loc)
          in
          Cil.ChangeToPost (new_functions, post actions)
      | GVarDecl (_, v, loc) ->
          begin
            match v.vtype with
            | TFun _ ->
                Kernel.debug "[filter:vglob_aux] GVarDecl %s (TFun)@." v.vname;
                let var_decl = (v, loc) in
                let (new_decls,actions) =
                  List.split (self#compute_fct_prototypes var_decl)
                in
                Cil.ChangeToPost (new_decls, post actions)
            | _ ->
                Kernel.debug "[filter:vglob_aux] GVarDecl %s (other)@." v.vname;
                Cil.DoChildren
          end
      | _ -> Cil.DoChildren
  end

  let build_cil_file new_proj_name pinfo =
    Kernel.debug "[filter:build_cil_file] in %s@." new_proj_name;
    let visitor = new filter_visitor pinfo in
    let prj = File.create_project_from_visitor new_proj_name visitor in
    Kernel.debug "[filter:build_cil_file] done.@.";
    prj
end

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)

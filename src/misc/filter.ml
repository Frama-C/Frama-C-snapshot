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

open Cil
open Cil_types
module FC_file = File (* overwritten by Cil_datatype *)
open Cil_datatype
open Extlib

let dkey = Kernel.register_category "filter"

let debug1 fmt = Kernel.debug ~current:true ~dkey fmt
let debug2 fmt = Kernel.debug ~current:true ~dkey ~level:2 fmt

module type RemoveInfo = sig
  type proj
  type fct

  exception EraseAssigns
  exception EraseAllocation

  val fct_info : proj -> kernel_function -> fct list

  val fct_name :  varinfo -> fct -> string

  val param_visible : fct -> int -> bool
  val body_visible : fct -> bool
  val loc_var_visible : fct -> varinfo -> bool
  val inst_visible : fct -> stmt -> bool
  val label_visible : fct -> stmt -> label -> bool

  val annotation_visible: fct -> stmt -> code_annotation -> bool

  val fun_precond_visible : fct -> predicate -> bool
  val fun_postcond_visible : fct -> predicate -> bool
  val fun_variant_visible : fct -> term -> bool

  val fun_frees_visible : fct -> identified_term -> bool
  val fun_allocates_visible : fct -> identified_term -> bool
  val fun_assign_visible : fct -> identified_term from -> bool
  val fun_deps_visible : fct -> identified_term -> bool

  val called_info : (proj * fct) -> stmt ->
    (kernel_function * fct) option
  val res_call_visible : fct -> stmt -> bool
  val result_visible : kernel_function -> fct -> bool
  val cond_edge_visible: fct -> stmt -> bool * bool
end

module F (Info : RemoveInfo) : sig

  val build_cil_file : string ->  Info.proj -> Project.t

end = struct

  type t = (string, Cil_types.varinfo) Hashtbl.t

  let mk_new_stmt s kind = s.skind <- kind
  let mk_skip loc = Instr (Skip loc)
  let mk_stmt_skip st = mk_skip (Stmt.loc st)

  let make_new_kf tbl kf v =
    try
      Cil_datatype.Varinfo.Hashtbl.find tbl v
    with Not_found ->
      let fundec =
        match kf.fundec with
          | Definition(f,l) -> Definition ( { f with svar = v },l)
          | Declaration(_,_,arg,l) ->
            Declaration(Cil.empty_funspec(),v,arg,l)
      in
      let kf =
        { fundec = fundec; spec = Cil.empty_funspec(); return_stmt = None }
      in
      Cil_datatype.Varinfo.Hashtbl.add tbl v kf; kf

  let rec can_skip keep_stmts stmt =
    stmt.labels = [] &&
    match stmt.skind with
      | Instr (Skip _) ->
          debug2 "@[Statement %d: can%s skip@]@." stmt.sid
            (if Stmt.Set.mem stmt keep_stmts then "'t" else "");
          not (Stmt.Set.mem stmt keep_stmts)
      | Block b -> is_empty_block keep_stmts b
      | UnspecifiedSequence seq -> is_empty_unspecified_sequence keep_stmts seq
      | _ -> false

  and is_empty_block keep_stmts block =
    List.for_all (can_skip keep_stmts) block.bstmts

  and is_empty_unspecified_sequence keep_stmts seq =
    List.for_all ((can_skip keep_stmts) $ (fun (x,_,_,_,_)->x)) seq

  let rec mk_new_block keep_stmts s blk loc =
    (* vblock has already cleaned up the statements (removed skip, etc...),
    * but now the block can still be empty or include only one statement. *)
    match blk.bstmts with
      | []  | _ when is_empty_block keep_stmts blk ->
          (* don't care about local variables since the block is empty. *)
          mk_new_stmt s (mk_skip loc)
      | { labels = [] } as s1 :: [] -> 
          (* one statement only, and no label *)
          begin
            match s1.skind with
              | Block b ->
                  (* drop blk, but keep local declarations. *)
                  b.blocals <- b.blocals @ blk.blocals;
                  mk_new_block keep_stmts s b loc
              | UnspecifiedSequence seq when blk.blocals = [] ->
                  mk_new_unspecified_sequence keep_stmts s seq loc
              | _ when blk.blocals = [] -> mk_new_stmt s s1.skind
              | _ -> mk_new_stmt s (Block blk)
          end
      | _ -> mk_new_stmt s (Block blk)

  (* same as above, but for unspecified sequences. *)
  and mk_new_unspecified_sequence keep_stmts s seq loc =
    (* vblock has already cleaned up the statements (removed skip, etc...),
     * but now the block can still be empty or include only one statement. *)
    match seq with
      | [] -> mk_new_stmt s (mk_skip loc)
      | _ when is_empty_unspecified_sequence keep_stmts seq ->
          mk_new_stmt s (mk_skip loc)
      | [stmt,_,_,_,_] -> (* one statement only *)
          begin
            if stmt.labels <> [] then s.labels <- s.labels @ stmt.labels;
            match stmt.skind with
            | UnspecifiedSequence seq ->
                mk_new_unspecified_sequence keep_stmts s seq loc
            | Block b -> mk_new_block keep_stmts s b loc
            | _ -> mk_new_stmt s stmt.skind
          end
      | _ -> mk_new_stmt s (UnspecifiedSequence seq)

  let add_label_if_needed mk_label finfo s =
    let rec pickLabel = function
      | [] -> None
      | Label _ as lab :: _ when Info.label_visible finfo s lab -> Some lab
      | _ :: rest -> pickLabel rest
    in match pickLabel s.labels with
      | Some _ -> None
      | None -> 
          let label = mk_label (Cil_datatype.Stmt.loc s) in
            debug2 "add label to sid:%d : %a" s.sid Printer.pp_label label;
            s.labels <- label::s.labels;
            Some label

  let rm_break_cont ?(cont=true) ?(break=true) mk_label finfo blk = 
    let change loc s =
      let dest = match s.succs with dest::_ -> dest | [] -> assert false in
      let new_l = add_label_if_needed mk_label finfo dest in
        mk_new_stmt s (Goto (ref dest, loc));
        debug2 "changed break/continue into @[%a@]@."
          Printer.pp_stmt s;
        new_l
    in
    let rec rm_aux cont break s =
      match s.skind with
        | Break    loc when break && Info.inst_visible finfo s ->
            let _ = change loc s in ()
        | Continue loc when cont  && Info.inst_visible finfo s -> 
            let _ = change loc s in ()
        | Instr _ | Return _ | Break _ | Continue _ | Goto _ -> ()
        | If (_, bthen, belse, _) ->
            List.iter (rm_aux cont break) bthen.bstmts;
            List.iter (rm_aux cont break) belse.bstmts;
        | Block blk ->
            List.iter (rm_aux cont break) blk.bstmts
        | UnspecifiedSequence seq ->
            let blk = Cil.block_from_unspecified_sequence seq in
              List.iter (rm_aux cont break) blk.bstmts
        | Loop _ -> (* don't go inside : break and continue change meaning*)
            ()
        | Switch (_, blk, _, _) -> 
            (* if change [continue] do it, but stop changing [break] *)
            if cont then 
              let break = false in List.iter (rm_aux cont break) blk.bstmts
        | TryFinally _ | TryExcept _ -> (* TODO ? *) ()
    in List.iter (rm_aux cont break) blk.bstmts

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

  let ff_var (fun_vars: t) kf finfo =
    let fct_var = Kernel_function.get_vi kf in
    let name = Info.fct_name fct_var finfo in
      try
        let ff_var = Hashtbl.find fun_vars name in
        debug2 "[ff_var] Use fct var %s:%d@." ff_var.vname ff_var.vid;
          ff_var
      with Not_found ->
        let ff_var = Cil.copyVarinfo fct_var name in
        if not (Info.result_visible kf finfo) then
          Cil.setReturnTypeVI ff_var Cil.voidType;
        (* Notice that we don't have to filter the parameter types here :
         * they will be update by [Cil.setFormals] later on. *)
        debug2 "[ff_var] Mem fct var %s:%d@."
          ff_var.vname ff_var.vid;
        Hashtbl.add fun_vars name ff_var;
        ff_var

  let optim_if fct keep_stmts s_orig s cond_opt bthen belse loc =
    let empty_then = is_empty_block keep_stmts bthen in
    let empty_else = is_empty_block keep_stmts belse in
    debug2 "[optim_if] @[sid:%d (orig:%d)@ \
            with %s cond, %s empty then, %s empty else@]@."
      s.sid s_orig.sid
      (if cond_opt = None then "no" else "")
      (if empty_then then "" else "not")
      (if empty_else then "" else "not");
    match cond_opt with
    | Some cond -> 
        if empty_then && empty_else then mk_new_stmt s (mk_skip loc)
        else (* cond visible and something in blocks : keep if *)
          mk_new_stmt s (If (cond, bthen, belse, loc))
    | None -> (* no cond *)
        let go_then, go_else = Info.cond_edge_visible fct s_orig in
          debug2 
            "[condition_truth_value] can go in then = %b - can go in else =%b@."
            go_then go_else;
          match go_then, empty_then, go_else, empty_else with
            | _, true, _, true -> (* both blocks empty -> skip *)
                 mk_new_stmt s (mk_skip loc)
            | true, false, false, true -> 
                (* else empty and always go to then -> block then *)
                mk_new_block keep_stmts s bthen loc
            | false, true, true, false ->
                (* then empty and always go to else -> block else *)
                mk_new_block keep_stmts s belse loc
            | false, false, true, _ ->
                (* always goes in the 'else' branch,
                * but the then branch is not empty : *)
                mk_new_stmt s (If (Cil.zero ~loc, bthen, belse, loc))
            | true, false, false, false ->
                (* always goes in the 'then' branch,
                * but the else branch is not empty :
                *)
                mk_new_stmt s (If (Cil.one ~loc, bthen, belse, loc))
            | true, true, false, false ->
                (* always goes in the 'then' empty branch,
                * but the else branch is not empty :
                * build (if (0) belse else empty.
                *)
                mk_new_stmt s (If (Cil.zero ~loc, belse, bthen, loc))
            | true,  false, true,  false 
            | false, false, false, false ->
                (* if both go_then and go_else are true:
                * can go in both branch but don't depend on cond ?
                * probably unreachable IF with reachable blocks by goto.
                * if both go_else and go_else are false:
                * never goes in any branch ?
                * both branch visible -> dummy condition *)
                mk_new_stmt s (If (Cil.one ~loc, bthen, belse, loc))
            | true,  _, true,  true  
            | false, _, false, true -> 
               (* can go in both or no branch (see above) : empty else *)
               mk_new_block keep_stmts s bthen loc
            | true,  true, true,  _ 
            | false, true, false, _ -> 
               (* can go in both or no branch (see above) : empty then *)
               mk_new_block keep_stmts s belse loc

  let visible_lval vars_visible lval =
    let visitor = object
      inherit Visitor.frama_c_inplace
      method! vvrbl v =
        if not v.vglob then
          ignore (Varinfo.Hashtbl.find vars_visible v);
        SkipChildren
    end
    in
    try
      ignore (Cil.visitCilLval (visitor :> Cil.cilVisitor) lval);
      true
    with Not_found ->
      false

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

    inherit Visitor.generic_frama_c_visitor (Cil.copy_visit prj)

    val mutable keep_stmts = Stmt.Set.empty
    val mutable fi = None
    val fi_table = Varinfo.Hashtbl.create 7
    val spec_table = Varinfo.Hashtbl.create 7
    val fun_vars: t = Hashtbl.create 7
    val local_visible = Varinfo.Hashtbl.create 7
    val formals_table = Varinfo.Hashtbl.create 7
    val my_kf = Varinfo.Hashtbl.create 7

    val lab_num = ref 0;
    val lab_prefix = "break_cont"
    method private fresh_label loc = 
      incr lab_num;
      let lname = Printf.sprintf "%s_%d" lab_prefix !lab_num in
      Label (lname, loc, false)
    method private is_our_label label = match label with
    | Label (lname, _, false) -> 
      let ok = 
        try 
          let prefix = String.sub lname 0 (String.length lab_prefix) in
          prefix = lab_prefix
        with Invalid_argument _ -> false
      in ok
    | _ -> false

    method private get_finfo () = Extlib.the fi

    method private add_stmt_keep stmt =
      keep_stmts <- Stmt.Set.add stmt keep_stmts

    (** Applied on each variable use :
        * must replace references to formal/local variables
        * and source function calls *)
    method! vvrbl (v: varinfo) =
      if v.vglob
      then
        try let v' = (Hashtbl.find fun_vars v.vname) in
            Cil.ChangeTo v'
        with Not_found ->
          Cil.SkipChildren
      else Cil.SkipChildren (*copy has already been done by default visitor*)

    (*method vvdec _ = SkipChildren (* everything is done elsewhere *)*)

    method private add_formals_bindings v formals =
      Varinfo.Hashtbl.add formals_table v formals

    method private get_formals_bindings v =
      Varinfo.Hashtbl.find formals_table v

    method private filter_formals formals =
      let formals = filter_params (self#get_finfo ()) formals in
      List.map
        (fun v ->
          Varinfo.Hashtbl.add local_visible v ();
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
          debug2 "[local] %s -> %s@." var.vname
            (if visible then "keep" else "remove");
          if visible
          then begin
            Varinfo.Hashtbl.add local_visible var ();
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

    method! vcode_annot v =
      Extlib.may Cil.CurrentLoc.set (Cil_datatype.Code_annotation.loc v);
      let stmt =
        Cil.get_original_stmt self#behavior (Extlib.the self#current_stmt)
      in
      debug1 "[annotation] stmt %d : %a @."
        stmt.sid Printer.pp_code_annotation v;
      if Info.annotation_visible (self#get_finfo ()) stmt v
      then begin
        self#add_stmt_keep stmt;
        ChangeDoChildrenPost (v,Logic_const.refresh_code_annotation)
      end else begin
        debug1 "\t-> ignoring annotation: %a@."
          Printer.pp_code_annotation v;
        ChangeTo
          (Logic_const.new_code_annotation
             (AAssert ([],
                       { name = []; loc = Lexing.dummy_pos,Lexing.dummy_pos;
                         content = Ptrue})))
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
        let new_funcexp = new_exp ~loc (Lval (Var var_slice, NoOffset)) in
        let new_args = filter_params called_finfo args in
        let need_lval = Info.res_call_visible finfo call_stmt in
        let new_lval = if need_lval then lval else None in
        let new_call = Call (new_lval, new_funcexp, new_args, loc) in
        debug1 "[process_call] call %s@." var_slice.vname;
        Instr (new_call)

    method! vblock (b: block) =
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
                || Annotations.has_code_annot st
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
      let keep = Stmt.Set.mem s keep_stmts in
      keep_stmts <- Stmt.Set.remove s keep_stmts;
      s.sid <- Cil.Sid.next ();
      Cil.set_stmt self#behavior orig s;
      Cil.set_orig_stmt self#behavior s orig;
      if keep then self#add_stmt_keep s;
      debug2 "@[finalize sid:%d->sid:%d@]@\n@." old s.sid 

    method private process_invisible_stmt s =
      let finfo = self#get_finfo () in
      debug2 "[process_invisible_stmt] does sid:%d@." s.sid;
      (* invisible statement : but still have to visit the children if any *)
      let oldskind = s.skind in
      let do_after s =
        self#change_sid s;
        s.skind <- oldskind;
        (match s.skind with
        | If (_,bthen,belse,loc) ->
          let bthen = Cil.visitCilBlock (self:>Cil.cilVisitor) bthen in
          let belse = Cil.visitCilBlock (self:>Cil.cilVisitor) belse in
          let s_orig = Cil.get_original_stmt self#behavior s in
          optim_if finfo keep_stmts s_orig s None bthen belse loc
        | Switch (_exp, body, _, loc) ->
             (* the switch is invisible : it can be translated into a block. *)
          rm_break_cont ~cont:false (self#fresh_label) finfo body;
          let block =  Cil.visitCilBlock (self:>Cil.cilVisitor) body in
          (mk_new_block keep_stmts s block loc)
        | Loop (_, body, loc, _lcont, _lbreak) ->
          rm_break_cont (self#fresh_label) finfo body;
          let bloop =  Cil.visitCilBlock (self:>Cil.cilVisitor) body in
          mk_new_block keep_stmts s bloop loc
        | Block _ | UnspecifiedSequence _ ->
          assert false (* a block is always visible *)
        | TryFinally _ | TryExcept _ -> assert false (*TODO*)
        | Return (_,l) -> mk_new_stmt s (Return (None,l))
        | _ -> mk_new_stmt s (mk_stmt_skip s));
        debug2 "@[<hov 10>[process_invisible_stmt] gives sid:%d@ @[%a@]@]@." 
          s.sid Printer.pp_stmt s;
        s
      in
      s.skind <- mk_stmt_skip s;
      ChangeDoChildrenPost(s, do_after)

    method private process_visible_stmt s =
      debug2 "[process_visible_stmt] does sid:%d@." s.sid; 
      let finfo = self#get_finfo () in
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
          let s_orig = Cil.get_original_stmt self#behavior s' in
          optim_if finfo keep_stmts s_orig s' (Some cond) bthen belse loc
        | Switch (e,b,c,l) ->
          let c' = List.filter (not $ (can_skip keep_stmts)) c in
          s'.skind <- Switch(e,b,c',l)
        | Block b ->
          let loc = Stmt.loc s' in
             (* must be performed after the optimisation
                of the block itself (see comment in vblock) *)
          Queue.add
            (fun () ->
              if b.bstmts = [] &&  b.battrs = [] then
                s'.skind <- (Instr (Skip loc)))
            self#get_filling_actions
        | UnspecifiedSequence _ ->
          let loc = Stmt.loc s' in
          let visible_stmt =
            let info = self#get_finfo () in
            (fun s -> Info.inst_visible info !s)
          in
          Queue.add
            (fun () ->
              match s'.skind with
              | UnspecifiedSequence l ->
                let res =
                  List.filter (fun (s,_,_,_,_) -> not (is_skip s.skind)) l
                in
                let res =
                  List.map
                    (fun (s,m,w,r,c) ->
                      (s,
                       List.filter (visible_lval local_visible) m,
                       List.filter (visible_lval local_visible) w,
                       List.filter (visible_lval local_visible) r,
                       List.filter visible_stmt c
                      )
                    )
                    res
                in
                (match res with
                  [] -> s'.skind <-  (Instr (Skip loc))
                | _ -> s'.skind <- UnspecifiedSequence res)
              | _ -> ())
            self#get_filling_actions
        | _ -> ());
        debug2 "@[<hov 10>[process_visible_stmt] gives sid:%d@ @[%a@]@]@."
          s'.sid Printer.pp_stmt s';
        s'
      in
      Cil.ChangeDoChildrenPost (s, do_after)

    method! vstmt_aux s =
      let finfo = self#get_finfo () in
      let rec filter_labels labels = match labels with
        | [] -> []
        | l :: labs ->
          let keep = Info.label_visible finfo s l || self#is_our_label l in
          debug2 "[filter_labels] %svisible %a@." 
            (if keep then "" else "in") Printer.pp_label l;
          if keep then l::(filter_labels labs) else filter_labels labs
      in
      let labels = filter_labels s.labels in
      s.labels <- labels;
      match s.skind with
      | Block _ | UnspecifiedSequence _ -> self#process_visible_stmt s
      | _ when Info.inst_visible finfo s -> self#process_visible_stmt s
      | _ -> self#process_invisible_stmt s

    method! vfunc f =
      debug1 "@[[vfunc] -> %s@\n@]@." f.svar.vname;
      fi <- Some (Varinfo.Hashtbl.find fi_table f.svar);
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
      Queue.add
        (fun () -> Cil.setFormals f new_formals) self#get_filling_actions;
      (* clean up the environment if we have more than one copy of the
         function in the sliced code. *)
      Cil.reset_behavior_stmt self#behavior;
      keep_stmts <- Stmt.Set.empty;
      Varinfo.Hashtbl.clear local_visible;
      Varinfo.Hashtbl.add spec_table f.svar
        (visitCilFunspec (self:>Cil.cilVisitor) 
           (Annotations.funspec ~populate:false (Extlib.the self#current_kf)));
      SkipChildren

    method private visit_pred p =
      Logic_const.new_predicate
        { name = p.ip_name;
          loc = p.ip_loc;
          content = visitCilPredicate (self:>Cil.cilVisitor) p.ip_content }

    method private visit_identified_term t =
      let t' = visitCilTerm (self:>Cil.cilVisitor) t.it_content in
      Logic_const.new_identified_term t'

    method! vfrom (b,f) =
      let finfo = self#get_finfo () in
      let from_visible t = Info.fun_deps_visible finfo t in
      let b = self#visit_identified_term b in
      let res =
        match f with
          FromAny -> b,FromAny
        | From l -> b, From (filter_list from_visible self#visit_identified_term l)
      in ChangeTo res

    method! vbehavior b =
      let finfo = self#get_finfo () in

      let pre_visible p =  Info.fun_precond_visible finfo p.ip_content in
      b.b_assumes <- filter_list pre_visible self#visit_pred b.b_assumes;
      b.b_requires <- filter_list pre_visible self#visit_pred b.b_requires;

      let ensure_visible (_,p) = Info.fun_postcond_visible finfo p.ip_content in
      b.b_post_cond <-
        filter_list ensure_visible (fun (k,p) -> k,self#visit_pred p)
        b.b_post_cond;

      let allocates_visible a = Info.fun_allocates_visible finfo a in
      let frees_visible a = Info.fun_frees_visible finfo a in
      (match b.b_allocation with
        FreeAllocAny -> ()
      | FreeAlloc(f,a) ->
        try
          let frees = filter_list frees_visible self#visit_identified_term f in
          let allocates = filter_list allocates_visible self#visit_identified_term a in
          b.b_allocation <- FreeAlloc (frees, allocates)
        with Info.EraseAllocation -> b.b_allocation <- FreeAllocAny
      );

      let from_visible a = Info.fun_assign_visible finfo a in
      let from_visit a = visitCilFrom (self:>Cil.cilVisitor) a in
      (match b.b_assigns with
        WritesAny -> ()
      | Writes l ->
        try
          let assigns = filter_list from_visible from_visit l in
          b.b_assigns <- Writes assigns
        with Info.EraseAssigns -> b.b_assigns <- WritesAny
      );
      SkipChildren (* see the warning on [SkipChildren] in [vspec] ! *)

    method! vspec spec =
      debug1 "@[[vspec] for %a @\n@]@."
        Kernel_function.pretty (Extlib.the self#current_kf);
      let finfo = self#get_finfo () in
      let b = Cil.visitCilBehaviors (self:>Cil.cilVisitor) spec.spec_behavior in
      let b = List.filter (not $ Cil.is_empty_behavior) b in
      spec.spec_behavior <- b;

      let new_variant = match spec.spec_variant with
        | None -> None
        | Some (t,n) -> if Info.fun_variant_visible finfo t
          then Some (visitCilTerm (self:>Cil.cilVisitor) t, n)
          else None
      in 
      spec.spec_variant <- new_variant ;

      let new_term = match spec.spec_terminates with
        | None -> None
        | Some p -> if  Info.fun_precond_visible finfo p.ip_content
          then Some (self#visit_pred p)
          else None
      in
      spec.spec_terminates <- new_term ;
      spec.spec_complete_behaviors <- [] (* TODO ! *) ;
      spec.spec_disjoint_behaviors <- [] (* TODO ! *) ;
      SkipChildren (* Be very careful that we can use [SkipChildren] here
                      only if everything that is in the new spec
                      has been visited above.
                      we need to put links to the appropriate copies of
                      variables (both pure C and logical ones)
                    *)

    method private build_proto finfo loc =
      let kf = Extlib.the self#current_kf in
      fi <- Some finfo;
      let new_var = ff_var fun_vars kf finfo in
      (* we're building a prototype. *)
      if not (Varinfo.Hashtbl.mem fi_table new_var) then begin
      new_var.vdefined <- false;
      let new_kf = make_new_kf my_kf kf new_var in
      Varinfo.Hashtbl.add fi_table new_var finfo;
      debug1 "@[[build_cil_proto] -> %s@\n@]@." new_var.vname;
      let action =
        let (rt,args,va,attrs) = Cil.splitFunctionType new_var.vtype in
        (match args with
        | None -> ()
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
                Cil.set_orig_logic_var self#behavior lv' lv)
            old_formals 
	    new_formals;
          (* adds the new parameters to the formals decl table *)
          Queue.add
            (fun () -> Cil.unsafeSetFormalsDecl new_var new_formals)
            self#get_filling_actions);
        let res =
          Cil.visitCilFunspec (self :> Cil.cilVisitor)
            (Annotations.funspec ~populate:false kf)
        in
        let action () =
          (* Replace the funspec copied by the default visitor, as
             varinfo of formals would not be taken into account correctly
             otherwise: everything would be mapped to the last set of
             formals... *)
          Queue.add
	    (fun () -> 
	      new_kf.spec <- res;
	      Annotations.register_funspec ~force:true new_kf)
	    self#get_filling_actions
        in 
	action
      in
      let orig_var = Ast_info.Function.get_vi kf.fundec in
      (* The first copy is also the default one for varinfo that are not handled
         by ff_var but directly by the visitor *)
      if (Cil.get_varinfo self#behavior orig_var) == orig_var then
        Cil.set_varinfo self#behavior orig_var new_var;
      (* Set the new_var as an already known one, coming from the vi associated
         to the current kf.
       *)
      Cil.set_varinfo self#behavior new_var new_var;
      Cil.set_orig_varinfo self#behavior new_var orig_var;
      Cil.set_kernel_function self#behavior kf new_kf;
      Cil.set_orig_kernel_function self#behavior new_kf kf;
      Queue.add 
        (fun () -> Globals.Functions.register new_kf) self#get_filling_actions;
      GVarDecl (Cil.empty_funspec(), new_var, loc), action
      end else begin
        let old_finfo = Varinfo.Hashtbl.find fi_table new_var in
        if not (finfo = old_finfo) then
          Kernel.fatal
            "Found two distinct slices of function %a with the same name %s"
            Kernel_function.pretty kf
            new_var.vname;
        (* already processed: no need for more *)
        GVarDecl(Cil.empty_funspec(),new_var,loc), fun () -> ()
      end

    method private compute_fct_prototypes (_fct_var,loc) =
      let finfo_list = Info.fct_info pinfo (Extlib.the self#current_kf) in
      debug1 "@[[compute_fct_prototypes] for %a (x%d)@\n@]@."
        Kernel_function.pretty (Extlib.the self#current_kf)
        (List.length finfo_list);
      let build_cil_proto finfo = self#build_proto finfo loc
      in List.map build_cil_proto finfo_list

    method private compute_fct_definitions f loc =
      let fvar = f.Cil_types.svar in
      let finfo_list = Info.fct_info pinfo (Extlib.the self#current_kf) in
      debug1 "@[[compute_fct_definitions] for %a (x%d)@\n@]@."
        Kernel_function.pretty
        (Extlib.the self#current_kf) (List.length finfo_list);
      let do_f finfo =
        if not (Info.body_visible finfo) then
          self#build_proto finfo loc
        else begin
          let kf = Extlib.the self#current_kf in
          let new_fct_var = ff_var fun_vars kf finfo in
          new_fct_var.vdefined <- true;
          let new_kf = make_new_kf my_kf kf new_fct_var in
          (* Set the new_var as an already known one,
           * coming from the vi associated to the current kf.  *)
          Cil.set_varinfo self#behavior new_fct_var new_fct_var;
          Cil.set_orig_varinfo self#behavior new_fct_var fvar;
          Cil.set_kernel_function self#behavior kf new_kf;
          Cil.set_orig_kernel_function self#behavior new_kf kf;
          Queue.add
            (fun () -> Globals.Functions.register new_kf)
            self#get_filling_actions;
          Varinfo.Hashtbl.add fi_table new_fct_var finfo;
          debug1 "@[[build_cil_fct] -> %s@\n@]@."
            (Info.fct_name
               (Kernel_function.get_vi (Extlib.the self#current_kf)) finfo);
          let action () =
            Queue.add
              (fun () ->
                new_kf.spec <- Varinfo.Hashtbl.find spec_table new_fct_var;
		Annotations.register_funspec ~force:true new_kf)
              self#get_filling_actions
          in let f = Kernel_function.get_definition new_kf in
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

    method! vglob_aux g =
      let post action g =
        List.iter (fun x -> x()) action; fi <- None;
        debug1 "[post action] done.@.";
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
          match Cil.unrollType v.vtype with
          | TFun _ ->
            debug1 "[vglob_aux] GVarDecl %s (TFun)@." v.vname;
            let var_decl = (v, loc) in
            let (new_decls,actions) =
              List.split (self#compute_fct_prototypes var_decl)
            in
            Cil.ChangeToPost (new_decls, post actions)
          | _ ->
            debug1 "[vglob_aux] GVarDecl %s (other)@." v.vname;
            Cil.DoChildren
        end
      | _ -> Cil.DoChildren
  end

  let build_cil_file new_proj_name pinfo =
    debug1 "[build_cil_file] in %s@." new_proj_name;
    let visitor = new filter_visitor pinfo in
    let prj = FC_file.create_project_from_visitor new_proj_name visitor in
    debug1 "[build_cil_file] done.@.";
    prj

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

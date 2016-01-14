(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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
open Cil_datatype


let is_admissible_conversion e ot nt =
  let ot' = Cil.typeDeepDropAllAttributes ot in
  let nt' = Cil.typeDeepDropAllAttributes nt in
  not (Cil.need_cast ot' nt') ||
  (match e.enode, Cil.unrollType nt with
   | Const(CEnum { eihost = ei }), TEnum(ei',_) -> ei.ename = ei'.ename
   | _ -> false)

let pretty_logic_var_kind fmt = function
  | LVGlobal -> Format.pp_print_string fmt "global logic declaration"
  | LVC -> Format.pp_print_string fmt "C variable"
  | LVFormal -> Format.pp_print_string fmt "formal parameter"
  | LVQuant -> Format.pp_print_string fmt "quantified variable"
  | LVLocal -> Format.pp_print_string fmt "local parameter"


let dkey_check = Kernel.register_category "check"
(* Use category "check:strict" to enable stricter tests *)
let dkey_check_volatile = Kernel.register_category "check:strict:volatile"


class check ?(is_normalized=true) what : Visitor.frama_c_visitor =
  let check_abort fmt =
    Kernel.fatal ~current:true ("[AST Integrity Check]@ %s@ " ^^ fmt) what
  in
  let abort_if cond = if cond then check_abort else Log.nullprintf in
  let check_label s =
    let rec has_label = function
      | Label _ :: _ -> ()
      | [] ->
        check_abort
          "Statement is referenced by \\at or goto without having a label"
      | _ :: rest -> has_label rest
    in has_label s.labels
  in
  object(self)
    inherit Visitor.frama_c_inplace as plain
    val known_enuminfos = Enuminfo.Hashtbl.create 7
    val known_enumitems = Enumitem.Hashtbl.create 7
    val known_loop_annot_id = Hashtbl.create 7
    val known_code_annot_id = Hashtbl.create 7
    val known_fields = Fieldinfo.Hashtbl.create 7
    val known_compinfos = Compinfo.Hashtbl.create 7
    val known_stmts = Stmt.Hashtbl.create 7
    val known_vars = Varinfo.Hashtbl.create 7
    val known_logic_info = Logic_var.Hashtbl.create 7
    val mutable local_vars = Varinfo.Set.empty
    val known_logic_vars = Logic_var.Hashtbl.create 7
    val switch_cases = Stmt.Hashtbl.create 7
    val unspecified_sequence_calls = Stack.create ()
    val mutable labelled_stmt = []

    val mutable globals_functions = Varinfo.Set.empty
    val mutable globals_vars = Varinfo.Set.empty

    val quant_orig = Stack.create ()

    method private remove_globals_function vi =
      globals_functions <- Varinfo.Set.remove vi globals_functions

    method private remove_globals_var vi =
      globals_vars <- Varinfo.Set.remove vi globals_vars

    method! venuminfo ei =
      Enuminfo.Hashtbl.add known_enuminfos ei ei;
      Cil.DoChildren

    method! venumitem ei =
      let orig =
        try Enuminfo.Hashtbl.find known_enuminfos ei.eihost
        with Not_found -> check_abort "Unknown enuminfo %s" ei.eihost.ename
      in
      if orig != ei.eihost then
        check_abort "Item %s is not tied correctly to its enuminfo %s"
          ei.einame
          ei.eihost.ename;
      Enumitem.Hashtbl.add known_enumitems ei ei;
      Cil.DoChildren

    method private remove_unspecified_sequence_calls s =
      Stack.iter
        (fun calls -> calls:= Stmt.Set.remove s !calls)
        unspecified_sequence_calls

    method! vvdec v =
      Kernel.debug ~dkey:dkey_check "Declaration of %s(%d)" v.vname v.vid;
      if Varinfo.Hashtbl.mem known_vars v then
        (let v' = Varinfo.Hashtbl.find known_vars v in
         if v != v' then (* we can see the declaration twice
                            (decl and def in fact) *)
           (check_abort "variables %s and %s have the same id (%d)"
              v.vname v'.vname v.vid))
      else
        Varinfo.Hashtbl.add known_vars v v;
      match v.vlogic_var_assoc with
      | None -> Cil.DoChildren
      | Some ({ lv_origin = Some v'} as lv) when v == v' ->
        Kernel.debug ~dkey:dkey_check
          "var %s(%d) has an associated %s(%d)"
          v.vname v.vid lv.lv_name lv.lv_id;
        (match lv.lv_type with
         | Ctype t ->
           if not (Cil_datatype.TypNoUnroll.equal t v.vtype) then
             check_abort
               "C variable %s and its associated variable do not have the \
                same type:@\nC     type is %a@\nLogic type is %a"
               v.vname Cil_datatype.Typ.pretty v.vtype
               Cil_datatype.Typ.pretty t
         | lt ->
           check_abort
             "Logic variable %s is associated to a C variable but has \
              a purely logic type, %a@."
             lv.lv_name Cil_datatype.Logic_type.pretty lt);
        Cil.DoChildren
      | Some lv ->
        (check_abort "C variable %s is not properly referenced by its \
                      associated logic variable %s"
           v.vname lv.lv_name)

    method! vvrbl v =
      let not_shared () =
        check_abort "variable %s is not shared between definition and use" v.vname
      in
      let unknown () = check_abort "variable %s is not declared" v.vname in
      if not v.vglob || not (Ast_info.is_frama_c_builtin v.vname) then
        (try
           if Varinfo.Hashtbl.find known_vars v != v then not_shared ()
         with Not_found -> unknown ()
        );
      Cil.DoChildren

    method! vquantifiers l =
      let orig =
        try Stack.top quant_orig
        with Stack.Empty ->
          check_abort
            "Internal error of check visitor: don't know which origin a logic \
             variable should be checked against"
      in
      List.iter
        (fun lv ->
           if lv.lv_kind <> orig then
             check_abort
               "logic variable %a is flagged as %a but declared as a %a"
               Printer.pp_logic_var lv
               pretty_logic_var_kind lv.lv_kind pretty_logic_var_kind lv.lv_kind)
        l;
      Cil.DoChildren

    method! vlogic_var_decl lv =
      Logic_var.Hashtbl.add known_logic_vars lv lv;
      match lv.lv_origin with
      (* lvkind for purely logical variables is checked at the parent level. *)
      | None -> Cil.DoChildren
      | Some v when lv.lv_kind <> LVC ->
        check_abort
          "logic variable %a as an associated variable %a, but is not \
           flagged as having a C origin"
          Printer.pp_logic_var lv Printer.pp_varinfo v
      | Some { vlogic_var_assoc = Some lv' } when lv == lv' -> Cil.DoChildren
      | Some v ->
        check_abort
          "logic variable %a is not properly referenced by the original \
           C variable %a"
          Printer.pp_logic_var lv Printer.pp_varinfo v

    method! vlogic_var_use v =
      if v.lv_name <> "\\exit_status" then begin
        if Logic_env.is_builtin_logic_function v.lv_name then begin
          if not
              (List.exists (fun x -> x.l_var_info == v)
                 (Logic_env.find_all_logic_functions v.lv_name))
          then
            check_abort
              "Built-in logic variable %s information is not shared \
               between environment and use"
              v.lv_name
        end else begin
          let unknown () =
            check_abort "logic variable %s (%d) is not declared" v.lv_name v.lv_id
          in
          let not_shared () =
            check_abort
              "logic variable %s (%d) is not shared between definition and use"
              v.lv_name v.lv_id
          in
          try
            if Logic_var.Hashtbl.find known_logic_vars v != v then not_shared ()
          with Not_found -> unknown ()
        end
      end;
      Cil.DoChildren

    method! vfunc f =
      (* Initial AST does not have kf *)
      if is_normalized then begin
        let kf = Extlib.the self#current_kf in
        if not (Kernel_function.is_definition kf) then
          check_abort
            "Kernel function %a is supposed to be a prototype, but it has a body"
            Kernel_function.pretty kf;
        if Kernel_function.get_definition kf != f then
          check_abort
            "Body of %a is not shared between kernel function and AST"
            Kernel_function.pretty kf;
      end;
      labelled_stmt <- [];
      Stmt.Hashtbl.clear known_stmts;
      Stmt.Hashtbl.clear switch_cases;
      local_vars <- Varinfo.Set.empty;
      List.iter
        (fun x -> local_vars <- Varinfo.Set.add x local_vars) f.slocals;
      let print_stmt fmt stmt =
        Format.fprintf fmt "@[%a (%d)@]" Printer.pp_stmt stmt stmt.sid
      in
      let check f =
        if Stmt.Hashtbl.length switch_cases <> 0 then
          begin
            Stmt.Hashtbl.iter
              (fun x _ ->
                 check_abort
                   "In function %a, statement %a \
                    does not appear in body of switch while porting a \
                    case or default label."
                   Printer.pp_varinfo f.svar print_stmt x)
              switch_cases
          end;
        List.iter
          (fun stmt ->
             try
               let stmt' = Stmt.Hashtbl.find known_stmts stmt in
               if  stmt' != stmt then
                 check_abort
                   "Label @[%a@]@ in function %a@ \
                    is not linked to the correct statement:@\n\
                    statement in AST is %a@\n\
                    statement referenced in goto or \\at is %a"
                   Printer.pp_stmt {stmt with skind = Instr (Skip (Stmt.loc stmt)) }
                   Printer.pp_varinfo f.svar
                   print_stmt stmt'
                   print_stmt stmt
             with Not_found ->
               check_abort
                 "Label @[%a@]@ in function %a@ \
                  does not refer to an existing statement"
                 Printer.pp_stmt {stmt with skind = Instr (Skip (Stmt.loc stmt)) }
                 Printer.pp_varinfo f.svar)
          labelled_stmt;
        labelled_stmt <- [];
        let check_one_stmt stmt _ =
          let check_cfg_edge stmt' =
            try
              let ast_stmt = Stmt.Hashtbl.find known_stmts stmt' in
              if  ast_stmt != stmt' then
                check_abort
                  "cfg info of statement %a in function %a \
                   is not linked to correct statement:@\n\
                   statement in AST is %a@\n\
                   statement referenced in cfg info is %a"
                  print_stmt stmt
                  Printer.pp_varinfo f.svar
                  print_stmt ast_stmt
                  print_stmt stmt'
            with Not_found ->
              check_abort
                "cfg info of statement %a in function %a does not \
                 refer to an existing statement.@\n\
                 Referenced statement is %a"
                print_stmt stmt Printer.pp_varinfo f.svar print_stmt stmt'
          in
          List.iter check_cfg_edge stmt.succs;
          List.iter check_cfg_edge stmt.preds;
          match stmt.skind with
          | Return _ | Throw _ ->
            if stmt.succs <> [] then
              check_abort
                "return statement %a in function %a \
                 has successors:@\n%a"
                print_stmt stmt Printer.pp_varinfo f.svar
                (Pretty_utils.pp_list ~sep:"@\n" print_stmt) stmt.succs
          |  Instr(Call (_, called, _, _))
            when Cil.typeHasAttribute "noreturn" (Cil.typeOf called) ->
            if stmt.succs <> [] then
              check_abort
                "exit statement %a in function %a \
                 has successors:@\n%a"
                print_stmt stmt Printer.pp_varinfo f.svar
                (Pretty_utils.pp_list ~sep:"@\n" print_stmt) stmt.succs
          |  Instr(Call (_, { enode = Lval(Var called,NoOffset)}, _, _))
            when Cil.hasAttribute "noreturn" called.vattr ->
            if stmt.succs <> [] then
              check_abort
                "exit statement %a in function %a \
                 has successors:@\n%a"
                print_stmt stmt Printer.pp_varinfo f.svar
                (Pretty_utils.pp_list ~sep:"@\n" print_stmt) stmt.succs
          | _ ->
            (* unnormalized code may not contain return statement,
               leaving perfectly normal statements without succs. *)
            if is_normalized && stmt.succs = [] then
              check_abort
                "statement %a in function %a has no successor."
                print_stmt stmt Printer.pp_varinfo f.svar
        in
        Stmt.Hashtbl.iter check_one_stmt known_stmts;
        Stmt.Hashtbl.clear known_stmts;
        if not (Varinfo.Set.is_empty local_vars) then begin
          check_abort
            "Local variables %a of function %a are not part of any block"
            (Pretty_utils.pp_list ~sep:",@ " Printer.pp_varinfo)
            (Varinfo.Set.elements local_vars)
            Printer.pp_varinfo f.svar
        end;
        f
      in
      Cil.ChangeDoChildrenPost(f,check)

    method! vstmt_aux s =
      Stmt.Hashtbl.add known_stmts s s;
      Stmt.Hashtbl.remove switch_cases s;
      self#remove_unspecified_sequence_calls s;
      (match s.skind with
       | Goto (s,_) ->
         check_label !s;
         labelled_stmt <- !s :: labelled_stmt; Cil.DoChildren
       | Switch(_,_,cases,loc) ->
         List.iter (fun s -> Stmt.Hashtbl.add switch_cases s loc) cases;
         Cil.DoChildren
       | UnspecifiedSequence seq ->
         let calls =
           List.fold_left
             (fun acc (_,_,_,_,calls) ->
                List.fold_left (fun acc x -> Stmt.Set.add !x acc) acc calls)
             Stmt.Set.empty
             seq
         in
         Stack.push (ref calls) unspecified_sequence_calls;
         let f s =
           let calls = Stack.pop unspecified_sequence_calls in
           if Stmt.Set.is_empty !calls then s
           else
             check_abort
               "@[Calls referenced in unspecified sequence \
                are not in the AST:@[<v>%a@]@]"
               (Pretty_utils.pp_list ~sep:"@ " Printer.pp_stmt)
               (Stmt.Set.elements !calls)
         in Cil.ChangeDoChildrenPost(s,f)
       | If (_,bt,be,_) -> begin
           (** Check that we have 2 successors, in the right order (then before
               else) *)
           match s.succs with
           | [st; se] -> begin
               (match bt.bstmts with
                | st' :: _ ->
                  abort_if (not (st == st')) "Invalid 'then' successor for If"
                | _ -> ());
               (match be.bstmts with
                | se' :: _ ->
                  abort_if (not (se == se')) "Invalid 'else' successor for If"
                | _ -> ());
               Cil.DoChildren
             end
           | l -> check_abort "If with %d successors" (List.length l)
         end
       | _ -> Cil.DoChildren);

    method! vblock b =
      (* ensures that the blocals are part of the locals of the function. *)
      List.iter
        (fun v ->
           if Varinfo.Set.mem v local_vars then begin
             local_vars <- Varinfo.Set.remove v local_vars;
           end else begin
             check_abort
               "In function %a, variable %a is supposed to be local to a block \
                but not mentioned in the function's locals."
               Printer.pp_varinfo (Kernel_function.get_vi (Extlib.the self#current_kf))
               Printer.pp_varinfo v
           end)
        b.blocals;
      Cil.DoChildren

    method! vcode_annot ca =
      if Hashtbl.mem known_code_annot_id ca.annot_id then
        (check_abort "duplicated code annotation")
      else Hashtbl.add known_code_annot_id ca.annot_id (); Cil.DoChildren

    method! voffs = function
      | NoOffset -> Cil.SkipChildren
      | Index _ -> Cil.DoChildren
      | Field(fi,_) ->
        begin
          try
            if not (fi == Fieldinfo.Hashtbl.find known_fields fi)
            then
              (check_abort
                 "field %s of type %s(%d) is not \
                  shared between declaration and use"
                 fi.fname fi.fcomp.cname fi.fcomp.ckey)
          with Not_found ->
            (check_abort "field %s of type %s(%d) is unbound in the AST"
               fi.fname fi.fcomp.cname fi.fcomp.ckey)
        end;
        Cil.DoChildren

    method! vterm_offset = function
      | TNoOffset -> Cil.SkipChildren
      | TIndex _ -> Cil.DoChildren
      | TModel(mi,_) ->
        (try
           let mi' = Logic_env.find_model_field mi.mi_name mi.mi_base_type in
           if mi' != mi then begin
             check_abort
               "model field %s of type %a is not shared \
                between declaration and use"
               mi.mi_name Printer.pp_typ mi.mi_base_type
           end
         with Not_found ->
           check_abort "unknown model field %s in type %a"
             mi.mi_name Printer.pp_typ mi.mi_base_type);
        Cil.DoChildren
      | TField(fi,_) ->
        begin
          try
            if not (fi == Fieldinfo.Hashtbl.find known_fields fi)
            then
              (check_abort "field %s of type %s is not \
                            shared between declaration and use"
                 fi.fname fi.fcomp.cname)
          with Not_found ->
            (check_abort
               "field %s of type %s(%d) is unbound in the AST"
               fi.fname fi.fcomp.cname fi.fcomp.ckey)
        end;
        Cil.DoChildren

    method private check_ei: 'a. enumitem -> 'a Cil.visitAction =
      fun ei ->
        try
          let ei' = Enumitem.Hashtbl.find known_enumitems ei in
          if ei != ei' then
            check_abort "enumitem %s is not shared between declaration and use"
              ei.einame;
          Cil.DoChildren
        with Not_found ->
          check_abort "enumitem %s is used but not declared"
            ei.einame

    method private check_logic_app li args =
      let expect = List.length li.l_profile in
      let actual = List.length args in
      if not (expect = actual) then
        check_abort "Function %a expects %d arguments but is used with %d"
          Printer.pp_logic_var li.l_var_info expect actual;
      List.iter2
        (fun lv arg ->
           if not
               (Logic_utils.is_instance_of li.l_tparams arg.term_type lv.lv_type)
           then
             check_abort
               "term %a has type %a, but is used as a parameter of type %a"
               Printer.pp_term arg Printer.pp_logic_type arg.term_type
               Printer.pp_logic_type lv.lv_type)
        li.l_profile args

    method! vterm t =
      match t.term_node with
      | TLval _ ->
        begin match t.term_type with
          | Ctype ty ->
            ignore
              (Kernel.verify (not (Cil.isVoidType ty))
                 "logic term with void type:%a" Printer.pp_term t);
            Cil.DoChildren
          | _ -> Cil.DoChildren
        end
      | TConst (LEnum ei) -> self#check_ei ei
      | Tif (_,t1,t2) ->
        if not (Cil_datatype.Logic_type.equal t1.term_type t2.term_type) then
          check_abort
            "Conditional operator %a@\nFirst branch has type %a@\n\
             Second branch has type %a"
            Printer.pp_term t
            Printer.pp_logic_type t1.term_type
            Printer.pp_logic_type t2.term_type;
        Cil.DoChildren
      | Tlet(li,_) ->
        if li.l_var_info.lv_kind <> LVLocal then
          check_abort
            "Local logic variable %a is flagged with wrong origin"
            Printer.pp_logic_var li.l_var_info;
        Cil.DoChildren
      | Tlambda _ ->
        Stack.push LVFormal quant_orig;
        Cil.DoChildrenPost (fun t -> ignore (Stack.pop quant_orig); t)
      | Tcomprehension _ ->
        Stack.push LVQuant quant_orig;
        Cil.DoChildrenPost (fun t -> ignore (Stack.pop quant_orig); t)
      | Tapp(li,_,args) ->
        (match li.l_type with
         | Some ty when
             Logic_utils.is_instance_of li.l_tparams ty t.term_type -> ()
         | Some ty ->
           check_abort
             "logic function %a has return type %a, \
              but application %a has type %a"
             Printer.pp_logic_var li.l_var_info
             Printer.pp_logic_type ty
             Printer.pp_term t
             Printer.pp_logic_type t.term_type
         | None ->
           check_abort "predicate %a is used as a logic function"
             Printer.pp_logic_var li.l_var_info);
        self#check_logic_app li args;
        Cil.DoChildren
      | _ -> Cil.DoChildren

    method! vinitoffs = self#voffs

    method! vcompinfo c =
      Kernel.debug2
        ~dkey:dkey_check "Checking composite type %s(%d)" c.cname c.ckey;
      Compinfo.Hashtbl.add known_compinfos c c;
      Kernel.debug2
        ~dkey:dkey_check "Adding fields for type %s(%d)" c.cname c.ckey;
      List.iter (fun x -> Fieldinfo.Hashtbl.add known_fields x x) c.cfields;
      Cil.DoChildren

    method! vfieldinfo f =
      Kernel.debug2
        ~dkey:dkey_check "Check field %s of type %s" f.fname f.fcomp.cname;
      try
        let c = Compinfo.Hashtbl.find known_compinfos f.fcomp in
        if f.fcomp != c then
          check_abort
            "field %s of type %s does not refer to the appropriate compinfo node"
            f.fname f.fcomp.cname;
        Cil.DoChildren
      with Not_found ->
        check_abort
          "field %s belongs to an unknown type %s" f.fname f.fcomp.cname

    (* In non-normalized mode, we can't rely on the Globals tables used by
       the normal Frama-C's vglob: jump directly to vglob_aux. *)
    method! vglob g = if is_normalized then plain#vglob g else self#vglob_aux g

    method! vglob_aux g =
      match g with
      | GFunDecl(_,v,_) ->
        self#remove_globals_function v;
        if not (Cil.isFunctionType v.vtype) then
          check_abort "Function %a has non-function type" Printer.pp_varinfo v;
        if is_normalized then begin
          if v.vdefined &&
             not (Kernel_function.is_definition (Globals.Functions.get v))
          then
            check_abort
              "Function %s(%d) is supposed to be defined, \
               but not registered as such"
              v.vname v.vid;
          if not v.vdefined &&
             Kernel_function.is_definition (Globals.Functions.get v)
          then
            check_abort
              "Function %s has a registered definition, \
               but is supposed to be only declared"
              v.vname
        end;
        (match Cil.splitFunctionType v.vtype with
         | (_,None,_,_) -> ()
         | (_,Some l,_,_) ->
           if is_normalized then begin
             try
               let l' = Cil.getFormalsDecl v in
               if List.length l <> List.length l' then
                 check_abort
                   "prototype %s has %d arguments but is associated to \
                    %d formals in FormalsDecl"
                   v.vname (List.length l) (List.length l')
               else
                 let kf = Globals.Functions.get v in
                 let l'' = Kernel_function.get_formals kf in
                 if List.length l' <> List.length l'' then
                   check_abort
                     "mismatch between FormalsDecl and Globals.Functions \
                      on prototype %s." v.vname;
                 if Kernel_function.is_definition kf then begin
                   List.iter2
                     (fun v1 v2 ->
                        if v1 != v2 then
                          check_abort
                            "formal parameters of %s are not shared \
                             between declaration and definition"
                            v.vname)
                     l' l''
                 end
             with Not_found ->
               check_abort
                 "prototype %s (%d) has no associated \
                  parameters in FormalsDecl"
                 v.vname v.vid
           end);
        Cil.DoChildren
      | GVarDecl(v,_) | GVar(v,_,_) ->
        if Cil.isFunctionType v.vtype then
          check_abort "Variable %a has function type" Printer.pp_varinfo v;
        self#remove_globals_var v;
        Cil.DoChildren
      | GFun (f,_) ->
        if not (Cil.isFunctionType f.svar.vtype) then
          check_abort "Function %a has non-function type"
            Printer.pp_varinfo f.svar;
        if not f.svar.vdefined then
          check_abort
            "Function %s has a definition, but is considered as not defined"
            f.svar.vname;
        self#remove_globals_function f.svar; Cil.DoChildren
      | _ -> Cil.DoChildren

    method! vfile _ =
      let check_end f =
        if not (Cil_datatype.Varinfo.Set.is_empty globals_functions)
           || not (Cil_datatype.Varinfo.Set.is_empty globals_vars)
        then begin
          let print_var_vid fmt vi =
            Format.fprintf fmt "%a(%d)" Printer.pp_varinfo vi vi.vid
          in
          check_abort
            "Following functions and variables are present in global tables but \
             not in AST:%a%a"
            (Pretty_utils.pp_list ~pre:"@\nFunctions:@\n" ~sep:"@ " print_var_vid)
            (Cil_datatype.Varinfo.Set.elements globals_functions)
            (Pretty_utils.pp_list ~pre:"@\nVariables:@\n" ~sep:"@ " print_var_vid)
            (Cil_datatype.Varinfo.Set.elements globals_vars)
        end;
        f
      in
      Cil.DoChildrenPost check_end

    method! vannotation a =
      match a with
      | Dfun_or_pred (li,_) | Dinvariant (li,_) | Dtype_annot (li,_) ->
        if
          not
            (List.memq li
               (Logic_env.find_all_logic_functions li.l_var_info.lv_name))
        then
          check_abort
            "Global logic function %a information is not in the environment"
            Printer.pp_logic_var li.l_var_info;
        if li.l_var_info.lv_kind <> LVGlobal then
          check_abort
            "Global logic function %a is flagged with a wrong origin"
            Printer.pp_logic_var li.l_var_info;
        Cil.DoChildren
      | Dmodel_annot (mi, _) ->
        (try
           let mi' = Logic_env.find_model_field mi.mi_name mi.mi_base_type in
           if mi != mi' then
             check_abort
               "field %s of type %a is not shared between \
                declaration and environment"
               mi.mi_name Printer.pp_typ mi.mi_base_type;
         with Not_found ->
           check_abort
             "field %s of type %a is not present in environment"
             mi.mi_name Printer.pp_typ mi.mi_base_type);
        Cil.DoChildren
      | _ -> Cil.DoChildren

    method! vlogic_label = function
      | StmtLabel l ->
        check_label !l;
        labelled_stmt <- !l::labelled_stmt;
        Cil.SkipChildren
      | _ -> Cil.DoChildren

    method! vpredicate = function
      | Papp(li,_,args) ->
        (match li.l_type with
         | None -> ()
         | Some _ ->
           check_abort "Logic function %a is used as a predicate"
             Printer.pp_logic_var li.l_var_info);
        self#check_logic_app li args;
        Cil.DoChildren
      | Plet(li,_) ->
        if li.l_var_info.lv_kind <> LVLocal then
          check_abort
            "Local logic variable %a is flagged with wrong origin"
            Printer.pp_logic_var li.l_var_info;
        Cil.DoChildren
      | Pforall _ | Pexists _ ->
        Stack.push LVQuant quant_orig;
        Cil.DoChildrenPost (fun p -> ignore (Stack.pop quant_orig); p)
      | _ -> Cil.DoChildren

    method! vlogic_info_decl li =
      Logic_var.Hashtbl.add known_logic_info li.l_var_info li;
      List.iter
        (fun lv ->
           if lv.lv_kind <> LVFormal then
             check_abort
               "Formal parameter %a of logic function/predicate %a is \
                flagged with wrong origin"
               Printer.pp_logic_var lv Printer.pp_logic_var li.l_var_info)
        li.l_profile;
      Cil.DoChildren

    method! vlogic_info_use li =
      let unknown () =
        check_abort "logic function %s has no information" li.l_var_info.lv_name
      in
      let not_shared () =
        check_abort
          "logic function %s information is not shared between declaration and \
           use"
          li.l_var_info.lv_name
      in
      if Logic_env.is_builtin_logic_function li.l_var_info.lv_name then
        begin
          if not
              (List.memq li
                 (Logic_env.find_all_logic_functions li.l_var_info.lv_name))
          then
            check_abort "Built-in logic function %s information is not shared \
                         between environment and use"
              li.l_var_info.lv_name
        end else begin
        try
          if not
              (li == Logic_var.Hashtbl.find known_logic_info li.l_var_info)
          then not_shared ()
        with Not_found -> unknown ()
      end;
      Cil.DoChildren

    val accept_array = Stack.create ()

    method private accept_array = function
      | SizeOfE _ | AlignOfE _ | CastE _ -> true
      | _ -> false

    method! vexpr e =
      if Cil.typeHasAttribute "volatile" (Cil.typeOf e) then begin
        let volatile_problem : (_, _, _) format =
          "Expression with volatile qualification %a"
        in
        if Kernel.is_debug_key_enabled dkey_check_volatile then
          check_abort volatile_problem Printer.pp_exp e
        else
          Kernel.warning ~current:true volatile_problem Printer.pp_exp e
      end;
      match e.enode with
      | Const (CEnum ei) -> self#check_ei ei
      | Lval lv when
          Cil.isArrayType (Cil.typeOfLval lv)
          && (Stack.is_empty accept_array || not (Stack.top accept_array)) ->
        check_abort "%a is an array, but used as an lval"
          Printer.pp_lval lv
      | StartOf lv when not (Cil.isArrayType (Cil.typeOfLval lv)) ->
        check_abort "%a is supposed to be an array, but has type %a"
          Printer.pp_lval lv Printer.pp_typ (Cil.typeOfLval lv)
      | _ ->
        Stack.push (self#accept_array e.enode) accept_array;
        Cil.ChangeDoChildrenPost (e,
                                  fun e -> ignore (Stack.pop accept_array); e)


    method! vinst i =
      match i with
      | Call(lvopt,{ enode = Lval(Var f, NoOffset)},args,_) ->
        let (treturn,targs,is_variadic,_) = Cil.splitFunctionTypeVI f in
        if Cil.isVoidType treturn && lvopt != None then
          check_abort
            "in call %a, assigning result of a function returning void"
            Printer.pp_instr i;
        (match lvopt with
         | None -> ()
         | Some lv ->
           let tlv = Cil.typeOfLval lv in
           if not (Cabs2cil.allow_return_collapse ~tlv ~tf:treturn) then
             check_abort "in call %a, cannot implicitly cast from \
                          function return type %a to type of %a (%a)"
               Printer.pp_instr i
               Printer.pp_typ treturn
               Printer.pp_lval lv
               Printer.pp_typ tlv);
        let rec aux l1 l2 =
          match l1,l2 with
          | [],[] -> Cil.DoChildren
          | _::_, [] ->
            check_abort "call %a has too few arguments" Printer.pp_instr i
          | [],e::_ ->
            if is_variadic then Cil.DoChildren
            else
              check_abort "call %a has too many arguments, starting from %a"
                Printer.pp_instr i Printer.pp_exp e
          | (_,ty1,_)::l1,arg::l2 ->
            let ty2 = Cil.typeOf arg in
            if not (is_admissible_conversion arg ty2 ty1) then
              check_abort "in call %a, arg %a has type %a instead of %a"
                Printer.pp_instr i
                Printer.pp_exp arg
                Printer.pp_typ ty2
                Printer.pp_typ ty1;
            aux l1 l2
        in
        (match targs with
         | None -> Cil.DoChildren
         | Some targs -> aux targs args)
      | _ -> Cil.DoChildren

    method! vtype ty =
      (match ty with
       | TArray (_, _, _, la) ->
         let elt, _ = Cil.splitArrayAttributes la in
         if elt != [] then
           Kernel.fatal
             "Element attribute on array type itself: %a"
             Printer.pp_attributes elt
       | _ -> ()
      );
      Cil.DoChildren


    initializer
      let add_func kf =
        let vi = Kernel_function.get_vi kf in
        if vi.vsource then
          globals_functions <- Cil_datatype.Varinfo.Set.add vi globals_functions
      in
      let add_var vi _ =
        if vi.vsource then
          globals_vars <- Cil_datatype.Varinfo.Set.add vi globals_vars
      in
      Globals.Functions.iter add_func;
      Globals.Vars.iter add_var

  end


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

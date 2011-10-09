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

open Cil_types
open Cil
open Cilutil
open Extlib
open Visitor
open Pretty_utils
open Cil_datatype

type file =
  | NeedCPP of
      string (* filename of the [.c] to preprocess *)
      * string (* Preprocessor command.
                  [filename.c -o tempfilname.i] will be appended at the
                    end.*)
  | NoCPP of string (** filename of a preprocessed [.c] *)
  | External of string * string (* file * name of plug-in that handles it *)

module D =
  Datatype.Make
    (struct
      include Datatype.Serializable_undefined
      type t = file
      let name = "File"
      let reprs = [ NeedCPP("", ""); NoCPP ""; External("", "") ]
      let structural_descr = Structural_descr.Abstract
      let mem_project = Datatype.never_any_project
      let copy = Datatype.identity (* immutable strings *)
      let internal_pretty_code p_caller fmt t =
        let pp fmt = match t with
          | NoCPP s -> Format.fprintf fmt "@[File.NoCPP %S@]" s
          | External (f,p) ->
            Format.fprintf fmt "@[File.External (%S,%S)@]" f p
          | NeedCPP (a,b) -> Format.fprintf fmt "@[File.NeedCPP (%S,%S)@]" a b
        in
        Type.par p_caller Type.Call fmt pp
     end)
include D

let check_suffixes = Hashtbl.create 17
let new_file_type = Hashtbl.add check_suffixes
let get_suffixes () =
  Hashtbl.fold
    (fun s _ acc -> s :: acc)
    check_suffixes
    [ ".c"; ";C"; ".i"; ".h" ]

let get_name = function NeedCPP (s,_) | NoCPP s | External (s,_) -> s

(* ************************************************************************* *)
(** {2 Preprocessor command} *)
(* ************************************************************************* *)

(* the preprocessor command is:
   If the program has an explicit argument -cpp-command "XX -Y"
   (quotes are required by the shell)
   then XX -Y
   else if the CPP environment variable is set, use it
   else the built-in "gcc -C -E -I." *)
let get_preprocessor_command () =
  let cmdline = Kernel.CppCommand.get() in
  if cmdline <> "" then cmdline
  else
    try Sys.getenv "CPP"
    with Not_found -> "gcc -C -E -I."

let from_filename ?(cpp=get_preprocessor_command ()) f =
  if Filename.check_suffix f ".i" then begin
    NoCPP f
  end else
    let suf =
      try
        let suf_idx = String.rindex f '.' in
        String.sub f suf_idx (String.length f - suf_idx)
      with Not_found -> (* raised by String.rindex if '.' \notin f *)
        ""
    in
    if Hashtbl.mem check_suffixes suf then External (f, suf)
    else NeedCPP (f, cpp)

(* ************************************************************************* *)
(** {2 Internal states} *)
(* ************************************************************************* *)

module Files : sig
  val get: unit -> t list
  val register: t list -> unit
  val pre_register: t -> unit
  val is_computed: unit -> bool
  val reset: unit -> unit
end = struct

  module S =
    State_builder.List_ref
      (D)
      (struct
         let dependencies =
           [ Kernel.CppCommand.self;
             Kernel.CppExtraArgs.self;
             Kernel.Files.self ]
         let name = "Files for preprocessing"
         let kind = `Internal
       end)

  let () =
    State_dependency_graph.Static.add_dependencies
      ~from:S.self
      [ Ast.self; Ast.UntypedFiles.self; Cabshelper.Comments.self ]

  (* Allow to register files in advance, e.g. prolog files for plugins *)
  let pre_register file =
    let prev_files = S.get () in
    S.set (prev_files @ [file])

  let register files =
    if S.is_computed () then
      raise (Ast.Bad_Initialization "[File.register] Too many initializations");
    let prev_files = S.get () in
    S.set (prev_files @ files);
    S.mark_as_computed ()

  let get = S.get
  let is_computed () = S.is_computed ()

  let reset () =
    let selection = State_selection.Static.with_dependencies S.self in
    Project.clear ~selection ()

end

let get_all = Files.get
let pre_register = Files.pre_register

(*****************************************************************************)
(** {2 AST Integrity check}                                                  *)
(*****************************************************************************)

let is_admissible_conversion e ot nt =
  let ots = Cil.typeSigWithAttrs (fun _ -> []) ot in
  let nts = Cil.typeSigWithAttrs (fun _ -> []) nt in
  Cilutil.equals ots nts ||
    (match e.enode, Cil.unrollType nt with
      | Const(CEnum { eihost = ei }), TEnum(ei',[]) -> ei.ename = ei'.ename
      | _ -> false)

(* performs various consistency checks over a cil file.
   Code may vary depending on current development of the kernel and/or
   identified bugs.
   what is a short string indicating which AST is checked

   NB: some checks are performed on the CFG, so it must have been computed on
   the file that is checked.
*)
class check_file_aux is_normalized what: Visitor.frama_c_visitor  =
  let check_abort fmt =
    Kernel.fatal ~current:true ("[AST Integrity Check]@ %s@ " ^^ fmt) what
  in
  let check_label s =
    let rec has_label = function
        Label _ :: _ -> ()
      | [] ->
          check_abort
            "Statement is referenced by \\at or goto without having a label"
      | _ :: rest -> has_label rest
    in has_label s.labels
  in
object(self)
  inherit Visitor.generic_frama_c_visitor (Project.current())
    (Cil.inplace_visit())
  val known_enuminfos = Enuminfo.Hashtbl.create 7
  val known_enumitems = Enumitem.Hashtbl.create 7
  val known_loop_annot_id = Hashtbl.create 7
  val known_code_annot_id = Hashtbl.create 7
  val known_fields = Fieldinfo.Hashtbl.create 7
  val known_stmts = Stmt.Hashtbl.create 7
  val known_vars = Varinfo.Hashtbl.create 7
  val known_logic_info = Logic_var.Hashtbl.create 7
  val mutable local_vars = Varinfo.Set.empty
  val known_logic_vars = Logic_var.Hashtbl.create 7
  val switch_cases = Stmt.Hashtbl.create 7
  val unspecified_sequence_calls = Stack.create ()
  val mutable labelled_stmt = []

  method venuminfo ei =
    Enuminfo.Hashtbl.add known_enuminfos ei ei;
    DoChildren

  method venumitem ei =
    let orig =
      try
        Enuminfo.Hashtbl.find known_enuminfos ei.eihost
      with Not_found ->
        check_abort "Unknown enuminfo %a"
          !Ast_printer.d_ident ei.eihost.ename
    in
    if orig != ei.eihost then
      check_abort "Item %a is not tied correctly to its enuminfo %a"
        !Ast_printer.d_ident ei.einame
        !Ast_printer.d_ident ei.eihost.ename;
    Enumitem.Hashtbl.add known_enumitems ei ei;
    DoChildren

  method private remove_unspecified_sequence_calls s =
    Stack.iter
      (fun calls -> calls:= Stmt.Set.remove s !calls)
      unspecified_sequence_calls

  method vvdec v =
    if Varinfo.Hashtbl.mem known_vars v then
      (let v' = Varinfo.Hashtbl.find known_vars v in
       if v != v' then (* we can see the declaration twice
                          (decl and def in fact) *)
         (check_abort "variables %a and %a have the same id (%d)"
                       !Ast_printer.d_ident v.vname
                       !Ast_printer.d_ident v'.vname v.vid))
    else
      Varinfo.Hashtbl.add known_vars v v;
    match v.vlogic_var_assoc with
        None -> DoChildren
      | Some ({ lv_origin = Some v'} as lv) when v == v' ->
          Kernel.debug "var %s(%d) has an associated %s(%d)" v.vname v.vid
            lv.lv_name lv.lv_id;
          DoChildren
      | Some lv ->
          (check_abort "C variable %a is not properly referenced by its \
                          associated logic variable %a"
               !Ast_printer.d_ident v.vname !Ast_printer.d_ident lv.lv_name)

  method vvrbl v =
    (try
       if Varinfo.Hashtbl.find known_vars v != v then
         (check_abort "variable %a is not shared between definition and use"
           !Ast_printer.d_ident v.vname)
    with Not_found ->
      (check_abort "variable %a is not declared" !Ast_printer.d_ident v.vname)
    );
    DoChildren

  method vlogic_var_decl lv =
    Logic_var.Hashtbl.add known_logic_vars lv lv;
    match lv.lv_origin with
        None -> DoChildren
      | Some { vlogic_var_assoc = Some lv' } when lv == lv' -> DoChildren
      | Some v ->
          (check_abort
               "logic variable %a is not properly referenced by the original \
                C variable %a"
               !Ast_printer.d_ident lv.lv_name !Ast_printer.d_ident v.vname
            )

  method vlogic_var_use v =
    if v.lv_name <> "\\exit_status" then begin
      if Logic_env.is_builtin_logic_function v.lv_name then begin
        if not
          (List.exists (fun x -> x.l_var_info == v)
             (Logic_env.find_all_logic_functions v.lv_name))
        then
          check_abort "Built-in logic variable %a information is not shared \
                       between environment and use"
            !Ast_printer.d_ident v.lv_name
      end else begin
        try
          if Logic_var.Hashtbl.find known_logic_vars v != v then
            (check_abort
               "logic variable %a(%d) is not shared between definition and use"
               !Ast_printer.d_ident v.lv_name v.lv_id)
        with Not_found ->
          (check_abort "logic variable %a(%d) is not declared"
             !Ast_printer.d_ident  v.lv_name v.lv_id)
      end
    end;
    DoChildren

  method vfunc f =
    labelled_stmt <- [];
    Stmt.Hashtbl.clear known_stmts;
    Stmt.Hashtbl.clear switch_cases;
    local_vars <- Varinfo.Set.empty;
    List.iter
      (fun x -> local_vars <- Varinfo.Set.add x local_vars) f.slocals;
    let print_stmt fmt stmt =
      Format.fprintf fmt "@[%a(%d)@]" !Ast_printer.d_stmt stmt stmt.sid
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
                 Cil.d_var f.svar print_stmt x)
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
                   statement referenced in goto is %a"
                  !Ast_printer.d_stmt
                  {stmt with skind = Instr (Skip (Stmt.loc stmt)) }
                  !Ast_printer.d_var f.svar print_stmt stmt' print_stmt stmt
           with Not_found ->
             check_abort
               "Label @[%a@]@ in function %a@ \
                   does not refer to an existing statement"
               !Ast_printer.d_stmt
               ({stmt with skind = Instr (Skip (Stmt.loc stmt)) })
               !Ast_printer.d_var f.svar)
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
                print_stmt stmt !Ast_printer.d_var f.svar
                print_stmt ast_stmt print_stmt stmt'
          with Not_found ->
            check_abort
              "cfg info of statement %a in function %a does not \
               refer to an existing statement.@\n\
               Referenced statement is %a"
              print_stmt stmt !Ast_printer.d_var f.svar print_stmt stmt'
        in
        List.iter check_cfg_edge stmt.succs;
        List.iter check_cfg_edge stmt.preds;
        match stmt.skind with
          | Return _ ->
            if stmt.succs <> [] then
              check_abort
                "return statement %a in function %a \
                 has successors:@\n%a"
                print_stmt stmt !Ast_printer.d_var f.svar
                (Pretty_utils.pp_list ~sep:nl_sep print_stmt) stmt.succs
          |  Instr(Call (_, called, _, _))
              when hasAttribute "noreturn" (typeAttrs (typeOf called)) ->
            if stmt.succs <> [] then
              check_abort
                "exit statement %a in function %a \
                 has successors:@\n%a"
                print_stmt stmt !Ast_printer.d_var f.svar
                (Pretty_utils.pp_list ~sep:nl_sep print_stmt) stmt.succs
          |  Instr(Call (_, { enode = Lval(Var called,NoOffset)}, _, _))
              when hasAttribute "noreturn" called.vattr ->
            if stmt.succs <> [] then
              check_abort
                "exit statement %a in function %a \
                 has successors:@\n%a"
                print_stmt stmt !Ast_printer.d_var f.svar
                (Pretty_utils.pp_list ~sep:nl_sep print_stmt) stmt.succs
          | _ ->
            (* unnormalized code may not contain return statement,
               leaving perfectly normal statements without succs. *)
            if is_normalized && stmt.succs = [] then
              check_abort
                "statement %a in function %a has no successor."
                print_stmt stmt !Ast_printer.d_var f.svar
      in
      Stmt.Hashtbl.iter check_one_stmt known_stmts;
      Stmt.Hashtbl.clear known_stmts;
      if not (Varinfo.Set.is_empty local_vars) then begin
        check_abort
          "Local variables %a of function %a are not part of any block"
          (pp_list ~sep:(no_sep ^^ ",") Cil.d_var)
          (Varinfo.Set.elements local_vars)
          Cil.d_var f.svar
      end;
      f
    in
    ChangeDoChildrenPost(f,check)

  method vstmt_aux s =
    Stmt.Hashtbl.add known_stmts s s;
    Stmt.Hashtbl.remove switch_cases s;
    self#remove_unspecified_sequence_calls s;
    (match s.skind with
         Goto (s,_) ->
           check_label !s;
           labelled_stmt <- !s :: labelled_stmt; DoChildren
       | Switch(_,_,cases,loc) ->
           List.iter (fun s -> Stmt.Hashtbl.add switch_cases s loc) cases;
           DoChildren
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
                 "Calls referenced in unspecified sequence \
                  are not in the AST@\n%a"
                 (Pretty_utils.pp_list ~sep:Pretty_utils.nl_sep d_stmt)
                 (Stmt.Set.elements !calls)
           in ChangeDoChildrenPost(s,f)
       | _ -> DoChildren);

  method vblock b =
    (* ensures that the blocals are part of the locals of the function. *)
    List.iter
      (fun v ->
         if Varinfo.Set.mem v local_vars then begin
           local_vars <- Varinfo.Set.remove v local_vars;
         end else begin
           check_abort
             "In function %a, variable %a is supposed to be local to a block \
              but not mentioned in function's locals."
             Cil.d_var (Kernel_function.get_vi (Extlib.the self#current_kf))
             Cil.d_var v
         end)
      b.blocals;
    DoChildren

  method vcode_annot ca =
    if Hashtbl.mem known_code_annot_id ca.annot_id then
      (check_abort "duplicated code annotation")
    else Hashtbl.add known_code_annot_id ca.annot_id (); DoChildren

  method voffs = function
      NoOffset -> SkipChildren
    | Index _ -> DoChildren
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
        DoChildren

  method vterm_offset = function
      TNoOffset -> SkipChildren
    | TIndex _ -> DoChildren
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
               "field %s of type %s is unbound in the AST"
               fi.fname fi.fcomp.cname)
        end;
        DoChildren

  method private check_ei: 'a. enumitem -> 'a visitAction =
    fun ei ->
      try
        let ei' = Enumitem.Hashtbl.find known_enumitems ei in
        if ei != ei' then
          check_abort "enumitem %a is not shared between declaration and use"
            !Ast_printer.d_ident ei.einame;
        DoChildren
      with Not_found ->
        check_abort "enumitem %a is used but not declared"
          !Ast_printer.d_ident ei.einame

  method vterm t =
    match t.term_node with
      | TLval _ ->
          begin match t.term_type with
            | Ctype ty ->
                ignore
                  (Kernel.verify (not (isVoidType ty))
                     "logic term with void type:%a" d_term t);
                DoChildren
            | _ -> DoChildren
          end
      | Tat(_,StmtLabel l) ->
          check_label !l;
          labelled_stmt <- !l::labelled_stmt; DoChildren
      | TConst (CEnum ei) -> self#check_ei ei
      | _ -> DoChildren

  method vinitoffs = self#voffs

  method vglob_aux = function
      GCompTag(c,_) ->
        Kernel.debug "Adding fields for type %s(%d)" c.cname c.ckey;
        List.iter
          (fun x -> Fieldinfo.Hashtbl.add known_fields x x) c.cfields;
        DoChildren
    | GVarDecl(_,v,_) when Cil.isFunctionType v.vtype ->
        (match Cil.splitFunctionType v.vtype with
             (_,None,_,_) -> ()
           | (_,Some l,_,_) ->
               try
                 let l' = Cil.getFormalsDecl v in
                 if List.length l <> List.length l' then
                   (check_abort
                        "prototype %a has %d arguments but is associated to \
                         %d formals in FormalsDecl" !Ast_printer.d_ident v.vname
                        (List.length l) (List.length l'))
                 else
                   let kf = Globals.Functions.get v in
                   let l'' = Kernel_function.get_formals kf in
                   if List.length l' <> List.length l'' then
                     (check_abort
                          "mismatch between FormalsDecl and Globals.Functions \
                           on prototype %a." !Ast_printer.d_ident v.vname);
                    if Kernel_function.is_definition kf then begin
                      List.iter2
                        (fun v1 v2 ->
                           if v1 != v2 then
                             check_abort
                               "formal parameters of %a are not shared \
                               between declaration and definition"
                               !Ast_printer.d_ident v.vname)
                        l' l''
                    end
               with Not_found ->
                 (check_abort
                    "prototype %a(%d) has no associated \
                     parameters in FormalsDecl" !Ast_printer.d_ident v.vname
                    v.vid
                   )
        );
        DoChildren
    | _ -> DoChildren

  method vannotation a =
    match a with
        Dfun_or_pred (li,_) | Dinvariant (li,_) | Dtype_annot (li,_) ->
          if
            not
              (List.memq li
                 (Logic_env.find_all_logic_functions li.l_var_info.lv_name))
          then
            check_abort
              "Global logic function %a information is not in the environment"
              !Ast_printer.d_ident li.l_var_info.lv_name;
          DoChildren
      | _ -> DoChildren

  method vpredicate = function
      Pat(_,StmtLabel l) ->
        check_label !l;
        labelled_stmt <- !l::labelled_stmt; DoChildren
    | _ -> DoChildren

  method vlogic_info_decl li =
    Logic_var.Hashtbl.add known_logic_info li.l_var_info li;
    DoChildren

  method vlogic_info_use li =
    if Logic_env.is_builtin_logic_function li.l_var_info.lv_name then
      begin
        if not
          (List.memq li
             (Logic_env.find_all_logic_functions li.l_var_info.lv_name))
        then
          check_abort "Built-in logic function %a information is not shared \
                       between environment and use"
            !Ast_printer.d_ident li.l_var_info.lv_name
      end else begin
        try
          if not
            (li == Logic_var.Hashtbl.find known_logic_info li.l_var_info)
          then
              (check_abort "logic function %a information is \
                     not shared between declaration and use"
                 !Ast_printer.d_ident li.l_var_info.lv_name)
        with Not_found ->
          (check_abort "logic function %a has no information"
             !Ast_printer.d_ident li.l_var_info.lv_name)
      end;
    DoChildren

  method vexpr e =
    match e.enode with
      | Const (CEnum ei) -> self#check_ei ei
      | _ -> DoChildren

  method vinst i =
    match i with
      | Call(_,{ enode = Lval(Var f, NoOffset)},args,_) ->
        let (_,targs,is_variadic,_) = Cil.splitFunctionTypeVI f in
        let rec aux l1 l2 =
          match l1,l2 with
              [],[] -> DoChildren
            | _::_, [] ->
              check_abort "call %a has too few arguments" !Ast_printer.d_instr i
            | [],e::_ ->
              if is_variadic then DoChildren
              else
                check_abort "call %a has too many arguments, starting from %a"
                  !Ast_printer.d_instr i !Ast_printer.d_exp e
            | (_,ty1,_)::l1,arg::l2 ->
              let ty2 = Cil.typeOf arg in
              if not (is_admissible_conversion arg ty2 ty1) then
                check_abort "in call %a, arg %a has type %a instead of %a"
                  !Ast_printer.d_instr i
                  !Ast_printer.d_exp arg
                  !Ast_printer.d_type ty2
                  !Ast_printer.d_type ty1;
              aux l1 l2
        in
        (match targs with
            None -> DoChildren
          | Some targs -> aux targs args)
      | _ -> DoChildren

end

class check_file what = object inherit check_file_aux true what end

(* ************************************************************************* *)
(** {2 Initialisations} *)
(* ************************************************************************* *)

let safe_remove_file f =
  if Kernel.debug_atleast 3 then
    Kernel.debug ~level:3 "File %s generated" f
  else
    try Extlib.safe_remove f
    with Sys_error _ -> Kernel.warning "cannot remove temporary file %s" f

let parse = function
  | NoCPP f ->
      if not (Sys.file_exists  f) then
        Kernel.abort "preprocessed file %S does not exist" f;
      Frontc.parse f ()
  | NeedCPP (f, cmdl) ->
      if not (Sys.file_exists  f) then
        Kernel.abort "source file %S does not exist" f;
      let ppf =
        try Filename.temp_file (Filename.basename f) ".i"
        with Sys_error s -> Kernel.abort "cannot create temporary file: %s" s
      in
      let cmd supp_args in_file out_file =
        try
          (* Format.eprintf "-cpp-command=|%s|@\n" cmdl; *)
          (* look at the command line to find two "%s" or one "%1" and a "%2"
          *)
          let percent1 = String.index cmdl '%' in
          (* Format.eprintf "-cpp-command percent1=%d@\n" percent1;
             Format.eprintf "-cpp-command %%%c@\n" (String.get cmdl
             (percent1+1)); *)
          let percent2 = String.index_from cmdl (percent1+1) '%' in
          (* Format.eprintf "-cpp-command percent2=%d@\n" percent2;
             Format.eprintf "-cpp-command %%%c@\n" (String.get cmdl
             (percent2+1)); *)
          let file1, file2 =
            match String.get cmdl (percent1+1), String.get cmdl (percent2+1)
            with
            | '1', '2' ->
                in_file, out_file
                  (* "%1" followed by "%2" is used to printf 'ppf' after 'f' *)
            | '2', '1' ->
                out_file, in_file
            | _, _ -> raise (Invalid_argument "maybe a bad cpp command")
          in
          let cmd1 = String.sub cmdl 0 percent1 in
          (* Format.eprintf "-cpp-command cmd1=|%s|@\n" cmd1; *)
          let cmd2 =
            String.sub cmdl (percent1 + 2) (percent2 - (percent1 + 2))
          in
          (* Format.eprintf "-cpp-command cmd2=|%s|@\n" cmd2; *)
          let cmd3 =
            String.sub cmdl (percent2 + 2) (String.length cmdl - (percent2 + 2))
          in
          (* Format.eprintf "-cpp-command cmd3=|%s|@\n" cmd3; *)
          Format.sprintf "%s%s %s %s%s%s" cmd1
            (* using Filename.quote for filenames which contain space or
               shell metacharacters *)
            (Filename.quote file1)
            supp_args
            cmd2 (Filename.quote file2) cmd3
        with
        | Invalid_argument _
        | Not_found ->
            Format.sprintf "%s %s -o %s %s" cmdl
              supp_args
              (* using Filename.quote for filenames which contain space or
                 shell metacharacters *)
              (Filename.quote out_file) (Filename.quote in_file)
      in
      let supp_args =
        (Kernel.CppExtraArgs.get_set ~sep:" " ()) ^
          (if Kernel.ReadAnnot.get() && Kernel.PreprocessAnnot.get()
           then " -dD" else "")
      in
      Kernel.feedback "@{<i>preprocessing@} with \"%s %s %s\"" cmdl supp_args f;
      if Sys.command (cmd supp_args f ppf) <> 0 then begin
        Extlib.safe_remove ppf;
        Kernel.abort "failed to run: %s@\n\
you may set the CPP environment variable to select the proper \
preprocessor command or use the option \"-cpp-command\"."
          (cmd supp_args f ppf);
      end;
      let ppf =
        if Kernel.ReadAnnot.get() && Kernel.PreprocessAnnot.get()
        then begin
          let ppf' =
            try Logic_preprocess.file (cmd "") ppf
            with Sys_error _ as e ->
              Extlib.safe_remove ppf;
              Kernel.abort "preprocessing of annotations failed (%s)"
                (Printexc.to_string e)
          in
          safe_remove_file ppf ;
          ppf'
        end else ppf
      in
      let (cil,(_,defs)) = Frontc.parse ppf () in
      cil.fileName <- f;
      safe_remove_file ppf;
      (cil,(f,defs))
  | External (f,suf) ->
      if not (Sys.file_exists f) then
        Kernel.abort "file %S does not exist." f;
      try Hashtbl.find check_suffixes suf f
      with Not_found ->
        Kernel.abort "could not find a suitable plugin for parsing %s." f

(** Keep defined entry point even if not defined.
    This function is meant to be passed to {!Rmtmps.removeUnusedTemps}.*)
let keep_entry_point g =
  Rmtmps.isDefaultRoot g ||
    match g with
      GVarDecl(spec,v,_) ->
        Kernel.MainFunction.get () = v.vname &&
        not (is_empty_funspec spec)
    | _ -> false

let files_to_cil files =
  (* BY 2011-05-10 Deactivated this mark_as_computed. Does not see to
     do anything useful anymore, and causes problem with the self-recovering
     gui (commit 13295)
  (* mark as computed early in case of a typing error occur: do not type check
     the erroneous program twice. *)
     Ast.mark_as_computed (); *)
  let debug_globals files =
    let level = 6 in
    if Kernel.debug_atleast level then begin
      List.iter
        (fun f ->
           (* NB: don't use frama-C printer here, as the
              annotations tables are not filled yet. *)
           List.iter
             (fun g -> Kernel.debug ~level "%a" Cil.d_global g)
             f.globals)
        files
    end
  in
  (* Parsing and merging must occur in the very same order.
     Otherwise the order of files on the command line will not be consistantly
     handled. *)
    Kernel.feedback ~level:2 "parsing";
    let files,cabs =
      List.fold_left
        (fun (accf,accc) f ->
           try
             let f,c = parse f in
             f::accf, c::accc
           with exn when Cilmsg.had_errors () ->
             if Kernel.Debug.get () >= 1 then raise exn
             else
               Kernel.abort "skipping file %S that has errors." (get_name f))
        ([],[])
        files
    in
      Ast.UntypedFiles.set cabs;
      debug_globals files;

  (* Clean up useless parts *)
  Kernel.feedback ~level:2 "cleaning unused parts";
  Rmtmps.rmUnusedStatic := false; (* a command line option will be available*)
  (* remove unused functions. However, we keep declarations that have a spec,
     since they might be merged with another one which is used. If this is not
     the case, these declarations will be removed after Mergecil.merge. *)
  let keep_spec g =
    Rmtmps.isDefaultRoot g ||
      (match g with
           GVarDecl(spec,_,_) -> not (is_empty_funspec spec)
         | _ -> false)
  in
  List.iter (Rmtmps.removeUnusedTemps ~isRoot:keep_spec) files;
  debug_globals files;

  Kernel.feedback ~level:2 "symbolic link";
  let merged_file = Mergecil.merge files "whole_program" in
  (* dumpFile defaultCilPrinter stdout p; *)
  if Cilmsg.had_errors () then
    Kernel.abort "Target code cannot be parsed; aborting analysis.";
  debug_globals files;

  Rmtmps.removeUnusedTemps ~isRoot:keep_entry_point merged_file;
  if Kernel.UnspecifiedAccess.get()
  then begin
    let rec not_separated_offset offs1 offs2 =
      match offs1, offs2 with
          NoOffset,_ | _, NoOffset -> true
        | Field (f1,offs1), Field(f2,offs2) ->
            f1.fname = f2.fname && f1.fcomp.ckey = f2.fcomp.ckey &&
            not_separated_offset offs1 offs2
        | Index(i1,offs1), Index(i2,offs2) ->
            (match
               Cil.isInteger (Cil.constFold true i1),
               Cil.isInteger (Cil.constFold true i2) with
                 Some c1, Some c2 ->
                   My_bigint.equal c1 c2 &&
                   not_separated_offset offs1 offs2
               | None, _ | _, None -> true)
        | (Index _|Field _), (Index _|Field _) ->
            (* A bit strange, but we're not immune against some ugly cast.
               Let's play safe here.
             *)
            true
    in
    let not_separated (base1,offs1)(base2,offs2) =
      match (base1,offs1), (base2,offs2) with
          (Mem _,_),(Mem _,_) -> true
        | (Var v,_),(Mem _,_) | (Mem _,_), (Var v,_)->
            v.vaddrof (* if the address of v is not taken,
                         it cannot be aliased*)
        | (Var v1,offs1),(Var v2,offs2) ->
            v1.vid = v2.vid && not_separated_offset offs1 offs2
    in
    let not_separated l1 l2 =
      Extlib.product_fold (fun f e1 e2 -> f || not_separated e1 e2)
        false l1 l2
    in
    let check_unspec = object
      inherit Cil.nopCilVisitor
      method vstmt s =
        (match s.skind with
             UnspecifiedSequence [] | UnspecifiedSequence [ _ ] -> ()
           | UnspecifiedSequence seq ->
               let my_stmt_print =
                 object(self)
                   inherit Cil.defaultCilPrinterClass as super
                   method pStmt fmt = function
                     | {skind = UnspecifiedSequence seq} ->
                         Pretty_utils.pp_list ~sep:Pretty_utils.nl_sep
                           (fun fmt (s,m,w,r,_) ->
                              Format.fprintf fmt
                                "/*@ %t%a@ <-@ %a@ */@\n%a"
                                (fun fmt -> if (Kernel.debug_atleast 2) then
                                   begin
                                     Pretty_utils.pp_list
                                       ~pre:(Pretty_utils.open_box ^^ "(")
                                       ~suf:(")" ^^ Pretty_utils.close_box)
                                       ~sep: Pretty_utils.space_sep
                                       self#pLval fmt m
                                   end)
                                (Pretty_utils.pp_list
                                   ~sep: Pretty_utils.space_sep self#pLval) w
                                (Pretty_utils.pp_list
                                   ~sep: Pretty_utils.space_sep self#pLval) r
                                self#pStmt s) fmt seq
                     | s -> super#pStmt fmt s
                 end
               in
               let remove_mod m l =
                 List.filter
                   (fun x -> not (List.exists (Lval.equal x) m))
                   l
               in
               let not_separated_modified l1 l2 =
                 List.fold_left
                   (fun flag (m,r) ->
                      flag || not_separated (remove_mod m l2) r) false l1
               in
               let warn,_,_ =
                 List.fold_left
                   (fun ((warn,writes,reads) as res) (_,m,w,r,_) ->
                      if warn then res else begin
                        let new_writes = w @ writes in
                        let new_reads = (m,r)::reads in
                        let new_warn =
                          warn || not_separated writes w ||
                            not_separated (remove_mod m writes) r ||
                            not_separated_modified reads w
                        in
                        new_warn,new_writes,new_reads
                      end)
                   (false, [], []) seq
                 in if warn then
                 Kernel.warning ~current:true ~once:true
                   "Unspecified sequence with side effect:@\n%a@\n"
                   (Cil.printStmt my_stmt_print) s
           | _ -> ());
        DoChildren
    end
    in
    Cil.visitCilFileSameGlobals check_unspec merged_file
  end;
  merged_file

let synchronize_source_annot has_new_stmt kf =
  match kf.fundec with
  | Definition (fd,_) ->
    let (visitor:cilVisitor) = object
      inherit nopCilVisitor as super
      val block_with_user_annots = ref None
      val user_annots_for_next_stmt = ref []
      method vstmt st =
        let stmt, father = match super#current_kinstr with
          | Kstmt stmt ->
            super#pop_stmt stmt;
            let father = super#current_stmt in
            super#push_stmt stmt;
            stmt, father
          | Kglobal -> assert false
        in
        let is_in_same_block () = match !block_with_user_annots,father with
          | None, None -> true
          | Some block, Some stmt_father when block == stmt_father -> true
          | _, _ -> false
        in
        let synchronize_user_annot a = Annotations.add kf st [] (User a) in
        let synchronize_previous_user_annots () =
          if !user_annots_for_next_stmt <> [] then begin
            if is_in_same_block ()
            then begin
              let my_annots = !user_annots_for_next_stmt in
              let post_action st =
                let treat_annot (has_contract,st as acc) annot =
                  if Logic_utils.is_contract annot then begin
                    if has_contract then begin
                      let new_stmt = 
                        Cil.mkStmt ~valid_sid:true (Block (Cil.mkBlock [st]))
                      in
                      has_new_stmt := true;
                      Annotations.add kf new_stmt [] (User annot);
                      (true,new_stmt)
                    end else begin
                      Annotations.add kf st [] (User annot);
                      (true,st)
                    end
                  end else begin
                    Annotations.add kf st [] (User annot);
                    acc
                  end
                in
                let (_,st) = List.fold_left treat_annot (false,st) my_annots in
                st
              in
              block_with_user_annots:=None;
              user_annots_for_next_stmt:=[];
              ChangeDoChildrenPost(st,post_action)
            end
            else begin
              Kernel.warning ~current:true ~once:true
                "Ignoring previous annotation relative \
                 to next statement effects" ;
              block_with_user_annots := None ;
              user_annots_for_next_stmt := [];
              DoChildren
            end 
          end else begin
            block_with_user_annots := None ;
            user_annots_for_next_stmt := [];
            DoChildren;
          end            
        in
        let add_user_annot_for_next_stmt annot =
          if !user_annots_for_next_stmt = [] then begin
            block_with_user_annots := father;
            user_annots_for_next_stmt := [annot]
          end else if is_in_same_block () then
              user_annots_for_next_stmt := annot::!user_annots_for_next_stmt
            else begin
              Kernel.warning ~current:true ~once:true
                "Ignoring previous annotation relative to next statement \
effects";
              block_with_user_annots := father;
              user_annots_for_next_stmt := [annot] ;
            end
        in
        assert (stmt == st) ;
        assert (!block_with_user_annots = None
               || !user_annots_for_next_stmt <> []);
        match st.skind with
        | Instr (Code_annot (annot,_)) ->
          (* Code annotation isn't considered as a real stmt.
             So, previous annotations should be relative to the next stmt.
             Only this [annot] may be synchronised to that stmt *)
          (if match annot.annot_content with
          | AStmtSpec _
          | APragma (Slice_pragma SPstmt | Impact_pragma IPstmt) ->
            (* Annotation relative to the effect of next statement *)
            true
          | APragma _ | AAssert _ | AAssigns _
          | AInvariant _ | AVariant _ (* | ALoopBehavior _ *) ->
            (* Annotation relative to the current control point *)
            false
            then (* To synchronize on the next statement *)
              add_user_annot_for_next_stmt annot
            else (* Synchronize this annotation on that statement *)
              synchronize_user_annot annot);
          super#vstmt st
        | Loop (annot, _, _, _, _) ->
          (* Synchronize previous annotations on that statement *)
          let res = synchronize_previous_user_annots () in
          (* Synchronize loop annotations on that statement *)
          List.iter synchronize_user_annot
            (List.sort (fun x y -> x.annot_id - y.annot_id) annot);
          res
        | _ ->
          (* Synchronize previous annotations on that statement *)
          synchronize_previous_user_annots () ;
    end
    in
    ignore (visitCilFunction visitor fd)
  | Declaration _ -> ()

let register_global = function
  | GFun (fundec, loc) ->
      (* ensure there is only one return *)
      Oneret.oneret fundec;
      (* Build the Control Flow Graph for all
         functions *)
      if Kernel.SimplifyCfg.get () then begin
        Cfg.prepareCFG ~keepSwitch:(Kernel.KeepSwitch.get ()) fundec;
        Cfg.clearCFGinfo fundec;
        Cfg.cfgFun fundec;
      end;
      Globals.Functions.add (Definition(fundec,loc))
  | GVarDecl (spec, ({vtype=typ } as f),loc) when isFunctionType typ ->
      (* global prototypes *)
      let args =
        try Some (Cil.getFormalsDecl f) with Not_found -> None
      in
      (* Use a copy of the spec, as the original one will be erased by
         AST cleanup.
       *)
      let spec = { spec with spec_variant = spec.spec_variant } in
      Globals.Functions.add (Declaration(spec,f,args,loc))
  | GVarDecl (_spec(*TODO*), ({vstorage=Extern} as vi),_) ->
      (* global variables declaration with no definitions *)
      Globals.Vars.add_decl vi
  | GVar (varinfo,initinfo,_) ->
      (* global variables definitions *)
      Globals.Vars.add varinfo initinfo;
  | GAnnot (annot, _loc) ->
      Globals.Annotations.add_user annot
  | _ ->
      ()

let computeCFG ~clear_id file =
  Cfg.clearFileCFG ~clear_id file;
  Cfg.computeFileCFG file

let cleanup file =
  let visitor = object(self)
    inherit Visitor.generic_frama_c_visitor
      (Project.current()) (Cil.inplace_visit())

    val mutable keep_stmt = Stmt.Set.empty

    val mutable changed = false

    method private remove_lexical_annotations stmt =
      match stmt.skind with
        | Instr(Code_annot(_,loc)) ->
            stmt.skind <- Instr(Skip(loc))
        | Loop (_::_, b1,l1,s1,s2) ->
            stmt.skind <- Loop ([], b1, l1, s1, s2)
        | _ -> ()

    method vstmt_aux st =
      self#remove_lexical_annotations st;
      let loc = Stmt.loc st in
      if Annotations.get_all st <> [] || st.labels <> [] then begin
        keep_stmt <- Stmt.Set.add st keep_stmt;
      end;
      match st.skind with
          Block b ->
            (* queue is flushed afterwards*)
            let b' = Cil.visitCilBlock (self:>cilVisitor) b in
            (match b'.bstmts with
                 [] ->
                   changed <- true;
                   st.skind <- (Instr (Skip loc));
                   SkipChildren
               | _ -> if b != b' then st.skind <- Block b'; SkipChildren)
        | _ -> DoChildren

    method vblock b =
      let optim b =
        b.bstmts <-
          List.filter
          (fun x ->
             not (Cil.is_skip x.skind) || Stmt.Set.mem x keep_stmt ||
               ( changed <- true; false) (* don't try this at home, kids...*)
          )
          b.bstmts;
        (* Now that annotations are in the table, we do not need to
           retain the block anymore.
         *)
        b.battrs <- List.filter
          (function
               (Attr(l,[])) when l = Cabs2cil.frama_c_keep_block -> false
             | _ -> true)
          b.battrs;
        b
      in
      (* uncomment if you don't want to consider scope of locals (see below) *)
      (* b.blocals <- [];*)
      ChangeDoChildrenPost(b,optim)

    method vglob_aux = function
    | GFun (f,_) ->
      (* No need to call [Kernel_function.set_spec] yet: will be done later *)
      f.sspec <- Cil.empty_funspec ();
      (* uncomment if you dont want to treat scope of locals (see above)*)
      (* f.sbody.blocals <- f.slocals; *)
      DoChildren
    | GVarDecl(s,_,_) ->
      (* No need to call [Kernel_function.set_spec] yet: will be done later *)
      Logic_utils.clear_funspec s;
      DoChildren
    | _ -> DoChildren

    method vfile f =
      ChangeDoChildrenPost
        (f,fun f -> if changed then begin
           Cfg.clearFileCFG ~clear_id:false f;
           Cfg.computeFileCFG f; f end
         else f)
  end
  in visitFramacFileSameGlobals visitor file

let print_renaming: Cil.cilVisitor = object
  inherit Cil.nopCilVisitor
  method vvdec v =
    if v.vname <> v.vorig_name then begin
      Kernel.result ~current:true
        "Variable %s has been renamed to %s" v.vorig_name v.vname
    end;
    DoChildren
end

let prepare_cil_file file =
  Kernel.feedback ~level:2 "preparing the AST";
  computeCFG ~clear_id:true file;
  if Kernel.Files.Check.get() then begin
   Cil.visitCilFileSameGlobals
     (new check_file_aux false "initial AST" :> Cil.cilVisitor) file;
  end;
  if Kernel.Files.Orig_name.get () then begin
    Cil.visitCilFileSameGlobals print_renaming file
  end;
  (* Compute the list of functions and their CFG *)
  (try
     List.iter register_global file.globals
   with Globals.Vars.AlreadyExists(vi,_) ->
     Kernel.fatal
       "Trying to add the same varinfo twice: %a (vid:%d)" Cil.d_var vi vi.vid
  );
  Rmtmps.removeUnusedTemps ~isRoot:keep_entry_point file;
  (* NB: register_global also calls oneret, which might introduce new
     statements and new annotations tied to them. Since sid are set by cfg,
     we must compute it again before annotation synchronisation *)
  Cfg.clearFileCFG ~clear_id:false file;
  Cfg.computeFileCFG file;
  let recompute = ref false in
  Globals.Functions.iter (synchronize_source_annot recompute);
  (* We might also introduce new blocks for synchronization. *)
  if !recompute then begin
    Cfg.clearFileCFG ~clear_id:false file;
    Cfg.computeFileCFG file;
  end;
  cleanup file;
  (* Check that normalization is correct. *)
  if Kernel.Files.Check.get() then begin
   Cil.visitCilFileSameGlobals
     (new check_file "AST after normalization" :> Cil.cilVisitor) file;
  end;
  (* Unroll loops in file *)
  Unroll_loops.compute (Kernel.UnrollingLevel.get ()) file;
  Cfg.clearFileCFG ~clear_id:false file;
  Cfg.computeFileCFG file;
  (* Annotate functions from declspec. *)
  Translate_lightweight.interprate file;
  (* Check that we start with a correct file. *)
  if Kernel.Files.Check.get() then begin
   Cil.visitCilFileSameGlobals
     (new check_file
        "Ast as set in Frama-C's original state" :> Cil.cilVisitor) file;
  end;
  Ast.set_file file

let set_formals_decl file =
  let treat_global = function
    | GVarDecl(_,vi,_) when isFunctionType vi.vtype && not vi.vdefined ->
        Cil.setFormalsDecl vi vi.vtype
    | _ -> ()
  in List.iter treat_global file.globals

let fill_built_ins () =
  if Cil.selfMachine_is_computed () then begin
    Kernel.debug "Machine is computed, just fill the built-ins";
    Cil.init_builtins ();
  end else begin
    Kernel.debug "Machine is not computed, initialize everything";
    Cil.initCIL (Logic_builtin.init());
  end;
  (* Fill logic tables with builtins *)
  Logic_env.Builtins.apply ();
  Logic_env.prepare_tables ()

let init_project_from_cil_file prj file =
  let selection =
    State_selection.Static.diff
      (State_selection.Static.diff
         State_selection.full
         (State_selection.Static.with_dependencies Cil.Builtin_functions.self))
      (State_selection.Static.with_dependencies Ast.self)
  in
  Project.copy ~selection prj;
  Project.on prj
    (fun file ->
       fill_built_ins ();
       prepare_cil_file file;
       set_formals_decl file)
    file

(* items in the machdeps list are of the form
   (machine, (is_public, action_when_selected))
   where
   - machine is the machine name
   - is_public is true if the machine is public (shown in -machdeps help)
   - action_when_selected is the action to perform when the corresponding
   machine is set as current (i.e. defining the right architecture via
   Machdep.DEFINE) *)

let default_machdeps =
  [ "x86_16",
    (true, fun () -> let module M = Machdep.DEFINE(Machdep_x86_16) in ());
    "x86_32",
    (true, fun () -> let module M = Machdep.DEFINE(Machdep_x86_32) in ());
    "x86_64",
    (true, fun () -> let module M = Machdep.DEFINE(Machdep_x86_64) in ());
    "ppc_32",
    (true, fun () -> let module M = Machdep.DEFINE(Machdep_ppc_32) in ());
  ]

let machdeps = Datatype.String.Hashtbl.create 7
let () =
  List.iter
    (fun (s, c) -> Datatype.String.Hashtbl.add machdeps s c)
    default_machdeps

let new_machdep s p f =
  if Datatype.String.Hashtbl.mem machdeps s then
    invalid_arg (Format.sprintf "machdep `%s' already exists" s);
  Datatype.String.Hashtbl.add machdeps s (p, f)

let pretty_machdeps fmt =
  Datatype.String.Hashtbl.iter
    (fun x (public, _) -> if public then Format.fprintf fmt "@ %s" x)
    machdeps

let set_machdep () =
  let m = Kernel.Machdep.get () in
  try snd (Datatype.String.Hashtbl.find machdeps m) ()
  with Not_found ->
    if m = "" then ()
    else if m = "help" then
      Kernel.feedback "supported machines are%t." pretty_machdeps
    else
      Kernel.error "unsupported machine %s. Try one of%t." m
        pretty_machdeps

let () = Cmdline.run_after_configuring_stage set_machdep 

let init_cil () =
  Cil.initCIL (Logic_builtin.init());
  Logic_env.Builtins.apply ();
  Logic_env.prepare_tables ()

(* Fill logic tables with builtins *)
let prepare_from_c_files () =
  init_cil ();
  let files = Files.get () in (* Allow pre-registration of prolog files *)
  let cil = files_to_cil files in
  prepare_cil_file cil

let prepare_from_visitor prj visitor =
  let visitor = (visitor prj :> Cil.cilVisitor) in
  (* queue of visitor is filled below *)
  let set_annotation annot = visitCilAnnotation visitor annot in
  Project.on
    ~selection:
    (State_selection.Static.with_dependencies Globals.Annotations.self)
    prj
    (List.iter
       (fun (a,f) ->
          if f then
            Globals.Annotations.add_generated (set_annotation a)
          else
            Globals.Annotations.add_user (set_annotation a)))
    (Globals.Annotations.get_all ());
  let file = Ast.get () in
  let file' = Cil.visitCilFileCopy visitor file in
  Project.on prj Cil.initCIL (fun () -> ());
  computeCFG ~clear_id:false file';
  Project.on
    ~selection:(State_selection.singleton Ast.self) prj Ast.set_file file';
  if Kernel.Files.Check.get() then
    Project.on
      prj
      (* eta-expansion required because of operations on the current project in
         the class construtor *)
      (fun f ->
        Cil.visitCilFile
          (new check_file ("AST of " ^ prj.Project.name) :> Cil.cilVisitor) f)
      file'

let create_project_from_visitor prj_name visitor =
  let selection =
    State_selection.Static.union
      (State_selection.Static.with_dependencies Cil.selfMachine)
      (State_selection.Static.with_dependencies Kernel.Files.self)
  in
  let selection =
    State_selection.Static.diff
      (State_selection.Static.diff State_selection.full selection)
      (State_selection.Static.with_dependencies Annotations.self)
  in
  let prj = Project.create_by_copy ~selection prj_name in
  (* reset projectified parameters to their default values *)
  let temp = Project.create "File.temp" in
  Project.copy ~selection:(Plugin.get_selection ()) ~src:temp prj;
  Project.remove ~project:temp ();
  Project.on prj init_cil ();
  prepare_from_visitor prj visitor;
  prj

let init_from_c_files files =
  (match files with [] -> () | _ :: _ -> Files.register files);
  prepare_from_c_files ()

let init_from_cmdline () =
  let prj1 = Project.current () in
  if Kernel.Files.Copy.get () then begin
    let selection =
      State_selection.Static.diff
        (State_selection.Static.diff
           State_selection.full
           (State_selection.of_list
              [ Cil.Builtin_functions.self;
                Logic_env.Logic_info.self;
                Logic_env.Logic_builtin_used.self;
                Logic_env.Logic_type_info.self;
                Logic_env.Logic_ctor_info.self;
                Globals.Annotations.self;
                Globals.Vars.self;
                Globals.Functions.self;
                Ast.self;
              ]))
        (State_selection.Static.with_dependencies Annotations.self)
    in
    let prj2 = Project.create_by_copy ~selection "debug_copy_prj" in
    Project.set_current prj2;
  end;
  let files = Kernel.Files.get () in
  if files = [] && not !Config.is_gui then Kernel.warning "no input file.";
  let files = List.map (fun s -> from_filename s) files in
  try
    init_from_c_files files;
    if Kernel.Files.Check.get () then begin
      Cil.visitCilFile
        (new check_file "Copy of original AST" :> Cil.cilVisitor) (Ast.get())
    end;
    if Kernel.Files.Copy.get () then begin
      Project.on prj1 fill_built_ins ();
      prepare_from_visitor prj1 (fun prj -> new Visitor.frama_c_copy prj);
      Project.set_current prj1;
    end;
  with Ast.Bad_Initialization s ->
    Kernel.fatal "@[<v 0>Cannot initialize from C files@ \
                        Kernel raised Bad_Initialization %s@]" s

let init_from_cmdline =
  Journal.register
    "File.init_from_cmdline"
    (Datatype.func Datatype.unit Datatype.unit)
    init_from_cmdline

let init_from_c_files =
  Journal.register
    "File.init_from_c_files"
    (Datatype.func (Datatype.list ty) Datatype.unit)
    init_from_c_files

let prepare_from_c_files =
  Journal.register
    "File.prepare_from_c_files"
    (Datatype.func Datatype.unit Datatype.unit)
    prepare_from_c_files

let () = Ast.set_default_initialization
  (fun () ->
     if Files.is_computed () then prepare_from_c_files ()
     else init_from_cmdline ())

let pp_file_to fmt_opt =
  let pp_ast = Cil.d_file (new Printer.print ()) in
  let ast = Ast.get () in
  (match fmt_opt with
    | None -> Kernel.CodeOutput.output (fun fmt -> pp_ast fmt ast)
    | Some fmt -> pp_ast fmt ast)

let unjournalized_pretty prj (fmt_opt:Format.formatter option) () =
  Project.on prj pp_file_to fmt_opt

let journalized_pretty_ast =
  Journal.register "File.pretty_ast"
    (Datatype.func3
       ~label1:("prj",Some Project.current) Project.ty
       ~label2:("fmt",Some (fun () -> None))
       (Datatype.option Datatype.formatter)
       Datatype.unit Datatype.unit)
    unjournalized_pretty

let pretty_ast ?(prj=Project.current ()) ?fmt () =
  journalized_pretty_ast prj fmt ()

let create_rebuilt_project_from_visitor ?(preprocess=false) prj_name visitor =
  let prj = create_project_from_visitor prj_name visitor in
  try
    let f =
      let name = "frama_c_project_" ^ prj_name ^ "_" in
      let ext = if preprocess then ".c" else ".i" in
      if Kernel.Debug.get () > 0 then Filename.temp_file name ext
      else Extlib.temp_file_cleanup_at_exit name ext
    in
    let cout = open_out f in
    let fmt = Format.formatter_of_out_channel cout in
    unjournalized_pretty prj (Some fmt) ();
    let redo () =
(*      Kernel.feedback "redoing initialization on file %s" f;*)
      Files.reset ();
      init_from_c_files [ if preprocess then from_filename f else NoCPP f ]
    in
    Project.on prj redo ();
    prj
  with Extlib.Temp_file_error s | Sys_error s ->
    Kernel.abort "cannot create temporary file: %s" s

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

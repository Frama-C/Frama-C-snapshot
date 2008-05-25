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

(* $Id: file.ml,v 1.81 2008/05/23 14:05:06 uid528 Exp $ *)

open Cil_types
open Cil
open Cilutil
open Db_types
open Extlib

type t =
  | NeedCPP of
      string (* filename of the [.c] to preprocess *)
      * string (* Preprocessor command.
                  [filename.c -o tempfilname.i] will be appended at the
		    end.*)
  | NoCPP of string (** filename of a preprocessed [.c] *)
  | CPLUSPLUS of string (** c++ file. Can only be analysed if C++ extension
                            is loaded. *)

let check_suffixes f =
  List.fold_left (fun flag suf -> flag || Filename.check_suffix f suf) false

let cxx_suffixes = ref []
let parse_cplusplus = Extlib.mk_fun "File.parse_cplusplus"

let name = function NeedCPP (s,_) | NoCPP s | CPLUSPLUS s -> s

(* ************************************************************************* *)
(** {2 Preprocessor command} *)
(* ************************************************************************* *)

(* the preprocessor command is
 * If the program has an explicit argument -cpp-command "XX -Y" (quotes are
 * required by the shell) then XX -Y
 * else if the CPP environment variable is set, use it
 * else the built-in "gcc -C -E -I."
 *)
let get_preprocessor_command () =
  let cmdline = Cmdline.CppCommand.get() in
  if cmdline <> "" then cmdline
  else
    try Sys.getenv "CPP"
    with Not_found -> "gcc -C -E -I."

let from_filename ?(cpp=get_preprocessor_command ()) f =
  if Filename.check_suffix f ".i" then
    NoCPP f
  else if check_suffixes f !cxx_suffixes then
    CPLUSPLUS f
  else
    NeedCPP (f, cpp)

(* ************************************************************************* *)
(** {2 Internal states} *)
(* ************************************************************************* *)

module Files : sig
  val get: unit -> t list
  val register: t list -> unit
  val pre_register: t -> unit
end = struct

  module Already =
    Computation.Ref
      (struct include Datatype.Bool let default = false end)
      (struct
	 let dependencies = []
	 let name = Project.Computation.Name.make "already files"
       end)

  module S =
    Computation.Ref
      (struct
	 include
	   Datatype.List
	   (Project.Datatype.Persistent(struct type tt = t type t = tt end))
	   (* actually strings are immutable here *)
	 let default = []
       end)
      (struct
	 let dependencies = []
	 let name = Project.Computation.Name.make "files"
       end)

  (* Allow to register files in advance, e.g. prolog files for plugins *)
  let pre_register file =
    let prev_files = S.get () in
    S.set (prev_files @ [file])

  let register files =
    if Already.get () then
      raise (Cil_state.Bad_Initialisation "Too many initializations");
    Already.set true;
    let prev_files = S.get () in
    S.set (prev_files @ files)

  let get = S.get

end

let get_all = Files.get
let pre_register = Files.pre_register

(* ************************************************************************* *)
(** {2 Initialisations} *)
(* ************************************************************************* *)

let parse = function
  | NoCPP f ->
      if not (Sys.file_exists  f) then
	Format.eprintf
	  "PPC: preprocessed file %s does not exist.\n%!" f;
      Frontc.parse f ()
  | NeedCPP (f, cmdl) ->
      if not (Sys.file_exists  f) then
	Format.eprintf "PPC: source file %s does not exist.\n%!" f;
      let ppf = Filename.temp_file (Filename.basename f) ".i" in
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
            match
              (String.get cmdl (percent1+1), String.get cmdl (percent2+1))
            with
            | '1', '2' ->
                in_file, out_file  (* "%1" followed by "%2" is used to printf 'ppf' after
			              'f' *)
            | '2', '1' ->
                out_file, in_file
            | _, _ -> raise (Invalid_argument "maybe a bad cpp command")
          in
	  let cmd1 = String.sub cmdl 0 percent1 in
	  (* Format.eprintf "-cpp-command cmd1=|%s|@\n" cmd1; *)
          let cmd2 =
	    String.sub cmdl (percent1+2) (percent2 - (percent1 + 2))
	  in
	  (* Format.eprintf "-cpp-command cmd2=|%s|@\n" cmd2; *)
          let cmd3 =
	    String.sub cmdl (percent2+2) ((String.length cmdl)-(percent2+2))
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
      ignore (Errormsg.log "[preprocessing] running %s %s\n" cmdl f);
      if Sys.command (cmd 
                        ((Cmdline.CppExtraArgs.get()) ^ " " ^ 
                           (if Cmdline.ReadAnnot.get() && 
                              Cmdline.PreprocessAnnot.get() then "-dD" else ""))
                        f ppf) <> 0 then 
        begin
          Format.eprintf "Failed to run: %s\n\t
           You may set the CPP environment variable to select the proper preprocessor command ...\n\t\
           or use the -cpp-command program argument.@\n%!" (cmd "" f ppf);
          begin try (Sys.remove ppf) with Sys_error _ -> () end
        end;
      let ppf =
        if Cmdline.ReadAnnot.get() && Cmdline.PreprocessAnnot.get()
        then
          let ppf' = Logic_preprocess.file (cmd " -P ") ppf in
          Sys.remove ppf; ppf'
        else ppf
      in
      let cil = Frontc.parse ppf () in Sys.remove ppf; cil
  | CPLUSPLUS f ->
      if not (Sys.file_exists  f) then
	Format.eprintf
	  "C++ file %s does not exist.\n%!" f;
      !parse_cplusplus f

let files_to_cil files =
  (*
    Warning : parsing and merging must occur in the very same order.
    Orelse scope tables build by module Scope will no be consistant
    with the renaming done by Mergecil.merge
  *)
  Format.printf "Parsing@\n";
  let files =
    List.fold_left
      (fun acc f ->
         try
	   parse f :: acc
         with
           Frontc.ParseError _ | Errormsg.Error
               when Cmdline.Debug.get () <= 1
                 (* obtain a backtrace when debugging level is high enough *)
             ->
               Format.eprintf "Skipping file %S that has errors.@." (name f);
               acc)
      []
      files
  in
  let files = List.rev files in
  if Cmdline.Debug.get() > 5 then
    List.iter
      (fun f -> (* NB: don't use frama-C printer here, as the
                   annotations tables are not filled yet. *)
         List.iter(Cil.d_global Format.std_formatter) f.globals)
      files;
  Format.printf "Cleaning unused parts@\n";
  let () = List.iter Rmtmps.removeUnusedTemps files in
  (* Clean up useless parts
     BM: modified to keep static functions. In fact we should
     mark the roots with a __constructor__ attribute *)
  if Cmdline.Debug.get() > 5 then
    List.iter
      (fun f -> (* NB: don't use frama-C printer here, as the
                   annotations tables are not filled yet. *)
         List.iter (Cil.d_global Format.std_formatter) f.globals)
      files;
  Format.printf "Symbolic link@\n";
  let merged_file = Mergecil.merge files "whole_program" in
  (* dumpFile defaultCilPrinter stdout p; *)
  if !Errormsg.hadErrors then
    begin
      Format.eprintf "Your code cannot be parsed. Aborting analysis.@.";
    end;
  if Cmdline.Debug.get() > 5 then
    List.iter
      (fun f -> (* NB: don't use frama-C printer here, as the
                   annotations tables are not filled yet. *)
         List.iter(Cil.d_global Format.std_formatter) f.globals)
      files;
  merged_file

let synchronize_source_annot kf =
  match kf.fundec with
    | Definition (fd,_) ->
        let (visitor:cilVisitor) = object
          inherit nopCilVisitor as super
          val block_with_user_annots = ref None
          val user_annots_for_next_stmt = ref []
          method vstmt st =
            let stmt, father = match super#current_stmt with
              | Some stmt ->
                  super#pop_stmt stmt ;
                  let father = super#current_stmt
                  in super#push_stmt stmt ; stmt, father
              | None -> assert(false) in
            let is_in_same_block () =
              match !block_with_user_annots,father with
                | None, None -> true
                | Some block, Some stmt_father when block == stmt_father -> true
                | _, _ -> false in
            let synchronize_user_annot annot =
              (*Format.printf "Synchronize to stmt:%d@." st.sid; *)
	      Annotations.add st (Before (User annot))
	    in
            let synchronize_previous_user_annots () =
              if !user_annots_for_next_stmt <> []
              then begin
                (* Format.printf "Synchronize previous annotations to stmt:%d@." st.sid; *)
                if is_in_same_block ()
                then
                  (* previous annotations are relative to the current stmt *)
                  List.iter synchronize_user_annot !user_annots_for_next_stmt
                else
                  ((*Format.printf "Warning: previous annotations are ignored at stmt:%d@." st.sid;*)
                    ignore
		      (CilE.warn_once
		         "ignoring previous annotation relative to next statement effects"));
                block_with_user_annots := None ;
                user_annots_for_next_stmt := []
              end
            in
            let add_user_annot_for_next_stmt annot =
              (*Format.printf "This annotation is not for this stmt:%d@." st.sid;*)
              if !user_annots_for_next_stmt = []
              then
                (block_with_user_annots := father;
                 user_annots_for_next_stmt := [annot])
              else if is_in_same_block ()
              then
                user_annots_for_next_stmt := annot::!user_annots_for_next_stmt
              else
                ((*Format.printf "Warning: previous annotations are ignored at stmt:%d@." st.sid;*)
                  ignore
		    (CilE.warn_once
		       "ignoring previous annotation relative to next statement effects");
                  block_with_user_annots := father;
                  user_annots_for_next_stmt := [annot]) in
              assert (stmt == st) ;
              assert (!block_with_user_annots = None
                  || !user_annots_for_next_stmt <> []);

              (*Format.printf "Visit stmt:%d@." st.sid;
              (match father with Some stmt -> Format.printf "stmt father:%d@." stmt.sid | None -> ());*)
            match st.skind with
              | Instr (Code_annot (annot,_)) ->
                    (* Code annotation isn't considered as a real stmt.
                       So, previous annotations should be relative to the next stmt.
                       Only this [annot] may be synchronised to that stmt *)
                    (*Format.printf "Code annotation in stmt:%d@." st.sid;*)
                    (if match annot.annot_content with
                       | AStmtSpec _
                       | APragma (Slice_pragma SPstmt | Impact_pragma IPstmt) ->
                           (* Annotation relative to the effect of next statement *)
                           true
                       | APragma _ | AAssert _ | AAssume _
                       | AInvariant _ | AVariant _ | AAssigns _ ->
                           (* Annotation relative to the current control point *)
                           false
                     then (* To synchronize on the next statement *)
                       add_user_annot_for_next_stmt annot
                     else (* Synchronize this annotation on that statement *)
                       synchronize_user_annot annot);
                    super#vstmt st
                | Loop (annot, _, _, _, _) -> (* Synchronize previous annotations on that statement *)
                    (*Format.printf "Loop annotation in stmt:%d@." st.sid;*)
                    synchronize_previous_user_annots () ;
                    (* Synchronize loop annotations on that statement *)
                    List.iter synchronize_user_annot annot;
                    super#vstmt st
                | _ -> (* Synchronize previous annotations on that statement *)
                    synchronize_previous_user_annots () ;
                    super#vstmt st
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
      if Cmdline.SimplifyCfg.get () then begin
	Cil.prepareCFG ~keepSwitch:(Cmdline.KeepSwitch.get ()) fundec;
        Cfg.clearCFGinfo fundec;
        Cfg.cfgFun fundec;
      end;
      Globals.Functions.add (Db_types.Definition(fundec,loc))
  | GVarDecl (spec, ({vtype=TFun (_,_,_,_) } as f),loc) ->
      (* global prototypes *)
      let args =
        try Some (Cil.getFormalsDecl f.vid) with Not_found -> None
      in
      Globals.Functions.add (Db_types.Declaration(spec,f,args,loc))
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

let computeCFG file =
  Cfg.clearFileCFG file;
  Cfg.computeFileCFG file

let cleanup file =
  let visitor = object(self)
    inherit Visitor.generic_frama_c_visitor
      (Project.current()) (Cil.inplace_visit())

    val mutable keep_stmt = Cilutil.StmtSet.empty

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
      let loc = Cil.get_stmtLoc st.skind in
      if Annotations.get st <> [] || st.labels <> [] then
        keep_stmt <- Cilutil.StmtSet.add st keep_stmt;
      match st.skind with
          Block b ->
            let b' = Cil.visitCilBlock (self :> Cil.cilVisitor) b in
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
             not (Cil.is_skip x.skind) || Cilutil.StmtSet.mem x keep_stmt ||
               ( changed <- true; false) (* don't try this at home, kids...*)
          )
          b.bstmts;
        (* Now that annotations are in the table, we do not need to
           retain the block anymore.
         *)
        b.battrs <- List.filter
          (function
               (Attr("FRAMA_C_KEEP_BLOCK",[])) -> false
             | _ -> true)
          b.battrs;
        b
      in ChangeDoChildrenPost(b,optim)

    method vglob_aux = function
        GFun (f,_) -> f.sspec <- Cil.empty_funspec(); DoChildren
      | GVarDecl(s,_,_) ->
          s.spec_requires <- [];
          s.spec_behavior <- [];
          s.spec_variant <- None;
          s.spec_terminates <- None;
          s.spec_complete_behaviors <- [];
          s.spec_disjoint_behaviors <- [];
          DoChildren
      | _ -> DoChildren

    method vfile f =
      ChangeDoChildrenPost
        (f,fun f -> if changed then begin
           Cfg.clearFileCFG ~clear_id:false f;
           Cfg.computeFileCFG f; f end
         else f)
  end
  in Cil.visitCilFileSameGlobals (visitor :> Cil.cilVisitor) file

let check_visitor : Cil.cilVisitor =
object
  inherit Cil.nopCilVisitor
  val case_stmt = Cilutil.StmtHashtbl.create 7

  method vstmt s =
    Cilutil.StmtHashtbl.remove case_stmt s;
    match s.skind with
        Switch(_,_,cases,loc) ->
          List.iter
            (fun x -> Cilutil.StmtHashtbl.add case_stmt x loc) cases;
          DoChildren
      | _ -> DoChildren

  method vfunc f =
    assert (Cilutil.StmtHashtbl.length case_stmt = 0);
    let check f =
      if Cilutil.StmtHashtbl.length case_stmt <> 0 then
        begin
          StmtHashtbl.iter
            (fun x loc ->
               Errormsg.s
                 (Cil.errorLoc loc
                    "[AST Integrity Check] In function %a, statement %a \
                      does not appear in body of switch while porting a \
                      case or default label."
               !Ast_printer.d_stmt x))
            case_stmt
        end;
      f
    in
    ChangeDoChildrenPost(f,check)

end

let prepare_cil_file file =
  Format.printf "Starting semantical analysis@\n";
  computeCFG file;
  if Cmdline.Files.Check.get() then begin
   Cil.visitCilFileSameGlobals check_visitor file;
  end;
  (* Compute the list of functions and their CFG *)
  List.iter register_global file.globals;
  Rmtmps.removeUnusedTemps file;
  Globals.Functions.iter synchronize_source_annot;
  cleanup file;
  (* Unroll loops in file *)
  Unroll_loops.compute (Cmdline.UnrollingLevel.get ()) file;
  Cfg.clearFileCFG ~clear_id:false file;
  Cfg.computeFileCFG file;
  Cil_state.set_file file;
  Cg.dump ()

let init_project_from_cil_file prj file =
  Project.copy
    ~except:
    (Project.Selection.singleton Cil_state.self Kind.Select_Dependencies)
    prj;
  Project.on prj
    (fun file ->
       (* Fill logic tables with builtins *)
       Logic_env.Builtins.apply ();
       prepare_cil_file file)
    file

(* performs various consistency checks over a cil file.
   Code may vary depending on current development of the kernel and/or
   identified bugs.
 *)
class check_file: Visitor.frama_c_visitor  =
object(self)
  inherit Visitor.generic_frama_c_visitor (Project.current())
    (Cil.inplace_visit())
  val known_loop_annot_id = Hashtbl.create 7
  val known_code_annot_id = Hashtbl.create 7
  val known_fields = FieldinfoHashtbl.create 7
  val known_stmts = StmtHashtbl.create 7
  val mutable labelled_stmt = []

  method vfunc f =
    labelled_stmt <- [];
    StmtHashtbl.clear known_stmts;
    let check f =
      List.iter
        (fun stmt ->
           try if StmtHashtbl.find known_stmts stmt != stmt then
             Errormsg.s
               (Cil.error
                  "[AST Integrity Check] Label %a in function %s \
                   is not linked to the correct statement"
                  Cil.d_stmt ({stmt with skind =
                                     Instr (Skip (Cil.get_stmtLoc stmt.skind))
                                 })
                  f.svar.vname)
           with Not_found ->
             Errormsg.s
               (Cil.error
                  "[AST Integrity Check] Label %a in function %s \
                   does not refer to an existing statement"
                  Cil.d_stmt ({stmt with skind =
                                  Instr (Skip (Cil.get_stmtLoc stmt.skind))
                              })
                  f.svar.vname))
        labelled_stmt;
      labelled_stmt <- [];
      StmtHashtbl.clear known_stmts;
      f
    in
    ChangeDoChildrenPost(f,check)

  method vstmt_aux s =
    StmtHashtbl.add known_stmts s s;
    (match s.skind with
         Goto (s,_) -> labelled_stmt <- !s :: labelled_stmt
       | _ -> ());
    DoChildren

  method vcode_annot ca =
    if Hashtbl.mem known_code_annot_id ca.annot_id then
      Errormsg.s (Cil.error  "[AST integrity check] duplicated code annotation")
    else Hashtbl.add known_code_annot_id ca.annot_id (); DoChildren

  method voffs = function
      NoOffset -> SkipChildren
    | Index _ -> DoChildren
    | Field(fi,_) ->
        begin
          try
            if not (fi == FieldinfoHashtbl.find known_fields fi)
            then
              Errormsg.s
                (Cil.error "[AST Integrity Check] field %s of type %s is not \
                            shared between declaration and use"
                   fi.fname fi.fcomp.cname)
          with Not_found ->
            Errormsg.s (Cil.error "[AST Integrity Check] field %s of \
                                   type %s is unbound in the AST"
                          fi.fname fi.fcomp.cname)
        end;
        DoChildren

  method vterm_offset = function
      TNoOffset -> SkipChildren
    | TIndex _ -> DoChildren
    | TField(fi,_) ->
        begin
          try
            if not (fi == FieldinfoHashtbl.find known_fields fi)
            then
              Errormsg.s
                (Cil.error "[AST Integrity Check] field %s of type %s is not \
                            shared between declaration and use"
                   fi.fname fi.fcomp.cname)
          with Not_found ->
            Errormsg.s (Cil.error "[AST Integrity Check] field %s of \
                                   type %s is unbound in the AST"
                          fi.fname fi.fcomp.cname)
        end;
        DoChildren

  method vtsets_offset =
    function
      TSNo_offset -> DoChildren
    | TSIndex _ | TSRange _ -> DoChildren
    | TSField(fi,_) ->
        begin
          try
            if not (fi == FieldinfoHashtbl.find known_fields fi)
            then
              Errormsg.s
                (Cil.error "[AST Integrity Check] field %s of type %s is not \
                            shared between declaration and use"
                   fi.fname fi.fcomp.cname)
          with Not_found ->
            Errormsg.s (Cil.error "[AST Integrity Check] field %s of \
                                   type %s is unbound in the AST"
                          fi.fname fi.fcomp.cname)
        end;
        DoChildren

  method vtsets_elem = function
    | TSAt(_,StmtLabel l) -> labelled_stmt <- !l::labelled_stmt; DoChildren
    | _ -> DoChildren

  method vterm t =
    match t.term_node with
      | TLval _ ->
	  begin match t.term_type with
	    | Ctype ty -> assert (not (isVoidType ty)); DoChildren
	    | _ -> DoChildren
	  end
      | Tat(_,StmtLabel l) -> labelled_stmt <- !l::labelled_stmt; DoChildren
      | _ -> DoChildren

  method vinitoffs = self#voffs

  method vglob_aux = function
      GCompTag(c,_) ->
        List.iter
          (fun x -> FieldinfoHashtbl.add known_fields x x) c.cfields;
        DoChildren
    | _ -> DoChildren

  method vpredicate = function
      Pat(_,StmtLabel l) ->  labelled_stmt <- !l::labelled_stmt; DoChildren
    | _ -> DoChildren

  method vpredicate_info_decl pi =
    if Cmdline.Debug.get() > 0 then
      Printf.eprintf "visiting decl of %s\n" pi.p_name;
    (try
       if Logic_env.find_predicate pi.p_name != pi then
         Errormsg.s
           (Cil.error "[AST Integrity Check] predicate %a information is \
                            not shared between declaration and use"
              !Ast_printer.d_ident pi.p_name)
     with Not_found ->
       Errormsg.s
         (Cil.error "[AST Integrity Check] predicate %a has no information"
            !Ast_printer.d_ident pi.p_name));
    DoChildren

  method vlogic_info_decl li =
    if Cmdline.Debug.get() > 0 then
      Printf.eprintf "visiting decl of %s\n" li.l_name;
    (try if Logic_env.find_logic_function li.l_name !=  li then
       Errormsg.s
         (Cil.error "[AST Integrity Check] logic function %a information is \
                            not shared between declaration and use"
            !Ast_printer.d_ident li.l_name)
     with Not_found ->
       Errormsg.s
         (Cil.error "[AST Integrity Check] logic function %a has no information"
            !Ast_printer.d_ident li.l_name));
    DoChildren

  method vpredicate_info_use pi =
    if Cmdline.Debug.get() > 0 then
      Printf.eprintf "visiting use of %s\n" pi.p_name;
    (try if  Logic_env.find_predicate pi.p_name !=  pi then
       Errormsg.s
         (Cil.error "[AST Integrity Check] predicate %a information is \
                            not shared between declaration and use"
            !Ast_printer.d_ident pi.p_name)
     with Not_found ->
       Errormsg.s
         (Cil.error "[AST Integrity Check] predicate %a has no information"
            !Ast_printer.d_ident pi.p_name));
    DoChildren

  method vlogic_info_use li =
    if Cmdline.Debug.get() > 0 then
      Printf.eprintf "visiting use of %s\n" li.l_name;
    (try if Logic_env.find_logic_function li.l_name !=  li then
       Errormsg.s
         (Cil.error "[AST Integrity Check] logic function %a information is \
                            not shared between declaration and use"
            !Ast_printer.d_ident li.l_name)
     with Not_found ->
       Errormsg.s
         (Cil.error "[AST Integrity Check] logic function %a has no information"
            !Ast_printer.d_ident li.l_name));
    DoChildren

end

let init_project_from_visitor_aux ?(files_copy=false) prj visitor =
  let except =
    let sel =
(*      if files_copy then*) Project.Selection.empty(* else Cmdline.get_selection () *)
    in
    Project.Selection.add Cil_state.self Kind.Select_Dependencies sel
  in
  Project.copy ~except prj;
  if not files_copy then begin
    let temp = Project.create "temp" in
    Project.copy ~only:(Cmdline.get_selection ()) ~src:temp prj;
    Project.remove ~project:temp ()
  end;
  let visitor =  (visitor prj :> Cil.cilVisitor) in
  let set_annotation annot = visitCilAnnotation visitor annot in
  Project.on
    ~only:(Project.Selection.singleton Globals.Annotations.self
             Kind.Do_Not_Select_Dependencies)
    prj
    (List.iter
       (fun (a,f) ->
          if f then
            Globals.Annotations.add_generated (set_annotation a)
          else
            Globals.Annotations.add_user (set_annotation a)
       ))
    (Globals.Annotations.get_all());
  let file = Cil_state.file () in
  let file' = Cil.visitCilFileCopy visitor file in
  Cfg.clearFileCFG ~clear_id:false file';
  Cfg.computeFileCFG file';
  Project.on
    ~only:(Project.Selection.singleton
	     Cil_state.self Kind.Do_Not_Select_Dependencies)
    prj
    Cil_state.set_file
    file';
  if Cmdline.Files.Check.get() then
    Project.on prj
      (Cil.visitCilFile (new check_file :> Cil.cilVisitor)) file'

let init_from_c_files files =
  (* Fill logic tables with builtins *)
  Logic_env.Builtins.apply ();
  Files.register files;
  let files = Files.get () in (* Allow pre-registration of prolog files *)
  let cil = files_to_cil files in
  prepare_cil_file cil

let init_from_cmdline () =
  let files = List.map (fun s -> from_filename s) (Cmdline.Files.get ()) in
  try
    init_from_c_files files;
    if Cmdline.Files.Check.get () then begin
      Cil.visitCilFile(new check_file :> Cil.cilVisitor) (Cil_state.file())
    end;
    if Cmdline.Files.Copy.get () then begin
      let prj = Project.create "debug_copy_prj" in
      init_project_from_visitor_aux ~files_copy:true
	prj (new Visitor.frama_c_copy);
      Project.set_current prj;
    end;
  with Cil_state.Bad_Initialisation s ->
    Format.eprintf "Bad initialisation: %s@." s; assert false

let pretty ?(prj=Project.current ()) fmt =
  Project.on
    prj
    (fun () ->
       Format.fprintf fmt "@[%a@]@\n" (Cil.d_file (new Printer.print ()))
	 (Cil_state.file ()))
    ()

(* hide the optional argument of [init_project_from_visitor_aux] *)
let init_project_from_visitor p = init_project_from_visitor_aux p

(* ************************************************************************* *)
(** {2 Initialisation} *)
(* ************************************************************************* *)

let () =
  (* Registering options *)
  Options.add_cmdline ~name:"files"
    ~debug:["-check", Arg.Unit Cmdline.Files.Check.on,
              "performs consistency checks over cil files";
            "-copy", Arg.Unit Cmdline.Files.Copy.on,
            "always perform a copy of the original AST before analysis begin"
           ]
    [ "-cpp-command",
      Arg.String Cmdline.CppCommand.set,
      "CPP : CPP string is used to build the preprocessing command.\n\t\
       Defaults to $CPP environment variable or else \"gcc -C -E -I.\"\n\t\
       If unset, the command is built as follow:\n\t\
       CPP -o <preprocessed file> <source file> \n\t\
       %1 and %2 can be used into CPP string to mark the position of \
       <source file> and <preprocessed file> respectively.";
      "-cpp-extra-args",
      Arg.String Cmdline.CppExtraArgs.set,
      "additional arguments passed to the preprocessor while preprocessing \
       the C code but not while preprocessing annotations.";
      "-no-annot",
      Arg.Unit (fun () ->
                  Cmdline.ReadAnnot.off ();
                  Clexer.annot_char := '\000';
                  (* Hack to prevent the C lexer interpretation of comments *)
	       ),
      ": do not read annotation.";
      "-pp-annot",
      Arg.Unit Cmdline.PreprocessAnnot.on,
      ": pre-process annotations (if they are read).";
]

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)

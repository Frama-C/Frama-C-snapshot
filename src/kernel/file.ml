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

open Cil_types
open Cil
open Cilutil
open Db_types
open Extlib
open Visitor
open Pretty_utils

type t =
  | NeedCPP of
      string (* filename of the [.c] to preprocess *)
      * string (* Preprocessor command.
                  [filename.c -o tempfilname.i] will be appended at the
		    end.*)
  | NoCPP of string (** filename of a preprocessed [.c] *)
  | External of string * string (* file * name of plug-in that handles it *)

let pp_file p_caller fmt t =
  let pp fmt = match t with
    | NoCPP s -> Format.fprintf fmt "@[File.NoCPP %S@]" s
    | External (f,p) ->
        Format.fprintf fmt "@[File.External (%S,%S)@]" f p
    | NeedCPP (a,b) -> Format.fprintf fmt "@[File.NeedCPP (%S,%S)@]" a b
  in
  Type.par p_caller Type.Call fmt pp

let ty =
  Type.register ~name:"File.t" ~value_name:(Some "File.ty") ~pp:pp_file
    [ NeedCPP("", ""); NoCPP ""; External("", "") ]

let check_suffixes = Hashtbl.create 17
let new_file_type = Hashtbl.add check_suffixes
let get_suffixes () =
  Hashtbl.fold
    (fun s _ acc -> s :: acc)
    check_suffixes
    [ ".c"; ";C"; ".i"; ".h" ]

let name = function NeedCPP (s,_) | NoCPP s | External (s,_) -> s

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
  let cmdline = Parameters.CppCommand.get() in
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
end = struct

  let name = "File.t"

  module S =
    Computation.Ref
      (struct
	 include
	   Datatype.List
	   (Project.Datatype.Persistent
	      (* actually strings are immutable here *)
	      (struct type tt = t type t = tt let name = name end))
	 let default () = []
       end)
      (struct
	 let dependencies =
	   [ Parameters.CppCommand.self;
             Parameters.CppExtraArgs.self;
             Parameters.Files.self ]
	 let name = name
       end)

  let () = Project.Computation.add_dependency Ast.self S.self
  let () = Project.Computation.add_dependency Ast.UntypedFiles.self S.self

  (* Allow to register files in advance, e.g. prolog files for plugins *)
  let pre_register file =
    let prev_files = S.get () in
    S.set (prev_files @ [file])

  let register files =
    if S.is_computed () then
      raise (Ast.Bad_Initialisation "[File.register] Too many initializations");
    let prev_files = S.get () in
    S.set (prev_files @ files);
    S.mark_as_computed ()

  let get = S.get
  let is_computed () = S.is_computed ()

end

let get_all = Files.get
let pre_register = Files.pre_register

(*****************************************************************************)
(** {2 AST Integrity check}                                                  *)
(*****************************************************************************)

(* performs various consistency checks over a cil file.
   Code may vary depending on current development of the kernel and/or
   identified bugs. *)
class check_file: Visitor.frama_c_visitor  =
  let check_abort fmt = Cil.fatal ("[AST Integrity Check]@ " ^^ fmt) in
object(self)
  inherit Visitor.generic_frama_c_visitor (Project.current())
    (Cil.inplace_visit())
  val known_loop_annot_id = Hashtbl.create 7
  val known_code_annot_id = Hashtbl.create 7
  val known_fields = FieldinfoHashtbl.create 7
  val known_stmts = StmtHashtbl.create 7
  val known_vars = VarinfoHashtbl.create 7
  val known_logic_info = LogicVarHashtbl.create 7
  val mutable local_vars = VarinfoSet.empty
  val known_logic_vars = LogicVarHashtbl.create 7
  val switch_cases = StmtHashtbl.create 7
  val mutable labelled_stmt = []

  method vvdec v =
    if VarinfoHashtbl.mem known_vars v then
      (let v' = VarinfoHashtbl.find known_vars v in
       if v != v' then (* we can see the declaration twice
                          (decl and def in fact) *)
         (check_abort "variables %a and %a have the same id (%d)"
                       !Ast_printer.d_ident v.vname
                       !Ast_printer.d_ident v'.vname v.vid))
    else
      VarinfoHashtbl.add known_vars v v;
    match v.vlogic_var_assoc with
        None -> DoChildren
      | Some ({ lv_origin = Some v'} as lv) when v == v' ->
          Cilmsg.debug "var %s(%d) has an associated %s(%d)" v.vname v.vid
            lv.lv_name lv.lv_id;
          DoChildren
      | Some lv ->
          (check_abort "C variable %a is not properly referenced by its \
                          associated logic variable %a"
               !Ast_printer.d_ident v.vname !Ast_printer.d_ident lv.lv_name)

  method vvrbl v =
    (try
       if VarinfoHashtbl.find known_vars v != v then
         (check_abort "variable %a is not shared between definition and use"
           !Ast_printer.d_ident v.vname)
    with Not_found ->
      (check_abort "variable %a is not declared" !Ast_printer.d_ident v.vname)
    );
    DoChildren

  method vlogic_var_decl lv =
    LogicVarHashtbl.add known_logic_vars lv lv;
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
      try
        if LogicVarHashtbl.find known_logic_vars v != v then
          (check_abort
             "logic variable %a(%d) is not shared between definition and use"
             !Ast_printer.d_ident v.lv_name v.lv_id)
      with Not_found ->
        (check_abort "logic variable %a(%d) is not declared"
           !Ast_printer.d_ident  v.lv_name v.lv_id)
    end;
    DoChildren

  method vfunc f =
    labelled_stmt <- [];
    StmtHashtbl.clear known_stmts;
    StmtHashtbl.clear switch_cases;
    local_vars <- VarinfoSet.empty;
    List.iter
      (fun x -> local_vars <- VarinfoSet.add x local_vars) f.slocals;
    let check f =
      if StmtHashtbl.length switch_cases <> 0 then
        begin
          StmtHashtbl.iter
            (fun x _ ->
               check_abort
                 "In function %a, statement %a \
                  does not appear in body of switch while porting a \
                  case or default label."
		 Cil.d_var f.svar !Ast_printer.d_stmt x)
	    switch_cases
	end;
      List.iter
        (fun stmt ->
           try if StmtHashtbl.find known_stmts stmt != stmt then
             (check_abort
                  "Label %a in function %a \
                   is not linked to the correct statement:@\n\
                   statement in AST is %a(%d)@\n\
                   statement referenced in goto is %a(%d)"
                  Cil.d_stmt
		  {stmt with skind =
		      Instr (Skip (Cilutil.get_stmtLoc stmt.skind)) }
                  Cil.d_var f.svar
                  Cil.d_stmt (StmtHashtbl.find known_stmts stmt)
                  (StmtHashtbl.find known_stmts stmt).sid
                  Cil.d_stmt stmt stmt.sid
               )
           with Not_found ->
             (check_abort
                  "Label %a in function %a \
                   does not refer to an existing statement"
                  Cil.d_stmt ({stmt with skind =
                                  Instr (Skip (Cilutil.get_stmtLoc stmt.skind))
                              })
                  Cil.d_var f.svar))
        labelled_stmt;
      labelled_stmt <- [];
      StmtHashtbl.clear known_stmts;
      if not (VarinfoSet.is_empty local_vars) then begin
        check_abort
          "Local variables %a of function %a are not part of any block"
          (pp_list ~sep:(no_sep ^^ ",") Cil.d_var)
          (VarinfoSet.elements local_vars)
          Cil.d_var f.svar
      end;
      f
    in
    ChangeDoChildrenPost(f,check)

  method vstmt_aux s =
    StmtHashtbl.add known_stmts s s;
    StmtHashtbl.remove switch_cases s;
    (match s.skind with
         Goto (s,_) -> labelled_stmt <- !s :: labelled_stmt
       | Switch(_,_,cases,loc) ->
           List.iter (fun s -> StmtHashtbl.add switch_cases s loc) cases;
       | _ -> ());
    DoChildren

  method vblock b =
    (* ensures that the blocals are part of the locals of the function. *)
    List.iter
      (fun v ->
         if VarinfoSet.mem v local_vars then begin
           local_vars <- VarinfoSet.remove v local_vars;
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
            if not (fi == FieldinfoHashtbl.find known_fields fi)
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
            if not (fi == FieldinfoHashtbl.find known_fields fi)
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

  method vterm t =
    match t.term_node with
      | TLval _ ->
	  begin match t.term_type with
	    | Ctype ty ->
                ignore
                  (Cilmsg.verify (not (isVoidType ty))
                     "logic term with void type:%a" d_term t);
                DoChildren
	    | _ -> DoChildren
	  end
      | Tat(_,StmtLabel l) -> labelled_stmt <- !l::labelled_stmt; DoChildren
      | _ -> DoChildren

  method vinitoffs = self#voffs

  method vglob_aux = function
      GCompTag(c,_) ->
        Cilmsg.debug "Adding fields for type %s(%d)" c.cname c.ckey;
        List.iter
          (fun x -> FieldinfoHashtbl.add known_fields x x) c.cfields;
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
                   let l'' =
                     Kernel_function.get_formals
                       (Globals.Functions.get v)
                   in
                   if List.length l' <> List.length l'' then
                     (check_abort
                          "mismatch between FormalsDecl and Globals.Functions \
                           on prototype %a." !Ast_printer.d_ident v.vname)
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
        Dfun_or_pred li | Dinvariant li | Dtype_annot li ->
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
      Pat(_,StmtLabel l) ->  labelled_stmt <- !l::labelled_stmt; DoChildren
    | _ -> DoChildren

  method vlogic_info_decl li =
    LogicVarHashtbl.add known_logic_info li.l_var_info li;
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
            (li == (LogicVarHashtbl.find known_logic_info li.l_var_info)) then
              (check_abort "logic function %a information is \
                     not shared between declaration and use"
                 !Ast_printer.d_ident li.l_var_info.lv_name)
        with Not_found ->
          (check_abort "logic function %a has no information"
             !Ast_printer.d_ident li.l_var_info.lv_name)
      end;
    DoChildren

end

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
	    String.sub cmdl (percent2 + 2) (String.length cmdl - percent2 + 2)
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
	(Parameters.CppExtraArgs.get_set ~sep:" " ()) ^
          (if Parameters.ReadAnnot.get() && Parameters.PreprocessAnnot.get()
	   then " -dD" else "")
      in
      Kernel.feedback "@{<i>preprocessing@} with \"%s %s %s\"" cmdl supp_args f;
      if Sys.command (cmd supp_args f ppf) <> 0 then begin
	Extlib.safe_remove ppf;
	Kernel.abort "failed to run: %s
you may set the CPP environment variable to select the proper \
preprocessor command or use the option \"-cpp-command\"."
	  (cmd supp_args f ppf);
      end;
      let ppf =
        if Parameters.ReadAnnot.get() && Parameters.PreprocessAnnot.get()
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
	Kernel.abort "file %S does not exist" f;
      try Hashtbl.find check_suffixes suf f
      with Not_found ->
        Kernel.abort "Could not find a suitable plugin for parsing %s" f

let files_to_cil files =
  (* mark as computed early in case of a typing error occur: do not type check
     the erroneous program twice. *)
  Ast.mark_as_computed ();
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
	       Kernel.abort "skipping file %S that has errors." (name f))
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
    Kernel.abort "your code cannot be parsed; aborting analysis.";
  debug_globals files;

  (* Get rid of leaf functions that have a spec but are not called
     anywhere. *)
  Rmtmps.removeUnusedTemps merged_file;
  if Parameters.UnspecifiedAccess.get()
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
                   Int64.compare c1 c2 = 0 &&
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
                           (fun fmt (s,m,w,r) ->
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
                   (fun x ->
                      not (List.exists
                             (fun y -> LvalComparable.compare x y = 0)
                             m)) l
               in
               let not_separated_modified l1 l2 =
                 List.fold_left
                   (fun flag (m,r) ->
                      flag || not_separated (remove_mod m l2) r) false l1
               in
               let warn,_,_ =
                 List.fold_left
                   (fun ((warn,writes,reads) as res) (_,m,w,r) ->
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
                  super#pop_stmt stmt;
                  let father = super#current_stmt in
		  super#push_stmt stmt;
		  stmt, father
              | None ->
		  assert false
	    in
            let is_in_same_block () = match !block_with_user_annots,father with
              | None, None -> true
              | Some block, Some stmt_father when block == stmt_father -> true
              | _, _ -> false
	    in
            let synchronize_user_annot annot =
	      Annotations.add st [] (Before (User annot))
	    in
            let synchronize_previous_user_annots () =
              if !user_annots_for_next_stmt <> []
              then begin
                if is_in_same_block ()
                then
                  List.iter synchronize_user_annot
                    (List.sort
                       (fun x y -> x.annot_id - y.annot_id)
                       !user_annots_for_next_stmt)
                else
                  Kernel.warning ~current:true ~once:true
		    "Ignoring previous annotation relative to next statement effects" ;
                block_with_user_annots := None ;
                user_annots_for_next_stmt := []
              end
            in
            let add_user_annot_for_next_stmt annot =
              if !user_annots_for_next_stmt = []
              then
                (block_with_user_annots := father;
                 user_annots_for_next_stmt := [annot])
              else if is_in_same_block ()
              then
                user_annots_for_next_stmt := annot::!user_annots_for_next_stmt
              else
		begin
		  Kernel.warning ~current:true ~once:true
		    "Ignoring previous annotation relative to next statement effects" ;
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
                    synchronize_previous_user_annots () ;
                    (* Synchronize loop annotations on that statement *)
                    List.iter synchronize_user_annot
                      (List.sort (fun x y -> x.annot_id - y.annot_id) annot);
                    super#vstmt st
                | _ ->
		    (* Synchronize previous annotations on that statement *)
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
      if Parameters.SimplifyCfg.get () then begin
	Cil.prepareCFG ~keepSwitch:(Parameters.KeepSwitch.get ()) fundec;
        Cfg.clearCFGinfo fundec;
        Cfg.cfgFun fundec;
      end;
      Globals.Functions.add (Db_types.Definition(fundec,loc))
  | GVarDecl (spec, ({vtype=TFun (_,_,_,_) } as f),loc) ->
      (* global prototypes *)
      let args =
        try Some (Cil.getFormalsDecl f) with Not_found -> None
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
      let loc = Cilutil.get_stmtLoc st.skind in
      if Annotations.get_all st <> [] || st.labels <> [] then
        keep_stmt <- Cilutil.StmtSet.add st keep_stmt;
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
      in
      (* uncomment if you don't want to consider scope of locals (see below) *)
      (* b.blocals <- [];*)
      ChangeDoChildrenPost(b,optim)

    method vglob_aux = function
        GFun (f,_) -> f.sspec <- Cil.empty_funspec();
          (* uncomment if you dont want to treat scope of locals (see above)*)
          (* f.sbody.blocals <- f.slocals; *)
          DoChildren
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
  in visitFramacFileSameGlobals visitor file

let print_renaming: Cil.cilVisitor = object
  inherit Cil.nopCilVisitor
  method vvdec v =
    if v.vname <> v.vorig_name then begin
      Cil.info "Variable %s has been renamed to %s" v.vorig_name v.vname
    end;
    DoChildren
end

let prepare_cil_file file =
  Kernel.feedback ~level:2 "preparing the AST";
  computeCFG file;
  if Parameters.Files.Check.get() then begin
   Cil.visitCilFileSameGlobals (new check_file :> Cil.cilVisitor) file;
  end;
  if Parameters.Files.Orig_name.get () then begin
    Cil.visitCilFileSameGlobals print_renaming file
  end;
  (* Compute the list of functions and their CFG *)
  (try
     List.iter register_global file.globals
   with Globals.Vars.AlreadyExists(vi,_) ->
     Kernel.fatal
       "Trying to add the same varinfo twice: %a (vid:%d)" Cil.d_var vi vi.vid
  );
  Rmtmps.removeUnusedTemps file;
  Globals.Functions.iter synchronize_source_annot;
  cleanup file;
  (* Unroll loops in file *)
  Unroll_loops.compute (Parameters.UnrollingLevel.get ()) file;
  Cfg.clearFileCFG ~clear_id:false file;
  Cfg.computeFileCFG file;
  (* Annotate functions from declspec. *)
  Translate_lightweight.interprate file;
  Ast.set_file file

let init_project_from_cil_file prj file =
  Project.copy
    ~except:
    (Project.Selection.singleton Ast.self Kind.Select_Dependencies)
    prj;
  Project.on prj
    (fun file ->
       (* Fill logic tables with builtins *)
       Logic_env.Builtins.apply ();
       Logic_env.prepare_tables ();
       prepare_cil_file file)
    file

(* items in the machdeps list are of the form
   (machine, (is_public, action_when_selected))
   where
   - machine is the machine name
   - is_public is true if the machine is public (shown in -machdeps help)
   - action_when_selected is the action to perform when the corresponding
   machine is set as current (i.e. defining the right architecture via
   Machdep.DEFINE) *)
let machdeps =
  [ "x86_16",
    (true, fun () -> let module M = Machdep.DEFINE(Machdep_x86_16) in ());
    "x86_32",
    (true, fun () -> let module M = Machdep.DEFINE(Machdep_x86_32) in ());
    "x86_64",
    (true, fun () -> let module M = Machdep.DEFINE(Machdep_x86_64) in ());
    "ppc_32",
    (true, fun () -> let module M = Machdep.DEFINE(Machdep_ppc_32) in ());
    "ppc_32_diab",
    (false, fun () -> let module M = Machdep.DEFINE(Machdep_ppc_32_diab) in ())
  ]

let pretty_machdeps fmt =
  List.iter
    (fun (x,(public,_)) -> if public then Format.fprintf fmt "@ %s" x)
    machdeps

let set_machdep () = match Parameters.Machdep.get () with
  | "" -> ()
  | s when List.exists (fun x -> fst x = s) machdeps ->
      (snd (List.assoc s machdeps)) ()
  | s ->
      if s = "help" then begin
	Kernel.feedback "supported machines are%t." pretty_machdeps;
      end else begin
	Kernel.error "unsupported machine %s. Try one of%t." s
	  pretty_machdeps;
      end

let () = Cmdline.run_after_configuring_stage set_machdep

let prepare_from_c_files () =
  Cil.initCIL (Logic_builtin.init());
  Logic_env.Builtins.apply ();
  Logic_env.prepare_tables ();
  let files = Files.get () in (* Allow pre-registration of prolog files *)
  let cil = files_to_cil files in
  prepare_cil_file cil

let create_project_from_visitor_aux ?(pure_copy=false) prj_name visitor =
  let except =
    if pure_copy then
      let add s = Project.Selection.add s Kind.Do_Not_Select_Dependencies in
      ((add Cil.BuiltinFunctions.self) $
	 (add Logic_env.LogicInfo.self) $
	 (add Logic_env.LogicTypeInfo.self) $
	 (add Logic_env.LogicCtorInfo.self) $
	 (add Globals.Annotations.self) $
	 (add Globals.Vars.self) $
	 (add Globals.Functions.self) (*$
	 (add Annotations.self)*))
        (Project.Selection.singleton Ast.self
	   Kind.Do_Not_Select_Dependencies)
    else
      Project.Selection.add
	Cil.selfMachine Kind.Select_Dependencies
	(Project.Selection.add Parameters.Files.self
           Kind.Select_Dependencies
           Logic_env.builtin_states)
  in
  let except =
    Project.Selection.add Annotations.self Kind.Select_Dependencies except
  in
  let prj = Project.create_by_copy ~except prj_name in
  (*  Project.copy ~only:Logic_env.builtin_states prj; *)
  if not pure_copy then begin
    (* reset projectified parameters to their default values *)
    let temp = Project.create "File.temp" in
    Project.copy ~only:(Plugin.get_selection ()) ~src:temp prj;
    Project.remove ~project:temp ()
  end;
  let visitor = (visitor prj :> Cil.cilVisitor) in
  (* queue of visitor is filled below *)
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
            Globals.Annotations.add_user (set_annotation a)))
    (Globals.Annotations.get_all());
  let file = Ast.get () in
  let file' = Cil.visitCilFileCopy visitor file in
  Project.on prj Cil.initCIL (fun () -> ());
  Cfg.clearFileCFG ~clear_id:false file';
  Cfg.computeFileCFG file';
  Project.on
    ~only:(Project.Selection.singleton
	     Ast.self Kind.Do_Not_Select_Dependencies)
    prj
    Ast.set_file
    file';
  if Parameters.Files.Check.get() then
    Project.on
      prj
      (* eta-expansion required because of operations on the current project in
	 the class construtor *)
      (fun f -> Cil.visitCilFile (new check_file :> Cil.cilVisitor) f)
      file';
  prj

let init_from_c_files files =
  (* Fill logic tables with builtins *)
  (match files with
   | [] -> ()
   | _ -> Files.register files);
  prepare_from_c_files ()

let init_from_cmdline () =
  let files = Parameters.Files.get () in
  if files = [] && not !Config.is_gui then Kernel.warning "no input file.";
  let files = List.map (fun s -> from_filename s) files in
  try
    init_from_c_files files;
    if Parameters.Files.Check.get () then begin
      Cil.visitCilFile(new check_file :> Cil.cilVisitor) (Ast.get())
    end;
    if Parameters.Files.Copy.get () then begin
      let prj =
	create_project_from_visitor_aux ~pure_copy:true
	  "debug_copy_prj" (new Visitor.frama_c_copy)
      in
      Project.set_current prj;
    end;
  with Ast.Bad_Initialisation s ->
    Kernel.fatal "bad initialisation: %s" s

let init_from_cmdline =
  Journal.register
    "File.init_from_cmdline"
    (Type.func Type.unit Type.unit)
    init_from_cmdline

let init_from_c_files =
  Journal.register
    "File.init_from_c_files"
    (Type.func (Type.list ty) Type.unit)
    init_from_c_files

let prepare_from_c_files =
  Journal.register
    "File.prepare_from_c_files"
    (Type.func Type.unit Type.unit)
    prepare_from_c_files

let () = Ast.set_default_initialization
  (fun () ->
     if Files.is_computed () then prepare_from_c_files ()
     else init_from_cmdline ())

let pp_file_to fmt_opt =
  let pp_ast = Cil.d_file (new Printer.print ()) in
  let ast = Ast.get () in
  (match fmt_opt with
    | None -> Parameters.CodeOutput.output "%a" pp_ast ast
    | Some fmt -> pp_ast fmt ast)

let unjournalized_pretty prj (fmt_opt:Format.formatter option) () =
  Project.on prj pp_file_to fmt_opt

let journalized_pretty =
  Journal.register "File.pretty"
    (Type.func3
       ~label1:("prj",Some Project.current) Project.ty
       ~label2:("fmt",Some (fun () -> None)) (Type.option Type.formatter)
       Type.unit Type.unit)
    unjournalized_pretty

let pretty ?(prj=Project.current ()) ?fmt () =
  journalized_pretty prj fmt ()

(* hide the optional argument of [create_project_from_visitor_aux] *)
let create_project_from_visitor p = create_project_from_visitor_aux p

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

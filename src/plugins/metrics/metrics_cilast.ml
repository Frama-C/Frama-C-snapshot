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

module Kernel_file = File
open Cil_datatype
open Cil_types
open Metrics_base
;;

type cilast_metrics = {
  fundecl_calls: int Metrics_base.VInfoMap.t;
  fundef_calls: int Metrics_base.VInfoMap.t;
  extern_global_vars: Metrics_base.VInfoSet.t;
  basic_global_metrics: BasicMetrics.t
}

(** Syntactic metrics
   =================
   The goal is to collect various (syntactic) information about the source code
   (slocs, assignments, loops, ...).
   From those one can compute McCabe's cyclomatic complexity.
*)
class type sloc_visitor = object
  inherit Visitor.generic_frama_c_visitor

  (* Get the number of times a function has been called if it has been
     defined (fundef) or not (fundecl).
  *)
  method fundecl_calls: int Metrics_base.VInfoMap.t
  method fundef_calls: int Metrics_base.VInfoMap.t
  (* Global variables with 'Extern' storage *)
  method extern_global_vars: Metrics_base.VInfoSet.t

  (* Get the computed metrics *)
  method get_global_metrics: BasicMetrics.t

  (* Print the metrics of a file [Datatype.Filepath.t] to a formatter
     Yields a fatal error if the file does not exist (or has no metrics).
  *)
  method pp_file_metrics: Format.formatter -> Datatype.Filepath.t -> unit

  method pp_detailed_text_metrics: Format.formatter -> unit
  (** Print results of all file and functions to the given formatter as text *)

  method print_stats: Format.formatter -> unit
  (** Print computed metrics to a formatter *)

  method get_metrics_map:
    (BasicMetrics.t OptionKf.Map.t) Datatype.Filepath.Map.t
  (** Compute and return per-function metrics *)
end

(* Various metrics computing visitor on Cil AST.
   These metrics are a necessary step to compute cyclomatic complexity.
*)
open BasicMetrics ;;
class slocVisitor ~libc : sloc_visitor = object(self)
  inherit Visitor.frama_c_inplace


  (* Global metrics store for this Cil AST *)
  val global_metrics = ref BasicMetrics.empty_metrics
  (* Local metrics in computation *)
  val local_metrics = ref BasicMetrics.empty_metrics

  (* Local metrics are kept stored after computation in this map of maps.
     Its storing hierarchy is as follows: filename -> function_name -> metrics
  *)
  val mutable metrics_map:
    (BasicMetrics.t OptionKf.Map.t) Datatype.Filepath.Map.t =
    Datatype.Filepath.Map.empty

  val mutable seen_vars = Varinfo.Set.empty;

  val fundecl_calls: int VInfoMap.t ref = ref VInfoMap.empty;
  val fundef_calls: int VInfoMap.t ref = ref VInfoMap.empty;
  val extern_global_vars = ref VInfoSet.empty

  (* Getters/setters *)
  method fundecl_calls = !fundecl_calls
  method fundef_calls = !fundef_calls
  method extern_global_vars = !extern_global_vars
  method get_global_metrics = !global_metrics
  method get_metrics_map = metrics_map

  method private update_metrics_map filename kfmap =
    metrics_map <- Datatype.Filepath.Map.add filename kfmap metrics_map

  (* Utility method to increase metrics counts *)
  method private incr_both_metrics f =
    apply_then_set f global_metrics;
    apply_then_set f local_metrics

  method private add_map map vinfo value =
    map := VInfoMap.add vinfo value !map

  method private stats_of_filename filename =
    try Datatype.Filepath.Map.find filename metrics_map
    with
      | Not_found ->
        Metrics_parameters.fatal "Metrics for file %a not_found@."
          Datatype.Filepath.pretty filename

  method pp_file_metrics fmt filename =
    Format.fprintf fmt "@[<v 0>%a@]"
      (fun fmt filename ->
        let fun_tbl = self#stats_of_filename filename in
        OptionKf.Map.iter (fun _fun_name fmetrics ->
          Format.fprintf fmt "@ %a" pp_base_metrics fmetrics)
          fun_tbl;
      ) filename

  method pp_detailed_text_metrics fmt =
    Datatype.Filepath.Map.iter
      (fun filename _func_tbl ->
        Format.fprintf fmt "%a" self#pp_file_metrics filename) metrics_map

  method print_stats fmt =
    Format.pp_set_formatter_tag_functions fmt Metrics_base.html_tag_functions;
    Format.pp_set_tags fmt true;
    let pr_hdr fmt hdr_name =
      Format.fprintf fmt "@{<th>%s@}" hdr_name in
    Datatype.Filepath.Map.iter
      (fun filename func_tbl ->
        Metrics_parameters.result ~level:2 "%a" self#pp_file_metrics filename;
        if func_tbl <> OptionKf.Map.empty then
          begin
            Format.fprintf fmt
              "@[<v 0>@{<h3>%a@}<br/>@ \
               @{<table>\
               @[<v 2>@ \
                 @[<v 2>@{<tbody>@ \
                    @{<tr>@[<v 2>@ \
                       %a@ %a@ %a@ %a@ %a@ %a@ %a@ %a@ %a@ @]@}@ \
                       %a@ \
                       @}@]@]@ @} \
               @]@ "
              Datatype.Filepath.pretty filename
              pr_hdr "Function" pr_hdr "#If stmts" pr_hdr "#Assignments"
              pr_hdr "#Loops" pr_hdr "#Calls" pr_hdr "#Gotos"
              pr_hdr "#Pointer dereferencing" pr_hdr "#Exits"
              pr_hdr "Cyclomatic value"
              (fun fmt fun_tbl ->
                OptionKf.Map.iter
                  (fun _fname fmetrics ->
                    Format.fprintf fmt "%a"
                      pp_base_metrics_as_html_row fmetrics;
                  ) fun_tbl
              ) func_tbl;
          end
        else 
          Metrics_parameters.warning
            "Filename <%a> has no functions@."
            Datatype.Filepath.pretty filename)
      metrics_map

(* Save the local metrics currently computed.
   Clears it before starting a new metrics computation (e.g. when entering a new
   function definition.
   Global metrics are never reset as they define metrics on the whole Cil.file.
*)
  method private record_and_clear_function_metrics metrics =
    let filename = metrics.cfile_name in
    let funcname = metrics.cfunc in
    local_metrics := BasicMetrics.set_cyclo !local_metrics
           (BasicMetrics.compute_cyclo !local_metrics);
    global_metrics := BasicMetrics.set_cyclo !global_metrics
           (!global_metrics.ccyclo + !local_metrics.ccyclo);
    (try
       let fun_tbl = Datatype.Filepath.Map.find filename metrics_map in
       self#update_metrics_map filename
         (OptionKf.Map.add funcname !local_metrics fun_tbl);
     with
       | Not_found ->
         let new_kfmap =
           OptionKf.Map.add funcname !local_metrics
             OptionKf.Map.empty
         in self#update_metrics_map filename new_kfmap;
    );
    local_metrics := empty_metrics;

  method! vvdec vi =
    if not (Varinfo.Set.mem vi seen_vars) then (
      if Cil.isFunctionType vi.vtype then (
        if consider_function ~libc vi then begin
          global_metrics := incr_funcs !global_metrics;
          (* Mark the function as seen, adding 0 to the number of calls *)
          self#update_call_maps vi 0;
        end
      ) else (
        if vi.vglob && not vi.vtemp &&
           Metrics_base.consider_variable ~libc vi
        then (
          global_metrics:= incr_glob_vars !global_metrics;
          if vi.vstorage = Extern then
            extern_global_vars := VInfoSet.add vi !extern_global_vars
        )
      );
      seen_vars <- Varinfo.Set.add vi seen_vars;
    );
    Cil.SkipChildren
      
  method! vfunc fdec =
    if consider_function ~libc fdec.svar then
      begin
        (* Here, we get to a fundec definition.this function has a body,
           let's put it to the "function with source" table. *)
        local_metrics :=
          {!local_metrics with
            cfile_name = file_of_fundef fdec;
            cfunc = Some (Globals.Functions.get fdec.svar);
            cfuncs = 1; (* Only one function is indeed being defined here *)};
        let fvinfo = fdec.svar in
        (if not (VInfoMap.mem fvinfo !fundef_calls) then
           (* Never seen before, including never been called *)
            self#add_map fundef_calls fvinfo 0);
        (* On return record the analysis of the function. *)
        Cil.ChangeDoChildrenPost
          (fdec,
           fun _ ->
             begin
               if !local_metrics <> empty_metrics
               then self#record_and_clear_function_metrics !local_metrics;
               fdec;
             end
          );
      end
    else Cil.SkipChildren

  method! vlval (host, _) =
    begin
      match host with
        | Mem _ -> self#incr_both_metrics incr_ptrs;
        | _ -> ()
    end;
    Cil.DoChildren

  method! vstmt s =
    self#incr_both_metrics incr_slocs;
    let do_children =
      match s.skind with
        | If _ ->
            self#incr_both_metrics incr_ifs;
            self#incr_both_metrics incr_dpoints;
            true
        | Loop _ -> self#incr_both_metrics incr_loops; true
        | Goto _ -> self#incr_both_metrics incr_gotos; true
        | Return _ -> self#incr_both_metrics incr_exits; true
        | Switch (_, _, _slist, _) -> true
        (* The catching block is one more possible flow alternative *)
        | TryFinally _
        | TryExcept _ -> self#incr_both_metrics incr_dpoints; true
        | UnspecifiedSequence l ->
            List.iter 
              (fun (s,_,_,_,_) ->
                ignore
                  (Visitor.visitFramacStmt (self:>Visitor.frama_c_visitor) s))
              l;
            false
        | _ -> true
    in
    (* Default cases are not path choice points, as normal labels.
       Non-default cases are ... just like if statements.
    *)
    let rec has_case_label labels =
      match labels with
        | (Case _) :: _->
          self#incr_both_metrics incr_dpoints;
        | _ :: labels -> has_case_label labels
        | [] -> ()
    in has_case_label s.labels;
    if do_children then Cil.DoChildren else Cil.SkipChildren

  method! vexpr e =
    begin
      (* Logical ands and ors are lazy and generate two different paths *)
      match e.enode with
        | BinOp ((LAnd | LOr), _, _, _) ->
          self#incr_both_metrics incr_dpoints;
        | _ -> ()
    end;
    Cil.DoChildren

  method private image (glob:global) =
    (* extract just the name of the global , for printing purposes *)
    match glob with
      | GVar (v, _, _) -> v.vname ^ " (GVar) "
      | GVarDecl (v, _) -> v.vname ^ " (GVarDecl) "
      | GFunDecl (_, v, _) -> v.vname ^ " (GFunDecl) "
      | GFun (fdec, _) -> fdec.svar.vname ^ " (GFun) "
      | GType (ty, _) -> ty.tname
      | GCompTag (ci, _) | GCompTagDecl (ci, _) -> ci.cname
      | GEnumTagDecl (ei, _) | GEnumTag (ei, _) -> ei.ename
      | GAsm (_, _) | GPragma _ | GText _ -> ""
      | GAnnot (an,_) ->
        begin
          match an with
            | Dfun_or_pred (li, _) -> li.l_var_info.lv_name
            | Dvolatile (_, _, _, _, _) -> " (Volatile) "
            | Daxiomatic (s, _, _, _) -> s
            | Dtype (lti, _) ->  lti.lt_name
            | Dlemma (ln, _, _, _, _, _, _) ->  ln
            | Dinvariant (toto, _) -> toto.l_var_info.lv_name
            | Dtype_annot (ta, _) -> ta.l_var_info.lv_name
            | Dmodel_annot (mi, _) -> mi.mi_name
            | Dcustom_annot (_c, _n, _, _) -> " (Custom) "
            | Dextended ((_, n, _, _), _, _) -> " (Extension " ^ n ^ ")"
        end

  method private images (globs:global list) =
    (* extract just the names of the globals, for printing purposes *)
    let les_images = List.map self#image globs in
    String.concat "," les_images

  method private update_call_maps vinfo increment =
    if consider_function ~libc vinfo then
      let update_call_map funcmap =
        self#add_map funcmap vinfo
          (increment + try VInfoMap.find vinfo !funcmap with Not_found-> 0)
      in
      if vinfo.vdefined
      then update_call_map fundef_calls
      else update_call_map fundecl_calls


  method! vinst i =
    begin match i with
      | Call(v, e, _, _) ->
        self#incr_both_metrics incr_calls;
        (match e.enode with
          | Lval(Var vinfo, NoOffset) -> self#update_call_maps vinfo 1
          | _ -> ());
        (match v with
         | Some _ -> self#incr_both_metrics incr_assigns
         | None -> ());
      | Set _ -> self#incr_both_metrics incr_assigns;
      | Local_init (_, AssignInit _, _) -> self#incr_both_metrics incr_assigns
      | Local_init (_, ConsInit(f,_, k),_) ->
        (* if f takes the address of the initialized variable as first
           argument, there's no explicit assignment in the current function. *)
        (match k with
         | Plain_func -> self#incr_both_metrics incr_assigns
         | Constructor -> ());
        self#incr_both_metrics incr_calls;
        self#update_call_maps f 1
      | Asm _ | Skip _ | Code_annot _ -> ()
    end;
    Cil.DoChildren

end

let dkey = Metrics_parameters.register_category "used-files"

class reachable_from_main visited_vardefs = object
  inherit Visitor.frama_c_inplace

  val visited_vardefs = ref visited_vardefs

  method get_visited_vardefs = !visited_vardefs

  method! vvrbl vi =
    if vi.vglob && not (Varinfo.Set.mem vi !visited_vardefs) then begin
      Metrics_parameters.feedback ~dkey "visiting %a" Printer.pp_varinfo vi;
      visited_vardefs := Varinfo.Set.add vi !visited_vardefs;
      try
        let kf = Globals.Functions.get vi in
        try
          let fd = Kernel_function.get_definition kf in
          let vis = new reachable_from_main !visited_vardefs in
          ignore (Visitor.visitFramacFunction (vis :> Visitor.frama_c_visitor) fd);
          visited_vardefs :=
            Varinfo.Set.union !visited_vardefs vis#get_visited_vardefs
        with Kernel_function.No_Definition -> ()
      with Not_found -> (* global var, not function *)
        let def = Ast.def_or_last_decl vi in
        let vis = new reachable_from_main !visited_vardefs in
        ignore (Visitor.visitFramacGlobal (vis :> Visitor.frama_c_visitor) def);
        visited_vardefs :=
          Varinfo.Set.union !visited_vardefs vis#get_visited_vardefs
    end;
    Cil.SkipChildren
end

let reachable_from_main () =
  try
    let (kf, _) = Globals.entry_point () in
    Metrics_parameters.feedback ~dkey "compute_reachable_from_main: %a"
      Kernel_function.pretty kf;
    try
      let main_fd = Kernel_function.get_definition kf in
      let vis = new reachable_from_main (Varinfo.Set.singleton main_fd.svar) in
      ignore (Visitor.visitFramacFunction (vis :> Visitor.frama_c_visitor) main_fd);
      Some (Varinfo.Set.elements vis#get_visited_vardefs)
    with Kernel_function.No_Definition -> None
  with Globals.No_such_entry_point _ -> None

(* Requires a main function *)
let compute_files_defining_globals gvars =
  List.fold_left (fun acc vi ->
      Metrics_parameters.feedback ~dkey "looking for global: %a"
        Printer.pp_varinfo vi;
      let def = Ast.def_or_last_decl vi in
      let is_def =
        match def with
        | GVar _ | GFun _ -> true
        | _ -> false
      in
      if is_def then
        let loc = Cil_datatype.Global.loc def in
        if Location.equal loc Location.unknown then acc
        else begin
          Metrics_parameters.feedback ~dkey "found %s at: %a"
            (if is_def then "definition" else "declaration")
            Printer.pp_location loc;
          Datatype.Filepath.Set.add ((fst loc).Filepath.pos_path) acc
        end
      else acc
    ) (Datatype.Filepath.Set.empty) gvars

class logic_loc_visitor = object
  inherit Visitor.frama_c_inplace

  val locs = ref Location.Set.empty
  method get_locs = !locs

  method! vterm t =
    locs := Cil_datatype.Location.Set.add t.term_loc !locs;
    Cil.DoChildren

  method! vpredicate p =
    locs := Cil_datatype.Location.Set.add p.pred_loc !locs;
    Cil.DoChildren
end

let get_filenames_in_funspec kf =
  try
    let spec = Annotations.funspec ~populate:false kf in
    Metrics_parameters.feedback ~dkey "looking for files in the spec of: %a"
      Kernel_function.pretty kf;
    List.fold_left (fun acc b ->
        let visitor = new logic_loc_visitor in
        ignore
          (Visitor.visitFramacBehavior (visitor :> Visitor.frama_c_visitor) b);
        let locs = visitor#get_locs in
        Cil_datatype.Location.Set.fold (fun loc acc' ->
            let path = (fst loc).Filepath.pos_path in
            Metrics_parameters.feedback ~dkey ~once:true
              "found annotation in: %a"
              Datatype.Filepath.pretty path;
            Datatype.Filepath.Set.add path acc'
          ) locs acc
      ) Datatype.Filepath.Set.empty spec.spec_behavior
  with Annotations.No_funspec _ -> Datatype.Filepath.Set.empty

let compute_files_defining_funspecs gvars =
  List.fold_left
    (fun acc vi ->
       try
         let kf = Globals.Functions.get vi in
         let fs = get_filenames_in_funspec kf in
         Datatype.Filepath.Set.union acc fs
       with Not_found -> acc
    ) Datatype.Filepath.Set.empty gvars

let used_files () =
  match reachable_from_main () with
  | None -> Metrics_parameters.abort
              "'%s' requires an entry point (-main) with a body"
              Metrics_parameters.UsedFiles.option_name
  | Some reachable_gvars ->
    let used_for_defs = compute_files_defining_globals reachable_gvars in
    let used_for_specs = compute_files_defining_funspecs reachable_gvars in
    Datatype.Filepath.Set.union used_for_defs used_for_specs

let pretty_used_files used_files =
  (* Note: used_files may also contain #include'd files,
           but we only want those given in the command line *)
  let cmdline_files = List.fold_left (fun acc file ->
      Datatype.Filepath.Set.add (
        Datatype.Filepath.of_string (Kernel_file.get_name file)
      ) acc
    ) Datatype.Filepath.Set.empty (Kernel_file.get_all ())
  in
  let used_cmdline_files, used_included_files =
    Datatype.Filepath.Set.partition (fun path ->
        Datatype.Filepath.Set.mem path cmdline_files
      ) used_files
  in
  let used_included_c_files =
    Datatype.Filepath.Set.filter
      (fun f ->
         Extlib.string_suffix ~strict:true ".c"
           (f : Filepath.Normalized.t :> string))
      used_included_files
  in
  let used_implicitly_included_c_files =
    Datatype.Filepath.Set.diff used_included_c_files cmdline_files
  in
  let unused_cmdline_files =
    Datatype.Filepath.Set.diff cmdline_files used_cmdline_files
  in
  let nb s = Datatype.Filepath.Set.cardinal s in
  let pp_filepaths title fmt paths =
    let n = nb paths in
    if n = 0 then Format.ifprintf fmt ""
    else
      let title_len = String.length title in
      Format.fprintf fmt
        "@\n%s (%d)\
         @\n%s\
         @\n%a@\n"
        title n (String.make (title_len + 4) '=')
        (Pretty_utils.pp_list ~sep:" \\@\n" ~suf:" \\" Datatype.Filepath.pretty)
        (Datatype.Filepath.Set.elements paths)
  in
  Metrics_parameters.result
    "Used files starting at function '%a':@\n\
     - command line has %d out of %d file(s) being used@\n\
     - %d used file(s) inside #include directives, of which %d '.c' file(s).@\n\
     %a%a%a"
    Kernel_function.pretty (fst (Globals.entry_point ()))
    (nb used_cmdline_files) (nb cmdline_files)
    (nb used_included_files) (nb used_included_c_files)
    (pp_filepaths "Used command-line files") used_cmdline_files
    (pp_filepaths "Unused command-line files") unused_cmdline_files
    (pp_filepaths "Used, but implicitly included C files")
    used_implicitly_included_c_files

let dump_html fmt cil_visitor =
  (* Activate tagging for html *)
  Format.pp_set_formatter_tag_functions fmt html_tag_functions;
  Format.pp_set_tags fmt true;

  let pr_row s fmt n =
    Format.fprintf fmt
      "@{<tr>@[<v 1>@ \
              @{<td class=\"entry\">%s@}@ \
              @{<td class=\"stat\">%d@}@]@ @} " s n
  in
  let pr_stats fmt visitor =
    let metrics = visitor#get_global_metrics in
    Format.fprintf fmt "@[<v 0>@{<table>%a@}@]"
      (fun fmt metrics ->
        List.iter2 (fun text value -> pr_row text fmt value)
          ["SLOC"; "Number of if statements"; "Number of assignments";
           "Number of loops"; "Number of calls"; "Number of gotos";
           "Number of pointer accesses";]
          [metrics.cslocs; metrics.cifs; metrics.cassigns;
           metrics.cloops; metrics.ccalls; metrics.cgotos;
           metrics.cptrs;]) metrics
  in
  let pr_prelude fmt cil_visitor =
    Format.fprintf fmt "@[<v 0>\
        @{<div>@ \
        @{<h1>@{<span>Metrics@}@}@ \
        @{<h2>Synthetic results@}@ <br/>@ \
        @{<span>Defined function(s)@} (%d): <br/>@ \
        @[&nbsp; %a@]@ <br/>@ <br/>@ \
        @{<span>Undefined function(s)@} (%d):@ <br/>@ \
        @[&nbsp; %a@]@ <br>@ <br/>@ \
        @{<span>'Extern' global variable(s)@} (%d):@ <br/>@ \
        @[&nbsp; %a@]@ <br>@ <br/>@ \
        @{<span>Potential entry point(s)@} (%d):@ <br/>@ \
        @[&nbsp; %a@]@ <br/>@ <br/>@ \
        @}@]"
      (VInfoMap.cardinal cil_visitor#fundef_calls)
      Metrics_base.pretty_set cil_visitor#fundef_calls
      (VInfoMap.cardinal cil_visitor#fundecl_calls)
      Metrics_base.pretty_set cil_visitor#fundecl_calls
      (VInfoSet.cardinal cil_visitor#extern_global_vars)
      Metrics_base.pretty_extern_vars cil_visitor#extern_global_vars
      (Metrics_base.number_entry_points cil_visitor#fundef_calls)
      Metrics_base.pretty_entry_points cil_visitor#fundef_calls
  in
  let pr_detailed_results fmt cil_visitor =
    Format.fprintf fmt "@[<v 0>\
        @{<div style=\"text-align: left;\">\
        @[<v 2>@ \
          @{<h2>Detailed results@}@ \
          @[<v 0>%a@ @]\
        @]@}"
      (fun fmt cil_visitor -> cil_visitor#print_stats fmt) cil_visitor
  in
  Format.fprintf fmt "@[<v 0>\
      <!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"\
          \"http://www.w3.org/TR/html4/strict.dtd\">@ \
      @{<html>@ \
      @{<head>@ \
       @{<title>%s@}@ \
       <meta content=\"text/html; charset=iso-8859-1\" \
        http-equiv=\"Content-Type\"/>@ \
        @{<style type=\"text/css\">%s@}@ \
      @}@ \
        @{<body>\
         @[<v 2>@ \
         %a@ \
         %a@ \
         %a@ \
         @]@}@}@]@?"
    "Metrics"
    Css_html.css
    pr_prelude cil_visitor
    pr_stats cil_visitor
    pr_detailed_results cil_visitor
;;

let pp_funinfo fmt vis =
  let nfundef = VInfoMap.cardinal vis#fundef_calls in
  let nfundecl = VInfoMap.cardinal vis#fundecl_calls in
  let nextern = VInfoSet.cardinal vis#extern_global_vars in
  let fundef_hdr = Format.sprintf "Defined functions (%d)" nfundef
  and fundecl_hdr = Format.sprintf "Undefined functions (%d)" nfundecl
  and extern_hdr = Format.sprintf "'Extern' global variables (%d)" nextern
  and entry_pts_hdr = Format.sprintf "Potential entry points (%d)"
    (Metrics_base.number_entry_points vis#fundef_calls) in
  Format.fprintf fmt
    "@[<v 0>@[<v 1>%a@ @[%a@]@]@ @ \
            @[<v 1>%a@ @[%a@]@]@ @ \
            @[<v 1>%a@ @[%a@]@]@ @ \
            @[<v 1>%a@ @[%a@]@]@ \
     @]"
    (Metrics_base.mk_hdr 1) fundef_hdr
    Metrics_base.pretty_set vis#fundef_calls
    (Metrics_base.mk_hdr 1) fundecl_hdr
    Metrics_base.pretty_set vis#fundecl_calls
    (Metrics_base.mk_hdr 1) extern_hdr
    Metrics_base.pretty_extern_vars vis#extern_global_vars
    (Metrics_base.mk_hdr 1) entry_pts_hdr
    Metrics_base.pretty_entry_points vis#fundef_calls
;;

let pp_with_funinfo fmt cil_visitor =
  Format.fprintf fmt "@[<v 0>%a@ %a@]"
    pp_funinfo cil_visitor
    pp_base_metrics cil_visitor#get_global_metrics
;;

let get_global_metrics ~libc =
  let file = Ast.get () in
   (* Do as before *)
  let cil_visitor = new slocVisitor ~libc in
  Visitor.visitFramacFileSameGlobals
    (cil_visitor:>Visitor.frama_c_visitor) file;
  cil_visitor#get_global_metrics
;;

let get_metrics_map ~libc =
  let file = Ast.get () in
   (* Do as before *)
  let cil_visitor = new slocVisitor ~libc in
  Visitor.visitFramacFileSameGlobals
    (cil_visitor:>Visitor.frama_c_visitor) file;
  cil_visitor#get_metrics_map
;;

let get_cilast_metrics ~libc =
  let file = Ast.get () in
  (* Do as before *)
  let cil_visitor = new slocVisitor ~libc in
  Visitor.visitFramacFileSameGlobals
    (cil_visitor:>Visitor.frama_c_visitor) file;
  {
    fundecl_calls = cil_visitor#fundecl_calls;
    fundef_calls = cil_visitor#fundef_calls;
    extern_global_vars = cil_visitor#extern_global_vars;
    basic_global_metrics = cil_visitor#get_global_metrics;
  }
;;

let compute_on_cilast ~libc =
  let file = Ast.get () in
  (* Do as before *)
  let cil_visitor = new slocVisitor ~libc in
  Visitor.visitFramacFileSameGlobals
    (cil_visitor:>Visitor.frama_c_visitor) file;
  if Metrics_parameters.ByFunction.get () then
    Metrics_parameters.result
      "@[<v 0>Cil AST@ %t@]" cil_visitor#pp_detailed_text_metrics;
(*  let r =  metrics_to_result cil_visitor in *)
  (* Print the result to file if required *)
  let out_fname = Metrics_parameters.OutputFile.get () in
  begin
    if out_fname <> "" then
      try
        let oc = open_out_bin out_fname in
        let fmt = Format.formatter_of_out_channel oc in
        (match Metrics_base.get_file_type out_fname with
          | Html -> dump_html fmt cil_visitor
          | Text -> pp_with_funinfo fmt cil_visitor
        );
        close_out oc;
      with Sys_error _ ->
        Metrics_parameters.failure "Cannot open file %s.@." out_fname
    else Metrics_parameters.result "%a" pp_with_funinfo cil_visitor
  end

(* Visitor for the recursive estimation of a stack size.
   Its arguments are the function currently being visited and the current
   callstack, as a list of kernel functions.
   The callstack is used to detect recursive calls.
   TODO: this computation is far from optimal; for instance, locals_size could
   be cached for each function. Also, it does not consider calls via function
   pointers. *)
class locals_size_visitor kf callstack = object

  val mutable locals_size_no_temps = Integer.zero
  method get_locals_size_no_temps = locals_size_no_temps

  val mutable locals_size_temps = Integer.zero
  method get_locals_size_temps = locals_size_temps

  val mutable max_size_calls_no_temps = Integer.zero
  method get_max_size_calls_no_temps = max_size_calls_no_temps

  val mutable max_size_calls_temps = Integer.zero
  method get_max_size_calls_temps = max_size_calls_temps

  inherit Visitor.frama_c_inplace

  method! vinst i = match i with
    | Call (_, { enode = Lval(Var vi, NoOffset) }, _, _)
    | Local_init(_, ConsInit(vi,_,_),_) ->
      begin
        try
          let kf' = Globals.Functions.find_by_name vi.vname in
          Metrics_parameters.debug
            "@[function %a:@;computing call to function %a@]"
            Kernel_function.pretty kf Kernel_function.pretty kf';
          let new_cs = kf' :: callstack in
          if List.mem kf' callstack then
            Metrics_parameters.abort
              "@[unsupported recursive call detected:@;%a@]"
              (Pretty_utils.pp_list ~sep:"@ <-@ " Kernel_function.pretty)
              (List.rev new_cs);
          let new_vis = new locals_size_visitor kf' new_cs in
          ignore (Visitor.visitFramacKf
                    (new_vis :> Visitor.frama_c_visitor) kf');
          let call_size_no_temps =
            Integer.add new_vis#get_max_size_calls_no_temps
              new_vis#get_locals_size_no_temps
          in
          let call_size_temps =
            Integer.add new_vis#get_max_size_calls_temps
              new_vis#get_locals_size_temps
          in
          max_size_calls_no_temps <-
            Integer.max max_size_calls_no_temps call_size_no_temps;
          max_size_calls_temps <-
            Integer.max max_size_calls_temps call_size_temps
        with Not_found ->
          (* should not happen *)
          Metrics_parameters.fatal ~current:true
            "@[function not found:@;%s@]" vi.vname;
      end;
      Cil.DoChildren
    | _ -> Cil.DoChildren

  method! vvdec vi =
    if not vi.vglob && not vi.vghost && vi.vstorage = NoStorage then
      begin
        let size_exp = Cil.sizeOf ~loc:vi.vdecl vi.vtype in
        match Cil.constFoldToInt size_exp with
        | None -> Metrics_parameters.error
                    "@[in function %a,@;cannot compute sizeof %a (type %a)@]"
                    Kernel_function.pretty kf Printer.pp_varinfo vi
                    Printer.pp_typ vi.vtype
        | Some size ->
          Metrics_parameters.debug "@[function %a:@;sizeof(%a) = %a (%s)@]"
            Kernel_function.pretty kf
            Printer.pp_varinfo vi (Integer.pretty ~hexa:false) size
            (if vi.vtemp then "temp" else "non-temp");
          if vi.vtemp then
            locals_size_temps <- Integer.add locals_size_temps size
          else
            locals_size_no_temps <- Integer.add locals_size_no_temps size
      end;
    Cil.DoChildren

end

(* Requires a computed Cil AST *)
let compute_locals_size kf =
  let vis = new locals_size_visitor kf [kf] in
  ignore (Visitor.visitFramacKf (vis :> Visitor.frama_c_visitor) kf);
  Metrics_parameters.result "@[%a\t%a\t%a\t%a\t%a@]"
    Kernel_function.pretty kf
    (Integer.pretty ~hexa:false) vis#get_locals_size_no_temps
    (Integer.pretty ~hexa:false)
    (Integer.add vis#get_locals_size_no_temps vis#get_locals_size_temps)
    (Integer.pretty ~hexa:false) vis#get_max_size_calls_no_temps
    (Integer.pretty ~hexa:false)
    (Integer.add vis#get_max_size_calls_no_temps vis#get_max_size_calls_temps)
;;

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

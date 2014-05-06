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

open Cil_datatype
open Cil_types
open Metrics_base
;;


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

  (* Get the computed metris *)
  method get_metrics: BasicMetrics.t

  (* Print the metrics of a file [string] to a formatter
     Yields a fatal error if the file does not exist (or has no metrics).
  *)
  method pp_file_metrics: Format.formatter -> string -> unit

  method pp_detailed_text_metrics: Format.formatter -> unit
  (** Print results of all file and functions to the given formatter as text *)

  method print_stats: Format.formatter -> unit
  (** Print computed metrics to a formatter *)
end

(* Various metrics computing visitor on Cil AST.
   These metrics are a necessary step to compute cyclomatic complexity.
*)
open BasicMetrics ;;
class slocVisitor : sloc_visitor = object(self)
  inherit Visitor.frama_c_inplace


  (* Global metrics store for this Cil AST *)
  val global_metrics = ref BasicMetrics.empty_metrics
  (* Local metrics in computation *)
  val local_metrics = ref BasicMetrics.empty_metrics

  (* Local metrics are kept stored after computation in this map of maps.
     Its storing hierachy is as follows: filename -> function_name -> metrics
  *)
  val mutable metrics_map:
      (BasicMetrics.t Datatype.String.Map.t) Datatype.String.Map.t =
    Datatype.String.Map.empty

  val mutable seen_vars = Varinfo.Set.empty;

  val fundecl_calls: int VInfoMap.t ref = ref VInfoMap.empty;
  val fundef_calls: int VInfoMap.t ref = ref VInfoMap.empty;

  (* Getters/setters *)
  method fundecl_calls = !fundecl_calls
  method fundef_calls = !fundef_calls
  method get_metrics = !global_metrics

  method private update_metrics_map filename strmap =
    metrics_map <- Datatype.String.Map.add filename strmap metrics_map

  (* Utility method to increase metrics counts *)
  method private incr_both_metrics f =
    apply_then_set f global_metrics;
    apply_then_set f local_metrics

  method private add_map map vinfo value =
    map := VInfoMap.add vinfo value !map

  method private stats_of_filename filename =
    try Datatype.String.Map.find filename metrics_map
    with
      | Not_found ->
        Metrics_parameters.fatal "Metrics for file %s not_found@." filename

  method pp_file_metrics fmt filename =
    Format.fprintf fmt "@[<v 0>%a@]"
      (fun fmt filename ->
        let fun_tbl = self#stats_of_filename filename in
        Datatype.String.Map.iter (fun _fun_name fmetrics ->
          Format.fprintf fmt "@ %a" pp_base_metrics fmetrics)
          fun_tbl;
      ) filename

  method pp_detailed_text_metrics fmt =
    Datatype.String.Map.iter
      (fun filename _func_tbl ->
        Format.fprintf fmt "%a" self#pp_file_metrics filename) metrics_map

  method print_stats fmt =
    Format.pp_set_formatter_tag_functions fmt Metrics_base.html_tag_functions;
    Format.pp_set_tags fmt true;
    let pr_hdr fmt hdr_name =
      Format.fprintf fmt "@{<th>%s@}" hdr_name in
    Datatype.String.Map.iter
      (fun filename func_tbl ->
        Metrics_parameters.result ~level:2 "%a" self#pp_file_metrics filename;
        if func_tbl <> Datatype.String.Map.empty then
          begin
            Format.fprintf fmt
              "@[<v 0>@{<h3>%s@}<br/>@ \
               @{<table>\
               @[<v 2>@ \
                 @[<v 2>@{<tbody>@ \
                    @{<tr>@[<v 2>@ \
                       %a@ %a@ %a@ %a@ %a@ %a@ %a@ %a@ %a@ @]@}@ \
                       %a@ \
                       @}@]@]@ @} \
               @]@ "
              filename
              pr_hdr "Function" pr_hdr "#If stmts" pr_hdr "#Assignments"
              pr_hdr "#Loops" pr_hdr "#Calls" pr_hdr "#Gotos"
              pr_hdr "#Pointer dereferencing" pr_hdr "#Exits"
              pr_hdr "Cyclomatic value"
              (fun fmt fun_tbl ->
                Datatype.String.Map.iter
                  (fun _fname fmetrics ->
                    Format.fprintf fmt "%a"
                      pp_base_metrics_as_html_row fmetrics;
                  ) fun_tbl
              ) func_tbl;
          end
        else 
	  Metrics_parameters.warning
	    "Filename <%s> has no functions@." filename)
      metrics_map

(* Save the local metrics currently computed.
   Clears it before starting a new metrics computation (e.g. when entering a new
   function definition.
   Global metrics are never reset as they define metrics on the whole Cil.file.
*)
  method private record_and_clear_function_metrics metrics =
    let filename = metrics.cfile_name in
    let funcname = metrics.cfunc_name in
    (try
       let fun_tbl = Datatype.String.Map.find filename metrics_map in
       self#update_metrics_map filename
         (Datatype.String.Map.add funcname !local_metrics fun_tbl);
     with
       | Not_found ->
         let new_stringmap =
           Datatype.String.Map.add funcname !local_metrics
             Datatype.String.Map.empty
         in self#update_metrics_map filename new_stringmap;
    );
    local_metrics := empty_metrics;

  method! vvdec vi =
    if not (Varinfo.Set.mem vi seen_vars) then (
      if Cil.isFunctionType vi.vtype then (
        if consider_function vi then
          global_metrics := incr_funcs !global_metrics;
      ) else (
        if vi.vglob && not vi.vgenerated
        then (
	  global_metrics:= incr_glob_vars !global_metrics;
        )
      );
      seen_vars <- Varinfo.Set.add vi seen_vars;
    );
    Cil.SkipChildren
      
  method! vfunc fdec =
    if consider_function fdec.svar then
      begin
        (* Here, we get to a fundec definition.this function has a body,
           let's put it to the "function with source" table. *)
        local_metrics :=
          {!local_metrics with
            cfile_name = file_of_fundef fdec;
            cfunc_name = fdec.svar.vname;
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
      | GVarDecl (_, v, _) -> v.vname ^ " (GVarDecl) "
      | GFun (fdec, _) -> fdec.svar.vname ^ " (GFun) "
      | GType (ty, _) -> ty.tname
      | GCompTag (ci, _) | GCompTagDecl (ci, _) -> ci.cname
      | GEnumTagDecl (ei, _) | GEnumTag (ei, _) -> ei.ename
      | GAsm (_, _) | GPragma _ | GText _ -> ""
      | GAnnot (an,_) ->
        begin
          match an with
            | Dfun_or_pred (li, _) -> li.l_var_info.lv_name
            | Dvolatile (_, _, _, _) -> " (Volatile) "
            | Daxiomatic (s, _, _) -> s
            | Dtype (lti, _) ->  lti.lt_name
            | Dlemma (ln, _, _, _, _, _) ->  ln
            | Dinvariant (toto, _) -> toto.l_var_info.lv_name
            | Dtype_annot (ta, _) -> ta.l_var_info.lv_name
            | Dmodel_annot (mi, _) -> mi.mi_name
            | Dcustom_annot (_c, _n, _) -> " (Custom) "
        end

  method private images (globs:global list) =
    (* extract just the names of the globals, for printing purposes *)
    let les_images = List.map self#image globs in
    String.concat "," les_images

  method! vinst i =
    begin match i with
      | Call(_, e, _, _) ->
        self#incr_both_metrics incr_calls;
        (match e.enode with
          | Lval(Var vinfo, NoOffset) ->
            if consider_function vinfo then
              begin
                let update_call_map funcmap =
                  self#add_map funcmap vinfo
                    (1 + try VInfoMap.find vinfo !funcmap with Not_found-> 0)
                in
                if vinfo.vdefined
                then update_call_map fundef_calls
                else update_call_map fundecl_calls
              end
          | _ -> ());
      | Set _ -> self#incr_both_metrics incr_assigns;
      | _ -> ()
    end;
    Cil.DoChildren

end


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
    let metrics = visitor#get_metrics in
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
        @{<span>Potential entry point(s)@} (%d):@ <br/>@ \
        @[&nbsp; %a@]@ <br/>@ <br/>@ \
        @}@]"
      (VInfoMap.cardinal cil_visitor#fundef_calls)
      (Metrics_base.pretty_set VInfoMap.iter) cil_visitor#fundef_calls
      (VInfoMap.cardinal cil_visitor#fundecl_calls)
      (Metrics_base.pretty_set VInfoMap.iter) cil_visitor#fundecl_calls
      (Metrics_base.number_entry_points VInfoMap.fold
         cil_visitor#fundef_calls)
      (Metrics_base.pretty_entry_points VInfoMap.iter)
      cil_visitor#fundef_calls
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

let pp_funinfo fmt cil_visitor =
  let function_definitions = VInfoMap.to_varinfo_map cil_visitor#fundef_calls in
  let function_declarations = VInfoMap.to_varinfo_map cil_visitor#fundecl_calls
  in
  let nfundef = Metrics_base.map_cardinal_varinfomap function_definitions in
  let nfundecl = Metrics_base.map_cardinal_varinfomap function_declarations in
  let fundef_hdr = Format.sprintf "Defined functions (%d)" nfundef
  and fundecl_hdr = Format.sprintf "Undefined functions (%d)" nfundecl
  and entry_pts_hdr = Format.sprintf "Potential entry points (%d)"
    (Metrics_base.number_entry_points Varinfo.Map.fold function_definitions) in
  Format.fprintf fmt
    "@[<v 0>@[<v 1>%a@ @[%a@]@]@ @ \
            @[<v 1>%a@ @[%a@]@]@ @ \
            @[<v 1>%a@ @[%a@]@]@ \
     @]"
    (Metrics_base.mk_hdr 1) fundef_hdr
    (Metrics_base.pretty_set Varinfo.Map.iter) function_definitions
    (Metrics_base.mk_hdr 1) fundecl_hdr
    (Metrics_base.pretty_set Varinfo.Map.iter) function_declarations
    (Metrics_base.mk_hdr 1) entry_pts_hdr
    (Metrics_base.pretty_entry_points Varinfo.Map.iter) function_definitions
;;

let pp_with_funinfo fmt cil_visitor =
  Format.fprintf fmt "@[<v 0>%a@ %a@]"
    pp_funinfo cil_visitor
    pp_base_metrics cil_visitor#get_metrics
;;

let get_metrics () =
  let file = Ast.get () in
   (* Do as before *)
  let cil_visitor = new slocVisitor in
  Visitor.visitFramacFileSameGlobals
    (cil_visitor:>Visitor.frama_c_visitor) file;
  cil_visitor#get_metrics
;;

let compute_on_cilast () =
  let file = Ast.get () in
  (* Do as before *)
  let cil_visitor = new slocVisitor in
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

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

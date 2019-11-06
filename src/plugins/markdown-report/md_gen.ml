(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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
open Markdown

type env =
  { is_draft: bool;
    remarks: Markdown.element list Datatype.String.Map.t; }

let insert_remark_opt env anchor placeholder =
  try Datatype.String.Map.find anchor env.remarks with Not_found -> placeholder

let insert_remark env anchor = insert_remark_opt env anchor []

(* apparently, pandoc, or at least its latex output,
   does not like anchors beginning with _ *)
let sanitize_anchor s =
  if s = "" then "a"
  else if s.[0] = '_' then "a" ^ s
  else s

let all_eva_domains =
  [ "-eva-apron-box", "box domain of the Apron library";
    "-eva-apron-oct", "octagon domain of the Apron library";
    "-eva-bitwise-domain", "domain for bitwise computations";
    "-eva-equality-domain",
    "domain for storing equalities between memory locations";
    "-eva-gauges-domain",
    "gauges domain for relations between memory locations and loop counter";
    "-eva-inout-domain",
    "domain for input and output memory locations";
    "-eva-polka-equalities",
    "linear equalities domain of the Apron library";
    "-eva-polka-loose",
    "loose polyhedra domain of the Apron library";
    "-eva-polka-strict",
    "strict polyhedra domain of the Apron library";
    "-eva-sign-domain", "sign domain (useful only for demos)";
    "-eva-symbolic-locations-domain",
    "domain computing ranges of variation for symbolic locations \
     (e.g. `a[i]` when `i` is not precisely known by `Cvalue`)"
  ]

let insert_marks env anchor =
  Comment "BEGIN_REMARK"
  :: insert_remark env anchor
  @ [Comment "END_REMARK"]

let plural l s =
  match l with
  | [] | [ _ ] -> s
  | _::_::_ -> s ^ "s"

let get_eva_domains () =
  Extlib.filter_map
    (fun (x,_) -> Dynamic.Parameter.Bool.get x ())
    (fun (x,y) -> (plain "option" @ bold x), plain y)
    all_eva_domains

let section_domains env =
  let anchor = "domains" in
  let head = H3 (plain "EVA Domains", Some anchor) in
  if env.is_draft then
    head
    :: Comment "You can give more information about the choice of EVA domains"
    :: insert_marks env anchor
  else begin
    let l = get_eva_domains () in
    head
    :: Block
      (match l with
       | [] ->
         [Text
            (plain
               "Only the base domain (`Cvalue`) \
                has been used for the analysis")]
       | _ ->
         [Text
            (plain
               "In addition to the base domain (`Cvalue`), additional \
                domains have been used by EVA");
          DL l]
      )
    :: insert_remark env anchor
  end

let section_stubs env =
  let stubbed_kf =
    List.concat
      (List.map
         (fun f ->
            let filename = Filepath.Normalized.of_string f in
            Globals.FileIndex.get_functions ~declarations:false filename)
         (Mdr_params.Stubs.get ())
      )
  in
  let stubbed_kf = List.filter Kernel_function.is_definition stubbed_kf in
  let opt = Dynamic.Parameter.String.get "-eva-use-spec" () in
  (* NB: requires OCaml >= 4.04 *)
  let l = String.split_on_char ',' opt in
  let use_spec =
    Extlib.filter_map
      (* The option can include categories in Frama-C's List/Set/Map sense,
         which begins with a '@'. In particular, @default is included by
         default. Theoretically, there could also be some '-' to suppress
         the inclusion of a function
      *)
      (fun s -> String.length s <> 0 && s.[0] <> '@' && s.[0] <> '-')
      (fun s ->
         let kf = Globals.Functions.find_by_name s in
         let anchor = sanitize_anchor s in
         let content =
           if env.is_draft then insert_marks env anchor
           else
             let intro = Markdown.text @@ Markdown.format
                 "`%s` has the following specification" s in
             let funspec = Markdown.codeblock ~lang:"acsl" "%a"
                 Printer.pp_funspec (Annotations.funspec kf) in
             Block ( intro @ funspec ) :: insert_remark env anchor
         in
         H4 (code s, Some anchor) :: content)
      l
  in
  let describe_func kf =
    let name = Kernel_function.get_name kf in
    let anchor = sanitize_anchor name in
    let loc = Kernel_function.get_location kf in
    let content =
      if env.is_draft then insert_marks env anchor
      else
        let intro = Markdown.text @@ Markdown.format
            "`%s` @[<h>is defined at %a@]"
            name Cil_datatype.Location.pretty loc in
        let fundecl = Markdown.codeblock ~lang:"c" "%a"
            Printer.pp_global (GFun (Kernel_function.get_definition kf,loc)) in
        Block ( intro @ fundecl ) :: insert_remark env anchor
    in
    H4 (code name, Some anchor) :: content
  in
  let content =
    if stubbed_kf <> [] then begin
      List.map describe_func stubbed_kf
    end else []
  in
  let content = content @ use_spec in
  let content = List.concat content in
  if content = [] then
    if env.is_draft then
      [ Comment "No stubs have been used" ]
    else
      [ Block [Text (plain "No stubs have been used for this analysis")]]
  else
    content

let get_files () =
  let dir_table = Datatype.String.Hashtbl.create 17 in
  let add_entry f =
    let dir = Filename.dirname f in
    let base = Filename.basename f in
    let suf =
      try
        let i = String.rindex base '.' in
        String.sub base i (String.length base - i)
      with Not_found -> ""
    in
    let entries =
      try Datatype.String.Hashtbl.find dir_table dir
      with Not_found -> Datatype.String.Map.empty
    in
    let subentries =
      try Datatype.String.Map.find suf entries
      with Not_found -> Datatype.String.Set.empty
    in
    Datatype.String.(
      Hashtbl.replace
        dir_table dir (Map.add suf (Set.add base subentries) entries))
  in
  List.iter add_entry (Kernel.Files.get());
  let treat_subentry dir dir_files suf files l =
    let dir_files =
      List.fold_left
        (fun acc s ->
           if Filename.check_suffix s suf then Datatype.String.Set.add s acc
           else acc)
        Datatype.String.Set.empty dir_files
    in
    if Datatype.String.Set.subset dir_files files then
      (dir ^ "/*" ^ suf) :: l
    else
      Datatype.String.Set.elements files @ l
  in
  let treat_entry dir map l =
    try
      let dir_files = Array.to_list (Sys.readdir dir) in
      Datatype.String.Map.fold (treat_subentry dir dir_files) map l
    with Sys_error s ->
      Mdr_params.warning "Unable to find directory %s: %s" dir s;
      Datatype.String.Map.fold
        (fun _ s l -> Datatype.String.Set.elements s @ l) map l
  in
  Datatype.String.Hashtbl.fold treat_entry dir_table []

let gen_inputs env =
  let anchor = "c-input" in
  let prelude =
    if env.is_draft then
      Comment
        "You can add here some remarks about the set of files \
         that is considered by Frama-C"
      :: insert_marks env anchor
    else
      insert_remark env anchor
  in
  H2 (plain "Input files", Some anchor)
  :: prelude
  @ [
    Block [
      Text
        (plain "The C source files (not including the headers `.h` files)" @
         plain "that have been considered during the analysis \
                are the following:"
        );
      UL (List.map (fun x -> text @@ code x) (get_files()));
    ]]

let gen_config env =
  let anchor = "options" in
  let header = H2 (plain "Configuration", Some anchor) in
  let content =
    if env.is_draft then
      Comment
        "You can add here some remarks about the options used for the analysis"
      :: insert_marks env anchor
    else begin
      let placeholder = [
        Block [
          Text
            (plain "The options that have been used for this analysis \
                    are the following.")]]
      in insert_remark_opt env anchor placeholder
    end
  in
  header :: content

let gen_context env =
  let context =
    let anchor = "intro" in
    let header = H1 (plain "Introduction", Some anchor) in
    if env.is_draft then
      header
      :: Comment "You can add here some overall introduction to the analysis"
      :: insert_marks env anchor
    else begin
      match insert_remark env anchor with
      | [] -> []
      | (_::_) as l -> header :: l
    end
  in
  context @
  H1 (plain "Context of the analysis", Some "context")
  :: gen_inputs env
  @ gen_config env
  @ section_domains env
  @ H3 (plain "Stubbed Functions", Some "stubs")
    :: (
      if env.is_draft then
        Comment
          "You can add here general comments about the stubs that have been used"
        :: insert_marks env "stubs"
      else insert_remark env "stubs")
  @ section_stubs env

let gen_coverage env =
  let anchor = "coverage" in
  let header = H1 (plain "Coverage", Some anchor) in
  let content = Eva_coverage.md_gen () in
  let content =
    if env.is_draft then
      content @
      Comment "You can comment on the coverage obtained by EVA"
      :: insert_marks env anchor
    else
      content @ insert_remark env anchor
  in
  header :: content

let string_of_pos pos = Format.asprintf "%a" Filepath.pp_pos pos

let string_of_pos_opt =
  function
  | None -> "Global"
  | Some pos -> string_of_pos pos

let string_of_loc (l1, _) = string_of_pos l1

let make_events_table print_kind caption events =
  let open Log in
  let caption = Some caption in
  let header =
    [
      plain "Location", Left;
      plain "Description", Left;
    ]
  in
  let header =
    if print_kind then (plain "Kind", Center) :: header else header
  in
  let kind = function
    | Result -> "Result"
    | Feedback -> "Feedback"
    | Debug -> "Debug"
    | Warning -> "Warning"
    | Error -> "User error"
    | Failure -> "Internal error"
  in
  let treat_event { evt_kind; evt_plugin; evt_source; evt_message } =
    let evt_message =
      Str.global_replace (Str.regexp_string "\n") " " evt_message
    in
    let line =
      [ plain (string_of_pos_opt evt_source);
        format "`%s` (emitted by `%s`)" evt_message evt_plugin ]
    in
    if print_kind then plain (kind evt_kind) :: line else line
  in
  let content = List.fold_left (fun l evt -> treat_event evt :: l) [] events in
  Table { caption; header; content }

let make_errors_table errs =
  make_events_table true
    (plain (plural errs "Error" ^  " reported by Frama-C")) errs

let make_warnings_table warnings =
  make_events_table
    false (plain (plural warnings "Warning" ^ " reported by Frama-C")) warnings

let section_event is_err env nb event =
  let open Log in
  let title =
    Format.asprintf "@[<h>%s %d (%s)@]"
      (if is_err then "Error" else "Warning")
      nb
      (string_of_pos_opt event.evt_source)
  in
  let lab =
    Format.asprintf "@[<h>%s-%d@]" (if is_err then "err" else "warn") nb
  in
  let content =
    if env.is_draft then
      insert_marks env lab
    else insert_remark env lab
  in
  H2 (plain title, Some lab)
  :: Block (
    (text @@ plain "Message:") @
    codeblock "[%s] %s" event.evt_plugin event.evt_message
  )
  :: content

let make_events_list is_err env l =
  List.concat (List.mapi (section_event is_err env) l)

let make_errors_list = make_events_list true

let make_warnings_list = make_events_list false

let gen_section_warnings env =
  let open Log in
  Messages.reset_once_flag ();
  let errs = ref [] in
  let warnings = ref [] in
  let add_event evt =
    match evt.evt_kind with
    | Error | Failure -> errs:= evt :: !errs
    | Warning -> warnings := evt :: !warnings
    | _ -> ()
  in
  Messages.iter add_event;
  let errs = !errs in
  let warnings = !warnings in
  let error_section =
    if Messages.nb_errors () <> 0 then begin
      (* Failure are supposed to stop the analyses right away, so that no
         report will be generated. On the other hand, Error messages can be
         triggered without stopping everything. Applying the same treatment
         to a Failure catched by an evil plugin cannot hurt.
      *)
      let prelude =
        if env.is_draft then
          [ Comment "you can comment on each individual error" ]
        else
          [
            Block ( text @@ glue [
                bold "Important warning:";
                plain "Frama-C did not complete its execution ";
                plain "successfully. Analysis results may be inaccurate.";
                plain ((plural errs "The error") ^ " listed below must be");
                plain "fixed first before examining other ";
                plain "warnings and alarms."
              ] ) ;
            make_errors_table errs
          ]
      in
      H1 (plain "Errors in the analyzer", Some "errors")
      :: prelude
      @ make_errors_list env (List.rev errs)
    end else []
  in
  if Messages.nb_warnings () <> 0 then begin
    let prelude =
      if env.is_draft then
        [Comment "you can comment on each individual error"]
      else
        [Block (
            (text @@ glue [
                plain ("The table below lists the " ^ plural warnings "warning");
                plain "that have been emitted by the analyzer.";
                plain "They might put additional assumptions on the relevance";
                plain "of the analysis results and must be reviewed carefully";
              ]) @
            (text @@ glue [
                plain "Note that this does not take into account emitted alarms:";
                plain "they are reported in";
                link ~text:(plain "the next section") ~name:"alarms" ()
              ])
          );
         make_warnings_table warnings ]
    in
    error_section @
    H1 (plain "Warnings", Some "warnings")
    :: prelude
    @ make_warnings_list env (List.rev warnings)
  end else error_section

let gen_section_alarms env =
  let treat_alarm e kf s ~rank:_ alarm annot (i, sec, content) =
    let label = "Alarm-" ^ string_of_int i in
    let link = link ~text:(format "%d" i) ~name:label () in
    let kind = code @@ Alarms.get_name alarm in
    let func = code @@ Kernel_function.get_name kf in
    let loc = string_of_loc @@ Cil_datatype.Stmt.loc s in
    let loc_text = plain loc in
    let emitter = code (Emitter.get_name e) in
    let descr = codeblock ~lang:"acsl" "%a" Printer.pp_code_annotation annot in
    let sec_title = format "Alarm %d at %s" i loc in
    let sec_content =
      if env.is_draft then
        Block descr :: insert_marks env label
      else
        Block
          ( (text @@ glue [
                plain "The following ACSL assertion must hold to avoid" ;
                plain (Alarms.get_description alarm |> String.lowercase_ascii) ;
                format "(undefined behavior)."
              ])
            @ descr )
        :: insert_remark env label
    in
    (i+1,
     sec @ H2 (sec_title, Some label) :: sec_content,
     [ link; kind; emitter; func; loc_text ] :: content)
  in
  let _,sections, content = Alarms.fold treat_alarm (0,[],[]) in
  let content = List.rev content in
  match content with
  | [] ->
    let anchor = "alarms" in
    let text_content =
      if env.is_draft then
        Comment "No alarm!" :: insert_marks env anchor
      else
        Block (text @@ glue [
            bold "No alarm"; plain "was found during the analysis";
            plain "Any execution starting from";
            code (Kernel.MainFunction.get_function_name ());
            plain "in a context matching the one used for the analysis";
            plain "will be immune from any undefined behavior."
          ])
        :: insert_remark env anchor
    in
    H1 (plain "Results of the analysis", Some anchor) :: text_content
  | _ :: l ->
    let alarm = if l = [] then "alarm" else "alarms" in
    let caption =
      Some (plain (String.capitalize_ascii alarm ^ " emitted by the analysis"))
    in
    let header =
      [ plain "No", Center;
        plain "Kind", Center;
        plain "Emitter", Center;
        plain "Function", Left;
        plain "Location", Left;
      ]
    in
    let text_content =
      if env.is_draft then begin
        sections
      end else begin
        Block (text @@ glue [
            plain ("The table below lists the " ^ alarm);
            plain "that have been emitted during the analysis.";
            plain "Any execution starting from";
            code (Kernel.MainFunction.get_function_name());
            plain "in a context matching the one used for the analysis";
            plain "will be immune from any other undefined behavior.";
            plain "More information on each individual alarm is";
            plain "given in the remainder of this section"
          ]) ::
        Table { content; caption; header } ::
        sections
      end
    in
    H1 (plain "Results of the analysis", Some "alarms") :: text_content

let gen_section_callgraph env =
  let f = Mdr_params.FlameGraph.get () in
  if f = "" then []
  else begin
    let anchor = "flamegraph" in
    let content =
      if env.is_draft then
        Comment
          "A flamegraph provides a visualization of the functions and \
           callstacks whose analysis is the most costly."
        :: insert_marks env anchor
      else
        par (
          plain "The image below shows the flamegraph (" @
          url "http://www.brendangregg.com/flamegraphs.html" @
          plain ") for the chosen entry point."
        )
        @ par (image ~alt:"Flamegraph visualization." ~file:f)
        @ insert_remark env anchor
    in
    H1 (plain "Flamegraph", Some anchor) :: content
  end

let gen_section_postlude env =
  let anchor = "conclusion" in
  let header = H1 (plain "Conclusion", Some anchor) in
  if env.is_draft then
    header ::
    Comment "You can put here some concluding remarks"
    :: insert_marks env anchor
  else begin
    match insert_remark env anchor with
    | [] -> []
    | (_::_) as l -> header :: l
  end

let gen_alarms env =
  gen_section_warnings env @
  gen_section_alarms env @
  gen_section_callgraph env @
  gen_section_postlude env

let mk_remarks is_draft =
  let f = Mdr_params.Remarks.get () in
  if f <> "" then Parse_remarks.get_remarks f
  else if is_draft then begin
    let f = Mdr_params.Output.get() in
    if Sys.file_exists f then begin
      Mdr_params.feedback
        "Re-using pre-existing remarks in draft file %s" f;
      Parse_remarks.get_remarks f
    end else Datatype.String.Map.empty
  end else  Datatype.String.Map.empty

let gen_report ~draft:is_draft () =
  let remarks = mk_remarks is_draft in
  let env = { remarks; is_draft } in
  let context = gen_context env in
  let coverage = gen_coverage env in
  let alarms = gen_alarms env in
  let title = Mdr_params.Title.get () in
  let title =
    if title = "" then begin
      if is_draft then
        plain "Draft report"
      else
        plain "Frama-C Analysis Report"
    end else plain title
  in
  let authors = List.map (fun x -> plain x) (Mdr_params.Authors.get ()) in
  let date = match Mdr_params.Date.get () with
    | "" -> None
    | s -> Some (plain s) in
  let elements = context @ coverage @ alarms in
  let elements =
    if is_draft then
      Comment
        "This file contains additional remarks that will be added to \
         automatically generated content by Frama-C's Markdown-report plugin. \
         For any section of the document, you can write pandoc markdown \
         content between the BEGIN and END comments. In addition, the plug-in \
         will consider any \\<!-- INCLUDE file.md --\\> comment (without backslashes) \
         as a directive to include the content of file.md in the corresponding \
         section. \
         Please don't alter the structure \
         of the document as it is used by the plugin to associate content to \
         the relevant section."
      :: elements
    else elements
  in
  let elements =
    Raw [ "\\let\\underscore\\_" ;
          "\\renewcommand{\\_}{\\discretionary{\\underscore}{}{\\underscore}}"]
    :: elements
  in
  let doc = Markdown.pandoc ~title ~authors ?date elements in
  let file = Mdr_params.Output.get() in
  try
    Command.print_file file (fun fmt -> Markdown.pp_pandoc fmt doc) ;
    Mdr_params.result "Report %s generated" file
  with Sys_error s ->
    Mdr_params.warning
      "Unable to open %s for writing (%s). No report generated" file s

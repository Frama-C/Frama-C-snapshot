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

open Sarif

let frama_c_sarif =
  let name = "frama-c" in
  let version = Config.version_and_codename in
  let semanticVersion = Config.version in
  let fullName = name ^ "-" ^ version in
  let downloadUri = "https://frama-c.com/download.html" in
  Tool.create
    ~name ~version ~semanticVersion ~fullName ~downloadUri ()


let get_remarks () =
  let f = Mdr_params.Remarks.get () in
  if f <> "" then Parse_remarks.get_remarks f
  else Datatype.String.Map.empty

let get_remark remarks label =
  match Datatype.String.Map.find_opt label remarks with
  | None -> []
  | Some l -> l

let command_line () = Array.to_list Sys.argv

module Analysis_cmdline =
  State_builder.Ref(Datatype.List(Datatype.String))
    (struct
      let name = "Sarif_gen.Analysis_cmdline"
      let dependencies = []
      let default = command_line
    end)

let gen_invocation () =
  let cl = Analysis_cmdline.get () in
  let commandLine = String.concat " " cl in
  let arguments = List.tl cl in
  Invocation.create ~commandLine ~arguments ()

let gen_remark alarm =
  let open Markdown in
  [ Block
      [ Text
          (plain
             (Printf.sprintf "This alarms represents a potential %s."
                (Alarms.get_description alarm)
             )
          )
      ]
  ]

let level_of_status =
  let open Property_status.Feedback in
  let open Sarif.Result_level in
  function
  | Never_tried -> notApplicable
  | Considered_valid | Valid | Valid_under_hyp | Valid_but_dead -> pass
  | Unknown | Unknown_but_dead -> warning
  | Invalid | Invalid_under_hyp | Invalid_but_dead -> error
  | Inconsistent -> note

let make_message alarm annot remark =
  let open Markdown in
  let name = Alarms.get_name alarm in
  let text = name ^ "." in
  let kind = plain (name ^ ":") in
  let descr = codeblock ~lang:"acsl" "%a" Printer.pp_code_annotation annot in
  let summary = Block (Text kind :: descr) in
  let markdown =
    match remark with
    | [] -> summary :: gen_remark alarm
    | _ -> summary :: remark
  in
  let richText =
    String.trim
      (Format.asprintf "@[%a@]" (Markdown.pp_elements ~page:"") markdown)
  in
  Message.create ~text ~richText ()

let gen_results remarks =
  let treat_alarm _e kf s ~rank:_ alarm annot (i, rules, content) =
    let prop = Property.ip_of_code_annot_single kf s annot in
    let ruleId = Alarms.get_name alarm in
    let rules =
      Datatype.String.Map.add ruleId (Alarms.get_description alarm) rules
    in
    let label = "Alarm-" ^ string_of_int i in
    let level = level_of_status (Property_status.Feedback.get prop) in
    let remark = get_remark remarks label in
    let message = make_message alarm annot remark in
    let locations = [ Location.of_loc (Cil_datatype.Stmt.loc s) ] in
    let res =
      Sarif_result.create ~level ~ruleId ~message ~locations ()
    in
    (i+1, rules, res :: content)
  in
  let _, rules, content =
    Alarms.fold treat_alarm (0, Datatype.String.Map.empty,[])
  in
  rules, List.rev content

let is_alarm = function
  | Property.(IPCodeAnnot { ica_ca }) -> Extlib.has_some (Alarms.find ica_ca)
  | _ -> false

let make_ip_message ip =
  let text = Format.asprintf "@[%a.@]" Property.short_pretty ip in
  Message.plain_text ~text ()

let gen_status ip =
  let status = Property_status.Feedback.get ip in
  let level = level_of_status status in
  let locations = [ Location.of_loc (Property.location ip) ] in
  let message = make_ip_message ip in
  Sarif_result.create ~level ~locations ~message ()

let gen_statuses () =
  let f ip content =
    if is_alarm ip then content else (gen_status ip) :: content
  in
  List.rev (Property_status.fold f [])

let gen_files () =
  let add_src_file f =
    let key = Filename.chop_extension (Filename.basename f) in
    let fileLocation = FileLocation.create ~uri:(Filepath.normalize f) () in
    let roles = [ Role.analysisTarget ] in
    let mimeType = "text/x-csrc" in
    key, File.create ~fileLocation ~roles ~mimeType ()
  in
  List.map add_src_file (Kernel.Files.get ())

let add_rule id desc l =
  let text = desc ^ "." in
  let shortDescription = Message.plain_text ~text () in
  let rule = Rule.create ~id ~shortDescription () in
  (id, rule) :: l

let make_rule_dictionary rules = Datatype.String.Map.fold add_rule rules []

let gen_run remarks =
  let tool = frama_c_sarif in
  let invocations = [gen_invocation ()] in
  let rules, results = gen_results remarks in
  let user_annot_results = gen_statuses () in
  let rules =
    match user_annot_results with
    | [] -> rules
    | _ ->
      Datatype.String.Map.add
        "user-spec" "User written ACSL specification" rules
  in
  let rules = make_rule_dictionary rules in
  let resources = Resources.create ~rules () in
  let results = results @ user_annot_results in
  let files = gen_files () in
  Run.create ~tool ~invocations ~results ~resources ~files ()

let generate () =
  let remarks = get_remarks () in
  let runs = [ gen_run remarks ] in
  let json = Schema.create ~runs () |> Schema.to_yojson in
  let file = Mdr_params.Output.get () in
  if file = "" then
    Log.print_on_output (fun fmt -> Yojson.Safe.pretty_print fmt json)
  else
    try
      Command.write_file file
        (fun out -> Yojson.Safe.pretty_to_channel ~std:true out json) ;
      Mdr_params.result "Report %s generated" file
    with Sys_error s ->
      Mdr_params.abort "Unable to generate %s (%s)" file s

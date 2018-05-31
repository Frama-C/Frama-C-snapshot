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

module R = Report_parameters
module T = Transitioning

type action = SKIP | INFO | ERROR | REVIEW 

let action s =
  match T.String.uppercase_ascii s with
  | "INFO" -> INFO
  | "ERROR" -> ERROR
  | "REVIEW" -> REVIEW
  | "SKIP" | "NONE" -> SKIP
  | a -> R.abort "Invalid action (%S)" a

let string_of_action = function
  | SKIP -> "SKIP"
  | INFO -> "INFO"
  | REVIEW -> "REVIEW"
  | ERROR -> "ERROR"

let pp_action fmt a = Format.pp_print_string fmt (string_of_action a)
let pp_source fmt =
  function None -> () | Some lex -> Cil_datatype.Position.pretty fmt lex

type rule = {
  r_id: string ;
  r_plugin: string ;
  r_regexp: Str.regexp ;
  r_action: action ;
  r_title: string ;
  r_descr: string ;
}

type rules = {
  rs_rules : (string,rule Queue.t) Hashtbl.t ; (* indexed by plug-in *)
  rs_action : (unit -> string) ;
  rs_name : string ;
}

let warnings = {
  rs_rules  = Hashtbl.create 0 ;
  rs_action = R.Warning.get ;
  rs_name = "warning" ;
}

let errors = {
  rs_rules  = Hashtbl.create 0 ;
  rs_action = R.Error.get ;
  rs_name = "error" ;
}

type props = {
  ps_name : string ;
  ps_rules : rule Queue.t ;
  ps_action : (unit -> string) ;
}

let props ps_name ps_action =
  { ps_rules = Queue.create () ; ps_action ; ps_name }

let untried = props "untried" R.UntriedStatus.get
let unknown = props "unknown" R.UnknownStatus.get
let invalid = props "invalid" R.InvalidStatus.get

(* -------------------------------------------------------------------------- *)
(* --- Configure Rules                                                    --- *)
(* -------------------------------------------------------------------------- *)

exception WrongFormat of string

let failwith msg =
  Pretty_utils.ksfprintf (fun s -> raise (WrongFormat s)) msg

let default = `NONE , {
  r_id = "unclassified" ;
  r_plugin = "kernel" ;
  r_title = "\\0" ;
  r_descr = "\\*" ;
  r_action = REVIEW ;
  r_regexp = Str.regexp ""
}

let rule_of_regexp t0 r t value =
  if t0 <> `NONE then failwith "Duplicate regexp" ;
  t , { r with r_regexp = value |> Str.regexp }

let rule_of_fields (t,r) (field,jvalue) =
  try
    let value = Json.string jvalue in
    match field with
    | "classid" -> t,{ r with r_id = value }
    | "plugin" -> t,{ r with r_plugin = value }
    | "title" -> t,{ r with r_title = value }
    | "descr" -> t,{ r with r_descr = value }
    | "action" -> t,{ r with r_action = value |> action }
    | "error" -> rule_of_regexp t r `ERROR value
    | "warning" -> rule_of_regexp t r `WARNING value
    | "unknown" -> rule_of_regexp t r `UNKNOWN value
    | "untried" -> rule_of_regexp t r `UNTRIED value
    | "invalid" -> rule_of_regexp t r `INVALID value
    | "unproved" -> rule_of_regexp t r `UNPROVED value
    | _ -> failwith "unexpected field"
  with
  | Invalid_argument _ ->
    failwith "Unexpected value for '%s'" field
  | WrongFormat msg ->
    failwith "for '%s': %s" field msg

let get_queue env plugin =
  try Hashtbl.find env.rs_rules plugin
  with Not_found ->
    let q = Queue.create () in
    Hashtbl.add env.rs_rules plugin q ; q

let add_rule jvalue =
  try
    match jvalue with
    | Json.Assoc fields ->
      let tgt , rule = List.fold_left rule_of_fields default fields in
      let properties p =
        if rule.r_plugin <> (snd default).r_plugin then
          failwith "Unexpected 'plugin' for property-rule" ;
        p.ps_rules in
      let queues = 
        match tgt with
        | `NONE -> failwith "Missing pattern"
        | `ERROR -> [get_queue errors rule.r_plugin]
        | `WARNING -> [get_queue warnings rule.r_plugin]
        | `UNTRIED -> [properties untried]
        | `UNKNOWN -> [properties unknown]
        | `INVALID -> [properties invalid]
        | `UNPROVED -> List.map properties [untried;unknown;invalid]
      in List.iter (Queue.add rule) queues
    | _ -> failwith "Classification rule expected"
  with WrongFormat msg ->
    failwith "@[<hv 0>%s@ @[<hov 2>in: %a@]@]" msg Json.pp jvalue

let configure file =
  begin
    R.feedback "Loading '%s'" (Filepath.pretty file) ;
    try
      match Json.load_file file with
      | Json.Array values -> List.iter add_rule values
      | _ -> failwith "Array expected"
    with
    | Json.Error(file,line,msg) ->
      let source = Log.source ~file ~line in
      R.abort ~source "%s" msg
    | WrongFormat msg ->
      let source = Log.source ~file ~line:1 in
      R.abort ~source "%s" msg
    | Sys_error msg ->
      R.abort "%s" msg
    | Invalid_argument msg | Failure msg ->
      R.abort "@[<hov 2>Parsing '%s' failed@ (%s)@]"
        file msg
  end

(* -------------------------------------------------------------------------- *)
(* --- Reporting Events                                                   --- *)
(* -------------------------------------------------------------------------- *)

type event = {
  e_classified : bool ;
  e_id : string ;
  e_action : action ;
  e_title : string ;
  e_descr : string ;
  e_source : Lexing.position option ;
}

let unclassified = {
  e_classified = false ;
  e_id = "" ;
  e_action = SKIP ;
  e_title = "" ;
  e_descr = "" ;
  e_source = None ;
}

let json_of_source = function
  | None -> []
  | Some lex ->
    let file =
      if R.AbsolutePath.get ()
      then Filepath.normalize lex.Lexing.pos_fname
      else Filepath.relativize lex.Lexing.pos_fname
    in
    [
      "file" , Json.of_string file ;
      "line" , Json.of_int lex.Lexing.pos_lnum ;
    ]

let json_of_event e =
  Json.Assoc
    begin [
      "classid" , Json.of_string e.e_id ;
      "action" , Json.of_string @@ string_of_action e.e_action ;
      "title" , Json.of_string e.e_title ;
      "descr" , Json.of_string e.e_descr ;
    ] @ json_of_source e.e_source
    end

module EVENTS = Set.Make
    (struct
      type t = event
      let compare = Pervasives.compare
    end)

let events_queue = Queue.create ()
let events_pool = ref EVENTS.empty
let nb_reviews = ref 0
let nb_errors = ref 0
let nb_unclassified = ref 0

let keep = function
  | SKIP -> R.debug_atleast 2
  | INFO | REVIEW | ERROR -> true

let push_event evt =
  if not (EVENTS.mem evt !events_pool) then
    begin
      events_pool := EVENTS.add evt !events_pool ;
      if not evt.e_classified then incr nb_unclassified ;
      if evt.e_action = REVIEW then incr nb_reviews ;
      if evt.e_action = ERROR then incr nb_errors ;
      Queue.push evt events_queue ;
    end

let clear_events () =
  begin
    nb_reviews := 0 ;
    nb_errors := 0 ;
    nb_unclassified := 0 ;
    events_pool := EVENTS.empty ;
    Queue.clear events_queue ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Matching a Rule                                                    --- *)
(* -------------------------------------------------------------------------- *)

let matches ~msg r = Str.string_match r.r_regexp msg 0
let replace ~msg text =
  let buffer = Buffer.create 80 in
  let rec scan k n =
    if k < n then
      let c = text.[k] in
      if k < n-1 && text.[k] = '\\' then
        (begin
          match text.[k+1] with
          | '*' -> Buffer.add_string buffer msg
          | 'n' -> Buffer.add_char buffer '\n'
          | '0'..'9' as a ->
            let i = int_of_char a - int_of_char '0' in
            ( try Buffer.add_string buffer (Str.matched_group i msg)
              with Not_found | Invalid_argument _ -> () )
          | _ as a -> Buffer.add_char buffer a
        end ; scan (k+2) n)
      else
        ( Buffer.add_char buffer c ; scan (k+1) n )
  in scan 0 (String.length text) ; Buffer.contents buffer

exception FOUND of rule

let find queue msg =
  try
    Queue.iter
      (fun r -> if matches ~msg r then raise (FOUND r))
      queue ;
    raise Not_found
  with FOUND r -> r

(* -------------------------------------------------------------------------- *)
(* --- Monitoring                                                         --- *)
(* -------------------------------------------------------------------------- *)

let monitor ~lookup ~msg ~source ~unclassified =
  try
    let rule = lookup msg in
    if keep rule.r_action then
      let title = replace ~msg rule.r_title in
      let descr = replace ~msg rule.r_descr in
      push_event {
        e_classified = true ;
        e_id = rule.r_id ;
        e_title = title ;
        e_descr = descr ;
        e_action = rule.r_action ;
        e_source = source ;
      }
  with Not_found ->
    let event = unclassified () in
    if keep event.e_action then
      let descr = if event.e_descr = "" then msg else event.e_descr in
      push_event { event with e_descr = descr ; e_source = source }

(* -------------------------------------------------------------------------- *)
(* --- Monitoring Events                                                  --- *)
(* -------------------------------------------------------------------------- *)

let monitor_log_event (evt : Log.event) =
  let open Log in
  try
    let env =
      match evt.evt_kind with
      | Warning -> warnings
      | Error | Failure -> errors
      | Result | Feedback | Debug -> raise Exit in
    let msg = evt.evt_message in
    let source = evt.evt_source in
    let lookup msg = find (Hashtbl.find env.rs_rules evt.evt_plugin) msg in
    let unclassified () =
      let e_id =
        Printf.sprintf "%s.unclassified.%s" evt.evt_plugin env.rs_name in
      let e_title =
        Printf.sprintf "Unclassified %s (Plugin '%s')"
          (T.String.capitalize_ascii env.rs_name) evt.evt_plugin in
      let e_action = action (env.rs_action ()) in 
      { unclassified with e_id ; e_title ; e_action } in
    monitor ~lookup ~msg ~source ~unclassified
  with Exit -> ()

let hooked = ref false
let monitor_log () =
  if not !hooked then
    begin
      hooked := true ;
      R.feedback "Monitoring events" ;
      R.Rules.iter configure ;
      R.debug "Unclassified warnings: %s"
        (R.Warning.get ()) ;
      R.debug "Unclassified errors: %s"
        (R.Error.get ()) ;
      Log.add_listener
        ~kind:[Log.Warning;Log.Error;Log.Failure]
        monitor_log_event ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Report on Property Status                                          --- *)
(* -------------------------------------------------------------------------- *)

module E = Emitter.Usable_emitter
module Pset = Property.Set
module Status = Property_status.Consolidation

let status ip =
  let open Status in
  match Status.get ip with
  | Never_tried -> `UNTRIED
  | Unknown _ -> `UNKNOWN
  | Considered_valid | Valid _ 
  | Valid_but_dead _ | Unknown_but_dead _ | Invalid_but_dead _
    -> `PROVED
  | Valid_under_hyp pending
  | Invalid_under_hyp pending
    -> `PENDING pending
  | Invalid _ | Inconsistent _
    -> `INVALID

let pending f pending =
  E.Map.iter
    (fun _ m -> E.Map.iter
	(fun _ ips -> Property.Set.iter f ips) m)
    pending

let monitor_status properties ip =
  let ps = Property_names.parts_of_property ip in
  if ps = [] then () else
    let msg = Property_names.string_of_parts ps in
    let lookup = find properties.ps_rules in
    let source = Property.source ip in
    let unclassified () =
      let e_id = "unclassified." ^ properties.ps_name in
      let e_title = msg in
      let e_action = properties.ps_action () |> action in
      let e_descr = T.String.capitalize_ascii properties.ps_name ^ " status" in
      { unclassified with e_id ; e_action ; e_title ; e_descr }
    in monitor ~lookup ~msg ~source ~unclassified

let monitor_property pool push ip =
  begin
    pool := Pset.add ip !pool ;
    match status ip with
    | `PENDING ips -> pending push ips
    | `PROVED -> ()
    | `UNTRIED -> monitor_status untried ip
    | `UNKNOWN -> monitor_status unknown ip
    | `INVALID -> monitor_status invalid ip
  end
    
let consolidate () =
  let pool = ref Pset.empty in
  let queue = Queue.create () in
  let push ip = if not (Pset.mem ip !pool) then Queue.push ip queue in
  begin
    Scan.source_properties (monitor_property pool push) ;
    while not (Queue.is_empty queue) do
      let ip = Queue.take queue in
      if not (Pset.mem ip !pool) then
        monitor_property pool push ip
    done
  end
  
(* -------------------------------------------------------------------------- *)
(* --- Run Classification                                                 --- *)
(* -------------------------------------------------------------------------- *)

let report_fmt ~long fmt =
  begin
    let bar = String.make 80 '-' in
    Queue.iter
      (fun e ->
         if long then Format.fprintf fmt "%s@." bar ;
         Format.fprintf fmt "@[<hv 0>@[<hov 0>" ;
         if long && e.e_source <> None then
           ( pp_source fmt e.e_source ; Format.pp_print_space fmt () ) ;
         Format.fprintf fmt "[%a:%s]@ %s@]"
           pp_action e.e_action e.e_id e.e_title ;
         if long && e.e_descr <> "" && e.e_descr <> e.e_title then
           ( Format.pp_print_space fmt () ;
             Format.pp_print_string fmt e.e_descr ) ;
         Format.fprintf fmt "@]@." ;
      ) events_queue ;
    if long then Format.fprintf fmt "%s@." bar
  end

let report_console () =
  let long = R.Stdout.get () || R.verbose_atleast 2 in
  Log.print_on_output (report_fmt ~long)

let report_stderr () =
  report_fmt ~long:true Format.err_formatter

let report_dump fmt =
  begin
    Format.fprintf fmt "[" ;
    let sep = ref false in
    Queue.iter
      (fun e ->
         if !sep then Format.fprintf fmt "," ;
         Format.fprintf fmt "@.  @[<hov 2>%a@]" Json.pp (json_of_event e) ;
         sep := true ;
      ) events_queue ;
    Format.fprintf fmt "@\n]@." ;
  end

let report_output file =
  R.feedback "Output %s@." file ;
  Command.print_file file report_dump

let report_number name nb opt =
  if nb > 0 then R.feedback "%s%4d" name nb ;
  let file = opt () in
  if file <> "" then
    let out = open_out file in
    output_string out (string_of_int nb) ;
    flush out ; close_out out

let classify () =
  begin
    R.feedback "Classification" ;
    if R.Status.get () then consolidate () ;
    if R.Stderr.get () then report_stderr () ;
    if R.Stdout.get () || R.verbose_atleast 2 ||
       (not (R.Stderr.get ()) &&
        not (R.Output.is_set ()))
    then report_console () ;
    let file = R.Output.get () in
    if file <> "" then report_output file ;
    report_number "Reviews     : " !nb_reviews R.OutputReviews.get ;
    report_number "Errors      : " !nb_errors R.OutputErrors.get ;
    report_number "Unclassified: " !nb_unclassified R.OutputUnclassified.get ;
    if !nb_errors > 0 && R.Exit.get () then
      R.abort "Classified errors found" ;
    if not !Config.is_gui then clear_events () ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Plug-in Registration                                               --- *)
(* -------------------------------------------------------------------------- *)

let classify, _ =
  State_builder.apply_once
    "Report.classify_once"
    [ R.Classify.self;
      R.Output.self;
      Property_status.self;
      Ast.self;
    ] classify

let register () =
  if R.Rules.is_set () || 
     R.Warning.is_set () ||
     R.Error.is_set ()
  then monitor_log ()

let main () =
  if R.Classify.get () then classify ()
  
let () =
  begin
    Cmdline.run_after_configuring_stage register ;
    Db.Main.extend main ;
  end

(* -------------------------------------------------------------------------- *)

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

open Property
open Cil_types


let pwd = Sys.getcwd ()

(* Prefixes and suffixes to remove on directory names *)
let prefixes = [pwd ^ Filename.dir_sep; pwd]

let string_del_prefix s del =
  let ld = String.length del in
  let ls = String.length s in
  if ls >= ld then
    let pre = String.sub s 0 ld in
    if pre = del then String.sub s ld (ls-ld)
    else s
  else s

let clean_dir s =
  let no_prefix = List.fold_left string_del_prefix s prefixes in
  if no_prefix = "" then "." else no_prefix

(* Split the location into 'dir,file,line number,char number' in this order *)
let split_loc loc =
  let open Lexing in
  let file = Filepath.pretty loc.pos_fname in
  let dir = Filename.dirname file in
  let dir = clean_dir dir in
  let file = Filename.basename file in
  dir, file, loc.pos_lnum, loc.pos_cnum

let status_kind prop =
  let open Property_status.Consolidation in
  match get prop with
  | Never_tried -> "Ignored"
  | Considered_valid  -> "Considered valid"
  | Valid _ -> "Valid"
  | Valid_under_hyp _ -> "Partially proven"
  | Unknown _ -> "Unknown"
  | Invalid _ -> "Invalid"
  | Invalid_under_hyp _ -> "Invalid or unreachable"
  | Invalid_but_dead _ | Valid_but_dead _ | Unknown_but_dead _ -> "Dead"
  | Inconsistent _ -> "Inconsistent"

let (|>) = Extlib.swap

(* For properties that we want to skip *)
exception Skip

let to_string_no_break pp =
  let b = Buffer.create 20 in
  let fmt = Format.formatter_of_buffer b in
  Format.pp_set_margin fmt 1000000;
  pp fmt;
  Format.pp_print_flush fmt ();
  Buffer.contents b

let to_string ip =
  let status = status_kind ip in
  let loc = Property.location ip in
  let default ?(pp = (Property.pretty |> ip)) kf kind =
    let loc =
      if Cil_datatype.Location.(equal loc unknown) then
        Kernel_function.get_location kf
      else loc
    in
    let txt = to_string_no_break pp in
    let loc = split_loc (fst loc) in
    (loc, Kernel_function.get_name kf, kind, status, txt)
  in
  begin
    match ip with
    | IPCodeAnnot (kf, _stmt, ca) -> begin
      let kind, pp = match ca.annot_content with
        | AAssert (_, ({content = p; name} as named)) -> begin
          match Alarms.find ca with
          | Some alarm ->
            (Alarms.get_name alarm), (Printer.pp_predicate |> p)
          | None ->
            if List.exists ((=) "missing_return") name then
              "missing_return", (Printer.pp_predicate_named |> named)
            else
              "user assertion", (Printer.pp_predicate |> p)
        end
        | AInvariant (_, _, {content = p}) ->
          "loop invariant", (Printer.pp_predicate |> p)
        | _ ->
          Report_parameters.warning ~source:(fst loc)
            "ignoring annotation '%a' in csv export"
            Printer.pp_code_annotation ca;
          "", (fun _ -> ())
      in
      if kind <> "" then default ~pp kf kind else raise Skip
    end
    | IPPredicate (pk, kf, _, p) -> begin
      let kind = match pk with
        | PKRequires _ -> "precondition"
        | PKAssumes _ -> "behavior assumption" (*should never happen *)
        | PKEnsures _ -> "postcondition"
        | PKTerminates -> "termination clause"
      in
      let pp = Printer.pp_identified_predicate |> p in
      default ~pp kf kind
    end
    | IPAssigns (kf, _, _, _) -> default kf "assigns clause" 
    | IPFrom (kf, _, _, _) -> default kf "from clause"

    | IPComplete (kf, _, _) -> default kf "complete behaviors"
    | IPDisjoint (kf, _, _) -> default kf "disjoint behaviors"

    (* Add new cases if new IPPropertyInstance are added *)
    | IPPropertyInstance (Some kf, Kstmt _stmt,
                          IPPredicate (PKRequires _b, kf', Kglobal,p)) ->
      let kind = "precondition of " ^ Kernel_function.get_name kf' in
      let pp = Printer.pp_identified_predicate |> p in
      default ~pp kf kind
    | IPReachable _ -> raise Skip (* Ignore: unimportant except for dead
                                     statuses,
                             that are listed elsewhere *)
    | IPAxiom _ | IPAxiomatic _ -> raise Skip (* Ignore: no status *)
    | IPBehavior _ -> raise Skip (* Ignore: redundant with its contents *)
    | _ ->
      Report_parameters.warning ~source:(fst loc)
        "ignoring property '%a' in csv export" Property.pretty ip;
      raise Skip
  end
;;

(* Compute the lines to export as a .csv, then sorts them *)
let lines () =
  let do_one_ip ip l =
    if Scan.report_ip ip then
      try to_string ip :: l
      with Skip -> l
    else l
  in
  let l = Property_status.fold do_one_ip [] in
  (* This [sort] removes fully identical lines, including identical alarms
     emitted on statements copied through loop unrolling. This is the desired
     semantics for now. However, since we compare entire locations, textually
     identical lines that refer to different expressions are kept separate *)
  Extlib.sort_unique Pervasives.compare l

let output file =
  let ch = open_out file in
  let fmt = Format.formatter_of_out_channel ch in
  Format.pp_set_margin fmt 1000000;
  Format.fprintf fmt "@[<v>";
  Format.fprintf fmt
    "@[directory\tfile\tline\tfunction\tproperty kind\tstatus\tproperty@]@ ";
  let pp ((dir, file, lnum, _), kf, kind, status, txt) =
    Format.fprintf fmt "@[<h>%s\t%s\t%d\t%s\t%s\t%s\t%s@]@ "
      dir file lnum kf kind status txt;
  in
  List.iter pp (lines ());
  Format.fprintf fmt "@]%!"


(** Registration of non-free options *)

let print_csv =
  Dynamic.register
    ~plugin:"Report"
    ~journalize:true
    "print_csv"
    (Datatype.func Datatype.string Datatype.unit)
    output

let print_csv_once () =
  let file = Report_parameters.CSVFile.get () in
  Report_parameters.feedback "Dumping properties in '%s'" file;
  print_csv file

let print_csv, _ =
  State_builder.apply_once
    "Report.print_csv_once"
    [ Report_parameters.PrintProperties.self; 
      Report_parameters.Specialized.self;
      Report_parameters.CSVFile.self;
      Property_status.self ]
    print_csv_once

let main () = if Report_parameters.CSVFile.get () <> "" then print_csv ()

let () = Db.Main.extend main


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

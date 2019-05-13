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

(* Split the location into 'dir,file,line number,char number' in this order *)
let split_loc loc =
  let file =
    Filepath.Normalized.to_pretty_string loc.Filepath.pos_path
  in
  let dir = Filename.dirname file in
  let dir = Filepath.relativize dir in
  let file = Filename.basename file in
  dir, file, loc.Filepath.pos_lnum, loc.Filepath.pos_cnum

(* For properties that we want to skip *)
exception Skip

let kf_of_property ip =
  match Property.get_kf ip with
  | Some kf -> kf
  | None -> fst (Globals.entry_point ())

let to_string ip =
  let status = Description.status_feedback (Property_status.Feedback.get ip) in
  let loc = Property.location ip in
  match Description.property_kind_and_node ip with
  | None -> raise Skip
  | Some (kind, txt) ->
    let kf = kf_of_property ip in
    let loc =
      if Cil_datatype.Location.(equal loc unknown) then
        Kernel_function.get_location kf
      else loc
    in
    let loc = split_loc (fst loc) in
    (loc, Kernel_function.get_name kf, kind, status, txt)

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
  Extlib.sort_unique Transitioning.Stdlib.compare l

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

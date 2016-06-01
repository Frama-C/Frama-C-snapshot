(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Plug-in Implementation                                             --- *)
(* -------------------------------------------------------------------------- *)

let print () = 
  Report_parameters.feedback "Computing properties status..." ;
  Log.print_on_output (fun fmt -> Scan.iter (Dump.create fmt))
    
let print =
  Dynamic.register
    ~plugin:"Report"
    ~journalize:true
    "print"
    (Datatype.func Datatype.unit Datatype.unit)
    print

let print, _ =
  State_builder.apply_once
    "Report.print_once"
    [ Report_parameters.Enabled.self; (* reprint if we explicitly ask for *)
      Report_parameters.PrintProperties.self; 
      Report_parameters.Specialized.self;
      Property_status.self ]
    print

let main () = if Report_parameters.Enabled.get () then print ()

let () =
  Db.Report.print := print;
  Db.Main.extend main;

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

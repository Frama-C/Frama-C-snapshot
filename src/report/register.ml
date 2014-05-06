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
      Property_status.self ]
    print

let main () = if Report_parameters.Enabled.get () then print ()

let () =
  Db.Report.print := print;
  Db.Main.extend main;

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

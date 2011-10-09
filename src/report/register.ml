(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

open Property_status

(* -------------------------------------------------------------------------- *)
(* --- Plug-in Implementation                                             --- *)
(* -------------------------------------------------------------------------- *)

module Self = Plugin.Register
  (struct
     let name = "report"
     let shortname = "report"
     let help = "Properties Status Report (experimental)"
   end)

module Enabled =
  Self.Action(struct
                let option_name = "-report"
                let help = "display a summary of properties status"
                let kind = `Tuning
              end)

let bar = String.make 60 '-'

let print () = 
  Self.feedback "Computing properties status..." ;
  Log.print_on_output (fun fmt -> Scan.iter (Dump.create fmt))
    
let print =
  Dynamic.register
    ~plugin:"Report"
    ~journalize:true
    "print"
    (Datatype.func Datatype.unit Datatype.unit)
    print

let main () =
  if Enabled.get () then
    begin
      print () ;
      Enabled.clear () ; (* Hack for not printing the report after -then *)
    end

let () =
  begin
    Db.Report.print := print ;
    Db.Main.extend main ;
  end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

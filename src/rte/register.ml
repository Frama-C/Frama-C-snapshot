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

open Rte_parameters

let start_msg () =
  feedback ~level:2 "====== RTE ANNOTATIONS GENERATION ======"

let end_msg () =
  feedback ~level:2 "====== RTE ANNOTATIONS COMPUTED ======"

let main () =
  (* reset "rte generated"/"called precond generated" properties for all
     functions *)
  if Enabled.get () then begin
    start_msg () ;
    !Db.RteGen.compute () ;
    if Print.get () then begin
      let fmt = Format.formatter_of_out_channel stdout in
      File.pretty_ast ~fmt ()
    end;
    end_msg ()
  end

let () = Db.Main.extend main

(*
  Local Variables:
  compile-command: "make -C ../.."
  End:
 *)

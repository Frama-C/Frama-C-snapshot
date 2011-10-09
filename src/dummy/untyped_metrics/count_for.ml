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

(** Counting for loops on the untyped AST.
*)


module Self =
  Plugin.Register
    (struct
       let name = "For loops counter"
       let shortname = "for_counter"
       let module_name = "Untyped_metrics.Count_for.Self"
       let help = "For counter on untyped AST"
       let is_dynamic = true
     end)

module Enabled = Self.False
  (struct
     let module_name = "Count_for.Enabled"
     let option_name = "-count-for"
     let help = "count the for loops"
     let kind = `Tuning
   end)

open Cabs

class count_for =
object inherit Cabsvisit.nopCabsVisitor as super
       val mutable counted_for = 0
       method counted_for = counted_for
       method vstmt s =
         begin match s.stmt_node with
           | FOR _ -> counted_for <- counted_for + 1
           | _ -> ()
         end;
         super#vstmt s
end

let count_for (fname,_ as file) =
  let counter = new count_for in
  ignore (Cabsvisit.visitCabsFile (counter:>Cabsvisit.cabsVisitor) file);
  fname,counter#counted_for

let print_stat (fname,n) = Format.printf "%s: %d@." fname n

let startup _ =
  if Enabled.get () then begin
    let untyped_files = Ast.UntypedFiles.get () in

    let stats = List.map count_for untyped_files in
      List.iter print_stat stats
  end


let () =
  Db.Main.extend startup

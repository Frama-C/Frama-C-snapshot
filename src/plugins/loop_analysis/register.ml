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

let analyze kf =
  if Kernel_function.is_definition kf
  then
    if Cil_datatype.Stmt.Set.is_empty (Loop.get_non_naturals kf)
    then (Loop_analysis.analyze kf;
          Slevel_analysis.analyze kf)
    else
      Options.warning "Could not analyze function %a;@ \
                       it contains a non-natural loop"
        Kernel_function.pretty kf
;;


let main() =
  if Options.Run.get() then begin
    Globals.Functions.iter analyze;
    Slevel_analysis.display_results()
  end
;;

Db.Main.extend main;;

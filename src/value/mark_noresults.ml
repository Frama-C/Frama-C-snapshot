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

open Cil_types

class mark_visitor = object(_self)
  inherit Cil.nopCilVisitor

  method! vstmt s =
    Db.Value.update_table s Cvalue.Model.top;
    Cil.DoChildren

end

let should_memorize_function name =
  not (Value_parameters.NoResultsAll.get() ||
	  (Value_parameters.ObviouslyTerminatesAll.get()) || 
	  let name = name.svar.vname in
	  let mem = Datatype.String.Set.mem in
	  mem name (Value_parameters.NoResultsFunctions.get ())
	  || mem name (Value_parameters.ObviouslyTerminatesFunctions.get ()))

let run () =
  let visitor = new mark_visitor in
  Globals.Functions.iter_on_fundecs
      (fun afundec ->
         if not (should_memorize_function afundec)
         then
           ignore (Cil.visitCilFunction (visitor:>Cil.cilVisitor) afundec))

let () = Db.Value.no_results :=
  (fun fd -> not (should_memorize_function fd))

(* Signal that some results are not stored. The gui, or some calls to
   Db.Value, may fail ungracefully *)
let no_memoization_enabled () =
  Value_parameters.NoResultsAll.get() ||
  Value_parameters.ObviouslyTerminatesAll.get() ||
  not (Value_parameters.NoResultsFunctions.is_empty ()) ||
  not (Value_parameters.ObviouslyTerminatesFunctions.is_empty ())



(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

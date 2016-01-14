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

let should_memorize_function f =
  not (Value_parameters.NoResultsAll.get()
       || Value_parameters.ObviouslyTerminatesAll.get ()
       || Cil_datatype.Fundec.Set.mem
         f (Value_parameters.NoResultsFunctions.get ())
       || Cil_datatype.Fundec.Set.mem
         f (Value_parameters.ObviouslyTerminatesFunctions.get ()))

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
compile-command: "make -C ../../.."
End:
*)

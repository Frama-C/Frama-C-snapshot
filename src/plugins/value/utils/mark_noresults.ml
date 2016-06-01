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
compile-command: "make -C ../../../.."
End:
*)

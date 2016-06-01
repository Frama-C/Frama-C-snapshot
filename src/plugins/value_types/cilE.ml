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

(* ************************************************************************* *)
(* [JS 2011/03/11] All the below stuff manage warnings of the value analysis
   plug-in. Refactoring required. *)
(* ************************************************************************* *)

type alarm_behavior = 
    { a_log: bool;
      a_call: unit -> unit;}

let a_ignore = {a_log=false; a_call=Extlib.nop}

type warn_mode = {imprecision_tracing:alarm_behavior;
                  defined_logic: alarm_behavior;
                  unspecified: alarm_behavior;
                  others: alarm_behavior;}

let warn_all_mode =
  let alog = {a_log=true; a_call=Extlib.nop} in
  { imprecision_tracing = alog;
    defined_logic = alog;
    unspecified = alog; 
    others = alog; }

let warn_none_mode =
  { imprecision_tracing = a_ignore; defined_logic = a_ignore;
    unspecified = a_ignore; others=a_ignore; }


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

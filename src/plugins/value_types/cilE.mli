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

(** Value analysis alarms
    @plugin development guide *)

(* ************************************************************************* *)
(* [JS 2011/03/11] All the below stuff manage warnings of the value analysis
   plug-in. Refactoring required. *)
(* ************************************************************************* *)

type alarm_behavior =
    { a_log: bool;
      (** should the alarm be sent to the log *)
      
      a_call: unit -> unit;
    (** call function after optionally emitting with field a_log. *)
    }

val a_ignore: alarm_behavior

type warn_mode =
    { imprecision_tracing: alarm_behavior
         (** informative messages for garbled values *);
      defined_logic: alarm_behavior
         (** operations that raise an error only in the C, not in the logic *);
      unspecified: alarm_behavior (** defined but unspecified behaviors *);
      others: alarm_behavior (** all the remaining undefined behaviors *);
       }
      (** An argument of type [warn_mode] is required by some of the access
          functions in {!Db.Value}  (the interface to the value analysis). This
          argument tells what should be done with the various messages
          that the value analysis emits during the call.

          Each [warn_mode] field indicates the expected treatment for one
          category of message. These fields are not completely fixed
          yet. However, you can still used functions {!warn_all_mode} and
          {!warn_none_mode} below when you have to provide an argument of type
          [warn_mode]. *)

val warn_all_mode: warn_mode
  (** Emit all messages, including alarms and informative messages
      regarding the loss of precision. *)

val warn_none_mode : warn_mode
  (** Do not emit any message. *)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

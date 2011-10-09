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

(** Wrapper for [Printexc] compatible with all OCaml versions.
    @since Boron-20100401 *)

(** Facilities for printing exceptions. *)

val to_string : exn -> string
(** [Printexc.to_string e] returns a string representation of
   the exception [e]. *)

val print : ('a -> 'b) -> 'a -> 'b
(** [Printexc.print fn x] applies [fn] to [x] and returns the result.
   If the evaluation of [fn x] raises any exception, the
   name of the exception is printed on standard error output,
   and the exception is raised again.
   The typical use is to catch and report exceptions that
   escape a function application. *)

val catch : ('a -> 'b) -> 'a -> 'b
(** [Printexc.catch fn x] is similar to {!Printexc.print}, but
   aborts the program with exit code 2 after printing the
   uncaught exception.  This function is deprecated: the runtime
   system is now able to print uncaught exceptions as precisely
   as [Printexc.catch] does.  Moreover, calling [Printexc.catch]
   makes it harder to track the location of the exception
   using the debugger or the stack backtrace facility.
   So, do not use [Printexc.catch] in new code.  *)

exception No_backtrace

val has_backtrace: bool
(** [true] if the backtrace feature is available (ocaml >= 3.11) *)

val print_backtrace: out_channel -> unit
(** [Printexc.print_backtrace oc] prints an exception backtrace
    on the output channel [oc].  The backtrace lists the program
    locations where the most-recently raised exception was raised
    and where it was propagated through function calls.

    @raise No_backtrace if this feature is not available
    (OCaml < 3.11). *)

val get_backtrace: unit -> string
(** [Printexc.get_backtrace ()] returns a string containing the
    same exception backtrace that [Printexc.print_backtrace] would
    print.

    @raise No_backtrace if this feature is not available
    (OCaml < 3.11). *)

val record_backtrace: bool -> unit
(** [Printexc.record_backtrace b] turns recording of exception backtraces
    on (if [b = true]) or off (if [b = false]).  Initially, backtraces
    are not recorded, unless the [b] flag is given to the program
    through the [OCAMLRUNPARAM] variable.

    @raise No_backtrace if this feature is not available
    (OCaml < 3.11). *)

val backtrace_status: unit -> bool
(** [Printexc.backtrace_status()] returns [true] if exception
    backtraces are currently recorded, [false] if not.

    @raise No_backtrace if this feature is not available
    (OCaml < 3.11). *)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)

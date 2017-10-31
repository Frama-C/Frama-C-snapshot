(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(** {2 Callstacks related types and functions} *)

(** A call_stack is a list, telling which function was called at which
    site. The head of the list tells about the latest call. *)
type call_site = (kernel_function * kinstr)
type callstack = call_site list

(** Functions dealing with call stacks. *)
val clear_call_stack : unit -> unit
val pop_call_stack : unit -> unit
val push_call_stack : kernel_function -> kinstr -> unit

(** The current function is the one on top of the call stack. *)
val current_kf : unit -> kernel_function
val call_stack : unit -> callstack

(** Prints the current callstack. *)
val pp_callstack : Format.formatter -> unit

(** {2 Others} *)

(* TODO: Document the rest of this file. *)
val get_rounding_mode : unit -> Fval.rounding_mode
val emitter : Emitter.t
val get_slevel : Kernel_function.t -> Value_parameters.SlevelFunction.value
val pretty_actuals :
  Format.formatter -> (Cil_types.exp * Cvalue.V.t * 'b) list -> unit
val pretty_current_cfunction_name : Format.formatter -> unit
val warning_once_current : ('a, Format.formatter, unit) format -> 'a

(** Emit an alarm, either as warning or as a result, according to
    option AlarmsWarnings. *)
val alarm_report: ?level:int -> 'a Log.pretty_printer

(* Statements for which the analysis has degenerated. [true] means that this is
   the statement on which the degeneration occurred, or a statement above in
   the callstack *)
module DegenerationPoints:
  State_builder.Hashtbl with type key = stmt and type data = bool


val create_new_var: string -> typ -> varinfo
(** Create and register a new variable inside Frama-C. The variable
    has its [vlogic] field set, meaning it is not a source variable. The
    freshness of the name must be ensured by the user. *)

val is_const_write_invalid: typ -> bool
(** Detect that the type is const, and that option [-global-const] is set. In
    this case, we forbid writing in a l-value that has this type. *)

val float_kind: Cil_types.fkind -> Fval.float_kind
(** Classify a [Cil_types.fkind] as either a 32 or 64 floating-point type.
    Emit a warning when the argument is [long double], and [long double]
    is not equal to [double] *)

val postconditions_mention_result: Cil_types.funspec -> bool
(** Does the post-conditions of this specification mention [\result]? *)

val conv_comp: binop -> Abstract_interp.Comp.t
val conv_relation: relation -> Abstract_interp.Comp.t

val normalize_as_cond: exp -> bool -> exp
(** [normalize_as_cond e positive] returns the expression corresponding to
    [e != 0] when [positive] is true, and [e == 0] otherwise. The
    resulting expression will always have a comparison operation at its
    root. *)

val is_value_zero: exp -> bool
(** Return [true] iff the argument has been created by {!normalize_as_cond} *)

val lval_to_exp: lval -> exp
(** This function is memoized to avoid creating too many expressions *)

val dump_garbled_mix: unit -> unit
(** print information on the garbled mix created during evaluation *)


(** Dependences of expressions and lvalues. *)

val zone_of_expr:
  (lval -> Precise_locs.precise_location) -> exp -> Locations.Zone.t
(** Given a function computing the location of lvalues, computes the memory zone
    on which the value of an expression depends. *)

val indirect_zone_of_lval:
  (lval -> Precise_locs.precise_location) -> lval -> Locations.Zone.t
(** Given a function computing the location of lvalues, computes the memory zone
    on which the offset and the pointer expression (if any) of an lvalue depend.
*)

(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)

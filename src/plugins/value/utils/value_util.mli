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

(** Print a call stack. The first one does not display the call sites. *)
val pretty_call_stack_short : Format.formatter -> callstack -> unit
val pretty_call_stack : Format.formatter -> callstack -> unit

(** Prints the current callstack. *)
val pp_callstack : Format.formatter -> unit

(** {2 Others} *)

(* TODO: Document the rest of this file. *)
val get_rounding_mode : unit -> Fval.rounding_mode
val stop_if_stop_at_first_alarm_mode : unit -> unit
val emitter : Emitter.t
val warn_all_mode : CilE.warn_mode
val with_alarm_stop_at_first : CilE.warn_mode
val with_alarms_raise_exn : exn -> CilE.warn_mode
val warn_all_quiet_mode : unit -> CilE.warn_mode
val get_slevel : Kernel_function.t -> Value_parameters.SlevelFunction.value
val warn_indeterminate: Kernel_function.t -> bool
val set_loc : kinstr -> unit
val pretty_actuals :
  Format.formatter -> (Cil_types.exp * Cvalue.V.t * 'b) list -> unit
val pretty_current_cfunction_name : Format.formatter -> unit
val warning_once_current : ('a, Format.formatter, unit) format -> 'a
val debug_result :
  Kernel_function.t ->
  Cvalue.V_Offsetmap.t option * 'a * Base.SetLattice.t -> unit


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

val written_formals: Cil_types.kernel_function -> Cil_datatype.Varinfo.Set.t
(** Over-approximation of its formals the given function may write into. *)

val bind_block_locals: State_set.t -> Cil_types.block -> State_set.t
(** Bind all locals of the block to their default value
    (namely UNINITIALIZED) *)

val conv_comp: binop -> Abstract_interp.Comp.t
val conv_relation: relation -> Abstract_interp.Comp.t

(* Test that two functions types are compatible; used to verify that a call
   through a function pointer is ok. In theory, we could only check that
   both types are compatible as defined by C99, 6.2.7. However, some industrial
   codes do not strictly follow the norm, and we must be more lenient.
   Thus, we emit a warning on undefined code, but we also return true
   if Value can ignore more or less safely the incompatibleness in the types. *)
val compatible_functions:
  with_alarms:CilE.warn_mode -> varinfo -> typ -> typ -> bool

val zero: exp -> exp
(** Retun a zero constant compatible with the type of the argument *)

val is_value_zero: exp -> bool
(** Return [true] iff the argument has been created by {!zero} *)

(** These two functions are used to model the calls to [malloc] and [free]
    correctly. *)
val register_malloced_base: Base.t -> unit
val malloced_bases: unit -> Base.Hptset.t

(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)

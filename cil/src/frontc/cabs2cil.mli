(****************************************************************************)
(*                                                                          *)
(*  Copyright (C) 2001-2003                                                 *)
(*   George C. Necula    <necula@cs.berkeley.edu>                           *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                    *)
(*                                                                          *)
(*  Redistribution and use in source and binary forms, with or without      *)
(*  modification, are permitted provided that the following conditions      *)
(*  are met:                                                                *)
(*                                                                          *)
(*  1. Redistributions of source code must retain the above copyright       *)
(*  notice, this list of conditions and the following disclaimer.           *)
(*                                                                          *)
(*  2. Redistributions in binary form must reproduce the above copyright    *)
(*  notice, this list of conditions and the following disclaimer in the     *)
(*  documentation and/or other materials provided with the distribution.    *)
(*                                                                          *)
(*  3. The names of the contributors may not be used to endorse or          *)
(*  promote products derived from this software without specific prior      *)
(*  written permission.                                                     *)
(*                                                                          *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE          *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,     *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;        *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER        *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT      *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN       *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE         *)
(*  POSSIBILITY OF SUCH DAMAGE.                                             *)
(*                                                                          *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux          *)
(*                        énergies alternatives)                            *)
(*               and INRIA (Institut National de Recherche en Informatique  *)
(*                          et Automatique).                                *)
(****************************************************************************)

(** Registers a new hook that will be applied each time a side-effect free
    expression whose result is unused is dropped. The string is the name
    of the current function.
*)
val register_ignore_pure_exp_hook: (string -> Cil_types.exp -> unit) -> unit

(** new hook called when an implicit prototype is generated.
    @since Carbon-20101201
*)
val register_implicit_prototype_hook: (Cil_types.varinfo -> unit) -> unit

(** new hook called when two conflicting declarations are found.
    The hook takes as argument the old and new varinfo, and a
    description of the issue.
    @since Carbon-20101201
*)
val register_incompatible_decl_hook:
  (Cil_types.varinfo -> Cil_types.varinfo -> string -> unit) -> unit

(** new hook called when a definition has a compatible but not
    strictly identical prototype than its declaration
    The hook takes as argument the old and new varinfo. Note that only the
    old varinfo is kept in the AST, and that its type will be modified in
    place just after to reflect the merge of the prototypes.
    @since Carbon-20101201
*)
val register_different_decl_hook:
  (Cil_types.varinfo -> Cil_types.varinfo -> unit) -> unit


(** new hook called when encountering a definition of a local function. The hook
    take as argument the varinfo of the local function.
    @since Carbon-20101201
*)
val register_local_func_hook: (Cil_types.varinfo -> unit) -> unit

(** new hook called when side-effects are dropped.
The first argument is the original expression, the second one
the (side-effect free) normalized expression.
*)
val register_ignore_side_effect_hook:
  (Cabs.expression -> Cil_types.exp -> unit) -> unit

(** new hook called when an expression with side-effect is evaluated
    conditionally (RHS of && or ||, 2nd and 3rd term of ?:). Note that in case
    of nested conditionals, only the innermost expression with side-effects
    will trigger the hook (for instance, in [(x && (y||z++))],
    we have a warning on [z++], not on [y||z++], and similarly, on
    [(x && (y++||z))], we only have a warning on [y++]).
    - First expression is the englobing expression
    - Second expression is the expression with side effects.
*)
val register_conditional_side_effect_hook:
  (Cabs.expression -> Cabs.expression -> unit) -> unit

(** new hook that will be called when processing a for loop. 
    Arguments are the four elements of the for clause 
    (init, test, increment, body)
    @since Oxygen-20120901 
*)
val register_for_loop_all_hook:
  (Cabs.for_clause ->
   Cabs.expression -> Cabs.expression -> Cabs.statement -> unit) -> unit

(** new hook that will be called when processing a for loop. Argument is
    the initializer of the for loop.
    @since Oxygen-20120901 
*)
val register_for_loop_init_hook: (Cabs.for_clause -> unit) -> unit

(** new hook that will be called when processing a for loop. Argument is
    the test of the loop.
    @since Oxygen-20120901
*)
val register_for_loop_test_hook: (Cabs.expression -> unit) -> unit

(** new hook that will called when processing a for loop. Argument is the
    body of the loop.
    @since Oxygen-20120901
*)
val register_for_loop_body_hook: (Cabs.statement -> unit) -> unit

(** new hook that will be called when processing a for loop. Argument is
    the increment part of the loop.
    @since Oxygen-20120901
*)
val register_for_loop_incr_hook: (Cabs.expression -> unit) -> unit

(** @plugin development guide *)
val convFile: Cabs.file -> Cil_types.file


(** Name of the attribute inserted by the elaboration to prevent user blocks
    from disappearing. It can be removed whenever block contracts have been
    processed. *)
val frama_c_keep_block: string

(** A hook into the code that creates temporary local vars.  By default this
  is the identity function, but you can overwrite it if you need to change the
  types of cabs2cil-introduced temp variables. *)
val typeForInsertedVar: (Cil_types.typ -> Cil_types.typ) ref

(** Like [typeForInsertedVar], but for casts.
    [typeForInsertedCast expr original_type destination_type]
    returns the type into which [expr], which has type [original_type] and
    whose type must be converted into [destination_type], must be casted.

    By default, returns [destination_type].

    This applies only to implicit casts. Casts already present
    in the source code are exempt from this hook. *)
val typeForInsertedCast:
  (Cil_types.exp -> Cil_types.typ -> Cil_types.typ -> Cil_types.typ) ref

(** [fresh_global prefix] creates a variable name not clashing with any other
    globals and starting with [prefix] *)
val fresh_global : string -> string

(** Check that [s] starts with the prefix [p]. *)
val prefix : string -> string -> bool

val anonCompFieldName : string

val find_field_offset:
  (Cil_types.fieldinfo -> bool) -> Cil_types.fieldinfo list -> Cil_types.offset
(** returns the offset (can be more than one field in case of unnamed members)
    corresponding to the first field matching the condition.
    @raise Not_found if no such field exists.
 *)

(** returns the type of the result of a logic operator applied to values of
    the corresponding input types.
 *)
val logicConditionalConversion: Cil_types.typ -> Cil_types.typ -> Cil_types.typ

(** returns the type of the result of an arithmetic operator applied to
    values of the corresponding input types.
    @deprecated Nitrogen-20111001 moved to Cil module
*)
val arithmeticConversion : Cil_types.typ -> Cil_types.typ -> Cil_types.typ

(** performs the usual integral promotions mentioned in C reference manual.
    @deprecated Nitrogen-20111001 moved to Cil module.
*)
val integralPromotion : Cil_types.typ -> Cil_types.typ

(** local information needed to typecheck expressions and statements *)
type local_env = private
    { authorized_reads: Cil_datatype.Lval.Set.t;
      (** sets of lvalues that can be read regardless of a potential
          write access between sequence points. Mainly for tmp variables
          introduced by the normalization.
       *)
      known_behaviors: string list;
      (** list of known behaviors at current point. *)
      is_ghost: bool;
      (** whether we're analyzing ghost code or not *)
    }

(** an empty local environment. *)
val empty_local_env: local_env

(** same as [empty_local_env], but sets the ghost status to the value of its
    argument
 *)
val ghost_local_env: bool -> local_env

(* [VP] Jessie plug-in needs this function to be exported
   for semi-good reasons. *)
val blockInitializer :
  local_env ->
  Cil_types.varinfo -> Cabs.init_expression ->
  Cil_types.block * Cil_types.init * Cil_types.typ

(** Returns a block of statements equivalent to the initialization [init]
    applied to lvalue [lval] of type [typ]. *)
val blockInit:
  ghost:bool ->
  Cil_types.lval -> Cil_types.init -> Cil_types.typ -> Cil_types.block

(** Applies [mkAddrOf] after marking variable whose address is taken. *)
val mkAddrOfAndMark : Cil_types.location -> Cil_types.lval -> Cil_types.exp

(** If called, sets a flag so that [continue] in while loops get transformed
    into forward gotos, like it is already done in do-while and for loops. *)
val setDoTransformWhile : unit -> unit

(** If called, sets a flag so that translation of conditionals does not result
    in forward ingoing gotos (from the if-branch to the else-branch). *)
val setDoAlternateConditional : unit -> unit

(** Raise Failure *)
val integral_cast: Cil_types.typ -> Cil_types.term -> Cil_types.term

(** Given a call [lv = f()], if [tf] is the return type of [f] and [tlv]
    the type of [lv], [allow_return_collapse ~tlv ~tf] returns false
    if a temporary must be introduced to hold the result of [f], and
    true otherwise.

    Currently, implicit cast between pointers or cast from an scalar type
    or a strictly bigger one are accepted without cast. This is subject
    to change without notice.

    @since Oxygen-20120901
*)
val allow_return_collapse: tlv:Cil_types.typ -> tf:Cil_types.typ -> bool

val compatibleTypes: Cil_types.typ -> Cil_types.typ -> Cil_types.typ
(** Check that the two given types are compatible (C99, 6.2.7), and
    return their composite type. Raise [Failure] with an explanation
    if the two types are not compatible

    @since Oxygen-20120901
*)

val compatibleTypesp: Cil_types.typ -> Cil_types.typ -> bool
(** Check that the two given types are compatible (C99, 6.2.7), and
    return a boolean.

    @since Neon-20130301
*)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

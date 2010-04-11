(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2001-2003                                               *)
(*   George C. Necula    <necula@cs.berkeley.edu>                         *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                        *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                         *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                         *)
(*  All rights reserved.                                                  *)
(*                                                                        *)
(*  Redistribution and use in source and binary forms, with or without    *)
(*  modification, are permitted provided that the following conditions    *)
(*  are met:                                                              *)
(*                                                                        *)
(*  1. Redistributions of source code must retain the above copyright     *)
(*  notice, this list of conditions and the following disclaimer.         *)
(*                                                                        *)
(*  2. Redistributions in binary form must reproduce the above copyright  *)
(*  notice, this list of conditions and the following disclaimer in the   *)
(*  documentation and/or other materials provided with the distribution.  *)
(*                                                                        *)
(*  3. The names of the contributors may not be used to endorse or        *)
(*  promote products derived from this software without specific prior    *)
(*  written permission.                                                   *)
(*                                                                        *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS   *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT     *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS     *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE        *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,   *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,  *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;      *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER      *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT    *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN     *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE       *)
(*  POSSIBILITY OF SUCH DAMAGE.                                           *)
(*                                                                        *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux        *)
(*                        énergies alternatives).                         *)
(**************************************************************************)

(** Registers a new hook that will be applied each time a side-effect free
    expression whose result is unused is dropped.
*)
val register_ignore_pure_exp_hook:
  (string -> Cil_types.location -> Cil_types.exp -> unit) -> unit

val convFile: Cabs.file -> Cil_types.file

(** NDC added command line parameter.
    Turn on tranformation that forces correct parameter evaluation order *)
val forceRLArgEval: bool ref

(** Set this integer to the index of the global to be left in CABS form. Use
    -1 to disable *)
val nocil: int ref

(** Indicates whether we're allowed to duplicate small chunks of code. *)
val allowDuplication: bool ref

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

(** CEA-LRI: exports for logic typing *)

(** Check that [s] starts with the prefix [p]. *)
val prefix : string -> string -> bool

val annonCompFieldName : string
val logicConditionalConversion: Cil_types.typ -> Cil_types.typ -> Cil_types.typ
val arithmeticConversion : Cil_types.typ -> Cil_types.typ -> Cil_types.typ
val integralPromotion : Cil_types.typ -> Cil_types.typ

(** local information needed to typecheck expressions and statements *)
type local_env = private
    { authorized_reads: Cilutil.LvalSet.t;
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
val blockInit :
  Cil_types.lval -> Cil_types.init -> Cil_types.typ -> Cil_types.block

(** Applies [mkAddrOf] after marking variable whose address is taken. *)
val mkAddrOfAndMark : Cil_types.lval -> Cil_types.exp

(** If called, sets a flag so that [continue] in while loops get transformed
    into forward gotos, like it is already done in do-while and for loops. *)
val setDoTransformWhile : unit -> unit

(** If called, sets a flag so that translation of conditionals does not result
    in forward ingoing gotos (from the if-branch to the else-branch). *)
val setDoAlternateConditional : unit -> unit

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)

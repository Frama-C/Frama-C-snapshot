(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(* -------------------------------------------------------------------------- *)
(* --- Compilation of ACSL Logic-Info                                     --- *)
(* -------------------------------------------------------------------------- *)

open LogicUsage
open Cil_types
open Cil_datatype
open Clabels
open Lang
open Lang.F
open Memory
open Definitions

module Make( M : Memory.Model ) :
sig

  (** {3 Definitions} *)

  type value = M.loc Memory.value
  type logic = M.loc Memory.logic
  type sigma = M.Sigma.t
  type chunk = M.Chunk.t

  (** {3 Frames} *)

  type frame
  val pp_frame : Format.formatter -> frame -> unit

  val frame : kernel_function -> frame
  val frame_copy : frame -> frame
  val call_pre   : kernel_function -> value list -> sigma -> frame
  val call_post  : kernel_function -> value list -> sigma sequence -> frame

  val formal : varinfo -> value option
  val return : unit -> typ
  val result : unit -> var
  val status : unit -> var
  val trigger : trigger -> unit

  val guards : frame -> pred list
  val mem_frame : c_label -> sigma
  val mem_at_frame : frame -> c_label -> sigma

  val in_frame : frame -> ('a -> 'b) -> 'a -> 'b
  val get_frame : unit -> frame

  (** {3 Environment} *)

  type env

  val new_env : Logic_var.t list -> env
  val move : env -> sigma -> env
  val sigma : env -> sigma
  val env_at : env -> c_label -> env
  val mem_at : env -> c_label -> sigma
  val env_let : env -> logic_var -> logic -> env
  val env_letval : env -> logic_var -> value -> env

  (** {3 Compiler} *)

  val term : env -> Cil_types.term -> term
  val pred : bool -> env -> predicate named -> pred
  val logic : env -> Cil_types.term -> logic 
  val region : env -> Cil_types.term -> M.loc sloc list 

  val bootstrap_term : (env -> Cil_types.term -> term) -> unit
  val bootstrap_pred : (bool -> env -> predicate named -> pred) -> unit
  val bootstrap_logic : (env -> Cil_types.term -> logic) -> unit
  val bootstrap_region : (env -> Cil_types.term -> M.loc sloc list) -> unit

  (** {3 Application} *)

  val call_fun : env -> logic_info
    -> (logic_label * logic_label) list
    -> F.term list -> F.term

  val call_pred : env -> logic_info
    -> (logic_label * logic_label) list
    -> F.term list -> F.pred

  (** {3 Logic Variable and ACSL Constants} *)

  val logic_var : env -> logic_var -> logic

  (** {3 Logic Lemmas} *)

  val lemma : logic_lemma -> dlemma

end

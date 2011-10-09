(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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

(* ------------------------------------------------------------------------ *)
(* ---  Translation of Term and Predicates                              --- *)
(* ------------------------------------------------------------------------ *)

open Ctypes
open Clabels
open Formula
open Cil_types

module Create
  (M : Mlogic.S)

  :
sig

  (** {2 Translation environments} *)

  type env

  val env: Kernel_function.t ->
    ?m_here:M.mem ->
    ?m_pre:M.mem ->
    ?m_post:M.mem ->
    ?x_result:M.F.var ->
    unit -> env

  val env_at : env -> c_label -> env
  val mem_at : env -> c_label -> M.mem
  val find_mem : env -> c_label -> M.mem option
  val subst_result : env -> M.value option -> M.F.pred -> M.F.pred
  val result_type : env -> typ
  val exit_status : env -> M.F.var
  val call_pre  : env -> Kernel_function.t -> M.value list -> M.mem -> env

  val call_post :
    env -> Kernel_function.t -> M.value list -> M.mem -> M.mem ->
    M.F.var option -> env

  val call_exit :
    env -> Kernel_function.t -> M.value list -> M.mem -> M.mem ->
    M.F.var -> env

  (** {2 Translation functions} *)

  (**  to be used to retreive variable add through [add_logic_vars]. *)
  val collect_logic_vars : env -> M.F.var list
  val add_logic_vars : env -> M.F.pool -> logic_var list -> env

  (** [term e t] interprets the C terms [t] in memory model environment [e]
      as a logic term.**)
  val term : env -> term -> M.F.abstract

  (** [prop e p] interprets an ACSL predicate as a logic predicats
      in memory model environment [e]. **)
  val prop : env -> predicate named -> M.F.pred

  (** Compiles an arbitrary term representing a set of left-values into a zone *)
  val assigned : env -> Cil_types.term -> M.loc M.F.assigned list

  (** {2 Axiomatics management} *)

  (** Compile an axiom and add it to the list of global declarations. *)
  val add_axiom : string -> Cil_types.logic_label list -> Cil_types.predicate named -> unit

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

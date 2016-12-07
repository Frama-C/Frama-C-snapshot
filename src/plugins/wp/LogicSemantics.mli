(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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
(* --- ACSL Translation                                                   --- *)
(* -------------------------------------------------------------------------- *)

open Definitions
open LogicUsage
open Cil_types
open Ctypes
open Clabels
open Lang.F
open Memory

type polarity = [ `Positive | `Negative | `NoPolarity ]

module Make(M : Memory.Model) :
sig

  type loc = M.loc
  type sigma  = M.Sigma.t
  type value  = M.loc Memory.value
  type logic  = M.loc Memory.logic
  type region = M.loc Memory.sloc list

  (** {3 Debug} *)

  val pp_logic : Format.formatter -> logic -> unit
  val pp_sloc : Format.formatter -> loc Memory.sloc -> unit
  val pp_region : Format.formatter -> region -> unit

  (** {3 Frames} *)

  type call
  type frame
  val pp_frame : Format.formatter -> frame -> unit
  val get_frame : unit -> frame
  val in_frame : frame -> ('a -> 'b) -> 'a -> 'b
  val mem_frame : c_label -> sigma
  val mem_at_frame : frame -> c_label -> sigma

  val call : kernel_function -> value list -> call
  val frame : kernel_function -> frame
  val call_pre   : sigma -> call -> sigma -> frame
  val call_post  : sigma -> call -> sigma sequence -> frame

  val return : unit -> typ
  val result : unit -> var
  val status : unit -> var

  val guards : frame -> pred list

  (** {3 Traductions} *)

  type env

  val new_env : logic_var list -> env
  val move : env -> sigma -> env
  val sigma : env -> sigma
  val mem_at : env -> c_label -> sigma
  val call_env : sigma -> env

  val term : env -> Cil_types.term -> term
  val pred : polarity -> env -> Cil_types.predicate -> pred
  val region : env -> Cil_types.term -> region
  val assigns : env -> identified_term assigns -> (c_object * region) list option
  val assigns_from : env -> identified_term from list -> (c_object * region) list

  val val_of_term : env -> Cil_types.term -> term
  val loc_of_term : env -> Cil_types.term -> loc

  val lemma : logic_lemma -> dlemma

  (** {3 Regions} *)

  val vars : region -> Vars.t
  val occurs : var -> region -> bool
  val valid : sigma -> acs -> c_object -> region -> pred
  val included : c_object -> region -> c_object -> region -> pred
  val separated : (c_object * region) list -> pred

end

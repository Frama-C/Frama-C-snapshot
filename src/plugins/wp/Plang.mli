(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(**************************************************************************)

open Format
open Qed.Logic
open Lang
open Lang.F

(** Lang Pretty-Printer *)

type scope = Qed.Engine.scope
module Env : Qed.Engine.Env with type term := term

type pool
val pool : unit -> pool
val xmark_e : pool -> (var -> unit) -> term -> unit
val xmark_p : pool -> (var -> unit) -> pred -> unit
val xmark : pool -> Vars.t

class engine :
  object
    inherit [Z.t,ADT.t,Field.t,Fun.t,tau,var,term,Env.t] Qed.Engine.engine
    method marks : Env.t * Lang.F.marks
    method pp_pred : Format.formatter -> pred -> unit
    method lookup : term -> scope
    (**/**)
    inherit Lang.idprinting
    method infoprover : 'a. 'a infoprover -> 'a
    method op_spaced : string -> bool
    (**/**)
  end

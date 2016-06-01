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

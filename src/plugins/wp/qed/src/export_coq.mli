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

open Logic
open Format
open Plib

(** Exportation Engine for Coq.

    Provides a full {{:Export.S.engine-c.html}engine}
    from a {{:Export.S.linker-c.html}linker}. *)

module Make(T : Term) :
sig

  open T
  module Env : Engine.Env with type term := term
  type trigger = (var,Fun.t) Engine.ftrigger
  type typedef = (tau,Field.t,Fun.t) Engine.ftypedef

  class virtual engine :
    object
      inherit [Z.t,ADT.t,Field.t,Fun.t,tau,var,term,Env.t] Engine.engine
      method marks : Env.t * T.marks
      method op_spaced : string -> bool
      method declare_type : formatter -> ADT.t -> int -> typedef -> unit
      method declare_axiom : formatter -> string -> var list -> trigger list list -> term -> unit
      method declare_fixpoint : prefix:string -> formatter -> Fun.t -> var list -> tau -> term -> unit
      method declare_signature : formatter -> Fun.t -> tau list -> tau -> unit
      method declare_definition : formatter -> Fun.t -> var list -> tau -> term -> unit
    end

end

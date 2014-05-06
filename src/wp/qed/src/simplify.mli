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
(* --- Solver infrastructure for Qed                                      --- *)
(* -------------------------------------------------------------------------- *)

open Logic

exception Absurd
exception Unknown

module Make(T : Logic.Term) :
sig
  
  type var
  type exp = var T.expression

  module Vset : Idxset.S with type elt = var
  module Vmap : Idxmap.S with type key = var

  val vtrue  : var
  val vfalse : var

  val map : (var -> var) -> exp -> exp
  val vmap : 
    (var -> var) -> ('a -> 'b) -> ('b -> 'b -> 'b) -> 
    'a Vmap.t -> 'b Vmap.t

  class type context =
    object
      method build : exp -> var
      method query : exp -> maybe
      method infer : exp -> unit
    end

  class type theory =
    object
      method copy : theory
      (** Never fails. *)
        
      method define : var -> exp -> unit
      (** May raise [Absurd]. *)

      method assume : exp -> bool -> unit
      (** May raise [Absurd]. *)

      method rewrite : Vset.t -> (var -> var) -> unit
      (** May raise [Absurd]. *)

      method resolve : context -> exp -> var
      (** May raise [Absurd] or [Unknown]. *)
    end

  type state

  val create : theory list -> state
  val copy : state -> state
    
  val assume : state -> T.term -> unit
    (** May raise [Absurd] when the {i augmented} state is inconsistent. *)

  val simplify : state -> ?timeout:int -> T.term -> T.term
    (** May raise [Absurd] when the state is inconsistent. *)
    
end

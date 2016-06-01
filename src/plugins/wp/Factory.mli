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

(* -------------------------------------------------------------------------- *)
(* --- Model Factory                                                      --- *)
(* -------------------------------------------------------------------------- *)

type mheap = Hoare | ZeroAlias | Typed of MemTyped.pointer
type mvar = Raw | Var | Ref | Caveat

type setup = {
  mvar : mvar ;
  mheap : mheap ;
  cint : Cint.model ;
  cfloat : Cfloat.model ;
}

type driver = LogicBuiltins.driver

val ident : setup -> string
val descr : setup -> string
val memory : mheap -> mvar -> (module Memory.Model)
val configure : setup -> driver -> Model.tuning
val instance : setup -> driver -> Model.t
val default : setup (** ["Var,Typed,Nat,Real"] memory model. *)
val parse :
  ?default:setup ->
  ?warning:(string -> unit) ->
  string list -> setup
(** 
   Apply specifications to default setup.
   Default setup is [Factory.default].
   Default warning is [Wp_parameters.abort]. *)

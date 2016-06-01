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
(* --- Exportation to Foreign Languages                                   --- *)
(* -------------------------------------------------------------------------- *)

(** Export Engine Factory *)

open Format
open Logic
open Plib
open Engine

val cmode : mode -> cmode
val amode : mode -> amode
val pmode : mode -> pmode
val tmode : ('a,'f) Logic.datatype -> mode
val ctau  : ('a,'f) Logic.datatype -> cmode

val is_ident : string -> bool
val extract_ident : string -> string

val debug : link -> string
val link_name : link -> string

module Make(T : Term) :
sig

  open T
  module TauMap : Map.S with type key = tau
  module Env : Env with type term := term

  type trigger = (var,Fun.t) ftrigger
  type typedef = (tau,Field.t,Fun.t) ftypedef
  
  class virtual engine :
    object

      method virtual datatype : ADT.t -> string
      method virtual field : Field.t -> string
      method basename : string -> string
      (** Allows to sanitize the basename used for in this engine for variable. *)
      method virtual link : Fun.t -> link

      method env : Env.t (** A safe copy of the environment *)
      method marks : Env.t * T.marks (** The current environment with empty marks *)
      method lookup : term -> scope
      method scope : Env.t -> (unit -> unit) -> unit
      method local : (unit -> unit) -> unit
      method global : (unit -> unit) -> unit
      method bind : var -> string
      method find : var -> string

      method virtual t_int  : string
      method virtual t_real : string
      method virtual t_bool : string
      method virtual t_prop : string
      method virtual t_atomic : tau -> bool
      method virtual pp_tvar : int printer
      method virtual pp_array : tau printer
      method virtual pp_farray : tau printer2
      method virtual pp_datatype : ADT.t -> tau list printer
      method pp_subtau : tau printer

      method mode : mode
      method with_mode : mode -> (mode -> unit) -> unit

      method virtual e_true : cmode -> string
      method virtual e_false : cmode -> string
      method virtual pp_int : amode -> Z.t printer
      method virtual pp_cst : Numbers.cst printer
      method pp_real : R.t printer

      method virtual is_atomic : term -> bool
      method virtual op_spaced : string -> bool
      method virtual callstyle : callstyle
      method virtual pp_apply : cmode -> term -> term list printer
      method pp_fun : cmode -> Fun.t -> term list printer

      method virtual op_scope : amode -> string option
      method virtual op_real_of_int : op
      method virtual op_add : amode -> op
      method virtual op_sub : amode -> op
      method virtual op_mul : amode -> op
      method virtual op_div : amode -> op
      method virtual op_mod : amode -> op
      method virtual op_minus : amode -> op
      method pp_times : formatter -> Z.t -> term -> unit

      method virtual op_equal : cmode -> op
      method virtual op_noteq : cmode -> op
      method virtual op_eq  : cmode -> amode -> op
      method virtual op_neq : cmode -> amode -> op
      method virtual op_lt  : cmode -> amode -> op
      method virtual op_leq : cmode -> amode -> op

      method virtual pp_array_get : formatter -> term -> term -> unit
      method virtual pp_array_set : formatter -> term -> term -> term -> unit

      method virtual pp_get_field : formatter -> term -> Field.t -> unit
      method virtual pp_def_fields : record printer

      method virtual op_not   : cmode -> op
      method virtual op_and   : cmode -> op
      method virtual op_or    : cmode -> op
      method virtual op_imply : cmode -> op
      method virtual op_equiv : cmode -> op

      method pp_not : term printer
      method pp_imply : formatter -> term list -> term -> unit
      method pp_equal : term printer2
      method pp_noteq : term printer2

      method virtual pp_conditional : formatter -> term -> term -> term -> unit

      method virtual pp_forall : tau -> string list printer
      method virtual pp_exists : tau -> string list printer
      method virtual pp_lambda : (string * tau) list printer

      method is_shareable : term -> bool
      method virtual pp_let : formatter -> pmode -> string -> term -> unit
      method pp_atom : term printer
      method pp_flow : term printer

      method pp_tau : tau printer
      method pp_var : string printer
      method pp_term : term printer
      method pp_prop : term printer
      method pp_sort : term printer
      method pp_expr : tau -> term printer

    end

end

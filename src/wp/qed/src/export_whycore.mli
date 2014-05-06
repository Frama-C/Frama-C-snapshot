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
(* --- Common Exportation Engine for Alt-Ergo and Why3                    --- *)
(* -------------------------------------------------------------------------- *)

open Logic
open Format
open Plib
open Engine

(** Common Exportation Engine for Why-3 and Alt-Ergo *)

module Make(T : Term) :
sig

  open T

  type trigger = (T.var,Fun.t) ftrigger
  type typedef = (tau,Field.t,Fun.t) ftypedef

  class virtual engine :
    object

      method virtual datatype : ADT.t -> string
      method virtual field : Field.t -> string
      method basename : string -> string
      method virtual link : Fun.t -> link

      method declare : string -> unit
      method declare_all : string list -> unit

      method local : (unit -> unit) -> unit
      method global : (unit -> unit) -> unit

      method t_int  : string
      method t_real : string
      method t_bool : string
      method t_prop : string
      method virtual t_atomic : tau -> bool
      method pp_tvar : int printer

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
      method pp_apply : cmode -> term -> term list printer
      method pp_fun : cmode -> Fun.t -> term list printer

      method op_scope : amode -> string option
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

      method pp_array_get : formatter -> term -> term -> unit
      method pp_array_set : formatter -> term -> term -> term -> unit

      method virtual op_record : string * string
      method pp_get_field : formatter -> term -> Field.t -> unit
      method pp_def_fields : record printer

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

      method virtual pp_forall : tau -> var list printer
      method virtual pp_intros : tau -> var list printer
      method virtual pp_exists : tau -> var list printer
      method pp_lambda : var list printer

      method bind : var -> unit
      method virtual pp_let : formatter -> pmode -> string -> term -> unit

      method is_shareable : term -> bool
      method pp_atom : term printer
      method pp_flow : term printer

      method pp_tau : tau printer
      method pp_var : var printer
      method pp_term : term printer
      method pp_prop : term printer
      method pp_expr : tau -> term printer

      method pp_param : var printer
      method virtual pp_trigger : trigger printer
      method virtual pp_declare_adt : formatter -> ADT.t -> int -> unit
      method virtual pp_declare_def : formatter -> ADT.t -> int -> tau -> unit
      method virtual pp_declare_sum : formatter -> ADT.t -> int -> (Fun.t * tau list) list -> unit

      method pp_declare_symbol : cmode -> formatter -> Fun.t -> unit
      method declare_type : formatter -> ADT.t -> int -> typedef -> unit
      method declare_axiom : formatter -> string -> T.var list -> trigger list list -> term -> unit
      method declare_prop : kind:string -> formatter -> string -> T.var list -> trigger list list -> term -> unit

    end

end

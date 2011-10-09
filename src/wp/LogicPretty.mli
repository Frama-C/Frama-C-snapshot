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

(* -------------------------------------------------------------------------- *)
(** Pretty Printing Library for Logic *)
(* -------------------------------------------------------------------------- *)

open LogicId
open LogicTau
open LogicRaw

type u_printer = Format.formatter -> unit (** Suitable for [%t] formatter *)
type 'a printer = Format.formatter -> 'a -> unit (** Suitable for [%a] formatter *)
type ('a,'b) printer2 = Format.formatter -> 'a -> 'b -> unit (** Non-formatter usage *)
type 'a fun_printer = Format.formatter -> 'a -> TERM.t list -> unit

(** {2 Utilities} *)

val pp_coma : string -> 'a printer -> 'a list printer
  (** [pp_coma ","] produces ["x1, x2, ... xn"] *)

val pp_assoc : string -> string -> 'a printer -> 'a list printer
  (** [pp_assoc "0" "+"] produces ["x1 + x2 + ... + xn"] and ["0"] for empty list *)

val pp_tuple : 'a printer -> 'a list printer
  (** [pp_tuple] produces ["(x1, x2, ... )"] and ["()"] for empty list *)

val pp_string : string -> u_printer
  (** The [%t] printer that print the string *)

val pp_tuple_call : 'a printer -> (string,'a list) printer2
  (** Prints ["f(x1,...,xn)"] and ["f()"] for empty list. *)

val pp_apply_call : 'a printer -> (string,'a list) printer2
  (** Printts ["(f x1 ... xn)"] of ["f"] for empty list. *)

val pp_fold_op : string -> string -> 'a printer -> 'a list printer
  (** [pp_fold_op "0" "+"] prints ["x+(...(y+z))"] or ["0"]. *)

val pp_fold_call : string -> string -> 'a printer -> 'a list printer
  (** [pp_fold_call "e" "f"] prints ["f(x,f(...,f(y,z)))"] or ["e"]. *)

val pp_fold_apply : string -> string -> 'a printer -> 'a list printer
  (** [pp_fold_apply "e" "f"] prints ["(f x (f...(f y z)))"] or ["e"]. *)

(** {2 Pretty printer engine} *)

type binder

type call_style =
  | FunCall (** the [pp_tuple_call] style *)
  | VFunCall (** the [pp_tuple_call] style with ["f"] for empty lists *)
  | ApplyCall (** the [pp_apply_call] style *)

type operator = 
  | Infix of string (** ["x (op) y"] *)
  | Prefix of string (** ["x (op)"] *)
  | Postfix of string (** ["(op) x"] *)
  | Assoc of string * string 
      (** [Assoc(nil,op)] prints [nil] 
	  for empty lists and associatively flatten infix [op] *)
  | Extern of string (** call [s] with current call-style *)
  | Call of id (** call [id] with current space and call-style *)

class engine : LogicId.space ->
object

  (** {3 Names} *)

  method id : id -> string
  method pp_id : id printer
    
  (** {3 Types} *)
    
  method pp_tau_int : u_printer
  method pp_tau_real : u_printer
  method pp_tau_bool : u_printer
  method pp_tau_pointer : u_printer
  method pp_tau_set : tau printer
  method pp_tau_array : (tau,tau) printer2
  method pp_tau_record : id printer
  method pp_tau_adt : (id,tau list) printer2
  method pp_tau_alpha : int printer
  method pp_tau : tau printer

  (** {3 Variables} *)

  method alpha : 'a. 'a printer -> 'a printer
    (** Runs the printer in an environment with global
	alpha-conversion.  The global alpha-conversion mode is
	reverted after [alpha].  Successive calls to [alpha] keep the
	global mode until the first call to [alpha] returns. 
	Exceptions are correctly tracked. *)

  method bind : 'a. VAR.t list -> (unit -> unit) -> unit
    (** [bind x pp] runs [pp] with variable [x] bound to a new
	identifier.  The identifier is released after [bind] unless
	the engine is in global alpha-conversion mode (see [alpha]). 
	Exceptions are correctly tracked. *)

  method var_id : VAR.t -> id
    (** Current identifier associated to a variable. 
	Raises [Not_found] if unbound. *)

  method pp_var : VAR.t printer
    (** Pretty print the variable with its associated identifier. 
	Prints a debugging name with format ["?<base>#<vid>"] if unbound. *)

  method pp_vartype : VAR.t printer
    (** Prints the type of the variable. *)

  method binder : VAR.t -> (binder -> unit) -> unit
    (** Allocates an identifier for binder, not-yet linked to the variable. 
	The bind is release after job. *)

  method pp_binder : binder printer
    (** Prints the identifier of the associated variable. *)

  method with_binder : 'a. binder -> 'a printer -> 'a printer
    (** Run the printer with the variable locally bound to its identifier. 
	Exception are correctly tracked. *)

  (** {3 Terms} *)

  method term_call : call_style
  method term_atomic : TERM.t -> bool (** Uses [term_operator] *)
  method term_operator : TERM.primitive -> operator

  method pp_term_int : string printer
  method pp_term_real : string printer
  method pp_term_true : u_printer
  method pp_term_false : u_printer
  method pp_term_extern : string fun_printer
    (** Uses [term_call] style *)
  method pp_term_call : id fun_printer
    (** Uses [term_call] style *)
  method pp_term_operator : operator fun_printer
  method pp_term_primitive : TERM.primitive fun_printer
    (** Uses [term_operator] and [pp_term_operator] 
	with flattening for associative cases. *)
  method pp_term_access : Format.formatter -> TERM.t -> TERM.t -> unit
  method pp_term_update : Format.formatter -> TERM.t -> TERM.t -> TERM.t -> unit
  method pp_term_getfield : Format.formatter -> TERM.t -> field -> unit
  method pp_term_setfield : Format.formatter -> TERM.t -> field -> TERM.t -> unit
  method pp_term_cond : Format.formatter -> TERM.t -> TERM.t -> TERM.t -> unit
  method pp_term_let : Format.formatter -> VAR.t -> TERM.t -> TERM.t -> unit
    
  method pp_term_atom : TERM.t printer 
    (** Prints with [pp_term] with parentheses for only non-atomic terms,
	with respect to [term_atomic] method. *)
  method pp_term : TERM.t printer
    (** Might result in non lexically-atomic print. 
	Use [pp_term_atom] for safe boxing. *)
    
  (** {3 Predicates} *)

  method pred_atomic : PRED.t -> bool
  method pred_relation : PRED.relation -> operator

  method pp_pred_true : u_printer
  method pp_pred_false : u_printer
  method pp_pred_relation : PRED.relation fun_printer
    (** Uses [pred_operator] and [pp_term_operator] *)
  method pp_pred_call : id fun_printer
    (** Uses [pp_term_call] *)
  method pp_pred_and : PRED.t list printer
  method pp_pred_or : PRED.t list printer
  method pp_pred_not : PRED.t printer
  method pp_pred_iff : (PRED.t,PRED.t) printer2
  method pp_pred_named : (id list,PRED.t) printer2
  method pp_pred_cond : Format.formatter -> TERM.t -> PRED.t -> PRED.t -> unit
  method pp_pred_let : Format.formatter -> VAR.t -> TERM.t -> PRED.t -> unit
  method pp_pred_forall : (VAR.t list,PRED.t) printer2
  method pp_pred_exists : (VAR.t list,PRED.t) printer2
  method pp_pred_implies : (PRED.t list,PRED.t) printer2

  method pp_pred_atom : PRED.t printer
    (** Prints with [pp_term] with parentheses for only non-atomic terms,
	with respect to [pred_atomic] method. *)
  method pp_pred : PRED.t printer
    (** Might result in non lexically-atomic print.
	USe [pp_pred_atom] for safe boxing. *)

end

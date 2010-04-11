(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
(*    INSA  (Institut National des Sciences Appliquees)                   *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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

(* $Id: data_for_ltl.mli,v 1.5 2008-12-17 17:06:27 uid588 Exp $ *)

(** Module of data management used in all the plugin Aorai. Operations are mainly accessors for data. The use of this module is mainly done through the ltl_utils module. *)

(* ************************************************************************* *)
(** {2 LTL/Promela primitives} *)
(* ************************************************************************* *)

(** Here are some operations used for generation of LTL AST or Promela AST. *)


(** Initializes some tables according to data from Cil AST. *)
val setCData : unit -> unit

(** *)
val setLtl_expressions : (string, (Cil_types.exp* string*Cil_types.predicate)) Hashtbl.t -> unit

(** *)
val ltl_expressions_iter : (string -> (Cil_types.exp * string*Cil_types.predicate) -> unit) -> unit

(** *)
val get_exp_from_tmpident : string -> Cil_types.exp

(** *)
val get_str_exp_from_tmpident : string -> string

val debug_ltl_expressions : unit -> unit


(** *)
val get_pred_from_tmpident : string -> Cil_types.predicate

(** *)
val add_logic : string -> Cil_types.logic_info -> unit

(** *)
val get_logic : string -> Cil_types.logic_info


(** *)
val add_predicate : string -> Cil_types.logic_info -> unit

(** *)
val get_predicate : string -> Cil_types.logic_info


(* ************************************************************************* *)

(**{b Constants} Some constant names used for generation. *)

(* Logic variables *)
(** Name of TransStart logic generated variable *)
val transStart   : string

(** Name of transStop logic generated variable *)
val transStop    : string

(** Name of transCond logic generated variable *)
val transCond    : string

(** Name of transCondP logic generated variable *)
val transCondP   : string

(** Name of the fresh loopInit logic generated variable *)
val loopInit     : string

(** C variables *)

(** Name of curOp C generated variable (Name of the curent operation) *)
val curOp        : string

(** Name of curOpStatus C generated variable (Status Return or Call of the curent operation) *)
val curOpStatus  : string

(** Name of curState C generated variable (Table of states that can be synchronized with the program) *)
val curState     : string

(** Name of curStateOld C generated variable (Last value of curState) *)
val curStateOld  : string

(** Name of curTrans C generated variable (Last transitions that can be crossed) *)
val curTrans     : string

(*val curTransTmp  : string DEPRECATED *)
(** Name of acceptSt C generated variable (List of acceptation States) *)

val acceptSt     : string
(* C constants #define -- DEPRECATED ?*)

(** DEPRECATED ?*)
val nbOp         : string

(** DEPRECATED ?*)
val nbStates     : string

(** DEPRECATED ?*)
val nbAcceptSt   : string

(** DEPRECATED ?*)
val nbTrans      : string

(* C Macros *)

(** DEPRECATED ?*)
val macro_ligth  : string

(** DEPRECATED ?*)
val macro_full   : string

(** DEPRECATED ?*)
val macro_pure   : string

(* C enumeration *)

(** Name of listOp C generated enumeration (List of operation names prefixed with 'op_') *)
val listOp       : string

(** Name of listStatus C generated enumeration (Status are Call or Return) *)
val listStatus   : string

(** Name of callStatus C generated enumeration (Name of the Call status) *)
val callStatus   : string

(** Name of termStatus C generated enumeration (Name of the return status) *)
val termStatus   : string

(** Name of the enum type representing states *)
val states  : string

(* C function -- DEPRECATED *)

(** DEPRECATED ?*)
val buch_sync    : string





(* ************************************************************************* *)
(**{b Buchi automata management}*)

(** Return the buchi automata as stored after parsing *)
val getAutomata : unit -> Promelaast.buchautomata

(** Stores the buchi automata and its variables and functions as such as it  is return by the parsing *)
val setAutomata : Promelaast.buchautomata -> (string, string) Hashtbl.t -> (string, string) Hashtbl.t -> unit


(** return the number of transitions of the automata *)
val getNumberOfTransitions : unit -> int

(** return the number of states of the automata *)
val getNumberOfStates : unit -> int


(** Return the list of all function name observed in the promela file. *)
val getFunctions_from_auto : unit -> string list

(** Return the list of all variables name observed in the promela file. *)
val getVariables_from_auto : unit -> string list

(** Return the list of all function name observed in the C file. *)
val getFunctions_from_c : unit -> string list

(** Return the list of all variables name observed in the C file. *)
val getVariables_from_c : unit -> string list

(** Return the list of names of all ignored functions. A function is ignored if it is used in C file and if its declaration is unavailable. *)
val getIgnoredFunctions : unit -> string list

(** Return the list of names of all ignored functions. A function is ignored if it is used in C file and if its declaration is unavailable. *)
val addIgnoredFunction : string -> unit

(** Return true if and only if the given string fname denotes an ignored function. *)
val isIgnoredFunction : string -> bool

(** Manage particular consistency verification between C file and promela specification.
    It returns true if and only if these checks are ok. *)
val check_consistency : unit -> bool



(* ************************************************************************* *)
(**{b Variables information} Usually it seems very usefull to access to varinfo
   structure of a variable by using only its name. These functions allow that.
   In practice it contains all variables (from promela and globals from C file) and only variables.
*)

(** Add a new variable into the association table name -> varinfo *)
val set_varinfo : string -> Cil_types.varinfo -> unit

(** Given a variable name, it returns its associated varinfo.
    If the variable is not found then an error message is print and an assert false is raised. *)
val get_varinfo : string -> Cil_types.varinfo

(** Same as get_varinfo, but the result is an option.
    Hence, if the variable is not found then None is return. *)
val get_varinfo_option : string -> Cil_types.varinfo option



(** Add a new param into the association table (funcname,paramname) -> varinfo *)
val set_paraminfo : string -> string -> Cil_types.varinfo -> unit

(** Given a function name and a param name, it returns the varinfo associated to the given param.
    If the variable is not found then an error message is print and an assert false is raised. *)
val get_paraminfo : string -> string -> Cil_types.varinfo 

(** Add a new param into the association table (funcname,paramname) -> varinfo *)
val set_returninfo : string -> Cil_types.varinfo -> unit

(** Given a function name and a param name, it returns the varinfo associated to the given param.
    If the variable is not found then an error message is print and an assert false is raised. *)
val get_returninfo : string -> Cil_types.varinfo 


(* ************************************************************************* *)
(**{b Pre and post condition of C functions} In our point of view, the pre or
   the post condition of a C function are defined by the set of states
   authorized just before/after the call, as such as the set of crossable
   transitions. The following functions manages these stored informations.
   Usually, the first array is for the authorized states, while the second one
   is for the crossable conditions *)

(** Returns the pre condition associated to the given C function *)
val get_func_pre  : ?securised:bool -> string -> (bool array)*(bool array)

(** Sets the pre condition of the given C function *)
val set_func_pre  : string -> (bool array)*(bool array) -> unit

(** Returns the post condition associated to the given C function *)
val get_func_post : ?securised:bool -> string -> (bool array)*(bool array)

(** Sets the post condition of the given C function *)
val set_func_post : string -> (bool array)*(bool array) -> unit

(** Returns the post condition associated to the given C function *)
val get_func_post_bycase : ?securised:bool -> string -> (bool array array)*(bool array array)

(** Sets the pre condition of the given C function *)
val set_func_post_bycase : string -> (bool array array)*(bool array array) -> unit


(** Gives the specification of the call stmt in the given C function at the given StmtId. *)
val get_func_pre_call : string -> int -> (bool array)*(bool array)

(** Sets the specification of the call stmt in the given C function at the given StmtId. *)
val set_func_pre_call : string -> int -> (bool array)*(bool array) -> unit

(** Sets the specification of the call stmt in the given C function at the given StmtId. *)
val set_func_pre_call_bycase : string -> int -> (bool array array)*(bool array array) -> unit




(* ************************************************************************* *)
(**{b Pre and post condition of loops} In our point of view, the pre or
   the post condition are defined by the set of states authorized just
   before/after the loop (external pre/post), and by the set of states
   authorized just before/after the execution of the internal block of the
   loop (internal pre/post).
   The following functions manages these stored informations.
   Usually, the first array is for the authorized states, while the second one
   is for the crossable conditions. *)

(** Returns the pre condition associated to the given C function *)
val get_loop_ext_pre  : Cil_types.stmt Pervasives.ref -> (bool array)*(bool array)

(** Returns the pre condition associated to the given C function *)
val get_loop_int_pre  : Cil_types.stmt Pervasives.ref -> (bool array)*(bool array)

(** Sets the external or the block pre condition of the given loop *)
val set_loop_ext_pre  : Cil_types.stmt Pervasives.ref -> (bool array)*(bool array) -> unit

(** Sets the external or the block pre condition of the given loop *)
val set_loop_int_pre  : Cil_types.stmt Pervasives.ref -> (bool array)*(bool array) -> unit


(** Returns the post condition associated to the given C function *)
val get_loop_ext_post : Cil_types.stmt Pervasives.ref -> (bool array)*(bool array)

(** Returns the post condition associated to the given C function *)
val get_loop_int_post : Cil_types.stmt Pervasives.ref -> (bool array)*(bool array)

(** Sets the external or the block post condition of the given loop *)
val set_loop_ext_post : Cil_types.stmt Pervasives.ref -> (bool array)*(bool array) -> unit

(** Sets the external or the block post condition of the given loop *)
val set_loop_int_post : Cil_types.stmt Pervasives.ref -> (bool array)*(bool array) -> unit



(** Returns the pre condition associated to the given C function *)
val get_loop_ext_pre_bycase  : Cil_types.stmt Pervasives.ref -> (bool array array)*(bool array array)

(** Returns the pre condition associated to the given C function *)
val get_loop_int_pre_bycase  : Cil_types.stmt Pervasives.ref -> (bool array array)*(bool array array)


(** Sets the external or the block pre condition of the given loop *)
val set_loop_ext_pre_bycase  : Cil_types.stmt Pervasives.ref -> (bool array array)*(bool array array) -> unit

(** Sets the external or the block pre condition of the given loop *)
val set_loop_int_pre_bycase  : Cil_types.stmt Pervasives.ref -> (bool array array)*(bool array array) -> unit


(** Returns the post condition associated to the given C function *)
val get_loop_ext_post_bycase : Cil_types.stmt Pervasives.ref -> (bool array array)*(bool array array)

(** Returns the post condition associated to the given C function *)
val get_loop_int_post_bycase : Cil_types.stmt Pervasives.ref -> (bool array array)*(bool array array)


(** Sets the external or the block post condition of the given loop *)
val set_loop_ext_post_bycase : Cil_types.stmt Pervasives.ref -> (bool array array)*(bool array array) -> unit

(** Sets the external or the block post condition of the given loop *)
val set_loop_int_post_bycase : Cil_types.stmt Pervasives.ref -> (bool array array)*(bool array array) -> unit



(** Returns a stmt_ref list. It is the set of all registered loop in loop_specs hashtables *)
val get_loops_index : unit -> Cil_types.stmt Pervasives.ref list


(* ************************************************************************* *)
(**{b Enumeration management}*)

(** Given the name of an enumeration element, this function returns the associated cenum structure.
    This function is not efficient. Thus if the enumeration is known it is recommended to use one of the following functions.*)
val get_cenum_option : string -> Cil_types.constant option

(** Given the name of a C operation, this function returns the associated cenum structure. *)
val func_to_cenum : string -> Cil_types.constant

(** Given the name of a C operation status (Call or Return), this function returns the associated cenum structure. *)
val op_status_to_cenum : Promelaast.funcStatus -> Cil_types.constant


(** Given the name of a function, it return the name of the associated element in the operation list. *)
val func_to_op_func : string -> string

(** These functions are direct accesses to the table memorizing the enuminfo data associated to the name of an enumeration structure, from which cenum info are computed.*)
val set_usedinfo : string -> Cil_types.enuminfo -> unit

(** These functions are direct accesses to the table memorizing the enuminfo data associated to the name of an enumeration structure, from which cenum info are computed.*)
val get_usedinfo : string -> Cil_types.enuminfo


val removeUnusedTransitionsAndStates : unit -> unit



(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)

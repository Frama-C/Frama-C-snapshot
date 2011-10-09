(**************************************************************************)
(*                                                                        *)
(*  This file is part of Aorai plug-in of Frama-C.                        *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
(*    INSA  (Institut National des Sciences Appliquees)                   *)
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

open Promelaast

(** Module of data management used in all the plugin Aorai. Operations
    are mainly accessors for data. The use of this module is mainly done
    through the ltl_utils module. *)

(* ************************************************************************* *)
(** {2 LTL/Promela primitives} *)
(* ************************************************************************* *)

(** Here are some operations used for generation of LTL AST or Promela AST. *)

module Aorai_state: Datatype.S_with_collections with type t = Promelaast.state

(** Initializes some tables according to data from Cil AST. *)
val setCData : unit -> unit

(** *)
val add_logic : string -> Cil_types.logic_info -> unit

(** *)
val get_logic : string -> Cil_types.logic_info


(** *)
val add_predicate : string -> Cil_types.logic_info -> unit

(** *)
val get_predicate : string -> Cil_types.logic_info

(** Given a logic info representing a set of pebbles and a label, returns
    the term corresponding to evaluating the set at the label.
*)
val pebble_set_at:
  Cil_types.logic_info -> Cil_types.logic_label -> Cil_types.term

(** Global auxiliary variables generated during type-checking of transitions *)
val aux_variables: unit -> Cil_types.varinfo list

(** Global logic info generated during type-checking (mostly encoding of
    ghost variables having a logic type)
*)
val abstract_logic_info: unit -> Cil_types.logic_info list

(** {2 Smart constructors for conditions} *)

(**/**)
val pand: condition -> condition -> condition
val por: condition -> condition -> condition
val pnot: condition -> condition

val cst_one: expression
val cst_zero: expression

(** {2 Utilities for parsed_conditions } *)
(** [true] iff the expression is 1 *)
val is_cst_one: expression -> bool

val is_cst_zero: expression -> bool

(** [true] if the element is not repeating itself ([min_rep = max_rep = 1]) *)
val is_single: seq_elt -> bool

(* ************************************************************************* *)

(**{b Constants} Some constant names used for generation. *)

(** Returns a string guaranteed not to clash with C/ACSL keywords or an
    existing global.
    @since Nitrogen-20111001
 *)
val get_fresh: string -> string

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

val new_state: string -> state

val new_trans: state -> state -> 'a -> 'a trans

(** Return the buchi automata as stored after parsing *)
val getAutomata : unit -> Promelaast.typed_automaton

(** Type-checks the parsed automaton and stores the result.
    This might introduce new global variables in case of sequences.
*)
val setAutomata: Promelaast.parsed_automaton -> unit

(** return the number of transitions of the automata *)
val getNumberOfTransitions : unit -> int

(** return the number of states of the automata *)
val getNumberOfStates : unit -> int

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

(** returns the state of given index.
    @since Nitrogen-20111001
*)
val getState: int -> Promelaast.state

val getStateName : int -> string

(** returns the transition having the corresponding id.
    @raise Not_found if this is not the case.
*)
val getTransition:
  int -> (Promelaast.typed_condition * Promelaast.action) Promelaast.trans

(* ************************************************************************* *)
(**{b Variables information} Usually it seems very useful to access to varinfo
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

(** get the logic variable corresponding to its C counterpart.
    @since Nitrogen-20111001
*)
val get_logic_var: string -> Cil_types.logic_var

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

(** Given the representation of an auxiliary counter 
    (found in a {!Promelaast.Counter_incr}), returns the maximal value
    that it can take according to the automaton.
 *)
val find_max_value: Cil_types.term -> Cil_types.term option

(** information we have about the range of values that an auxiliary variable
    can take.
 *)
type range =
  | Fixed of int (** constant value *)
  | Interval of int * int (** range of values *)
  | Bounded of int * Cil_types.term
    (** range bounded by a logic term (depending on program parameter). *)
  | Unbounded of int (** only the lower bound is known,
                         there is no upper bound *)

module Range: Datatype.S_with_collections with type t = range

module Intervals: Datatype.S with type t = range Cil_datatype.Term.Map.t

module Vals: Datatype.S with type t = Intervals.t Cil_datatype.Term.Map.t

(** [get_action_path kf ki pre post] returns the possible values of the
   auxiliary variables that may have changed in a call of [kf] starting in [pre]
   at stmt [ki] in state [post].
*)
val get_action_bindings:
  Cil_types.kernel_function ->
  Cil_types.kinstr -> Promelaast.state -> Promelaast.state -> Vals.t

val all_action_bindings:
  unit ->
  ((Cil_types.kernel_function * Cil_types.kinstr *
      Promelaast.state * Promelaast.state) * Vals.t) list

(** sets the possible values of auxiliary variables at a given stmt. If
    there was a previous map, it is erased.
 *)
val set_action_bindings:
  Cil_types.kernel_function -> Cil_types.kinstr ->
  Promelaast.state -> Promelaast.state -> Vals.t -> unit

(** Given a term and a minimal value, returns the absolute range of variation
    of the corresponding auxiliary variable, depending on its usage in the
    instrumentation of the code.
*)
val absolute_range: Cil_types.term -> int -> Range.t

(** Given an auxiliary variable, a base for its variations and two ranges of
    variations, returns a range that encompasses both.
 *)
val merge_range:
  Cil_types.term -> Cil_types.term -> Range.t -> Range.t -> Range.t

(** sets the possible values of auxiliary variables at a given stmt. If
    there was a previous map, the content of both maps is merged.
 *)
val merge_action_bindings:
  Cil_types.kernel_function -> Cil_types.kinstr ->
  Promelaast.state -> Promelaast.state -> Vals.t -> unit

(** [get_action_path kf ki pre post] returns the possible values of the
   auxiliary variables that may have changed in a call of [kf] starting in [pre]
   at stmt [ki] in state [post]. Values are of the form [base, min, max],
   stating that the location has a value between [base+min] and [base+max].
   For counters that are initialized during the call, [base] is [0].
*)
val get_action_path:
  Cil_types.kernel_function ->
  Cil_types.kinstr -> Promelaast.state -> Promelaast.state ->
  (Cil_types.term * (Cil_types.term * range) list) list

(** gets a specific binding or raise Not_found if no such binding exist. *)
val get_action_path_binding:
  Cil_types.kernel_function ->
  Cil_types.kinstr -> Promelaast.state -> Promelaast.state ->
  Cil_types.term -> Cil_types.term -> range

(** Adds a new possible value for the given auxiliary variable in the given
   "execution path"
 *)
val add_action_path:
  Cil_types.kernel_function -> Cil_types.kinstr ->
  Promelaast.state -> Promelaast.state ->
  Cil_types.term -> (Cil_types.term * range) -> unit

(** Removes an unreachable path. *)
val remove_action_path:
  Cil_types.kernel_function -> Cil_types.kinstr ->
  Promelaast.state -> Promelaast.state -> unit

val clear_actions: unit -> unit

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
val get_loop_ext_pre  : Cil_types.stmt -> (bool array)*(bool array)

(** Returns the pre condition associated to the given C function *)
val get_loop_int_pre  : Cil_types.stmt -> (bool array)*(bool array)

(** Sets the external or the block pre condition of the given loop *)
val set_loop_ext_pre  : Cil_types.stmt -> (bool array)*(bool array) -> unit

(** Sets the external or the block pre condition of the given loop *)
val set_loop_int_pre  : Cil_types.stmt -> (bool array)*(bool array) -> unit

(** Returns the post condition associated to the given C function *)
val get_loop_ext_post : Cil_types.stmt -> (bool array)*(bool array)

(** Returns the post condition associated to the given C function *)
val get_loop_int_post : Cil_types.stmt -> (bool array)*(bool array)

(** Sets the external or the block post condition of the given loop *)
val set_loop_ext_post : Cil_types.stmt -> (bool array)*(bool array) -> unit

(** Sets the external or the block post condition of the given loop *)
val set_loop_int_post : Cil_types.stmt -> (bool array)*(bool array) -> unit

(** Returns the pre condition associated to the given C function *)
val get_loop_ext_pre_bycase  : Cil_types.stmt -> (bool array array)*(bool array array)

(** Returns the pre condition associated to the given C function *)
val get_loop_int_pre_bycase  : Cil_types.stmt -> (bool array array)*(bool array array)

(** Sets the external or the block pre condition of the given loop *)
val set_loop_ext_pre_bycase  :
  Cil_types.stmt -> (bool array array)*(bool array array) -> unit

(** Sets the external or the block pre condition of the given loop *)
val set_loop_int_pre_bycase  :
  Cil_types.stmt -> (bool array array)*(bool array array) -> unit

(** Returns the external post-condition of the given loop *)
val get_loop_ext_post_bycase :
  Cil_types.stmt -> (bool array array)*(bool array array)

(** Returns the block post condition of the given loop *)
val get_loop_int_post_bycase :
  Cil_types.stmt -> (bool array array)*(bool array array)

(** Sets the external or the block post condition of the given loop *)
val set_loop_ext_post_bycase :
  Cil_types.stmt -> (bool array array)*(bool array array) -> unit

(** Sets the external or the block post condition of the given loop *)
val set_loop_int_post_bycase :
  Cil_types.stmt -> (bool array array)*(bool array array) -> unit

(** Returns a stmt list. It is the set of all registered
    loops in loop_specs hashtables *)
val get_loops_index : unit -> Cil_types.stmt list

(* ************************************************************************* *)
(**{b Enumeration management}*)

(** Given the name of an enumeration element, this function returns the associated cenum structure.
    This function is not efficient. Thus if the enumeration is known it is recommended to use one of the following functions.*)
val get_cenum_option : string -> Cil_types.constant option

val func_enum_type: unit -> Cil_types.typ

val status_enum_type: unit -> Cil_types.typ

(** Given the name of a C operation, this function returns the
    associated cenum structure. *)
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

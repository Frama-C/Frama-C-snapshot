(**************************************************************************)
(*                                                                        *)
(*  This file is part of Aorai plug-in of Frama-C.                        *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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

open Cil_types
open Promelaast

(** Module of data management used in all the plugin Aorai. Operations
    are mainly accessors for data. The use of this module is mainly done
    through the ltl_utils module. *)

(* ************************************************************************* *)
(** {2 LTL/Promela primitives} *)
(* ************************************************************************* *)

(** Here are some operations used for generation of LTL AST or Promela AST. *)

module Aorai_state: Datatype.S_with_collections with type t = Promelaast.state

module Aorai_typed_trans: 
  Datatype.S_with_collections 
  with 
    type t = (Promelaast.typed_condition * Promelaast.action) Promelaast.trans

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

(** returns the C variable associated to a given state
    (non-deterministic mode only).
 *)
val get_state_var: state -> varinfo

(** returns the logic variable associated to a given state.
    (non-deterministic mode only).
*)
val get_state_logic_var: state -> logic_var

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

(** [true] iff the given state is the rejection state for automaton with
    sequences. *)
val is_reject_state: state -> bool

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

(** {2 Dataflow analysis} *)

val tlval: Cil_types.term_lval -> Cil_types.term

(** The propagated state: Mapping from possible start states 
    to reachable states, with 
     - set of states for the initial transition leading to the corresponding
       reachable state.
     - set of states for the last transition.
     - possible values for intermediate variables.
 *)
type end_state = 
    (Aorai_state.Set.t * Aorai_state.Set.t * Vals.t) Aorai_state.Map.t

module Case_state: 
  Datatype.S with type t = end_state Aorai_state.Map.t

type state = Case_state.t

val pretty_end_state: Aorai_state.t -> Format.formatter -> end_state -> unit

val pretty_state: Format.formatter -> state -> unit

(** [included_state st1 st2] is [true] iff [st1] is included in [st2], i.e:
  - possible start states of [st1] are included in [st2]
  - for each possible start state, reachable states in [st1] are included in
    the one of [st2]
  - for each possible path in [st1], range of possible values for intermediate
    variables are included in the corresponding one in [st2].
*)
val included_state: state -> state -> bool

(** merges two sets of possible bindings for aux variables *)
val merge_bindings: Vals.t -> Vals.t -> Vals.t

val merge_end_state: end_state -> end_state -> end_state

(** Merges two state: union of possible start states, of possible paths, and
    merge of ranges of possible values. *)
val merge_state: state -> state -> state


(** Register a new init state for kernel function.
    If there is already an init state registered, the new one is merged with
    the old.
 *)
val set_kf_init_state: Kernel_function.t -> state -> unit

(** Register a new end state for kernel function.
    If there is already an end state registered, the new one is merged with
    the old.
*)
val set_kf_return_state: Kernel_function.t -> state -> unit

(** sets the initial state when entering a loop (merging it if a state is
    already present. *)
val set_loop_init_state: Cil_types.stmt -> state -> unit

(** sets the invariant of a loop. *)
val set_loop_invariant_state: Cil_types.stmt -> state -> unit

val replace_kf_init_state: Kernel_function.t -> state -> unit

val replace_kf_return_state: Kernel_function.t -> state -> unit

val replace_loop_init_state: Cil_types.stmt -> state -> unit

val replace_loop_invariant_state: Cil_types.stmt -> state -> unit

val get_kf_init_state: Kernel_function.t -> state

val get_kf_return_state: Kernel_function.t -> state

val get_loop_init_state: Cil_types.stmt -> state

val get_loop_invariant_state: Cil_types.stmt -> state

val debug_computed_state: ?dkey:Log.category -> unit -> unit
(** Pretty-prints all computed states. Default key is dataflow. *) 

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

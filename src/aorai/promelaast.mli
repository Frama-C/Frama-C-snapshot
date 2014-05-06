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

(** The abstract tree of promela representation. Such tree is used by promela 
    parser/lexer before its translation into Data_for_aorai module. *)

type expression =
  | PVar of string
  | PPrm of string * string (* f().N *)
  | PCst of Logic_ptree.constant
  | PBinop of Logic_ptree.binop * expression * expression
  | PUnop of Logic_ptree.unop * expression
  | PArrget of expression * expression
  | PField of expression * string
  | PArrow of expression * string

type condition =
  | PRel of Logic_ptree.relation * expression * expression
  | PTrue
  | PFalse
  | POr of condition * condition
  | PAnd of condition * condition
  | PNot of condition
  | PCall of string * string option 
      (** Call might be done in a given behavior *)
  | PReturn of string

and seq_elt =
    { condition: condition option;
      nested: sequence;
      min_rep: expression option;
      max_rep: expression option;
    }

and sequence = seq_elt list

(** Promela parsed abstract syntax trees. Either a sequence of event or the
    otherwise keyword. A single condition is expressed with a singleton 
    having an empty nested sequence and min_rep and max_rep being equal to one.
*)
type parsed_condition = Seq of sequence | Otherwise

type typed_condition =
    | TOr of typed_condition * typed_condition  (** Logical OR *)
    | TAnd of typed_condition * typed_condition (** Logical AND *)
    | TNot of typed_condition             (** Logical NOT *)
    | TCall of Cil_types.kernel_function * Cil_types.funbehavior option
    (** Predicate modelling the call of an operation *)
    | TReturn of Cil_types.kernel_function
    (** Predicate modelling the return of an operation *)
    | TTrue                         (** Logical constant TRUE *)
    | TFalse                        (** Logical constant FALSE *)
    | TRel of Cil_types.relation * Cil_types.term * Cil_types.term
       (** Condition. If one of the terms contains TResult, TRel is in 
           conjunction with exactly one TReturn event, and the TResult is
           tied to the corresponding value.
        *)

type single_action =
  | Counter_init of Cil_types.term_lval
  | Counter_incr of Cil_types.term_lval
  | Pebble_init of
      Cil_types.logic_info * Cil_types.logic_var * Cil_types.logic_var
      (** adds a new pebble. [Pebble_init(set,aux,count)] indicates that
          pebble [count] is put in [set] whose content is governed by C
          variable [aux].
       *)
  | Pebble_move of
      Cil_types.logic_info * 
        Cil_types.logic_var * Cil_types.logic_info * Cil_types.logic_var
        (** [Pebble_move(new_set,new_aux,old_set,old_aux)] 
            moves pebbles from [old_set] to [new_set], governed by the
            corresponding aux variables. *)
  | Copy_value of Cil_types.term_lval * Cil_types.term
      (** copy the current value of the given term into the given location
          so that it can be accessed by a later state. *)

(** Additional actions to perform when crossing a transition.
    There is at most one Pebble_* action for each transition, and
    each transition leading to a state with multi-state has such an action.
 *)
type action = single_action list

(** Internal representation of a State from the Buchi automata. *)
type state =
    { name : string            (** State name *);
      mutable acceptation : Bool3.t
      (** True iff state is an acceptation state *);
      mutable init : Bool3.t   (** True iff state is an initial state *);
      mutable nums : int;       (** Numerical ID of the state *)
      mutable multi_state: 
        (Cil_types.logic_info * Cil_types.logic_var) option
        (** Translation of some sequences might lead to some kind of pebble
            automaton, where we need to distinguish various branches. This is
            done by having a set of pebbles instead of just a zero/one switch
            to know if we are in the given state. The guards apply to each
            active pebble and are thus of the form 
            \forall integer x; in(x,multi_state) ==> guard.
            multi_state is the first lvar of the pair, x is the second
         *)
    }

(** Internal representation of a transition from the Buchi automata. *)
type 'condition trans = 
    { start : state ;     (** Starting state of the transition *)
      stop : state ;      (** Ending state of the transition *)
      mutable cross : 'condition ; (** Cross condition of the transition *)
      mutable numt : int  (** Numerical ID of the transition *)
    }

(** Internal representation of a Buchi automata : a list of states and a list of transitions.*)
type 'condition automaton = (state list) * ('condition trans list)

type parsed_automaton = parsed_condition automaton

type typed_automaton = (typed_condition * action) automaton

(** An operation can have two status: currently calling or returning. *)
type funcStatus =
    | Call
    | Return

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
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
(**************************************************************************)

(* $Id: promelaast.mli,v 1.4 2008/10/13 09:21:24 uid588 Exp $ *)

(** The abstract tree of promela representation. Such tree is used by promela parser/lexer before its translation into Data_for_ltl module. *)

open Bool3

(** Promela parsed abstract syntax trees *)
type condition =  
    | POr of condition * condition  (** Logical OR *)
    | PAnd of condition * condition (** Logical AND *)
    | PNot of condition             (** Logical NOT *)
    | PCall of string               (** Predicate modelling the call of an operation *)
    | PReturn of string             (** Predicate modelling the return of an operation *)
    | PCallOrReturn of string       (** Predicate modelling the call or the return of an operation *)
    | PTrue                         (** Logical constant TRUE *)
    | PFalse                        (** Logical constant FALSE *)
    | PIndexedExp of string         (** Variable introduced during ltl pre-process. It correponds to an expression lmanaged by the Data_for_ltl module. *)


(** Internal representation of a State from the Buchi automata. *)
type state = {name : string ;               (** State name *)
	      mutable acceptation : bool3 ; (** True iff state is an acceptation state *)
	      mutable init : bool3 ;        (** True iff state is an initial state *)
	      nums : int                    (** Numerical ID of the state *)
	     }

(** Internal representation of a transition from the Buchi automata. *)
type trans = { start : state ;     (** Starting state of the transition *)
	       stop : state ;      (** Ending state of the transition *)
	       cross : condition ; (** Cross condition of the transition *)
	       mutable numt : int  (** Numerical ID of the transition *)
	     }

(** Internal representation of a Buchi automata : a list of states and a list of transitions.*)
type buchautomata = (state list) * (trans list)


(** An operation can have two status: currently calling or returning. *)
type funcStatus = 
    | Call 
    | Return



(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)

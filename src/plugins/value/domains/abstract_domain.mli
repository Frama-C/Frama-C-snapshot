(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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

(** Abstract domains of the analysis. *)

(** An abstract domain is a collection of abstract states propagated through
    the control flow graph by a dataflow analysis. At a program point, they
    are abstractions of the set of possible concrete states that may arise
    during any execution of the program.

    In EVA, different abstract domains may communicate through alarms, values
    and locations.
    Alarms report undesirable behaviors that may occur during an execution
    of the program. They are defined in {!Alarmset}, while values and locations
    depend on the domain.
    Values are numerical and non-relational abstractions of the set of the
    possible values of expressions at a program point. Locations are similar
    abstractions for memory locations. The main values and locations used
    in the analyzer are respectively available in {!Main_values} and
    {!Main_locations}. Values and locations abstractions are extensible, should
    a domain requires new abstractions. Their signature are in
    {!Abstract_value.S} and {!Abstract_location.S}.

    Lvalues and expressions are cooperatively evaluated into locations and
    values using all the information provided by all domains. These computed
    values and locations are then available for the domain transformers which
    model the action of statements on abstract states.
    However, a domain could ignore this mechanism; its values and locations
    should then be the unit type.


    This file gathers the definition of the module types for an abstract
    domain.

    The module type {!S} requires all the functions to be implemented to define
    the abstract semantics of a domain, divided in three categories:
    - {!Lattice} gives a semi-lattice structure to the abstract states;
    - {!Queries} extracts information from a state, by giving a value to an
      expression.
    - {!Transfer} are the transfer functions of the domain for assignments,
      assumptions and function calls. It is a functor from a {!Valuation}
      module, which are maps containing all locations and values computed by
      the evaluation of the expressions involved in the considered statement.

    The module type {!S_with_Structure} is {!S}, plus a special OCaml value
    describing the internal structure of the domain and identifying it.
    This structure enables automatic accessors to the domain when
    combined to others. See {!Structure} for details.
    {!S_with_Structure} is the interface to implement in order to introduce
    a now domain in EVA.

    The module type {!Internal} contains some other functionnalities needed by
    the analyzer, but that can be automatically generated from the previous
    one. The functor {!Domain_builder.Complete} produces an {!Internal} module
    from a {!S_with_Structure} one.

    {!Internal} modules can then be lifted on more general values and locations
    through {!Domain_lift.Make}, and be combined through {!Domain_product.Make}.

    Finally, {!External} is the type of the final modules built and used by EVA.
    It contains the generic accessors to specific domains, described in
    {!Interface}.
*)

(* The types of the Cil AST. *)
open Cil_types

(* Definition of the types frequently used in EVA. *)
open Eval


(** Lattice structure of a domain. *)
module type Lattice = sig
  type state

  val top: state
  (** Greatest element. *)
  val is_included: state -> state -> bool
  (** Inclusion test. *)
  val join: state -> state -> state
  (** Semi-lattice structure. *)
  val join_and_is_included: state -> state -> state * bool
  (** Do both operations simultaneously *)
  val widen: kernel_function -> stmt -> state -> state -> state
  (** [widen h t1 t2] is an over-approximation of [join t1 t2].
      Assumes [is_included t1 t2] *)
end


(** Extraction of information: queries for values or locations inferred by a
    domain about expressions and lvalues.
    Used in the evaluation of expressions and lvalues. *)
module type Queries = sig

  type state     (** Domain state. *)
  type value     (** Numerical values to which the expressions are evaluated. *)
  type location  (** Abstract memory locations associated to left values. *)

  (** The [origin] is used by the domain combiners to track the origin
       of a value. An abstract domain can always use a dummy type unit for
       origin, or use it to encode some specific information about a value. *)
  type origin

  (** Queries functions return a pair of:
      - the set of alarms that ensures the of the soundness of the evaluation
        of [exp]. [Alarmset.all] is always a sound over-approximation of these
        alarms.
      - a value for the expression, which can be:
        - `Bottom if its evaluation is infeasible;
        - `Value (v, o) where [v] is an over-approximation of the abstract
           value of the expression [exp], and [o] is the origin of the value. *)

  (** Query function for compound expressions:
      [eval oracle t exp] returns the known value of [exp] by the state [t].
      [oracle] is an evaluation function and can be used to find the answer
      by evaluating some others expressions, especially by relational domain.
      No recursive evaluation should be done by this function. *)
  val extract_expr :
    (exp -> value evaluated) ->
    state -> exp -> (value * origin) evaluated

  (** Query function for lvalues:
      [find oracle t lval typ loc] returns the known value stored at
      the location [loc] of the left value [lval] of type [typ]. *)
  val extract_lval :
    (exp -> value evaluated) ->
    state -> lval -> typ -> location -> (value * origin) evaluated

  (** [backward_location state lval typ loc v] reduces the location [loc] of the
      lvalue [lval] of type [typ], so that only the locations that may have value
      [v] are kept.
      The returned location must be included in [loc], but it is always sound
      to return [loc] itself.
      Also returns the value that may have the returned location, if not bottom. *)
  val backward_location :
    state -> lval -> typ -> location -> value -> (location * value) or_bottom

  (** Given a reduction [expr] = [value], provides more reductions that may
      be performed. *)
  val reduce_further : state -> exp -> value -> (exp * value) list

end


(** Transfer function of the domain. *)
module type Transfer = sig

  type state
  type return
  type value
  type location
  type valuation

  (** [update valuation t] updates the state [t] by the values of expressions
      and the locations of lvalues stored in [valuation]. *)
  val update : valuation -> state -> state

  (** [assign kinstr lv expr v valuation state] is the transfer function for the
      assignment [lv = expr] for [state]. It must return the state where the
      assignment has been performed.
      - [kinstr] is the statement of the assignment, or Kglobal for the
        initialization of a global variable.
      - [v] carries the value being assigned to [lv], i.e. the value of the
        expression [expr]. [v] also denotes the kind of assignement: Assign for
        the default assignment of the value, or Copy for the exact copy of a
        location if the right expresssion [expr] is a lvalue.
      - [valuation] is a cache of all sub-expressions and locations computed
        for the evaluation of [lval] and [expr]; it can also be used to reduce
        the state. *)
  val assign :
    kinstr -> location left_value -> exp -> value assigned ->
    valuation -> state -> state or_bottom

  (** Transfer function for an assumption.
      [assume stmt expr bool valuation state] returns a state in which the
      boolean expression [expr] evaluates to [bool].
      - [stmt] is the statement of the assumption.
      - [valuation] is a cache of all sub-expressions and locations computed
        for the evaluation and the reduction of [expr]; it can also be used
        to reduce the state *)
  val assume : stmt -> exp -> bool -> valuation -> state -> state or_bottom

  (** Decision on a function call:
      [start_call stmt call valuation state] decides on the analysis of
      a call site. It returns either an initial state for a standard dataflow
      analysis of the called function, or a direct result for the entire call.
      See {!Eval.call_action} for details.
      - [stmt] is the statement of the call site;
      - [call] represents the call: the called function and the arguments;
      - [state] is the abstract state before the call;
      - [valuation] is a cache for all values and locations computed during
        the evaluation of the function and its arguments. *)
  val start_call:
    stmt -> value call -> valuation -> state ->
    (state, return, value) call_action

  (** [finalize_call stmt call ~pre ~post] computes the state after a function
      call, given the state [pre] before the call, and the state [post] at the
      end of the called function.
      - [stmt] is the statement of the call site;
      - [call] represents the function call and its arguments.
      - [pre] and [post] are the states before and at the end of the call
      respectively.
  *)
  val finalize_call:
    stmt -> value call -> pre:state -> post:state -> state or_bottom

  (** [make_return kf stmt assigned valuation state] makes an abstraction of the
      return value of the function [kf].
      [stmt] is the return statement of [kf] and [state] the state of the
      domain at this statement. [assigned] represents the value of the lvalue
      being returned, and [valuation] is the valuation resulting from its
      evaluation.
      The return abstraction computed by this function can then be used for
      the assignment at the call site. *)
  val make_return:
    kernel_function -> stmt -> value assigned -> valuation -> state -> return

  (** [assign_return stmt lv kf return value valuation state] models on states
      the effect of the assignment of the return value of a called function at
      the call site.
      - [stmt] is the statement of the call;
      - [lv] is the lvalue being assigned, with its type and location;
      - [kf] is the called function, whose return is assigned;
      - [return] is an abstraction of this return, computed by make_return;
      - [value] is a value abstraction for this return, computed by a standard
        evaluation in the called function [kf];
      - [valuation] results from the evaluation of the location of [lv];
      - [state] is the state before the assignment, but after the call. *)
  val assign_return:
    stmt -> location left_value ->
    kernel_function -> return -> value assigned ->
    valuation -> state -> state or_bottom

  val default_call:
    stmt -> value call -> state ->
    (state, return, value) call_result

  val enter_loop: stmt -> state -> state
  val incr_loop_counter: stmt -> state -> state
  val leave_loop: stmt -> state -> state

end


(** Logic evaluation. Temporary API.
    TODO: factorization of these functions for generic abstract domain. *)
module type Logic = sig
  type state
  type eval_env (** Evaluation environment. *)
  val env_current_state: eval_env -> state or_bottom
  val env_annot: pre:state -> here:state -> unit -> eval_env
  val env_pre_f: pre:state -> unit -> eval_env
  val env_post_f: pre:state -> post:state -> result:varinfo option ->  unit -> eval_env
  val eval_predicate: eval_env -> predicate -> Alarmset.status
  val reduce_by_predicate: eval_env -> bool -> predicate -> eval_env
end

(** Results of an evaluation: the results of all intermediate calculation (the
    value of each expression and the location of each lvalue) are cached in a
    map. *)
module type Valuation = sig
  type t
  type value  (** Abstract value. *)
  type origin (** Origin of abstract values. *)
  type loc    (** Abstract memory location. *)
  val find : t -> exp -> (value, origin) record_val or_top
  val fold : (exp -> (value, origin) record_val -> 'a -> 'a) -> t -> 'a -> 'a
  val find_loc : t -> lval -> loc record_loc or_top
end

(** Signature for the abstract domains of the analysis. *)
module type S = sig
  type state
  include Datatype.S_with_collections with type t = state

  (** {3 Lattice Structure } *)
  include Lattice with type state := t

  (** {3 Queries } *)
  include Queries with type state := t

  (** Abstraction of the return value of a function.
      Allows information to flow from the return statement of a called function
      to the assignment of the call site. *)
  type return

  (** Datatypes *)
  module Return : Datatype.S with type t = return

  (** {3 Transfer Functions } *)

  (** Transfer functions from the result of evaluations.
      See {eval.mli} for more details about valuation. *)
  module Transfer
      (Valuation: Valuation with type value = value
                             and type origin = origin
                             and type loc = location)
    : Transfer with type state = t
                and type return = return
                and type value = value
                and type location = location
                and type valuation = Valuation.t


  (** {3 Logic } *)

  (* TODO: revise this signature. *)
  val compute_using_specification:
    kinstr -> kernel_function * funspec -> t ->
    (t, return, value) call_result

  include Logic with type state := t

  (** {3 Miscellaneous } *)

  (** Scoping: abstract transformers for entering and exiting blocks.
      [kf] is the englobing function, and the variables of the list [vars]
      should be added or removed from the abstract state here.
      Note that the formals of a function enter the scope through the transfer
      function {!Transfer.start_call}, but leave it through a
      call to {!leave_scope}. *)
  val enter_scope: kernel_function -> varinfo list -> t -> t
  val leave_scope: kernel_function -> varinfo list -> t -> t

  (** Initialization *)
  val empty: unit -> t
  val initialize_var: t -> lval -> location -> (value * bool) or_bottom -> t
  val initialize_var_using_type: t -> varinfo -> t
  val global_state: unit -> (t or_bottom) option

  (** Mem exec. *)
  val filter_by_bases: Base.Hptset.t -> t -> t
  val reuse: current_input:t -> previous_output:t -> t
end


(** Keys identify abstract domains through the type of their abstract values. *)
type 'a key = 'a Structure.Key_Domain.k

(** Describes the internal structure of a domain.
    Used internally to automatically generate efficient accessors from a
    generic abstract domain, which may be a combination of several domains,
    to a specific one. *)
type 'a structure = 'a Structure.Key_Domain.structure =
  | Void : 'a structure
  | Leaf : 'a key -> 'a structure
  | Node : 'a structure * 'b structure -> ('a * 'b) structure

(**
   For a simple 'leaf' domain, the structure should be:
     [let key = Structure.Key_Domain.create_key "name_of_the_domain";;
      let structure = Leaf key;;]

   Then, the key should be exported by the domain, to allow the use of the
   functions defined in the {!Interface} interface below.

   A compound domain may use the {!Node} constructor to provide a separate
   access to each of its parts.

   A domain can also use the {!Void} constructor to prevent access to itself.
*)

(** Structure of a domain. *)
module type S_with_Structure = sig
  include S
  (** A structure matching the type of the domain. *)
  val structure : t structure
end

(** External interface of a domain, with accessors.
    Automatically built by the functor {!Structure.Open}.
    When a generic domain is a combination of several domains, these functions
    allow interacting with its subparts. Note that their behavior is undefined
    if the overall domain contains several times the same domain. *)
module type Interface = sig
  type t

  (** Tests whether a key belongs to the domain. *)
  val mem : 'a key -> bool

  (** For a key of type [k key]:
      - if the states of this domain (of type [t]) contain a part (of type [k])
        from the domain identified by the key,
        then [get key] returns an accessor for this part of a state.
      - otherwise, [get key] returns None. *)
  val get : 'a key -> (t -> 'a) option

  (** For a key of type [k key]:
      - if the states of this domain (of type [t]) contain a part (of type [k])
        from the domain identified by the key,
        then [set key d state] returns the state [state] in which this part
        has been replaced by [d].
      - otherwise, [set key _] is the identity function. *)
  val set : 'a key -> 'a -> t -> t
end

(** Automatic storage of the states computed during the analysis. *)
module type Store = sig
  type state
  val register_initial_state: Value_types.callstack -> state -> unit
  val register_state_before_stmt: Value_types.callstack -> stmt -> state -> unit
  val register_state_after_stmt: Value_types.callstack -> stmt -> state -> unit

  val get_initial_state: kernel_function -> state or_bottom
  val get_initial_state_by_callstack:
    kernel_function -> state Value_types.Callstack.Hashtbl.t option

  val get_stmt_state: stmt -> state or_bottom
  val get_stmt_state_by_callstack:
    after:bool -> stmt -> state Value_types.Callstack.Hashtbl.t option
end

(** Full implementation of domains. Automatically built by
    {!Domain_builder.Complete} from an {!S_with_Structure} domain. *)
module type Internal = sig
  include S_with_Structure
  module Store: Store with type state := state
end

(** Final interface of domains, as generated and used by EVA, with generic
    accessors for domains. *)
module type External = sig
  include Internal
  include Interface with type t := state
end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)

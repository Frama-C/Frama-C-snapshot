(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

(** Database in which static plugins are registered.
    @plugin development guide *)

(**
   Modules providing general services:
   - {!Dynamic}: API for plug-ins linked dynamically
   - {!Journal}: journalisation
   - {!Log}: message outputs and printers
   - {!Plugin}: general services for plug-ins
   - {!Project} and associated files: {!Kind}, {!Datatype} and {!State_builder}.

   Other main kernel modules:
   - {!Ast}: the cil AST
   - {!Ast_info}: syntactic value directly computed from the Cil Ast
   - {!File}: Cil file initialization
   - {!Globals}: global variables, functions and annotations
   - {!Annotations}: annotations associated with a statement
   - {!Properties_status}: status of annotations
   - {!Kernel_function}: C functions as seen by Frama-C
   - {!Stmts_graph}: the statement graph
   - {!Loop}: (natural) loops
   - {!Visitor}: frama-c visitors
   - {!Kernel}: general parameters of Frama-C (mostly set from the command
   line)
*)

open Cil_types
open Cil_datatype

(* ************************************************************************* *)
(** {2 Registering} *)
(* ************************************************************************* *)

(** How to journalize the given function.
    @since Beryllium-20090601-beta1 *)
type 'a how_to_journalize =
  | Journalize of string * 'a Type.t
      (** Journalize the value with the given name and type. *)
  | Journalization_not_required
      (** Journalization of this value is not required
          (usually because it has no effect on the Frama-C global state). *)
  | Journalization_must_not_happen of string
      (** Journalization of this value should not happen
          (usually because it is a low-level function: this function is always
          called from a journalized function).
          The string is the function name which is used for displaying suitable
          error message. *)

val register: 'a how_to_journalize -> 'a ref -> 'a -> unit
  (** Plugins must register values with this function. *)

val register_compute:
  string ->
  State.t list ->
  (unit -> unit) ref -> (unit -> unit) -> State.t
  (** @modify Boron-20100401 now return the state of the computation. *)

val register_guarded_compute:
  string ->
  (unit -> bool) ->
  (unit -> unit) ref -> (unit -> unit) -> unit

(** Frama-C main interface.
    @since Lithium-20081201 
    @plugin development guide *)
module Main: sig

  val extend : (unit -> unit) -> unit
    (** Register a function to be called by the Frama-C main entry point.
        @plugin development guide *)

  val play: (unit -> unit) ref
    (** Run all the Frama-C analyses. This function should be called only by
        toplevels.
        @since Beryllium-20090901 *)

  (**/**)
  val apply: unit -> unit
    (** Not for casual user. *)
  (**/**)

end

module Toplevel: sig

  val run: ((unit -> unit) -> unit) ref
    (** Run a Frama-C toplevel playing the game given in argument (in
        particular, applying the argument runs the analyses).
        @since Beryllium-20090901 *)

end

(* ************************************************************************* *)
(** {2 Graphs} *)
(* ************************************************************************* *)

(** Callgraph computed by value analysis. It contains function pointers! *)
module Semantic_Callgraph : sig
  val dump: (unit -> unit) ref
    (** Dump the semantic callgraph in stdout or in a file. *)

  val topologically_iter_on_functions : ((kernel_function -> unit) -> unit) ref
    (** Compute values if required. *)

  val iter_on_callers :
    ((kernel_function -> unit) -> kernel_function -> unit) ref
    (** Compute values if required. *)

  val accept_base :
    (with_formals:bool ->
     with_locals:bool ->
     kernel_function -> Base.t -> bool
     (** [accept_base formals locals kf b] returns [true] if and only [b] is
         - a global
         - a formal or local of one of the callers of [kf]
         - a formal or local of [kf] and the corresponding argument is [true]
     *)
    ) ref
end

(* ************************************************************************* *)
(** {2 Values} *)
(* ************************************************************************* *)

(** The Value analysis itself.
    @see <../value/index.html> internal documentation. *)
module Value : sig

  type state = Cvalue.Model.t
      (** Internal state of the value analysis. *)

  type t = Cvalue.V.t
      (** Internal representation of a value. *)

  exception Aborted

  val self : State.t
    (** Internal state of the value analysis from projects viewpoint.
        @plugin development guide *)

  val mark_as_computed: unit -> unit
    (** Indicate that the value analysis has been done already. *)

  val compute : (unit -> unit) ref
    (** Compute the value analysis using the entry point of the current
        project. You may set it with {!Globals.set_entry_point}.
        @raise Globals.No_such_entry_point if the entry point is incorrect
        @raise Db.Value.Incorrect_number_of_arguments if some arguments are
        specified for the entry point using {!Db.Value.fun_set_args}, and
        an incorrect number of them is given.
        @plugin development guide *)

  val is_computed: unit -> bool
    (** Return [true] iff the value analysis has been done.
        @plugin development guide *)

  module Table:
    State_builder.Hashtbl with type key = stmt and type data = state
    (** Table containing the results of the value analysis, ie.
        the state before the evaluation of each reachable statement. *)

  module AfterTable:
    State_builder.Hashtbl with type key = stmt and type data = state
    (** Table containing the state of the value analysis after the evaluation
        of each reachable and evaluable statement. Filled only if
        [Value_parameters.ResultsAfter] is set. *)

  val ignored_recursive_call: kernel_function -> bool
  (** This functions returns true if the value analysis found and ignored
      a recursive call to this function during the analysis. *)

  val condition_truth_value: stmt -> bool * bool
  (** Provided [stmt] is an 'if' construct, [fst (condition_truth_value stmt)]
      (resp. snd) is true if and only if the condition of the 'if' has been
      evaluated to true (resp. false) at least once during the analysis. *)

  (** {3 Parameterization} *)

  exception Outside_builtin_possibilities
 
  (** Type for a Value builtin function *)
  type builtin_sig =
      (** Memory state at the beginning of the function *)
      state ->
      (** Args for the function: the expressions corresponding to the formals
          of the functions at the call site, the actual value of those formals,
          and a more precise view of those formals using offsetmaps (for eg.
          structs)  *)
      (Cil_types.exp * Cvalue.V.t * Cvalue.V_Offsetmap.t) list ->
    Value_types.call_result

  val register_builtin: (string -> builtin_sig -> unit) ref
    (** [!record_builtin name ?override f] registers an abstract function [f]
        to use everytime a C function named [name] is called in the program.
        See also option [-val-builtin] *)

  val mem_builtin: (string -> bool) ref
  val use_spec_instead_of_definition: (kernel_function -> bool) ref
  (** To be called by derived analyses to determine if they must use
      the body of the function (if available), or only its spec. Used for
      value builtins, and option -val-use-spec. *)


  (** {4 Arguments of the main function} *)

  (** The functions below are related to the arguments that are passed to the
      function that is analysed by the value analysis. Specific arguments
      are set by [fun_set_args]. Arguments reset to default values when
      [fun_use_default_args] is called, when the ast is changed, or
      if the options [-libentry] or [-main] are changed. *)

  (** Specify the arguments to use. This function is not journalized, and
      will generate an error when the journal is replayed *)
  val fun_set_args : t list -> unit

  val fun_use_default_args : unit -> unit

  (** For this function, the result [None] means that
      default values are used for the arguments. *)
  val fun_get_args : unit -> t list option

  exception Incorrect_number_of_arguments
  (** Raised by [Db.Compute] when the arguments set by [fun_set_args]
      are not coherent with the prototype of the function (if there are
      too few or too many of them) *)


  (** {4 Initial state of the analysis} *)

  (** The functions below are related to the the value of the global variables
      when the value analysis is started. If [globals_set_initial_state] has not
      been called, the given state is used. A default state (which depends on
      the option [-libentry]) is used when [globals_use_default_initial_state]
      is called, or when the ast changes. *)

  (** Specify the initial state to use. This function is not journalized,
      and will generate an error when the journal is replayed *)
  val globals_set_initial_state : state -> unit

  val globals_use_default_initial_state : unit -> unit

  (** Initial state used by the analysis *)
  val globals_state : unit -> state


  (** @return [true] if the initial state for globals used by the value
      analysis has been supplied by the user (through
      [globals_set_initial_state]), or [false] if it is automatically
      computed by the value analysis *)
  val globals_use_supplied_state : unit -> bool


  (** {3 Getters} *)
  (** State of the analysis at various points *)

  val get_initial_state : kernel_function -> state
  val get_state : kinstr -> state

    
  val get_stmt_state_callstack: 
    after:bool -> stmt -> state Value_types.Callstack.Hashtbl.t option

  val get_stmt_state : stmt -> state
  (** @plugin development guide *)

  val find : state -> Locations.location ->  t

  (** {3 Evaluations} *)

  val eval_lval :
    (with_alarms:CilE.warn_mode ->
     Locations.Zone.t option ->
     state ->
     lval ->
     Locations.Zone.t option * t) ref

  val eval_expr :
    (with_alarms:CilE.warn_mode -> state -> exp -> t) ref

  val eval_expr_with_state :
    (with_alarms:CilE.warn_mode -> state -> exp -> state * t) ref

  val find_lv_plus :
    (with_alarms:CilE.warn_mode ->
      Cvalue.Model.t -> Cil_types.exp ->
      (Cil_types.lval * Ival.t) list) ref
  (** returns the list of all decompositions of [expr] into the sum an lvalue
      and an interval. *)

  (** {3 Values and kernel functions} *)

  val expr_to_kernel_function :
    (kinstr
     -> with_alarms:CilE.warn_mode
     -> deps:Locations.Zone.t option
     -> exp
     -> Locations.Zone.t * Kernel_function.Hptset.t) ref

  val expr_to_kernel_function_state :
    (state
     -> deps:Locations.Zone.t option
     -> exp
     -> Locations.Zone.t * Kernel_function.Hptset.t) ref

  exception Not_a_call
  val call_to_kernel_function : stmt -> Kernel_function.Hptset.t
    (** Return the functions that can be called from this call.
        @raise Not_a_call if the statement is not a call. *)

  val valid_behaviors: (kernel_function -> state -> funbehavior list) ref

  val add_formals_to_state: (state -> kernel_function -> exp list -> state) ref
    (** [add_formals_to_state state kf exps] evaluates [exps] in [state]
        and binds them to the formal arguments of [kf] in the resulting
        state *)

  (** {3 Reachability} *)

  val is_accessible : kinstr -> bool
  val is_reachable : state -> bool
  (** @plugin development guide *)

  val is_reachable_stmt : stmt -> bool

  (** {3 About kernel functions} *)

  exception Void_Function
  val find_return_loc : kernel_function -> Locations.location
    (** Return the location of the returned lvalue of the given function.
        @raise Void_Function if the function does not return any value. *)

  val is_called: (kernel_function -> bool) ref

  val callers: (kernel_function -> (kernel_function*stmt list) list) ref
    (** @return the list of callers with their call sites. Each function is
        present only once in the list. *)

  (** {3 State before a kinstr} *)

  val access : (kinstr -> lval ->  t) ref
  val access_expr : (kinstr -> exp ->  t) ref
  val access_location : (kinstr -> Locations.location ->  t) ref


  (** {3 Locations of left values} *)

  val lval_to_loc :
    (kinstr -> with_alarms:CilE.warn_mode -> lval -> Locations.location) ref

  val lval_to_loc_with_deps :
    (kinstr
     -> with_alarms:CilE.warn_mode
      -> deps:Locations.Zone.t
      -> lval
      -> Locations.Zone.t * Locations.location) ref

  val lval_to_loc_with_deps_state :
    (state
      -> deps:Locations.Zone.t
      -> lval
      -> Locations.Zone.t * Locations.location) ref

  val lval_to_loc_state :
    (state -> lval -> Locations.location) ref

  val lval_to_offsetmap :
    ( kinstr -> lval -> with_alarms:CilE.warn_mode ->
      Cvalue.V_Offsetmap.t option) ref

  val lval_to_offsetmap_state :
    (state -> lval -> Cvalue.V_Offsetmap.t option) ref
    (** @since Carbon-20110201 *)

  val lval_to_zone :
    (kinstr -> with_alarms:CilE.warn_mode -> lval -> Locations.Zone.t) ref

  val lval_to_zone_state :
    (state -> lval -> Locations.Zone.t) ref
    (** Does not emit alarms. *)

  val lval_to_zone_with_deps_state:
    (state -> for_writing:bool -> deps:Locations.Zone.t option -> lval ->
     Locations.Zone.t * Locations.Zone.t * bool) ref
  (** [lval_to_zone_with_deps_state state ~for_writing ~deps lv] computes
      [res_deps, zone_lv, exact], where [res_deps] are the memory zones needed
      to evaluate [lv] in [state] joined  with [deps]. [zone_lv] contains the
      valid memory zones that correspond to the location that [lv] evaluates
      to in [state]. If [for_writing] is true, [zone_lv] is restricted to
      memory zones that are writable. [exact] indicates that [lv] evaluates
      to a valid locatio of cardinal at most one. *)

  (** Evaluation of the [\from] clause of an [assigns] clause.*)
  val assigns_inputs_to_zone :
    (state -> identified_term assigns -> Locations.Zone.t) ref

  (** Evaluation of the left part of [assigns] clause (without [\from]).*)
  val assigns_outputs_to_zone :
    (state -> result:varinfo option -> identified_term assigns -> Locations.Zone.t) ref

  (** Evaluation of the left part of [assigns] clause (without [\from]). Each
      assigns term results in one location. *)
  val assigns_outputs_to_locations :
    (state -> result:varinfo option -> identified_term assigns -> Locations.location list) ref


  (** {3 Evaluation of logic terms and predicates} *)
  module Logic : sig
  (** The APIs of this module are not stabilized yet, and are subject
      to change between Frama-C versions. *)

    val eval_predicate:
      (pre:state -> here:state -> predicate named ->
       Property_status.emitted_status) ref
      (** Evaluate the given predicate in the given states for the Pre
          and Here ACSL labels.
	  @since Neon-20130301 *)
  end


  (** {3 Callbacks} *)

  type callstack = Value_types.callstack

  (** Actions to perform at end of each function analysis. Not compatible with
      option [-memexec-all] *)

  module Record_Value_Callbacks:
    Hook.Iter_hook with type param = callstack * (state Stmt.Hashtbl.t) Lazy.t

  module Record_Value_Superposition_Callbacks:
    Hook.Iter_hook with type param = callstack * (state list Stmt.Hashtbl.t) Lazy.t

  module Record_Value_After_Callbacks:
    Hook.Iter_hook with type param = callstack * (state Stmt.Hashtbl.t) Lazy.t

  (**/**)
    (* Temporary API, do not use *)
  module Record_Value_Callbacks_New: Hook.Iter_hook
    with type param =
      callstack *
      (state Stmt.Hashtbl.t) Lazy.t Value_types.callback_result
  (**/**)

  val no_results: (fundec -> bool) ref
  (** Returns [true] if the user has requested that no results should
      be recorded for this function. If possible, hooks registered
      on [Record_Value_Callbacks] and [Record_Value_Callbacks_New]
      should not force their lazy argument *)

  (** Actions to perform at each treatment of a "call" statement. *)
  module Call_Value_Callbacks:
    Hook.Iter_hook with type param = state * callstack

  (** Actions to perform whenever a statement is handled. *)
  module Compute_Statement_Callbacks:
    Hook.Iter_hook with type param = stmt * callstack * state list


  (** {3 Pretty printing} *)

  val pretty : Format.formatter -> t -> unit
  val pretty_state : Format.formatter -> state -> unit


  val display : (Format.formatter -> kernel_function -> unit) ref

  (**/**)
  (** {3 Internal use only} *)

  val noassert_get_state : kinstr -> state
    (** To be used during the value analysis itself (instead of
        {!get_state}). *)
  val noassert_get_stmt_state : stmt -> state
    (** To be used during the value analysis itself (instead of
        {!get_stmt_state}). *)

  val recursive_call_occurred: kernel_function -> unit

  val merge_conditions: int Cil_datatype.Stmt.Hashtbl.t -> unit
  val mask_then: int
  val mask_else: int

  val initial_state_only_globals : (unit -> state) ref

  val update_table : stmt -> state -> unit
  (* Merge the given state with others associated to the given stmt. *)

  val update_callstack_table: after:bool -> stmt -> callstack -> state -> unit
  (* Merge a new state in the table indexed by callstacks. *)


  val memoize : (kernel_function -> unit) ref
(*  val compute_call :
    (kernel_function -> call_kinstr:kinstr -> state ->  (exp*t) list
       -> Cvalue.V_Offsetmap.t option (** returned value of [kernel_function] *) * state) ref
*)
  val merge_initial_state : kernel_function -> state -> unit
    (** Store an additional possible initial state for the given function as
        well as its values for actuals. *)

  val initial_state_changed: (unit -> unit) ref
end

(** Functional dependencies between function inputs and function outputs.
    @see <../from/index.html> internal documentation. *)
module From : sig

  val compute_all : (unit -> unit) ref
  val compute_all_calldeps : (unit -> unit) ref

  val compute : (kernel_function -> unit) ref

  val is_computed: (kernel_function -> bool) ref
    (** Check whether the from analysis has been performed for the given
        function.
        @return true iff the analysis has been performed *)

  val get : (kernel_function -> Function_Froms.t) ref
  val access : (Locations.Zone.t -> Function_Froms.Memory.t
                -> Locations.Zone.t) ref
  val find_deps_no_transitivity : (stmt -> exp -> Locations.Zone.t) ref
  val find_deps_no_transitivity_state :
    (Value.state -> exp -> Locations.Zone.t) ref

  val find_deps_term_no_transitivity_state :
    (Value.state -> term -> Value_types.logic_dependencies) ref

  val self: State.t ref

  (** {3 Pretty printing} *)

  val pretty : (Format.formatter -> kernel_function -> unit) ref
  val display : (Format.formatter -> unit) ref

  (** {3 Callback} *)

 module Record_From_Callbacks:
   Hook.Iter_hook with type param =
   Kernel_function.t Stack.t *
     Function_Froms.Memory.t Stmt.Hashtbl.t *
     (Kernel_function.t * Function_Froms.Memory.t) list
     Stmt.Hashtbl.t

  (** {3 Access to callwise-stored data} *)

  module Callwise : sig
    val iter : ((kinstr -> Function_Froms.t -> unit) -> unit) ref
    val find : (kinstr -> Function_Froms.t) ref
  end
end

(** Functions used by another function.
    @see <../users/index.html> internal documentation. *)
module Users : sig
  val get: (kernel_function -> Kernel_function.Hptset.t) ref
end

(** Do not use yet. *)
module Access_path : sig
  type t = (Locations.Zone.t * Locations.Location_Bits.t) Base.Map.t
  val compute: (Cvalue.Model.t -> Base.Set.t -> t) ref
  val filter: (t -> Locations.Zone.t -> t) ref
  val pretty: (Format.formatter -> t -> unit) ref
end

(* ************************************************************************* *)
(** {2 Properties} *)
(* ************************************************************************* *)

(** Dealing with logical properties. *)
module Properties : sig

  (** Interpretation of logic terms. *)
  module Interp : sig

    (** {3 From C terms to logic terms} *)

    val lval : (kernel_function -> stmt -> string -> Cil_types.term_lval) ref
    val expr : (kernel_function -> stmt -> string -> Cil_types.term) ref

    (** {3 From logic terms to C terms} *)

    val term_lval_to_lval:
      (result: Cil_types.varinfo option -> term_lval -> Cil_types.lval) ref
      (** @raise Invalid_argument if the argument is not a left value. *)

    val term_to_lval:
      (result: Cil_types.varinfo option -> term -> Cil_types.lval) ref
      (** @raise Invalid_argument if the argument is not a left value. *)

    val term_to_exp:
      (result: Cil_types.varinfo option -> term -> Cil_types.exp) ref
      (** @raise Invalid_argument if the argument is not a valid expression. *)

    val loc_to_exp:
      (result: Cil_types.varinfo option -> term -> Cil_types.exp list) ref
      (** @return a list of C expressions.
          @raise Invalid_argument if the argument is not a valid set of
          expressions. *)

    val loc_to_lval:
      (result: Cil_types.varinfo option -> term -> Cil_types.lval list) ref
      (** @return a list of C locations.
          @raise Invalid_argument if the argument is not a valid set of
          left values. *)

    val term_offset_to_offset:
      (result: Cil_types.varinfo option -> term_offset -> offset) ref
      (** @raise Invalid_argument if the argument is not a valid offset. *)

    val loc_to_offset:
      (result: Cil_types.varinfo option -> term -> Cil_types.offset list) ref
      (** @return a list of C offset provided the term denotes location who
          have all the same base address.  *)


    (** {3 From logic terms to Locations.location} *)

    val loc_to_loc:
      (result: Cil_types.varinfo option -> Value.state -> term -> 
       Locations.location) ref
      (** @raise Invalid_argument if the translation fails. *)

    val loc_to_locs:
      (result: Cil_types.varinfo option -> Value.state -> term -> 
       Locations.location list * Locations.Zone.t) ref
      (** Translate a term more precisely than [loc_to_loc] if the term
          evaluates to an ACSL tset. The zone returned is the locations
          that have been read during evaluation.
          Warning: This API is not stabilized, and is likely to change in
          the future.
          @raise Invalid_argument in some cases. *)

    (** {3 From logic terms to Zone.t} *)

    module To_zone : sig
      type t_ctx =
          {state_opt:bool option;
           ki_opt:(stmt * bool) option;
           kf:Kernel_function.t}

      val mk_ctx_func_contrat:
        (kernel_function -> state_opt:bool option -> t_ctx) ref
        (** To build an interpretation context relative to function
            contracts. *)

      val mk_ctx_stmt_contrat:
        (kernel_function -> stmt -> state_opt:bool option -> t_ctx) ref
        (** To build an interpretation context relative to statement
            contracts. *)

      val mk_ctx_stmt_annot:
        (kernel_function -> stmt -> t_ctx) ref
        (** To build an interpretation context relative to statement
            annotations. *)

      type t = {before:bool ; ki:stmt ; zone:Locations.Zone.t}
      type t_zone_info = (t list) option
           (** list of zones at some program points.
           *   None means that the computation has failed. *)

      type t_decl = {var: Varinfo.Set.t ; (* related to vars of the annot *)
                     lbl: Logic_label.Set.t} (* related to labels of the annot *)
      type t_pragmas =
          {ctrl: Stmt.Set.t ; (* related to //@ slice pragma ctrl/expr *)
           stmt: Stmt.Set.t}  (* related to statement assign and
      //@ slice pragma stmt *)

      val from_term: (term -> t_ctx -> t_zone_info * t_decl) ref
        (** Entry point to get zones needed to evaluate the [term] relative to
            the [ctx] of interpretation. *)

      val from_terms: (term list -> t_ctx -> t_zone_info * t_decl) ref
        (** Entry point to get zones needed to evaluate the list of [terms]
            relative to the [ctx] of interpretation. *)

      val from_pred: (predicate named -> t_ctx -> t_zone_info * t_decl) ref
        (** Entry point to get zones needed to evaluate the [predicate]
            relative to the [ctx] of interpretation. *)

      val from_preds: (predicate named list -> t_ctx -> t_zone_info * t_decl) ref
        (** Entry point to get zones needed to evaluate the list of
            [predicates] relative to the [ctx] of interpretation. *)

      val from_zone: (identified_term -> t_ctx -> t_zone_info * t_decl) ref
        (** Entry point to get zones needed to evaluate the [zone] relative to
            the [ctx] of interpretation. *)

      val from_stmt_annot:
        (code_annotation -> stmt * kernel_function
          -> (t_zone_info * t_decl) * t_pragmas) ref
        (** Entry point to get zones needed to evaluate an annotation on the
            given stmt. *)

      val from_stmt_annots:
        ((code_annotation -> bool) option ->
           stmt * kernel_function -> (t_zone_info * t_decl) * t_pragmas) ref
        (** Entry point to get zones needed to evaluate annotations of this
            [stmt]. *)

      val from_func_annots:
        (((stmt -> unit) -> kernel_function -> unit) ->
           (code_annotation -> bool) option ->
             kernel_function -> (t_zone_info * t_decl) * t_pragmas) ref
        (** Entry point to get zones
            needed to evaluate annotations of this [kf]. *)

      val code_annot_filter:
        (code_annotation ->
           threat:bool -> user_assert:bool -> slicing_pragma:bool ->
          loop_inv:bool -> loop_var:bool -> others:bool -> bool) ref
        (** To quickly build an annotation filter *)

    end

    (** Does the interpretation of the predicate rely on the intepretation
        of the term result?
        @since Carbon-20110201 *)
    val to_result_from_pred:
      (predicate named -> bool) ref

    (** {3 Internal use only} *)

    val code_annot :
      (kernel_function -> stmt -> string -> code_annotation)
      ref

  end

  (** {3 Assertions} *)

  val add_assert: Emitter.t -> kernel_function -> stmt -> string -> unit
    (** @deprecated since Oxygen-20120901 
        Ask for {ACSL_importer plug-in} if you need such functionality. 
	@modify Boron-20100401 takes as additional argument the
        computation which adds the assert. 
	@modify Oxygen-20120901 replaces the State.t list by an Emitter.t
     *) 
 
end

(* ************************************************************************* *)
(** {2 Plugins} *)
(* ************************************************************************* *)

(** Interface for the syntactic_callgraph plugin.
    @see <../syntactic_callgraph/index.html> internal documentation. *)
module Syntactic_Callgraph: sig
  val dump: (unit -> unit) ref
end



(** Declarations common to the various postdominators-computing modules *)
module PostdominatorsTypes: sig

  exception Top
  (** Used for postdominators-related functions, when the
      postdominators of a statement cannot be computed. It means that
      there is no path from this statement to the function return. *)

  module type Sig = sig
    val compute: (kernel_function -> unit) ref

    val stmt_postdominators:
      (kernel_function -> stmt -> Stmt.Hptset.t) ref
    (** @raise Top (see above) *)

    val is_postdominator:
      (kernel_function -> opening:stmt -> closing:stmt -> bool) ref

    val display: (unit -> unit) ref

    val print_dot : (string -> kernel_function -> unit) ref
      (** Print a representation of the postdominators in a dot file
          whose name is [basename.function_name.dot]. *)
  end
end

(** Syntaxic postdominators plugin.
    @see <../postdominators/index.html> internal documentation. *)
module Postdominators: PostdominatorsTypes.Sig

(** Postdominators using value analysis results.
    @see <../postdominators/index.html> internal documentation. *)
module PostdominatorsValue: PostdominatorsTypes.Sig

(** Runtime Error Annotation Generation plugin.
    @see <../rte/index.html> internal documentation. *)
module RteGen : sig
  val compute : (unit -> unit) ref
  val annotate_kf : (kernel_function -> unit) ref
  val self: State.t ref
  val do_precond : (kernel_function -> unit) ref
  val do_all_rte : (kernel_function -> unit) ref
  val do_rte : (kernel_function -> unit) ref
  type status_accessor = 
      string (* name *)
      * (kernel_function -> bool -> unit) (* for each kf and each kind of
					     annotation, set/unset the fact
					     that there has been generated *)
      * (kernel_function -> bool) (* is this kind of annotation generated in 
				     kf? *) 
  val get_all_status : (unit -> status_accessor list) ref
  val get_precond_status : (unit -> status_accessor) ref
  val get_signedOv_status : (unit -> status_accessor) ref
  val get_divMod_status : (unit -> status_accessor) ref
  val get_downCast_status : (unit -> status_accessor) ref
  val get_memAccess_status : (unit -> status_accessor) ref
  val get_unsignedOv_status : (unit -> status_accessor) ref
  val get_unsignedDownCast_status : (unit -> status_accessor) ref
end

(** Dump Properties-Status consolidation tree. *)
module Report :
sig
  val print : (unit -> unit) ref
end

(** Constant propagation plugin.
    @see <../constant_propagation/index.html> internal documentation. *)
module Constant_Propagation: sig
  val get : (Datatype.String.Set.t -> cast_intro:bool -> Project.t) ref
    (** Propagate constant into the functions given by name.
        note: the propagation is performed into all functions when the set is
        empty; and casts can be introduced when [cast_intro] is true. *)

  val compute: (unit -> unit) ref
    (** Propage constant into the functions given by the parameters (in the
        same way that {!get}. Then pretty print the resulting program.
        @since Beryllium-20090901 *)

end


(** Impact analysis.
    @see <../impact/index.html> internal documentation. *)
module Impact : sig
  val compute_pragmas: (unit -> stmt list) ref
    (** Compute the impact analysis from the impact pragma in the program.
        Print and slice the results according to the parameters -impact-print
        and -impact-slice.
        @return the impacted statements *)
  val from_stmt: (stmt -> stmt list) ref
    (** Compute the impact analysis of the given statement.
        @return the impacted statements *)
  val from_nodes:
    (kernel_function -> PdgTypes.Node.t list -> PdgTypes.NodeSet.t) ref
    (** Compute the impact analysis of the given set of PDG nodes,
        that come from the given function.
        @return the impacted nodes *)
  val slice: (stmt list -> unit) ref
    (** Slice the given statement according to the impact analysis. *)
end

(** Security analysis.
    @see <../security/index.html> internal documentation. *)
module Security : sig

  val run_whole_analysis: (unit -> unit) ref
    (** Run all the security analysis. *)

  val run_ai_analysis: (unit -> unit) ref
    (** Only run the analysis by abstract interpretation. *)

  val run_slicing_analysis: (unit -> Project.t) ref
    (** Only run the security slicing pre-analysis. *)

  val self: State.t ref

end

(** Program Dependence Graph.
    @see <../pdg/index.html> PDG internal documentation. *)
module Pdg : sig

  exception Bottom
    (** Raised by most function when the PDG is Bottom because we can hardly do
        nothing with it. It happens when the function is unreachable because we
        have no information about it. *)

  exception Top
    (** Raised by most function when the PDG is Top because we can hardly do
        nothing with it. It happens when we didn't manage to compute it, for
        instance for a variadic function. *)

  type t = PdgTypes.Pdg.t
      (** PDG type *)

  type t_nodes_and_undef =
      ((PdgTypes.Node.t * Locations.Zone.t option) list * Locations.Zone.t option)
        (** type for the return value of many [find_xxx] functions when the
            answer can be a list of [(node, z_part)] and an [undef zone].
            For each node, [z_part] can specify which part of the node
            is used in terms of zone ([None] means all).
         *)

  val self : State.t ref

  (** {3 Getters} *)

  val get : (kernel_function -> t) ref
    (** Get the PDG of a function. Build it if it doesn't exist yet. *)

  val node_key : (PdgTypes.Node.t -> PdgIndex.Key.t) ref

  val from_same_fun : t -> t -> bool

  (** {3 Finding PDG nodes} *)

  val find_decl_var_node : (t -> Cil_types.varinfo -> PdgTypes.Node.t) ref
    (** Get the node corresponding the declaration of a local variable or a
        formal parameter.
        @raise Not_found if the variable is not declared in this function.
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val find_ret_output_node : (t -> PdgTypes.Node.t) ref
    (** Get the node corresponding return stmt.
        @raise Not_found if the ouptut state in unreachable
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val find_output_nodes :
    (t -> PdgIndex.Signature.out_key -> t_nodes_and_undef) ref
    (** Get the nodes corresponding to a call output key in the called pdg.
        @raise Not_found if the ouptut state in unreachable
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val find_input_node : (t -> int -> PdgTypes.Node.t) ref
    (** Get the node corresponding to a given input (parameter).
        @raise Not_found if the number is not an input number.
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val find_all_inputs_nodes : (t -> PdgTypes.Node.t list) ref
    (** Get the nodes corresponding to all inputs.
        {!node_key} can be used to know their numbers.
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val find_stmt_node : (t -> Cil_types.stmt -> PdgTypes.Node.t) ref
    (** Get the node corresponding to the statement.
        It shouldn't be a call statement.
        See also {!find_simple_stmt_nodes} or {!find_call_stmts}.
        @raise Not_found if the given statement is unreachable.
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val find_simple_stmt_nodes : (t -> Cil_types.stmt -> PdgTypes.Node.t list) ref
    (** Get the nodes corresponding to the statement.
        It is usualy composed of only one node (see {!find_stmt_node}),
        except for call statement.
        Be careful that for block statements, it only retuns a node
        corresponding to the elementary stmt
                           (see {!find_stmt_and_blocks_nodes} for more)
        @raise Not_found if the given statement is unreachable.
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val find_label_node : (t -> Cil_types.stmt -> Cil_types.label -> PdgTypes.Node.t) ref
    (** Get the node corresponding to the label.
        @raise Not_found if the given label is not in the PDG.
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val find_stmt_and_blocks_nodes : (t -> Cil_types.stmt -> PdgTypes.Node.t list) ref
    (** Get the nodes corresponding to the statement like
    * {!find_simple_stmt_nodes} but also add the nodes of the enclosed
    * statements if [stmt] contains blocks.
        @raise Not_found if the given statement is unreachable.
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val find_top_input_node : (t -> PdgTypes.Node.t) ref
    (** @raise Not_found if there is no top input in the PDG.
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val find_entry_point_node : (t -> PdgTypes.Node.t) ref
    (** Find the node that represent the entry point of the function, i.e. the
        higher level block.
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val find_location_nodes_at_stmt :
    (t -> Cil_types.stmt -> before:bool -> Locations.Zone.t
     -> t_nodes_and_undef) ref
    (** Find the nodes that define the value of the location at the given
        program point. Also return a zone that might be undefined at that
        point.
        @raise Not_found if the given statement is unreachable.
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val find_location_nodes_at_end :
    (t -> Locations.Zone.t -> t_nodes_and_undef) ref
    (** Same than {!find_location_nodes_at_stmt} for the program point located
        at the end of the function.
        @raise Not_found if the output state is unreachable.
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val find_location_nodes_at_begin :
    (t -> Locations.Zone.t -> t_nodes_and_undef) ref
    (** Same than {!find_location_nodes_at_stmt} for the program point located
        at the beginning of the function.
       Notice that it can only find formal argument nodes.
       The remaining zone (implicit input) is returned as undef.
        @raise Not_found if the output state is unreachable.
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val find_call_stmts:
    (kernel_function -> caller:kernel_function -> Cil_types.stmt list) ref
    (** Find the call statements to the function (can maybe be somewhere
        else).
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val find_call_ctrl_node : (t ->  Cil_types.stmt -> PdgTypes.Node.t) ref
    (** @raise Not_found if the call is unreachable.
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val find_call_input_node : (t ->  Cil_types.stmt -> int -> PdgTypes.Node.t) ref
    (** @raise Not_found if the call is unreachable or has no such input.
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val find_call_output_node : (t ->  Cil_types.stmt -> PdgTypes.Node.t) ref
    (** @raise Not_found if the call is unreachable or has no output node.
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val find_code_annot_nodes :
    (t -> Cil_types.stmt -> Cil_types.code_annotation ->
       PdgTypes.Node.t list * PdgTypes.Node.t list * (t_nodes_and_undef option)) ref
    (** The result is composed of three parts :
       - the first part of the result are the control dependencies nodes
         of the annotation,
       - the second part is the list of declaration nodes of the variables
         used in the annotation;
       - the third part is similar to [find_location_nodes_at_stmt] result
         but  for all the locations needed by the annotation.
         When the third part  is globally [None],
         it means that we were not able to compute this information.
        @raise Not_found if the statement is unreachable.
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val find_fun_precond_nodes :
    (t -> Cil_types.predicate -> PdgTypes.Node.t list * (t_nodes_and_undef option)) ref
    (** Similar to [find_code_annot_nodes] (no control dependencies nodes) *)

  val find_fun_postcond_nodes :
    (t -> Cil_types.predicate -> PdgTypes.Node.t list * (t_nodes_and_undef option)) ref
    (** Similar to [find_fun_precond_nodes] *)

  val find_fun_variant_nodes :
    (t -> Cil_types.term -> (PdgTypes.Node.t list * t_nodes_and_undef option)) ref
    (** Similar to [find_fun_precond_nodes] *)

  (** {3 Propagation}
      See also [Pdg.mli] for more function that cannot be here because
        they use polymorphic types.
  **)

  val find_call_out_nodes_to_select :
    (t -> PdgTypes.NodeSet.t -> t ->  Cil_types.stmt -> PdgTypes.Node.t list) ref
  (** [find_call_out_nodes_to_select pdg_called called_selected_nodes
      pdg_caller call_stmt]
      @return the call outputs nodes [out] such that
      [find_output_nodes pdg_called out_key]
      intersects [called_selected_nodes]. *)

  val find_in_nodes_to_select_for_this_call :
    (t -> PdgTypes.NodeSet.t -> Cil_types.stmt -> t -> PdgTypes.Node.t list) ref
    (** [find_in_nodes_to_select_for_this_call
        pdg_caller caller_selected_nodes call_stmt pdg_called]
        @return the called input nodes such that the corresponding nodes
        in the caller intersect [caller_selected_nodes]
        @raise Not_found if the statement is unreachable.
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  (** {3 Dependencies} *)

  val direct_dpds : (t -> PdgTypes.Node.t -> PdgTypes.Node.t list) ref
    (** Get the nodes to which the given node directly depend on.
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val direct_ctrl_dpds : (t -> PdgTypes.Node.t -> PdgTypes.Node.t list) ref
    (** Similar to {!direct_dpds}, but for control dependencies only.
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val direct_data_dpds : (t -> PdgTypes.Node.t -> PdgTypes.Node.t list) ref
    (** Similar to {!direct_dpds}, but for data dependencies only.
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val direct_addr_dpds : (t -> PdgTypes.Node.t -> PdgTypes.Node.t list) ref
    (** Similar to {!direct_dpds}, but for address dependencies only.
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val all_dpds : (t -> PdgTypes.Node.t list -> PdgTypes.Node.t list) ref
    (** Transitive closure of {!direct_dpds} for all the given nodes.
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val all_data_dpds : (t -> PdgTypes.Node.t list -> PdgTypes.Node.t list) ref
    (** Gives the data dependencies of the given nodes, and recursively, all
        the dependencies of those nodes (regardless to their kind).
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val all_ctrl_dpds : (t -> PdgTypes.Node.t list -> PdgTypes.Node.t list) ref
    (** Similar to {!all_data_dpds} for control dependencies.
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val all_addr_dpds : (t -> PdgTypes.Node.t list -> PdgTypes.Node.t list) ref
    (** Similar to {!all_data_dpds} for address dependencies.
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val direct_uses : (t -> PdgTypes.Node.t -> PdgTypes.Node.t list) ref
    (** build a list of all the nodes that have direct dependencies on the
        given node.
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val direct_ctrl_uses : (t -> PdgTypes.Node.t -> PdgTypes.Node.t list) ref
    (** Similar to {!direct_uses}, but for control dependencies only.
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val direct_data_uses : (t -> PdgTypes.Node.t -> PdgTypes.Node.t list) ref
    (** Similar to {!direct_uses}, but for data dependencies only.
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val direct_addr_uses : (t -> PdgTypes.Node.t -> PdgTypes.Node.t list) ref
    (** Similar to {!direct_uses}, but for address dependencies only.
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val all_uses : (t -> PdgTypes.Node.t list -> PdgTypes.Node.t list) ref
    (** build a list of all the nodes that have dependencies (even indirect) on
        the given nodes.
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val custom_related_nodes :
    ((PdgTypes.Node.t -> PdgTypes.Node.t list) -> PdgTypes.Node.t list -> PdgTypes.Node.t list) ref
    (** [custom_related_nodes get_dpds node_list] build a list, starting from
        the node in [node_list], and recursively add the nodes given by the
        function [get_dpds].  For this function to work well, it is important
        that [get_dpds n] returns a subset of the nodes directly related to
        [n], ie a subset of [direct_uses] U [direct_dpds].
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  val iter_nodes : ((PdgTypes.Node.t -> unit) -> t -> unit) ref
    (** apply a given function to all the PDG nodes
        @raise Bottom if given PDG is bottom.
        @raise Top if the given pdg is top. *)

  (** {3 Pretty printing} *)

  val extract : (t -> string -> unit) ref
    (** Pretty print pdg into a dot file.
        @see <../pdg/index.html> PDG internal documentation. *)

  val pretty_node : (bool -> Format.formatter -> PdgTypes.Node.t -> unit) ref
    (** Pretty print information on a node : with [short=true], only the id
        of the node is printed.. *)

  val pretty_key : (Format.formatter -> PdgIndex.Key.t -> unit) ref
    (** Pretty print information on a node key *)

  val pretty : (?bw:bool -> Format.formatter -> t -> unit) ref
    (** For debugging... Pretty print pdg information.
      Print codependencies rather than dependencies if [bw=true]. *)

end

(** Interface for the Scope plugin.
    @see <../scope/index.html> internal documentation. *)
module Scope : sig
  val get_data_scope_at_stmt :
    (kernel_function -> stmt -> lval ->
       Stmt.Hptset.t * (Stmt.Hptset.t * Stmt.Hptset.t)) ref
    (**
    * @raise Kernel_function.No_Definition if [kf] has no definition.
    * @return 3 statement sets related to the value of [lval] before [stmt] :
    * - the forward selection,
    * - the both way selection,
    * - the backward selection.
    *)

  val get_prop_scope_at_stmt :
    (kernel_function -> stmt -> code_annotation ->
       Stmt.Hptset.t * code_annotation list) ref
    (** compute the set of statements where the given annotation has the same
    * value than it has before the given stmt.
    * Also return the *)

  val check_asserts : (unit -> code_annotation list) ref
    (** Print how many assertions could be removed based on the previous
    * analysis ([get_prop_scope_at_stmt]) and return the annotations
    * that can be removed. *)

  val rm_asserts : (unit -> unit) ref
    (** Same analysis than [check_asserts] but change assert to remove by true
      * *)

  val get_defs :
    (kernel_function -> stmt -> lval ->
     (Stmt.Hptset.t * Locations.Zone.t option) option) ref
    (** @return the set of statements that define [lval] before [stmt] in [kf].
    * Also returns the zone that is possibly not defined.
    * Can return [None] when the information is not available (Pdg missing).
    * *)

  val get_defs_with_type :
    (kernel_function -> stmt -> lval ->
     ((bool * bool) Stmt.Map.t * Locations.Zone.t option) option) ref
    (** @return a map from the statements that define [lval] before [stmt] in
        [kf]. The first boolean indicates the possibility of a direct
        modification at this statement, ie. [lval = ...] or [lval = f()].
        The second boolean indicates a possible indirect modification through
        a call.
        Also returns the zone that is possibly not defined.
        Can return [None] when the information is not available (Pdg missing).
    *)

  (** {3 Zones} *)

  type t_zones = Locations.Zone.t Stmt.Hashtbl.t
  val build_zones :
    (kernel_function -> stmt -> lval -> Stmt.Hptset.t * t_zones) ref
  val pretty_zones : (Format.formatter -> t_zones -> unit) ref
  val get_zones : (t_zones ->  Cil_types.stmt -> Locations.Zone.t) ref

end

(** Interface for the unused code detection.
    @see <../sparecode/index.html> internal documentation. *)
module Sparecode : sig
  val get: (select_annot:bool -> select_slice_pragma:bool -> Project.t) ref
     (** Remove in each function what isn't used to compute its outputs,
      *   or its annotations when [select_annot] is true,
      *   or its slicing pragmas when [select_slice_pragmas] is true.
      *  @return a new project where the sparecode has been removed.
      *)
  val rm_unused_globals : (?new_proj_name:string -> ?project:Project.t -> unit -> Project.t) ref
    (** Remove  unused global types and variables from the given project
      * (the current one if no project given).
      * The source project is not modified.
      * The result is in the returned new project.
      * optional argument [new_proj_name] added @since Carbon-20110201
      * *)
end

(** Interface for the occurrence plugin.
    @see <../occurrence/index.html> internal documentation. *)
module Occurrence: sig
  type t = (kernel_function option * kinstr * lval) list
  val get: (varinfo -> t) ref
    (** Return the occurrences of the given varinfo.
        An occurrence [ki, lv] is a left-value [lv] which uses the location of
        [vi] at the position [ki]. *)
  val get_last_result: (unit -> (t * varinfo) option) ref
    (** @return the last result computed by occurrence *)
  val print_all: (unit -> unit) ref
    (** Print all the occurrence of each variable declarations. *)
  val self: State.t ref
end

(** Interface for the slicing tool.
    @see <../slicing/index.html> internal documentation. *)
module Slicing : sig

  exception No_Project
  exception Existing_Project

  val self: State.t ref
    (** Internal state of the slicing tool from project viewpoints. *)

  val set_modes : (?calls:int -> ?callers:bool -> ?sliceUndef:bool
    -> ?keepAnnotations:bool -> ?print:bool
    -> unit -> unit) ref

  (** Slicing project management. *)
  module Project : sig

    type t = SlicingTypes.sl_project
        (** Abstract data type for slicing project. *)
    val dyn_t : t Type.t
        (** For dynamic type checking and journalization. *)

    val mk_project : (string -> t) ref
      (** To use to start a new slicing project.
          Several projects from a same current project can be managed.
          @raise Existing_Project if an axisting project has the same name.*)

    val from_unique_name : (string -> t) ref
      (** Find a slicing project from its name.
          @raise No_Project when no project is found. *)

    val get_all : (unit -> t list) ref
      (** Get all slicing projects. *)

    val set_project : (t option -> unit) ref
      (** Get the current project. *)

    val get_project : (unit -> t option) ref
      (** Get the current project. *)

    val get_name : (t -> string) ref
      (** Get the slicing project name. *)

    (** {3 Kernel function} *)

    val is_called : (t -> kernel_function -> bool) ref
      (** Return [true] iff the source function is called (even indirectly via
          transitivity) from a [Slice.t]. *)

    val has_persistent_selection : (t -> kernel_function -> bool) ref
      (** return [true] iff the source function has persistent selection *)

    val change_slicing_level : (t -> kernel_function -> int -> unit) ref
      (** change the slicing level of this function
          (see the [-slicing-level] option documentation to know the meaning of the
          number)
          @raise SlicingTypes.ExternalFunction if [kf] has no definition.
          @raise SlicingTypes.WrongSlicingLevel if [n] is not valid.
      *)

    (** {3 Extraction} *)

    val default_slice_names : (kernel_function -> bool  -> int -> string) ref

    val extract : (string ->
                   ?f_slice_names:(kernel_function -> bool  -> int -> string) ->
                   t -> Project.t) ref
      (** Build a new [Db.Project.t] from all [Slice.t] of a project.
      * Can optionally specify how to name the sliced functions
      * by defining [f_slice_names].
      * [f_slice_names kf src_visi num_slice] has to return the name
      * of the exported functions based on the source function [kf].
      * - [src_visi] tells if the source function name is used
      *              (if not, it can be used for a slice)
      * - [num_slice] gives the number of the slice to name.
      * The entry point function is only exported once :
      * it is VERY recommanded to give to it its original name,
      * even if it is sliced.
      * *)

    val print_extracted_project : (?fmt:Format.formatter ->
                                    extracted_prj:Project.t -> unit) ref
      (** Print the extracted project when "-slice-print" is set. *)

    val print_dot : (filename:string -> title:string -> t -> unit) ref
      (** Print a representation of the slicing project (call graph)
          in a dot file which name is the given string. *)

    (** {3 Internal use only} *)

    val pretty : (Format.formatter -> t -> unit) ref
      (** For debugging... Pretty print project information. *)

    val is_directly_called_internal : (t -> kernel_function -> bool) ref
      (** Return [true] if the source function is directly (even via pointer
          function) called from a [Slice.t]. *)

  end

  (** Acces to slicing results. *)
  module Mark : sig

    type t = SlicingTypes.sl_mark
        (** Abtract data type for mark value. *)
    val dyn_t : t Type.t
        (** For dynamic type checking and journalization. *)

    val make : (data:bool -> addr:bool -> ctrl:bool -> t) ref
      (** To construct a mark such as
          [(is_ctrl result, is_data result, isaddr result) =
          (~ctrl, ~data, ~addr)],
          [(is_bottom result) = false] and
          [(is_spare result) = not (~ctrl || ~data || ~addr)]. *)

    val compare : (t -> t -> int) ref
      (** A total ordering function similar to the generic structural
          comparison function [compare].
          Can be used to build a map from [t] marks to, for exemple, colors for
          the GUI. *)

    val is_bottom : (t -> bool) ref
      (** [true] iff the mark is empty: it is the only case where the
          associated element is invisible. *)

    val is_spare : (t -> bool) ref
      (** Smallest visible mark. Usually used to mark element that need to be
          visible for compilation purpose, not really for the selected
          computations. *)

    val is_data : (t -> bool) ref
      (** The element is used to compute selected data.
          Notice that a mark can be [is_data] and/or [is_ctrl] and/or [is_addr]
          at the same time. *)

    val is_ctrl : (t -> bool) ref
      (** The element is used to control the program point of a selected
          data. *)

    val is_addr : (t -> bool) ref
      (** The element is used to compute the address of a selected data. *)

    val get_from_src_func : (Project.t -> kernel_function -> t) ref
      (** The mark [m] related to all statements of a source function [kf].
          Property : [is_bottom (get_from_func proj kf) = not (Project.is_called proj kf) ] *)

    val pretty : (Format.formatter -> t -> unit) ref
      (** For debugging... Pretty mark information. *)

  end

  (** Slicing selections. *)
  module Select : sig

    type t = SlicingTypes.sl_select
        (** Internal selection. *)
    val dyn_t : t Type.t
        (** For dynamic type checking and journalization. *)

    type set = SlicingTypes.Fct_user_crit.t Cil_datatype.Varinfo.Map.t
        (** Set of colored selections. *)
    val dyn_set : set Type.t
        (** For dynamic type checking and journalization. *)

    val empty_selects : set
      (** Empty selection. *)

    val select_stmt :
      (set -> spare:bool -> stmt -> kernel_function -> set) ref
      (** To select a statement. *)

    val select_stmt_ctrl :
      (set -> spare:bool -> stmt -> kernel_function -> set) ref
      (** To select a statement reachability.
          Note: add also a transparent selection on the whole statement. *)

    val select_stmt_lval_rw :
      (set ->
       Mark.t ->
       rd:Datatype.String.Set.t ->
       wr:Datatype.String.Set.t ->
       stmt ->
       scope:stmt ->
       eval:stmt ->
       kernel_function -> set) ref
      (** To select rw accesses to lvalues (given as string) related to a statement.
          Variables of [~rd] and [~wr] string are bounded
          relatively to the scope of the statement [~scope].
          The interpretation of the address of the lvalues is
          done just before the execution of the statement [~eval].
          The selection preserve the [~rd] and ~[wr] accesses contained into the statement [ki].
          Note: add also a transparent selection on the whole statement. *)

    val select_stmt_lval :
      (set -> Mark.t -> Datatype.String.Set.t -> before:bool -> stmt ->
        scope:stmt -> eval:stmt -> kernel_function -> set) ref
      (** To select lvalues (given as string) related to a statement.
          Variables of [lval_str] string are bounded
          relatively to the scope of the statement [~scope].
          The interpretation of the address of the lvalue is
          done just before the execution of the statement [~eval].
          The selection preserve the value of these lvalues before or
          after (c.f. boolean [~before]) the statement [ki].
          Note: add also a transparent selection on the whole statement. *)

    val select_stmt_zone :
      (set -> Mark.t -> Locations.Zone.t -> before:bool -> stmt ->
       kernel_function -> set) ref
      (** To select a zone value related to a statement.
          Note: add also a transparent selection on the whole statement. *)

    val select_stmt_term :
      (set -> Mark.t -> term -> stmt ->
        kernel_function -> set) ref
      (** To select a predicate value related to a statement.
          Note: add also a transparent selection on the whole statement. *)

    val select_stmt_pred :
      (set -> Mark.t -> predicate named -> stmt ->
        kernel_function -> set) ref
      (** To select a predicate value related to a statement.
          Note: add also a transparent selection on the whole statement. *)

    val select_stmt_annot :
      (set -> Mark.t -> spare:bool -> code_annotation ->  stmt ->
       kernel_function -> set) ref
      (** To select the annotations related to a statement.
          Note: add also a transparent selection on the whole statement. *)

    val select_stmt_annots :
      (set -> Mark.t -> spare:bool -> threat:bool -> user_assert:bool ->
        slicing_pragma:bool -> loop_inv:bool -> loop_var:bool ->
        stmt -> kernel_function -> set) ref
      (** To select the annotations related to a statement.
          Note: add also a transparent selection on the whole statement. *)

    val select_func_lval_rw :
      (set -> Mark.t -> rd:Datatype.String.Set.t -> wr:Datatype.String.Set.t ->
        scope:stmt -> eval:stmt -> kernel_function -> set) ref
      (** To select rw accesses to lvalues (given as a string) related to a function.
          Variables of [~rd] and [~wr] string are bounded
          relatively to the scope of the statement [~scope].
          The interpretation of the address of the lvalues is
          done just before the execution of the statement [~eval].
          The selection preserve the value of these lvalues into the whole project. *)

    val select_func_lval :
      (set -> Mark.t -> Datatype.String.Set.t -> kernel_function -> set) ref
      (** To select lvalues (given as a string) related to a function.
          Variables of [lval_str] string are bounded
          relatively to the scope of the first statement of [kf].
          The interpretation of the address of the lvalues is
          done just before the execution of the first statement [kf].
          The selection preserve the value of these lvalues before
          execution of the return statement. *)

    val select_func_zone :
      (set -> Mark.t -> Locations.Zone.t -> kernel_function -> set) ref
      (** To select an output zone related to a function. *)

    val select_func_return :
      (set -> spare:bool -> kernel_function -> set) ref
      (** To select the function result (returned value). *)

    val select_func_calls_to :
      (set -> spare:bool -> kernel_function -> set) ref
      (** To select every calls to the given function, i.e. the call keeps
          its semantics in the slice. *)

    val select_func_calls_into :
      (set -> spare:bool -> kernel_function -> set) ref
      (** To select every calls to the given function without the selection of
          its inputs/outputs. *)

    val select_func_annots :
      (set -> Mark.t -> spare:bool -> threat:bool -> user_assert:bool ->
        slicing_pragma:bool -> loop_inv:bool -> loop_var:bool ->
        kernel_function -> set) ref
      (** To select the annotations related to a function. *)

    (** {3 Internal use only} *)

    val pretty : (Format.formatter -> t -> unit) ref
      (** For debugging... Pretty print selection information. *)

    val get_function : (t -> kernel_function) ref
      (** The function related to an internal selection. *)

    val merge_internal : (t -> t -> t) ref
      (** The function related to an internal selection. *)

    val add_to_selects_internal : (t -> set -> set) ref
    val iter_selects_internal : ((t -> unit) -> set -> unit) ref
    val fold_selects_internal : (('a -> t -> 'a) -> 'a -> set -> 'a)

    val select_stmt_internal : (kernel_function -> ?select:t ->
                                  stmt -> Mark.t -> t) ref
      (** Internally used to select a statement :
          - if [is_ctrl_mark m],
          propagate ctrl_mark on ctrl dependencies of the statement
         - if [is_addr_mark m],
          propagate addr_mark on addr dependencies of the statement
         - if [is_data_mark m],
          propagate data_mark on data dependencies of the statement
         - mark the node with a spare_mark and propagate so that
           the dependencies that were not selected yet will be marked spare.
          When the statement is a call, its functionnal inputs/outputs are
          also selected (The call is still selected even it has no output).
          When the statement is a composed one (block, if, etc...),
            all the sub-statements are selected.
          @raise SlicingTypes.NoPdg if ?
        *)

    val select_label_internal : (kernel_function -> ?select:t ->
                                  Logic_label.t -> Mark.t -> t) ref

    val select_min_call_internal :
      (kernel_function -> ?select:t -> stmt -> Mark.t -> t) ref
      (** Internally used to select a statement call without its
          inputs/outputs so that it doesn't select the statements computing the
          inputs of the called function as [select_stmt_internal] would do.
          Raise [Invalid_argument] when the [stmt] isn't a call.
          @raise SlicingTypes.NoPdg if ?
      *)

    val select_stmt_zone_internal :
      (kernel_function -> ?select:t ->
       stmt -> before:bool -> Locations.Zone.t -> Mark.t -> t) ref
      (** Internally used to select a zone value at a program point.
          @raise SlicingTypes.NoPdg if ?
       *)

    val select_zone_at_entry_point_internal :
      (kernel_function -> ?select:t -> Locations.Zone.t -> Mark.t -> t) ref
      (** Internally used to select a zone value at the beginning of a function.
      * For a defined function, it is similar to [select_stmt_zone_internal]
      * with the initial statement, but it can also be used for undefined
      * functions.
      *
          @raise SlicingTypes.NoPdg if ?
       *)

    val select_zone_at_end_internal :
      (kernel_function -> ?select:t -> Locations.Zone.t -> Mark.t -> t) ref
      (** Internally used to select a zone value at the end of a function.
      * For a defined function, it is similar to [select_stmt_zone_internal]
      * with the return statement, but it can also be used for undefined
      * functions.
      *
          @raise SlicingTypes.NoPdg if ?
       *)

    val select_modified_output_zone_internal :
      (kernel_function -> ?select:t -> Locations.Zone.t -> Mark.t -> t) ref
      (** Internally used to select the statements that modify the
      * given zone considered as in output.
      * Be careful that it is NOT the same than selectiong the zone at end !
      * ( the 'undef' zone is not propagated...)
      * *)

    val select_stmt_ctrl_internal :
                   (kernel_function -> ?select:t -> stmt -> t) ref
      (** Internally used to select a statement reachability :
          Only propagate a ctrl_mark on the statement control dependencies.
          @raise SlicingTypes.NoPdg if ?
      *)

    val select_pdg_nodes_internal :
      (kernel_function -> ?select:t -> PdgTypes.Node.t list -> Mark.t -> t) ref
      (** Internally used to select PDG nodes :
          - if [is_ctrl_mark m],
          propagate ctrl_mark on ctrl dependencies of the statement
         - if [is_addr_mark m],
          propagate addr_mark on addr dependencies of the statement
         - if [is_data_mark m],
          propagate data_mark on data dependencies of the statement
         - mark the node with a spare_mark and propagate so that
           the dependencies that were not selected yet will be marked spare.
        *)

    val select_entry_point_internal :
                 (kernel_function -> ?select:t ->  Mark.t -> t) ref
    val select_return_internal :
                 (kernel_function -> ?select:t ->  Mark.t -> t) ref
    val select_decl_var_internal :
                 (kernel_function -> ?select:t ->  Cil_types.varinfo -> Mark.t -> t) ref
    val select_pdg_nodes :
      (set -> Mark.t  -> PdgTypes.Node.t list -> kernel_function -> set) ref
  end

  (** Function slice. *)
  module Slice : sig

    type t = SlicingTypes.sl_fct_slice
        (** Abtract data type for function slice. *)
    val dyn_t : t Type.t
        (** For dynamic type checking and journalization. *)

    val create : (Project.t -> kernel_function -> t) ref
      (** Used to get an empty slice (nothing selected) related to a
          function. *)

    val remove : (Project.t -> t -> unit) ref
      (** Remove the slice from the project. The slice shouldn't be called. *)

    val remove_uncalled : (Project.t -> unit) ref
      (** Remove the uncalled slice from the project. *)

    (** {3 Getters} *)

    val get_all: (Project.t -> kernel_function -> t list) ref
      (** Get all slices related to a function. *)

    val get_function : (t -> kernel_function) ref
      (** To get the function related to a slice *)

    val get_callers : (t -> t list) ref
      (** Get the slices having direct calls to a slice. *)

    val get_called_slice : (t -> stmt -> t option) ref
      (** To get the slice directly called by the statement of a slice.
          Returns None when the statement mark is bottom,
          or else the statement isn't a call
          or else the statement is a call to one or several (via pointer)
          source functions. *)

    val get_called_funcs : (t -> stmt -> kernel_function list) ref
      (** To get the source functions called by the statement of a slice.
          Returns an empty list when the statement mark is bottom,
          or else the statement isn't a call
          or else the statement is a call to a function slice. *)

    val get_mark_from_stmt : (t -> stmt -> Mark.t) ref
      (** Get the mark value of a statement. *)

    val get_mark_from_label : (t -> stmt -> Cil_types.label -> Mark.t) ref
      (** Get the mark value of a label. *)

    val get_mark_from_local_var : (t -> varinfo -> Mark.t) ref
      (** Get the mark value of local variable. *)

    val get_mark_from_formal : (t -> varinfo -> Mark.t) ref
      (** Get the mark from the formal of a function. *)

    val get_user_mark_from_inputs : (t -> Mark.t) ref
      (** Get a mark that is the merged user inputs marks of the slice *)

    (** {3 Internal use only} *)

    val get_num_id : (t -> int) ref

    val from_num_id : (Project.t -> kernel_function -> int -> t) ref

    val pretty : (Format.formatter -> t -> unit) ref
      (** For debugging... Pretty print slice information. *)

  end

  (** Requests for slicing jobs.
      Slicing resquests are part of a slicing project.
      So, user requests affect slicing project. *)
  module Request : sig

    val apply_all: (Project.t -> propagate_to_callers:bool -> unit) ref
      (** Apply all slicing requests. *)

    (** {3 Adding a request} *)

    val add_selection: (Project.t -> Select.set -> unit) ref
      (** Add a selection request to all slices (existing)
          of a function to the project requests. *)

    val add_persistent_selection: (Project.t -> Select.set -> unit) ref
      (** Add a persistent selection request to all slices (already existing or
          created later) of a function to the project requests. *)

    val add_persistent_cmdline : (Project.t -> unit) ref
      (** Add persistent selection from the command line. *)

    val is_already_selected_internal: (Slice.t -> Select.t -> bool) ref
      (** Return true when the requested selection is already selected into the
      * slice. *)

    val add_slice_selection_internal:
      (Project.t -> Slice.t -> Select.t -> unit) ref
      (** Internaly used to add a selection request for a function slice
          to the project requests. *)

    val add_selection_internal:
      (Project.t -> Select.t -> unit) ref
      (** Internaly used to add a selection request to the project requests.
          This selection will be applied to every slicies of the function
          (already existing or created later). *)

    val add_call_slice:
      (Project.t -> caller:Slice.t -> to_call:Slice.t -> unit) ref
      (** change every call to any [to_call] source or specialisation in order
          to call [to_call] in [caller]. *)

    val add_call_fun:
      (Project.t -> caller:Slice.t -> to_call:kernel_function -> unit) ref
      (** change every call to any [to_call] source or specialisation
      * in order to call the source function [to_call] in [caller] *)

    val add_call_min_fun:
      (Project.t -> caller:Slice.t -> to_call:kernel_function -> unit) ref
      (** For each call to [to_call] in [caller] such so that, at least, it
          will be visible at the end, ie. call either the source function or
          one of [to_call] slice (depending on the [slicing_level]). *)

    (** {3 Internal use only} *)

    val apply_all_internal: (Project.t -> unit) ref
      (** Internaly used to apply all slicing requests. *)

    val apply_next_internal: (Project.t -> unit) ref
      (** Internaly used to apply the first slicing request of the project list
          and remove it from the list.
          That may modify the contents of the remaing list.
          For exemple, new requests may be added to the list. *)

    val is_request_empty_internal: (Project.t -> bool) ref
      (** Internaly used to know if internal requests are pending. *)

    val merge_slices:
      (Project.t -> Slice.t  -> Slice.t -> replace:bool -> Slice.t) ref
        (** Build a new slice which marks is a merge of the two given slices.
            [choose_call] requests are added to the project in order to choose
            the called functions for this new slice.
            If [replace] is true, more requests are added to call this new
            slice instead of the two original slices. When these requests will
            be applied, the user will be able to remove those two slices using
            [Db.Slicing.Slice.remove]. *)

    val copy_slice:
      (Project.t -> Slice.t  -> Slice.t) ref
      (** Copy the input slice. The new slice is not called,
      * so it is the user responsability to change the calls if he wants to. *)

    val split_slice:
      (Project.t -> Slice.t  -> Slice.t list) ref
      (** Copy the input slice to have one slice for each call of the original
      * slice and generate requests in order to call them.
      * @return the newly created slices.
      *)

    val propagate_user_marks : (Project.t -> unit) ref
      (** Apply pending request then propagate user marks to callers
          recursively then apply pending requests *)

    val pretty : (Format.formatter -> Project.t -> unit) ref
      (** For debugging... Pretty print the resquest list. *)

  end

end


(** Signature common to some Inout plugin options. The results of
    the computations are available on a per function basis. *)
module type INOUTKF = sig

  type t

  val self_internal: State.t ref
  val self_external: State.t ref

  val compute : (kernel_function -> unit) ref

  val get_internal : (kernel_function -> t) ref
    (** Inputs/Outputs with local and formal variables *)

  val get_external : (kernel_function -> t) ref
    (** Inputs/Outputs without either local or formal variables *)

  (** {3 Pretty printing} *)

  val display : (Format.formatter -> kernel_function -> unit) ref
  val pretty : Format.formatter -> t -> unit

end
(** Signature common to inputs and outputs computations. The results
    are also available on a per-statement basis. *)
module type INOUT = sig
  include INOUTKF

  val statement : (stmt -> t) ref
  val kinstr : kinstr -> t option
end

(** State_builder.of read inputs.
    That is over-approximation of zones read by each function.
    @see <../inout/Inputs.html> internal documentation. *)
module Inputs : sig

  include INOUT with type t = Locations.Zone.t

  val expr : (stmt -> exp -> t) ref

  val self_with_formals: State.t ref

  val get_with_formals : (kernel_function -> t) ref
    (** Inputs with formals and without local variables *)

  val display_with_formals: (Format.formatter -> kernel_function -> unit) ref

end

(** State_builder.of outputs.
    That is over-approximation of zones written by each function.
    @see <../inout/Outputs.html> internal documentation. *)
module Outputs : sig
  include INOUT with type t = Locations.Zone.t
  val display_external : (Format.formatter -> kernel_function -> unit) ref
end

(** State_builder.of operational inputs.
    That is:
    - over-approximation of zones whose input values are read by each function,
    State_builder.of sure outputs
    - under-approximation of zones written by each function.
    @see <../inout/Context.html> internal documentation. *)
module Operational_inputs : sig
  include INOUTKF with type t = Inout_type.t
  val get_internal_precise: (?stmt:stmt -> kernel_function -> Inout_type.t) ref
    (** More precise version of [get_internal] function. If [stmt] is
        specified, and is a possible call to the given kernel_function,
        returns the operational inputs for this call (if option -inout-callwise
        has been set). *)

(**/**)
    (* Internal use *)
  module Record_Inout_Callbacks:
    Hook.Iter_hook with type param = Value_types.callstack * Inout_type.t
(**/**)
end


(**/**)
(** Do not use yet.
    @see <../inout/Derefs.html> internal documentation. *)
module Derefs : INOUT with type t = Locations.Zone.t
(**/**)

(** {3 GUI} *)

(** This function should be called from time to time by all analysers taking
    time. In GUI mode, this will make the interface reactive.
    @plugin development guide *)
val progress: (unit -> unit) ref

(** This exception may be raised by {!progress} to interrupt computations. *)
exception Cancel

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

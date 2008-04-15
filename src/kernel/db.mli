(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(* $Id: db.mli,v 1.467 2008/12/12 19:31:32 uid528 Exp $ *)

(** Database in which plugins are registered. 
    @plugin development guide *)

(**
   Other kernel modules:
   - {!Ast_info}: syntactic value directly computed from the Cil Ast
   - {!Cil_state}: the cil file
   - {!File}: Cil file initialization
   - {!Globals}: global variables, functions and annotations
   - {!Annotations}:  annotations associated with a statement
   - {!Kernel_function}: kernel functions
   - {!Stmts_graph}: the statement graph
   - {!Loop}: (natural) loops
   - {!Project} and associated files: {!Kind}, {!Datatype} and {!Computation}
   - {!Visitor}: frama-c visitors
*)

open Cil_types
open Cil
open Cilutil
open Db_types

(* ************************************************************************* *)
(** {2 Registering} *)
(* ************************************************************************* *)

type 'a p_ref = (*private*) 'a ref

val register: 
  journalize:((string * 'a Type.t) option) -> 
  'a p_ref -> 'a -> unit
  (** Plugins must register values with this function. 
      References are morally private. *)

val register_compute: 
  string -> 
  Project.Computation.t list ->
  (unit -> unit) p_ref -> (unit -> unit) -> unit

val register_guarded_compute: 
  string -> 
  (unit -> bool) ->
  (unit -> unit) p_ref -> (unit -> unit) -> unit

(** Frama-C main interface *)
module Main : sig

  val extend : (Format.formatter -> unit) -> unit
    (** Register a function to be called by the Frama-C main entry point. 
	@plugin development guide *)

  (**/**)
  val apply: Format.formatter -> unit 
    (** Apply entry points previously registered . *)
  (**/**)

end

(* ************************************************************************* *)
(** {2 Graphs} *)
(* ************************************************************************* *)

(** Callgraph computed by value analysis. It contains function pointers! *)
module Semantic_Callgraph : sig
  val topologically_iter_on_functions : ((kernel_function -> unit) -> unit) p_ref
    (** Compute values if required. *)
end

(* ************************************************************************* *)
(** {2 Values} *)
(* ************************************************************************* *)

(** The Value analysis itself.
    @see <../value/index.html> internal documentation. *)
module Value : sig

  type state = Relations_type.Model.t
      (** Internal state of the value analysis. *)

  type t = Cvalue_type.V.t
      (** Internal representation of a value. *)
      
  exception Aborted

  val self : Project.Computation.t
    (** Internal state of the value analysis from project viewpoints. 
	@plugin development guide *)

  val mark_as_computed: unit -> unit
    (** Indicate that the value analysis has been done already. *)

  val compute : (unit -> unit) p_ref
    (** Compute the value analysis using the entry point of the current
	project. You may set it with {!Globals.set_entry_point}. 
	@plugin development guide *)

  val is_computed: unit -> bool
    (** Return [true] iff the value analysis has been done. 
	@plugin development guide *)

  val degeneration_occurred:
    (Cil_types.kinstr -> Cil_types.lval option -> unit) p_ref
    (** This hook is called by the value analysis in the seldom
        case a total degeneration occurs. *)

  (** {3 Getters} *)

  val get_initial_state : kernel_function -> state
  val get_state : kinstr -> state

  val globals_state : unit -> state
    (** Return the default initial values of globals. This might be
        different from the initial state of the value analysis in
        the library case.*)

  val find : state -> Locations.location ->  t

  (** {3 Evaluations} *)

  val eval_lval :
    (with_alarms:CilE.warn_mode ->
     Locations.Zone.t option ->
     state ->
     lval ->
     Locations.Zone.t option * Cvalue_type.V.t) p_ref

  val eval_expr : (with_alarms:CilE.warn_mode -> state -> exp -> Cvalue_type.V.t) p_ref

  (** {3 Values and kernel functions} *)

  val expr_to_kernel_function :
    (kinstr
     -> with_alarms:CilE.warn_mode
     -> deps:Locations.Zone.t option
     -> exp
     -> Locations.Zone.t * kernel_function list) p_ref

  val expr_to_kernel_function_state :
    (state
     -> deps:Locations.Zone.t option
     -> exp
     -> Locations.Zone.t * kernel_function list) p_ref

  exception Not_a_call
  val call_to_kernel_function : stmt ->  kernel_function list
    (** Return the functions that can be called from this call.
    * @raise Not_a_call if the statement is not a call. *)

  val valid_behaviors: (kernel_function -> state -> funbehavior list) p_ref

  (** {3 Reachability} *)

  val is_accessible : kinstr -> bool
  val is_reachable : state -> bool
  val is_reachable_stmt : stmt -> bool

  (** {3 About kernel functions} *)

  exception Void_Function
  val find_return_loc : kernel_function -> Locations.location
    (** Return the location of the returned lvalue of the given function.
	@raise Void_Function is the function does not return any value. *)

  val is_called: (kernel_function -> bool) p_ref

  val callers: (kernel_function -> (kernel_function*stmt list) list) p_ref
    (** @return the list of callers with their call sites. Each function is present only once
        in the list. *)

  val never_terminates: (kernel_function -> bool) p_ref
    (** Return [true] if the function never reaches its return. *)

  (** {3 Values in kinstr} *)

  val access : (kinstr -> lval ->  t) p_ref
  val access_expr : (kinstr -> exp ->  t) p_ref
  val access_location : (kinstr -> Locations.location ->  t) p_ref

  val access_after : (kinstr -> lval -> t) p_ref
    (* @raise No_found if the kinstr has no accessible successors. *)
  val access_location_after : (kinstr -> Locations.location -> t) p_ref
    (* @raise No_found if the kinstr has no accessible successors. *)
  val lval_to_offsetmap_after : (kinstr -> lval ->
    Cvalue_type.V_Offsetmap.t option) p_ref
    (* @raise No_found if the kinstr has no accessible successors. *)

  (** {3 Locations of left values} *)

  val lval_to_loc :
    (kinstr -> with_alarms:CilE.warn_mode -> lval -> Locations.location) p_ref

  val lval_to_loc_with_deps :
    (kinstr
     -> with_alarms:CilE.warn_mode
      -> deps:Locations.Zone.t
      -> lval
      -> Locations.Zone.t * Locations.location) p_ref

  val lval_to_loc_with_deps_state :
    (state
      -> deps:Locations.Zone.t
      -> lval
      -> Locations.Zone.t * Locations.location) p_ref

  val lval_to_loc_state :
    (state -> lval -> Locations.location) p_ref

  val lval_to_offsetmap :
    ( kinstr -> lval -> with_alarms:CilE.warn_mode ->
      Cvalue_type.V_Offsetmap.t option) p_ref

  val lval_to_zone :
    (kinstr -> with_alarms:CilE.warn_mode -> lval -> Locations.Zone.t) p_ref

  val lval_to_zone_state :
    (state -> lval -> Locations.Zone.t) p_ref
    (** Does not emit alarms. *)

  val assigns_to_zone_inputs_state :
    (state -> identified_tsets assigns list -> Locations.Zone.t) p_ref

  (** {3 Callbacks} *)

  (** Actions to perform at end of each function analysis. *)
  module Record_Value_Callbacks:
    Hook.S with type param = (kernel_function * kinstr) list 
    * state Cilutil.InstrHashtbl.t

  (** Actions to perform at each treatment of a "call" statement. 
      @plugin development guide *)
  module Call_Value_Callbacks:
    Hook.S with type param = state * (kernel_function * kinstr) list

  (** {3 Pretty printing} *)

  val pretty : Format.formatter -> t -> unit
  val pretty_state_without_null : Format.formatter -> state -> unit
  val pretty_state : Format.formatter -> state -> unit

  val display : Format.formatter -> kernel_function -> unit
  val display_globals : Format.formatter -> unit -> unit

  (** {3 Internal use only} *)

  val noassert_get_state : kinstr -> state
    (** To be used during the value analysis itself (instead of
	{!get_state}). *)

  val initial_state_only_globals : (unit -> state) p_ref
  val update_table : kinstr -> state -> unit
  val memoize : (kernel_function -> unit) p_ref
(*  val compute_call :
    (kernel_function -> call_kinstr:kinstr -> state ->  (exp*t) list
       -> Cvalue_type.V_Offsetmap.t option (** returned value of [kernel_function] *) * state) p_ref
*)
  val merge_initial_state : kernel_function -> state -> unit
    (** Store an additional possible initial state for the given function as
	well as its values for actuals. *)
end

(** Functional dependencies between function inputs and function outputs.
    @see <../from/index.html> internal documentation. *)
module From : sig

  val compute_all : (unit -> unit) p_ref
  val compute_all_calldeps : (unit -> unit) p_ref
    
  val compute : (kernel_function -> unit) p_ref

  val is_computed: (kernel_function -> bool) p_ref
    (** Check whether the from analysis has been performed for the given
	function.
	@return true iff the analysis has been performed *)
 
  val get : (kernel_function -> Function_Froms.t) p_ref
  val access : (Locations.Zone.t -> from_model -> Locations.Zone.t) p_ref
  val find_deps_no_transitivity : (kinstr -> exp -> Locations.Zone.t) p_ref
  val self: Project.Computation.t p_ref
    (** @plugin development guide *)

  (** {3 Pretty printing} *)

  val pretty : (Format.formatter -> kernel_function -> unit) p_ref
  val display : (Format.formatter -> unit) p_ref

  (** {3 Internal use only} *)

  val update :
    (Locations.location -> Locations.Zone.t -> from_model -> from_model) p_ref

  module Callwise : sig
    val iter : ((kinstr -> Function_Froms.t -> unit) -> unit) p_ref
    val find : (kinstr -> Function_Froms.t) p_ref
  end
end

(** Functions used by another function.
    @see <../users/index.html> internal documentation. *)
module Users : sig
  val get: (kernel_function -> Kernel_function.Set.t) p_ref
end

(**/**)
(** Do not use yet. *)
module Memzone : sig
  type t = Memzone_type.Lmap_bitwise_with_empty_default.t
  val compute : (unit -> t) p_ref
  val pretty : (Format.formatter -> t -> unit) p_ref
end

(** Do not use yet. *)
module Access_path : sig
  type t = (Locations.Zone.t * Locations.Location_Bits.t) BaseUtils.BaseMap.t
  val compute: (Relations_type.Model.t -> BaseUtils.BaseSet.t -> t) p_ref
  val filter: (t -> Locations.Zone.t -> t) p_ref
  val pretty: (Format.formatter -> t -> unit) p_ref
end
(**/**)

(* ************************************************************************* *)
(** {2 Properties} *)
(* ************************************************************************* *)

(** Dealing with logical properties. *)
module Properties : sig

  (** Interpretation of logic terms. *)
  module Interp : sig

    (** {3 From C terms to logic terms} *)

    val lval : (kernel_function -> stmt -> string -> Cil_types.term_lval) p_ref
    val expr : (kernel_function -> stmt -> string -> Cil_types.term) p_ref

    (** {3 From logic terms to C terms} *)

    val term_lval_to_lval: (term_lval -> Cil_types.lval) p_ref
      (** @raise Invalid_argument if the argument is not a left value. *)

    val term_to_lval: (term -> Cil_types.lval) p_ref
      (** @raise Invalid_argument if the argument is not a left value. *)

    val term_to_exp: (term -> Cil_types.exp) p_ref
      (** @raise Invalid_argument if the argument is not a valid expression. *)

    val force_term_to_exp: 
      (result_type:typ -> term -> exp * opaque_term_env) p_ref

    val force_back_exp_to_term: (opaque_term_env -> exp -> term) p_ref

    val force_exp_to_term: (location -> exp -> term) p_ref

    val force_lval_to_term_lval: (location -> lval -> term_lval) p_ref

    val force_term_offset_to_offset:
      (result_type:typ -> term_offset -> offset * opaque_term_env) p_ref

    val force_back_offset_to_term_offset: 
      (opaque_term_env -> offset -> term_offset) p_ref

    val force_exp_to_predicate: (location -> exp -> predicate named) p_ref

    val force_exp_to_assertion: (location -> exp -> code_annotation) p_ref

    val force_term_lval_to_lval: 
      (result_type:typ -> term_lval -> lval * opaque_term_env) p_ref

    val force_back_lval_to_term_lval:
      (opaque_term_env -> lval -> term_lval) p_ref

    val from_range_to_comprehension: 
      (visitor_behavior -> Project.t -> file -> unit) p_ref

    val from_comprehension_to_range: 
      (visitor_behavior -> Project.t -> file -> unit) p_ref

    val force_tsets_elem_to_exp:
      (result_type:typ -> tsets_elem -> exp * opaque_term_env) p_ref

    val force_back_exp_to_tsets_elem:
      (opaque_term_env -> exp -> tsets_elem) p_ref

    val force_tsets_lval_to_lval: 
      (result_type:typ -> tsets_lval -> lval * opaque_term_env) p_ref

    val force_back_lval_to_tsets_lval:
      (opaque_term_env -> lval -> tsets_lval) p_ref

    val force_tsets_offset_to_offset:
      (result_type:typ -> tsets_offset -> offset * opaque_term_env) p_ref

    val force_back_offset_to_tsets_offset:
      (opaque_term_env -> offset -> tsets_offset) p_ref

    val force_tsets_elem_to_term: (tsets_elem -> term) p_ref

    val force_back_term_to_tsets_elem: (term -> tsets_elem) p_ref

    val force_tsets_lval_to_term_lval: (tsets_lval -> term_lval) p_ref

    val force_back_term_lval_to_tsets_lval: (term_lval -> tsets_lval) p_ref

    val term_offset_to_offset: (term_offset -> offset) p_ref
      (** @raise Invalid_argument if the argument is not a valid offset. *)

    val tsets_to_exp: (tsets -> Cil_types.exp list) p_ref
      (** @raise Invalid_argument if the argument is not a valid expression. *)

    val tsets_to_offset: (tsets -> offset list) p_ref
      (** @raise Invalid_argument in the following cases:
            - if one of the tsets does not have a valid offset.
            - if the tsets do not p_refer to the same base.
       *)

    val tsets_to_lval: (tsets -> Cil_types.lval list) p_ref
      (** @raise Invalid_argument if the argument does not denote a set
          of C tsets. *)

    val tsets_elem_to_lval: (tsets_elem -> Cil_types.lval list) p_ref
      (** @raise Invalid_argument if the argument does not denote a set of
          C tsets
       *)

    val tsets_elem_to_exp: (tsets_elem -> Cil_types.exp list) p_ref
      (** @raise Invalid_argument if the argument is not a valid expression. *)

    (** {3 From logic terms to Zone.t} *)

    module To_zone : sig
      type t_ctx = {state_opt:bool option; ki_opt:(stmt * bool) option; kf:Kernel_function.t}
      val mk_ctx_func_contrat: (kernel_function -> state_opt:bool option -> t_ctx) p_ref
        (** to build an interpretation context relative to function contracts. *)
      val mk_ctx_stmt_contrat: (kernel_function -> stmt -> state_opt:bool option -> t_ctx) p_ref
        (** to build an interpretation context relative to statement contracts. *)
      val mk_ctx_stmt_annot: (kernel_function -> stmt -> before:bool -> t_ctx) p_ref
        (** to build an interpretation context relative to statement annotations. *)
        
      type t = {before:bool ; ki:stmt ; zone:Locations.Zone.t}
      type t_decl = VarinfoSet.t
      type t_pragmas = {ctrl: Cilutil.StmtSet.t ; (* related to //@ slice pragma ctrl/expr *)
                        stmt: Cilutil.StmtSet.t}  (* related to statement assign and //@ slice pragma stmt *)
      val from_term:
	(term -> t_ctx -> t list * t_decl)
        p_ref
        (** Entry point to get zones
            needed to evaluate the [term] relative to the [ctx] of interpretation. *)

      val from_terms:
	(term list -> t_ctx -> t list * t_decl)
        p_ref
        (** Entry point to get zones
            needed to evaluate the list of [terms] relative to the [ctx] of interpretation. *)

      val from_pred:
	(predicate named -> t_ctx -> t list * t_decl)
        p_ref
        (** Entry point to get zones
            needed to evaluate the [predicate] relative to the [ctx] of interpretation. *)

      val from_preds:
	(predicate named list -> t_ctx -> t list * t_decl)
        p_ref
        (** Entry point to get zones
            needed to evaluate the list of [predicates] relative to the [ctx] of interpretation. *)

      val from_zones:
	(identified_tsets zone list -> t_ctx -> t list * t_decl)
        p_ref
        (** Entry point to get zones
            needed to evaluate the list of [predicates] relative to the [ctx] of interpretation. *)

      val from_zone:
	(identified_tsets zone -> t_ctx -> t list * t_decl)
        p_ref
        (** Entry point to get zones
            needed to evaluate the [zone] relative to the [ctx] of interpretation. *)

      val from_stmt_annot:
	(code_annotation -> before:bool -> stmt * kernel_function
	-> (t list * t_decl) * t_pragmas)
        p_ref
        (** Entry point to get zones
            needed to evaluate annotations of this [stmt]. *)

      val from_stmt_annots:
	((rooted_code_annotation before_after -> bool) option ->
	   stmt * kernel_function -> (t list * t_decl) * t_pragmas)
        p_ref
        (** Entry point to get zones needed to evaluate annotations of this
	    [stmt]. *)

      val from_func_annots:
	(((stmt -> unit) -> kernel_function -> unit) ->
	   (rooted_code_annotation before_after -> bool) option ->
	     kernel_function -> (t list * t_decl) * t_pragmas)
        p_ref
        (** Entry point to get zones
            needed to evaluate annotations of this [kf]. *)

      val code_annot_filter:
	(rooted_code_annotation before_after ->
	   ai:bool -> user_assert:bool -> slicing_pragma:bool ->
          loop_inv:bool -> loop_var:bool -> others:bool -> bool)
        p_ref
        (** To quickly build an annotation filter *)

    end

    (** {3 Internal use only} *)

    val code_annot :
      (kernel_function -> stmt -> before:bool -> string -> code_annotation)
      p_ref

  end

  val predicates_on_stmt :
    stmt -> 
    (predicate named*(annotation_status -> unit)) list 
    * (predicate named*(annotation_status -> unit)) list 
    * funspec option
      (** returns the predicates holding before, the predicates holding after 
          together with the function to update their status in-place
          and the contract (if any) for the given statement
     *)

  (** {3 Alarms} *)

  val synchronize_alarms : unit -> unit
    (** Transform current set of alarms into code properties. This has to be
	called at the end of an alarm generator. By example, this is
	automatically called at the end of {!Db.Value.compute}. *)

  val add_alarm :
    kernel_function -> stmt -> Alarms.t -> Cil_types.code_annotation -> unit
    (** Emit an alarm. {!synchronize_alarms} must be called as soon as one
	need to see the alarms as properties to be checked on the code. *)

  (** {3 Assertions} *)

  val add_assert : kernel_function -> stmt -> before:bool -> string -> unit
  val get_user_assert :
    kernel_function -> stmt -> before:bool -> Cil_types.code_annotation list

  (** {3 Weakest preconditions} *)

end
 
(* ************************************************************************* *)
(** {2 Plugins} *)
(* ************************************************************************* *)

(** Interface for the syntactic_callgraph plugin.
    @see <../syntactic_callgraph/index.html> internal documentation. *)
module Syntactic_callgraph: sig
  val dump: (unit -> unit) p_ref
end

(** Postdominators plugin.
    @see <../postdominators/index.html> internal documentation. *)
module Postdominators: sig
  val compute: (kernel_function -> unit) p_ref

  exception Top
    (** Used for {!stmt_postdominators} when the postdominators of a statement
	cannot be computed. It means that there is no path from this
	statement to the function return. *)

  val stmt_postdominators: (kernel_function -> stmt -> Cilutil.StmtSet.t) p_ref
    (** @raise Top (see above) *)

  val is_postdominator:
    (kernel_function -> opening:stmt -> closing:stmt -> bool) p_ref

  val display: (unit -> unit) p_ref

  val print_dot : (string -> kernel_function -> unit) p_ref
    (** Print a representation of the postdominators in a dot file
    * which name is [basename.function_name.dot]. *)
end

(** Dominators plugin.
    @see <../postdominators/index.html> internal documentation. *)
module Dominators: sig
  val compute: (kernel_function -> unit) p_ref

  exception Top
    (** Used for {!stmt_postdominators} when the postdominators of a statement
	cannot be computed. It means that there is no path from this
	statement to the function return. *)

  val stmt_dominators: (kernel_function -> stmt -> Cilutil.StmtSet.t) p_ref
    (** @raise Top (see above) *)

  val is_dominator:
    (kernel_function -> opening:stmt -> closing:stmt -> bool) p_ref

  val display: (unit -> unit) p_ref

  val print_dot : (string -> kernel_function -> unit) p_ref
    (** Print a representation of the postdominators in a dot file
    * which name is [basename.function_name.dot]. *)
end

(** Constant propagation plugin.
    @see <../constant_propagation/index.html> internal documentation. *)
module Constant_Propagation: sig
  val get : (Cilutil.StringSet.t -> cast_intro:bool -> Project.t) p_ref
    (** propagate constant into the functions given by name.
        note: the propagation is performed into all functions when the set is
	empty; and casts can be introduced when [cast_intro] is true. *)
end

(** Customisation for cxx plugin. *)
module Cxx: sig

  (** get the original C++ ast of the current project. *)
  val get_original_files: (unit -> Cc_ast.translationUnit list) p_ref

  (** change the C++ files of the current project. *)
  val set_original_files: (Cc_ast.translationUnit list -> unit) p_ref

  (** pretty-prints a translation unit *)
  val print_cxx: (Format.formatter -> Cc_ast.translationUnit -> unit) p_ref

  val loc_of_elsa_loc: (Cc_ast.location -> Cabs.cabsloc) p_ref

  (** {b Internal use only} *)

  val suffixes: string list

  val mangle_cmdline_name: ((unit ->string) -> (string -> unit) -> unit) p_ref

  val mangle_cmdline_names:
    ((unit -> Cilutil.StringSet.t) -> (string -> unit) ->
       (string -> unit) -> unit) p_ref
end

(** Customization for the combination of Cxx and Slicing plugin. *)
module CxxSlicing: sig
  val slice_cxx: (string option -> unit) p_ref
    (** what to do on original cxx files after a slice operation.
        Defaults to nothing. The optional argument is a filename
        (pretty-printing on stdout if set to None) *)
end

module Miel : sig
  val extract_all : (unit -> unit) p_ref
  val run_gui : (unit -> unit) p_ref
  val gui_present : bool p_ref
end

(** C to Jessie translation.
    @see <../jessie/index.html> internal documentation. *)
module Jessie : sig
  val run_analysis : (unit -> unit) p_ref
end

(** Weakest-Precondition computation
    @see <../wp/index.html> internal documentation. *)
module Wp : sig
  exception Failed of string
  val compute_with_cfg : (kernel_function -> unit) p_ref
  val compute_for_post : (kernel_function -> Fol.predicate) p_ref
end

(** Impact analysis.
    @see <../impact/index.html> internal documentation. *)
module Impact : sig
  val compute_pragmas: (unit -> unit) p_ref
    (** @plugin development guide *)
  val from_stmt: (stmt -> stmt list) p_ref
end

(** Security analysis.
    @see <../security/index.html> internal documentation. *)
module Security : sig

  val run_whole_analysis: (unit -> unit) p_ref
    (** Run all the security analysis. *)

  val run_ai_analysis: (unit -> unit) p_ref
    (** Only run the analysis by abstract interpretation. *)

  val run_slicing_analysis: (unit -> Project.t) p_ref
    (** Only run the security slicing pre-analysis. *)

  val get_component: (stmt -> stmt list) p_ref
  val get_direct_component: (stmt -> stmt list) p_ref
  val get_indirect_backward_component: (stmt -> stmt list) p_ref
  val get_forward_component: (stmt -> stmt list) p_ref

  val impact_analysis: (kernel_function -> stmt -> stmt list) p_ref

  val self: Project.Computation.t p_ref

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

  exception NotFound
    (** Raised by the [find_xxx] functions when the searched element is not in
	the PDG. The most common reason is when it is an unreachable
	statement. *)

  type t = PdgTypes.Pdg.t
      (** PDG type *)

  type t_node = PdgTypes.Node.t
      (** Type of the PDG nodes *)

  type t_node_key = PdgIndex.Key.t
      (** Those keys are used to identify elements of a function.
	  See {!module:PdgIndex.Key}
	  to know more about it and to get functions to build some keys. *)

  type t_nodes_and_undef = 
      (t_node * Locations.Zone.t option) list * Locations.Zone.t option
	(** type for the return value of many [find_xxx] functions when the
	    answer can be a list of nodes and an undef zone For each node, we
	    can also have which part is used in terms of zone ([None] means 
	    all) *)

  val self : Project.Computation.t p_ref

  (** {3 Getters} *)

  val get : (kernel_function -> t) p_ref
    (** Get the PDG of a function. Build it if it doesn't exist yet. *)

  val node_key : (t_node -> t_node_key) p_ref

  val from_same_fun : t -> t -> bool

  (** {3 Finding PDG nodes} *)

  val find_decl_var_node : (t -> Cil_types.varinfo -> t_node) p_ref
    (** Get the node corresponding the declaration of a local variable or a
	formal parameter.
	@raise NotFound if the variable is not declared in this function.
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val find_ret_output_node : (t -> t_node) p_ref
    (** Get the node corresponding return stmt.
	@raise NotFound if the ouptut state in unreachable
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val find_output_nodes :
    (t -> PdgIndex.Signature.t_out_key -> t_nodes_and_undef) p_ref
    (** Get the nodes corresponding to a call output key in the called pdg.
    	@raise NotFound if the ouptut state in unreachable
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val find_input_node : (t -> int -> t_node) p_ref
    (** Get the node corresponding to a given input (parameter).
        @raise NotFound if the number is not an input number.
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val find_all_inputs_nodes : (t -> t_node list) p_ref
    (** Get the nodes corresponding to all inputs.
	{!node_key} can be used to know their numbers.
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val find_stmt_node : (t -> Cil_types.stmt -> t_node) p_ref
    (** Get the node corresponding to the statement.
	It shouldn't be a call statement.
        See also {!find_simple_stmt_nodes} or {!find_call_stmts}.
    	@raise NotFound if the given statement is unreachable.
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val find_simple_stmt_nodes : (t -> Cil_types.stmt -> t_node list) p_ref
    (** Get the nodes corresponding to the statement.
	It is usualy composed of only one node (see {!find_stmt_node}),
        except for call statement.
        Be cap_reful that for block statements, it only retuns a node 
        corresponding to the elementary stmt 
                           (see {!find_stmt_and_blocks_nodes} for more)
    	@raise NotFound if the given statement is unreachable.
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val find_stmt_and_blocks_nodes : (t -> Cil_types.stmt -> t_node list) p_ref
    (** Get the nodes corresponding to the statement like
    * {!find_simple_stmt_nodes} but also add the nodes of the enclosed
    * statements if [stmt] contains blocks.
    	@raise NotFound if the given statement is unreachable.
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val find_top_input_node : (t -> t_node) p_ref
    (** @raise NotFound if there is no top input in the PDG.
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val find_entry_point_node : (t -> t_node) p_ref
    (** Find the node that represent the entry point of the function, i.e. the
	higher level block.
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val find_location_nodes_at_stmt :
    (t -> Cil_types.stmt -> before:bool -> Locations.Zone.t ->
      t_nodes_and_undef) p_ref
    (** Find the nodes that define the value of the location at the given
	program point. Also return a zone that might be undefined at that
	point.
	@raise NotFound if the given statement is unreachable.
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val find_location_nodes_at_end :
    (t -> Locations.Zone.t -> t_nodes_and_undef) p_ref
    (** Same than {!find_location_nodes_at_stmt} for the program point located
	at the end of the function.
	@raise NotFound if the output state is unreachable.
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val find_location_nodes_at_begin :
    (t -> Locations.Zone.t -> t_nodes_and_undef) p_ref
    (** Same than {!find_location_nodes_at_stmt} for the program point located
	at the beginning of the function. 
       Notice that it can only find formal argument nodes. 
       The remaining zone (implicit input) is returned as undef.
	@raise NotFound if the output state is unreachable.
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val find_call_stmts:
    (kernel_function -> caller:kernel_function -> Cil_types.stmt list) p_ref
    (** Find the call statements to the function (can maybe be somewhere
	else).
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val find_call_ctrl_node : (t ->  Cil_types.stmt -> t_node) p_ref
    (** @raise NotFound if the call is unreachable.
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val find_call_input_node : (t ->  Cil_types.stmt -> int -> t_node) p_ref
    (** @raise NotFound if the call is unreachable or has no such input.
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val find_call_output_node : (t ->  Cil_types.stmt -> t_node) p_ref
    (** @raise NotFound if the call is unreachable or has no output node.
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val find_code_annot_nodes :
    (t -> before:bool -> Cil_types.stmt -> Cil_types.code_annotation -> 
       t_node list * (t_nodes_and_undef)) p_ref
    (** The first part of the result are the control dependencies nodes
    * of the annotation, and the second part is similar to 
    * [find_location_nodes_at_stmt] result but  for all the locations
    * needed by the annotation.
    * @raise NotFound if the statement is unreachable.
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val find_fun_precond_nodes :
    (t -> Cil_types.predicate -> (t_nodes_and_undef)) p_ref

  val find_fun_postcond_nodes :
    (t -> Cil_types.predicate -> (t_nodes_and_undef)) p_ref

  val find_fun_variant_nodes :
    (t -> Cil_types.term -> (t_nodes_and_undef)) p_ref

  (** {3 Propagation} *)

  val find_call_out_nodes_to_select : 
    (t -> t_node list -> t ->  Cil_types.stmt -> t_node list) p_ref
  (** [find_call_out_nodes_to_select pdg_called called_selected_nodes
                                     pdg_caller call_stmt]
    @return the call outputs nodes [out] such that 
            [find_output_nodes pdg_called out_key] 
            intersects [called_selected_nodes].
  *)

  val find_in_nodes_to_select_for_this_call :
    (t -> t_node list -> Cil_types.stmt -> t -> t_node list) p_ref
    (** [find_in_nodes_to_select_for_this_call
    *            pdg_caller caller_selected_nodes call_stmt pdg_called]
    * @return the called input nodes such that the corresponding nodes
    * in the caller intersect [caller_selected_nodes] *)

  (** {3 Dependencies} *)

  val direct_dpds : (t -> t_node -> t_node list) p_ref
    (** Get the nodes to which the given node directly depend on.
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val direct_ctrl_dpds : (t -> t_node -> t_node list) p_ref
    (** Similar to {!direct_dpds}, but for control dependencies only.
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val direct_data_dpds : (t -> t_node -> t_node list) p_ref
    (** Similar to {!direct_dpds}, but for data dependencies only.
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val direct_addr_dpds : (t -> t_node -> t_node list) p_ref
    (** Similar to {!direct_dpds}, but for address dependencies only.
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val all_dpds : (t -> t_node list -> t_node list) p_ref
    (** Transitive closure of {!direct_dpds} for all the given nodes.
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val all_data_dpds : (t -> t_node list -> t_node list) p_ref
    (** Gives the data dependencies of the given nodes, and recursively, all
	the dependencies of those nodes (regardless to their kind).
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val all_ctrl_dpds : (t -> t_node list -> t_node list) p_ref
    (** Similar to {!all_data_dpds} for control dependencies.
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val all_addr_dpds : (t -> t_node list -> t_node list) p_ref
    (** Similar to {!all_data_dpds} for address dependencies.
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val direct_uses : (t -> t_node -> t_node list) p_ref
    (** build a list of all the nodes that have direct dependencies on the
	given node.
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val direct_ctrl_uses : (t -> t_node -> t_node list) p_ref
    (** Similar to {!direct_uses}, but for control dependencies only.
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val direct_data_uses : (t -> t_node -> t_node list) p_ref
    (** Similar to {!direct_uses}, but for data dependencies only.
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val direct_addr_uses : (t -> t_node -> t_node list) p_ref
    (** Similar to {!direct_uses}, but for address dependencies only.
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val all_uses : (t -> t_node list -> t_node list) p_ref
    (** build a list of all the nodes that have dependencies (even indirect) on
	the given nodes.
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val custom_related_nodes :
    ((t_node -> t_node list) -> t_node list -> t_node list) p_ref
    (** [custom_related_nodes get_dpds node_list] build a list, starting from
	the node in [node_list], and recursively add the nodes given by the
	function [get_dpds].  For this function to work well, it is important
	that [get_dpds n] returns a subset of the nodes directly related to
	[n], ie a subset of [direct_uses] U [direct_dpds].
	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  val iter_nodes : ((t_node -> unit) -> t -> unit) p_ref
    (** apply a given function to all the PDG nodes
 	@raise Bottom if given PDG is bottom.
	@raise Top if the given pdg is top. *)

  (** {3 Pretty printing} *)

  val extract : (t -> string -> unit) p_ref
    (** Pretty print pdg into a dot file.
	@see <../pdg/index.html> PDG internal documentation. *)

  val pretty_node : (bool -> Format.formatter -> t_node -> unit) p_ref
    (** Pretty print information on a node :
    * with [short=true] if only print a number of the node,
    * else it prints a bit more. *)

  val pretty_key : (Format.formatter -> t_node_key -> unit) p_ref
    (** Pretty print information on a node key *)

  val pretty : (?bw:bool -> Format.formatter -> t -> unit) p_ref
    (** For debugging... Pretty print pdg information. 
    * Print codependencies rather than dependencies if [bw=true].
    * *)

  (** {3 Functors to compute marks for the PDG} *)

  (** [F_FctMarks] can be used to propagate marks 
      It also provides information for an interprocedural analysis.
      Alternatively, one can use [F_ProjMarks] below. *)
  module F_FctMarks (M:PdgMarks.T_Mark)
    : PdgMarks.T_Fct with type t_mark = M.t
		      and type t_call_info = M.t_call_info

  (* [F_ProjMarks] handle the full interprocedural propagation
      (cf. [Pdg.Register.F_Proj]) *)

                                (*
  type 't_mark t_info_caller_inputs = 't_mark PdgMarks.t_info_caller_inputs
  type 't_mark t_info_inter = 't_mark PdgMarks.t_info_inter
  *)

end

(** Interface for the Scope plugin.
    @see <../scope/index.html> internal documentation. *)
module Scope : sig
  val get_data_scope_at_stmt : 
    (kernel_function -> stmt -> lval -> 
       Cilutil.StmtSet.t * Cilutil.StmtSet.t * Cilutil.StmtSet.t) p_ref
    (** 
    * @raise Kernel_function.No_Definition if [kf] has no definition.
    * @return 3 statement sets related to the value of [lval] before [stmt] :
    * - the forward selection,
    * - the both way selection,
    * - the backward selection.
    *)

  val get_prop_scope_at_stmt : 
    (kernel_function -> stmt -> code_annotation -> 
       Cilutil.StmtSet.t * Cilutil.StmtSet.t) p_ref
  val check_asserts : (unit -> unit) p_ref

  val get_defs : (kernel_function -> stmt -> lval -> 
                    (Cilutil.StmtSet.t * Locations.Zone.t option) option)p_ref
    (** @return the set of statements that define [lval] before [stmt] in [kf].
    * Also returns the zone that is possibly not defined. 
    * Can return [None] when the information is not available (Pdg missing).
    * *)

  (** {3 Zones} *)

  type t_zones = Locations.Zone.t Inthash.t
  val build_zones : 
      (kernel_function -> stmt -> lval -> StmtSet.t * t_zones) p_ref
  val pretty_zones : (Format.formatter -> t_zones -> unit) p_ref
  val get_zones : (t_zones ->  Cil_types.stmt -> Locations.Zone.t) p_ref

end

(** Interface for the unused code detection.
    @see <../sparecode/index.html> internal documentation. *)
module Sparecode : sig
  val get: (select_annot:bool -> select_slice_pragma:bool -> Project.t) p_ref
     (** Remove in each function what isn't used to compute its outputs,
      *   or its annotations when [select_annot] is true,
      *   or its slicing pragmas when [select_slice_pragmas] is true.
      *  @return a new project where the sparecode has been removed.
      *)
  val rm_unused_globals : (?project:Project.t -> unit -> unit) p_ref
    (** Remove  unused global types and variables from the given project
    * (the current one if no project given) *)
end

(** Interface for the occurrence plugin.
    @see <../occurrence/index.html> internal documentation. *)
module Occurrence: sig
  type t = (kinstr * lval) list
  val get: (varinfo -> t) p_ref
    (** Return the occurrences of the given varinfo.
	An occurrence [ki, lv] is a left-value [lv] which uses the location of
	[vi] at the position [ki]. 
	@plugin development guide *)
  val get_last_result: (unit -> (t * varinfo) option) p_ref
    (** @return the last result computed by occurrence *)
  val print_all: (unit -> unit) p_ref
    (** Print all the occurrence of each variable declarations. *)
  val self: Project.Computation.t p_ref
    (** @plugin development guide *)
end

(** Interface for the slicing tool.
    @see <../slicing/index.html> internal documentation. *)
module Slicing : sig

  exception No_Project

  val self: Project.Computation.t p_ref
    (** Internal state of the slicing tool from project viewpoints. *)


  (** Slicing project management. *)
  module Project : sig

    type t = SlicingTypes.sl_project
        (** Abstract data type for slicing project. *)

    val mk_project : (string -> t) p_ref
      (** To use to start a new slicing project.
          Several projects from a same current project can be managed. *)

    val get_all : (unit -> t list) p_ref
      (** Get all slicing projects. *)

    val set_project : (t option -> unit) p_ref
      (** Get the current project. *)

    val get_project : (unit -> t option) p_ref
      (** Get the current project. *)

    val get_name : (t -> string) p_ref
      (** Get the slicing project name. *)

    (** {3 Kernel function} *)

    val is_called : (t -> kernel_function -> bool) p_ref
      (** Return [true] iff the source function is called (even indirectly via
	  transitivity) from a [Slice.t]. *)

    val has_persistent_selection : (t -> kernel_function -> bool) p_ref
      (** return [true] iff the source function has persistent selection *)

    val change_slicing_level : (t -> kernel_function -> int -> unit) p_ref
      (** change the slicing level of this function
          (see the [-slicing-level] option documentation to know the meaning of the
          number)
          @raise SlicingTypes.ExternalFunction if [kf] has no definition.
          @raise SlicingTypes.WrongSlicingLevel if [n] is not valid.
      *)

    (** {3 Extraction} *)

    val extract : (string ->
                   ?f_slice_names:(kernel_function -> bool  -> int -> string) ->
                   t -> Project.t) p_ref
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

    val print_dot : (filename:string -> title:string -> t -> unit) p_ref
      (** Print a representation of the slicing project (call graph)
          in a dot file which name is the given string. *)

    (** {3 Internal use only} *)

    val pretty : (Format.formatter -> t -> unit) p_ref
      (** For debugging... Pretty print project information. *)

    val is_directly_called_internal : (t -> kernel_function -> bool) p_ref
      (** Return [true] if the source function is directly (even via pointer
	  function) called from a [Slice.t]. *)

  end

  (** Acces to slicing results. *)
  module Mark : sig

    type t = SlicingTypes.sl_mark
        (** Abtract data type for mark value. *)

    val make : (data:bool -> addr:bool -> ctrl:bool -> t) p_ref
      (** To construct a mark such as
          [(is_ctrl result, is_data result, isaddr result) =
          (~ctrl, ~data, ~addr)],
          [(is_bottom result) = false] and
          [(is_spare result) = not (~ctrl || ~data || ~addr)]. *)

    val compare : (t -> t -> int) p_ref
      (** A total ordering function similar to the generic structural
	  comparison function [compare].
          Can be used to build a map from [t] marks to, for exemple, colors for
	  the GUI. *)

    val is_bottom : (t -> bool) p_ref
      (** [true] iff the mark is empty: it is the only case where the
	  associated element is invisible. *)

    val is_spare : (t -> bool) p_ref
      (** Smallest visible mark. Usually used to mark element that need to be
	  visible for compilation purpose, not really for the selected
	  computations. *)

    val is_data : (t -> bool) p_ref
      (** The element is used to compute selected data.
	  Notice that a mark can be [is_data] and/or [is_ctrl] and/or [is_addr]
	  at the same time. *)

    val is_ctrl : (t -> bool) p_ref
      (** The element is used to control the program point of a selected
	  data. *)

    val is_addr : (t -> bool) p_ref
      (** The element is used to compute the address of a selected data. *)

    val get_from_src_func : (Project.t -> kernel_function -> t) p_ref
      (** The mark [m] related to all statements of a source function [kf].
          Property : [is_bottom (get_from_func proj kf) = not (Project.is_called proj kf) ] *)

    (** {3 Internal use only} *)

    val pretty : (Format.formatter -> t -> unit) p_ref
      (** For debugging... Pretty mark information. *)

  end

  (** Slicing selections. *)
  module Select : sig

    type t = SlicingTypes.sl_select
        (** Internal selection. *)

    type t_set = SlicingTypes.sl_selects
        (** Set of colored selections. *)

    val empty_selects : (unit -> t_set) p_ref
      (** Empty selection. *)

    val select_stmt :
      (t_set -> spare:bool -> stmt -> kernel_function -> t_set) p_ref
      (** To select a statement. *)

    val select_stmt_ctrl :
      (t_set -> spare:bool -> stmt -> kernel_function -> t_set) p_ref
      (** To select a statement reachability.
          Note: add also a transparent selection on the whole statement. *)

    val select_stmt_lval_rw :
      (t_set -> Mark.t -> rd:Cilutil.StringSet.t -> wr:Cilutil.StringSet.t -> stmt ->
        scope:stmt -> eval:stmt -> kernel_function -> t_set) p_ref
      (** To select rw accesses to lvalues (given as string) related to a statement.
          Variables of [~rd] and [~wr] string are bounded
          relatively to the scope of the statement [~scope].
          The interpretation of the address of the lvalues is
          done just before the execution of the statement [~eval].
          The selection preserve the [~rd] and ~[wr] accesses contained into the statement [ki]. 
          Note: add also a transparent selection on the whole statement. *)

    val select_stmt_lval :
      (t_set -> Mark.t -> Cilutil.StringSet.t -> before:bool -> stmt ->
        scope:stmt -> eval:stmt -> kernel_function -> t_set) p_ref
      (** To select lvalues (given as string) related to a statement.
          Variables of [lval_str] string are bounded
          relatively to the scope of the statement [~scope].
          The interpretation of the address of the lvalue is
          done just before the execution of the statement [~eval].
          The selection preserve the value of these lvalues before or
          after (c.f. boolean [~before]) the statement [ki]. 
          Note: add also a transparent selection on the whole statement. *)

    val select_stmt_zone :
      (t_set -> Mark.t -> Locations.Zone.t -> before:bool -> stmt ->
	kernel_function -> t_set) p_ref
      (** To select a zone value related to a statement.
          Note: add also a transparent selection on the whole statement. *)

    val select_stmt_term :
      (t_set -> Mark.t -> term -> before:bool -> stmt ->
	kernel_function -> t_set) p_ref
      (** To select a predicate value related to a statement.
          Note: add also a transparent selection on the whole statement. *)

    val select_stmt_pred :
      (t_set -> Mark.t -> predicate named -> before:bool -> stmt ->
	kernel_function -> t_set) p_ref
      (** To select a predicate value related to a statement.
          Note: add also a transparent selection on the whole statement. *)

    val select_stmt_annot :
      (t_set -> Mark.t -> spare:bool -> code_annotation -> before:bool -> stmt -> kernel_function -> t_set) p_ref
      (** To select the annotations related to a statement.
          Note: add also a transparent selection on the whole statement. *)

    val select_stmt_annots :
      (t_set -> Mark.t -> spare:bool -> ai:bool -> user_assert:bool ->
	slicing_pragma:bool -> loop_inv:bool -> loop_var:bool ->
        stmt -> kernel_function -> t_set) p_ref
      (** To select the annotations related to a statement.
          Note: add also a transparent selection on the whole statement. *)

    val select_func_lval_rw :
      (t_set -> Mark.t -> rd:Cilutil.StringSet.t -> wr:Cilutil.StringSet.t ->
        scope:stmt -> eval:stmt -> kernel_function -> t_set) p_ref
      (** To select rw accesses to lvalues (given as a string) related to a function.
          Variables of [~rd] and [~wr] string are bounded
          relatively to the scope of the statement [~scope].
          The interpretation of the address of the lvalues is
          done just before the execution of the statement [~eval].
          The selection preserve the value of these lvalues into the whole project. *)

    val select_func_lval :
      (t_set -> Mark.t -> Cilutil.StringSet.t -> kernel_function -> t_set) p_ref
      (** To select lvalues (given as a string) related to a function.
          Variables of [lval_str] string are bounded
          relatively to the scope of the first statement of [kf].
          The interpretation of the address of the lvalues is
          done just before the execution of the first statement [kf].
          The selection preserve the value of these lvalues before
          execution of the return statement. *)
      
    val select_func_zone :
      (t_set -> Mark.t -> Locations.Zone.t -> kernel_function -> t_set) p_ref
      (** To select an output zone related to a function. *)

    val select_func_return :
      (t_set -> spare:bool -> kernel_function -> t_set) p_ref
      (** To select the function result (returned value). *)

    val select_func_calls_to :
      (t_set -> spare:bool -> kernel_function -> t_set) p_ref
      (** To select every calls to the given function, i.e. the call keeps
          its semantics in the slice. *)

    val select_func_calls_into :
      (t_set -> spare:bool -> kernel_function -> t_set) p_ref
      (** To select every calls to the given function without the selection of
	  its inputs/outputs. *)

    val select_func_annots :
      (t_set -> Mark.t -> spare:bool -> ai:bool -> user_assert:bool ->
	slicing_pragma:bool -> loop_inv:bool -> loop_var:bool ->
        kernel_function -> t_set) p_ref
      (** To select the annotations related to a function. *)

    (** {3 Internal use only} *)

    val pretty : (Format.formatter -> t -> unit) p_ref
      (** For debugging... Pretty print selection information. *)

    val get_function : (t -> kernel_function) p_ref
      (** The function related to an internal selection. *)

    val merge_internal : (t -> t -> t) p_ref
      (** The function related to an internal selection. *)

    val add_to_selects_internal : (t -> t_set -> unit) p_ref
    val iter_selects_internal : ((t -> unit) -> t_set -> unit) p_ref
    val fold_selects : (('a -> t -> 'a) -> 'a -> t_set -> 'a)

    val select_stmt_internal : (kernel_function -> ?select:t ->
                                  stmt -> Mark.t -> t) p_ref
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

    val select_min_call_internal :
      (kernel_function -> ?select:t -> stmt -> Mark.t -> t) p_ref
      (** Internally used to select a statement call without its
	  inputs/outputs so that it doesn't select the statements computing the
	  inputs of the called function as [select_stmt_internal] would do.
          Raise [Invalid_argument] when the [stmt] isn't a call.
          @raise SlicingTypes.NoPdg if ?
      *)

    val select_stmt_zone_internal :
      (kernel_function -> ?select:t ->
       stmt -> before:bool -> Locations.Zone.t -> Mark.t -> t) p_ref
      (** Internally used to select a zone value at a program point.
          @raise SlicingTypes.NoPdg if ?
       *)

    val select_zone_at_entry_point_internal :
      (kernel_function -> ?select:t -> Locations.Zone.t -> Mark.t -> t) p_ref
      (** Internally used to select a zone value at the beginning of a function.
      * For a defined function, it is similar to [select_stmt_zone_internal]
      * with the initial statement, but it can also be used for undefined
      * functions.
      *
          @raise SlicingTypes.NoPdg if ?
       *)

    val select_zone_at_end_internal :
      (kernel_function -> ?select:t -> Locations.Zone.t -> Mark.t -> t) p_ref
      (** Internally used to select a zone value at the end of a function.
      * For a defined function, it is similar to [select_stmt_zone_internal]
      * with the return statement, but it can also be used for undefined
      * functions.
      *
          @raise SlicingTypes.NoPdg if ?
       *)

    val select_modified_output_zone_internal :
      (kernel_function -> ?select:t -> Locations.Zone.t -> Mark.t -> t) p_ref
      (** Internally used to select the statements that modify the
      * given zone considered as in output.
      * Be cap_reful that it is NOT the same than selectiong the zone at end ! 
      * ( the 'undef' zone is not propagated...)
      * *)

    val select_stmt_ctrl_internal : 
                   (kernel_function -> ?select:t -> stmt -> t) p_ref
      (** Internally used to select a statement reachability :
          Only propagate a ctrl_mark on the statement control dependencies.
          @raise SlicingTypes.NoPdg if ?
      *)

    val select_pdg_nodes_internal :
      (kernel_function -> ?select:t -> Pdg.t_node list -> Mark.t -> t) p_ref
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
                   (kernel_function -> ?select:t ->  Mark.t -> t) p_ref
    val select_return_internal :
                   (kernel_function -> ?select:t ->  Mark.t -> t) p_ref
    val select_pdg_nodes :
      (t_set -> Mark.t  -> Pdg.t_node list -> kernel_function -> t_set) p_ref
  end

  (** Function slice. *)
  module Slice : sig

    type t = SlicingTypes.sl_fct_slice
        (** Abtract data type for function slice. *)

    val create : (Project.t -> kernel_function -> t) p_ref
      (** Used to get an empty slice (nothing selected) related to a
	  function. *)

    val remove : (Project.t -> t -> unit) p_ref
      (** Remove the slice from the project. The slice shouldn't be called. *)

    val remove_uncalled : (Project.t -> unit) p_ref
      (** Remove the uncalled slice from the project. *)

    (** {3 Getters} *)

    val get_all: (Project.t -> kernel_function -> t list) p_ref
      (** Get all slices related to a function. *)

    val get_function : (t -> kernel_function) p_ref
      (** To get the function related to a slice *)

    val get_callers : (t -> t list) p_ref
      (** Get the slices having direct calls to a slice. *)

    val get_called_slice : (t -> stmt -> t option) p_ref
      (** To get the slice directly called by the statement of a slice.
          Returns None when the statement mark is bottom,
          or else the statement isn't a call
          or else the statement is a call to one or several (via pointer)
	  source functions. *)

    val get_called_funcs : (t -> stmt -> kernel_function list) p_ref
      (** To get the source functions called by the statement of a slice.
          Returns an empty list when the statement mark is bottom,
          or else the statement isn't a call
          or else the statement is a call to a function slice. *)

    val get_mark_from_stmt : (t -> stmt -> Mark.t) p_ref
      (** Get the mark value of a statement. *)

    val get_mark_from_label : (t -> stmt -> Cil_types.label -> Mark.t) p_ref
      (** Get the mark value of a label. *)

    val get_mark_from_local_var : (t -> varinfo -> Mark.t) p_ref
      (** Get the mark value of local variable. *)

    val get_mark_from_formal : (t -> varinfo -> Mark.t) p_ref
      (** Get the mark from the formal of a function. *)

    val get_user_mark_from_inputs : (t -> Mark.t) p_ref
      (** Get a mark that is the merged user inputs marks of the slice *)

    (** {3 Internal use only} *)

    val pretty : (Format.formatter -> t -> unit) p_ref
      (** For debugging... Pretty print slice information. *)

  end

  (** Requests for slicing jobs.
      Slicing resquests are part of a slicing project.
      So, user requests affect slicing project. *)
  module Request : sig

    val apply_all: (Project.t -> propagate_to_callers:bool -> unit) p_ref
      (** Apply all slicing requests. *)

    (** {3 Adding a request} *)

    val add_selection: (Project.t -> Select.t_set -> unit) p_ref
      (** Add a selection request to all slices (existing)
          of a function to the project requests. *)

    val add_persistent_selection: (Project.t -> Select.t_set -> unit) p_ref
      (** Add a persistent selection request to all slices (already existing or
	  created later) of a function to the project requests. *)

    val add_persistent_cmdline : (Project.t -> unit) p_ref
      (** Add persistent selection from the command line. *)
      
    val is_already_selected_internal: (Slice.t -> Select.t -> bool) p_ref
      (** Return true when the requested selection is already selected into the
      * slice. *)

    val add_slice_selection_internal:
      (Project.t -> Slice.t -> Select.t -> unit) p_ref
      (** Internaly used to add a selection request for a function slice
          to the project requests. *)

    val add_selection_internal:
      (Project.t -> Select.t -> unit) p_ref
      (** Internaly used to add a selection request for a function slice
          to the project requests.
          This selection will be applied to every slicies of the function
          (already existing or created later). *)

    val add_call_slice:
      (Project.t -> caller:Slice.t -> to_call:Slice.t -> unit) p_ref
      (** change every call to any [to_call] source or specialisation in order
          to call [to_call] in [caller]. *)

    val add_call_fun:
      (Project.t -> caller:Slice.t -> to_call:kernel_function -> unit) p_ref
      (** change every call to any [to_call] source or specialisation
      * in order to call the source function [to_call] in [caller] *)

    val add_call_min_fun:
      (Project.t -> caller:Slice.t -> to_call:kernel_function -> unit) p_ref
      (** For each call to [to_call] in [caller] such so that, at least, it
	  will be visible at the end, ie. call either the source function or
	  one of [to_call] slice (depending on the [slicing_level]). *)

    (** {3 Internal use only} *)

    val apply_all_internal: (Project.t -> unit) p_ref
      (** Internaly used to apply all slicing requests. *)

    val apply_next_internal: (Project.t -> unit) p_ref
      (** Internaly used to apply the first slicing request of the project list
	  and remove it from the list.
          That may modify the contents of the remaing list.
          For exemple, new requests may be added to the list. *)

    val merge_slices:
      (Project.t -> Slice.t  -> Slice.t -> replace:bool -> Slice.t) p_ref
        (** Build a new slice which marks is a merge of the two given slices.
	    [choose_call] requests are added to the project in order to choose
	    the called functions for this new slice.
            If [replace] is true, more requests are added to call this new
	    slice instead of the two original slices. When these requests will
	    be applied, the user will be able to remove those two slices using
	    [Db.Slicing.Slice.remove]. *)

    val copy_slice:
      (Project.t -> Slice.t  -> Slice.t) p_ref
      (** Copy the input slice. The new slice is not called,
      * so it is the user responsability to change the calls if he wants to. *)

    val split_slice:
      (Project.t -> Slice.t  -> Slice.t list) p_ref
      (** Copy the input slice to have one slice for each call of the original
      * slice and generate requests in order to call them.
      * @return the newly created slices.
      *)

    val propagate_user_marks : (Project.t -> unit) p_ref
      (** Apply pending request then propagate user marks to callers
	  recursively then apply pending requests *)

    val pretty : (Format.formatter -> Project.t -> unit) p_ref
      (** For debugging... Pretty print the resquest list. *)

  end

end

(** Signature for inputs and/or outputs computations. *)
module type INOUT = sig

  type t

  val self_internal: Project.Computation.t p_ref
  val self_external: Project.Computation.t p_ref

  val compute : (kernel_function -> unit) p_ref

  val get_internal : (kernel_function -> t) p_ref
    (** Inputs/Outputs with local and formal variables *)

  val get_external : (kernel_function -> t) p_ref
    (** Inputs/Outputs without either local or formal variables *)

  val statement : (stmt -> t) p_ref
  val expr : (stmt -> exp -> t) p_ref

  val kinstr : kinstr -> t option
    (** Effects of the given statement or of the englobing statement *)

  (** {3 Pretty printing} *)

  val display : (Format.formatter -> kernel_function -> unit) p_ref
  val pretty : Format.formatter -> t -> unit

end

(** Computation of read inputs.
    That is over-approximation of zones read by each function.
    @see <../inout/Inputs.html> internal documentation. *)
module Inputs : 
  sig
    include INOUT with type t = Locations.Zone.t
    
    val self_with_formals: Project.Computation.t p_ref

  val get_with_formals : (kernel_function -> t) p_ref
    (** Inputs with formals and without local variables *)

  val display_with_formals: (Format.formatter -> kernel_function -> unit) p_ref

  end

(** Computation of outputs.
    That is over-approximation of zones written by each function.
    @see <../inout/Outputs.html> internal documentation. *)
module Outputs : INOUT with type t = Locations.Zone.t

(** Computation of operational inputs.
    That is:
    - over-approximation of zones whose input values are read by each function,
    Computation of sure outputs
    - under-approximation of zones written by each function.
    @see <../inout/Context.html> internal documentation. *)
module InOutContext : sig

  include INOUT with type t = Inout_type.t

  type t' = Locations.Zone.t

end

(** Metrics.
    @see <../metrics/Metrics.html> internal documentation. *)
module Metrics : sig
  type t =
      { sloc: int;
	call_statements: int;
	goto_statements: int;
	assign_statements: int;
	if_statements: int;
	loop_statements: int;
	mem_access: int;
	functions_without_source: int Cilutil.VarinfoHashtbl.t;
	functions_with_source: int Cilutil.VarinfoHashtbl.t;
      }
  val get : (unit -> t) p_ref
  val pretty : (Format.formatter -> t -> unit) p_ref
  val dump: (string -> t -> unit) p_ref
  val last_result: (unit -> t) p_ref
    (** @raise Not_found if there is no last result *)
end


(**/**)
(** Do not use yet.
    @see <../inout/Derefs.html> internal documentation. *)
module Derefs : INOUT with type t = Locations.Zone.t
(**/**)

(** Toplevel of command line interface.
    @see <../toplevel/main.html> internal documentation. *)
module Toplevel : sig
  val replay : (unit -> unit) p_ref
  val run_all_plugins : (Format.formatter -> unit) p_ref
end

(** {3 GUI} *)

(** This function should be called from time to time by all analysers taking
    time. In GUI mode, this will make the interface reactive. 
    @plugin development guide *)
val progress: (unit -> unit) p_ref


(** Ltl_to_acsl plugin.
    @see <../ltl_to_acsl/index.html> internal documentation *)
module Ltl_to_acsl : sig 
  val run : (unit -> unit) ref (**  *)
end


(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)

(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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
(** Common Types and Signatures *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Ctypes
open Lang.F
open Interpreted_automata

(* -------------------------------------------------------------------------- *)
(** {1 General Definitions} *)
(* -------------------------------------------------------------------------- *)

type 'a sequence = { pre : 'a ; post : 'a }

type 'a binder = { bind: 'b 'c. 'a -> ('b -> 'c) -> 'b -> 'c }

(** Oriented equality or arbitrary relation *)
type equation =
  | Set of term * term (** [Set(a,b)] is [a := b]. *)
  | Assert of pred

(** Access conditions *)
type acs =
  | RW (** Read-Write Access *)
  | RD (** Read-Only Access *)

(** Abstract location or concrete value *)
type 'a value =
  | Val of term
  | Loc of 'a

(** Contiguous set of locations *)
type 'a rloc =
  | Rloc of c_object * 'a
  | Rrange of 'a * c_object * term option * term option

(** Structured set of locations *)
type 'a sloc =
  | Sloc of 'a
  | Sarray of 'a * c_object * int (** full sized range (optimized assigns) *)
  | Srange of 'a * c_object * term option * term option
  | Sdescr of var list * 'a * pred

(** Typed set of locations *)
type 'a region = (c_object * 'a sloc) list

(** Logical values, locations, or sets of *)
type 'a logic =
  | Vexp of term
  | Vloc of 'a
  | Vset of Vset.set
  | Lset of 'a sloc list

(** Scope management for locals and formals *)
type scope = Enter | Leave

(** Container for the returned value of a function *)
type 'a result =
  | R_loc of 'a
  | R_var of var

(** Polarity of predicate compilation *)
type polarity = [ `Positive | `Negative | `NoPolarity ]

(** Frame Conditions.
    Consider a function [phi(m)] over memory [m],
    we want memories [m1,m2] and condition [p] such that
    [p(m1,m2) -> phi(m1) = phi(m2)].
    - [name] used for generating lemma
    - [triggers] for the lemma
    - [conditions] for the frame lemma to hold
    - [mem1,mem2] to two memories for which the lemma holds *)
type frame = string * Definitions.trigger list * pred list * term * term

(* -------------------------------------------------------------------------- *)
(** {1 Reversing Models}

    It is sometimes possible to reverse memory models abstractions
    into ACSL left-values via the definitions below. *)
(* -------------------------------------------------------------------------- *)

(** Reversed ACSL left-value *)
type s_lval = s_host * s_offset list

and s_host =
  | Mvar of varinfo (** Variable *)
  | Mmem of term    (** Pointed value *)
  | Mval of s_lval  (** Pointed value of another abstract left-value *)

and s_offset = Mfield of fieldinfo | Mindex of term

(** Reversed abstract value *)
type mval =
  | Mterm (** Not a state-related value *)
  | Maddr of s_lval (** The value is the address of an l-value in current memory *)
  | Mlval of s_lval (** The value is the value of an l-value in current memory *)
  | Mchunk of string (** The value is an abstract memory chunk (description) *)

(** Reversed update *)
type update = Mstore of s_lval * term
(** An update of the ACSL left-value with the given value *)

(* -------------------------------------------------------------------------- *)
(** {1 Memory Models} *)
(* -------------------------------------------------------------------------- *)

(** Memory Chunks.

    The concrete memory is partionned into a vector of abstract data.
    Each component of the partition is called a {i memory chunk} and
    holds an abstract representation of some part of the memory.

    Remark: memory chunks are not required to be independant from each other,
    provided the memory model implementation is consistent with the chosen
    representation. Conversely, a given object might be represented by
    several memory chunks. See {!Model.domain}.

*)
module type Chunk =
sig

  type t
  val self : string (** Chunk names, for pretty-printing. *)
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pretty : Format.formatter -> t -> unit

  val tau_of_chunk : t -> tau
  (** The type of data hold in a chunk. *)

  val basename_of_chunk : t -> string
  (** Used when generating fresh variables for a chunk. *)

  val is_framed : t -> bool
  (** Whether the chunk is local to a function call.

      Means the chunk is separated from anyother call side-effects.
      If [true], entails that a function assigning everything can not modify
      the chunk. Only used for optimisation, it would be safe to always
      return [false]. *)

end

(** Memory Environments.

    Represents the content of the memory, {i via} a vector of logic
    variables for each memory chunk.
*)
module type Sigma =
sig

  type chunk (** The type of memory chunks. *)
  module Chunk : Qed.Collection.S with type t = chunk

  (** Memory footprint. *)
  type domain = Chunk.Set.t

  (** Environment assigning logic variables to chunk.

      Memory chunk variables are assigned lazily. Hence, the vector is
      empty unless a chunk is accessed. Pay attention to this
      when you merge or havoc chunks.

      New chunks are generated from the context pool of {!Lang.freshvar}.
  *)
  type t

  val pretty : Format.formatter -> t -> unit
  (** For debugging purpose *)

  val create : unit -> t (** Initially empty environment. *)

  val mem : t -> chunk -> bool (** Whether a chunk has been assigned. *)
  val get : t -> chunk -> var (** Lazily get the variable for a chunk. *)
  val value : t -> chunk -> term (** Same as [Lang.F.e_var] of [get]. *)

  val copy : t -> t (** Duplicate the environment. Fresh chunks in the copy
                        are {i not} duplicated into the source environment. *)

  val join : t -> t -> Passive.t
  (** Make two environment pairwise equal {i via} the passive form.

      Missing chunks in one environment are added with the corresponding
      variable of the other environment. When both environments don't agree
      on a chunk, their variables are added to the passive form. *)

  val assigned : pre:t -> post:t -> domain -> pred Bag.t
  (** Make chunks equal outside of some domain.

      This is similar to [join], but outside the given footprint of an
      assigns clause. Although, the function returns the equality
      predicates instead of a passive form.

      Like in [join], missing chunks are reported from one side to the
      other one, and common chunks are added to the equality bag. *)

  val choose : t -> t -> t
  (** Make the union of each sigma, choosing the minimal variable
      in case of conflict.
      Both initial environments are kept unchanged. *)

  val merge : t -> t -> t * Passive.t * Passive.t
  (** Make the union of each sigma, choosing a {i new} variable for
      each conflict, and returns the corresponding joins.
      Both initial environments are kept unchanged. *)

  val merge_list : t list -> t * Passive.t list
  (** Same than {!merge} but for a list of sigmas. Much more efficient
      than folding merge step by step. *)

  val iter : (chunk -> var -> unit) -> t -> unit
  (** Iterates over the chunks and associated variables already
      accessed so far in the environment. *)

  val iter2 : (chunk -> var option -> var option -> unit) -> t -> t -> unit
  (** Same as [iter] for both environments. *)

  val havoc_chunk : t -> chunk -> t
  (** Generate a new fresh variable for the given chunk. *)

  val havoc : t -> domain -> t
  (** All the chunks in the provided footprint are generated and made fresh.

      Existing chunk variables {i outside} the footprint are copied into the new
      environment. The original environement itself is kept unchanged. More
      efficient than iterating [havoc_chunk] over the footprint.
  *)

  val havoc_any : call:bool -> t -> t
  (** All the chunks are made fresh. As an optimisation,
      when [~call:true] is set, only non-local chunks are made fresh.
      Local chunks are those for which [Chunk.is_frame] returns [true]. *)

  val remove_chunks : t -> domain -> t
  (** Return a copy of the environment where chunks in the footprint
      have been removed. Keep the original environment unchanged. *)

  val domain : t -> domain
  (** Footprint of a memory environment.
      That is, the set of accessed chunks so far in the environment. *)

  val union : domain -> domain -> domain (** Same as [Chunk.Set.union] *)
  val empty : domain (** Same as [Chunk.Set.empty] *)

  val writes : t sequence -> domain
  (** [writes s] indicates which chunks are new in [s.post] compared
      to [s.pre]. *)
end

(** Memory Models. *)
module type Model =
sig

  (** {2 Model Definition} *)

  val configure : WpContext.tuning
  (** Initializers to be run before using the model.
      Typically sets {!Context} values. *)

  val configure_ia: automaton -> vertex binder
  (** Given an automaton, return a vertex's binder.
      Currently used by the automata compiler to bind current vertex.
      See {!StmtSemantics}. *)

  val datatype : string
  (** For projectification. Must be unique among models. *)

  val hypotheses : unit -> MemoryContext.clause list
  (** Computes the memory model hypotheses including separation and validity
      clauses to be verified for this model. *)

  module Chunk : Chunk
  (** Memory model chunks. *)

  module Heap : Qed.Collection.S
    with type t = Chunk.t
  (** Chunks Sets and Maps. *)

  module Sigma : Sigma
    with type chunk = Chunk.t
     and module Chunk = Heap
  (** Model Environments. *)

  type loc
  (** Representation of the memory location in the model. *)

  type chunk = Chunk.t
  type sigma = Sigma.t
  type domain = Sigma.domain
  type segment = loc rloc

  (** {2 Reversing the Model} *)

  type state
  (** Internal (private) memory state description for later reversing the model. *)

  (** Returns a memory state description from a memory environement. *)
  val state : sigma -> state

  (** Try to interpret a term as an in-memory operation
      located at this program point. Only best-effort
      shall be performed, otherwise return [Mvalue].

      Recognized [Cil] patterns:
      - [Mvar x,[Mindex 0]] is rendered as [*x] when [x] has a pointer type
      - [Mmem p,[Mfield f;...]] is rendered as [p->f...] like in Cil
      - [Mmem p,[Mindex k;...]] is rendered as [p[k]...] to catch Cil [Mem(AddPI(p,k)),...] *)
  val lookup : state -> term -> mval

  (** Try to interpret a sequence of states into updates.

      The result shall be exhaustive with respect to values that are printed as [Sigs.mval]
      values at [post] label {i via} the [lookup] function.
      Otherwise, those values would not be pretty-printed to the user. *)
  val updates : state sequence -> Vars.t -> update Bag.t

  (** Propagate a sequent substitution inside the memory state. *)
  val apply : (term -> term) -> state -> state

  (** Debug *)
  val iter : (mval -> term -> unit) -> state -> unit

  val pretty : Format.formatter -> loc -> unit
  (** pretty printing of memory location *)

  (** {2 Memory Model API} *)

  val vars : loc -> Vars.t
  (** Return the logic variables from which the given location depend on. *)

  val occurs : var -> loc -> bool
  (** Test if a location depend on a given logic variable *)

  val null : loc
  (** Return the location of the null pointer *)

  val literal : eid:int -> Cstring.cst -> loc
  (** Return the memory location of a constant string,
      the id is a unique identifier. *)

  val cvar : varinfo -> loc
  (** Return the location of a C variable. *)

  val pointer_loc : term -> loc
  (** Interpret an address value (a pointer) as an abstract location.
      Might fail on memory models not supporting pointers. *)

  val pointer_val : loc -> term
  (** Return the adress value (a pointer) of an abstract location.
      Might fail on memory models not capable of representing pointers. *)

  val field : loc -> fieldinfo -> loc
  (** Return the memory location obtained by field access from a given
      memory location. *)

  val shift : loc -> c_object -> term -> loc
  (** Return the memory location obtained by array access at an index
      represented by the given {!term}. The element of the array are of
      the given {!c_object} type. *)

  val base_addr : loc -> loc
  (** Return the memory location of the base address of a given memory
      location. *)

  val base_offset : loc -> term
  (** Return the offset of the location, in bytes, from its base_addr. *)

  val block_length : sigma -> c_object -> loc -> term
  (**  Returns the length (in bytes) of the allocated block containing
       the given location. *)

  val cast : c_object sequence -> loc -> loc
  (** Cast a memory location into another memory location.
      For [cast ty loc] the cast is done from [ty.pre] to [ty.post].
      Might fail on memory models not supporting pointer casts. *)

  val loc_of_int : c_object -> term -> loc
  (** Cast a term representing an absolute memory address (to some c_object)
      given as an integer, into an abstract memory location. *)

  val int_of_loc : c_int -> loc -> term
  (** Cast a memory location into its absolute memory address,
      given as an integer with the given C-type. *)

  val domain : c_object -> loc -> domain
  (** Compute the set of chunks that hold the value of an object with
      the given C-type. It is safe to retun an over-approximation of the
      chunks involved. *)

  val load : sigma -> c_object -> loc -> loc value
  (** Return the value of the object of the given type at the given location in
      the given memory state. *)

  val copied : sigma sequence -> c_object -> loc -> loc -> equation list
  (**
     Return a set of equations that express a copy between two memory state.

     [copied sigma ty loc1 loc2] returns a set of formula expressing that the
     content for an object [ty] is the same in [sigma.pre] at [loc1] and in
     [sigma.post] at [loc2].
  *)

  val stored : sigma sequence -> c_object -> loc -> term -> equation list
  (**
     Return a set of formula that express a modification between two memory
     state.

     [copied sigma ty loc t] returns a set of formula expressing that
     [sigma.pre] and [sigma.post] are identical except for an object [ty] at
     location [loc] which is represented by [t] in [sigma.post].
  *)

  val assigned : sigma sequence -> c_object -> loc sloc -> equation list
  (**
     Return a set of formula that express that two memory state are the same
     except at the given set of memory location.

     This function can over-approximate the set of given memory location (e.g
     it can return [true] as if the all set of memory location was given).
  *)

  val is_null : loc -> pred
  (** Return the formula that check if a given location is null *)

  val loc_eq : loc -> loc -> pred
  val loc_lt : loc -> loc -> pred
  val loc_neq : loc -> loc -> pred
  val loc_leq : loc -> loc -> pred
  (** Memory location comparisons *)

  val loc_diff : c_object -> loc -> loc -> term
  (** Compute the length in bytes between two memory locations *)

  val valid : sigma -> acs -> segment -> pred
  (** Return the formula that tests if a memory state is valid
      (according to {!acs}) in the given memory state at the given
      segment.
  *)

  val frame : sigma -> pred list
  (** Assert the memory is a proper heap state preceeding the function
      entry point. *)

  val alloc : sigma -> varinfo list -> sigma
  (** Allocates new chunk for the validity of variables. *)

  val invalid : sigma -> segment -> pred
  (** Returns the formula that tests if the entire memory is invalid
      for write access. *)

  val scope : sigma sequence -> scope -> varinfo list -> pred list
  (** Manage the scope of variables.  Returns the updated memory model
      and hypotheses modeling the new validity-scope of the variables. *)

  val global : sigma -> term -> pred
  (** Given a pointer value [p], assumes this pointer [p] (when valid)
      is allocated outside the function frame under analysis. This means
      separated from the formals and locals of the function. *)

  val included : segment -> segment -> pred
  (** Return the formula that tests if two segment are included *)

  val separated : segment -> segment -> pred
  (** Return the formula that tests if two segment are separated *)

end

(* -------------------------------------------------------------------------- *)
(** {1 C and ACSL Compilers} *)
(* -------------------------------------------------------------------------- *)

(** Compiler for C expressions *)
module type CodeSemantics =
sig

  module M : Model (** The underlying memory model *)

  type loc = M.loc
  type nonrec value = loc value
  type nonrec result = loc result
  type sigma = M.Sigma.t

  val pp_value : Format.formatter -> value -> unit

  val cval : value -> term
  (** Evaluate an abstract value. May fail because of [M.pointer_val]. *)

  val cloc : value -> loc
  (** Interpret a value as a location. May fail because of [M.pointer_loc]. *)

  val cast : typ -> typ -> value -> value
  (** Applies a pointer cast or a conversion.

      [cast tr te ve] transforms a value [ve] with type [te] into a value
      with type [tr]. *)

  val equal_typ : typ -> value -> value -> pred
  (** Computes the value of [(a==b)] provided both [a] and [b] are values
      with the given type. *)

  val not_equal_typ : typ -> value -> value -> pred
  (** Computes the value of [(a==b)] provided both [a] and [b] are values
      with the given type. *)

  val equal_obj : c_object -> value -> value -> pred
  (** Same as [equal_typ] with an object type. *)

  val not_equal_obj : c_object -> value -> value -> pred
  (** Same as [not_equal_typ] with an object type. *)

  val exp : sigma -> exp -> value
  (** Evaluate the expression on the given memory state. *)

  val cond : sigma -> exp -> pred
  (** Evaluate the conditional expression on the given memory state. *)

  val lval : sigma -> lval -> loc
  (** Evaluate the left-value on the given memory state. *)

  val call : sigma -> exp -> loc
  (** Address of a function pointer.
      Handles [AddrOf], [StartOf] and [Lval] as usual. *)

  val instance_of : loc -> kernel_function -> pred
  (** Check whether a function pointer is (an instance of)
      some kernel function. Currently, the meaning
      of "{i being an instance of}" is simply equality. *)

  val loc_of_exp : sigma -> exp -> loc
  (** Compile an expression as a location.
      May (also) fail because of [M.pointer_val]. *)

  val val_of_exp : sigma -> exp -> term
  (** Compile an expression as a term.
      May (also) fail because of [M.pointer_loc]. *)

  val result : sigma -> typ -> result -> term
  (** Value of an abstract result container. *)

  val return : sigma -> typ -> exp -> term
  (** Return an expression with a given type.
      Short cut for compiling the expression, cast into the desired type,
      and finally converted to a term. *)

  val is_zero : sigma -> c_object -> loc -> pred
  (** Express that the object (of specified type) at the given location
      is filled with zeroes. *)

  (**
     Express that all objects in a range of locations have a given value.

     More precisely, [is_exp_range sigma loc ty a b v] express that
     value at [( ty* )loc + k] equals [v], forall [a <= k < b].
     Value [v=None] stands for zero.
  *)
  val is_exp_range :
    sigma -> loc -> c_object -> term -> term ->
    value option ->
    pred

  val unchanged : M.sigma -> M.sigma -> varinfo -> pred
  (** Express that a given variable has the same value in two memory states. *)

  type warned_hyp = Warning.Set.t * pred

  val init : sigma:M.sigma -> varinfo -> init option -> warned_hyp list
  (** Express that some variable has some initial value at the
      given memory state.

      Remark: [None] initializer are interpreted as zeroes. This is consistent
      with the [init option] associated with global variables in CIL,
      for which the default initializer are zeroes. There is no
      [init option] value associated with local initializers.
  *)

end

(** Compiler for ACSL expressions *)
module type LogicSemantics =
sig

  module M : Model (** Underlying memory model *)

  type loc = M.loc
  type nonrec value  = M.loc value
  type nonrec logic  = M.loc logic
  type nonrec region = M.loc region
  type nonrec result = M.loc result
  type sigma = M.Sigma.t

  (** {2 Frames}

      Frames are compilation environment for ACSL. A frame typically
      manages the current function, formal paramters, the memory environments
      at different labels and the [\result] and [\exit_status] values.

      The frame also holds the {i gamma} environment responsible for
      accumulating typing constraints, and the {i pool} for generating
      fresh logic variables.

      Notice that a [frame] is not responsible for holding the environment
      at label [Here], since this is managed by a specific compilation
      environment, see {!env} below.
  *)

  type frame
  val pp_frame : Format.formatter -> frame -> unit

  (** Get the current frame, or raise a fatal error if none. *)
  val get_frame : unit -> frame

  (** Execute the given closure with the specified current frame.
      The [Lang.gamma] and [Lang.pool] contexts are also set accordingly. *)
  val in_frame : frame -> ('a -> 'b) -> 'a -> 'b

  (** Get the memory environment at the given label.
      A fresh environment is created lazily if required.
      The label must {i not} be [Here]. *)
  val mem_at_frame : frame -> Clabels.c_label -> sigma

  (** Update a frame with a specific environment for the given label. *)
  val set_at_frame : frame -> Clabels.c_label -> sigma -> unit

  (** Same as [mem_at_frame] but for the current frame. *)
  val mem_frame : Clabels.c_label -> sigma

  (** Full featured constructor for frames, with fresh pool and gamma. *)
  val mk_frame :
    ?kf:Cil_types.kernel_function ->
    ?result:result ->
    ?status:Lang.F.var ->
    ?formals:value Cil_datatype.Varinfo.Map.t ->
    ?labels:sigma Clabels.LabelMap.t ->
    ?descr:string ->
    unit -> frame

  (** Make a local frame reusing the {i current} pool and gamma. *)
  val local : descr:string -> frame

  (** Make a fresh frame with the given function. *)
  val frame : kernel_function -> frame

  type call (** Internal call data. *)

  (** Create call data from the callee point of view,
      deriving data (gamma and pools) from the current frame.
      If [result] is specified, the called function will stored its result
      at the provided location in the current frame (the callee). *)
  val call : ?result:M.loc -> kernel_function -> value list -> call

  (** Derive a frame from the call data suitable for compiling the
      called function contracts in the provided pre-state. *)
  val call_pre   : sigma -> call -> sigma -> frame

  (** Derive a frame from the call data suitable for compiling the
      called function contracts in the provided pre-state and post-state. *)
  val call_post  : sigma -> call -> sigma sequence -> frame

  (** Result type of the current function in the current frame. *)
  val return : unit -> typ

  (** Result location of the current function in the current frame. *)
  val result : unit -> result

  (** Exit status for the current frame. *)
  val status : unit -> var

  (** Returns the current gamma environment from the current frame. *)
  val guards : frame -> pred list

  (** {2 Compilation Environment} *)

  type env
  (**
     Compilation environment for terms and predicates. Manages
     the {i current} memory state and the memory state at [Here].

     Remark: don't confuse the {i current} memory state with the
     memory state {i at label} [Here]. The current memory state is the one
     we have at hand when compiling a term or a predicate. Hence, inside
     [\at(e,L)] the current memory state when compiling [e] is the one at [L].
  *)

  (** Create a new environment.

      Current and [Here] memory points are initialized to [~here], if
      provided.

      The logic variables stand for
      formal parameters of ACSL logic function and ACSL predicates. *)
  val mk_env :
    ?here:sigma ->
    ?lvars:logic_var list ->
    unit -> env

  (** The {i current} memory state. Must be propertly initialized
      with a specific {!move} before. *)
  val current : env -> sigma

  (** Move the compilation environment to the specified [Here] memory state.
      This memory state becomes also the new {i current} one. *)
  val move_at : env -> sigma -> env

  (** Returns the memory state at the requested label.
      Uses the local environment for [Here] and the current frame
      otherwize. *)
  val mem_at : env -> Clabels.c_label -> sigma

  (** Returns a new environment where the current memory state is
      moved to to the corresponding label. Suitable for compiling [e] inside
      [\at(e,L)] ACSL construct. *)
  val env_at : env -> Clabels.c_label -> env

  (** {2 Compilers} *)

  (** Compile a term l-value into a (typed) abstract location *)
  val lval : env -> Cil_types.term_lval -> Cil_types.typ * M.loc

  (** Compile a term expression. *)
  val term : env -> Cil_types.term -> term

  (** Compile a predicate. The polarity is used to generate a weaker or
      stronger predicate in case of unsupported feature from WP or the
      underlying memory model. *)
  val pred : polarity -> env -> Cil_types.predicate -> pred

  (** Compile a term representing a set of memory locations into an abstract
      region. When [~unfold:true], compound memory locations are expanded
      field-by-field. *)
  val region : env -> unfold:bool -> Cil_types.term -> region

  (** Computes the region assigned by a list of froms. *)
  val assigned_of_froms :
    env -> unfold:bool -> from list -> region

  (** Computes the region assigned by an assigns clause.
      [None] means everyhting is assigned. *)
  val assigned_of_assigns :
    env -> unfold:bool -> assigns -> region option

  (** Same as [term] above but reject any set of locations. *)
  val val_of_term : env -> Cil_types.term -> term

  (** Same as [term] above but expects a single loc or a single
      pointer value. *)
  val loc_of_term : env -> Cil_types.term -> loc

  (** Compile a lemma definition. *)
  val lemma : LogicUsage.logic_lemma -> Definitions.dlemma

  (** {2 Regions} *)

  (** Qed variables appearing in a region expression. *)
  val vars : region -> Vars.t

  (** Member of vars. *)
  val occurs : var -> region -> bool

  (** Check assigns inclusion.
      Compute a formula that checks whether written locations are either
      invalid (at the given memory location)
      or included in some assignable region. *)
  val check_assigns : sigma -> written:region -> assignable:region -> pred

end

(** Compiler for Performing Assigns *)
module type LogicAssigns = sig

  module M : Model
  module L : LogicSemantics with module M = M
  open M

  (** Memory footprint of a region. *)
  val domain : loc region -> Heap.set

  (** Relates two memory states corresponding to an assigns clause
      with the specified set of locations. *)
  val apply_assigns : sigma sequence -> loc region -> pred list

end

(** All Compilers Together *)
module type Compiler = sig
  module M : Model
  module C : CodeSemantics with module M = M
  module L : LogicSemantics with module M = M
  module A : LogicAssigns with module M = M and module L = L
end

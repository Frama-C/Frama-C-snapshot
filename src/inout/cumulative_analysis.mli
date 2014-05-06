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

open Cil_types

(** Implementation of a simple meta-analysis on top of the results of
    the value analysis. This implementation correctly handles
    memoization and apparent recursive calls during the value analysis.

    The underlying analysis is supposed to be cumulative at the level
    of a kernel_function (its results are derived from the results on
    all its statements), and mostly non-contextual (all the informations
    can be gathered using a Cil visitor).
*)


val specialize_state_on_call: ?stmt:stmt -> kernel_function -> Db.Value.state
  (** If the given statement is a call to the given function,
      enrich the superposed memory state at this statement with
      the formal arguments of this function. This is usually more precise
      than the superposition of all initial states of the function  *)


(** Frama-C visitor for cumulative analyses: we add a few useful methods.
    The method [compute_kf] must be used to add the effects of a call to the
    given kernel function to the pool of results *)
class virtual ['a] cumulative_visitor : object
  inherit Visitor.frama_c_inplace

  method specialize_state_on_call: kernel_function -> Db.Value.state
    (** If the current statement is a call to the given function,
        enrich the superposed memory state at this statement with
        the formal arguments of this function. Useful to do an analysis
        with a limited amount of context *)
  

  method virtual compute_kf: kernel_function -> 'a
    (** Virtual function to use when one needs to compute the effect
        of a function call. This function carries implictly a context:
        thus calling [self#compute_kf k1; self#compute_kf k2]
        is different from calling one within the other *)
end


class type virtual ['a] cumulative_class = object
  inherit ['a] cumulative_visitor

  method bottom: 'a

  (** Result of the analysis *)
  method result: 'a
    (** Adding partial results to the current ones *)
  method join: 'a -> unit

  (** Function that computes and returns the partial results on a funspec.
      May consult [self#current_stmt] to specialize itself, and return
      partially contextual results *)
  method compute_funspec : kernel_function -> 'a

  (** Assuming [v] are the results of the analysis for [f] (ie. the union
      of the results on all the statements of [f], or [compute_funspec f]
      if [f] has no body), [clean_kf_result k v] cleans those results
      before storing them. Use for example to remove out-of-scope locals *)
  method clean_kf_result: kernel_function -> 'a -> 'a
end



module Make (X:
  sig
    val analysis_name: string

    (** Type of the results *)
    type t
    module T: Datatype.S with type t = t

    (** Class that implements the analysis. Must not deal with memoization,
        as this is automatically done by the functor *)
    class virtual do_it: [t] cumulative_class
  end) :
sig

  (** Module that contains the memoized results *)
  module Memo: sig val self: State.t end

  (** Class that implements a cached version of the above analysis.
      Recursion in the dynamic call graphs are handled, provided the value
      analysis terminated without detecting a real recursion *)
  class do_it_cached: Kernel_function.t list ->
  object
    inherit X.do_it

    (** Internal methods that gives the functions for which a cycle
        has been detected in the dynamic call-graph. Results cannot
        be safely memoized if this set is not empty *)
    method cycle: Kernel_function.Hptset.t

    (** Memoized version of the analysis of a kernel-function *)
    method compute_kf: kernel_function -> X.t
  end

  (** Effects of the given kernel_function, using memoization *)
  val kernel_function: kernel_function -> X.t

  (** Effects of a statement, using memoization if it contains a function call*)
  val statement: stmt -> X.t

  (** Effects of the given expression (wich is supposed to be at the given
      statement *)
  val expr: stmt -> exp -> X.t
end

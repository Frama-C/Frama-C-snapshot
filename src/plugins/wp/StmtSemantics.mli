(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

open Cil_types
open Clabels

module Make(Compiler : Sigs.Compiler) :
sig

  module Cfg : CfgCompiler.Cfg with module S = Compiler.M.Sigma

  type node = Cfg.node
  type goal = {
    goal_pred : Cfg.P.t;
    goal_prop : WpPropId.prop_id;
  }
  type cfg = Cfg.cfg
  type paths = {
    paths_cfg : cfg;
    paths_goals : goal Bag.t;
  }

  val goals_nodes: goal Bag.t -> Cfg.Node.Set.t

  exception LabelNotFound of c_label

  (** Compilation environment *)

  type env

  val empty_env : Kernel_function.t -> env
  val bind : c_label -> node -> env -> env
  
  val result : env -> Lang.F.var

  val (@^) : paths -> paths -> paths (** Same as [Cfg.concat] *)
  val (@*) : env -> ( c_label * node ) list -> env (** fold bind *)
  val (@:) : env -> c_label -> node
  (** LabelMap.find with refined excpetion.
      @raise LabelNotFound instead of [Not_found] *)
    
  val (@-) : env -> (c_label -> bool) -> env
  
  val sequence : (env -> 'a -> paths) -> env -> 'a list -> paths
  (** Chain compiler by introducing fresh nodes between each element
      of the list. For each consecutive [x;y] elements, a fresh node [n]
      is created, and [x] is compiled with [Next:n] and [y] is compiled with
      [Here:n]. *)

  val choice : ?pre:c_label -> ?post:c_label ->
    (env -> 'a -> paths) -> env -> 'a list -> paths
  (** Chain compiler in parallel, between labels [~pre] and [~post], which
      defaults to resp. [here] and [next]. 
      The list of eventualities is exhastive, hence an [either] assumption
      is also inserted. *)

  val parallel : ?pre:c_label -> ?post:c_label ->
    (env -> 'a -> Cfg.C.t * paths) -> env -> 'a list -> paths
  (** Chain compiler in parallel, between labels [~pre] and [~post], which
      defaults to resp. [here] and [next]. 
      The list of eventualities is exhastive, hence an [either] assumption
      is also inserted. *)

  (** {2 Instructions Compilation}

      Each instruction or statement is typically compiled between
      [Here] and [Next] nodes in the [flow]. [Pre], [Post] and [Exit] are
      reserved for the entry and exit points of current function.
      in [flow] are used when needed such as [Break] and [Continue] and 
      should be added before calling.
  *)

  val set : env -> lval -> exp -> paths
  val scope : env -> Sigs.scope -> varinfo list -> paths
  val instr : env -> instr -> paths
  val return : env -> exp option -> paths
  val assume : Cfg.P.t -> paths

  val call_kf : env -> lval option -> kernel_function -> exp list -> paths
  val call : env -> lval option -> exp -> exp list -> paths

  (** {2 ACSL Compilation}  *)

  val spec : env -> spec -> paths
  val assume_ : env -> Sigs.polarity -> predicate -> paths
  val assigns : env -> assigns -> paths
  val froms : env -> from list -> paths

  (** {2 Automata Compilation} *)

  val automaton : env -> Interpreted_automata.automaton -> paths

  val init: is_pre_main:bool -> env -> paths

  (** {2 Full Compilation} 
      
      Returns the set of all paths for the function, with all proof
      obligations. The returned node corresponds to the [Init] label. *)
  val compute_kf: Kernel_function.t -> paths * node
end

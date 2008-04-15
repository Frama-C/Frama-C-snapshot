(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* $Id: jc_fenv.ml,v 1.10 2008/11/14 16:00:58 ayad Exp $ *)

open Jc_stdlib
open Jc_env
open Jc_envset
open Jc_region
open Jc_ast

module rec LocationOrd
  : Map.OrderedType with type t = Effect.logic_info location =
struct
  type t = Effect.logic_info location
  let compare loc1 loc2 = 
    match loc1#node,loc2#node with
      | JCLvar v1, JCLvar v2 -> VarOrd.compare v1 v2
      | JCLvar _v, _ -> 1
      | _, JCLvar _v -> -1
      | _,_ -> 0
	  (* TODO: complete def of Location *)
end

and Location
  : Map.OrderedType with type t = Effect.logic_info location * Memory.t =
  PairOrd(LocationOrd)(Memory)

and LocationSet : Set.S with type elt = Location.t = Set.Make(Location)

and LocationMap : Map.S with type key = Location.t = Map.Make(Location)

and Effect : 
sig 
  type effect =
      {
	jc_effect_alloc_tables: LogicLabelSet.t AllocMap.t;
	jc_effect_tag_tables: LogicLabelSet.t TagMap.t;
	jc_effect_raw_memories: LogicLabelSet.t MemoryMap.t;
	jc_effect_precise_memories: LogicLabelSet.t LocationMap.t;
	jc_effect_memories: LogicLabelSet.t MemoryMap.t;
	jc_effect_globals: LogicLabelSet.t VarMap.t;
	jc_effect_locals: LogicLabelSet.t VarMap.t;
	jc_effect_mutable: StringSet.t;
	jc_effect_committed: StringSet.t;
      }

  type fun_effect =
      {
	jc_reads : effect;
	jc_writes : effect;
	jc_raises : ExceptionSet.t;
      }

  type logic_info =
      {
	jc_logic_info_tag : int;
	jc_logic_info_name : string;
	mutable jc_logic_info_final_name : string;
	mutable jc_logic_info_result_type : jc_type option;
	mutable jc_logic_info_result_region : region;
	mutable jc_logic_info_parameters : var_info list;
	mutable jc_logic_info_param_regions : region list;
	mutable jc_logic_info_effects : effect;
	mutable jc_logic_info_calls : logic_info list;
	mutable jc_logic_info_axiomatic : string option;
(* obsolete
	mutable jc_logic_info_is_recursive : bool;
*)
	mutable jc_logic_info_labels : label list;
      }

  type builtin_treatment =
    | TreatGenFloat of float_operator_type

  type fun_info = 
      {
	jc_fun_info_tag : int;
	jc_fun_info_name : string;
	mutable jc_fun_info_final_name : string;
	mutable jc_fun_info_builtin_treatment : builtin_treatment option; 
	jc_fun_info_result : var_info;
	jc_fun_info_return_region : region;
	(* If function has a label "return_label", this is a label denoting
	   the return statement of the function, to be used by static 
	   analysis to avoid merging contexts *)
	mutable jc_fun_info_has_return_label : bool;
	mutable jc_fun_info_parameters : var_info list;
	mutable jc_fun_info_param_regions : region list;
	mutable jc_fun_info_calls : fun_info list;
	mutable jc_fun_info_is_recursive : bool;
	mutable jc_fun_info_logic_apps : logic_info list;
	mutable jc_fun_info_effects : fun_effect;
      }
  end =
struct
  type effect =
      {
	jc_effect_alloc_tables: LogicLabelSet.t AllocMap.t;
	jc_effect_tag_tables: LogicLabelSet.t TagMap.t;
	jc_effect_raw_memories: LogicLabelSet.t MemoryMap.t;
	jc_effect_precise_memories: LogicLabelSet.t LocationMap.t;
	jc_effect_memories: LogicLabelSet.t MemoryMap.t;
	jc_effect_globals: LogicLabelSet.t VarMap.t;
	jc_effect_locals: LogicLabelSet.t VarMap.t;
	jc_effect_mutable: StringSet.t;
	jc_effect_committed: StringSet.t;
      }

  type fun_effect =
      {
	jc_reads : effect;
	jc_writes : effect;
	jc_raises : ExceptionSet.t;
      }

  type logic_info =
      {
	jc_logic_info_tag : int;
	jc_logic_info_name : string;
	mutable jc_logic_info_final_name : string;
	mutable jc_logic_info_result_type : jc_type option;
        (*r None for predicates *)
	mutable jc_logic_info_result_region : region;
	mutable jc_logic_info_parameters : var_info list;
	mutable jc_logic_info_param_regions : region list;
	mutable jc_logic_info_effects : effect;
	mutable jc_logic_info_calls : logic_info list;
(* obsolete
	mutable jc_logic_info_is_recursive : bool;
*)
	mutable jc_logic_info_axiomatic : string option;
	mutable jc_logic_info_labels : label list;
      }

  type builtin_treatment =
    | TreatGenFloat of float_operator_type

  type fun_info = 
      {
	jc_fun_info_tag : int;
	jc_fun_info_name : string;
	mutable jc_fun_info_final_name : string;
	mutable jc_fun_info_builtin_treatment : builtin_treatment option; 
	jc_fun_info_result : var_info;
	jc_fun_info_return_region : region;
	mutable jc_fun_info_has_return_label : bool;
	mutable jc_fun_info_parameters : var_info list;
	mutable jc_fun_info_param_regions : region list;
	mutable jc_fun_info_calls : fun_info list;
	mutable jc_fun_info_is_recursive : bool;
	mutable jc_fun_info_logic_apps : logic_info list;
	mutable jc_fun_info_effects : fun_effect;
      }
end

include Effect

type app = logic_info Jc_ast.app
type term_node = logic_info Jc_ast.term_node
type term = logic_info Jc_ast.term
type tag_node = logic_info Jc_ast.tag_node
type tag = logic_info Jc_ast.tag
type location_set_node = logic_info Jc_ast.location_set_node
type location_set = logic_info Jc_ast.location_set
type location_node = logic_info Jc_ast.location_node
type location = logic_info Jc_ast.location
type assertion = logic_info Jc_ast.assertion
type assertion_node = logic_info Jc_ast.assertion_node
type term_or_assertion = logic_info Jc_ast.term_or_assertion
type loop_annot = logic_info Jc_ast.loop_annot
type behavior = logic_info Jc_ast.behavior
type fun_spec = logic_info Jc_ast.fun_spec

type expr_node = (logic_info,fun_info) Jc_ast.expr_node
type expr = (logic_info,fun_info) Jc_ast.expr
type callee = (logic_info,fun_info) Jc_ast.callee
type call = (logic_info,fun_info) Jc_ast.call

(*
  Local Variables: 
  compile-command: "LC_ALL=C make -j -C .. bin/jessie.byte"
  End: 
*)

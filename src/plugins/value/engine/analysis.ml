(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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
open Eval

module type Results = sig
  type state
  type value
  type location

  val get_stmt_state : stmt -> state or_bottom
  val get_kinstr_state: kinstr -> state or_bottom
  val get_stmt_state_by_callstack:
    after:bool -> stmt -> state Value_types.Callstack.Hashtbl.t or_top_or_bottom
  val get_initial_state_by_callstack:
    kernel_function -> state Value_types.Callstack.Hashtbl.t or_top_or_bottom

  val eval_expr : state -> exp -> value evaluated
  val copy_lvalue: state -> lval -> value flagged_value evaluated
  val eval_lval_to_loc: state -> lval -> location evaluated
  val eval_function_exp: state -> exp -> kernel_function list evaluated
end

module type S = sig
  include Abstractions.S
  include Results with type state := Dom.state
                   and type value := Val.t
                   and type location := Loc.location
end

module type Analyzer = sig
  include S
  val compute_from_entry_point : kernel_function -> lib_entry:bool -> unit
  val compute_from_init_state: kernel_function -> Dom.t -> unit
  val initial_state: lib_entry:bool -> Dom.t or_bottom
end


module Make (Abstract: Abstractions.S) = struct

  include Abstract

  module Eval = Evaluation.Make (Abstract.Val) (Abstract.Loc) (Abstract.Dom)

  include Compute_functions.Make (Abstract) (Eval)

  let get_stmt_state stmt =
    let fundec = Kernel_function.(get_definition (find_englobing_kf stmt)) in
    if Mark_noresults.should_memorize_function fundec && Db.Value.is_computed ()
    then Abstract.Dom.Store.get_stmt_state stmt
    else `Value Abstract.Dom.top

  let get_kinstr_state = function
    | Kglobal -> Abstract.Dom.Store.get_global_state ()
    | Kstmt stmt -> get_stmt_state stmt

  let get_stmt_state_by_callstack =
    Abstract.Dom.Store.get_stmt_state_by_callstack

  let get_initial_state_by_callstack =
    Abstract.Dom.Store.get_initial_state_by_callstack

  let eval_expr state expr = Eval.evaluate state expr >>=: snd

  let copy_lvalue state expr = Eval.copy_lvalue state expr >>=: snd

  let eval_lval_to_loc state lv =
    let get_loc (_, loc, _) = loc in
    let for_writing = false in
    Eval.lvaluate ~for_writing state lv >>=: get_loc

  let eval_function_exp state e =
    Eval.eval_function_exp e state >>=: (List.map fst)

end


module Legacy = Make (Abstractions.Legacy)

module Default =
  (val
    (if Abstractions.default_config = Abstractions.legacy_config
     then (module Legacy)
     else (module Make (Abstractions.Default)))
    : Analyzer)


(* Reference to the current configuration (built by Abstractions.configure from
   the parameters of Eva regarding the abstractions used in the analysis) and
   the current Analyzer module. *)
let ref_analyzer =
  ref (Abstractions.default_config, (module Default : Analyzer))

(* Returns the current Analyzer module. *)
let current_analyzer () = (module (val (snd !ref_analyzer)): S)

(* Set of hooks called whenever the current Analyzer module is changed.
   Useful for the GUI parts that depend on it. *)
module Analyzer_Hook = Hook.Build (struct type t = (module S) end)

(* Register a new hook. *)
let register_hook = Analyzer_Hook.extend

(* Sets the current Analyzer module for a given configuration.
   Calls the hooks above. *)
let set_current_analyzer config (analyzer: (module Analyzer)) =
  Analyzer_Hook.apply (module (val analyzer): S);
  ref_analyzer := (config, analyzer)

let cvalue_initial_state () =
  let module A = (val snd !ref_analyzer) in
  let _, lib_entry = Globals.entry_point () in
  Cvalue_domain.extract A.Dom.get (A.initial_state ~lib_entry)

(* Builds the Analyzer module corresponding to a given configuration,
   and sets it as the current analyzer. *)
let make_analyzer config =
  let analyzer =
    if config = Abstractions.legacy_config then (module Legacy: Analyzer)
    else if config = Abstractions.default_config then (module Default)
    else
      let module Abstract = (val Abstractions.make config) in
      let module Analyzer = Make (Abstract) in
      (module Analyzer)
  in
  set_current_analyzer config analyzer

(* Builds the analyzer according to the parameters of Eva. *)
let reset_analyzer () =
  let config = Abstractions.configure () in
  (* If the configuration has not changed, do not reset the Analyzer but uses
     the reference instead. *)
  if config <> fst !ref_analyzer
  then make_analyzer config

(* Builds the analyzer if needed, and run the analysis. *)
let force_compute () =
  Ast.compute ();
  let kf, lib_entry = Globals.entry_point () in
  reset_analyzer ();
  let module Analyzer = (val snd !ref_analyzer) in
  Analyzer.compute_from_entry_point ~lib_entry kf

let set_hook_on_parameter parameter =
  let open Typed_parameter in
  match parameter.accessor with
  | Bool (accessor, _)   -> accessor.add_set_hook (fun _ _ -> reset_analyzer ())
  | Int (accessor, _)    -> accessor.add_set_hook (fun _ _ -> reset_analyzer ())
  | String (accessor, _) -> accessor.add_set_hook (fun _ _ -> reset_analyzer ())

(* Resets the Analyzer whenever an abstraction parameter or the current project
   is changed. This maintains the analyzer consistent with the Eva parameters. *)
let () =
  List.iter set_hook_on_parameter Value_parameters.parameters_abstractions;
  Project.register_after_set_current_hook
    ~user_only:true (fun _ -> reset_analyzer ());
  Project.register_after_global_load_hook reset_analyzer

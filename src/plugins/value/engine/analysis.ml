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

open Cil_types
open Eval

module type Results = sig
  type state
  type value

  val get_stmt_state : stmt -> state or_bottom
  val eval_expr : state -> exp -> value evaluated
end

module type S = sig
  include Abstractions.S
  include Results with type state = Dom.state
                   and type value = Val.t
end

module type Analyzer = sig
  include S
  val compute : ?library:bool -> kernel_function -> unit
end


module Make (Abstract: Abstractions.S) = struct

  include Abstract

  type state = Abstract.Dom.state
  type value = Abstract.Val.t

  let computed = ref false

  module Eva = Evaluation.Make (Abstract.Val) (Abstract.Loc) (Abstract.Dom)
  module Eval = Non_linear_evaluation.Make (Abstract.Val) (Eva)

  module Init =
    Initialization.Make (Abstract.Val) (Abstract.Loc) (Abstract.Dom) (Eval)

  module Computer =
    Compute_functions.Make
      (Abstract.Val) (Abstract.Loc) (Abstract.Dom) (Eval) (Init)


  let compute ?(library=false) kf =
    Compute_functions.run Computer.compute_from_entry_point ~library kf;
    computed := true


  let get_stmt_state stmt =
    let fundec = Kernel_function.(get_definition (find_englobing_kf stmt)) in
    if Mark_noresults.should_memorize_function fundec && !computed
    then Abstract.Dom.Store.get_stmt_state stmt
    else `Value Abstract.Dom.top

  let eval_expr state expr = Eva.evaluate state expr >>=: snd

end


module Legacy = Make (Abstractions.Legacy)

let cvalue_initial_state () =
  Cvalue_domain.extract Abstractions.Legacy.Dom.get (Legacy.Init.initial_state ())

module Default =
  (val
    (if Abstractions.default_config = Abstractions.legacy_config
     then (module Legacy)
     else (module Make (Abstractions.Default)))
    : Analyzer)

let abstracts config =
  if config = Abstractions.default_config
  then (module Abstractions.Default : Abstractions.S)
  else Abstractions.make config

let ref_analyzer =
  ref (Abstractions.default_config, (module Default : Analyzer))


let current = ref (module Default : S)


let compute config ?(library=false) kf =
  let analyzer =
    if config = Abstractions.legacy_config then (module Legacy: Analyzer)
    else if config = Abstractions.default_config then (module Default)
    else if config = fst !ref_analyzer then snd !ref_analyzer
    else
      let module Abstract = (val abstracts config) in
      let module Analyzer = Make (Abstract) in
      ref_analyzer := (config, (module Analyzer));
      (module Analyzer)
  in
  let module Analyzer = (val analyzer) in
  current := (module Analyzer: S);
  Analyzer.compute ~library kf

let force_compute () =
  Ast.compute ();
  let kf, library = Globals.entry_point () in
  let config = Abstractions.configure () in
  compute config ~library kf
